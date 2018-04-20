USE [DIIG]
GO

/****** Object:  View [Vendor].[EntityIDhistory]    Script Date: 10/11/2017 4:13:56 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







ALTER VIEW [Vendor].[EntityIDhistory]
AS



	
	--Assign EntitySize and related states via Unique_Transaction_ID/StandardizedVendorName
	select CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN Parent.EntityID 
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN ParentDuns.EntityID
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN DUNS.EntityID
		ELSE coalesce(vn.EntityID,Duns.entityID)
		 end  as EntityID
		,c.fiscal_year
		,parent.parentid
		,c.parentdunsnumber
		,c.dunsnumber
		,u.StandardizedVendorName
	, CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN Parent.ParentID 
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN ParentDuns.Dunsnumber
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN DUNS.Dunsnumber
		ELSE coalesce(vn.standardizedVendorName,Duns.Dunsnumber)
		 end  as EntityText
	
	, CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN 'PID'
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN 'PDuns'
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN 'Duns'
		WHEN vn.EntityID is not null
		THEN 'Name'
		ELSE 'Duns'
		 END as EntityCategory
		 	,CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN pidh.EntitySizeCode 
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN ParentDtPCH.EntitySizeCode
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN DtPCH.EntitySizeCode
		ELSE coalesce(u.EntitySizeCode,
			DtPCH.EntitySizeCode)
		 end  as EntitySizeCode
	,CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN pidh.IsEntityAbove1990constantReportingThreshold 
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN ParentDtPCH.IsEntityAbove1990constantReportingThreshold
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN DtPCH.IsEntityAbove1990constantReportingThreshold
		ELSE coalesce(u.IsEntityAbove1990constantReportingThreshold,
			DtPCH.IsEntityAbove1990constantReportingThreshold)
		 end  as IsEntityAbove1990constantReportingThreshold
		 ,CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN pidh.IsEntityAbove2016constantReportingThreshold 
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN ParentDtPCH.IsEntityAbove2016constantReportingThreshold
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN DtPCH.IsEntityAbove2016constantReportingThreshold
		ELSE coalesce(u.IsEntityAbove2016constantReportingThreshold,
			DtPCH.IsEntityAbove2016constantReportingThreshold)
		 end  as IsEntityAbove2016constantReportingThreshold
	 ,CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN pidh.AnyEntityUSplaceOfPerformance 
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN ParentDtPCH.AnyEntityUSplaceOfPerformance
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN DtPCH.AnyEntityUSplaceOfPerformance
		ELSE coalesce(u.AnyEntityUSplaceOfPerformance,
			DtPCH.AnyEntityUSplaceOfPerformance)
		 end  as AnyEntityUSplaceOfPerformance
	,coalesce(parent.ParentID,c.dunsnumber) as AllContractor
	--,interior.fiscal_year
	,c.ObligatedAmount
	,c.NumberOfActions
	,PARENT.Top100Federal
	,PARENT.Top6
	,PARENT.UnknownCompany
	from Contract.FPDS as C
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DtPCH
			ON DtPCH.FiscalYear=C.fiscal_year AND DtPCH.DUNSNUMBER=C.DUNSNumber
		LEFT OUTER JOIN Contractor.Dunsnumber as DUNS
			ON DUNS.Dunsnumber=DtPCH.Dunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENT
			ON PARENT.ParentID=DtPCH.ParentID
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCH
			ON ParentDtPCH.FiscalYear=C.fiscal_year  AND  ParentDtPCH.DUNSnumber=C.parentdunsnumber
		LEFT OUTER JOIN Contractor.Dunsnumber as ParentDUNS
			ON ParentDUNS.Dunsnumber=ParentDtPCH.Dunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquared
			ON PARENTsquared.ParentID= ParentDtPCH.ParentID 
		left outer join contract.CSIStransactionID ctid
			on ctid.CSIStransactionID=c.CSIStransactionID
		left outer join contract.CSIScontractID ccid
			on ctid.CSIScontractID=ccid.CSIScontractID
		left outer join contract.UnlabeledDunsnumberUniqueTransactionIDentityIDhistory u
		on u.unique_transaction_id=c.unique_transaction_id
		and u.fiscal_year=c.fiscal_year
		left outer join FPDSTypeTable.Country3LetterCode c3lc
		on c3lc.Country3LetterCode=c.PlaceOfPerformanceCountryCode
		left outer join Location.CountryCodes country
		on country.[alpha-3]=c3lc.isoAlpha3
		left outer join FPDSTypeTable.statecode sc
		on sc.statecode=c.pop_state_code 
		left outer join Vendor.VendorName vn
		on vn.VendorName=u.StandardizedVendorName
		left outer join Vendor.ParentIDHistory pidh
	on pidh.ParentID=PARENT.ParentID and pidh.FiscalYear=c.fiscal_year
	--WHERE (Parent.ParentID is null or isnull(Parent.UnknownCompany,0)=1 ) and
	--	 (c.parentdunsnumber is null or isnull(ParentSquared.UnknownCompany,0)=1)
	--	 and (c.dunsnumber is null or isnull(Parent.UnknownCompany,0)=1 )
	--	 and u.unique_transaction_id is not null
	--GROUP BY 
	--	u.fiscal_year
	--	,u.StandardizedVendorName			
	--	,parent.parentid
	--	) as interior
	--	on u.StandardizedVendorName=interior.StandardizedVendorName
	--	and u.fiscal_year=interior.fiscal_year
	--left outer join Contractor.ParentContractor pid
	--on pid.parentid=interior.ParentID
	






	


	--select 
	--interior.Entity
	--, CASE
	--	WHEN parent.Top6=1 and parent.JointVenture=1
	--	THEN 'Large: Big 5 JV'
	--	WHEN Top6=1
	--	THEN 'Large: Big 5'
	--	WHEN parent.LargeGreaterThan3B=1
	--	THEN 'Large'
	--	WHEN parent.LargeGreaterThan1B=1
	--	THEN 'Medium >1B'
	--	WHEN MaxofSmall=1 and MinOfSmall=1
	--	THEN 'Always Small'
	--	WHEN MaxofSmall=1 and MinOfSmall=0
	--	THEN 'Sometimes Small'
	--	when parent.UnknownCompany=1
	--	Then 'Unlabeled'
	--	ELSE 'Medium <1B'
	--END AS EntitySize
	--,IsAbove1990constantReportingThreshold
	--	  ,IsAbove2016constantReportingThreshold	
	--,EntityCategory
	--,AllContractor
	--,interior.fiscal_year
	----,interior.LargeGreaterThan1B
	----,interior.LargeGreaterThan3B
	----,interior.MaxOfobligatedAmountMultiyear
	--,interior.SumOfobligatedAmount
	----,interior.MaxOfSmall
	----,interior.MinOfSmall
	--,interior.SumOfnumberOfActions
	----,interior.Top100Federal
	----,interior.Top6
	----,interior.UnknownCompany
	--,interior.MinOfPlaceOfPerformanceIsForeign as AnyUSplaceOfPerformance
	--from 
	--	(
	--	SELECT 
	--	C.fiscal_year
	--	, CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN Parent.ParentID 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN c.parentdunsnumber
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN c.dunsnumber
	--	ELSE coalesce(vn.standardizedvendorname
	--		,vlon.standardizedvendorname
	--		,vdabn.standardizedvendorname
	--		,van.standardizedvendorname
	--		,dn.standardizedvendorname
	--		,c.dunsnumber
	--	)
	--	END as Entity 
	--	, CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN 'PID'
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN 'PDuns'
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN 'DtPCH'
	--	ELSE 'Name'
	--	 END as EntityCategory
	--	, Max(IIF(C.contractingofficerbusinesssizedetermination='S'
	--			,1
	--			,0)) AS MaxOfSmall
	--	, Min(IIF(C.contractingofficerbusinesssizedetermination='S'
	--			,1
	--			,0)) AS MinOfSmall
	--	--, C.IDVPIID
	--	--, C.PIID
	--	, Sum(C.obligatedamount) AS SumOfobligatedAmount
	--	,max(cast(IsAbove1990constantReportingThreshold as integer)) as IsAbove1990constantReportingThreshold
	--	,max(cast(IsAbove2016constantReportingThreshold as integer)) as IsAbove2016constantReportingThreshold
	--	--, max(contracttotal.SumOfbaseandexercisedoptionsvalue) AS MaxOfBaseAndAllOptionsValueMultiyear
	--	--, max(contracttotal.SumofObligatedAmount) AS MaxOfobligatedAmountMultiyear
	--	--, sum(contracttotal.SumOfbaseandexercisedoptionsvalue) as SumOfMulbaseandexercisedoptionsvalueMultiyear
	--	--, Max(contracttotal.SumOfbaseandexercisedoptionsvalue) as MaxOfbaseandexercisedoptionsvalueMultiyear
	--	--, sum(contracttotal.SumOfbaseandalloptionsvalue) as SumOfbaseandalloptionsvalueMultiyear
	--	--, Max(contracttotal.SumOfbaseandalloptionsvalue) as MaxOfSumOfbaseandalloptionsvalueMultiyear
	--	, Sum(C.numberOfActions) AS SumOfnumberOfActions
	--	--, parent.JointVenture
	--	--, parent.UnknownCompany
	--	--, parent.Top100Federal
	--	--, parent.JointVenture
	--	--, parent.LargeGreaterThan3B
	--	--, parent.LargeGreaterThan1B
	--	--, parent.Top6
	--	--, Max(C.obligatedAmount) AS MaxOfobligatedAmount
	--	, isnull(parent.parentid,C.dunsnumber)  AS AllContractor
	--	,parent.parentid
	--	,min(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MinOfPlaceOfPerformanceIsForeign
	--FROM
	-- Contract.FPDS as C
 
	--	LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DtPCH
	--		ON DtPCH.FiscalYear=C.fiscal_year AND DtPCH.DUNSNUMBER=C.DUNSNumber
	--	LEFT OUTER JOIN Contractor.ParentContractor as PARENT
	--		ON PARENT.ParentID=DtPCH.ParentID
	--	LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCH
	--		ON ParentDtPCH.FiscalYear=C.fiscal_year  AND  ParentDtPCH.DUNSnumber=C.parentdunsnumber
	--	LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquared
	--		ON PARENTsquared.ParentID= ParentDtPCH.ParentID 
	--	LEFT OUTER JOIN FPDSTYPETABLE.AgencyID as AGENCY
	--		ON Agency.AGENCYID =C.contractingOfficeAgencyID
	--	left outer join contract.CSIStransactionID ctid
	--		on ctid.CSIStransactionID=c.CSIStransactionID
	--	left outer join contract.CSIScontractID ccid
	--		on ctid.CSIScontractID=ccid.CSIScontractID
	--	left outer join Vendor.VendorName vn
	--		on vn.vendorname=c.vendorname
	--		and isnull(vn.isunknownvendorname,0)=0
	--	left outer join Vendor.VendorName vlon
	--		on vlon.vendorname=c.vendorlegalorganizationname
	--		and isnull(vlon.isunknownvendorname,0)=0
	--	left outer join Vendor.VendorName vdabn
	--		on vdabn.vendorname=c.vendordoingasbusinessname
	--		and isnull(vdabn.isunknownvendorname,0)=0
	--	left outer join Vendor.VendorName van
	--		on van.vendorname=c.vendoralternatename
	--		and isnull(van.isunknownvendorname,0)=0
	--	left outer join Vendor.VendorName dn
	--		on dn.vendorname=c.divisionname
	--		and isnull(dn.isunknownvendorname,0)=0
	--	left outer join FPDSTypeTable.Country3LetterCode c3lc
	--	on c3lc.Country3LetterCode=c.PlaceOfPerformanceCountryCode
	--	left outer join Location.CountryCodes country
	--	on country.[alpha-3]=c3lc.isoAlpha3
	--	left outer join FPDSTypeTable.statecode sc
	--	on sc.statecode=c.pop_state_code 
	--GROUP BY 
	--	C.fiscal_year
	--	, CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN Parent.ParentID 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN c.parentdunsnumber
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN c.dunsnumber
	--	ELSE coalesce(vn.standardizedvendorname
	--		,vlon.standardizedvendorname
	--		,vdabn.standardizedvendorname
	--		,van.standardizedvendorname
	--		,dn.standardizedvendorname
	--		,c.dunsnumber
	--	)
	--	END
	--	, CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN 'PID'
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN 'PDuns'
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN 'DtPCH'
	--	ELSE 'Name'
	--	 END 
	--	--, parent.UnknownCompany
	--	--, parent.Top100Federal
	--	--, parent.LargeGreaterThan3B
	--	--, parent.LargeGreaterThan1B
	--	--, parent.JointVenture
	--	--, parent.Top6
	--	,parent.parentid
	--	, isnull(parent.parentid,C.dunsnumber) 
	--	) as interior
	--	LEFT OUTER JOIN Contractor.ParentContractor as PARENT
	--		ON interior.ParentID = PARENT.ParentID
	--inner join Economic.MicroTransactionThreshold mtt
	--on interior.fiscal_year=mtt.fiscal_year















GO


