/****** Object:  StoredProcedure [Vendor].[sp_AutomatedEntityIDupdates]    Script Date: 4/1/2019 2:26:25 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
ALTER PROCEDURE [Vendor].[sp_AutomatedEntityIDupdates]
	-- Add the parameters for the stored procedure here
	
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	select  DUNSnumber
	,dtpch.StandardizedTopContractor
	,dtpch.ParentID
	,sum(dtpch.totalAmount) as SumOfTotalAmount
	,max(dtpch.TopVendorNameTotalAmount/nullif(dtpch.TotalAmount,0)) as MaxOfTopVendorNamePercent
	,min(dtpch.fiscalyear) MinOfFiscal_Year
	,max(dtpch.fiscalyear) MaxOfFiscal_Year
	,CSISmodifiedBy
	from contractor.DunsnumberToParentContractorHistory dtpch
	where dunsnumber in (select distinct DUNSnumber
	from contractor.DunsnumberToParentContractorHistory dtpch
	left outer join contractor.ParentContractor p
	on dtpch.ParentID=p.ParentID
	where p.UnknownCompany=1
	)
	group by DUNSnumber
	,dtpch.StandardizedTopContractor
	,dtpch.ParentID
	,CSISmodifiedBy
	order by DUNSnumber
	




--Automated proporagation un unknown vendor parentIDs for those dunsnumber where it is uncontroversial	
	update dtpch
	set parentid=interior.parentid,
	CSISmodifiedBy='Automated unknown vendor spreading by '+CURRENT_USER,
	CSISmodifiedDate=GETDATE()
	from contractor.DunsnumberToParentContractorHistory dtpch
	inner join
	(select dunsnumber, max(ParentID) as ParentID
	,min(fiscalyear) as MinOfFiscalYear
	from contractor.DunsnumberToParentContractorHistory dtpch
	where dunsnumber in (select distinct DUNSnumber
	from contractor.DunsnumberToParentContractorHistory dtpch
	left outer join contractor.ParentContractor p
	on dtpch.ParentID=p.ParentID
	where p.UnknownCompany=1
	)
	group by DUNSnumber
	having min(parentid)=max(parentid)) interior
	on interior.DUNSnumber=dtpch.DUNSnumber
	where 	dtpch.parentid is null
	and FiscalYear>=MinOfFiscalYear

	update ccid
	set
	IsAbove2016constantOneMillionThreshold=iif( contracttotal.SumOfobligatedAmountConstant2016>=1000000 or
		contracttotal.SumOfbaseandexercisedoptionsvalue>=mtt.OneMillionThreshold2016constant or
		contracttotal.SumOfBaseandalloptionsvalue>=mtt.OneMillionThreshold2016constant ,1,0) 
	 ,IsAbove1990constantReportingThreshold=iif( contracttotal.SumOfobligatedAmountConstant1990>=25000 or
		contracttotal.SumOfbaseandexercisedoptionsvalue>=mtt.MicroTransactionThreshold1990constant  or
		contracttotal.SumOfBaseandalloptionsvalue>=mtt.MicroTransactionThreshold1990constant ,1,0) 
	 ,IsAbove2016constantReportingThreshold=iif( contracttotal.SumOfobligatedAmountConstant2016>=3500 or
		contracttotal.SumOfbaseandexercisedoptionsvalue>=mtt.MicroTransactionThreshold2016constant  or
		contracttotal.SumOfBaseandalloptionsvalue>=mtt.MicroTransactionThreshold2016constant ,1,0) 
	from  (select CSIScontractID
	,min(f.fiscal_year) as MinOfFiscalYear
	,sum(obligatedAmount/d.GDPdeflator1990) as SumOfobligatedAmountConstant1990
	,sum(obligatedAmount/d.GDPdeflator2016) as SumOfobligatedAmountConstant2016
	,sum(baseandexercisedoptionsvalue) as SumOfbaseandexercisedoptionsvalue
	,sum(Baseandalloptionsvalue) as SumOfBaseandalloptionsvalue
	from contract.fpds f
	inner join contract.CSIStransactionID ctid
	on f.CSIStransactionID=ctid.CSIStransactionID
	left outer join economic.Deflators d
	on f.fiscal_year=d.Fiscal_Year
	group by ctid.CSIScontractID) as contracttotal
	inner join Economic.MicroTransactionThreshold mtt
	on contracttotal.MinOfFiscalYear=mtt.fiscal_year
inner join Contract.CSIScontractID ccid
on ccid.CSIScontractID =contracttotal.CSIScontractID



	insert into Contract.UnlabeledDunsnumberCSIStransactionIDentityID
(CSIStransactionID
,fiscal_year
,vendorname
,vendorlegalorganizationname
,vendordoingasbusinessname
,vendoralternatename
,divisionname
,dunsnumber
,StandardizedVendorName)
	select c.CSIStransactionID
	,c.fiscal_year
	,vn.vendorname 
			,c.vendorlegalorganizationname
			,c.vendordoingasbusinessname
			,c.vendoralternatename
			,c.divisionname
			,c.dunsnumber
	,coalesce(vn.StandardizedVendorName
			,vlon.StandardizedVendorName
			,vdabn.StandardizedVendorName
			,van.StandardizedVendorName
			,dn.StandardizedVendorName
		) as StandardizedVendorName
	from contract.FPDS c
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DtPCH
			ON DtPCH.FiscalYear=C.fiscal_year AND DtPCH.DUNSNUMBER=C.DUNSNumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENT
			ON PARENT.ParentID=DtPCH.ParentID
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCH
			ON ParentDtPCH.FiscalYear=C.fiscal_year  AND  ParentDtPCH.DUNSnumber=C.parentdunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquared
			ON PARENTsquared.ParentID= ParentDtPCH.ParentID 
		left outer join Vendor.VendorName vn
			on vn.vendorname=c.vendorname
			and isnull(vn.isunknownvendorname,0)=0
		left outer join Vendor.VendorName vlon
			on vlon.vendorname=c.vendorlegalorganizationname
			and isnull(vlon.isunknownvendorname,0)=0
		left outer join Vendor.VendorName vdabn
			on vdabn.vendorname=c.vendordoingasbusinessname
			and isnull(vdabn.isunknownvendorname,0)=0
		left outer join Vendor.VendorName van
			on van.vendorname=c.vendoralternatename
			and isnull(van.isunknownvendorname,0)=0
		left outer join Vendor.VendorName dn
			on dn.vendorname=c.divisionname
			and isnull(dn.isunknownvendorname,0)=0
		left outer join contract.UnlabeledDunsnumberCSIStransactionIDentityID u
		on u.CSIStransactionID=c.CSIStransactionID
	where 	(Parent.ParentID is null or isnull(Parent.UnknownCompany,0)=1)
		and (c.parentdunsnumber is null or isnull(ParentSquared.UnknownCompany,0)=1) 
		and (c.dunsnumber is null or isnull(Parent.UnknownCompany,0)=1 ) 
		and u.CSIStransactionID is null
	and coalesce(vn.StandardizedVendorName
			,vlon.StandardizedVendorName
			,vdabn.StandardizedVendorName
			,van.StandardizedVendorName
			,dn.StandardizedVendorName
		) is not null



	

--Add new Dunsnumbers into EntityID
insert into vendor.EntityID
(EntityText, Dunsnumber)
select distinct d.dunsnumber,d.dunsnumber
from contractor.Dunsnumber d
left outer join vendor.EntityID e
on d.DUNSnumber=e.Dunsnumber
where e.Dunsnumber is null


update d
set EntityID=ent.EntityID
from contractor.Dunsnumber d
inner join Vendor.EntityID ent
on d.DUNSnumber=ent.Dunsnumber
and d.entityid is null


--Add new parentIDs into entityID
insert into vendor.EntityID 
(EntityText,ParentID)
select distinct p.ParentID,p.ParentID
from contractor.ParentContractor p
left outer join vendor.EntityID e
on p.ParentID=e.ParentID
where e.ParentID is null


update p
set EntityID=ent.EntityID
from contractor.ParentContractor p
inner join Vendor.EntityID ent
on p.parentid=ent.parentid
and p.entityid is null



--Add standardized vendor names for unlabeled contractors
insert into vendor.EntityID 
(EntityText,VendorName)
select distinct u.StandardizedVendorName,u.StandardizedVendorName
from contract.UnlabeledDunsnumberUniqueTransactionIDentityIDhistory u
left outer join vendor.EntityID e
on u.StandardizedVendorName=e.VendorName
where e.VendorName is null	
	
update vn
set EntityID=ent.EntityID
from vendor.VendorName vn
left outer join vendor.entityid ent
on vn.Vendorname=ent.VendorName
and vn.entityid is null


	--Assign EntitySize and related states to ParentIDHistory
	update ph
	set EntitySizeCode=CASE
		WHEN pid.Top6=1 and pid.JointVenture=1
		THEN 'J'
		WHEN Top6=1
		THEN 'B'
		WHEN pid.LargeGreaterThan3B=1
		THEN 'L'
		WHEN MaxofSmall=1 and MinOfSmall=1
		THEN 'S'
		WHEN MaxofSmall=1 and MinOfSmall=0
		THEN 'N'
		when pid.UnknownCompany=1
		Then 'U'
		ELSE 'M'
	END 
	,IsEntityAbove2016constantOneMillionThreshold=IsAbove2016constantOneMillionThreshold
	,IsEntityAbove1990constantReportingThreshold=IsAbove1990constantReportingThreshold
	,IsEntityAbove2016constantReportingThreshold=IsAbove2016constantReportingThreshold	
	,AnyEntityUSplaceOfPerformance=~interior.MinOfPlaceOfPerformanceIsForeign 
	,AnyEntityForeignPlaceOfPerformance=interior.MaxOfPlaceOfPerformanceIsForeign
	from Vendor.ParentIDhistory ph
	inner join
		(
		SELECT parent.parentid
		,c.fiscal_year
		,max(cast(IsAbove2016constantOneMillionThreshold as integer)) as IsAbove2016constantOneMillionThreshold
		,max(cast(IsAbove1990constantReportingThreshold as integer)) as IsAbove1990constantReportingThreshold
		,max(cast(IsAbove2016constantReportingThreshold as integer)) as IsAbove2016constantReportingThreshold
		,min(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MinOfPlaceOfPerformanceIsForeign
		,max(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MaxOfPlaceOfPerformanceIsForeign
		, Max(IIF(C.contractingofficerbusinesssizedetermination='S'
				,1
				,0)) AS MaxOfSmall
		, Min(IIF(C.contractingofficerbusinesssizedetermination='S'
				,1
				,0)) AS MinOfSmall
	FROM
	 Contract.FPDS as C
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DtPCH
			ON DtPCH.FiscalYear=C.fiscal_year AND DtPCH.DUNSNUMBER=C.DUNSNumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENT
			ON PARENT.ParentID=DtPCH.ParentID
		left outer join contract.CSIStransactionID ctid
			on ctid.CSIStransactionID=c.CSIStransactionID
		left outer join contract.CSIScontractID ccid
			on ctid.CSIScontractID=ccid.CSIScontractID
		left outer join FPDSTypeTable.Country3LetterCode c3lc
		on c3lc.Country3LetterCode=c.PlaceOfPerformanceCountryCode
		left outer join Location.CountryCodes country
		on country.[alpha-3]=c3lc.isoAlpha3
		left outer join FPDSTypeTable.statecode sc
		on sc.statecode=c.pop_state_code 
	WHERE Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	GROUP BY 
		C.fiscal_year
		,parent.parentid
		) as interior
		on ph.parentid=interior.ParentID
		and ph.fiscalyear=interior.fiscal_year
	inner join Contractor.ParentContractor pid
	on pid.parentid=interior.ParentID


	
	--Assign EntitySize and related states via parentDunsnumber
	update ParentDtPCH
	set EntitySizeCode=CASE
		WHEN pid.Top6=1 and pid.JointVenture=1
		THEN 'J'		
		WHEN Top6=1
		THEN 'B'
		WHEN pid.LargeGreaterThan3B=1
		THEN 'L'
		WHEN MaxofSmall=1 and MinOfSmall=1
		THEN 'S'
		WHEN MaxofSmall=1 and MinOfSmall=0
		THEN 'N'
		when pid.UnknownCompany=1
		Then 'U'
		ELSE 'M'
	END 
	,ChildCount=interior.ChildCount
	,IsEntityAbove2016constantOneMillionThreshold=IsAbove2016constantOneMillionThreshold
	,IsEntityAbove1990constantReportingThreshold=IsAbove1990constantReportingThreshold
	,IsEntityAbove2016constantReportingThreshold=IsAbove2016constantReportingThreshold	
	,AnyEntityUSplaceOfPerformance=~interior.MinOfPlaceOfPerformanceIsForeign 
	,AnyEntityForeignPlaceOfPerformance=interior.MaxOfPlaceOfPerformanceIsForeign
	from Contractor.DunsnumberToParentContractorHistory ParentDtPCH
	inner join
		(
		SELECT parent.parentid
		,iif(c.parentdunsnumber is not null and parentsquared.UnknownCompany=0
			,c.parentdunsnumber 
			,dtpch.parentdunsnumber) as ParentDunsnumber
		,c.fiscal_year
		,count(distinct c.dunsnumber)+min(iif(not c.parentdunsnumber =c.dunsnumber,1,0 )) as ChildCount
		--This lists how many dunsnumbers reference a given parentdunsnumber.
		--The second clause is to make sure that parentduns inlude themselves in the child count.
		,max(cast(IsAbove2016constantOneMillionThreshold as integer)) as IsAbove2016constantOneMillionThreshold
		,max(cast(IsAbove1990constantReportingThreshold as integer)) as IsAbove1990constantReportingThreshold
		,max(cast(IsAbove2016constantReportingThreshold as integer)) as IsAbove2016constantReportingThreshold
		,min(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MinOfPlaceOfPerformanceIsForeign
		,max(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MaxOfPlaceOfPerformanceIsForeign
		, Max(IIF(C.contractingofficerbusinesssizedetermination='S'
				,1
				,0)) AS MaxOfSmall
		, Min(IIF(C.contractingofficerbusinesssizedetermination='S'
				,1
				,0)) AS MinOfSmall
	FROM
	 Contract.FPDS as C
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DtPCH
			ON DtPCH.FiscalYear=C.fiscal_year AND DtPCH.DUNSNUMBER=C.DUNSNumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENT
			ON PARENT.ParentID=DtPCH.ParentID
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCH
			ON ParentDtPCH.FiscalYear=C.fiscal_year  AND  ParentDtPCH.DUNSnumber=C.parentdunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquared
			ON PARENTsquared.ParentID= ParentDtPCH.ParentID 
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCHimputed
			ON ParentDtPCHimputed.FiscalYear=C.fiscal_year  AND  ParentDtPCHimputed.DUNSnumber=DtPCH.parentdunsnumber
		LEFT OUTER JOIN Contractor.Dunsnumber as ParentDUNSimputed
			ON ParentDUNSimputed.Dunsnumber=ParentDtPCHimputed.Dunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquaredImputed
			ON PARENTsquaredImputed.ParentID= ParentDtPCHimputed.ParentID 
		left outer join contract.CSIStransactionID ctid
			on ctid.CSIStransactionID=c.CSIStransactionID
		left outer join contract.CSIScontractID ccid
			on ctid.CSIScontractID=ccid.CSIScontractID
		left outer join FPDSTypeTable.Country3LetterCode c3lc
		on c3lc.Country3LetterCode=c.PlaceOfPerformanceCountryCode
		left outer join Location.CountryCodes country
		on country.[alpha-3]=c3lc.isoAlpha3
		left outer join FPDSTypeTable.statecode sc
		on sc.statecode=c.pop_state_code 
	WHERE (Parent.ParentID is null or isnull(Parent.UnknownCompany,0)=1 ) and
		 c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 and 
		 dtpch.parentdunsnumber is not null and isnull(ParentSquaredimputed.UnknownCompany,0)=0 
	GROUP BY 
		C.fiscal_year
		,iif(c.parentdunsnumber is not null and parentsquared.UnknownCompany=0
			,c.parentdunsnumber 
			,dtpch.parentdunsnumber)
		,parent.parentid
		) as interior
	on ParentDtPCH.DUNSnumber=interior.ParentDunsnumber
	and ParentDtPCH.FiscalYear=interior.fiscal_year
	left outer join Contractor.ParentContractor pid
	on pid.parentid=interior.ParentID
	

	
	
	--Assign EntitySize and related states via Dunsnumber
	update DtPCH
	set EntitySizeCode=CASE
		WHEN pid.Top6=1 and pid.JointVenture=1
		THEN 'J'
		WHEN Top6=1
		THEN 'B'
		WHEN pid.LargeGreaterThan3B=1
		THEN 'L'
		WHEN MaxofSmall=1 and MinOfSmall=1
		THEN 'S'
		WHEN MaxofSmall=1 and MinOfSmall=0
		THEN 'N'
		when pid.UnknownCompany=1
		Then 'U'
		ELSE 'M'
	END 
	,IsEntityAbove2016constantOneMillionThreshold=IsAbove2016constantOneMillionThreshold
	,IsEntityAbove1990constantReportingThreshold=IsAbove1990constantReportingThreshold
	,IsEntityAbove2016constantReportingThreshold=IsAbove2016constantReportingThreshold	
	,AnyEntityUSplaceOfPerformance=~interior.MinOfPlaceOfPerformanceIsForeign 
	,AnyEntityForeignPlaceOfPerformance=interior.MaxOfPlaceOfPerformanceIsForeign
	from Contractor.DunsnumberToParentContractorHistory DtPCH
	inner join
		(
		SELECT parent.parentid
		,c.dunsnumber
		,c.fiscal_year
		,max(cast(IsAbove2016constantOneMillionThreshold as integer)) as IsAbove2016constantOneMillionThreshold
		,max(cast(IsAbove1990constantReportingThreshold as integer)) as IsAbove1990constantReportingThreshold
		,max(cast(IsAbove2016constantReportingThreshold as integer)) as IsAbove2016constantReportingThreshold
		,min(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MinOfPlaceOfPerformanceIsForeign
		,max(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MaxOfPlaceOfPerformanceIsForeign
		, Max(IIF(C.contractingofficerbusinesssizedetermination='S'
				,1
				,0)) AS MaxOfSmall
		, Min(IIF(C.contractingofficerbusinesssizedetermination='S'
				,1
				,0)) AS MinOfSmall
	FROM
	 Contract.FPDS as C
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DtPCH
			ON DtPCH.FiscalYear=C.fiscal_year AND DtPCH.DUNSNUMBER=C.DUNSNumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENT
			ON PARENT.ParentID=DtPCH.ParentID
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCH
			ON ParentDtPCH.FiscalYear=C.fiscal_year  AND  ParentDtPCH.DUNSnumber=C.parentdunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquared
			ON PARENTsquared.ParentID= ParentDtPCH.ParentID 
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCHimputed
			ON ParentDtPCHimputed.FiscalYear=C.fiscal_year  AND  ParentDtPCHimputed.DUNSnumber=DtPCH.parentdunsnumber
		LEFT OUTER JOIN Contractor.Dunsnumber as ParentDUNSimputed
			ON ParentDUNSimputed.Dunsnumber=ParentDtPCHimputed.Dunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquaredImputed
			ON PARENTsquaredImputed.ParentID= ParentDtPCHimputed.ParentID 
		left outer join contract.CSIStransactionID ctid
			on ctid.CSIStransactionID=c.CSIStransactionID
		left outer join contract.CSIScontractID ccid
			on ctid.CSIScontractID=ccid.CSIScontractID
		left outer join FPDSTypeTable.Country3LetterCode c3lc
		on c3lc.Country3LetterCode=c.PlaceOfPerformanceCountryCode
		left outer join Location.CountryCodes country
		on country.[alpha-3]=c3lc.isoAlpha3
		left outer join FPDSTypeTable.statecode sc
		on sc.statecode=c.pop_state_code
		left outer join contract.UnlabeledDunsnumberUniqueTransactionIDentityIDhistory u
		on u.unique_transaction_id=c.unique_transaction_id
		and u.fiscal_year=c.fiscal_year
	WHERE (Parent.ParentID is null or isnull(Parent.UnknownCompany,0)=1 ) and
		( c.parentdunsnumber is null or isnull(ParentSquared.UnknownCompany,0)=1) and 
		( dtpch.parentdunsnumber is null or isnull(ParentSquaredimputed.UnknownCompany,0)=1) and 
		((c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0) or
		u.unique_transaction_id is null)
	GROUP BY 
		C.fiscal_year
		,c.dunsnumber
		,parent.parentid
		) as interior
	on DtPCH.DUNSnumber=interior.Dunsnumber
	and DtPCH.FiscalYear=interior.fiscal_year
	left outer join Contractor.ParentContractor pid
	on pid.parentid=interior.ParentID
	

	
	--Assign EntitySize and related states via Unique_Transaction_ID/StandardizedVendorName
	update v
	set EntitySizeCode=coalesce(pidh.EntitySizeCode
		,CASE
		WHEN pid.Top6=1 and pid.JointVenture=1
		THEN 'J'
		WHEN Top6=1
		THEN 'B'
		WHEN pid.LargeGreaterThan3B=1
		THEN 'L'
		WHEN MaxofSmall=1 and MinOfSmall=1
		THEN 'S'
		WHEN MaxofSmall=1 and MinOfSmall=0
		THEN 'N'
		when pid.UnknownCompany=1
		Then 'U'
		ELSE 'M'
	END )
	,IsEntityAbove2016constantOneMillionThreshold=coalesce(pidh.IsEntityAbove2016constantOneMillionThreshold
		,interior.IsAbove2016constantOneMillionThreshold)
	,IsEntityAbove1990constantReportingThreshold=coalesce(pidh.IsEntityAbove1990constantReportingThreshold
		,interior.IsAbove1990constantReportingThreshold)
	,IsEntityAbove2016constantReportingThreshold=coalesce(pidh.IsEntityAbove2016constantReportingThreshold
		,interior.IsAbove2016constantReportingThreshold)	
	,AnyEntityUSplaceOfPerformance=coalesce(pidh.AnyEntityUSplaceOfPerformance
		,~interior.MinOfPlaceOfPerformanceIsForeign)	
	,AnyEntityForeignPlaceOfPerformance=coalesce(pidh.AnyEntityForeignPlaceOfPerformance
		,interior.MaxOfPlaceOfPerformanceIsForeign)	
	from [Vendor].[StandardizedVendorNameHistory] v
	inner join
		(
		SELECT parent.parentid
		,u.StandardizedVendorName
		,u.fiscal_year
		,max(cast(IsAbove2016constantOneMillionThreshold as integer)) as IsAbove2016constantOneMillionThreshold
		,max(cast(IsAbove1990constantReportingThreshold as integer)) as IsAbove1990constantReportingThreshold
		,max(cast(IsAbove2016constantReportingThreshold as integer)) as IsAbove2016constantReportingThreshold
		,min(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MinOfPlaceOfPerformanceIsForeign
		,max(cast(coalesce(country.isforeign, ~ sc.usa) as tinyint)) as MaxOfPlaceOfPerformanceIsForeign
		, Max(IIF(C.contractingofficerbusinesssizedetermination='S'
				,1
				,0)) AS MaxOfSmall
		, Min(IIF(C.contractingofficerbusinesssizedetermination='S'
				,1
				,0)) AS MinOfSmall
	FROM
	 Contract.FPDS as C
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DtPCH
			ON DtPCH.FiscalYear=C.fiscal_year AND DtPCH.DUNSNUMBER=C.DUNSNumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENT
			ON PARENT.ParentID=DtPCH.ParentID
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCH
			ON ParentDtPCH.FiscalYear=C.fiscal_year  AND  ParentDtPCH.DUNSnumber=C.parentdunsnumber
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
	WHERE (Parent.ParentID is null or isnull(Parent.UnknownCompany,0)=1 ) and
		 (c.parentdunsnumber is null or isnull(ParentSquared.UnknownCompany,0)=1)
		 and (c.dunsnumber is null or isnull(Parent.UnknownCompany,0)=1 )
		 and u.unique_transaction_id is not null
	GROUP BY 
		u.fiscal_year
		,u.StandardizedVendorName			
		,parent.parentid
		) as interior
		on v.StandardizedVendorName=interior.StandardizedVendorName
		and v.fiscal_year=interior.fiscal_year
	left outer join Contractor.ParentContractor pid
	on pid.parentid=interior.ParentID
	left outer join Vendor.ParentIDHistory pidh
	on pidh.ParentID=interior.ParentID and pidh.FiscalYear=interior.fiscal_year









--select  StandardizedVendorName,  dunsnumber,count(*) as CountOfRows
--from contract.UnlabeledDunsnumberUniqueTransactionIDentityIDhistory
--where fiscal_year<2000
--group by  StandardizedVendorName,dunsnumber
--order by CountOfRows desc

--select unique_transaction_id,fiscal_year,StandardizedVendorName,count(*) as c
--from contract.UnlabeledDunsnumberUniqueTransactionIDentityIDhistory
--group by unique_transaction_id,fiscal_year,StandardizedVendorName
--having count(*)>1

--delete contract.UnlabeledDunsnumberUniqueTransactionIDentityIDhistory
--where StandardizedVendorName is null

--select *
--from contract.UnlabeledDunsnumberUniqueTransactionIDentityIDhistory
--order by fiscal_year
--where fiscal_year<2000

--update vn set EntityID=NULL
--from vendor.EntityID ent
--inner join vendor.VendorName vn
--on ent.EntityID=vn.EntityID
--where vn.isunknownvendorname=1





--update t
--set EntityID=ent.EntityID
--from Vendor.VendorName t
--inner join Vendor.EntityID ent
--on t.VendorName=ent.EntityText

--update ent
--set VendorName=t.VendorName
--from Vendor.VendorName t
--inner join Vendor.EntityID ent
--on t.EntityID=ent.EntityID


----alter table Vendor.EntityID
----add ParentID nvarchar(255) references contractor.ParentContractor(parentID)

--alter table Vendor.EntityID
--add VendorName nvarchar(150) references vendor.VendorName(VendorName)


END
GO


