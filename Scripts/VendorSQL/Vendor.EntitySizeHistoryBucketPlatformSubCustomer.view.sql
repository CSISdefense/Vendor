/****** Object:  View [Vendor].[EntitySizeHistoryBucketPlatformSubCustomer]    Script Date: 4/19/2018 11:50:29 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

ALTER VIEW [Vendor].[EntitySizeHistoryBucketPlatformSubCustomer]
AS

select interior.EntityID
		,interior.fiscal_year
		,eidh.[EntityText]
      ,eidh.[EntityCategory]
      ,eidh.[EntitySizeCode]
      ,eidh.[IsEntityAbove2016constantOneMillionThreshold]
      ,eidh.[IsEntityAbove2016constantReportingThreshold]
      ,eidh.[IsEntityAbove1990constantReportingThreshold]
      ,eidh.[AnyEntityUSplaceOfPerformance]
      ,eidh.[AnyEntityForeignPlaceOfPerformance]	
		,interior.parentid
		,interior.parentdunsnumber
		,interior.dunsnumber
		,interior.StandardizedVendorName
,interior.AllContractor
	,interior.ObligatedAmount
	,interior.NumberOfActions
	,interior.Top100Federal
	,interior.Top6
	,interior.UnknownCompany
	, interior.ProductOrServiceArea
	, interior.PlatformPortfolio
	, interior.Customer
	, interior.SubCustomer
	, interior.IsService
	, interior.Simple
	, interior.IsSoCal
	, interior.IsInSam
	, interior.isPresent
	, interior.SAMregyear
	, interior.duns
	--, interior.nextfiscal_year
	, interior.is_absent_nextyear_FPDS
	, interior.is_present_after_absent_FPDS
	,interior.FY_presaftabs
	,interior.FY_absaftpres
	,interior.minFY_FPDS
	--Assign EntitySize and related states via Unique_Transaction_ID/StandardizedVendorName
from (select CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN Parent.EntityID 
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN ParentDuns.EntityID
		WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
		THEN ParentDUNSimputed.EntityID
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN DUNS.EntityID
		ELSE coalesce(vn.EntityID,Duns.entityID)
		 end  as EntityID
		,c.fiscal_year
		,parent.parentid
		,c.parentdunsnumber
		,c.dunsnumber
		,u.StandardizedVendorName
		
	--, CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN Parent.ParentID 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN ParentDuns.Dunsnumber
	--	WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
	--	THEN DtPCH.parentdunsnumber
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN DUNS.Dunsnumber
	--	ELSE coalesce(vn.standardizedVendorName,Duns.Dunsnumber)
	--	 end  as Entity
	-- 	,CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN pidh.EntitySizeCode 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN ParentDtPCH.EntitySizeCode
	--	WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
	--	THEN ParentDtPCHimputed.EntitySizeCode
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN DtPCH.EntitySizeCode
	--	ELSE coalesce(u.EntitySizeCode,
	--		DtPCH.EntitySizeCode)
	--	 end  as EntitySizeCode
	--, CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN 'PID'
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 and ParentDtPCH.ChildCount=1
	--	THEN 'PDuns-1'
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 and ParentDtPCH.ChildCount=2
	--	THEN 'PDuns-2'
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 and ParentDtPCH.ChildCount>=3
	--	THEN 'PDuns-3+'
	--	--Next check the imputed parentdunsnumber
	--	WHEN ParentDtPCHimputed.parentdunsnumber is not null and isnull(ParentSquaredImputed.UnknownCompany,0)=0 and ParentDtPCHimputed.ChildCount=1
	--	THEN 'PDuns-1'
	--	WHEN ParentDtPCHimputed.parentdunsnumber is not null and isnull(ParentSquaredImputed.UnknownCompany,0)=0 and ParentDtPCHimputed.ChildCount=2
	--	THEN 'PDuns-2'
	--	WHEN ParentDtPCHimputed.parentdunsnumber is not null and isnull(ParentSquaredImputed.UnknownCompany,0)=0 and ParentDtPCHimputed.ChildCount>=3
	--	THEN 'PDuns-3+'
	--	--Even if using Dunsnumber here other entries may think of this as a parent.
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 and DtPCH.ChildCount=1
	--	THEN 'PDuns-1'
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 and DtPCH.ChildCount=2
	--	THEN 'PDuns-2'
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 and DtPCH.ChildCount>=3
	--	THEN 'PDuns-3+'
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN 'Duns'
	--	WHEN vn.EntityID is not null
	--	THEN 'Name'
	--	ELSE 'Duns'
	--	 END as EntityCategory
	--,CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN pidh.IsEntityAbove2016constantOneMillionThreshold 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN ParentDtPCH.IsEntityAbove2016constantOneMillionThreshold
	--	WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
	--	THEN ParentDtPCHimputed.IsEntityAbove2016constantOneMillionThreshold
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN DtPCH.IsEntityAbove2016constantOneMillionThreshold
	--	ELSE coalesce(u.IsEntityAbove2016constantOneMillionThreshold,
	--		DtPCH.IsEntityAbove2016constantOneMillionThreshold)
	--	 end  as IsEntityAbove2016constantOneMillionThreshold
	--,CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN pidh.IsEntityAbove2016constantReportingThreshold 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN ParentDtPCH.IsEntityAbove2016constantReportingThreshold
	--	WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
	--	THEN ParentDtPCHimputed.IsEntityAbove2016constantReportingThreshold
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN DtPCH.IsEntityAbove2016constantReportingThreshold
	--	ELSE coalesce(u.IsEntityAbove2016constantReportingThreshold,
	--		DtPCH.IsEntityAbove2016constantReportingThreshold)
	--	 end  as IsEntityAbove2016constantReportingThreshold
	--,CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN pidh.IsEntityAbove1990constantReportingThreshold 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN ParentDtPCH.IsEntityAbove1990constantReportingThreshold
	--	WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
	--	THEN ParentDtPCHimputed.IsEntityAbove1990constantReportingThreshold
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN DtPCH.IsEntityAbove1990constantReportingThreshold
	--	ELSE coalesce(u.IsEntityAbove1990constantReportingThreshold,
	--		DtPCH.IsEntityAbove1990constantReportingThreshold)
	--	 end  as IsEntityAbove1990constantReportingThreshold
	-- ,CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN pidh.AnyEntityUSplaceOfPerformance 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN ParentDtPCH.AnyEntityUSplaceOfPerformance
	--	WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
	--	THEN ParentDtPCHimputed.AnyEntityUSplaceOfPerformance
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN DtPCH.AnyEntityUSplaceOfPerformance
	--	WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
	--	THEN ParentDtPCHimputed.AnyEntityUSplaceOfPerformance
	--	ELSE coalesce(u.AnyEntityUSplaceOfPerformance,
	--		DtPCH.AnyEntityUSplaceOfPerformance)
	--	 end  as AnyEntityUSplaceOfPerformance
	--,CASE
	--	WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN pidh.AnyEntityForeignPlaceOfPerformance 
	--	WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
	--	THEN ParentDtPCH.AnyEntityForeignPlaceOfPerformance
	--	WHEN DtPCH.parentdunsnumber is not null and isnull(PARENTsquaredImputed.UnknownCompany,0)=0 
	--	THEN ParentDtPCHimputed.AnyEntityForeignPlaceOfPerformance
	--	WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
	--	THEN DtPCH.AnyEntityForeignPlaceOfPerformance
	--	ELSE coalesce(u.AnyEntityForeignPlaceOfPerformance,
	--		DtPCH.AnyEntityForeignPlaceOfPerformance)
	--	 end  as AnyEntityForeignPlaceOfPerformance
	,coalesce(parent.ParentID,c.dunsnumber) as AllContractor
	--,interior.fiscal_year
	,c.ObligatedAmount
	,c.NumberOfActions
	,PARENT.Top100Federal
	,PARENT.Top6
	,PARENT.UnknownCompany
	, psc.ProductOrServiceArea
	, coalesce(proj.PlatformPortfolio, Agency.PlatformPortfolio, cpc.PlatformPortfolio, psc.platformPortfolio) as PlatformPortfolio
	, ISNULL(Agency.Customer, Agency.AGENCYIDText) as Customer
	, Agency.SubCustomer
	, psc.IsService
	, psc.Simple
	, zip.IsSoCal
	, iif(SAM.duns is null,0,1) as IsInSAM
	, year(SAM.registrationDate) as SAMregyear
	, c.dunsnumber as duns
	--, iif(c.fiscal_year is null, 0,1) as IsPresent We don't need this
	--, (c.fiscal_year + 1) as nextfiscal_year
	, CASE
	WHEN coalesce(absaftpres.IsPresent,0)=0 --This is where we use in present, in order to make sure that it's a genuine appearance
	THEN 'YES'
	ELSE 'NO'
	END AS is_absent_nextyear_FPDS
	, CASE
	WHEN coalesce(presaftabs.IsPresent,0)=0
	THEN 'YES'
	ELSE 'NO'
	END AS is_present_after_absent_FPDS
	,presaftabs.fiscalyear as FY_presaftabs
	,absaftpres.fiscalyear as FY_absaftpres
	,minfy.minFY_FPDS
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
		LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCHimputed
			ON ParentDtPCHimputed.FiscalYear=C.fiscal_year  AND  ParentDtPCHimputed.DUNSnumber=DtPCH.parentdunsnumber
		LEFT OUTER JOIN Contractor.Dunsnumber as ParentDUNSimputed
			ON ParentDUNSimputed.Dunsnumber=ParentDtPCHimputed.Dunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquaredImputed
			ON PARENTsquaredImputed.ParentID= ParentDtPCHimputed.ParentID 
		
		LEFT OUTER JOIN dbo.allSAM as SAM
			ON SAM.duns = C.dunsnumber
		LEFT JOIN Contractor.DunsnumberToParentContractorHistory as absaftpres
			ON (absaftpres.dunsnumber = c.dunsnumber 
			AND  absaftpres.fiscalyear = (c.fiscal_year + 1))  --Not sure about order of operations, so adding perens
		LEFT JOIN Contractor.DunsnumberToParentContractorHistory as presaftabs
			ON (presaftabs.dunsnumber = c.dunsnumber 
			AND  presaftabs.fiscalyear = c.fiscal_year - 1)
		INNER JOIN (select dunsnumber, min(fiscal_year) as minFY_FPDS
			FROM Contract.FPDS 
			GROUP BY dunsnumber) as minfy ON c.dunsnumber = minfy.dunsnumber
		
		
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
			LEFT JOIN FPDSTypeTable.ProductOrServiceCode AS PSC 
			ON C.productorservicecode = PSC.ProductOrServiceCode 
	LEFT OUTER JOIN FPDSTYPETABLE.AgencyID as AGENCY
			ON Agency.AGENCYID =C.contractingOfficeAgencyID
	left outer join FPDSTypeTable.ClaimantProgramCode cpc
		on cpc.ClaimantProgramCode=c.claimantprogramcode
		
		left join contract.CSISidvmodificationID as idvmod
			on ctid.CSISidvmodificationID=idvmod.CSISidvmodificationID
		left join contract.CSISidvpiidID as idv
			on idv.CSISidvpiidID=idvmod.CSISidvpiidID
			      left join contract.CSIScontractID as cid
                     on cid.CSIScontractID=ctid.CSIScontractID
			LEFT OUTER JOIN Contract.ContractDiscretization AS CD
					ON CD.CSIScontractID = CTID.CSIScontractID

		--Block of Contract Label and ProjectID 
              left join Contract.ContractLabelID label
                     on coalesce(ctid.ContractLabelID,ccid.COntractlabelid,idv.ContractLabelID) = label.ContractLabelID
              LEFT JOIN Project.SystemEquipmentCodetoProjectIDhistory as SYS
                     ON SYS.systemequipmentcode=C.systemequipmentcode
                     and SYS.StartFiscalYear <= c.fiscal_year
                     and isnull(SYS.EndFiscalYear,9999) >= c.fiscal_year
              left join project.projectID Proj
                     on proj.projectid=isnull(sys.projectid,label.PrimaryProjectID)
			left outer join Location.Zipcode zip
				on zip.zipcode=c.placeofperformancezipcode
	
) as Interior
left outer join Vendor.EntityIDhistory eidh
on interior.entityid =eidh.entityid 
and interior.fiscal_year=eidh.fiscal_year





GO


