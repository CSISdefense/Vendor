/****** Object:  View [Location].[OriginCountryHistoryBucketSubCustomer]    Script Date: 5/15/2018 1:22:15 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO












ALTER VIEW [Location].[OriginCountryHistoryBucketSubCustomer]
AS
SELECT 
C.fiscal_year
,getdate() AS Query_Run_Date
, ISNULL(CAgency.Customer, CAgency.AgencyIDtext) AS ContractingCustomer
	, CAgency.SubCustomer as ContractingSubCustomer
	, COALESCE(FAgency.Customer, FAgency.AgencyIDText, CAgency.Customer, CAgency.AGENCYIDText) as FundingAgency
	, COALESCE(FAgency.SubCustomer, FAgency.AgencyIDText, CAgency.SubCustomer, CAgency.AGENCYIDText) as FundingSubAgency
	,originiso.[USAID region] as OriginUSAIDregion
	,COALESCE(StateCode.CoCOM
		, PlaceCountryCode.CombatantCommand
		, placeiso.CombatantCommand) as PlaceCombatantCommand
	,vendoriso.[USAID region] as VendorUSAIDregion
	,placeiso.[USAID region] as PlaceUSAIDregion
	,coalesce(placeiso.[USAID region],vendoriso.[USAID region], originiso.[USAID region]) as GuessUSAIDregion
,case 
			when PlaceCountryCode.IsInternational=0 
				and isnull(VendorCountryCode.IsInternational,OriginCountryCode.IsInternational) = 0
			then 'Domestic US'
			when PlaceCountryCode.IsInternational=0 
				and isnull(VendorCountryCode.IsInternational,OriginCountryCode.IsInternational)= 1
			then 'Foreign Vendor in US'
			when PlaceCountryCode.ISOalpha3=
				isnull(vendorcountrycode.ISOalpha3,origincountrycode.ISOalpha3)
			then 'Host Nation Vendor'
			when PlaceCountryCode.ISOalpha3=origincountrycode.ISOalpha3
			then 'Possible Host Nation Vendor with contradiction'
			when PlaceCountryCode.IsInternational=1 
				and  isnull(VendorCountryCode.IsInternational,OriginCountryCode.IsInternational) =0 
			then 'US Vendor abroad'
			when PlaceCountryCode.IsInternational=1 and 
				isnull(VendorCountryCode.IsInternational,OriginCountryCode.IsInternational)=1
			then 'Third Country Vendor abroad'
			when PlaceCountryCode.IsInternational=1 and 
				isnull(VendorCountryCode.IsInternational,OriginCountryCode.IsInternational) is null
			then 'Unknown vendor abroad' 
			when PlaceCountryCode.IsInternational=0 and 
				isnull(VendorCountryCode.IsInternational,OriginCountryCode.IsInternational) is null
			then 'Unknown vendor in US' 
			when PlaceCountryCode.IsInternational is null and 
				isnull(VendorCountryCode.IsInternational,OriginCountryCode.IsInternational)=0
			then 'US vendor, unknown location' 
			when PlaceCountryCode.IsInternational is null and 
				isnull(VendorCountryCode.IsInternational,OriginCountryCode.IsInternational)=1
			then 'Foreign vendor, unknown location' 
			else 'Unlabeled'
			end as VendorPlaceType
,PSC.ServicesCategory
,C.productorservicecode
,Scat.IsService
,PSC.ProductOrServiceArea
,PSC.ProductServiceOrRnDarea
,PSC.DoDportfolio
,coalesce(proj.PlatformPortfolio, CAgency.PlatformPortfolio, cpc.PlatformPortfolio, psc.platformPortfolio) as platformPortfolio
,c.isforeignownedandlocated
,c.isforeigngovernment
,c.isinternationalorganization
,c.organizationaltype
,C.fundedbyforeignentity
,PlaceCountryCode.IsInternational as PlaceIsInternational
,PlaceCountryCode.Country3LetterCodeText as PlaceCountryText
,PlaceISO.WarTheater as PlaceWarTheater
,OriginCountryCode.IsInternational as OriginIsInternational
,OriginCountryCode.Country3LetterCodeText as OriginCountryText
,OriginISO.WarTheater as OriginWarTheater
,VendorCountryCode.IsInternational as VendorIsInternational
,VendorCountryCode.Country3LetterCodeText as VendorCountryText
,VendorISO.WarTheater as VendorWarTheater
,pom.placeofmanufactureText
,pom.BAAcategory
,C.obligatedAmount
,C.numberOfActions

FROM Contract.FPDS as C
		LEFT OUTER JOIN
			FPDSTypeTable.AgencyID AS CAgency ON C.contractingofficeagencyid = CAgency.AgencyID
		LEFT OUTER JOIN
			FPDSTypeTable.AgencyID AS FAgency ON C.fundingrequestingagencyid = FAgency.AgencyID



	--Location Code
	LEFT JOIN FPDStypeTable.StateCode as StateCode
		ON c.StateCode= StateCode.StateCode
	LEFT JOIN FPDSTypeTable.Country3lettercode as PlaceCountryCode
		ON C.placeofperformancecountrycode=PlaceCountryCode.Country3LetterCode
	left outer join location.CountryCodes as PlaceISO
		on PlaceCountryCode.isoAlpha3 =placeiso.[alpha-3]
	LEFT JOIN FPDSTypeTable.Country3lettercode as OriginCountryCode
		ON C.countryoforigin=OriginCountryCode.Country3LetterCode
	left outer join location.CountryCodes as OriginISO
		on OriginCountryCode.isoAlpha3 =OriginISO.[alpha-3]
	LEFT JOIN FPDSTypeTable.vendorcountrycode as VendorCountryCodePartial
		ON C.vendorcountrycode=VendorCountryCodePartial.vendorcountrycode
	LEFT JOIN FPDSTypeTable.Country3lettercode as VendorCountryCode
		ON vendorcountrycode.Country3LetterCode=VendorCountryCodePartial.Country3LetterCode
	left outer join location.CountryCodes as VendorISO
		on VendorCountryCode.isoAlpha3=VendorISO.[alpha-3]
	left outer join FPDSTypeTable.placeofmanufacture as PoM
		on c.placeofmanufacture=pom.placeofmanufacture

	--Others
		LEFT OUTER JOIN FPDSTypeTable.ProductOrServiceCode AS PSC 
		ON C.productorservicecode = PSC.ProductorServiceCode 

	LEFT JOIN ProductOrServiceCode.ServicesCategory As Scat
		ON Scat.ServicesCategory = PSC.ServicesCategory
	LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DUNS
		ON C.fiscal_year = DUNS.FiscalYear 
		AND C.DUNSNumber = DUNS.DUNSNUMBER
	LEFT OUTER JOIN Contractor.ParentContractor as PARENT
		ON DUNS.ParentID = PARENT.ParentID
	

			left outer join FPDSTypeTable.ClaimantProgramCode  as cpc on cpc.ClaimantProgramCode=c.claimantprogramcode

	--Block of CSISIDjoins
    left join contract.csistransactionid as CTID
                     on ctid.CSIStransactionID=c.CSIStransactionID
              left join contract.CSISidvmodificationID as idvmod
                     on idvmod.CSISidvmodificationID=ctid.CSISidvmodificationID
              left join contract.CSISidvpiidID as idv
                     on idv.CSISidvpiidID=idvmod.CSISidvpiidID
              left join contract.CSIScontractID as cid
                     on cid.CSIScontractID=ctid.CSIScontractID
			LEFT OUTER JOIN Contract.ContractDiscretization AS CD
					ON CD.CSIScontractID = CTID.CSIScontractID

	
--Block of Contract Label and ProjectID 
              left join Contract.ContractLabelID label
                     on coalesce(ctid.ContractLabelID,cid.COntractlabelid,idv.ContractLabelID) = label.ContractLabelID
              LEFT JOIN Project.SystemEquipmentCodetoProjectIDhistory as SYS
                     ON SYS.systemequipmentcode=C.systemequipmentcode
                     and SYS.StartFiscalYear <= c.fiscal_year
                     and isnull(SYS.EndFiscalYear,9999) >= c.fiscal_year
              left join project.projectID Proj
                     on proj.projectid=isnull(sys.projectid,label.PrimaryProjectID)





















GO


