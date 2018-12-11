	USE CSIS360
GO

/****** Object:  View [Vendor].[VendorFPDShistoryPlatformSubCustomerDirectDiscretization]    Script Date: 10/5/2017 9:47:32 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




Alter VIEW [Vendor].[VendorHistoryNaicsPlatformSubCustomer]
AS

SELECT C.Fiscal_year
, ISNULL(Agency.Customer, Agency.AGENCYIDText) as Customer
, Agency.SubCustomer
,coalesce(proj.PlatformPortfolio, Agency.PlatformPortfolio, cpc.PlatformPortfolio, psc.platformPortfolio) as platformPortfolio
,	 CASE
		WHEN Parent.ParentID is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN Parent.EntityID 
		WHEN c.parentdunsnumber is not null and isnull(ParentSquared.UnknownCompany,0)=0 
		THEN ParentDuns.EntityID
		WHEN c.dunsnumber is not null and isnull(Parent.UnknownCompany,0)=0 
		THEN DUNS.EntityID
		ELSE coalesce(vn.EntityID,Duns.entityID)
		 end  as EntityID
, isnull(parent.parentid,C.dunsnumber) AS AllContractor 
, parent.parentid
, Parent.UnknownCompany
, Parent.Top100Federal
, Parent.IsSiliconValley
, coalesce(PCN.CSISName, parent.parentid + '^',C.dunsnumber + '^')  as ContractorDisplayName
, Parent.jointventure
, C.Dunsnumber
, C.obligatedAmount
, C.numberOfActions
, n.principalNAICScode
, substring(n.principalNAICScode,1,2) NAICS2
, C.contractingofficerbusinesssizedetermination as DirectCOBSD
, n.principalnaicscodeText
, c.SignedDate
, year(c.SignedDate) as CalendarYear
, c.veteranownedflag
, c.minorityownedbusinessflag
, c.womenownedflag
, c.isforeigngovernment
, c.csistransactionID
, ctid.csiscontractid
, c.typeofsetaside
,psc.IsService
--Grouping Sub-Query
, IIf(C.contractingofficerbusinesssizedetermination='S' 
   And Not (parent.largegreaterthan3B=1 Or parent.Largegreaterthan3B=1)
   ,1
   ,0) AS Small
, iif(parent.parentid is null or
		parent.firstyear>c.fiscal_year or
		parent.mergeryear<=c.fiscal_year,1,0) as WarningFlag
		,OriginCountryCode.IsInternational as OriginIsInternational
,OriginCountryCode.Country3LetterCodeText as OriginCountryText
,VendorCountryCode.IsInternational as VendorIsInternational
,VendorCountryCode.Country3LetterCodeText as VendorCountryText
FROM Contract.FPDS as C
			left outer join FPDSTypeTable.AgencyID AS Agency
			ON C.contractingofficeagencyid = Agency.AgencyID 
		LEFT JOIN Contractor.DunsnumberToParentContractorHistory as DtPCH
			ON (C.DUNSNumber=DtPCH.DUNSNUMBER) 
			AND (C.fiscal_year=DtPCH.FiscalYear) 
		LEFT JOIN Contractor.ParentContractor as Parent
			ON DtPCH.ParentID=Parent.ParentID	
		LEFT JOIN FPDSTypeTable.ProductOrServiceCode AS PSC 
			ON PSC.ProductOrServiceCode = C.productorservicecode
		left outer join FPDSTypeTable.ClaimantProgramCode cpc
			on c.claimantprogramcode=cpc.claimantprogramcode
--Block of CSISIDjoins
              left join contract.csistransactionid as CTID
                     on ctid.CSIStransactionID=c.CSIStransactionID
              left join contract.CSISidvmodificationID as idvmod
                     on idvmod.CSISidvmodificationID=ctid.CSISidvmodificationID
              left join contract.CSISidvpiidID as idv
                     on idv.CSISidvpiidID=idvmod.CSISidvpiidID
              left join contract.CSIScontractID as cid
                     on cid.CSIScontractID=ctid.CSIScontractID
--Block of Contract Label and ProjectID 
              left join Contract.ContractLabelID label
                     on coalesce(ctid.ContractLabelID,cid.COntractlabelid,idv.ContractLabelID) = label.ContractLabelID
              LEFT JOIN Project.SystemEquipmentCodetoProjectIDhistory as SYS
                     ON SYS.systemequipmentcode=C.systemequipmentcode
                     and SYS.StartFiscalYear <= c.fiscal_year
                     and isnull(SYS.EndFiscalYear,9999) >= c.fiscal_year
              left join project.projectID Proj
                     on proj.projectid=isnull(sys.projectid,label.PrimaryProjectID)
	LEFT OUTER JOIN Contract.ContractDiscretization AS CD
		ON CD.CSIScontractID = CTID.CSIScontractID
--End Grouping Subquery
	LEFT JOIN Contractor.ParentContractorNameHistory as PCN
		ON Parent.ParentID = PCN.ParentID
		AND C.Fiscal_Year = PCN.FiscalYear
	left outer join FPDSTypeTable.principalNAICScode n
		on c.principalnaicscode=n.principalNAICScode
	--Vendor specific things
		left outer join contract.UnlabeledDunsnumberUniqueTransactionIDentityIDhistory u
		on u.unique_transaction_id=c.unique_transaction_id
	
			LEFT OUTER JOIN Contractor.Dunsnumber as DUNS
			ON DUNS.Dunsnumber=DtPCH.Dunsnumber
					left outer join Vendor.VendorName vn
		on vn.VendorName=u.StandardizedVendorName
	
			LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as ParentDtPCH
			ON ParentDtPCH.FiscalYear=C.fiscal_year  AND  ParentDtPCH.DUNSnumber=C.parentdunsnumber
		LEFT OUTER JOIN Contractor.Dunsnumber as ParentDUNS
			ON ParentDUNS.Dunsnumber=ParentDtPCH.Dunsnumber
		LEFT OUTER JOIN Contractor.ParentContractor as PARENTsquared
			ON PARENTsquared.ParentID= ParentDtPCH.ParentID 
--Vendor Country


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


















GO


