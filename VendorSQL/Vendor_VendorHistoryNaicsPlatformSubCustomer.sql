	USE [DIIG]
GO

/****** Object:  View [Vendor].[VendorFPDShistoryPlatformSubCustomerDirectDiscretization]    Script Date: 10/5/2017 9:47:32 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




Create VIEW [Vendor].[VendorHistoryNaicsPlatformSubCustomer]
AS

SELECT C.Fiscal_year
, ISNULL(Agency.Customer, Agency.AGENCYIDText) as Customer
, Agency.SubCustomer
,coalesce(proj.PlatformPortfolio, Agency.PlatformPortfolio, cpc.PlatformPortfolio, psc.platformPortfolio) as platformPortfolio
, isnull(parent.parentid,C.dunsnumber) AS AllContractor 
, parent.parentid
, Parent.UnknownCompany
, Parent.Top100Federal
, Parent.IsSiliconValley
, coalesce(PCN.CSISName, parent.parentid + '^',C.dunsnumber + '^')  as ContractorDisplayName
, Parent.jointventure
, C.obligatedAmount
, C.numberOfActions
, n.NAICS_Code
, n.Industry_TEXT
--Grouping Sub-Query
, IIf(C.contractingofficerbusinesssizedetermination='S' 
   And Not (parent.largegreaterthan3B=1 Or parent.Largegreaterthan3B=1)
   ,1
   ,0) AS Small
, iif(parent.parentid is null or
		parent.firstyear>c.fiscal_year or
		parent.mergeryear<=c.fiscal_year,1,0) as WarningFlag
FROM Contract.FPDS as C
			left outer join FPDSTypeTable.AgencyID AS Agency
			ON C.contractingofficeagencyid = Agency.AgencyID 
		LEFT JOIN Contractor.DunsnumberToParentContractorHistory as Dunsnumber
			ON (C.DUNSNumber=Dunsnumber.DUNSNUMBER) 
			AND (C.fiscal_year=Dunsnumber.FiscalYear) 
		LEFT JOIN Contractor.ParentContractor as Parent
			ON Dunsnumber.ParentID=Parent.ParentID	
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
	left outer join FPDSTypeTable.NAICScode n
		on c.principalnaicscode=n.NAICS_Code;
	



















GO


