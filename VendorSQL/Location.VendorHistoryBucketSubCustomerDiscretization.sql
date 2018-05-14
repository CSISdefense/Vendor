/****** Object:  View [Location].[VendorHistoryBucketSubCustomerDiscretization]    Script Date: 5/14/2018 1:06:14 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO














ALTER VIEW [Location].[VendorHistoryBucketSubCustomerDiscretization]
AS

SELECT 
[Fiscal_year]
      ,[Vendor]
      ,c.[parentid]
	  ,c.StandardizedTopContractor
      ,[Small]
      ,[jointventure]
      ,[warningflag]
      ,[UnknownCompany]
      ,[Top100Federal]
      ,[fundedbyforeignentity]
      ,[isforeignownedandlocated]
      ,[isforeigngovernment]
      ,[isinternationalorganization]
      ,[organizationaltype]
 ,[PlaceIsUnitedstates]
      ,[PlaceCountryText]
      ,[OriginIsUnitedStates]
      ,[OriginCountryText]
      ,[VendorIsUnitedStates]
      ,[VendorCountryText]
      ,[placeofmanufactureText]
      ,[vendorcountrycode]
,(SELECT ContractorDisplayName from contractor.ContractorDisplayName(
			null --@ServiceCategory as varchar(255)
			,null --,@ServicesOnly as bit
			,null --,@Customer as varchar(255)
			,null --,@SubCustomer as varchar(255)
			,C.SumOfobligatedAmount--,@SumOfobligatedAmount as decimal(19,4)
			,C.UnknownCompany--,@UnknownCompany as bit
			,C.Top100Federal--,@Top100Federal as bit
			,PCN.CSISName--,@CSISname as nvarchar(255)
			,C.Vendor--,@Vendor as varchar(255)
		)) as ContractorDisplayName
      ,[SumOfobligatedAmount]
      ,[SumOfnumberOfActions]
FROM (
SELECT C.Fiscal_year
, isnull(parent.parentid,C.dunsnumber) AS Vendor 
, iif(parent.parentid is null
	,dunsnumber.StandardizedTopContractor
	,NULL) as StandardizedTopContractor
, parent.parentid
, Max(IIf(C.contractingofficerbusinesssizedetermination='S' 
       And Not (parent.largegreaterthan3B=1 Or parent.Largegreaterthan3B=1)
       ,1
       ,0)) AS Small
, Parent.jointventure
, iif(parent.parentid is null or
	parent.firstyear>c.fiscal_year or
	parent.mergeryear<=c.fiscal_year,1,0) as warningflag
, Parent.UnknownCompany
, Parent.Top100Federal
, c.fundedbyforeignentity
,c.isforeignownedandlocated
,c.isforeigngovernment
,c.isinternationalorganization
,c.organizationaltype
,PlaceCountryCode.IsInternational as PlaceIsUnitedstates
,PlaceCountryCode.Country3LetterCodeText as PlaceCountryText
,OriginCountryCode.IsInternational as OriginIsUnitedStates
,OriginCountryCode.Country3LetterCodeText as OriginCountryText
,VendorCountryCode.IsInternational as VendorIsUnitedStates
,VendorCountryCode.Country3LetterCodeText as VendorCountryText
,pom.placeofmanufactureText
,c.vendorcountrycode
, Sum(C.obligatedAmount) AS SumOfobligatedAmount
, Sum(C.numberOfActions) AS SumOfnumberOfActions
FROM (Contract.FPDS as C
       LEFT JOIN Contractor.DunsnumberToParentContractorHistory as Dunsnumber
              ON (C.DUNSNumber=Dunsnumber.DUNSNUMBER) 
                     AND (C.fiscal_year=Dunsnumber.FiscalYear)) 
       LEFT JOIN Contractor.ParentContractor as Parent
              ON Dunsnumber.ParentID=Parent.ParentID
		LEFT JOIN FPDSTypeTable.AgencyID AS Agency
		ON C.AgencyID=Agency.AgencyID
	LEFT JOIN FPDSTypeTable.ProductOrServiceCode AS PSC
		ON C.productorservicecode=PSC.ProductOrServiceCode
	LEFT JOIN FPDSTypeTable.Country3lettercode as PlaceCountryCode
		ON C.placeofperformancecountrycode=PlaceCountryCode.Country3LetterCode
	LEFT JOIN FPDSTypeTable.Country3lettercode as OriginCountryCode
		ON C.countryoforigin=OriginCountryCode.Country3LetterCode
	LEFT JOIN FPDSTypeTable.vendorcountrycode as VendorCountryCodePartial
		ON C.vendorcountrycode=VendorCountryCodePartial.vendorcountrycode
	LEFT JOIN FPDSTypeTable.Country3lettercode as VendorCountryCode
		ON vendorcountrycode.Country3LetterCode=VendorCountryCodePartial.Country3LetterCode
	LEFT JOIN ProductOrServiceCode.ServicesCategory As Scat
		ON Scat.ServicesCategory = PSC.ServicesCategory
	LEFT OUTER JOIN Contractor.DunsnumbertoParentContractorHistory as DUNS
		ON C.fiscal_year = DUNS.FiscalYear 
		AND C.DUNSNumber = DUNS.DUNSNUMBER
	left outer join FPDSTypeTable.placeofmanufacture as PoM
		on c.placeofmanufacture=pom.placeofmanufacture
GROUP BY C.fiscal_year
, Parent.jointventure
, iif(parent.parentid is null
	,dunsnumber.StandardizedTopContractor
	,NULL)
, iif(parent.parentid is null or
	parent.firstyear>c.fiscal_year or
	parent.mergeryear<=c.fiscal_year,1,0) 
, Parent.UnknownCompany
, Parent.Top100Federal
, isnull(parent.parentid,C.dunsnumber) 
, c.fundedbyforeignentity
,c.isforeignownedandlocated
,c.isforeigngovernment
,c.isinternationalorganization
,c.organizationaltype
,PlaceCountryCode.IsInternational 
,PlaceCountryCode.Country3LetterCodeText
,OriginCountryCode.IsInternational
,OriginCountryCode.Country3LetterCodeText 
,VendorCountryCode.IsInternational
,VendorCountryCode.Country3LetterCodeText
,pom.placeofmanufactureText
,c.vendorcountrycode
, parent.parentid


)  as C
		LEFT JOIN Contractor.ParentContractorNameHistory as PCN
			ON C.ParentID = PCN.ParentID
			AND C.Fiscal_Year = PCN.FiscalYear;
	










GO


