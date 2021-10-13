

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

select ProductOrServiceCode
,ProductOrServiceCodeText
,Simple
,ProductServiceOrRnDarea
,platformportfolio
,[claimantprogramcode]
,ClaimantProgramCodeText
,ProjectAbbreviation
,ProjectName
,EntityID
,EntityText
,EntityVendorName
--,f.[principalnaicscode]
--,f.principalnaicscodeText
,Contractingcustomer
,contractingofficeagencyid
,ContractingAgencyText
,fiscal_year
,PlaceCountryText
,fundedbyforeignentity
 --Pricing
	     ,[TypeOfContractPricing5Category]
	  	  ,PricingFee
		  , PricingUCA
		  ,VehicleClassification		  
,sum(obligatedamount) as obligatedamount
from contract.[FPDSclassification] f
--where Contractingcustomer='Defense'-- and  ProductOrServiceArea in ('Electronics & Communications'  , 'ICT')
group by  productorservicecode
,ServicesCategory
,IsService
,Simple
,ProductServiceOrRnDarea
,platformportfolio
,[claimantprogramcode]
,ClaimantProgramCodeText
,ProjectAbbreviation
,ProjectName
,EntityID
,EntityText
,EntityVendorName
,ProductOrServiceCode
,ProductOrServiceCodeText
,Contractingcustomer
,contractingofficeagencyid
,ContractingAgencyText
,fiscal_year
,PlaceCountryText
,fundedbyforeignentity
	     ,[TypeOfContractPricing5Category]
	  	  ,PricingFee
		  , PricingUCA
		  ,VehicleClassification



		
select ProductOrServiceCode
,ProductOrServiceCodeText
,Simple
,ProductServiceOrRnDarea
,platformportfolio
,[claimantprogramcode]
,ClaimantProgramCodeText
,ProjectAbbreviation
,ProjectName
,ProjectPlatform
--,EntityID
--,EntityText
--,EntityVendorName
--,f.[principalnaicscode]
--,f.principalnaicscodeText
,Contractingcustomer
,contractingofficeagencyid
,ContractingAgencyText
,fiscal_year
--,PlaceCountryText
--,fundedbyforeignentity
 --Pricing
	   --  ,[TypeOfContractPricing5Category]
	  	--  ,PricingFee
		  --, PricingUCA
		  --,VehicleClassification		  
,sum(obligatedamount) as obligatedamount
from contract.[FPDSclassification] f
--where Contractingcustomer='Defense'-- and  ProductOrServiceArea in ('Electronics & Communications'  , 'ICT')
group by  productorservicecode
,ServicesCategory
,IsService
,Simple
,ProductServiceOrRnDarea
,platformportfolio
,[claimantprogramcode]
,ClaimantProgramCodeText
,ProjectAbbreviation
,ProjectName
,ProjectPlatform
--,EntityID
--,EntityText
--,EntityVendorName
,ProductOrServiceCode
,ProductOrServiceCodeText
,Contractingcustomer
,contractingofficeagencyid
,ContractingAgencyText
,fiscal_year
--,PlaceCountryText
--,fundedbyforeignentity
--	     ,[TypeOfContractPricing5Category]
--	  	  ,PricingFee
--		  , PricingUCA
--		  ,VehicleClassification



--ProdServAgency
select ProductOrServiceCode
,ProductOrServiceCodeText
,Simple
,ProductServiceOrRnDarea
--,platformportfolio
--,[claimantprogramcode]
--,ClaimantProgramCodeText
--,ProjectAbbreviation
--,ProjectName
--,ProjectPlatform
--,EntityID
--,EntityText
--,EntityVendorName
--,f.[principalnaicscode]
--,f.principalnaicscodeText
,Contractingcustomer
,contractingofficeagencyid
,ContractingAgencyText
,fundingagency
,fundingsubagency
      ,f.[fundingrequestingagencyid]
	  ,FundingAgencyText
--,fundingagencytext
,fiscal_year
--,PlaceCountryText
--,fundedbyforeignentity
 --Pricing
	   --  ,[TypeOfContractPricing5Category]
	  	--  ,PricingFee
		  --, PricingUCA
		  --,VehicleClassification		  
,sum(obligatedamount) as obligatedamount
from contract.[FPDSclassification] f
where ProductOrServiceArea in ('Electronics & Communications'  , 'ICT') --Contractingcustomer='Defense'-- and  
group by  productorservicecode
,ServicesCategory
,IsService
,Simple
,ProductServiceOrRnDarea
--,platformportfolio
--,[claimantprogramcode]
--,ClaimantProgramCodeText
--,ProjectAbbreviation
--,ProjectName
--,ProjectPlatform
--,EntityID
--,EntityText
--,EntityVendorName
,ProductOrServiceCode
,ProductOrServiceCodeText
,Contractingcustomer
,contractingofficeagencyid
,ContractingAgencyText
,fiscal_year
,fundingagency
,fundingsubagency
      ,f.[fundingrequestingagencyid]
,fundingagencytext
--,PlaceCountryText
--,fundedbyforeignentity
--	     ,[TypeOfContractPricing5Category]
--	  	  ,PricingFee
--		  , PricingUCA
--		  ,VehicleClassification