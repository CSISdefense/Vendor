

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

select ProductOrServiceCode
,ProductOrServiceCodeText
,Simple
,ProductServiceOrRnDarea
,platformportfolio
,f.[claimantprogramcode]
,f.ClaimantProgramCodeText
,f.ProjectAbbreviation
,f.ProjectName
--,f.[principalnaicscode]
--,f.principalnaicscodeText
,Contractingcustomer
,contractingofficeagencyid
,ContractingAgencyText
,fiscal_year
 --Pricing
	     ,[TypeOfContractPricing5Category]
	  	  ,PricingFee
		  , PricingUCA
		  ,VehicleClassification		  
,sum(obligatedamount) as obligatedamount
from contract.[FPDSclassification]
where Contractingcustomer='Defense'-- and  ProductOrServiceArea in ('Electronics & Communications'  , 'ICT')
group by  productorservicecode
,ServicesCategory
,IsService
,Simple
,ProductOrServiceArea
,ProductServiceOrRnDarea
,ProductOrServiceCode
,ProductOrServiceCodeText
,Contractingcustomer
,contractingofficeagencyid
,ContractingAgencyText
,fiscal_year
	     ,[TypeOfContractPricing5Category]
	  	  ,PricingFee
		  , PricingUCA
		  ,VehicleClassification