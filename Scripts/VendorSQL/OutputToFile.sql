USE CSIS360
GO


--For instructions see https://github.com/CSISdefense/DIIGsql/blob/master/Doc/Output_Large_Dataset.md



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;


select fiscal_year
,fiscal_quarter
,ContractingCustomer
,sum(obligatedAmount) as obligatedAmount
from [Contract].[FPDSpartial]
where ContractingCustomer='Defense'
group by  fiscal_year
,fiscal_quarter
,ContractingCustomer
order by fiscal_year,fiscal_quarter



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
exec Summary.SP_SoftwareDetail
@customer=NULL

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--8H29m
exec Summary.SP_SoftwareC2Detail
@customer='Defense'


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
/****** Script for SelectTopNRows command from SSMS  ******/
SELECT  [EntityID]
      ,[fiscal_year]
      ,[EntityText]
      ,[EntityCategory]
      ,[EntitySizeCode]
      ,[IsEntityAbove2016constantOneMillionThreshold]
	  ,IsEntityAbove2018constant10ThousandThreshold	
      ,[IsEntityAbove2016constantReportingThreshold]
      ,[IsEntityAbove1990constantReportingThreshold]
      ,[AnyEntityUSplaceOfPerformance]
      ,[AnyEntityForeignPlaceOfPerformance]
      ,[ObligatedAmount]
	  ,ObligatedAmountIsSmall
      ,[NumberOfActions]
      ,[Top100Federal]
      ,[Top6]
      ,[UnknownCompany]
  FROM [Vendor].[EntityIDhistory]
  where fiscal_year>=2000
  

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

--This way took 5 minutes.
--gt250k_change_outliers.txt
select f.*, t.CSIScontractID
from contract.csistransactionid t
inner join contract.fpds f
on f.csistransactionid = t.csistransactionid
where f.CSIStransactionID in 
(select CSIStransactionID
from contract.csistransactionid ctid
where ctid.csiscontractid in (
1431340,
2966598,
7878880,
8157803,
8341560,
8567148,
10090818,
18671771,
18671780,
18671888,
18671981,
19005830,
24719937,
24807877,
24816950,
24905030,
24596370, 
1306182, 
18189486, 
3463800, 
23709649, 
23881549, 
18802894, 
8677136, 
18189315, 
27098370, 
1294337, 
3473194, 
18199664, 
8673962, 
24590177, 
2964273, 
24590159, 
9991092, 
18189308, 
8675166, 
8005655, 
18186731, 
15461175, 
16642987, 
18189518, 
61686173, 
8012194, 
8008914, 
27512038, 
18801607,
8496473, 
8496476
)
)

select *
from FPDSTypeTable.AgencyID
where agencyid in ('5300')



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--3h38mm
--2h12m
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
 EXEC [Location].[SP_ProdServPlatformAgencyPlaceOriginVendor]
 @customer=NULL

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Contract.SP_UnmodifiedScopeHistory
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@IsDefense = 1


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Contract.SP_ContractUnmodifiedScope
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@IsDefense = 1


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

--4h48m
--2h32m (with 5 cores)
--8h55m 1m rows (5 cores)
DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense',-- [Summary].[SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer]
		@SubCustomer = NULL,
		@PlatformPortfolio =NULL

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;


DECLARE	@return_value int
--11h25m  fail
--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerInternational
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense',
		@SubCustomer = NULL,
		@PlatformPortfolio =NULL

		

		
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int
--13h20m
--10h25m
--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [budget].[SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerFMS]
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = NULL -- 'Defense'


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [Vendor].SP_EntityIDhistoryCalendar
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [Contract].[SP_UnmodifiedScopeHistory]
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@IsDefense =1
		
--SELECT	'Return Value' = @return_value


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = Contract.SP_ContractBucketPlatformCustomer
		@Customer = NULL
		,@StartFiscalYear = 2000
		
		
--SELECT	'Return Value' = @return_value


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
--EXEC	@return_value = [Vendor].[SP_EntityIDhistoryNAICS]
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
EXEC	@return_value = [Contract].[SP_ContractCompetitionVehicleCustomer]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int
--1h41me
--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [Vendor].[SP_EntityIDhistoryPlatform]
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int
--SELECT	'Return Value' = @return_value
--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [Vendor].[sp_EntityCountHistoryCustomer]
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer =NULL

--1h32m
DECLARE	@return_value int
--SELECT	'Return Value' = @return_value
--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [Vendor].SP_TopVendorHistoryCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer ='Defense',
		@IsService=NULL
		

--SET ANSI_WARNINGS OFF;
--SET NOCOUNT ON;
--DECLARE	@return_value int

----EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
--EXEC	@return_value = [Vendor].[SP_EntityIDhistoryNAICS]
----EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
--		@Customer = NULL
--This resulted in a blank file!
--SELECT	'Return Value' = @return_value

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Vendor.sp_EntityCountHistoryCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--9h20m
DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Vendor.sp_EntityCountHistoryPlatformCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Vendor.sp_EntityCountHistorySubCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value





SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Vendor.sp_EntityCountHistoryPlatformSubCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value

==
		
--SELECT	'Return Value' = @return_value






SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

--DECLARE	@return_value int

EXEC	
 [Vendor].[SP_DunsnumberNewEntrants]
		@Customer = NULL,
		@IsSAMduns = NULL

--SELECT	'Return Value' = @return_value

GO





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



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

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
--		  ,VehicleClassificationpl