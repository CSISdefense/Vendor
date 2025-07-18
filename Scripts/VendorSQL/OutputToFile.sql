USE CSIS360
GO


--For instructions see https://github.com/CSISdefense/DIIGsql/blob/master/Doc/Output_Large_Dataset.md

SET QUERY_GOVERNOR_COST_LIMIT 0
--1h31 m
--2h15m at 47%
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
  select  isnull(trim(descriptionofcontractrequirement),'Unlabled'),PlatformPortfolioRemote,sum(obligatedamount) as obligatedamount
  from contract.FPDSpartial
  where fiscal_year >= 2000
  group by isnull(trim(descriptionofcontractrequirement),'Unlabled'),PlatformPortfolioRemote
  

--3H11M TO reach 28% for entiredatabase ~30m for one incomplete
--54 % at 14h15m (with parallel runs happenign)
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
select fiscal_year, contract_transaction_unique_key, csistransactionid, last_modified_date
from contract.fpds
where fiscal_year in (2023)

--Not yet run
--9m53s for 2.5 m rows
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
select contract_transaction_unique_key, last_modified_date,USAspending_file_name
from errorlogging.fpdsstage1

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
  select  f.descriptionofcontractrequirement,f.obligatedamount,f.csistransactionid,ctid.csiscontractid
  from contract.FPDS f
  left outer join contract.csistransactionid ctid
  on f.csistransactionid=ctid.csistransactionid
  where f.fiscal_year >= 2000 and f.obligatedamount>=5000000
  
  
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
  SELECT [recipient_uei]
      ,[Fiscal_Year]
      --,[ParentID]
      --,[StandardizedTopContractor]
      --,[TopVendorNameTotalAmount]
      --,[Recipient_Parent_UEI]
      --,[Recipient_Parent_UEIFirstDate]
      --,[Dunsnumber]
      --,[ParentDUNSnumber]
      --,[HeadquarterCode]
      --,[Cage]
      --,[ObligatedAmount]
      --,[fed_funding_amount]
      --,[topISO3countrycode]
      --,[topISO3countrytotalamount]
      --,[MaxOfCAUobligatedAmount]
      --,[AnyIsSmall]
      --,[AlwaysIsSmall]
      --,[ObligatedAmountIsSmall]
      --,[IsOnePercentPlusSmall]
      --,[EntitySizeCode]
      --,[IsEntityAbove1990constantReportingThreshold]
      --,[IsEntityAbove2016constantReportingThreshold]
      --,[IsEntityAbove2018constant10ThousandThreshold]
      --,[IsEntityAbove2016constantOneMillionThreshold]
      --,[AnyEntityUSplaceOfPerformance]
      --,[AnyEntityForeignPlaceOfPerformance]
      --,[ChildCount]
      --,[IsPresent]
      --,[CSISmodifiedDate]
      --,[CSIScreateddate]
      --,[CSISmodifiedBy]
      ,[TopDunsnumber]
      ,[TopDunsnumberAmount]
      ,[TotalAmount]
      ,[TopDunsnumberCount]
      ,[TotalCount]
  FROM [Vendor].[Recipient_UEIhistory]

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


--2478175
--2j46m with errors
--942,792 rows
--3h30m
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
select  ProductOrServiceCode
,ContractingCustomer
,platformportfolio
,[claimantprogramcode]
,ProjectID
,principalnaicscode
,PricingUCA
,costaccountingstandardsclause
,costorpricingdata
,fiscal_year
,sum(obligatedamount) as obligatedamount 
from  contract.FPDSpartial
where  ContractingCustomer='Defense'
group by ProductOrServiceCode
,ContractingCustomer
,platformportfolio
,[claimantprogramcode]
,ProjectID
,principalnaicscode
,ProductOrServiceCode
,PricingUCA
,costaccountingstandardsclause
,costorpricingdata
,fiscal_year


--2478175
--2j46m with errors
--942,792 rows
--3h30m
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

--2h41m 5,5390,957 rows
SET QUERY_GOVERNOR_COST_LIMIT 0
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
Exec [Economic].[SP_NAICSprodservNonTraditionalHistory]
	@customer='Defense',
	@startfiscalyear=2007
	

--Australia 3h36m
	SET QUERY_GOVERNOR_COST_LIMIT 0
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
Exec [Location].[SP_CountryDetail]
 	@countryISOalpha3='AUS'

--5h53m 4,517,133 rows
--6h19m 4,517,133 rows
		SET QUERY_GOVERNOR_COST_LIMIT 0
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
Exec [Location].[SP_CountryDetail]
@countryISOalpha3=NULL

	SET QUERY_GOVERNOR_COST_LIMIT 0
--11h57, got cocky and tried all space companies plus CAU. Failed.
--2h35m 2m620 rows. We could probably aggregate this to the CAU level easily enoough.
--4h03m 4717 w/ boeing and is traditional
--4h33m 20,453 rows.
--4h53m 75,805 rows
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
exec ProductOrServiceCode.SP_SpaceDetail
@customer=NULL


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
exec Summary.SP_SoftwareDetail
@customer=NULL
--Fails at 5h44m
--9h06m
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
exec Summary.SP_JADC2detail
@customer=NULL

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--8H29m
exec Summary.SP_SoftwareC2Detail
@customer='Defense'


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--1h03m 2,939,179 rows
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
	  ,AnyDefenseCustomer
      ,[ObligatedAmount]
	  ,ObligatedAmountIsSmall
      ,[NumberOfActions]
      ,[Top100Federal]
      ,[Top6]
      ,[UnknownCompany]
  FROM [Vendor].[EntityIDhistory]
  --where fiscal_year>=2000

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;  
  --15m but probably not necessary
  select [EntityID]
  ,1 as AnyDefenseCustomer
  ,fiscal_year 
  from contract.fpdspartial
  where contractingcustomer='Defense' and fiscal_year>=2000
  group by fiscal_year, [EntityID]

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

<<<<<<< Updated upstream
--6h58m
--7h11m
--103,313 rows
=======
>>>>>>> Stashed changes
SET QUERY_GOVERNOR_COST_LIMIT 0
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
EXEC	[Contract].[SP_PBLcompetitionContractSizeHistorySubCustomer]
		@Customer = N'Defense'





SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--3h38mm
--2h12m
--8h58m 15,656,547 rows
--10h53m 15,871,392 rows, 3 cores.
--11h05m 15,872,583 rows. Erros
--2h45m; 16,497,747 (new desktop?)
--2h26m; 17,095,081 rows
--2h28m; 17,102,058 rows
--3h31m:18,864,534 rows 4/19/2023
--4h24m:18,964,297 rows
--4h32m: 20,088,352 rows 6/08/2025
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
 EXEC [Location].[SP_ProdServPlatformAgencyPlaceOriginVendor]
 @customer=NULL
 
 SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--2h12
--2h39m 12,547,505 rows
 EXEC 
 [Location].SP_ProdServPlatformAgencyCongressionalDistrict
  @customer='Defense'
 
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
--1h21m
EXEC	@return_value = Contract.SP_ContractUnmodifiedScope
		@IsDefense = NULL


		SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--1h25m
DECLARE	@return_value int
EXEC	@return_value = Contract.SP_ContractCeilingBreachCustomer
		@Customer = NULL

		SET ANSI_WARNINGS OFF;
		--1h15m
SET NOCOUNT ON;
DECLARE	@return_value int
EXEC	@return_value = Contract.SP_ContractUnmodifiedCompetitionvehicleCustomer
		@Customer = NULL




		SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--1h28m
DECLARE	@return_value int
EXEC	@return_value = Contract.SP_ContractCompetitionVehicleCustomer
		@IsDefense = NULL


		SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--2h35	m This is probably dependent on automated contract update runs.
select * from economic.[ProdServPlatformNAICS]

			SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--0h05m This is probably dependent on automated contract update runs.
DECLARE	@return_value int
EXEC	@return_value = Contract.SP_ContractTopPSCofficeNAICS
		@IsDefense = NULL

			SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--1h24m
DECLARE	@return_value int
EXEC	@return_value = Contract.SP_ContractLocationCustomer
		@IsDefense = NULL

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
DECLARE	@return_value int
EXEC	@return_value = Contract.SP_ContractPricingCustomer
		@IsDefense = NULL
					

--Do we really need both of these?
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
DECLARE	@return_value int
EXEC	@return_value = Contract.SP_ContractPricingUCA
		@IsDefense = NULL



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = Contract.SP_ContractBucketPlatformCustomer
		@Customer = NULL
		,@StartFiscalYear = 2000
		
		
--SELECT	'Return Value' = @return_value




--6h26m 15,328,597 rows for federal
--7h30m or so for defense only after postgres

SET QUERY_GOVERNOR_COST_LIMIT 0
--7,833,306 rows; 9h39m. (Finished right when I got home, which seems odd. One or more errors).
--8,357,821 rows r9h04m 
--6,127,978 rows 8h59m.  I believe reduced size due to switch to fiscal_quarter_YTD
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
EXEC	Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength
		@Customer = 'Defense',-- [Summary].[SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer]
		@SubCustomer = NULL,
		@PlatformPortfolio =NULL

SET QUERY_GOVERNOR_COST_LIMIT 0
--7,833,306 rows; 9h39m. (Finished right when I got home, which seems odd. One or more errors).
--8,357,821 rows r9h04m 
--6,127,978 rows 8h59m.  I believe reduced size due to switch to fiscal_quarter_YTD
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
EXEC	Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength
		@Customer = NULL, --'Defense',-- [Summary].[SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer]
		@SubCustomer = NULL,
		@PlatformPortfolio =NULL


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
DECLARE	@return_value int

--4h48m
--2h32m (with 5 cores)
--2h58m
--8h55m 1m rows (5 cores)
--3h21m 1637944 rows (with all national interest action codes

set QUERY_GOVERNOR_COST_LIMIT  0
--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	 Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense',-- [Summary].[SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer]
		@SubCustomer = NULL,
		@PlatformPortfolio =NULL,
		@StartFiscal_Year=NULL
		

		
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--Note we store this over in FMS repo, not vendor repo
--12h23m w / 4,582,038 rows
--10h49m
--No plan shown until the very end.
--1023 completed with errors 4,532,322
--4h41 4,656,745
--6h13m 10,891,220 post FPDS.partial
--6h39m 11,007,974 post UEI implementation.
--5h11m 11,343,896 more vendor classifications and 
--7h46m 11,391,836 after more labeling.
EXEC	budget.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerFMS
		@Customer = NULL
		

		

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = [Vendor].SP_EntityIDhistoryCalendar
		@Customer = 'Defense'
		


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = [Contract].[SP_UnmodifiedScopeHistory]
		@IsDefense =1
		
--SELECT	'Return Value' = @return_value

--1h29m 2 cores 35,347,828 rows
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = [Contract].[SP_ContractDefenseSubCustomer]

		
--SELECT	'Return Value' = @return_value




SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--0h56m
DECLARE	@return_value int

EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = [Contract].[SP_ContractCompetitionVehicleCustomer]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int
--1h41me
--3h16m (w/ error) with FPDS partial 
--1h306 m 2,391k
EXEC	@return_value = [Vendor].[SP_EntityIDhistoryPlatform]
		@Customer = 'Defense'


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int
--SELECT	'Return Value' = @return_value
EXEC	@return_value = [Vendor].[sp_EntityCountHistoryCustomer]
		@Customer =NULL

		SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

--1h32m
DECLARE	@return_value int
--SELECT	'Return Value' = @return_value
EXEC	@return_value = [Vendor].SP_TopVendorHistoryCustomer
		@Customer ='Defense',
		@IsService=NULL
		
	

	
		SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--1h22m
--1h37m one or more errors, with simplearea included
		--Completing with errors, may need a reboot. No errors this time!
DECLARE	@return_value int
--SELECT	'Return Value' = @return_value
EXEC	@return_value = [Vendor].SP_TopVendorHistoryPlatformUAVsubCustomer
		@IsDefense =NULL
	

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

EXEC	@return_value = Vendor.sp_EntityCountHistoryCustomer
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--9h20m
DECLARE	@return_value int

EXEC	@return_value = Vendor.sp_EntityCountHistoryPlatformCustomer
		@Customer = 'Defense'
	
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--9h20m
DECLARE	@return_value int
EXEC Summary.SP_ContractDatesSubCustomerPlatform
	@Customer = NULL --'Defense'

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--9h20m
DECLARE	@return_value int

EXEC	@return_value = Vendor.sp_EntityCountHistoryPlatformRemoteCustomer
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value

SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = Vendor.sp_EntityCountHistorySubCustomer
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value





SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

EXEC	@return_value = Vendor.sp_EntityCountHistoryPlatformSubCustomer
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
,fiscal_quarter
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
,fiscal_quarter
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



--Defense Contract Dataset
--2h49m 38.5m rows
EXEC	
 [Contract].[SP_ContractSampleCriteriaDetailsCustomer]
		@IsDefense = 1
		

--SELECT	'Return Value' = @return_value
--1,122,215
--38s
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
  SELECT  [UEI]
      ,[Fiscal_Year]
      ,[ParentID]
      ,[StandardizedTopContractor]
      --,[TopVendorNameTotalAmount]
      ,[Parent_UEI]
      ,[Parent_UEIFirstDate]
      --,[Dunsnumber]
      --,[ParentDUNSnumber]
      --,[HeadquarterCode]
      --,[Cage]
      ,[ObligatedAmount]
      --,[fed_funding_amount]
      ,[topISO3countrycode]
      --,[topISO3countrytotalamount]
      ,[MaxOfCAUobligatedAmount]
      ,[AnyIsSmall]
      ,[AlwaysIsSmall]
      ,[ObligatedAmountIsSmall]
      ,[IsOnePercentPlusSmall]
      ,[EntitySizeCode]      
      --,[ChildCount]
      --,[IsPresent]
      --,[CSISmodifiedDate]
      --,[CSIScreateddate]
      --,[CSISmodifiedBy]
      --,[TopDunsnumber]
      --,[TopDunsnumberAmount]
      --,[TotalAmount]
      --,[TopDunsnumberCount]
      --,[TotalCount]
	  ,AnyDefense
	  ,DefenseObligated
	  ,[IsEntityAbove1990constantMTAthreshold]
      ,[IsEntityAbove2016constantMTAthreshold]
      ,[IsEntityAbove2018constantMTAthreshold]
      ,[IsEntityAbove2016constantArbitrary1000k]
      ,[IsEntityAbove2018constantSimplifedAcquisition250kThreshold]
      ,[IsEntityAbove2018constantCommercialItem7500k]
      ,[IsEntityAbove2018constantCostAccounting2000kThreshold]
	  ,[AnyEntityUSplaceOfPerformance]
      ,[AnyEntityForeignPlaceOfPerformance]
      ,[IsEntityTraditional]
  FROM [Vendor].[UEIhistory]
   where IsPresent=1


   
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
   SELECT  [Parent_UEI]
      ,[Fiscal_Year]
      ,[ObligatedAmount]
      ,[MaxOfCAUobligatedAmount]
      ,[AnyIsSmall]
      ,[AlwaysIsSmall]
      ,[ObligatedAmountIsSmall]
      ,[IsOnePercentPlusSmall]
      ,[AnyDefense]
      ,[DefenseObligated]
      ,[IsEntityAbove1990constantMTAthreshold]
      ,[IsEntityAbove2016constantMTAthreshold]
      ,[IsEntityAbove2018constantMTAthreshold]
      ,[IsEntityAbove2016constantArbitrary1000k]
      ,[IsEntityAbove2018constantSimplifedAcquisition250kThreshold]
      ,[IsEntityAbove2018constantCommercialItem7500k]
      ,[IsEntityAbove2018constantCostAccounting2000kThreshold]
      ,[AnyEntityUSplaceOfPerformance]
      ,[AnyEntityForeignPlaceOfPerformance]
	  ,AnyCASclause
	  ,AlwaysCAUisCASexemptOrWaived
      ,[IsEntityTraditional]
  FROM [Vendor].[Parent_UEIhistory]


  --18m13s
  --16m30s after changing over to a new parent rollup approach.
  SET QUERY_GOVERNOR_COST_LIMIT 0
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
  SELECT fiscal_year
      ,[IsAbove2018constantCommercialItem7500k]
      ,[IsAbove2018constantCostAccounting2000kThreshold]
      ,[CostAccountingStandardsClause]
      ,[costorpricingdata]
      ,[IsSealedBid]
      ,[IsCommercialitemacquisitionprocedures]
      ,[isforeigngovernment]
      ,[FixedPriceEffective]
      ,[AlwaysCAUisCASexemptOrWaived]
	  ,contractingofficerbusinesssizedetermination
	  	,IsEntityTraditional 
		,IsEntityAbove2018constantCommercialItem7500k 
		,AlwaysIsSmall
		,AnyCASclause
	  ,count([contract_award_unique_key]) as n_cau
	  		,sum(obligatedamount) as obligatedamount		
  FROM [Contract].[DefenseCostAccountingStandardCrosscheck]
  group by fiscal_year
      ,[IsAbove2018constantCommercialItem7500k]
      ,[IsAbove2018constantCostAccounting2000kThreshold]
      ,[CostAccountingStandardsClause]
      ,[costorpricingdata]
      ,[IsSealedBid]
      ,[IsCommercialitemacquisitionprocedures]
      ,[isforeigngovernment]
      ,[FixedPriceEffective]
      ,[AlwaysCAUisCASexemptOrWaived]
	  ,contractingofficerbusinesssizedetermination
	  	,IsEntityTraditional 
		,IsEntityAbove2018constantCommercialItem7500k 
		,AlwaysIsSmall
		,AnyCASclause

--12m
		SELECT fiscal_year
      ,[IsAbove2018constantCommercialItem7500k]
      ,[IsAbove2018constantCostAccounting2000kThreshold]
      ,[CostAccountingStandardsClause]
      ,[costorpricingdata]
      ,[IsSealedBid]
      ,[IsCommercialitemacquisitionprocedures]
      ,[isforeigngovernment]
      ,[FixedPriceEffective]
      ,[AlwaysCAUisCASexemptOrWaived]
	  ,contractingofficerbusinesssizedetermination
	  	,IsEntityTraditional 
		,IsEntityAbove2018constantCommercialItem7500k 
		,AlwaysIsSmall
	  ,[contract_award_unique_key]
	  		,obligatedamount
			,contract_award_unique_key
			,recipient_uei
			,recipient_parent_uei
			,parent_uei
			,AnyCASclause
  FROM Contract.DefenseCostAccountingStandardCrosscheck
  where [CostAccountingStandardsClause]='Y' and (IsEntityTraditional=0 or AnyCASclause=0)

  select *
   ,case 
		when nullif([Parent_UEI],'') is not null
		then nullif([Parent_UEI],'')
		when nullif([topParent_UEI],'') is not null and topParent_UEIcount*2 >= TotalCount and abs(topParent_UEIamount)*2 >= abs(TotalAmount)  
		then nullif([topParent_UEI],'')
		else [UEI]
		end as Parent_UEI
  from vendor.UEIhistory
  where 'F7NGF6NJFB96' in (uei,parent_uei) or 'NANFEH9AAH43' in (uei,parent_uei)


select 
  [recipient_uei] ,
	[fiscal_year] ,
[dunsnumber],[parentdunsnumber],[headquartercode],[CAGE],[recipient_parent_uei],[obligatedamount],[vendorcountrycode],[vendorname],[contractingofficerbusinesssizedetermination],[contract_award_unique_key],[manufacturingorganizationtype],[isforeigngovernment],[costaccountingstandardsclause]
from contract.fpds
where recipient_uei in('F7NGF6NJFB96','NANFEH9AAH43') or recipient_parent_uei in ('F7NGF6NJFB96','NANFEH9AAH43')


select	[contract_award_unique_key] ,
[fiscal_year],[obligatedamount],[baseandexercisedoptionsvalue],[baseandalloptionsvalue],[recipient_uei],[recipient_parent_uei],[dunsnumber],[parentdunsnumber],[contractingofficerbusinesssizedetermination],[placeofperformancecountrycode],[pop_state_code],[costaccountingstandardsclause],[costorpricingdata],[total_dollars_obligated],[current_total_value_of_award],[potential_total_value_of_award],[solicitationprocedures],[placeofmanufacture],[isforeigngovernment],[typeofcontractpricing],[extentcompeted],[statutoryexceptiontofairopportunity],[numberofoffersreceived],[signeddate],[award_or_idv_flag],[currentcompletiondate],[ultimatecompletiondate],[lastdatetoorder],[multiyearcontract],[commercialitemacquisitionprocedures]
from contract.fpds
where contract_award_unique_key='CONT_AWD_0001_9700_DACW4101D0001_9700'

select *

from vendor.Parent_UEIhistory
where Parent_UEI  in('F7NGF6NJFB96','NANFEH9AAH43')

select *
from contract.contract_award_unique_key
where contract_award_unique_key='CONT_AWD_0001_9700_DACW4101D0001_9700'

--Find biggest grandparent CAUs
SET QUERY_GOVERNOR_COST_LIMIT 0
select top 100 coalesce(pcau.parent_contract_award_unique_key,cau.parent_contract_award_unique_key, f.contract_award_unique_key) as 
	grand_parent_contract_award_unique_key
,count(contract_transaction_unique_key) n
from contract.FPDS f
inner join contract.contract_award_unique_key cau
on f.contract_award_unique_key=cau.contract_award_unique_key
left outer join contract.contract_award_unique_key pcau
on cau.parent_contract_award_unique_key=pcau.contract_award_unique_key
inner join FPDSTypeTable.AgencyID a
on cau.AgencyID=a.AgencyID
--where fiscal_year>=2013
group by coalesce(pcau.parent_contract_award_unique_key,cau.parent_contract_award_unique_key, f.contract_award_unique_key)
order by count(contract_transaction_unique_key) desc