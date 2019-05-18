/****** Object:  StoredProcedure [Location].[SP_DefenseCompetitionPoPstateHistoryBucketPlatformSubCustomer]    Script Date: 5/14/2018 12:46:34 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- Batch submitted through debugger: SQLQuery2.sql|9|0|C:\Users\Jarcher\AppData\Local\Temp\~vs9033.sql


ALTER PROCEDURE [Location].[SP_DefenseCompetitionPoPstateHistoryBucketPlatformSubCustomer]



AS

--IF (@Customer is not null) --Begin sub path where competition data is returned for one Platform Portfolio
--	--BEGIN
--		--Copy the start of your query here
--		SELECT S.fiscal_year
--			,S.Customer
--			,S.SubCustomer
--			,S.ServicesCategory
--			,S.PlatformPortfolio
--			,S.Size
--			,S.CompetitionClassification
--			,S.ClassifyNumberOfOffers
--			,sum(S.SumOfobligatedAmount) as SumOfobligatedAmount
--			,sum(S.SumOfnumberOfActions) as SumOfnumberOfActions
--		FROM Vendor.CompetitionVendorSizeHistoryBucketSubCustomerClassification as S
--		--Here's the where clause for @ServicesOnly is null and Customer is not null
		
--		--Copy the end of your query here
--		GROUP BY S.fiscal_year
--			,S.Customer
--			,S.SubCustomer
--			,S.ServicesCategory
--			,S.PlatformPortfolio
--			,S.Size
--			,S.CompetitionClassification
--			,S.ClassifyNumberOfOffers
--		--End of your query
--		END
--ELSE --Begin sub path where data for all platform Portfolio is returned for One SubCustomer	
--	BEGIN
		--Copy the start of your query here
		SELECT S.fiscal_year
			,S.Customer
			--,S.SubCustomer
			,S.StateCode as PoPStatecode 
			,S.Simple
			,S.ProductOrServiceArea
			--,s.TypeofContractPricingtext
			--,s.VehicleClassification
			,S.PlatformPortfolio
			--,S.Size
			,s.NumberOfOffersReceived
			,S.CompetitionClassification
			,S.ClassifyNumberOfOffers
			,sum(S.SumOfobligatedAmount) as SumOfobligatedAmount
			,sum(S.SumOfnumberOfActions) as SumOfnumberOfActions
		FROM vendor.CompetitionVendorSizeHistoryBucketSubCustomerClassification as S
		inner join fpdstypetable.statecode sc
			on s.statecode=sc.statecode
		--Here's the where clause for @ServicesOnly is null and Customer is not null
		WHERE S.Customer ='Defense' and fiscal_year>=2000 and sc.usa=1
		
		--Copy the end of your query here
		GROUP BY S.fiscal_year
			,S.Customer
			--,S.SubCustomer
			,S.statecode
			,s.NumberOfOffersReceived
			--,s.TypeofContractPricingtext
			--,s.VehicleClassification
			,S.Simple
			,S.ProductOrServiceArea
			,S.PlatformPortfolio
			--,S.Size
			,S.CompetitionClassification
			,S.ClassifyNumberOfOffers
		--End of your query
	--END
GO


