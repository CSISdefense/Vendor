/****** Object:  StoredProcedure [Vendor].[sp_EntityCountHistoryPlatformSubCustomer]    Script Date: 4/17/2018 10:39:32 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





ALTER PROCEDURE [Vendor].[sp_EntityCountHistoryPlatformSubCustomer]
@Customer VARCHAR(255)

AS

	IF (@Customer is not null) --Begin sub path where only services only one Customer will be returned
	BEGIN
		--Copy the start of your query here
			SELECT [fiscal_year]
			,Customer
			,SubCustomer
			,PlatformPortfolio
			,ent.EntitySizeCode      
			,esc.EntitySizeText
			,EntityCategory
		  ,AnyEntityUSplaceOfPerformance
		  ,AnyEntityForeignPlaceOfPerformance
		  ,IsEntityAbove2016constantOneMillionThreshold
		  ,IsEntityAbove1990constantReportingThreshold
		  ,IsEntityAbove2016constantReportingThreshold
		  ,count(distinct [EntityID]) [EntityCount]
		  ,count(distinct [AllContractor]) as [AllContractorCount]
		  ,sum([numberOfActions]) as numberOfActions
		  ,sum([ObligatedAmount]) as ObligatedAmount
	  FROM [Vendor].EntitySizeHistoryBucketPlatformSubCustomer ent
	  left outer join Vendor.EntitySizeCode esc
	  on esc.EntitySizeCode=ent.entitysizecode
	  --Here's the where clause for @ServicesOnly=1 and Customer is not null
		WHERE ent.Customer=@Customer
		--Copy the end of your query here
	  group by 	  [fiscal_year]
	  			,Customer
				,SubCustomer
				,PlatformPortfolio
	  ,ent.EntitySizeCode
	  ,esc.EntitySizeText
	  ,EntityCategory
	  ,AnyEntityUSplaceOfPerformance
	  ,AnyEntityForeignPlaceOfPerformance
	  		  ,IsEntityAbove2016constantOneMillionThreshold
	  ,IsEntityAbove1990constantReportingThreshold
		  ,IsEntityAbove2016constantReportingThreshold
		--End of your query
		END
	ELSE --Begin sub path where only services but all Customers will be returned
		BEGIN
		--Copy the start of your query here
			SELECT [fiscal_year]
			,ent.Customer
			,SubCustomer
			,PlatformPortfolio
			,ent.EntitySizeCode      
			,esc.EntitySizeText
			,EntityCategory
		  ,AnyEntityUSplaceOfPerformance
		  ,AnyEntityForeignPlaceOfPerformance
		  		  ,IsEntityAbove2016constantOneMillionThreshold
		  ,IsEntityAbove1990constantReportingThreshold
		  ,IsEntityAbove2016constantReportingThreshold
		  ,count(distinct [EntityID]) [EntityCounty]
		  ,count(distinct [AllContractor]) as [AllContractorCount]
		  ,sum([numberOfActions]) as numberOfActions
		  ,sum([ObligatedAmount]) as ObligatedAmount
	  FROM [Vendor].EntitySizeHistoryBucketPlatformSubCustomer ent
	  left outer join Vendor.EntitySizeCode esc
	  on esc.EntitySizeCode=ent.entitysizecode
	  group by 	  [fiscal_year]
	  			,Customer
				,SubCustomer
				,PlatformPortfolio
	  ,ent.EntitySizeCode
	  ,esc.EntitySizeText
	  ,EntityCategory
	  ,AnyEntityUSplaceOfPerformance
	  ,AnyEntityForeignPlaceOfPerformance
	  		  ,IsEntityAbove2016constantOneMillionThreshold
	  ,IsEntityAbove1990constantReportingThreshold
		  ,IsEntityAbove2016constantReportingThreshold
		--End of your query
		END

	










GO


