USE CSIS360
GO

/****** Object:  StoredProcedure [Vendor].[SP_TopVendorHistoryPlatformSubCustomer]    Script Date: 10/5/2017 9:43:57 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


ALTER PROCEDURE [Vendor].[SP_EntityIDhistoryNAICS]

@Customer VARCHAR(255)

AS

IF (@Customer is not null) --Begin sub path where all product and services but only one Customer will be returned
	BEGIN
		--Copy the start of your query here
		SELECT 
			C.fiscal_year
				,c.NAICS_Code
				,c.Industry_TEXT
				,C.EntityID
				,C.parentid
				,C.ContractorDisplayName
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag
				,c.Small
				,Sum(C.obligatedAmount) AS obligatedAmount
			,Sum(C.numberOfActions) AS numberOfActions
			,C.WarningFlag
			FROM [Vendor].[VendorHistoryNaicsPlatformSubCustomer] as C
		--Here's the where clause for @IsService is null and Customer is not null
		WHERE C.Customer=@Customer 
		--Copy the end of your query here
		GROUP BY 
				C.fiscal_year
				,c.Customer
				,c.NAICS_Code
				,c.Industry_TEXT
				,C.EntityID
				,C.parentid
				,C.ContractorDisplayName
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag
				,c.Small
		order by
				c.Fiscal_year
				,C.Customer
				,c.NAICS_Code
				,Sum(C.obligatedAmount) desc
		--End of your query
		END
ELSE --Begin sub path where all products and services amd all Customers will be returned
		BEGIN
		--Copy the start of your query here
		SELECT 
			C.fiscal_year
				,c.NAICS_Code
				,c.Industry_TEXT
				,C.EntityID
				,C.parentid
				,C.ContractorDisplayName
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag
				,c.Small
				,Sum(C.obligatedAmount) AS obligatedAmount
			,Sum(C.numberOfActions) AS numberOfActions
			FROM [Vendor].[VendorHistoryNaicsPlatformSubCustomer] as C
		--Here's the where clause for @IsService is null and Customer is not null
		WHERE C.Customer=@Customer 
		--Copy the end of your query here
		GROUP BY 
			C.fiscal_year
				,c.NAICS_Code
				,c.Industry_TEXT
				,C.EntityID
				,C.parentid
				,C.ContractorDisplayName
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag
				,c.Small
		order by
				c.Fiscal_year
				,c.NAICS_Code

				,Sum(C.obligatedAmount) desc
		--End of your query
		END








































GO


