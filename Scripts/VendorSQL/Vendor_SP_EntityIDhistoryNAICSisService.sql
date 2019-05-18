/****** Object:  StoredProcedure [Vendor].[sp_EntityIDhistoryNAICS]    Script Date: 12/4/2018 12:33:52 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



Alter PROCEDURE [Vendor].[sp_EntityIDhistoryNAICSisService]

@Customer VARCHAR(255)

AS

IF (@Customer is not null) --Begin sub path where all product and services but only one Customer will be returned
	BEGIN
		--Copy the start of your query here
		SELECT 
			CalendarYear
				,c.principalNAICScode
				,c.principalnaicscodeText
				,c.IsService
				,C.EntityID
				,c.Small
				,Sum(C.obligatedAmount) AS obligatedAmount
			,Sum(C.numberOfActions) AS numberOfActions
			FROM [Vendor].[VendorHistoryNaicsPlatformSubCustomer] as C
		--Here's the where clause for @IsService is null and Customer is not null
		WHERE C.Customer=@Customer 
		--Copy the end of your query here
		GROUP BY 
				CalendarYear
				,c.Customer
				,c.principalNAICScode
				,c.principalnaicscodeText
								,c.IsService
				,C.EntityID
				,c.Small
		order by
				c.CalendarYear
				,C.Customer
				,c.principalNAICScode
				,Sum(C.obligatedAmount) desc
		--End of your query
		END
ELSE --Begin sub path where all products and services amd all Customers will be returned
		BEGIN
		--Copy the start of your query here
		SELECT 
			 CalendarYear
				,c.principalNAICScode
				,c.principalnaicscodeText
								,c.IsService
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
			CalendarYear 
				,c.principalNAICScode
				,c.principalnaicscodeText
								,c.IsService
				,C.EntityID
				,C.parentid
				,C.ContractorDisplayName
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag
				,c.Small
		order by
				c.CalendarYear
				,c.principalNAICScode

				,Sum(C.obligatedAmount) desc
		--End of your query
		END
GO


