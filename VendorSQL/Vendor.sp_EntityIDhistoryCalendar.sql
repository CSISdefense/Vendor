/****** Object:  StoredProcedure [Vendor].[SP_EntityIDhistory]    Script Date: 8/2/2018 5:09:17 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [Vendor].[SP_EntityIDhistoryCalendar]

@Customer VARCHAR(255)

AS

IF (@Customer is not null) --Begin sub path where all product and services but only one Customer will be returned
	BEGIN
		--Copy the start of your query here
		SELECT 
			C.CalendarYear
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
				C.CalendarYear
				,c.Customer
				,C.EntityID
				,C.parentid
				,C.ContractorDisplayName
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag
				,c.Small
		order by
				c.CalendarYear
				,C.Customer
				,Sum(C.obligatedAmount) desc
		--End of your query
		END
ELSE --Begin sub path where all products and services amd all Customers will be returned
		BEGIN
		--Copy the start of your query here
		SELECT 
			C.CalendarYear
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
		--Copy the end of your query here
		GROUP BY 
			C.CalendarYear
				,C.EntityID
				,C.parentid
				,C.ContractorDisplayName
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag
				,c.Small
		order by
				c.CalendarYear
				,Sum(C.obligatedAmount) desc
		--End of your query
		END








































GO


