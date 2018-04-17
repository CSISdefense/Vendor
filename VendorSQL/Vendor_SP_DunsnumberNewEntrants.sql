/****** Object:  StoredProcedure [Vendor].[SP_EntityIDhistory]    Script Date: 3/7/2018 12:51:45 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [Vendor].[SP_DunsnumberNewEntrants]

@Customer VARCHAR(255)

AS

IF (@Customer is not null) --Begin sub path where all product and services but only one Customer will be returned
	BEGIN
		--Copy the start of your query here
		SELECT 
			C.fiscal_year
			,C.dunsnumber
			,C.parentid
			,C.EntityID
			,max(c.SignedDate) as MaxOfSignedDate
			,Sum(C.obligatedAmount) AS obligatedAmount
			,Sum(C.numberOfActions) AS numberOfActions
			FROM [Vendor].[VendorHistoryNaicsPlatformSubCustomer] as C
		--Here's the where clause for @IsService is null and Customer is not null
		WHERE C.Customer=@Customer 
		--Copy the end of your query here
		GROUP BY 
			C.fiscal_year
			,C.dunsnumber
			,C.parentid
			,C.EntityID
		order by
			C.dunsnumber
			,c.Fiscal_year
				
		--End of your query
		END
ELSE --Begin sub path where all products and services amd all Customers will be returned
		BEGIN
		--Copy the start of your query here
		SELECT 
			C.fiscal_year
			,C.dunsnumber
			,C.parentid
			,C.EntityID
			,max(c.SignedDate) as MaxOfSignedDate
			,Sum(C.obligatedAmount) AS obligatedAmount
			,Sum(C.numberOfActions) AS numberOfActions
			FROM [Vendor].[VendorHistoryNaicsPlatformSubCustomer] as C
		--Copy the end of your query here
		GROUP BY 
			C.fiscal_year
			,C.dunsnumber
			,C.parentid
			,C.EntityID
		order by
			C.dunsnumber
			,c.Fiscal_year
		--End of your query
		END








































GO


