/****** Object:  StoredProcedure [Vendor].[SP_EntityIDhistory]    Script Date: 4/13/2018 8:41:55 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



Alter PROCEDURE [Vendor].[SP_DunsnumberNewEntrants]

@Customer VARCHAR(255)

AS

IF (@Customer is not null) --Begin sub path where all product and services but only one Customer will be returned
	BEGIN
		--Copy the start of your query here
		SELECT 
			C.fiscal_year
			,c.Dunsnumber
				,c.UnknownCompany
				,C.EntityID
				,C.parentid
				,C.customer
				,c.Small as CSISsmall
				,C.DirectCOBSD
				,C.NAICS2
				, c.veteranownedflag
, c.minorityownedbusinessflag
, c.womenownedflag
, c.isforeigngovernment
, c.csiscontractid
, c.OriginIsInternational
, c.VendorIsInternational
				,max(c.SignedDate) as AnnualMaxOfSignedDate
				,Sum(C.obligatedAmount) AS obligatedAmount
			,Sum(C.numberOfActions) AS numberOfActions
			,count(c.csistransactionID) as TransactionCount 
			FROM [Vendor].[VendorHistoryNaicsPlatformSubCustomer] as C
		--Here's the where clause for @IsService is null and Customer is not null
		WHERE C.Customer=@Customer 
		--Copy the end of your query here
		GROUP BY 
			C.fiscal_year
			,c.Dunsnumber
				,c.UnknownCompany
				,C.EntityID
				,C.parentid
				,C.customer
				,c.Small
				,C.DirectCOBSD
				,C.NAICS2
				, c.veteranownedflag
, c.minorityownedbusinessflag
, c.womenownedflag
, c.isforeigngovernment
, c.csiscontractid
, c.typeofsetaside
, c.OriginIsInternational
, c.VendorIsInternational
		order by
				C.Dunsnumber
				,c.Fiscal_year
		--End of your query
		END
ELSE --Begin sub path where all products and services amd all Customers will be returned
		BEGIN
		--Copy the start of your query here
	SELECT 
			C.fiscal_year
			,c.Dunsnumber
				,c.UnknownCompany
				,C.EntityID
				,C.parentid
				,C.customer
				,c.Small as CSISsmall
				,C.DirectCOBSD
				,C.NAICS2
				, c.veteranownedflag
, c.minorityownedbusinessflag
, c.womenownedflag
, c.isforeigngovernment
, c.csiscontractid
, c.OriginIsInternational
, c.VendorIsInternational
				,max(c.SignedDate) as AnnualMaxOfSignedDate
				,Sum(C.obligatedAmount) AS obligatedAmount
			,Sum(C.numberOfActions) AS numberOfActions
			,count(c.csistransactionID) as TransactionCount 
			FROM [Vendor].[VendorHistoryNaicsPlatformSubCustomer] as C
		--No clause for Customer is null
		--Copy the end of your query here
		GROUP BY 
			C.fiscal_year
			,c.Dunsnumber
				,c.UnknownCompany
				,C.EntityID
				,C.parentid
				,C.customer
				,c.Small
				,C.DirectCOBSD
				,C.NAICS2
				, c.veteranownedflag
, c.minorityownedbusinessflag
, c.womenownedflag
, c.isforeigngovernment
, c.csiscontractid
, c.typeofsetaside
, c.OriginIsInternational
, c.VendorIsInternational
		order by
				C.Dunsnumber
				,c.Fiscal_year
		--End of your query
		END








































GO


