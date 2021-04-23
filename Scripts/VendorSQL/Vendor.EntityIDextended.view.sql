USE CSIS360
GO

/****** Object:  StoredProcedure [Vendor].[SP_TopVendorHistoryPlatformSubCustomer]    Script Date: 10/5/2017 9:43:57 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


Create VIEW [Vendor].[EntityIDextended]

AS

		--Copy the start of your query here
		SELECT 
			min(C.fiscal_year) as EntityIDminOfFiscalYear
			,max(C.fiscal_year) as EntityIDmaxOfFiscalYear
			,count(distinct fiscal_year) as EntityIDcountOfYears
			,count(distinct fiscal_year) as EntityIDcountOfYears2001to2010

				,C.EntityID
				,C.parentid
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag
				,Sum(C.obligatedAmount) AS obligatedAmount
			,Sum(C.numberOfActions) AS numberOfActions
			FROM [Vendor].[VendorHistoryNaicsPlatformSubCustomer] as C
		GROUP BY 
				C.EntityID
				,C.parentid
				,C.jointventure
				,c.UnknownCompany
				,C.WarningFlag








































GO


