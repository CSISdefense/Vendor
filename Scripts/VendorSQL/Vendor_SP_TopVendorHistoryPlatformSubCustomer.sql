USE [DIIG]
GO

/****** Object:  StoredProcedure [Vendor].[SP_TopVendorHistoryPlatformSubCustomer]    Script Date: 10/5/2017 9:43:57 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


ALTER PROCEDURE [Vendor].[SP_TopVendorHistoryPlatformSubCustomer]

@Customer VARCHAR(255),
@SubCustomer VARCHAR(255)

AS

IF (@SubCustomer is not null) --Begin sub path where all product and services but only one Customer will be returned
	BEGIN
		--Copy the start of your query here
		SELECT 
			C.Fiscal_Year
			,C.Customer
			,C.SubCustomer
			,C.PlatformPortfolio
			,iif(VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100 and UnknownCompany =0,ContractAnnualPlatformSubCustomerVendorRank
					,0) as AnnualPlatformPortfolioSubCustomerVendorRank			
			,Iif(c.VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100
				,C.ContractorDisplayName+iif(C.jointventure=1,'*','')+iif(C.WarningFlag=1,'!','')
				,NULL
			)  as ContractorDisplayName 
			,Iif(c.VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100
				,C.AllContractor
				,'Unranked Vendor'
			)  as ParentID			
			,C.JointVenture
			,Sum(C.SumOfobligatedAmount) AS SumOfobligatedAmount
			,Sum(C.SumOfnumberOfActions) AS SumOfnumberOfActions
			,C.WarningFlag
			FROM Vendor.VendorFPDShistoryPlatformSubCustomerDirectDiscretization as C
		--Here's the where clause for @IsService is null and Customer is not null
		WHERE C.SubCustomer=@SubCustomer 
		--Copy the end of your query here
		GROUP BY 
				C.fiscal_year
				,C.Customer
				,C.PlatformPortfolio
				,C.SubCustomer
				,vendorisranked
				,iif(VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100 and UnknownCompany =0,ContractAnnualPlatformSubCustomerVendorRank
					,0)
				,Iif(c.VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100
					,C.ContractorDisplayName+iif(C.jointventure=1,'*','')+iif(C.WarningFlag=1,'!','')
					,NULL
				)  
				,Iif(c.VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100
					,C.AllContractor
					,'Unranked Vendor'
				)
				,C.JointVenture
				,C.WarningFlag
		order by
				c.Fiscal_year
				,C.Customer
				,Sum(C.SumOfobligatedAmount) desc
		--End of your query
		END
ELSE --Begin sub path where all products and services amd all Customers will be returned
		BEGIN
		--Copy the start of your query here
		SELECT 
			C.Fiscal_Year
			,C.Customer
			,C.SubCustomer
			,C.PlatformPortfolio
			,iif(VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100 and UnknownCompany =0,ContractAnnualPlatformSubCustomerVendorRank
					,0) as AnnualPlatformPortfolioSubCustomerVendorRank			
			,Iif(c.VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100
				,C.ContractorDisplayName+iif(C.jointventure=1,'*','')+iif(C.WarningFlag=1,'!','')
				,NULL
			)  as ContractorDisplayName 
			,Iif(c.VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100
				,C.AllContractor
				,'Unranked Vendor'
			)  as ParentID			
			,C.JointVenture
			,Sum(C.SumOfobligatedAmount) AS SumOfobligatedAmount
			,Sum(C.SumOfnumberOfActions) AS SumOfnumberOfActions
			,C.WarningFlag
			FROM Vendor.VendorFPDShistoryPlatformSubCustomerDirectDiscretization as C
		--Here's the where clause for @IsService is null and Customer is not null
		WHERE C.Customer=@Customer 
		--Copy the end of your query here
		GROUP BY 
				C.fiscal_year
				,C.Customer
				,C.PlatformPortfolio
				,C.SubCustomer
				,vendorisranked
				,iif(VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100 and UnknownCompany =0,ContractAnnualPlatformSubCustomerVendorRank
					,0)
				,Iif(c.VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100
					,C.ContractorDisplayName+iif(C.jointventure=1,'*','')+iif(C.WarningFlag=1,'!','')
					,NULL
				)  
				,Iif(c.VendorIsranked=1 or ContractAnnualPlatformSubCustomerVendorRank<=100
					,C.AllContractor
					,'Unranked Vendor'
				)
				,C.JointVenture
				,C.WarningFlag
		order by
				c.Fiscal_year
				,C.Customer
				,Sum(C.SumOfobligatedAmount) desc
		--End of your query
		END








































GO


