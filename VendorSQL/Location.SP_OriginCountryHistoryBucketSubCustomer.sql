/****** Object:  StoredProcedure [Location].[SP_OriginCountryHistoryBucketSubCustomer]    Script Date: 5/14/2018 12:45:49 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	Assign a parent ID to a dunsnumber for a range of years
-- =============================================
ALTER PROCEDURE [Location].[SP_OriginCountryHistoryBucketSubCustomer]
	-- Add the parameters for the stored procedure here
	@country nvarchar(255)

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
	if @country is null 
	begin
		select 
			l.fiscal_year
			,l.contractingcustomer
			,l.contractingsubcustomer
			,l.fundingagency
			,l.fundingsubagency
			,l.isforeignownedandlocated
			,l.isforeigngovernment
			,l.isinternationalorganization
			,l.PlaceIsInternational
			,l.OriginIsInternational
			,l.VendorIsInternational
			,l.VendorWarTheater
			,l.PlaceCountryText
			,l.VendorCountryText
			,l.OriginCountryText
			,l.placeofmanufactureText
			,l.BAAcategory
			,l.ProductOrServiceArea
			,l.ProductOrServiceArea
			,psc.ProductOrServiceCodeText
			,sum(l.numberOfActions) as SumOfnumberOfActions
			,sum(l.obligatedAmount) as SumOfobligatedAmount
		from location.OriginCountryHistoryBucketSubCustomer as l
		left outer join fpdstypetable.ProductOrServiceCode as psc
		on l.productorservicecode=psc.ProductOrServiceCode
		group by 

			l.fiscal_year
			,l.contractingcustomer
			,l.contractingsubcustomer
			,l.fundingagency
			,l.fundingsubagency
			,l.isforeignownedandlocated
			,l.isforeigngovernment
			,l.isinternationalorganization
			,l.PlaceIsInternational
			,l.OriginIsInternational
			,l.VendorIsInternational
						,l.VendorWarTheater
			,l.PlaceCountryText
			,l.VendorCountryText
			,l.OriginCountryText
			,l.placeofmanufactureText
			,l.BAAcategory
			,l.ProductOrServiceArea
			,l.ProductOrServiceArea
			,psc.ProductOrServiceCodeText
		end
	else
	begin



		--Verify that the parameter is already in the relevant type table.
	if(select p.vendorcountrycode
		from FPDSTypeTable.vendorcountrycode as p
		where p.vendorcountrycode=@country) is null 
	begin
		--If there's a similar value to the missing one, suggest that instead.
		if (select top 1 p.vendorcountrycode
			from FPDSTypeTable.vendorcountrycode as p
			where p.vendorcountrycode like '%'+@country+'%') is not null 
		begin
			select p.vendorcountrycode
				from FPDSTypeTable.vendorcountrycode as p
				where p.vendorcountrycode like '%'+@country+'%'
			select 'The value for @country is not found in FPDSTypeTable.vendorcountrycode. Did you mean one of the above?' as ErrorDescription
			return -1
		end
		--If there's no similar value, return an error, they'll have to check the table themselves.
		else
		begin
			raiserror('The value for @country is not found in FPDSTypeTable.vendorcountrycode.',15,1)
		end
	end
	--End input protection




		select 
			l.fiscal_year
	,l.contractingcustomer
			,l.contractingsubcustomer
			,l.fundingagency
			,l.fundingsubagency
			,l.isforeignownedandlocated
			,l.isforeigngovernment
			,l.isinternationalorganization
			,l.PlaceIsInternational
		,l.OriginIsInternational
		,l.VendorIsInternational
		,sum(l.numberOfActions) as SumOfnumberOfActions
		,sum(l.obligatedAmount) as SumOfobligatedAmount
	from location.OriginCountryHistoryBucketSubCustomer as l
	where  @country in (l.VendorCountryText, l.OriginCountryText )
	group by 
		l.fiscal_year
	,l.contractingcustomer
			,l.contractingsubcustomer
			,l.fundingagency
			,l.fundingsubagency
		,l.isforeignownedandlocated
		,l.isforeigngovernment
		,l.isinternationalorganization
		,l.PlaceIsInternational
		,l.OriginIsInternational
		,l.VendorIsInternational
	end
		



end
GO


