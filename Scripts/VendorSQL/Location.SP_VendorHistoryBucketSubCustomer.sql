/****** Object:  StoredProcedure [Location].[SP_VendorHistoryBucketSubCustomer]    Script Date: 5/14/2018 12:45:16 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	Assign a parent ID to a dunsnumber for a range of years
-- =============================================
ALTER PROCEDURE [Location].[SP_VendorHistoryBucketSubCustomer]
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
			 [Fiscal_year]
      ,[ContractorDisplayName]
      ,[Small]
      ,[jointventure]
      ,[warningflag]
      ,[UnknownCompany]
      ,[Top100Federal]
      ,[fundedbyforeignentity]
      ,[isforeignownedandlocated]
      ,[isforeigngovernment]
      ,[isinternationalorganization]
      ,[organizationaltype]
       ,[PlaceIsUnitedstates]
      ,[PlaceCountryText]
      ,[OriginIsUnitedStates]
      ,[OriginCountryText]
      ,[VendorIsUnitedStates]
      ,[VendorCountryText]
      ,[placeofmanufactureText]
      ,[vendorcountrycode]
			,sum(l.[SumOfnumberOfActions]) as SumOfnumberOfActions
			,sum(l.[SumOfobligatedAmount]) as SumOfobligatedAmount
		from location.VendorHistoryBucketSubCustomerDiscretization as l
		group by 
			 [Fiscal_year]
      ,[ContractorDisplayName]
      ,[Small]
      ,[jointventure]
      ,[warningflag]
      ,[UnknownCompany]
      ,[Top100Federal]
      ,[fundedbyforeignentity]
      ,[isforeignownedandlocated]
      ,[isforeigngovernment]
      ,[isinternationalorganization]
      ,[organizationaltype]
 ,[PlaceIsUnitedstates]
      ,[PlaceCountryText]
      ,[OriginIsUnitedStates]
      ,[OriginCountryText]
      ,[VendorIsUnitedStates]
      ,[VendorCountryText]
      ,[placeofmanufactureText]
      ,[vendorcountrycode]
		end
	else
	begin
		select 
			 [Fiscal_year]
      ,[Vendor]
	  ,StandardizedTopContractor
      ,[ContractorDisplayName]
      ,[Small]
      ,[jointventure]
      ,[warningflag]
      ,[UnknownCompany]
      ,[Top100Federal]
      ,[fundedbyforeignentity]
      ,[isforeignownedandlocated]
      ,[isforeigngovernment]
      ,[isinternationalorganization]
      ,[organizationaltype]
        ,[PlaceCountryText]
      ,[OriginIsUnitedStates]
      ,[OriginCountryText]
      ,[VendorIsUnitedStates]
      ,[VendorCountryText]
      ,[placeofmanufactureText]
      ,[vendorcountrycode]
			,sum(l.[SumOfnumberOfActions]) as SumOfnumberOfActions
			,sum(l.[SumOfobligatedAmount]) as SumOfobligatedAmount
		from location.VendorHistoryBucketSubCustomerDiscretization as l
			where  @country in (l.VendorCountryText, 
			l.OriginCountryText ,
			l.PlaceCountryText
			)
		group by 
			 [Fiscal_year]
			 ,StandardizedTopContractor
      ,[Vendor]
      ,[ContractorDisplayName]
      ,[Small]
      ,[jointventure]
      ,[warningflag]
      ,[UnknownCompany]
      ,[Top100Federal]
      ,[fundedbyforeignentity]
      ,[isforeignownedandlocated]
      ,[isforeigngovernment]
      ,[isinternationalorganization]
      ,[organizationaltype]
 ,[PlaceIsUnitedstates]
      ,[PlaceCountryText]
      ,[OriginIsUnitedStates]
      ,[OriginCountryText]
      ,[VendorIsUnitedStates]
      ,[VendorCountryText]
      ,[placeofmanufactureText]
      ,[vendorcountrycode]


		
end


end
GO


