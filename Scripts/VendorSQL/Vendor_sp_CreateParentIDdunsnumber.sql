USE [DIIG]
GO

/****** Object:  StoredProcedure [Vendor].[sp_CreateParentIDdunsnumber]    Script Date: 10/10/2017 11:56:14 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO












-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	Assign a parent ID to a dunsnumber for a range of years
-- =============================================
ALTER PROCEDURE [Vendor].[sp_CreateParentIDdunsnumber]
	-- Add the parameters for the stored procedure here
	@dunsnumber varchar(13)
	,@parentid nvarchar(255)
	,@startyear int
	,@endyear int
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;




	if @dunsnumber is null
		raiserror('The value for @dunsnumber shold not be null.',15,1)
	if @parentid is null
		raiserror('The value for @parentid shold not be null. To erase a parentid, use contractor.sp_EraseParentID',15,1)
	if @startyear is null
		raiserror('The value for @startyear shold not be null. If assigning a single year, @startyear and @endyear should match.',15,1)
	if @endyear is null
		raiserror('The value for @endyear shold not be null. If assigning a single year, @startyear and @endyear should match.',15,1)
	if @endyear<@startyear
		raiserror('The value for @endyear must be greater than or equal to @startyear',15,1)
    -- Insert statements for procedure here

	

INSERT INTO [Contractor].[ParentContractor]
           ([ParentID]
           ,[Ticker]
           ,[ShortName]
           ,BloombergID
           ,[DIIGIndex]
           ,[LargeGreaterThan1B]
           ,[LargeGreaterThan3B]
           ,[PMC]
           ,[HRFprivatemilitary]
           ,[SIGIRprivemilitary]
           ,[SIGIRDuns]
           ,[Subsidiary]
           ,[MergerYear]
		   ,HooverID
		   ,LexisNexisID
           ,[RevenueInMillions]
           ,[RevenueYear]
           ,[RevenueSourceLink]
           ,[Replace]
           ,[JointVenture]
           ,[LastYear]
           ,[FirstYear]
           ,[SizeGuess]
           ,[NumberOfYears]
           ,[DACIM]
           ,[UnknownCompany]
           ,[FPDSannualRevenue]
           ,[Top100Federal]
           ,[AlwaysDisplay]
           ,[MergerDate]
           ,[MergerURL]
           ,[FirstDate]
           ,[FirstURl]
           ,[SpunOffFrom]
           ,[Top6])
     VALUES
           (@parentid
           ,NULL --<Ticker, nvarchar(255),>
           ,NULL --<ShortName, nvarchar(255),>
           ,NULL --<BloombergID, nvarchar(255),>
           ,0 --<DIIGIndex, bit,>
           ,0--<LargeGreaterThan1B, bit,>
           ,0 --<LargeGreaterThan3B, bit,>
           ,0 --<PMC, bit,>
           ,0--<HRFprivatemilitary, bit,>
           ,0--<SIGIRprivemilitary, bit,>
           ,0--<SIGIRDuns, bit,>
           ,0--<Subsidiary, bit,>
           ,NULL --<MergerYear, int,>
		   ,NULL --<HooverID, nvarchar(255),>
		   ,NULL --<LexisNexisID, nvarchar(255),>
           ,NULL --<RevenueInMillions, decimal(19,4),>
           ,NULL --<RevenueYear, int,>
           ,NULL --<RevenueSourceLink, nvarchar(255),>
           ,NULL --<Replace, nvarchar(255),>
           ,0--<JointVenture, bit,>
           ,NULL --<LastYear, int,>
           ,NULL --<FirstYear, int,>
           ,0--<LargeGuess, bit,>
           ,NULL --<NumberOfYears, int,>
           ,0--<DACIM, bit,>
           ,0--<UnknownCompany, bit,>
           ,NULL --<FPDSannualRevenue, int,>
           ,0--<Top100Federal, bit,>
           ,0--<AlwaysDisplay, bit,>
           ,NULL --<MergerDate, datetime,>
           ,NULL --<MergerURL, nvarchar(255),>
           ,NULL --<FirstDate, datetime,>
           ,NULL --<FirstURl, nvarchar(255),>
           ,NULL --<SpunOffFrom, nvarchar(255),>
           ,0--<Top6, bit,>
		   )



DECLARE	@return_value int

EXEC	@return_value = vendor.[sp_AssignParentID]
		@dunsnumber ,
		@parentid ,
		@startyear ,
		@endyear 
		
END












GO


