USE [DIIG]
GO

/****** Object:  StoredProcedure [Vendor].[sp_AssignParentID]    Script Date: 10/10/2017 11:38:19 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO






-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	Assign a parent ID to a dunsnumber for a range of years
-- =============================================
ALTER PROCEDURE [Vendor].[sp_AssignParentID]
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
	if not exists (select parentid from contractor.ParentContractor 
			where parentid=@parentid
			)
		raiserror('The parentid does not yet exist',15,1)
	if not exists (select dunsnumber from contractor.DunsnumberToParentContractorHistory 
			where dunsnumber=@dunsnumber and
		fiscalyear>=@startyear and
		fiscalyear<=@endyear)
		raiserror('The dunsnumber does not exist within the selected date range',15,1)
    -- Insert statements for procedure here
	update contractor.DunsnumberToParentContractorHistory
	set parentid=@parentid
	,CSISmodifiedBy=system_user
	,csismodifieddate=getdate()
	where dunsnumber=@dunsnumber and
		fiscalyear>=@startyear and
		fiscalyear<=@endyear and
		(parentid<>@parentid or parentid is null)

END






GO


