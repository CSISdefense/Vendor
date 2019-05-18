/****** Object:  StoredProcedure [Vendor].[SP_ReAssignParentIDoverRange]    Script Date: 3/12/2018 2:01:17 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	Assign a parent ID to a dunsnumber for a range of years
-- =============================================
ALTER PROCEDURE [Vendor].[SP_ReAssignParentIDoverRange]
	-- Add the parameters for the stored procedure here
	@originalparentid nvarchar(255)
	,@newparentid nvarchar(255)
	,@startyear int
	,@endyear int
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	if @originalparentid is null
		raiserror('The value for @originalparentid  shold not be null.',15,1)
	if not exists (select parentid from contractor.ParentContractor 
		where parentid=@originalparentid
		)
		raiserror('The @originalparentid does not exist',15,1)
	if @newparentid is null
		raiserror('The value for @newparentid shold not be null. To erase a parentid, use contractor.sp_EraseParentID',15,1)
	if not exists (select parentid from contractor.ParentContractor 
		where parentid=@newparentid
		)
		raiserror('The @@newparentid does not exist',15,1)
	if @startyear is null
		raiserror('The value for @startyear shold not be null. If assigning a single year, @startyear and @endyear should match.',15,1)
	if @endyear is null
		raiserror('The value for @endyear shold not be null. If assigning a single year, @startyear and @endyear should match.',15,1)
	if @endyear<@startyear
		raiserror('The value for @endyear must be greater than or equal to @startyear',15,1)
    -- Insert statements for procedure here
	update contractor.DunsnumberToParentContractorHistory
	set parentid=@newparentid
	,CSISmodifiedBy=system_user
	,csismodifieddate=getdate()
	where parentid=@originalparentid and
		fiscalyear>=@startyear and
		fiscalyear<=@endyear
END
GO


