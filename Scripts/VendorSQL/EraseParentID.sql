USE [DIIG]
GO

DECLARE	@return_value int

EXEC	@return_value = [Vendor].[sp_EraseParentID]
		@dunsnumber = N'1070803760000',
		@startyear = 2002,
		@endyear = 2002

SELECT	'Return Value' = @return_value

GO
