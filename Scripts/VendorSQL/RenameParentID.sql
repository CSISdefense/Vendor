USE [DIIG]
GO

DECLARE	@return_value int

EXEC	@return_value = [Vendor].[SP_RenameParentID]
		@oldparentid = N'FAIRCHILD',
		@newparentid = N'FAIRCHILD SPACE & DEFENSE'

SELECT	'Return Value' = @return_value

GO
