USE [DIIG]
GO

DECLARE	@return_value int

EXEC	@return_value = [Vendor].[sp_AssignSubsidiaryStatus]
		@parentidOfSubsidiary = N'FAIRCHILD',
		@MergerDate = N'06/02/1994',
		@MergerURL = N'http://articles.baltimoresun.com/1994-06-02/business/1994153004_1_orbital-fairchild-satellite',
		@parentidOfOwner = N'ORBITAL SCIENCES'

SELECT	'Return Value' = @return_value

GO
