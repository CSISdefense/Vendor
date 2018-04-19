USE CSIS360
GO

/****** Object:  StoredProcedure [Vendor].[SP_DunsnumberWithOutOfRangeParentIDsFast]    Script Date: 11/3/2017 3:40:52 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
















-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	List the top unlabeled DUNSnumbers
-- =============================================
ALTER PROCEDURE [Vendor].[SP_DunsnumberWithOutOfRangeParentIDsFast]
	-- Add the parameters for the stored procedure here
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here
	SELECT top 1000 
	'ParentID is in use after parent.MergerYear' as ReasonForInclusion
		,min(D.[fiscalyear]) as  MinFiscalYear
		,max(D.[fiscalyear]) as  MaxFiscalYear
		,d.MergerYear
	  ,d.MergerDate
      ,D.[parentid]
	  ,D.DUNSNumber
	  ,d.StandardizedTopContractor
	  ,d.Owners
	  ,d.BGovParentIDSuggestion
	  ,d.ParentDunsnumberParentIDsuggestion
	  ,d.FirstYear
	  ,d.SpunOffFrom
	  ,sum(D.[ConstantObligatedBillions]) as TotalConstantObligatedBillions
	  ,max(D.[ConstantObligatedBillions]) as MaxAnnualConstantObligatedBillions
	FROM Contractor.DunsnumbersToInvestigateFast as D
	where d.parentid is not null
	group by
       D.[parentid]
	   ,D.DUNSNumber
	   ,d.StandardizedTopContractor
      ,d.MergerYear
	  ,d.MergerDate
	  ,d.Owners
	  ,d.BGovParentIDSuggestion
	  ,d.ParentDunsnumberParentIDsuggestion
	  ,d.FirstYear
	  ,d.SpunOffFrom
	having (d.MergerYear <=max(d.FiscalYear)
		 or d.FirstYear>min(d.fiscalyear)) 
		--and (sum(D.[ConstantObligatedBillions])>1 
		--or max(D.[ConstantObligatedBillions])>=0.25)

	Order by d.parentid, sum(d.[ConstantObligatedBillions]) desc
	


END
















GO


