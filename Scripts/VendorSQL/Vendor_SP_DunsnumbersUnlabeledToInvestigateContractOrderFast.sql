USE CSIS360

/****** Object:  StoredProcedure [Vendor].[SP_DunsnumbersUnlabeledToInvestigateContractOrderFast]    Script Date: 3/7/2018 1:25:53 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	List the top unlabeled DUNSnumbers
-- =============================================
ALTER PROCEDURE [Vendor].[SP_DunsnumbersUnlabeledToInvestigateContractOrderFast]
	-- Add the parameters for the stored procedure here
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here
	SELECT top 1000 min(D.[fiscalyear]) as  MinFiscalYear
		,max(D.[fiscalyear]) as  MaxFiscalYear
		,D.[DUNSNumber]
      ,sum(D.[ConstantObligatedBillions]) as SumOfConstantObligatedBillions
	  ,max(D.[ConstantObligatedBillions]) as MaxOfConstantObligatedBillions
	  ,sum(D.ConstantFedFundedBillions) as SumOfConstantFedFundedBillions
	  ,sum(D.ConstantTotalBillions) as SumOfConstantTotalBillions
	  ,max(D.ConstantTotalBillions) as MaxOfConstantTotalBillions
      ,D.[StandardizedTopContractor]
		,d.parentdunsnumberparentidsuggestion
	FROM Contractor.DunsnumbersToInvestigateFast as D
	left outer join (select 
		sum(iif(len(dunsnumber)<=9,obligatedamount,0))/def.GDPdeflator as AnnualObligated
		,sum(iif(len(dunsnumber)<=9,obligatedamount,0))/def.GDPdeflator/1000 as TenthOnePercent
		,FiscalYear
		from contractor.DunsnumberToParentContractorHistory 
		inner join Economic.Deflators as def
			on FiscalYear=def.Fiscal_Year
		where fiscalyear>=1990
		group by fiscalyear, def.GDPdeflator
	) ao
	on d.FiscalYear=ao.fiscalyear
	where d.parentid is null
	group by
      D.[DUNSNumber]
      ,D.[StandardizedTopContractor]
		,d.parentdunsnumberparentidsuggestion 
	having max(D.ConstantObligatedBillions)>=0.25 
		or sum(D.ConstantObligatedBillions)>=1
		or max(iif(D.ConstantObligatedBillions>=ao.TenthOnePercent/1000000000,1,0))=1
	Order by sum(D.[ConstantObligatedBillions]) desc
	


END
GO


