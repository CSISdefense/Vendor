USE [CSIS360]
GO

/****** Object:  StoredProcedure [Contract].[SP_ContractUnmodifiedandOutcomeDetailsCustomer]    Script Date: 9/14/2017 4:27:48 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



-- =============================================
-- Author:		Greg Sanders
-- Create date: 2/01/2013
-- Description:	Break down contracts by size.
-- =============================================
Alter PROCEDURE [Contract].[SP_UnmodifiedScopeHistory]
	-- Add the parameters for the stored procedure here
	@IsDefense bit
	--@ServicesOnly Bit

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statementIDVPIID.
	SET NOCOUNT ON;

	-- Insert statements for procedure here

		--Copy the start of your query here
	 
		select count(ctid.CSIScontractID) as ContractCount
		,count(ctid.CSIStransactionID) as TransactionCount
		,sum(f.obligatedamount) as obligatedamount
		,sum(f.numberofactions) as numberofactions
,f.fiscal_year
,cc.StartFiscal_Year


--,cc.SumOfUnmodifiedobligatedAmount
--,cc.UnmodifiedBase
--,cc.UnmodifiedCeiling
--,cc.ChangeOrderBaseAndAllOptionsValue
--,cc.ChangeOrderCeilingGrowth
--,cc.UnmodifiedNumberOfOffersReceived
,datediff(DAY,cc.MinOfSignedDate,cc.UnmodifiedCurrentCompletionDate)+1 as UnmodifiedDays
--,cc.UnmodifiedCurrentCompletionDate
--,cc.UnmodifiedUltimateCompletionDate
--,cc.UnmodifiedLastDateToOrder

,pb.SimpleArea
--, cc.IsClosed
		--, cc.IsModified
		--, cc.IsTerminated
		--,cc.SumOfisChangeOrder
		--,cc.MaxOfisChangeOrder
		--,cc.SumOfisNewWork
		--,cc.MaxOfisNewWork
		,cc.SizeOfUnmodifiedObligatedAmount
		,cc.SizeOfUnmodifiedSumOfbaseandalloptionsvalue
		,cc.SizeOfUnmodifiedSumOfbaseandexercisedoptionsvalue
		

from contract.fpds f
	inner join contract.CSIStransactionID ctid
	on f.CSIStransactionID=ctid.CSIStransactionID
	inner join FPDSTypeTable.agencyid a
	on ctid.contractingofficeagencyid=a.AgencyID
	inner join contract.ContractDiscretization cc
	on cc.CSIScontractID=ctid.CSIScontractID
	inner join contract.ContractPlatformBucket pb 
	on ctid.CSIScontractID=pb.CSIScontractID
	where @IsDefense is null or
	(a.Customer='Defense' and @IsDefense=1) or (a.Customer<>'Defense' and @IsDefense=0)
group by ctid.CSIScontractID	
,f.fiscal_year
,cc.StartFiscal_Year
,pb.SimpleArea
,datediff(DAY,cc.MinOfSignedDate,cc.UnmodifiedCurrentCompletionDate) 
		,cc.SizeOfUnmodifiedObligatedAmount
		,cc.SizeOfUnmodifiedSumOfbaseandalloptionsvalue
		,cc.SizeOfUnmodifiedSumOfbaseandexercisedoptionsvalue







END


GO



