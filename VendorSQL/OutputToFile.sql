USE CSIS360
GO


--For instructions see https://github.com/CSISdefense/DIIGsql/blob/master/Doc/Output_Large_Dataset.md


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [Vendor].[SP_EntityIDhistoryNAICS]
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Vendor.sp_EntityCountHistoryPlatformSubCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Vendor.sp_EntityCountHistorySubCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Vendor.sp_EntityCountHistoryPlatformCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Vendor.sp_EntityCountHistoryCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [Vendor].[SP_EntityIDhistoryNAICS]
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = NULL
		,@IsSAMduns=1
--SELECT	'Return Value' = @return_value

