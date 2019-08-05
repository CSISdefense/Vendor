USE CSIS360
GO


--For instructions see https://github.com/CSISdefense/DIIGsql/blob/master/Doc/Output_Large_Dataset.md



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;





SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;

--This way takes hours!
--select *
--from contract.csistransactionid ctid
--inner join contract.fpds f
--on f.csistransactionid = ctid.csistransactionid
--where ctid.csiscontractid in (19005830,
--1431340,
--24816950,
--10090818,
--8341560,
--24807877,
--24719937,
--24905030
--)

--This way took 5 minutes.
--gt250k_change_outliers.txt
select f.*, t.CSIScontractID
from contract.csistransactionid t
inner join contract.fpds f
on f.csistransactionid = t.csistransactionid
where f.CSIStransactionID in 
(select CSIStransactionID
from contract.csistransactionid ctid
where ctid.csiscontractid in (
1431340,
2966598,
7878880,
8157803,
8341560,
8567148,
10090818,
18671771,
18671780,
18671888,
18671981,
19005830,
24719937,
24807877,
24816950,
24905030
) or IDVPIID='N0002400D6000'
)

DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense',
		@SubCustomer = NULL,
		@PlatformPortfolio =NULL



DECLARE	@return_value int

--EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
EXEC	@return_value = [Vendor].SP_EntityIDhistoryCalendar
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value

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
EXEC	@return_value = [Vendor].[SP_EntityIDhistoryPlatform]
--EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
		@Customer = 'Defense'
		
--SELECT	'Return Value' = @return_value


--SET ANSI_WARNINGS OFF;
--SET NOCOUNT ON;
--DECLARE	@return_value int

----EXEC	@return_value = [Vendor].[SP_EntityIDhistory]
--EXEC	@return_value = [Vendor].[SP_EntityIDhistoryNAICS]
----EXEC	@return_value = Contract.[SP_ContractBudgetDecisionTree]
--		@Customer = NULL
--This resulted in a blank file!
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

--DECLARE	@return_value int

EXEC	
 [Vendor].[SP_DunsnumberNewEntrants]
		@Customer = NULL,
		@IsSAMduns = NULL

--SELECT	'Return Value' = @return_value

GO
