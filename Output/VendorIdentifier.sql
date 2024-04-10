SET QUERY_GOVERNOR_COST_LIMIT 0



EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'Milspecs'



'Blue Origin'
'ABL Space'
'SPACEX'
'United Launch alliance'
'Firefly Aerospace'
'Rocket Lab'
'Virgin Orbit'
'RUSSIA SPACE AGENCY'

GO


EXEC	 [Vendor].[sp_InvestigateUEI]
		@UEI = N'D5KBWNTJ5AC6'


GO
EXEC	 [Vendor].[sp_AssignUEIparentID]
		@uei = N'MQ9ZJNM827J3',
		@parentid = N'PWR',
		@startyear = 2008,
		@endyear = 2024

EXEC	 [Vendor].[sp_CreateParentIDuei]
		@UEI = N'RL85D7QELNF6',
		@parentid = N'ADVANCED NAVIGATION',
		@startyear = 2012,
		@endyear = 2024




SELECT	'Return Value' = @return_value


exec  [Vendor].[sp_EraseParentIDuei]
@UEI = N'MQ9ZJNM827J3',
@startyear=2019,
@endyear=2021

GO


018895098
756060554
756060554
756060554
756060554
967776456
EXEC	 [Vendor].[sp_InvestigateDunsnumber]
		@dunsnumber = N'086585986'
		
783280696
		
EXEC	 [Vendor].[sp_CreateParentIDdunsnumber]
		@dunsnumber = N'86585986',
		@parentid = N'PWR',
		@startyear = 1991,
		@endyear = 2022

EXEC	 [Vendor].[sp_AssignParentID]
		@dunsnumber = N'618997217',
		@parentid = N'PWR',
		@startyear = 1991,
		@endyear = 2022

GO

SET QUERY_GOVERNOR_COST_LIMIT 0

EXEC	[Vendor].[SP_MergeParentId]
		@oldparentid = N'BLUE ORIGIN FEDERATION',
		@mergedparentid = N'BLUE ORIGIN'

SELECT	'Return Value' = @return_value

GO
