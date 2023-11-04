

EXEC	 [Vendor].[SP_UEIunlabeledToInvestigateContractOrderFast]


EXEC	[Vendor].[SP_ParentIDToInvestigateFast]

GO
XDJBAL68ELM5


'ASAHI REFINING USA'

EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'SUSTAINABLE ENERGY'

EXEC	 [Vendor].[sp_AssignUEIparentID]
		@uei = N'XDJBAL68ELM5',
		@parentid = N'BWXT',
		@startyear = 2000,
		@endyear = 2023


EXEC	 [Vendor].[sp_CreateParentIDuei]
		@UEI = N'X7WUS5LRBQU3',
		@parentid = N'ALLIANCE FOR SUSTAINABLE ENERGY',
		@startyear = 1990,
		@endyear = 2023







GO

EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'	'




EXEC	[Vendor].[sp_AssignJointVentureStatus]
		@parentid = N'TRIAD NATIONAL SECURITY [BATELLE/CAL/TEXAS]',
		@JointVenture = 1,
		@FirstOwner = N'Battelle',
		@SecondOwner = N'UNIVERSITY OF CALIFORNIA'

	EXEC	[Vendor].[sp_AssignOwnertoParentIDOrRemove]
			@ParentID ='TRIAD NATIONAL SECURITY [BATELLE/CAL/TEXAS]'
			,@OwnerParentID = 'UNIVERSITY OF TEXAS'
			,@IsRemoval = 0


EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'TRIAD NATIONAL SECURITY [BATELLE/CAL/TEXAS]'

		Battelle ; UNIVERSITY OF CALIFORNIA; UNIVERSITY OF TEXAS


		
EXEC	[Vendor].[SP_MergeParentId]
		@oldparentid = N'US INVESTIGATIONS SERVICES',
		@mergedparentid = N'Altegrity'
