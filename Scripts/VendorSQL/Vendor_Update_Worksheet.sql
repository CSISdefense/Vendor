

EXEC	 [Vendor].[SP_UEIunlabeledToInvestigateContractOrderFast]


EXEC	[Vendor].[SP_ParentIDToInvestigateFast]


GO
	set QUERY_GOVERNOR_COST_LIMIT  0


EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'United tech'


		RAM-System GmbH is owned by MBDA Deutschland (50%), Diehl Stiftung (25%) and Diehl BGT Defence (25%).

EXEC	 [Vendor].[sp_AssignUEIparentID]
		@uei = N'RXN6RGGRJD71',
		@parentid = N'FLUOR-BWXT PORTSMOUTH',
		@startyear = 2000,
		@endyear = 2023


EXEC	 [Vendor].[sp_CreateParentIDuei]
		@UEI = N'RXN6RGGRJD71',
		@parentid = N'IPG DXTRA',
		@startyear = 1990,
		@endyear = 2023







GO

EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'guidewell'

		--Assign revenue
EXEC	[Vendor].[sp_AssignRevenue]
		@parentid = N'MATSON',
		@RevenueInMillions = 4340,
		@RevenueYear = 2022,
		@RevenueSourceLink = N'https://www.google.com/finance/quote/MATX:NYSE',
		@SizeGuess = 0

		select *
		from contractor.ParentContractor
		where RevenueSourceLink = N'https://www.google.com/finance/quote/TD.PF.K:TSE'

--Handle a Joint venture
--Find the parents

EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'roche'



EXEC	[Vendor].[sp_CreateOwnerParentID]
		@parentid = N'webuild',
		@RevenueInMillions = 8310,
		@RevenueYear = 2022,
		@RevenueSourceLink = N'https://www.google.com/finance/quote/0N4O:LON',
		@SizeGuess = 0



-- Assign status
EXEC	[Vendor].[sp_AssignJointVentureStatus]
		@parentid = N'RAM-System GmbH',
		@JointVenture = 1,
		@FirstOwner = N'MBDA Deutschland',
		@SecondOwner = N'Diehl'

	EXEC	[Vendor].[sp_AssignOwnertoParentIDOrRemove]
			@ParentID ='RAM-System GmbH'
			,@OwnerParentID = 'Deihl'
			,@IsRemoval = 0


EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'v2x'

		Battelle ; UNIVERSITY OF CALIFORNIA; UNIVERSITY OF TEXAS

--Duplication and subsidiaries
		
EXEC	[Vendor].[SP_MergeParentId]
		@oldparentid = N'MAGELLAN HEALTH SERVICE',
		@mergedparentid = N'MAGELLAN HEALTH'


EXEC	 [Vendor].[sp_AssignUEIparentID]
		@uei = N'DVGLNXBURMP3',
		@parentid = N'ALLIANCE FOR SUSTAINABLE ENERGY',
		@startyear = 2000,
		@endyear = 2023

	EXEC	[Vendor].[sp_AssignSubsidiaryStatus]
		@parentidOfSubsidiary = N'MAGELLAN HEALTH',
		@MergerDate = N'1/4/2022',
		@MergerURL = N'https://www.centene.com/news/centene-completes-acquisition-of-magellan-health.html',
		@parentidOfOwner = N'CENTENE'

		
EXEC	[Vendor].[sp_AssignParentIDtoSubsidiary]
		@subsidiaryparentid = N'BEARINGPOINT',
		@ownerparentid = N'CENTENE',
		@startyear = 2022,
		@endyear = 2023