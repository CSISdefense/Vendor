--Instructions ( https://github.com/CSISdefense/DIIGsql/blob/master/docs/vendor_update_instructions.md )

--https://cage.dla.mil/search

--This is helpful for gliches on my home desktop
set QUERY_GOVERNOR_COST_LIMIT  0

--Identify problem UEIs ( https://github.com/CSISdefense/DIIGsql/blob/master/docs/vendor_update_instructions.md#identify-problem-ueis )

EXEC	 [Vendor].[SP_UEIunlabeledToInvestigateContractOrderFast]


-- Determine the Actual Owner ( https://github.com/CSISdefense/DIIGsql/blob/master/docs/vendor_update_instructions.md#determine-the-actual-owner )



EXEC Vendor.sp_InvestigateUEI
	@UEI = 'J6LFYSFAMVN5'



EXEC Vendor.sp_InvestigateUEI
	@UEI = 'WMR6E5BGV967'

--Slower, use only if results are puzzling.
EXEC Vendor.sp_InvestigateUEIdetail	
	@UEI = 'INSERT_UEI_HERE'
	
-- Assign the ParentID to the UEI ( https://github.com/CSISdefense/DIIGsql/blob/master/docs/vendor_update_instructions.md#assign-the-parentid-to-the-uei )

EXEC	 [Vendor].[sp_AssignUEIparentID]
		@uei = N'WMR6E5BGV967',
		@parentid = N'CHEMRING GROUP PLC',
		@startyear = 2010,
		@endyear = 2024

set QUERY_GOVERNOR_COST_LIMIT  0
EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'WYLE'
		KBR WYLE SERVICES  LLC

EXEC	 [Vendor].[sp_CreateParentIDuei]
		@UEI = N'DTNNRMAK6WN1',
		@parentid = N'CHEMRING GROUP PLC',
		@startyear = 2009, --Update if for fewer years
		@endyear = 2024  --Update if for fewer years


EXEC	@return_value = [Vendor].[sp_CreateParentIDstandardizedVendorName]
		@standardizedVendorName = N'INSERT_NAME_here', --only use for non-generic names.
		@parentid = N'INSERT_parentID_here',
		@startyear = 1990, --Update if for fewer years
		@endyear = 2024 --Update if for fewer years


--
GO













GO
set QUERY_GOVERNOR_COST_LIMIT  0
EXEC	 [Vendor].[sp_InvestigateParentID]
		@parentid = N'martin'

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
--Fixing mistakes

EXEC	 [Vendor].[sp_EraseUEIparentID]
		@UEI = N'XNGAANYA3F95',
		@startyear = 2004,
		@endyear = 2006

--Duplication and subsidiaries
		
EXEC	[Vendor].[SP_MergeParentId]
		@oldparentid = N'WYLE',
		@mergedparentid = N'WYLE LABORATORIES'


EXEC	 [Vendor].[sp_AssignUEIparentID]
		@uei = N'DVGLNXBURMP3',
		@parentid = N'ALLIANCE FOR SUSTAINABLE ENERGY',
		@startyear = 2000,
		@endyear = 2023

	EXEC	[Vendor].[sp_AssignSubsidiaryStatus]
		@parentidOfSubsidiary = N'ADI TECHNOLOGIES',
		@MergerDate = N'1/4/2022',
		@MergerURL = N'https://www.centene.com/news/centene-completes-acquisition-of-magellan-health.html',
		@parentidOfOwner = N'THALES'

		
EXEC	[Vendor].[sp_AssignParentIDtoSubsidiary]
		@subsidiaryparentid = N'BEARINGPOINT',
		@ownerparentid = N'CENTENE',
		@startyear = 2022,
		@endyear = 2023



			EXEC	[Vendor].[sp_AssignParentHeadquartersCountry]
		@parentid = N'ROCKWELL COLLINS',
		@parentheadquarterscountrycodetext = N'United States'


	

			EXEC	[Vendor].[sp_AssignParentHeadquartersCountry]
		@parentid = N'AIRBUS',
		@parentheadquarterscountrycodetext = N'EUROPEAN UNION'

	EXEC	[Vendor].[sp_AssignParentHeadquartersCountry]
		@parentid = N'SUMITOMO HEAVY INDUSTRIES',
		@parentheadquarterscountrycodetext = N'Japan'
		
			EXEC	[Vendor].[sp_AssignParentHeadquartersCountry]
		@parentid = N'EADS NV',
		@parentheadquarterscountrycodetext = N'France'


			EXEC	[Vendor].[sp_AssignParentHeadquartersCountry]
		@parentid = N'SIEMENS',
		@parentheadquarterscountrycodetext = N'Germany'
		
		
			
	EXEC	[Vendor].[sp_AssignParentHeadquartersCountry]
		@parentid = N'CHEMRING GROUP PLC',
		@parentheadquarterscountrycodetext = N'United Kingdom'
		
			EXEC	[Vendor].[sp_AssignParentHeadquartersCountry]
		@parentid = N'AUSTAL',
		@parentheadquarterscountrycodetext = N'Australia'

		


