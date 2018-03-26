USE CSIS360
GO
/****** Object:  StoredProcedure [Vendor].[sp_InvestigateParentID]    Script Date: 2/13/2018 5:49:51 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	Assign a parent ID to a dunsnumber for a range of years
-- =============================================
CREATE  PROCEDURE [Vendor].[sp_InvestigateParentID]
	-- Add the parameters for the stored procedure here
	@parentid nvarchar(255)

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
	if @parentid is null
		raiserror('The value for @parentid shold not be null.',15,1)


if(select p.ParentID
from contractor.ParentContractor as p
where p.ParentID=@parentid) is null 
begin
	if (select top 1 p.ParentID
				from contractor.ParentContractor as p
				where p.ParentID like '%'+@parentid+'%') is not null 
	begin
		
		select  p.ParentID
				from contractor.ParentContractor as p
				where p.ParentID like '%'+@parentid+'%'
		select 'The value for @parentid is not found in contractor.parentcontractor. Did you mean one of the above?' as ErrorDescription
		--return -1
	end
	--else
	--begin
	--	raiserror('The value for @parentid is not found in contractor.parentcontractor.',15,1)
	--end
end

	select
		'Contractor.ParentContractor' as SourceTable
		,[ParentID]
      ,[Ticker]
      ,[ShortName]
      ,[BloombergID]
      ,[DIIGIndex]
      ,[LargeGreaterThan1B]
      ,[LargeGreaterThan3B]
      ,[PMC]
      ,[HRFprivatemilitary]
      ,[SIGIRprivemilitary]
      ,[SIGIRDuns]
      ,[Subsidiary]
      ,[MergerYear]
      ,[RevenueInMillions]
      ,[RevenueYear]
      ,[RevenueSourceLink]
      ,[Replace]
      ,[JointVenture]
      ,[LastYear]
      ,[FirstYear]
      ,[SizeGuess]
      ,[NumberOfYears]
      ,[DACIM]
      ,[UnknownCompany]
      ,[FPDSannualRevenue]
      ,[Top100Federal]
      ,[AlwaysDisplay]
      ,[MergerDate]
      ,[MergerURL]
      ,[FirstDate]
      ,[FirstURL]
      ,[SpunOffFrom]
      ,[Top6]
      ,[HooverID]
      ,[LexisNexisID]
      ,[TopStandardizedVendor]
      ,[CSIScreatedDate]
      ,[CSISmodifiedDate]
      ,[CSISmodifiedBy]
      ,[overrideparentdunsnumber]
      ,[parentheadquarterscountrycode]
	  ,isSiliconValley
      ,[isforeign]
      ,[isinternationalNGO]
      ,[isenterprise]
      ,[ismultilateral]
      ,[isngo]
	  ,[isnonprofit]
      ,[isgovernment]
      ,[multilateraltype]
      ,[isfaithbased]
      ,[isnetwork]
      ,[ispublicprivatepartnership]
      ,[isUniversityorResearchInstitute]
      ,[topISO3countrycode]
      ,[totalamount]
      ,[topISO3countrytotalamount]
      ,[isInNeedOfInvestigation]
      ,[isoverrideISO3countrycode]
      
	from contractor.parentcontractor as P
	where p.parentid like ('%'+@parentid+'%') or p.parentid =@parentid
	order by parentid

    -- Insert statements for procedure here
	select 'Owners',parentid,owners,OwnerParentID
	from vendor.ParentIDtoOwnerParentID p
	where p.parentid like ('%'+@parentid+'%') or p.parentid =@parentid
	order by parentid, ownerparentid


	select
	'Dunsnumber Matches' as SourceTable
	,[DUNSnumber]
      ,min([FiscalYear]) as MinOfFiacalYear
	  ,max(fiscalyear) as MaxOfFiscalYear
      ,[ParentID]
	  ,[StandardizedTopContractor]
	  ,sum([ObligatedAmount]) as SumOfObligatedAmount
	  ,sum(d.fed_funding_amount) as SumOffed_funding_amount
	  ,sum(d.TotalAmount) as SumOfTotalAmount
      ,sum(d.TopVendorNameTotalAmount) as SumOfTopVendorNameTotalAmount
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  ,d.ParentDunsnumberParentID
	  ,d.HeadquarterCode
	  ,d.HeadquarterCodeParentID
	  ,d.cage
	  ,d.TopCountryName
	  ,sum(topISO3countrytotalamount) as SumOftopISO3countrytotalamount
	from [Vendor].[DunsnumberAlternateParentID] d
	where parentid like ('%'+@parentid+'%') or parentid =@parentid
	group by [DUNSnumber]
      ,[ParentID]
	  ,[StandardizedTopContractor]
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  	  ,d.ParentDunsnumberParentID
	  ,HeadquarterCode
	  	  ,d.HeadquarterCodeParentID
	  ,cage
	    ,d.TopCountryName
	order by --parentid
		--, StandardizedTopContractor
		--,
		 dunsnumber
		, min(FiscalYear)



	select
	'ParentDunsnumber only matches' as SourceTable
	,[Parentdunsnumber]
	,d.ParentID
	,d.ParentDunsnumberParentID
	,[DUNSnumber]
      ,min([FiscalYear]) as MinOfFiacalYear
	  ,max(fiscalyear) as MaxOfFiscalYear
      ,[ParentID] as DunsNumberParentID
	  ,[StandardizedTopContractor]
	  ,sum([ObligatedAmount]) as SumOfObligatedAmount
	  ,sum(d.fed_funding_amount) as SumOffed_funding_amount
	  ,sum(d.TotalAmount) as SumOfTotalAmount
      ,sum(d.TopVendorNameTotalAmount) as SumOfTopVendorNameTotalAmount
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]

	  ,d.HeadquarterCode
	  ,d.HeadquarterCodeParentID
	  ,d.cage
	  ,d.TopCountryName
	  ,sum(topISO3countrytotalamount) as SumOftopISO3countrytotalamount
	from [Vendor].[DunsnumberAlternateParentID] d
	where (d.ParentDunsnumberParentID like ('%'+@parentid+'%') or 
			d.ParentDunsnumberParentID =@parentid)
		and not (d.parentid like ('%'+@parentid+'%') or d.parentid =@parentid)
	group by [DUNSnumber]
      ,[ParentID]
	  ,[StandardizedTopContractor]
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  	  ,d.ParentDunsnumberParentID
	  ,HeadquarterCode
	  	  ,d.HeadquarterCodeParentID
	  ,cage
	    ,d.TopCountryName
	order by --parentid
		--, StandardizedTopContractor
		--,
		 dunsnumber
		, min(FiscalYear)


	select
	'HeadquartersCode only matches' as SourceTable
		  ,d.HeadquarterCode
	  ,d.HeadquarterCodeParentID
	,[DUNSnumber]
      ,min([FiscalYear]) as MinOfFiacalYear
	  ,max(fiscalyear) as MaxOfFiscalYear
      ,[ParentID] as DunsNumberParentID
	  ,[StandardizedTopContractor]
	  ,sum([ObligatedAmount]) as SumOfObligatedAmount
	  ,sum(d.fed_funding_amount) as SumOffed_funding_amount
	  ,sum(d.TotalAmount) as SumOfTotalAmount
      ,sum(d.TopVendorNameTotalAmount) as SumOfTopVendorNameTotalAmount
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
	  	,[Parentdunsnumber]
	,d.ParentDunsnumberParentID

	  ,d.cage
	  ,d.TopCountryName
	  ,sum(topISO3countrytotalamount) as SumOftopISO3countrytotalamount
	from [Vendor].[DunsnumberAlternateParentID] d
	where (d.HeadquarterCodeParentID like ('%'+@parentid+'%') or d.HeadquarterCodeParentID =@parentid)
		and not (d.parentid like ('%'+@parentid+'%') or d.parentid =@parentid)
	group by [DUNSnumber]
      ,[ParentID]
	  ,[StandardizedTopContractor]
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  	  ,d.ParentDunsnumberParentID
	  ,HeadquarterCode
	  	  ,d.HeadquarterCodeParentID
	  ,cage
	    ,d.TopCountryName
	order by --parentid
		--, StandardizedTopContractor
		--,
		 dunsnumber
		, min(FiscalYear)



	select
	'ParentID and HeadquartersCode mismatch' as SourceTable
		  ,d.HeadquarterCode
	  ,d.HeadquarterCodeParentID
	,[DUNSnumber]
      ,min([FiscalYear]) as MinOfFiacalYear
	  ,max(fiscalyear) as MaxOfFiscalYear
      ,[ParentID] as DunsNumberParentID
	  ,[StandardizedTopContractor]
	  ,sum([ObligatedAmount]) as SumOfObligatedAmount
	  ,sum(d.fed_funding_amount) as SumOffed_funding_amount
	  ,sum(d.TotalAmount) as SumOfTotalAmount
      ,sum(d.TopVendorNameTotalAmount) as SumOfTopVendorNameTotalAmount
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
	  	,[Parentdunsnumber]
	,d.ParentDunsnumberParentID

	  ,d.cage
	  ,d.TopCountryName
	  ,sum(topISO3countrytotalamount) as SumOftopISO3countrytotalamount
	from [Vendor].[DunsnumberAlternateParentID] d
	where (not (d.HeadquarterCodeParentID like ('%'+@parentid+'%') or d.HeadquarterCodeParentID =@parentid) or
	not (d.ParentDunsnumberParentID like ('%'+@parentid+'%') or d.ParentDunsnumberParentID =@parentid))
		and (d.parentid like ('%'+@parentid+'%') or d.parentid =@parentid)
	group by [DUNSnumber]
      ,[ParentID]
	  ,[StandardizedTopContractor]
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  	  ,d.ParentDunsnumberParentID
	  ,HeadquarterCode
	  	  ,d.HeadquarterCodeParentID
	  ,cage
	    ,d.TopCountryName
	order by --parentid
		--, StandardizedTopContractor
		--,
		 dunsnumber
		, min(FiscalYear)




	select
	'StandardVendorName Matches' as SourceTable
	,[DUNSnumber]
      ,min(d.[FiscalYear]) as MinOfFiacalYear
	  ,max(d.fiscalyear) as MaxOfFiscalYear
      ,d.[ParentID] as DUNSnumberParentID
	  ,d.[StandardizedTopContractor]
	  ,v.parentid as VendorNameParentID
	  ,vh.ParentID as VendorNameHistoryParentID
	  ,sum([ObligatedAmount]) as SumOfObligatedAmount
	  ,sum(d.fed_funding_amount) as SumOffed_funding_amount
	  ,sum(d.TotalAmount) as SumOfTotalAmount
      ,sum(d.TopVendorNameTotalAmount) as SumOfTopVendorNameTotalAmount
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  ,d.ParentDunsnumberParentID
	  ,d.HeadquarterCode
	  ,d.HeadquarterCodeParentID
	  ,d.cage
	  ,d.TopCountryName
	  ,sum(topISO3countrytotalamount) as SumOftopISO3countrytotalamount
	from [Vendor].[DunsnumberAlternateParentID] d
	left outer join vendor.StandardizedVendorParentIDHistoryNoContradictions vh
	on d.StandardizedTopContractor=vh.StandardizedTopContractor 
		and d.FiscalYear = vh.FiscalYear
	left outer join vendor.StandardizedVendorParentIDNoContradictions v
	on d.StandardizedTopContractor=v.StandardizedTopContractor
	where not(d.parentid like ('%'+@parentid+'%') or d.parentid =@parentid)
		and (d.StandardizedTopContractor like ('%'+@parentid+'%') or d.StandardizedTopContractor =@parentid
		or v.parentid like ('%'+@parentid+'%') or v.parentid =@parentid
		or vh.parentid like ('%'+@parentid+'%') or vh.parentid =@parentid
		)
	group by [DUNSnumber]
      ,d.[ParentID]
	  ,d.[StandardizedTopContractor]
	  ,vh.ParentID
	  ,v.parentid
	        ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  	  ,d.ParentDunsnumberParentID
	  ,HeadquarterCode
	  	  ,d.HeadquarterCodeParentID
	  ,cage
	    ,d.TopCountryName
	order by --parentid
		--, StandardizedTopContractor
		--,
		 dunsnumber
		, min(d.FiscalYear)

		
	
	select
	'Complete Contested Years from Contractor.DunsnumberToParentContractorHistory' as SourceTable
	,[DUNSnumber]
      ,min([FiscalYear]) as MinOfFiacalYear
	  ,max(fiscalyear) as MaxOfFiscalYear
      ,[ParentID]
	  ,[StandardizedTopContractor]
	  ,sum([ObligatedAmount]) as SumOfObligatedAmount
	  ,sum(d.fed_funding_amount) as SumOffed_funding_amount
	  ,sum(d.TotalAmount) as SumOfTotalAmount
      ,sum(d.TopVendorNameTotalAmount) as SumOfTopVendorNameTotalAmount
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  ,l.name as TopCountryName
	  ,sum(topISO3countrytotalamount) as SumOftopISO3countrytotalamount
	from contractor.DunsnumberToParentContractorHistory as D
		left outer join Location.CountryCodes l
	on l.[alpha-3]=d.topISO3countrycode
	where dunsnumber in  
		(select
		dunsnumber
		from 
	
		(select
		[DUNSnumber],parentid
		from contractor.DunsnumberToParentContractorHistory
		where parentid like ('%'+@parentid+'%') or parentid =@parentid
			or StandardizedTopContractor like ('%'+@parentid+'%') or StandardizedTopContractor =@parentid
		group by [DUNSnumber],parentid
		) as innermost
		where innermost.parentid is null or not (parentid like ('%'+@parentid+'%') or parentid =@parentid)
		--Exclude dunsnumbers where the entire number is labeled.
	)
	group by [DUNSnumber]
      ,[ParentID]
	  ,[StandardizedTopContractor]
      ,[Notes]
      ,[TooHard]
      ,[NotableSubdivision]
      ,[SubdivisionName]
      ,[Parentdunsnumber]
	  ,l.name
	order by --parentid
		--, StandardizedTopContractor
		--,
		 dunsnumber
		, min(FiscalYear)


select
	'Totals by ParentID from prospective Contractor.DunsnumberToParentContractorHistory' as SourceTable
      ,min([FiscalYear]) as MinOfFiacalYear
	  ,max(fiscalyear) as MaxOfFiscalYear
      ,[ParentID]
	  ,sum([ObligatedAmount]) as SumOfObligatedAmount
	  ,sum(d.fed_funding_amount) as SumOffed_funding_amount
	  ,sum(d.TotalAmount) as SumOfTotalAmount
	from contractor.DunsnumberToParentContractorHistory as D
		left outer join Location.CountryCodes l
	on l.[alpha-3]=d.topISO3countrycode
	where dunsnumber in  
		(select
		dunsnumber
		from 
	
		(select
		[DUNSnumber],parentid
		from contractor.DunsnumberToParentContractorHistory
		where parentid like ('%'+@parentid+'%') or parentid =@parentid
			or StandardizedTopContractor like ('%'+@parentid+'%') or StandardizedTopContractor =@parentid
		group by [DUNSnumber],parentid
		) as innermost
		--Exclude dunsnumbers where the entire number is labeled.
	)
	group by [ParentID]
	  
	order by parentid
		--, StandardizedTopContractor
		--,
		

	SELECT 'Contractor.ParentContractorNameHistory' as SourceTable
	  ,[ParentID]
      ,[FiscalYear]
      ,[CSISname]
      ,[LongName]
      ,[SourceURL]
      ,[TopStandardizedVendorName]
      ,[MaxOfTopVendorNameTotalAmount]
      ,[SumOfTotalAmount]
      ,[CSISmodifiedDate]
      ,[CSIScreateddate]
      ,[CSISmodifiedBy]
  FROM [Contractor].[ParentContractorNameHistory]
  	where parentid like ('%'+@parentid+'%') or parentid =@parentid 
		or csisname= @parentid or csisname like ('%'+@parentid+'%')
order by FiscalYear

END





GO
