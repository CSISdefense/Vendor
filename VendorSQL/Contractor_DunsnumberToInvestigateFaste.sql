USE [DIIG]
GO

/****** Object:  View [Contractor].[DunsnumbersToInvestigateFast]    Script Date: 11/3/2017 3:58:51 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

























ALTER VIEW [Contractor].[DunsnumbersToInvestigateFast]
AS

SELECT
	case
		when (isnull(parent.LargeGreaterThan3B,0)=0 
			and isnull(parent.LargeGreaterThan1B,0)=0
			and parent.RevenueInMillions is null
			and isnull(parent.UnknownCompany,0)=0)
		then 'RevenueInMillions is null'
		when parent.MergerYear<=DtPCH.FiscalYear
		then 'ParentID is in use after parent.MergerYear'
		when (parent.FirstYear>DtPCH.FiscalYear and parent.SpunOffFrom is not null)
		then 'ParentID is in use before parent.firstyear and parent.spunofffrom is not null'
		else 'ERROR'
	end as ReasonForInclusion
	,dtpch.fiscalyear
	, dtpch.obligatedAmount/1000000000/def.GDPdeflator AS ConstantObligatedBillions
	, dtpch.TotalAmount/1000000000/def.GDPdeflator AS ConstantTotalBillions
	, dtpch.fed_funding_amount/1000000000/def.GDPdeflator AS ConstantFedFundedBillions
	, dtpch.DUNSNumber
	, dtpch.StandardizedTopContractor
	,dtpch.Parentdunsnumber
	,dtpch.HeadquarterCode
	,dtpch.CAGE
	, dtpch.parentid
	, parentduns.parentid as ParentDunsnumberParentIDsuggestion
	, HeadquarterCodeDTP.parentid as HeadquarterCodeParentIDsuggestion
	, parent.LargeGreaterThan3B
	, parent.LargeGreaterThan1B
	, parent.RevenueInMillions
	, parent.jointventure
	, parent.MergerYear
	, parent.MergerDate
	, parent.FirstYear
	, parent.SpunOffFrom
	,parent.isenterprise
	,parent.isfaithbased
	,parent.isngo
	,parent.isnetwork
	,parent.isgovernment
	,parent.ismultilateral
	,parent.ispublicprivatepartnership
	,parent.isUniversityorResearchInstitute
	,parent.isinternationalNGO
	,parent.isforeign
FROM 
	contractor.DunsnumberToParentContractorHistory as DtPCH 
	LEFT Outer JOIN contractor.ParentContractor as Parent
	ON dtpch.Parentid = parent.parentid
	Left outer join Economic.Deflators as def
	on DtPCH.FiscalYear=def.Fiscal_Year
	left outer join contractor.DunsnumberToParentContractorHistory as Parentdtp
	on dtpch.parentdunsnumber=parentdtp.dunsnumber and dtpch.fiscalyear=Parentdtp.fiscalyear
	left outer join contractor.DunsnumberToParentContractorHistory as HeadquarterCodeDTP
	on dtpch.HeadquarterCode=HeadquarterCodeDTP.dunsnumber and dtpch.fiscalyear=HeadquarterCodeDTP.fiscalyear
	left outer join contractor.parentcontractor as Parentduns
	on parentdtp.parentid=parentduns.parentid
WHERE 
	(isnull(parent.LargeGreaterThan3B,0)=0 
	and isnull(parent.LargeGreaterThan1B,0)=0
	 and parent.RevenueInMillions is null
	 and isnull(parent.UnknownCompany,0)=0)
	 or parent.MergerYear<=DtPCH.FiscalYear
	 or parent.FirstYear>DtPCH.FiscalYear



















GO


