  select CalendarFirstFiscalYear
  ,sum(SAMregDunsCount) as SAMregDunsCount
  ,sum(SAM_match_in_FPDS) as SAM_match_in_FPDS
  ,sum(SAM_match_in_Contractor_DUNSnumber) as SAM_match_in_Contractor_DUNSnumber
  ,sum(FPDSappearanceDunsCount) as FPDSappearanceDunsCount
  ,sum(FPDSmissingFromSAM) as FPDSmissingFromSAM
  from(
  select
   year(SAM.registrationDate)+iif(month(SAM.registrationDate)>=10,1,0) as CalendarFirstFiscalYear
  ,count(distinct sam.duns) as SAMregDunsCount
  ,count(distinct fpds.dunsnumber) as SAM_match_in_FPDS
  ,count(distinct cd.dunsnumber) as SAM_match_in_Contractor_DUNSnumber
  ,NULL as  FPDSappearanceDunsCount
  ,NULL as FPDSmissingFromSAM
  FROM [dbo].[allSAM] SAM
  left outer join (select distinct dunsnumber 
  FROM contractor.dunsnumbertoparentcontractorhistory dtpch
  where dtpch.ispresent=1
  ) as fpds
  on sam.duns=fpds.dunsnumber
  left outer join contractor.dunsnumber  as cd
  on sam.duns=cd.dunsnumber  
  group by year(SAM.registrationDate)+iif(month(SAM.registrationDate)>=10,1,0)

    union all

  select CalendarFirstFiscalYear
  ,NULL as SAMregDunsCount
  ,NULL as SAM_match_in_FPDS
  ,NULL as SAM_match_in_Contractor_DUNSnumber
  ,count(distinct dunsnumber) as FPDSappearanceDunsCount
    ,count(distinct SAMdunsnumber)as FPDSmissingFromSAM
  from (
	select dunsnumber
	,duns as SAMdunsnumber
	, min(dtpch.fiscalyear) as CalendarFirstFiscalYear
	FROM contractor.dunsnumbertoparentcontractorhistory dtpch
	left outer join (select distinct duns from dbo.allsam) as SAM
	on dtpch.dunsnumber=sam.duns
	where dtpch.ispresent=1
	group by dunsnumber,duns) as f
  group by CalendarFirstFiscalYear
  ) as sd
group by CalendarFirstFiscalYear
order by CalendarFirstFiscalYear


  