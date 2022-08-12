DECLARE	@return_value int

EXEC	@return_value = [Project].[sp_CreateProjectID]
		@ProjectName = N'RQ-7 Shadow',
		@ProjectAbbreviation = N'RQ7',
		@ProjectPrettyName = N'RQ-7 Shadow',
		@MinOfFiscalYear = NULL,
		@RANDID = NULL,
		@IsParentProjectID = 1,
		@ParentProjectID = NULL

SELECT	'Return Value' = @return_value

DECLARE	@return_value int

EXEC	@return_value = [Contract].[SP_CreateContractLabelID]
		@contractlabeltext = N'RQ-7 Shadow',
		@PrimaryProjectID = 677,
		@TFBSOoriginated = NULL,
		@TFBSOmentioned = NULL,
		@IsPerformanceBasedLogistics = NULL

SELECT	'Return Value' = @return_value

GO


GO
select * from project.projectid
where ProjectAbbreviation='RQ-7 Shadow'

update project.projectid
set IsRemotelyOperated=1
where ProjectAbbreviation='RQ7'

update contract.contractlabelid
set primaryprojectid=2259
where contractlabelid=1069
	

  update [Project].[CTID_FPDS10m_descrip_plat] 
  set clid=2259
  where -- remotely_crewed='MQ-25'
  (remotely_crewed is not null or remotely_crewed=1064)
  and   remotely_crewed in ('RQ-7')
  and (clid is null  or clid=1069)

    select distinct remotely_crewed, clid
  from [Project].[CTID_FPDS10m_descrip_plat] 

  where clid=1065 and clid is not null
  order by remotely_crewed

  update ctid
  set ContractLabelID=f.clid
  from contract.CSIStransactionID ctid
  inner join [Project].[CTID_FPDS10m_descrip_plat]  f
  on ctid.CSIStransactionID=f.CSIStransactionID
  where (ctid.ContractLabelID is null or ctid.ContractLabelID=1065) and f.clid is not null and ctid.ContractLabelID<>f.clid  
  
  


  
  select * 
  from [Project].[CTID_FPDS10m_descrip_plat] 
  where CSIStransactionID is null

/****** Script for SelectTopNRows command from SSMS  ******/
--SELECT dp.[PlatformPortfolioRemote]
--      ,dp.[obligatedamount]
--      ,dp.[descrip_plat_row_number]
--      ,dp.[Remotely_Crewed]
--      ,dp.[UAS]
--      ,dp.[CUAS]
--      ,dp.[Maritime]
--      ,dp.[mq]
--      ,dp.[rq]a
--      ,dp.[any_uas]
--      ,dp.[descriptionofcontractrequirement]
--	  ,f.CSIStransactionID
--	  into [Project].[CTID_FPDS10m_descrip_plat] 
--  FROM [Project].[FPDS10m_descrip_plat] dp
--  left join contract.fpds f
--  on f.descriptionofcontractrequirement=dp.descriptionofcontractrequirement
--  where [Remotely_Crewed] is not null


/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [majorprogramcode]
      ,[MajorProgramText]
      ,[Remotely_Crewed]
      ,[MajorProgramCode_row_number]
      ,[UAS]
      ,[CUAS]
      ,[Maritime]
      ,[mq]
      ,[rq]
      ,[any_uas]
  FROM [Project].[contract_MajorProgramCode]
  where Remotely_Crewed is not null

  /****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [ProjectID]
      ,[ProjectName]
      ,[ProjectPrettyName]
      ,[ProjectAbbreviation]
      ,[IsJointDevelopmentCaseStudy]
      ,[CSISmodifiedDate]
      ,[CSISmodifiedBy]
      ,[IsPerformanceBasedLogistics]
      ,[PlatformPortfolio]
      ,[ObligatedAmount]
      ,[MinOfFiscalYear]
      ,[MaxOfFiscalYear]
      ,[TopPlatformPortfolio]
      ,[TopPlatformPortfolioObligatedAmount]
      ,[TopParentID]
      ,[TopParentIDObligatedAmount]
      ,[Notes]
      ,[IsUnknown]
      ,[WasUncategorizedMissilesOrSpaceSystem]
      ,[IsUninvestigated]
      ,[IsRemotelyOperated]
      ,[RANDID]
      ,[IsParentProjectID]
      ,[ParentProjectID]
      ,[IDAID]
      ,[IsUnidentified]
      ,[IncludesMOSA]
  FROM [Project].[ProjectID]
  where  IsRemotelyOperated=1


  select clid.*, p.ProjectName from contract.ContractLabelID clid
  inner join project.ProjectID p
  on clid.PrimaryProjectID=p.ProjectID
  where p.IsRemotelyOperated=1
  
  update contract.ContractLabelID 
  set ContractLabelText='MQ-9' where contractlabeltext=424 and contractlabelid=1021
