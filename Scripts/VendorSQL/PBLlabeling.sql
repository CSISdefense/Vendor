SET QUERY_GOVERNOR_COST_LIMIT 0	
SELECT TOP (1000) --[announcement_date]
      --,[service]
      --,[awarding_agency]
      --,[announced_amount]
      --,[contract_type]
      [relatedUniqueAwardKey]
	  ,cau.contract_award_unique_key
	  	  ,clid.ContractLabelID
	  ,clid.ContractLabelText	
	  ,pcau.contract_award_unique_key as pcau
      ,[Parent_Award_Unique_Key]
  	  ,pclid.ContractLabelID
	  ,pclid.ContractLabelText	
      --,[equipment]
      --,[announcement]
  FROM [Contract].[250506_DoD Contracts Announcement(20161001-20250505)] dca
  left outer join contract.contract_award_unique_key cau 
  on cau.contract_award_unique_key=dca.[relatedUniqueAwardKey]
  left outer join contract.CSIScontractID ccid 
  on cau.CSIScontractID=ccid.CSIScontractID
  left outer join contract.ContractLabelID clid
  on ccid.ContractLabelID=clid.ContractLabelID
  left outer join contract.contract_award_unique_key pcau 
  on pcau.contract_award_unique_key=dca.[Parent_Award_Unique_Key]
  left outer join contract.CSIScontractID pccid 
  on pcau.CSIScontractID=pccid.CSIScontractID
  left outer join contract.ContractLabelID pclid
  on pccid.ContractLabelID=pclid.ContractLabelID
  
  group by [relatedUniqueAwardKey]
  ,cau.contract_award_unique_key
	  ,cau.contract_award_unique_key
      ,[Parent_Award_Unique_Key]
	  	  ,pcau.contract_award_unique_key
		  ,clid.ContractLabelID
	  ,clid.ContractLabelText
	   	  ,pclid.ContractLabelID
	  ,pclid.ContractLabelText	


	  update ccid
set ContractLabelID=5,
CSISmodifiedBy=CURRENT_USER+' NKwon Contract Announcement',
CSISmodifieddate=getdate()
  FROM [Contract].[250506_DoD Contracts Announcement(20161001-20250505)] dca
  left outer join contract.contract_award_unique_key cau 
  on cau.contract_award_unique_key=dca.[relatedUniqueAwardKey]
  left outer join contract.CSIScontractID ccid 
  on cau.CSIScontractID=ccid.CSIScontractID
  left outer join contract.ContractLabelID clid
  on ccid.ContractLabelID=clid.ContractLabelID
  left outer join contract.contract_award_unique_key pcau 
  on pcau.contract_award_unique_key=dca.[Parent_Award_Unique_Key]
  left outer join contract.CSIScontractID pccid 
  on pcau.CSIScontractID=pccid.CSIScontractID
  left outer join contract.ContractLabelID pclid
  on pccid.ContractLabelID=pclid.ContractLabelID
  where ccid.ContractLabelID is null
  --group by [relatedUniqueAwardKey]
  --,cau.contract_award_unique_key
	 -- ,cau.contract_award_unique_key
  --    ,[Parent_Award_Unique_Key]
	 -- 	  ,pcau.contract_award_unique_key
		--  ,clid.ContractLabelID
	 -- ,clid.ContractLabelText
	 --  	  ,pclid.ContractLabelID
	 -- ,pclid.ContractLabelText	