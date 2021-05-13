USE CSIS360
GO


--For instructions see https://github.com/CSISdefense/DIIGsql/blob/master/Doc/Output_Large_Dataset.md



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
exec [Economic].[SP_NASbioEconomy]

select *
from FPDSTypeTable.PrincipalNaicsCode
where principalNAICS5DigitCode='54171'

update FPDSTypeTable.PrincipalNaicsCode
set principalnaicscodeText='Research and Development in Nanotechnology'
where principalnaicscode=541713 and principalnaicscodeText is null

update FPDSTypeTable.PrincipalNaicsCode
set NASbioEconomy='Bioeconomy R&D services' 
,NASbioEconomyPT=1
where principalnaicscode in ('541710','541712')

select *
from fpdstypetable.recoveredmaterialclauses

select CSIScontractID, CSIStransactionID, descriptionofcontractrequirement, obligatedAmount,agencyid,principalnaicscode, principalnaicscodeText,ProductOrServiceCode,ProductOrServiceCodeText, piid,idvpiid,claimantprogramcode,
ContractingOfficeID,ContractingOfficeName
from contract.FPDSpartial
where contractingofficeagencyid='97JC' and principalnaicscode in ('541711','541714')

77209836
