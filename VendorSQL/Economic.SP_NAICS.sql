/****** Object:  StoredProcedure [Economic].[SP_NAICS]    Script Date: 6/5/2018 10:26:48 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO












ALTER PROCEDURE [Economic].[SP_NAICS]


AS

--alter table fpdstypetable.principalnaicscode
--add constraint fk_fpdstypetable_principalnaicscode_principalNAICS3DigitCode 
--foreign key (principalNAICS3DigitCode )
--references fpdstypetable.principalnaicscode(principalnaicscode)
--alter column principalNAICS3DigitCode varchar(6)
--alter column principalNAICS4DigitCode varchar(6)
--alter column principalNAICS5DigitCode varchar(6)


select *
from fpdstypetable.principalnaicscode original 
left outer join fpdstypetable.principalnaicscode derived
on original.principalnaicscode =derived.principalNAICS3DigitCode
where derived.principalnaicscode is null and 
original.principalNAICS3DigitCode is not null

select distinct [principalNAICS2DigitCode], SUBSTRING([principalnaicscode],1,2)
from FPDSTypeTable.PrincipalNaicsCode p
order by SUBSTRING([principalnaicscode],1,2)


select distinct [principalNAICS3DigitCode], SUBSTRING([principalnaicscode],1,3)
from FPDSTypeTable.PrincipalNaicsCode p
order by SUBSTRING([principalnaicscode],1,3)

update p
set principalnaics2digitcode =NULL
,[principalNAICS3DigitCode] =NULL
,[principalNAICS4DigitCode]=NULL
from fpdstypetable.PrincipalNaicsCode p
where principalnaics2digitcode in ('',':') --or principalnaics2digitcode is null
and (principalnaics2digitcode  is not null or 
[principalNAICS3DigitCode] is not null or
[principalNAICS4DigitCode] is not null)

update FPDSTypeTable.PrincipalNaicsCode 
set [principalNAICS2DigitCode]=case 
when SUBSTRING([principalnaicscode],1,2) in ('31','32','33')
then '31-33'
when SUBSTRING([principalnaicscode],1,2) in ('44','45')
then '44-45'
when SUBSTRING([principalnaicscode],1,2) in ('48','49')
then '48-49'
else SUBSTRING([principalnaicscode],1,2)
end
where [principalNAICS2DigitCode] is null and isnumeric([principalnaicscode])=1


update FPDSTypeTable.PrincipalNaicsCode 
set [principalNAICS3DigitCode]=SUBSTRING([principalnaicscode],1,3)
where [principalNAICS3DigitCode] is null 
	and len([principalnaicscode])>=3 and isnumeric([principalnaicscode])=1


update FPDSTypeTable.PrincipalNaicsCode 
set [principalNAICS4DigitCode]=SUBSTRING([principalnaicscode],1,4)
where [principalNAICS4DigitCode] is null and 
	len([principalnaicscode])>=4 and isnumeric([principalnaicscode])=1



--SELECT  [Unseperated]
--      ,[principalnaicscode]
--      ,[principalnaicscodeText]
--      ,[principalNAICS2DigitCode]
--      ,[principalNAICS3DigitCode]
--      ,[PDTcategory]
--      ,[PDTSsplit2DigitCode]
--      ,[PDTSsplit3DigitCode]
--      ,[NAICS_Code]
--      ,[Industry_TEXT]
--      ,[Industry_Cat_TEXT]
--      ,[Industry_SubCat_TEXT]
--      ,[Industry_SubSubCat_TEXT]
--  FROM [Contractor].[NAICScode] n
--    where principalnaicscode is null
--  or naics_code is null
--  order by n.[principalnaicscode]

--insert into FPDSTypeTable.PrincipalNaicsCode
-- ([principalnaicscode]
--      ,[principalnaicscodeText]
--      ,[principalNAICS2DigitCode]
--      ,[principalNAICS3DigitCode]
--	  ,[principalNAICS4DigitCode]
--	  )
--select [NAICS_Code] as [principalnaicscode]
--      ,[Industry_Cat_TEXT] as [principalnaicscodeText]
--	  ,[NAICS_Code] 
--	  ,NULL
--	  ,NULL
--  FROM [Contractor].[NAICScode] n
--    where principalnaicscode is null
--	and len([NAICS_Code])=2
--  order by n.[principalnaicscode]


--  insert into FPDSTypeTable.PrincipalNaicsCode
-- ([principalnaicscode]
--      ,[principalnaicscodeText]
--      ,[principalNAICS2DigitCode]
--      ,[principalNAICS3DigitCode]
--	  ,[principalNAICS4DigitCode]
--	  )
--select [NAICS_Code] as [principalnaicscode]
--      ,[Industry_SubCat_TEXT] as [principalnaicscodeText]
--	  ,substring([NAICS_Code] ,1,2) as [principalNAICS2DigitCode]
--	  ,[NAICS_Code] as [principalNAICS3DigitCode]
--	  ,NULL
--  FROM [Contractor].[NAICScode] n
--    where principalnaicscode is null
--	and len([NAICS_Code])=3
--  order by n.[principalnaicscode]

--    insert into FPDSTypeTable.PrincipalNaicsCode
-- ([principalnaicscode]
--      ,[principalnaicscodeText]
--      ,[principalNAICS2DigitCode]
--      ,[principalNAICS3DigitCode]
--	  ,[principalNAICS4DigitCode]
--	  )
--select [NAICS_Code] as [principalnaicscode]
--      ,[Industry_SubSubCat_TEXT] as [principalnaicscodeText]
--	  ,substring([NAICS_Code] ,1,2) as [principalNAICS2DigitCode]
--	  ,substring([NAICS_Code] ,1,3) as [principalNAICS3DigitCode]
--	  ,[NAICS_Code] as [principalNAICS4DigitCode]
--  FROM [Contractor].[NAICScode] n
--    where principalnaicscode is null
--	and len([NAICS_Code])=4
--  order by n.[principalnaicscode]



--SELECT  [Unseperated]
--      ,[principalnaicscode]
--      ,[principalnaicscodeText]
--      ,[NAICS_Code]
--      ,[Industry_TEXT]
--      ,[Industry_Cat_TEXT]
--      ,[Industry_SubCat_TEXT]
--      ,[Industry_SubSubCat_TEXT]
--  FROM [Contractor].[NAICScode] n
--    where nullif([principalnaicscodeText],'') is null
--	and naics_code is not null
--  order by n.[principalnaicscode]


--Update pn 
--set [principalnaicscodeText]=Industry_TEXT
--  FROM FPDSTypeTable.PrincipalNaicsCode pn
--  inner JOIN FPDSTypeTable.NAICScode as Nc
--ON Pn.principalnaicscode = Nc.NAICS_Code 
--    where nullif(pn.[principalnaicscodeText],'') is null
--	and nc.naics_code is not null
--	and len(pn.principalnaicscode)=6


GO


