

update contract.fpds
set recipient_uei=''
where dunsnumber=recipient_uei
and len(recipient_uei) in (8,9)


select *
from errorlogging.UEIdunsnumberSwap

update f
set dunsnumber=f.recipient_uei
,recipient_uei=''
from contract.fpds f
inner join errorlogging.UEIdunsnumberSwap u
on u.contract_transaction_unique_key=f.contract_transaction_unique_key
where len(f.recipient_uei) in (8,9)


update f
set parentdunsnumber=f.recipient_parent_uei
,recipient_parent_uei=''
from contract.fpds f
inner join errorlogging.UEIdunsnumberSwap u
on u.contract_transaction_unique_key=f.contract_transaction_unique_key
where len(f.recipient_parent_uei) in (8,9)


update vendor.UEIhistory
set Dunsnumber=NULL
where len(dunsnumber)=12

update vendor.UEIhistory
set TopDunsnumber=NULL
where len(TopDunsnumber)=12


update vendor.UEIhistory
set ParentDUNSnumber=NULL
where len(ParentDUNSnumber)=12

update contractor.DunsnumberToParentContractorHistory
set recipient_uei=NULL
where len(recipient_uei) in (8,9)


update contractor.DunsnumberToParentContractorHistory
set recipient_parent_uei=NULL
where len(recipient_parent_uei) in (8,9)

select count(*)
,len(dunsnumber) as dunsnumber
,len(recipient_uei) as recipient_uei
,len(parentdunsnumber) as parentdunsnumber
,len(recipient_parent_uei) as recipient_parent_uei
from contract.fpds
where 
len(dunsnumber) =12 or 
len(recipient_uei) in (8,9) or
len(parentdunsnumber) =12 or
len(recipient_parent_uei) in (8,9)
group by 
len(dunsnumber) 
,len(recipient_uei) 
,len(parentdunsnumber) 
,len(recipient_parent_uei) 

select contract_transaction_unique_key
,dunsnumber
,recipient_uei
,parentdunsnumber
,recipient_parent_uei
into errorlogging.UEIdunsnumberSwap
from contract.fpds
where len(dunsnumber) =12 or 
len(recipient_uei) in (8,9) or
len(parentdunsnumber) =12 or
len(recipient_parent_uei) in (8,9)







--26s
select top 1000 dunsnumber
from contract.fpds f
where len(dunsnumber)=12 and len(recipient_uei) in (8,9)

--Roughly 1 million rows an hour (2.2m at 1h57m)
update f
set UEItemp=dunsnumber
from contract.fpds f
where len(dunsnumber)=12 and len(recipient_uei) in (8,9)


--1h08m
select count(*)
from contract.fpds f
where len(dunsnumber)=12 and len(recipient_uei) in (8,9) and  UEItemp is null


--***********Dunsnumber, not present in new downloads*************
-- 'Insert new dunsnumber into [Contractor].[dunsnumber]'
INSERT INTO [Contractor].[dunsnumber] (dunsnumber)
SELECT
ErrorTable.recipient_uei
FROM
contract.fpds AS ErrorTable
LEFT OUTER JOIN [Contractor].[dunsnumber]  AS PKTable
ON
PKTable.dunsnumber=ErrorTable.recipient_uei
WHERE ErrorTable.recipient_uei is not null and PKTable.dunsnumber is null and
 len(errortable.dunsnumber)=12 and len(errortable.recipient_uei) in (8,9)
GROUP BY ErrorTable.recipient_uei

SELECT
ErrorTable.recipient_uei
FROM
contract.fpds AS ErrorTable
LEFT OUTER JOIN [Contractor].[dunsnumber]  AS PKTable
ON
PKTable.dunsnumber=ErrorTable.recipient_uei
WHERE PKTable.dunsnumber is null and
 len(errortable.dunsnumber)=12 and len(errortable.recipient_uei) in (8,9)

-- 'Insert new dunsnumber and fiscal_year into [Contractor].[DunsnumberToParentContractorHistory]'
--3h56m
INSERT INTO [Contractor].DunsnumberToParentContractorHistory(dunsnumber,FiscalYear)
SELECT
Errortable.recipient_uei,
errortable.fiscal_year
FROM contract.FPDS AS ErrorTable
LEFT OUTER JOIN [Contractor].DunsnumberToParentContractorHistory  AS PKTable
ON
PKTable.dunsnumber=ErrorTable.recipient_uei
and PKTable.FiscalYear=ErrorTable.fiscal_year
WHERE ErrorTable.recipient_uei is not null and PKTable.dunsnumber is null and
 len(errortable.dunsnumber)=12 and len(errortable.recipient_uei) in (8,9)
GROUP BY ErrorTable.recipient_uei, errortable.fiscal_year

--Finished!

-- Insert new [parent_recipient_uei] into Vendor.recipient_uei
--1hr46m
INSERT INTO Vendor.recipient_uei
(recipient_uei)
SELECT fk.UEItemp
FROM contract.fpds as fk
LEFT OUTER JOIN Vendor.recipient_uei as pk
On pk.recipient_uei=fk.UEItemp
WHERE pk.recipient_uei is NULL and fk.UEItemp is not null and
 len(fk.dunsnumber)=12 and len(fk.recipient_uei) in (8,9)
GROUP BY fk.UEItemp

	-- Insert new [recipient_uei] into Vendor.Recipient_UEIhistory
INSERT INTO Vendor.Recipient_UEIhistory
(fiscal_year,recipient_uei)
SELECT fk.fiscal_year,fk.UEItemp
FROM contract.fpds as fk
LEFT OUTER JOIN Vendor.Recipient_UEIhistory as pk
On pk.recipient_uei=fk.recipient_uei and
		pk.fiscal_year=fk.fiscal_year 
WHERE pk.recipient_uei is NULL and
 len(fk.dunsnumber)=12 and len(fk.recipient_uei) in (8,9)
GROUP BY fk.UEItemp,fk.fiscal_year

--Stopping run here
update f
set dunsnumber=recipient_uei
	,recipient_uei=UEItemp
from contract.fpds f
where len(dunsnumber)=12 and len(recipient_uei) in (8,9)
and UEItemp is not null


--/****** Object:  Index [pk_vendor_recipient_uei]    Script Date: 8/13/2023 11:40:28 AM ******/
--ALTER TABLE [Vendor].[UEI] ADD  CONSTRAINT [pk_vendor_recipient_uei] PRIMARY KEY CLUSTERED 
--(
--	[UEI] ASC
--)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ONLINE = OFF, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
--GO

--/****** Object:  Index [pk_vendor_recipient_ueitoParentIDhistory]    Script Date: 8/13/2023 11:57:07 AM ******/
--ALTER TABLE [Vendor].[UEIhistory] ADD  CONSTRAINT [pk_vendor_recipient_ueitoParentIDhistory] PRIMARY KEY CLUSTERED 
--(
--	[UEI] ASC,
--	[Fiscal_Year] ASC
--)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ONLINE = OFF, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
--GO



--ALTER TABLE [Vendor].[UEI]  WITH CHECK ADD FOREIGN KEY([Parent_UEI])
--REFERENCES [Vendor].[UEI] ([UEI])
--GO




--ALTER TABLE [Contract].[FPDS]  WITH CHECK ADD  CONSTRAINT [fk_contract_fpds_recipient_parent_UEIhistory] FOREIGN KEY([recipient_parent_uei], [fiscal_year])
--REFERENCES [Vendor].[UEIhistory] ([UEI], [Fiscal_Year])
--GO

--ALTER TABLE [Contract].[FPDS] CHECK CONSTRAINT [fk_contract_fpds_recipient_UEIhistory]
--GO

--ALTER TABLE [Contractor].[Dunsnumber]  WITH CHECK ADD FOREIGN KEY([recipient_uei])
--REFERENCES [Vendor].[UEI] ([UEI])
--GO

--ALTER TABLE [Contractor].[Dunsnumber]  WITH CHECK ADD FOREIGN KEY([recipient_parent_uei])
--REFERENCES [Vendor].[UEI] ([UEI])
--GO

--ALTER TABLE [Contract].[FPDS]  WITH CHECK ADD  CONSTRAINT [fk_contract_fpds_recipient_parent_uei] FOREIGN KEY([recipient_parent_uei])
--REFERENCES [Vendor].[UEI] ([UEI])
--GO

--ALTER TABLE [Contract].[FPDS] CHECK CONSTRAINT [fk_contract_fpds_recipient_parent_uei]
--GO

--ALTER TABLE [Contract].[FPDS]  WITH CHECK ADD  CONSTRAINT [fk_contract_fpds_recipient_uei] FOREIGN KEY([recipient_uei])
--REFERENCES [Vendor].[UEI] ([UEI])
--GO

--ALTER TABLE [Contract].[FPDS] CHECK CONSTRAINT [fk_contract_fpds_recipient_uei]
--GO


ALTER TABLE [Vendor].[UEIhistory]  WITH CHECK ADD FOREIGN KEY([UEI])
REFERENCES [Vendor].[UEI] ([UEI])
GO


ALTER TABLE [Vendor].[UEIhistory]  WITH CHECK ADD FOREIGN KEY([Parent_UEI])
REFERENCES [Vendor].[UEI] ([UEI])
GO

ALTER TABLE [Contractor].[DunsnumberToParentContractorHistory]  WITH CHECK ADD FOREIGN KEY([recipient_uei])
REFERENCES [Vendor].[UEI] ([UEI])
GO

ALTER TABLE [Contractor].[DunsnumberToParentContractorHistory]  WITH CHECK ADD FOREIGN KEY([recipient_parent_uei])
REFERENCES [Vendor].[UEI] ([UEI])
GO




---Duns
ALTER TABLE [Vendor].[UEIhistory] DROP CONSTRAINT [FK__Recipient__Paren__5793BE78]
GO



delete from
contractor.DunsnumberToParentContractorHistory
where len(DUNSnumber) =12

delete from
contractor.Dunsnumber
where len(DUNSnumber) =12

ALTER TABLE [Vendor].[UEIhistory]  WITH CHECK ADD FOREIGN KEY([ParentDUNSnumber])
REFERENCES [Contractor].[Dunsnumber] ([DUNSnumber])
GO


