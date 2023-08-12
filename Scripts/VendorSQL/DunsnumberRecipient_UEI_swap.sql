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