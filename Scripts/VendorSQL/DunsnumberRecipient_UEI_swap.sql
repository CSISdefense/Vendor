--26s
select top 1000 dunsnumber
from contract.fpds f
where len(dunsnumber)=12 and len(recipient_uei) in (8,9)

--Roughly 1 million rows an hour (2.2m at 1h57m)
update f
set dunsnumbertemp=dunsnumber
from contract.fpds f
where len(dunsnumber)=12 and len(recipient_uei) in (8,9)

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

-- 'Insert new dunsnumber and fiscal_year into [Contractor].[DunsnumberToParentContractorHistory]'
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


	-- Insert new [recipient_uei] into Vendor.recipient_uei
INSERT INTO Vendor.recipient_uei
(recipient_uei)
SELECT fk.[recipient_uei]
FROM ErrorLogging.FPDSstage2 as fk
LEFT OUTER JOIN Vendor.recipient_uei as pk
On pk.recipient_uei=fk.[recipient_uei]
WHERE pk.recipient_uei is NULL
GROUP BY fk.[recipient_uei]

	-- Insert new [parent_recipient_uei] into Vendor.recipient_uei
INSERT INTO Vendor.recipient_uei
(recipient_uei)
SELECT fk.dunsnumbertemp
FROM contract.fpds as fk
LEFT OUTER JOIN Vendor.recipient_uei as pk
On pk.recipient_uei=fk.dunsnumbertemp
WHERE pk.recipient_uei is NULL
GROUP BY fk.dunsnumbertemp

	-- Insert new [recipient_uei] into Vendor.Recipient_UEIhistory
INSERT INTO Vendor.Recipient_UEIhistory
(fiscal_year,dunsnumbertemp)
SELECT fk.fiscal_year,fk.recipient_uei
FROM contract.fpds as fk
LEFT OUTER JOIN Vendor.Recipient_UEIhistory as pk
On pk.recipient_uei=fk.recipient_uei and
		pk.fiscal_year=fk.fiscal_year 
WHERE pk.recipient_uei is NULL 
GROUP BY fk.dunsnumbertemp,fk.fiscal_year

update f
set dunsnumber=recipient_uei
	,recipient_uei=dunsnumbertemp
from contract.fpds f
where len(dunsnumber)=12 and len(recipient_uei) in (8,9)
and dunsnumbertemp is not null