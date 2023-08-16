--26s
select top 1000
parentdunsnumber,recipient_parent_uei
from contract.fpds f
where len(parentdunsnumber)<12 and len(recipient_parent_uei) in (8,9) and
parentdunsnumber<>recipient_parent_uei
group by parentdunsnumber,recipient_parent_uei


--26s
select count(*),
len(parentdunsnumber),len(recipient_parent_uei) 
from contract.fpds f
where len(parentdunsnumber)=12 or len(recipient_parent_uei) in (8,9)
group by len(parentdunsnumber),len(recipient_parent_uei) 

alter table contract.fpds 
add parent_UEItemp varchar(12)

--Roughly 1 million rows an hour (2.2m at 1h57m)
update f
set parent_UEItemp=parentdunsnumber
from contract.fpds f
where len(parentdunsnumber)=12 and len(recipient_parent_uei) in (8,9)


--1h08m
select count(*)
from contract.fpds f
where len(parentdunsnumber)=12 and len(recipient_parent_uei) in (8,9) and  parent_UEItemp is null


--***********parentdunsnumber, not present in new downloads*************
-- 'Insert new parentdunsnumber into [Contractor].[parentdunsnumber]'
INSERT INTO [Contractor].[dunsnumber] (dunsnumber)
SELECT
ErrorTable.recipient_parent_uei
FROM
contract.fpds AS ErrorTable
LEFT OUTER JOIN [Contractor].[dunsnumber]  AS PKTable
ON
PKTable.dunsnumber=ErrorTable.recipient_parent_uei
WHERE ErrorTable.recipient_parent_uei is not null and PKTable.dunsnumber is null and
 len(errortable.parentdunsnumber)=12 and len(errortable.recipient_parent_uei) in (8,9)
GROUP BY ErrorTable.recipient_parent_uei

SELECT
ErrorTable.recipient_parent_uei
FROM
contract.fpds AS ErrorTable
LEFT OUTER JOIN [Contractor].[dunsnumber]  AS PKTable
ON
PKTable.dunsnumber=ErrorTable.recipient_parent_uei
WHERE PKTable.dunsnumber is null and
 len(errortable.parentdunsnumber)=12 and len(errortable.recipient_parent_uei) in (8,9)

-- 'Insert new dunsnumber and fiscal_year into [Contractor].[DunsnumberToParentContractorHistory]'
--3h56m
INSERT INTO [Contractor].DunsnumberToParentContractorHistory(dunsnumber,FiscalYear)
SELECT
Errortable.recipient_parent_uei,
errortable.fiscal_year
FROM contract.FPDS AS ErrorTable
LEFT OUTER JOIN [Contractor].DunsnumberToParentContractorHistory  AS PKTable
ON
PKTable.dunsnumber=ErrorTable.recipient_parent_uei
and PKTable.FiscalYear=ErrorTable.fiscal_year
WHERE ErrorTable.recipient_parent_uei is not null and PKTable.dunsnumber is null and
 len(errortable.parentdunsnumber)=12 and len(errortable.recipient_parent_uei) in (8,9)
GROUP BY ErrorTable.recipient_parent_uei, errortable.fiscal_year

--Finished!

-- Insert new [parent_recipient_uei] into Vendor.uei
--1hr46m
INSERT INTO Vendor.uei
(uei)
SELECT fk.parent_UEItemp
FROM contract.fpds as fk
LEFT OUTER JOIN Vendor.uei as pk
On pk.uei=fk.parent_UEItemp
WHERE pk.uei is NULL and fk.parent_UEItemp is not null and
 len(fk.parentdunsnumber)=12 and len(fk.recipient_parent_uei) in (8,9)
GROUP BY fk.parent_UEItemp

	-- Insert new [recipient_uei] into Vendor.ueihistory
INSERT INTO Vendor.ueihistory
(fiscal_year,uei)
SELECT fk.fiscal_year,fk.parent_UEItemp
FROM contract.fpds as fk
LEFT OUTER JOIN Vendor.ueihistory as pk
On pk.uei=fk.parent_UEItemp and
		pk.fiscal_year=fk.fiscal_year 
WHERE pk.uei is NULL and
 len(fk.parentdunsnumber)=12 and len(fk.recipient_parent_uei) in (8,9)
GROUP BY fk.parent_UEItemp,fk.fiscal_year

select top 1000 recipient_parent_uei, parent_UEItemp, parentdunsnumber
from contract.fpds f
where len(parentdunsnumber)=12 and len(recipient_parent_uei) in (8,9)

--Stopping run here
--Seems dramatically faster. 
--The UPDATE statement conflicted with the FOREIGN KEY constraint "fk_contract_fpds_recipient_UEIhistory". The conflict occurred in database "CSIS360", table "Vendor.ueihistory".

--Termianted at 1hr6m
update f
set parentdunsnumber=recipient_parent_uei
	,recipient_parent_uei=parent_UEItemp
from contract.fpds f
where len(parentdunsnumber)=12 and len(recipient_parent_uei) in (8,9)
