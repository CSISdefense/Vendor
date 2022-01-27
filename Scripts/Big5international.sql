select *
from contractor.DunsnumberToParentContractorHistory dtpch
left outer join contractor.ParentContractor p
on dtpch.parentid=p.parentid
where p.big5=1 and 
dtpch.inter

select vendorname
,dunsnumber
,parentdunsnumber
,ParentID
,VendorIsoAlpha3
,OriginIsoAlpha3
,PlaceIsoAlpha3
,placeofmanufactureText
,sum(obligatedAmount) as obligatedAmount
from contract.FPDSpartial
where vendorsize='Big 5'
and VendorIsInternational=1
group by vendorname
,dunsnumber
,parentdunsnumber
,ParentID
,VendorIsoAlpha3
,OriginIsoAlpha3
,PlaceIsoAlpha3
,placeofmanufactureText