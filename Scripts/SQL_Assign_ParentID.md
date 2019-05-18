Contractor.SP_CreateParentID
* Enter value of DUNSnumber, parentid, startyear, and endyear

Contractor.SP_AssignParentID
* Enter same values of DUNSnumber, Parentid, startyear, and endyear

Contractor.SP_AssignRevenueAndJV
* Look online (google, company annual reports) for most recent annual revenue for the company
* Fill in value for RevenueInMillions
* SizeGuess and JointVenture: 1,0 for yes,no
* Owners: Names of the companies IF a jointventure



If you have a company that's a subsidairy of another. If the subsidary was never independent (e.g. Clark Construction which is a division of Clark Enterprises), then it can be merged with the parentID.

* If there is an existing ParentID for the owner company, use Vendor.SP_MergeParentID
* If there is no existing ParentID then use Vendor.SP_RenameParentID to change the name of the ParentID to that of the parent firm.

Companies that are owned by venture capital firms are a special case and difficult to puzzle out. For now, as a general rule, don't treat venture capital funds like Veritas, Cerberus, the Carlyle Group as owners. Do go ahead and check with an RA/Fellow.

For companies that were once independent, but are now owned by a different firm, look up the merger completion date and use  Vendor.sp_AssignSubsidiaryStatus

For companies that were once part of a larger firm, but are now independent, use Vendor.sp_AssignSpinoffStatus.

For companies that you cannot find, use Vendor.sp_CreateParentIDdunsnumber to create a new parentID using a Dunsnumber or vendor.sp_CreateParentIDstandardizedVendorName to do the same using a standardized vendorname.