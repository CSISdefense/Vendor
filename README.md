# Vendor

Working repository for acquisition trends and defense-industrial base analysis that is not specifically focused on international aspects.

Copyright: [Center for Strategic and International Studies (CSIS)](https://www.csis.org/), [Defense-Industrial Initiatives Group (DIIG)](https://www.csis.org/diig/).

Contributors: [Gregory Sanders](https://www.csis.org/people/gregory-sanders), Henry Carroll

## Data Sets

-   **fed_data**: Range of acquisition characteristics for U.S. federal contract data.
    -   SQL: 10h59m 12,872,101 rows
        -   Stored Procedure: [Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.StoredProcedure.sql)
        -   SQL Filters: @Customer==NULL, @SubCustomer = NULL, @PlatformPortfolio =NULL
        -   Download file: [Scripts / VendorSQL / OutputToFile.sql](https://github.com/CSISdefense/Vendor/blob/master/Scripts/VendorSQL/OutputToFile.sql)
        -   Zip from SQL: [Data / semi_clean / Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.zip](https://github.com/CSISdefense/Vendor/blob/master/Data/semi_clean/Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.zip)
        -   Last downloaded: June 19, 2025
        -   Processing File: [Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [analysis / Chartmaker / unaggregated_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/analysis/FPDS_chart_maker/unaggregated_FPDS.Rda)
        -   Catalog file: [Docs / catalog / unaggregated_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/unaggregated_FPDS.csv)
        -   Supporting variables: column Key: fed_ck, labels and colors: fed_lc.
        -   Superset reduced dataset: simple_fed_data
        -   Zip of csv for superset: [data / clean / simple_fed_data.zip](https://github.com/CSISdefense/Trade/blob/master/data/clean/simple_fed_data.zip)
    -   Subset: **def_data**:
        -   Range of acquisition characteristics for U.S. defense contract data.
        -   R Filters: Customer=="Defense"
        -   RDA file: [analysis / Chartmaker / unaggregated_def.Rda](https://github.com/CSISdefense/Vendor/blob/master/analysis/FPDS_chart_maker/unaggregated_def.Rda)
        -   Catalog file: [Docs / catalog / unaggregated_def.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/unaggregated_def.csv)
        -   Supporting variables: column Key: def_ck, labels and colors: def_lc.
        -   Superset reduced dataset: simple_def_data
        -   Zip of csv for superset: [data / clean / simple_def_data.zip](https://github.com/CSISdefense/Trade/blob/master/data/clean/simple_def_data.zip)
-   **platpscintl**: Detailed project, product or service type, and nationality date for U.S. federal contract data.
    -   SQL: 4h32m: 20,088,352 rows
        -   Stored Procedure: [Location.SP_ProdServPlatformAgencyPlaceOriginVendor](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Location.SP_ProdServPlatformAgencyPlaceOriginVendor.StoredProcedure.sql)
        -   SQL Filters: @Customer=NULL
        -   Download file: [Scripts / VendorSQL / OutputToFile.sql](https://github.com/CSISdefense/Vendor/blob/master/Scripts/VendorSQL/OutputToFile.sql)
        -   Zip from SQL: [Data / semi_clean / Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.zip)
        -   Last downloaded: June 7, 2025
        -   Processing File: [Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [Data / Clean / Federal_platpscintl_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/Federal_platpscintl_FPDS.Rda)
        -   Catalog file: [Docs / catalog / platpscintl_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/platpscintl_FPDS.csv)
        -   Supporting variables: column key: fedpsc_ck, labels and colors: fedpsc_lc.
    -   Subset: **platpscintl_def**
        -   Detailed project, product or service type, and nationality date for U.S. defense contract data.
        -   R Filters: ContractingCustomer=="Defense" after download
        -   RDA file: [Data / Clean / platpscintl_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/platpscintl_FPDS.Rda)
        -   Catalog file: [Docs / catalog / platpscintl_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/platpscintl_FPDS.csv)
        -   Supporting variables: column key: def_ck, labels and colors: def_lc.
-   **shipdef**: Detailed acquisition characteristic, project, product or service type, and nationality date for U.S. defense ships and submarines portfolio contract data.
    -   SQL: 5h29m: 1,312,476 rows
        -   Stored Procedure: [ProductOrServiceCode.SP_ShipsAndSubmarines](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/ProductOrServiceCode.SP_ShipsAndSubmarines.sql)
        -   Download file: [Scripts / VendorSQL / OutputToFile.sql](https://github.com/CSISdefense/Vendor/blob/master/Scripts/VendorSQL/OutputToFile.sql)
        -   SQL Filters: @IsDefense=1
        -   Zip from SQL: [Data / semi_clean / Defense_ProductOrServiceCode_SP_ShipsAndSubmarinesDetail.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//Defense_ProductOrServiceCode_SP_ShipsAndSubmarinesDetail.zip)
        -   Last downloaded: September 25, 2025
        -   Processing File: [Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [Data / Clean / Defense_Ship_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/Defense_Ship_FPDS.Rda)
        -   Catalog file: [Docs / catalog / Defense_Ship_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/Defense_Ship_FPDS.csv)
        -   Supporting variables: column key: ship_ck, labels and colors: ship_lc.
        -   Superset dataset: Defense_Ship_FPDS
        -   Zip of csv for superset: [data / clean / Defense_Ship_FPDS.zip](https://github.com/CSISdefense/Trade/blob/master/data/clean/Defense_Ship_FPDS.zip)
-   **munitiondef**: Detailed acquisition characteristic, project, product or service type, and nationality date for U.S. defense orndnance and missiles and air and missile defense portfolios contract data. Detailed project, product or service type, and nationality date for U.S. federal contract data.
    -   SQL: 17h08m: 385,011 rows
        -   Stored Procedure: [ProductOrServiceCode.SP_OrdnanceMissilesAirAndMissileDefense](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/ProductOrServiceCode.SP_OrdnanceMissilesAirAndMissileDefense.sql)
        -   Download file: [Scripts / VendorSQL / OutputToFile.sql](https://github.com/CSISdefense/Vendor/blob/master/Scripts/VendorSQL/OutputToFile.sql)
        -   SQL Filters: @IsDefense=1
        -   Zip from SQL: [Data / semi_clean / Defense_ProductOrServiceCode_SP_OrdnanceMissilesAirAndMissileDefenseDetail.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//Defense_ProductOrServiceCode_SP_OrdnanceMissilesAirAndMissileDefenseDetail.zip)
        -   Last downloaded: October 25, 2025
        -   Processing File: [Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [Data / Clean / Defense_Munition_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/Defense_Munition_FPDS.Rda)
        -   Catalog file: [Docs / catalog / Defense_Munition_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/Defense_Munition_FPDS.csv)
        -   Supporting variables: column key: munition_ck, labels and colors: munition_lc.
        -   Superset dataset: Defense_Munition_FPDS
        -   Zip of csv for superset: [data / clean / Defense_Munition_FPDS.zip](https://github.com/CSISdefense/Trade/blob/master/data/clean/Defense_Munition_FPDS.zip)
-   **ota_def**: Defense OTA transaction as provided in its entirity by SAM.gov
-   **def_kota**: Combined defense contract and defense OTA transactions for FY 2015 and later
    -   Merger: **ota_def** and **def_data** keeping overlapping columns.
    -   RDA file: [Data / Clean / def_kota.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/def_kota.Rda)
    -   Supporting variables: column Key: kota_ck; labels and colors: kota_lc.
-   **ruh**: Recipient Unique Entity IDs (UEI) and fiscal year dyads. UEIs are worksite level categorization for the federal acquisition industrial base. "ruh" stands for recipient UEI history.
    -   SQL: 20s: 3,217,659 rows
        -   View: [RecipientUEIhistory](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Vendor.RecipientUEIhistory.view.sql)
        -   Download file: [Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
        -   SQL Filters: IsPresent==1
        -   Zip from SQL: [Data / semi_clean / RecipientUEIhistory.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//RecipientUEIhistory.zip)
        -   Last downloaded: November 21, 2025
        -   Processing File: [Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [Data / Clean / RecipientUEI.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/RecipientUEI.Rda)
        -   Catalog file: [Docs / catalog / ruh.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/ruh.csv)
        -   Supporting variables: column key: ruh_ck, labels and colors: ruh_lc.
-   **pruh**: Parent Recipient UEIs and fiscal year dyads. Parent UEIs are the parent vendors as classified by reporting in FPDS. This is an imperfect systems, mergers are sometimes backdated to years before they actually took place. "pruh" stands for Parent recipient UEI history. These stats aggregate all of the information under a given parent UEI in a year. When a recipient UEI does not list a parent, it is treated as its own parent.
    -   SQL: 20s: 3,217,659 rows
        -   View: [RecipientUEIhistory](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Vendor.RecipientUEIhistory.view.sql)
        -   Download file: [Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
        -   Zip from SQL: [Data / semi_clean / RecipientUEIhistory.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//RecipientUEIhistory.zip)
        -   Last downloaded: November 21, 2025
        -   Processing File: [Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [Data / Clean / RecipientUEI.rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/RecipientUEI.rda)
        -   Catalog file: [Docs / catalog / pruh.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/pruh.csv)
        -   Supporting variables: column key: pruh_ck, labels and colors: pruh_lc.

## Analysis

-   **industrial_base_ruh_pruh**: Analyzes parent vendor counts, as measured by parent UEIs. Applies minimum thresholds for reporting to adjust for inconsistent standards over the years.
    -   Datasets employed: puh, pruh
    -   Analysis file: [Analysis / industrial_base_ruh_pruh.Rmd](https://github.com/CSISdefense/Vendor/blob/master/analysis/industrial_base_ruh_pruh.Rmd)
    -   Knit markdown file: [Analysis / industrial_base_ruh_pruh.md](https://github.com/CSISdefense/Vendor/blob/master/analysis/industrial_base_ruh_pruh.md)
    -   Knit html file: [Analysis / industrial_base_ruh_pruh.html](https://github.com/CSISdefense/Vendor/blob/master/analysis/industrial_base_ruh_pruh.html)
