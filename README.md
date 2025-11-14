# Vendor

Working repository for acquisition trends and defense-industrial base analysis that is not specifically focused on international aspects.

Copyright: [Center for Strategic and International Studies (CSIS)](https://www.csis.org/), [Defense-Industrial Initiatives Group (DIIG)](https://www.csis.org/diig/).

Contributors: [Gregory Sanders](https://www.csis.org/people/gregory-sanders), Henry Carroll

## Data Sets

-   **fed_data**: Range of acquisition characteristics for U.S. federal contract data.
    -   SQL: 10h59m 12,872,101 rows
        -   Stored Procedure: [Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.StoredProcedure.sql)
        -   SQL Filters: @Customer==NULL, @SubCustomer = NULL, @PlatformPortfolio =NULL
        -   Zip from SQL: [ Data / semi_clean / Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.zip](https://github.com/CSISdefense/Vendor/blob/master/Data/semi_clean/Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.zip)
        -   Last downloaded: June 19, 2025
        -   Processing File: [ Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [ analysis / Chartmaker / unaggregated_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/analysis/FPDS_chart_maker/unaggregated_FPDS.Rda)
        -   Catalog file: [ Docs / catalog / unaggregated_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/unaggregated_FPDS.csv)
        -   Supporting variables: column Key: fed_ck, labels and colors: fed_lc.
        -   Superset reduced dataset: simple_fed_data
        -   Zip of csv for superset: [data / clean / simple_fed_data.zip](https://github.com/CSISdefense/Trade/blob/master/data/clean/simple_fed_data.zip)
    -   Subset: **def_data**:
        -   Range of acquisition characteristics for U.S. defense contract data.
        -   R Filters: Customer=="Defense"
        -   RDA file: [ analysis / Chartmaker / unaggregated_def.Rda](https://github.com/CSISdefense/Vendor/blob/master/analysis/FPDS_chart_maker/unaggregated_def.Rda)
        -   Catalog file: [ Docs / catalog / unaggregated_def.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/unaggregated_def.csv)
        -   Supporting variables: column Key: def_ck, labels and colors: def_lc.
        -   Superset reduced dataset: simple_def_data
        -   Zip of csv for superset: [data / clean / simple_def_data.zip](https://github.com/CSISdefense/Trade/blob/master/data/clean/simple_def_data.zip)
-   **platpscintl**: Detailed project, product or service type, and nationality date for U.S. federal contract data.
    -   SQL: 4h32m: 20,088,352 rows
        -   Stored Procedure: [Location.SP_ProdServPlatformAgencyPlaceOriginVendor](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Location.SP_ProdServPlatformAgencyPlaceOriginVendor.StoredProcedure.sql)
        -   SQL Filters: @Customer=NULL
        -   Zip from SQL: [ Data / semi_clean / Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.zip)
        -   Last downloaded: June 7, 2025
        -   Processing File: [ Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [ Data / Clean / Federal_platpscintl_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/Federal_platpscintl_FPDS.Rda)
        -   Catalog file: [ Docs / catalog / platpscintl_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/platpscintl_FPDS.csv)
        -   Supporting variables: column key: fedpsc_ck, labels and colors: fedpsc_lc.
    -   Subset: **platpscintl_def**
        -   Detailed project, product or service type, and nationality date for U.S. defense contract data.
        -   R Filters: ContractingCustomer=="Defense" after download
        -   RDA file: [ Data / Clean / platpscintl_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/platpscintl_FPDS.Rda)
        -   Catalog file: [ Docs / catalog / platpscintl_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/platpscintl_FPDS.csv)
        -   Supporting variables: column key: def_ck, labels and colors: def_lc.
-   **shipdef**: Detailed acquisition characteristic, project, product or service type, and nationality date for U.S. defense ships and submarines portfolio contract data.
    -   SQL: 5h29m: 1,312,476 rows
        -   Stored Procedure: [ProductOrServiceCode.SP_ShipsAndSubmarines](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/ProductOrServiceCode.SP_ShipsAndSubmarines.sql)
        -   SQL Filters: @IsDefense=1
        -   Zip from SQL: [ Data / semi_clean / Defense_ProductOrServiceCode_SP_ShipsAndSubmarinesDetail.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//Defense_ProductOrServiceCode_SP_ShipsAndSubmarinesDetail.zip)
        -   Last downloaded: September 25, 2025
        -   Processing File: [ Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [ Data / Clean / Defense_Ship_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/Defense_Ship_FPDS.Rda)
        -   Catalog file: [ Docs / catalog / Defense_Ship_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/Defense_Ship_FPDS.csv)
        -   Supporting variables: column key: ship_ck, labels and colors: ship_lc.
        -   Superset dataset: Defense_Ship_FPDS
        -   Zip of csv for superset: [data / clean / Defense_Ship_FPDS.zip](https://github.com/CSISdefense/Trade/blob/master/data/clean/Defense_Ship_FPDS.zip)
-   **munitiondef**: Detailed acquisition characteristic, project, product or service type, and nationality date for U.S. defense orndnance and missiles and air and missile defense portfolios contract data.
Detailed project, product or service type, and nationality date for U.S. federal contract data.
    -   SQL: 17h08m: 385,011 rows
        -   Stored Procedure: [ProductOrServiceCode.SP_OrdnanceMissilesAirAndMissileDefense](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/ProductOrServiceCode.SP_OrdnanceMissilesAirAndMissileDefense.sql)
        -   SQL Filters: @IsDefense=1
        -   Zip from SQL: [ Data / semi_clean / Defense_ProductOrServiceCode_SP_OrdnanceMissilesAirAndMissileDefenseDetail.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//Defense_ProductOrServiceCode_SP_OrdnanceMissilesAirAndMissileDefenseDetail.zip)
        -   Last downloaded: October 25, 2025
        -   Processing File: [ Scripts / FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [ Data / Clean / Defense_Munition_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/Defense_Munition_FPDS.Rda)
        -   Catalog file: [ Docs / catalog / Defense_Munition_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/Defense_Munition_FPDS.csv)
        -   Supporting variables: column key: munition_ck, labels and colors: munition_lc.
        -   Superset dataset: Defense_Munition_FPDS
        -   Zip of csv for superset: [data / clean / Defense_Munition_FPDS.zip](https://github.com/CSISdefense/Trade/blob/master/data/clean/Defense_Munition_FPDS.zip)
-   **ota_def**: Defense OTA transaction as provided in its entirity by SAM.gov
-   **def_kota**: Combined defense contract and defense OTA transactions for FY 2015 and later
    -   Merger: **ota_def** and **def_data** keeping overlapping columns.
    -   RDA file: [ Data / Clean / def_kota.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/def_kota.Rda)
    -   Supporting variables: column Key: kota_ck; labels and colors: kota_lc.

## Analysis
