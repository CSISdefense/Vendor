# Vendor

Working repository for acquisition trends and defense-industrial base analysis that is not specifically focused on international aspects.

Copyright: [Center for Strategic and International Studies (CSIS)](https://www.csis.org/), [Defense-Industrial Initiatives Group (DIIG)](https://www.csis.org/diig/).

Contributors: [Gregory Sanders](https://www.csis.org/people/gregory-sanders), Henry Carroll

## Data Sets
-   **fed_data**: Range of acquisition characteristics for U.S. federal contract data.
    -   SQL: 10h59m   12,872,101 rows 
        -   Stored Procedure: [Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.StoredProcedure.sql)
        -   SQL Filters: @Customer==NULL, @SubCustomer = NULL, @PlatformPortfolio =NULL
        -   Zip from SQL: [Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.zip](https://github.com/CSISdefense/Vendor/blob/master/Data/semi_clean/Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.zip)
        -   Last downloaded: June 19, 2025
        -   Processing File: [FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [unaggregated_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/analysis/FPDS_chart_maker/unaggregated_FPDS.Rda)
        -   Catalog file: [unaggregated_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/unaggregated_FPDS.csv)
        -   Supporting variables: column Key: fed_ck, labels and colors: fed_lc.
    -   Subset:  **def_data**: 
        -   Range of acquisition characteristics for U.S. defense contract data.
        -   R Filters: Customer=="Defense"
        -   RDA file: [unaggregated_def.Rda](https://github.com/CSISdefense/Vendor/blob/master/analysis/FPDS_chart_maker/unaggregated_def.Rda)
        -   Catalog file: [unaggregated_def.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/unaggregated_def.csv)
        -   Supporting variables: column Key: def_ck, labels and colors: def_lc.

-   **platpscintl**: Detailed project, product or service type, and nationality date for U.S. federal contract data.
    -   SQL: 4h32m: 20,088,352 rows
        -   Stored Procedure: [Location.SP_ProdServPlatformAgencyPlaceOriginVendor](https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Location.SP_ProdServPlatformAgencyPlaceOriginVendor.StoredProcedure.sql)
        -   SQL Filters: @Customer=NULL
        -   Zip from SQL: [Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.zip](https://github.com/CSISdefense/Vendor/blob/master/Data//semi_clean//Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.zip)
        -   Last downloaded: June 7, 2025
        -   Processing File: [FPDS_data_processing.R](https://github.com/CSISdefense/Vendor/blob/master/Scripts/FPDS_data_processing.R)
    -   Complete:
        -   RDA file: [Federal_platpscintl_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/Federal_platpscintl_FPDS.Rda)
        -   Catalog file: [platpscintl_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/platpscintl_FPDS.csv)
        -   Supporting variables: column key: fedpsc_ck, labels and colors: fedpsc_lc.
    -   Subset: **platpscintl_def** 
        -   Detailed project, product or service type, and nationality date for U.S. defense contract data.
        -   R Filters: ContractingCustomer=="Defense" after download
        -   RDA file: [platpscintl_FPDS.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/platpscintl_FPDS.Rda)
        -   Catalog file: [platpscintl_FPDS.csv](https://github.com/CSISdefense/Vendor/blob/master/Docs/catalog/platpscintl_FPDS.csv)
        -   Supporting variables: column key: def_ck, labels and colors: def_lc.
-   **def_kota**: Combined defense contract and defense OTA transactions for FY 2015 and later
    -   Merger: **ota_def** and **def_data** keeping overlapping columns.
    -   RDA file: [def_kota.Rda](https://github.com/CSISdefense/Vendor/blob/master/Data/Clean/def_kota.Rda)
    -   Supporting variables: column Key: kota_ck; labels and colors: kota_lc.

## Analysis
