# Vendor

Working repository for acquisition trends and defense-industrial base analysis that is not specifically focused on international aspects.

Copyright: [Center for Strategic and International Studies (CSIS)](https://www.csis.org/), [Defense-Industrial Initiatives Group (DIIG)](https://www.csis.org/diig/).

Contributors: [Gregory Sanders](https://www.csis.org/people/gregory-sanders)

## Data Sets

-   platpscintl_def: Range of acquisition characteristics for U.S. defense contract data.
    -   SQL: 4h32m: 20,088,352 rows
      - Stored Procedure: [Location.SP_ProdServPlatformAgencyPlaceOriginVendor]("https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Location.SP_ProdServPlatformAgencyPlaceOriginVendor.StoredProcedure.sql")
      - Filters: @Customer=NULL
      - FIltered down to ContractingCustomer=="Defense" after download
      - Last downloaded: June 7, 2025
    -   Files:
        -   Text from SQL: [Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.txt]("Data//semi_clean//Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.txt")
        -   RDA file: [platpscintl_FPDS.Rda.Rda]("data/clean/platpscintl_FPDS.Rda.Rda")
        -   Catalog file: [platpscintl_FPDS.Rda.csv]("Docs/catalog/platpscintl_FPDS.csv")
    -   Supporting variables: column Key: def_ck, labels and colors: def_lc,
-   def_data: Detailed project, product or service type, and nationality date  for U.S. defense contract data.
    -   SQL: 6,127,978 rows 8h59m
      - Stored Procedure: [Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength]("https://github.com/CSISdefense/DIIGsql/blob/master/SQL/Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.StoredProcedure.sql")
      - Filters: @Customer=='Defense', @SubCustomer = NULL, @PlatformPortfolio =NULL
      - Last downloaded: June 7, 2025
    -   Files:
        -   Text from SQL: [Defense_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.txt](Data//semi_clean//Defense_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.txt)
        -   RDA file: [unaggregated_def.Rda]("analysis/FPDS_chart_maker/unaggregated_def.Rda")
        -   Catalog file: [unaggregated_def.csv]("Docs/catalog/unaggregated_def.csv")
    -   Supporting variables: column Key: def_ck, labels and colors: def_lc,
-   def_kota: Combined contract and OTA transactions
    -   Filters: ContractingCustomer=='Defense'
    -   RDA file: [def_kota.Rda](%22data/clean/def_kota.Rda%22)
    -   Column Key: kota_ck
    -   Labels and Colors: kota_lc,

## Analysis
