# Metrics for reproducibility

This repo contains all R-code used to prepare and/or extract data used in our review of currently used or suggested metrics to quantify reproducibility. The review was pre-registered on the [Open Science Framework](https://osf.io/j65wb).

## Zotero library
All literature used at any point in our review is uploaded and curated in the following [Zotero library](https://www.zotero.org/groups/5397531/reproducibilitymetrics/library).

## Repository content: 

- The `application_papers` folder includes a folder with the our call for application, a`.tex` file for of the call document which was subsequently published on the [Open Science Framework](https://osf.io/a2wrj) and shared in the [FORRT community](https://forrt.org/) slack. Another sub folder `data_extraction` includes the `data_extract_clean.csv` which was produced using the `prep_data.R` script since the SyRF data extraction export was not formatted nice enough to be useful. Another `R` script (`summary_table.R`) uses the clean data to produce some summary statistics. 

- This data extraction (currently) only includes the application paper part of the review.

- The `zotero` folder includes the `.xml` files of all application papers (`ReproducibilityMetrics.xml` and `truststudy.xml`), and the methodological papers extracted from scopus (`scopus_20240513.xml`) and ebsco (`scopus_20240513.xml`) in May 2024. The functions from `functions.R` are used in the `script_to_translate_xml_to_syrfCSV.R` to translate the `.xml` files to a `.csv` that can be imported into SyRF. Note that for the methodological papers the script also includes code to de-duplicate the hits based on their DOI (the first hit is retained).