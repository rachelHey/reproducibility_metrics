# Metrics for reproducibility

This repo contains all R-code used to prepare and/or extract data used in our review of currently used or suggested metrics to quantify reproducibility. The review was pre-registered on the [Open Science Framework](https://osf.io/j65wb).

## Zotero library
All literature used at any point in our review is uploaded and curated in Zotero: 

- the literature included in our review after screening is [here](https://www.zotero.org/groups/5397531/reproducibilitymetrics/library),  
- and the records included and screened in various review steps are [here](https://www.zotero.org/groups/5630395/reproducibilitymetrics_methodsscreening/library).

## Repository content: 

- The `application_papers` folder includes 

  - a folder with our call for application (`call_for_collab/`): a`.tex` file for of the call document which was subsequently published on the [Open Science Framework](https://osf.io/a2wrj) and shared in the [FORRT community](https://forrt.org/) slack. 
  - a sub folder `data_extraction/` including the `data_extract_clean.csv` which was produced using the `prep_data.R` script since the SyRF data extraction export was not formatted nice enough to be useful. The individual data extractions in `data_extract_clean.csv` were merged in  `data_extract_clean_MERGED.csv`. The `R` script (`summary_table.R`) uses the clean and merged data to produce some summary statistics on the application papers. 

- The `methodologyical_papers` folder includes  
  - two sub-folders with `R` scripts to produce the screening data and the data for the screening test. An additional sub-folder  `snowballing` includes the `R` script for the snowballing (`forward_backward.R`).
  - a script to prepare the methods papers for final screening (`prep_all_methods.R`) and a script to put together the finally included methods papers for data extraction (`final_methods_list.R`).
  
- The `zotero` folder includes the functions `functions.R` used in the script `script_to_translate_xml_to_syrfCSV.R` to translate the `.xml` files to a `.csv` that can be imported into SyRF. Note that for the methodological papers the script also includes code to de-duplicate the hits based on their DOI (the first hit is retained).

Note that all `.xml` files and most `.CSV` are currently shared with the project collaborators mainly via the common GDrive - this will be updated, once the paper is ready for submission.