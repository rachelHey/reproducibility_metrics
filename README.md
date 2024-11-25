# Metrics for reproducibility

This repo contains all R-code used to prepare and/or extract data used in our review of currently used or suggested metrics to quantify reproducibility. The review was pre-registered on the [Open Science Framework](https://osf.io/j65wb).

## Zotero library
All literature used at any point in our review is uploaded and curated in Zotero: 

- the literature included in our review after screening is [here](https://www.zotero.org/groups/5397531/reproducibilitymetrics/library),  
- and the records included and screened in various review steps are [here](https://www.zotero.org/groups/5630395/reproducibilitymetrics_methodsscreening/library).

## Repository content: 

- The `application_papers` folder includes 

  - a folder with our call for application (`call_for_collab/`): a`.tex` file for of the call document which was subsequently published on the [Open Science Framework](https://osf.io/a2wrj) and shared in the [FORRT community](https://forrt.org/) slack. 
  - a sub-folder `data_extraction/` including the `prep_data.R` script that changed the SyRF data extraction export (the raw data) into a useful version, now on the OSF (see below). The `R` script, `summary_table.R`, uses the clean and merged data to produce some summary statistics on the application papers. 

- The `methodological_papers` folder includes  
  - Sub-folders with `R` scripts to produce the screening data (`screening_raw_data/`), the data for the screening test (`screening_test/`). An additional sub-folder,  `snowballing/`, includes the `R` script for the snowballing (`forward_backward.R`) and additional scripts for the interesting application papers (in `data_extraction_interesting_applications/`).  
  - a sub-folder, `data_extraction/`, with scripts to prepare the data extraction, `prep_data_methodological_extraction.R`, and organise the extracted data into summary statistics, `summary_stats_methods.R`.
  - a script to prepare the methods papers for final screening (`prep_all_methods.R`) and a script to put together the finally included methods papers for data extraction (`final_methods_list.R`) .
  
- The `zotero` folder includes the functions `functions.R` used in the script `script_to_translate_xml_to_syrfCSV.R` to translate the `.xml` files to a `.csv` that can be imported into SyRF. Note that for the methodological papers the script also includes code to de-duplicate the hits based on their DOI (the first hit is retained).

Note that all `.xml` files and some `.CSV` are currently shared with the project collaborators mainly via the common GDrive - the most relevant CSVs are shared via the Open Science Framework: [osf.io/sbcy3/](https://osf.io/sbcy3/).