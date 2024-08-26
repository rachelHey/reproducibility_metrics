# Script to put together the final list of methods papers
# author: R. Heyard

# Libraries needed

library(tidyverse)
library(here)
library(janitor)

## 1. Get the methods papers, other than interesting application papers,
# that made it through the FIRST screening -----------------------------
# ----------------------------------
# Get all cleaned data:
methods_papers_screened <- read_csv(here("methodological_papers",
                                         "screening_raw_data",
                                         "data_screening_clean.csv"))
# Get rid of any more duplicates
methods_papers_screened <- methods_papers_screened %>%
  mutate(keep = TRUE) %>%
  group_by(tolower(title)) %>%
  arrange(tolower(title), doi) %>%
  mutate(n = 1:n()) %>%
  ungroup() %>%
  mutate(keep =
           case_when(n > 1 ~ FALSE,
                     TRUE ~ keep)) %>%
  filter(keep) %>%
  select(study_id, title, authors, publication_name, abstract, url, year, doi,
         screening_status, interesting_application_paper)

methods_papers_included <- methods_papers_screened %>%
  filter(screening_status == "Included", !interesting_application_paper) %>%
  select(-interesting_application_paper)



## 2. Get the methods papers, that made it through the SECOND screening
# ---------------------------------------------------------------------
# Get all cleaned data:
final_screening_raw <-
  read_csv(here("methodological_papers",
                "screening_raw_data",
                "Final_Screening_data_2024_08_08_long_format.csv")) %>%
  clean_names()
# quick test:
final_screening_raw %>% select(study_id) %>% distinct() %>% nrow()
final_screening_raw %>% group_by(study_id) %>%
  summarise(n = n_distinct(investigator_name)) %>% count(n)

# who?
final_screening_raw %>% count(investigator_name)

final_screening_included <- final_screening_raw %>%
  select(study_id, title, authors, publication_name, abstract, url, year,
         doi, screening_status) %>%
  distinct() %>%
  filter(screening_status == "Included")


## 3. Merge all methods paper together:
all_methods_included <- methods_papers_included %>%
  bind_rows(final_screening_included)

# Get them into Zotero via DOI:
# paste0(all_methods_included$doi, collapse = " ")
# without DOI:
all_methods_included %>%
  filter(is.na(doi)) %>%
  select(title, authors)


# Extract xml from Zotero and produce CSV for SyRF:
library(XML)
source(here("zotero", "functions.R"))

data_methods <- read_xml(here("XML",
                              "final_list_of_97_methods_papers.xml"))

write_for_syrf(data_methods, filename = here("CSVs_for_SyRF",
                                             "syrf_97_methods_papers.csv"))



