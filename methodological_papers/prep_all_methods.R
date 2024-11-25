# Script to collect all methods papers in a table
# author: R. Heyard

# Libraries needed

library(tidyverse)
library(here)
library(janitor)

## 1. Get the methods papers, other than interesting application papers,
# that made it through the screening -----------------------------------
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

## 2. Get the papers cited in the large-scale application papers
# --------------------------------------------------------------
# Get the data:
dois_application_papers_merged <-
  read_csv("https://osf.io/download/ubmyt/") %>%
  clean_names() %>% pull(paste_the_doi_of_all_paper_s)

dois_application_papers_merged <-
  str_replace_all(str_replace_all(dois_application_papers_merged,
                                  " \\s*\\[[^\\]]+\\]", " "),
                  "\\s*\\([^\\)]+\\)", "")
dois_application_papers_merged <-
  dois_application_papers_merged[which(!is.na(dois_application_papers_merged))]

dois_application_papers_merged <-
  unlist(str_split(unlist(str_split(unlist(str_split(
    dois_application_papers_merged, ";")), ",")), " "))

dois_application_papers_merged <-
  dois_application_papers_merged[which(!dois_application_papers_merged %in%
                                         c("", "No", "DOI"))]

dois_application_papers_merged <-
  str_remove(dois_application_papers_merged, "https://doi.org/") %>%
  unique()

# delete the osf.io once, as they are all applications:
dois_application_papers_merged <-
  dois_application_papers_merged[which(
    !grepl("osf.io", dois_application_papers_merged))]


## 3. Get the papers cited/references in or by the included methods papers, that
# seemed relevant after screening, by RH ---------------------------------------
# --------------------------------------
# Get the screened data file
methods_snowball_papers_screened <-
  read_csv(here("methodological_papers",
                "snowballing",
                "screening_snowball_methods.csv"))
# Get rid of any more duplicates
methods_snowball_papers_screened <- methods_snowball_papers_screened %>%
  select(StudyId, Title, Authors, PublicationName, Abstract,
         Url, Year, Doi, ScreeningStatus) %>%
  clean_names() %>%
  mutate(keep = TRUE) %>%
  group_by(tolower(title)) %>%
  arrange(tolower(title), doi) %>%
  mutate(n = 1:n()) %>%
  ungroup() %>%
  mutate(keep =
           case_when(n > 1 ~ FALSE,
                     TRUE ~ keep)) %>%
  filter(keep)

methods_snowball_papers_included <- methods_snowball_papers_screened %>%
  filter(screening_status == "Included") %>%
  select(-keep, -n, -`tolower(title)`)

## 4. Get the papers cited by the interesting application papers
# --------------------------------------------------------------
# Get the column with the DOIs
dois_data_extraction_interesting_application_papers <-
  read_csv("https://osf.io/download/eabps/") %>%
  clean_names() %>%
  filter(!is.na(paste_the_doi_of_all_paper_s)) %>%
  # Delete this one paper, as factor replicability is not a type repro we are
  # looking for
  filter(doi != "10.1016/j.paid.2015.01.004") %>%
  pull(paste_the_doi_of_all_paper_s)

# Seperate all dois that were added together
dois_data_extraction_interesting_application_papers <-
  unlist(str_split(unlist(str_split(unlist(str_split(
    dois_data_extraction_interesting_application_papers, ";")), ",")), " "))
# Get rid of the different doi.orgs to keep a unified DOI record
dois_data_extraction_interesting_application_papers <-
  str_remove(dois_data_extraction_interesting_application_papers,
             "https://doi.org/")
dois_data_extraction_interesting_application_papers <-
  str_remove(dois_data_extraction_interesting_application_papers,
             "http://dx.doi.org/")
dois_data_extraction_interesting_application_papers <-
  str_remove(dois_data_extraction_interesting_application_papers,
             "http://doi.org/")
# Delete the empty ones
dois_data_extraction_interesting_application_papers <-
  dois_data_extraction_interesting_application_papers[which(
    dois_data_extraction_interesting_application_papers != "")]

# Some have a . at the end. Delete it, and unique
dois_data_extraction_interesting_application_papers <-
  gsub("\\.*$", "", dois_data_extraction_interesting_application_papers) %>%
  unique()

## 5. Merge all together, and get rid of any duplicates
# -----------------------------------------------------

# Merge DOIs of 2. and 4 together
dois_methods_data_extractions <-
  c(dois_application_papers_merged,
    dois_data_extraction_interesting_application_papers) %>%
  unique()

all_methods_screened <-
  methods_papers_included %>%
  bind_rows(methods_snowball_papers_included) %>%
  mutate(title = str_to_title(title)) %>%
  select(-study_id) %>%
  unique()

all_methods_screened <- all_methods_screened %>%
  filter(!is.na(doi)) %>%
  bind_rows(
    all_methods_screened %>%
      filter(is.na(doi)) %>%
      # Hand-code dois
      mutate(doi = c("10.1111/stan.12312",
                     "10.48550/arXiv.2203.03443",
                     # The third is an application paper
                     NA,
                     "10.18653/v1/2022.acl-long.2",
                     "DOI unfindable"))) %>%
  filter(!is.na(doi)) %>%
  select(title, authors, doi) %>%
  unique()

# FOR LATER:
all_methods_screened %>% filter(doi == "DOI unfindable")
# 1 Replikačné Štúdie V Psychológii: Pojednanie O Dvoch Dôležitých Otázkách = Replication Studies In Psychology: A Discourse On Two Important Issues Kačmár Pavol ; Adamkovič Matúš

# PASTE ALL DOIS together and get rid of duplicates:

all_methods_dois <-
  c(dois_methods_data_extractions,
    all_methods_screened$doi) %>%
  unique()

# They were all uploaded to Zotero, and 121 manually uploaded.
paste0(all_methods_dois[-121], collapse = " ")

# NOT FOUND | and decided to ignore:
# 10.1016/S0140-673690837-8
# 10.1016%2Fj.jcm.2016.02.012 #  A guideline for selecting and reporting intraclass correlation
# coefficients for reliability research.

# Got the XML from Zotero - extract those that were not screened yet - transform to CSV for SyRF:
library(XML)
source("zotero/functions.R")

# GET DATA FOR APPLICATION PAPERS READY FOR SYRF:
data <- read_xml(here("XMLs/FinalMethods4Screening.xml"))

# those without doi, are all part of the ones that were already screened
data %>%
  filter(is.na(doi) | (doi %in% methods_papers_included$doi)) %>%
  write_for_syrf(filename = here("CSVs_for_SyRF",
                                 "FinalMethods4Screening_alreadyscreened.csv"))

data %>%
  filter(!is.na(doi) & (!doi %in% methods_papers_included$doi)) %>%
  write_for_syrf(filename = here("CSVs_for_SyRF",
                                 "FinalMethods4Screening_norscreenedyet.csv"))
