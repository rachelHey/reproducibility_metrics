# Script to prepare for snowballing, e.g. forward and backward tracking
# The interesting application papers will be done by "hand"
# The methods papers using OpenAlex

# https://cran.r-project.org/web/packages/citationchaser/index.html
library(citationchaser)
library(tidyverse)
library(here)
library(openalexR)
library(dplyr)
library(XML)
# Get function to change XML to SyRF CSV
source(here("zotero", "functions.R"))

# Get all cleaned data:
screening_clean <- read_csv(here("methodological_papers/",
                                 "screening_raw_data",
                                 "data_screening_clean.csv"))
# Get rid of any more duplicates
screening_clean <- screening_clean %>%
  mutate(keep = TRUE) %>%
  group_by(tolower(title)) %>%
  arrange(tolower(title), doi) %>%
  mutate(n = 1:n()) %>%
  ungroup() %>%
  mutate(keep =
           case_when(n > 1 ~ FALSE,
                     TRUE ~ keep))
# for the flow-chart:
screening_clean %>%
  count(keep, screening_status)

screening_clean <- screening_clean %>%
  filter(keep)

### Interesting applications ###
################################
# For those we will do data extraction, and extract how they defined
# "reproducibility" and extract any dois of relevant methodological papers

# Get interesting application papers for reference screening:
dois_interesting_applications <- screening_clean %>%
  filter(screening_status == "Included", interesting_application_paper) %>%
  filter(!is.na(doi)) %>%
  pull(doi)

# DOIs uploaded to Zotero
# https://www.zotero.org/groups/5397531/reproducibilitymetrics/collections/I3TF9PB2/items/W5Z7AWTU/collection
# Downloaded again into XML, load into R ....
interesting_applications <- read_xml(here("XMLs",
                                          "Interesting_application_papers.xml"))
# .... and write into SyRF CSV:
write_for_syrf(interesting_applications,
               filename = here("XMLs",
                               "Interesting_application_papers.csv"))


### Methods papers ###
######################
dois_methods <- screening_clean %>%
  filter(screening_status == "Included", !interesting_application_paper) %>%
  filter(!is.na(doi)) %>%
  pull(doi)

# Get references and citations
snowball_doi_methods_papers <- oa_snowball(
  doi = dois_methods,
  # citing_params = list(from_publication_date = "2022-01-01"),
  citing_params = list(to_publication_date = "2024-07-11"),
  # cited_by_params = list(),
  verbose = TRUE
)
# Change into DF
snowball_methods <-
  snowball2df(snowball_doi_methods_papers)

# nrow(snowball_methods)
# [1] 4388

# Find, using keywords, papers that (in their title) discuss a measure,
# metric or rating
keywords <-
  c("quantify ", "measure ", "evaluate ", "assess ", # e.g. quantify STH
    "quantifying ", "measuring ", "evaluating ", "assessing ",
    "metric", "score", "rating")
relevant_snowball_methods <- snowball_methods %>%
  rowwise() %>%
  # mutate(keep = any(str_detect(paste(title, ab), paste0(keywords, collapse = "|")))) %>%
  mutate(keep = any(str_detect(tolower(title), paste0(keywords, collapse = "|")))) %>%
  ungroup() %>%
  filter(keep)

relevant_snowball_methods %>%
  filter(!is.na(doi)) %>%
  pull(doi) %>%
  str_remove(., pattern = "https://doi.org/") %>%
  paste0("DOI(", ., ")") %>%
  paste0(collapse = " OR ")


# To paste into advanced search in Scopus, to get snowball_methods_08072024.csv
# Extract RIS - import into Zotero and export XML
# Downloaded again into XML, load into R ....
snowball_methods <- read_xml(here("XMLs",
                                  "snowball_methods_11072024.xml"))
# .... and write into SyRF CSV:
write_for_syrf(snowball_methods,
               filename = here("methodological_papers",
                               "snowballing",
                               "snowball_methods_08072024.csv"))
# Additionnal ones:
add_keywords <-
  c("quantification", "measurement", "evaluation", "assessment")
add_relevant_snowball_methods <- snowball_methods %>%
  rowwise() %>%
  # mutate(keep = any(str_detect(paste(title, ab), paste0(keywords, collapse = "|")))) %>%
  mutate(keep = any(str_detect(tolower(title), paste0(add_keywords, collapse = "|")))) %>%
  ungroup() %>%
  filter(keep)

add_relevant_snowball_methods %>%
  filter(!is.na(doi)) %>%
  pull(doi) %>%
  str_remove(., pattern = "https://doi.org/") %>%
  paste0("DOI(", ., ")") %>%
  paste0(collapse = " OR ")
# To paste into advanced search in Scopus, to get snowball_methods_08072024.csv
# Extract RIS - import into Zotero and export XML
# Downloaded again into XML, load into R ....
add_snowball_methods <- read_xml(here("XMLs",
                                      "snowball_methods_more_11072024.xml"))

write_for_syrf(add_snowball_methods,
               filename = here("methodological_papers",
                               "snowballing",
                               "snowball_methods_11072024.csv"))




