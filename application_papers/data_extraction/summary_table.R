# Script to produce Summary Table - data extraction application papers #
########################################################################
# author: R. Heyard

library(tidyverse)
library(here)
library(janitor)
library(openalexR)

#### Load data ######
#####################
data_extraction_merged <-
  read_csv(here("application_papers",
                "data_extraction",
                "data_extract_clean_MERGED.csv")) %>%
  clean_names()

# find the one record with an NA in DOI:
# data_extraction_merged %>% filter(is.na(doi))
# this is a study protocol.


#### Get meta-data from OpenAlex ###
####################################
works_from_dois <- oa_fetch(
  entity = "works",
  doi = data_extraction_merged$doi,
  verbose = TRUE
)

# Get the domain
get_firstdomain <-
  sapply(works_from_dois$topics, function(X) {
     X %>% filter(i == 1 & name == "domain") %>% pull(display_name)
  }) %>% as_tibble_col(column_name = "domain_openA") %>%
  mutate(doi = works_from_dois$doi) %>%
  rowwise() %>%
  mutate(doi = str_remove(doi, "https://doi.org/"))

# Get number of authors
get_number_authors <-
  sapply(works_from_dois$author,
         function(X) {
           if (!is.null(dim(X))) return(nrow(X))
             else return(0)}
         ) %>%
  as_tibble_col(column_name = "n_authors") %>%
  mutate(doi = works_from_dois$doi) %>%
  rowwise() %>%
  mutate(doi = str_remove(doi, "https://doi.org/")) %>%
  # first one is not 0 but 260 (if I counted correctly)
  mutate(n_authors = case_when(doi == "10.1126/science.aac4716" ~ 260,
                               TRUE ~ n_authors))

# Merge meta-data to our data
data_extraction <- data_extraction_merged %>%
  select(-investigator_name) %>%
  mutate(doi = tolower(doi)) %>%
  left_join(get_firstdomain,
            by = "doi") %>%
  # Add "Health Sciences" to the one with NA doi - "real-world health records"
  # And merge health and life sciences to one
  mutate(domain_openA = case_when(is.na(doi) ~ "Health and Life Sciences",
                                  domain_openA %in% c("Health Sciences",
                                                      "Life Sciences") ~
                                    "Health and Life Sciences",
                                  TRUE ~ domain_openA)) %>%
  # Add cited_by_count (the one with missing DOI was cited once according to
  # Google scholar (July 4)
  left_join(works_from_dois %>% select(doi, cited_by_count),
            by = "doi") %>%
  mutate(cited_by_count = case_when(is.na(doi) ~ 1,
                                  TRUE ~ cited_by_count)) %>%
  # Get number of authors (the one with missing DOI has 3
  left_join(get_number_authors %>% select(doi, n_authors),
            by = "doi") %>%
  mutate(n_authors = case_when(is.na(doi) ~ 3,
                               TRUE ~ n_authors))


# Some counts for a TABLE:
data_extraction %>%
  count(domain_openA)

data_extraction$number_of_measures %>%
  summary()

data_extraction %>%
  count(type_of_project)

data_extraction %>%
  filter(did_the_paper_refer_to_other_papers_for_more_information_on_the_metric_s_used) %>%
  pull(paste_the_doi_of_all_paper_s)
# +/- 14 additional papers




