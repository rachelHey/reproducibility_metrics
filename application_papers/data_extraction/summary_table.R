# Script to produce Summary Table - data extraction application papers #
########################################################################
# author: R. Heyard

library(tidyverse)
library(here)
library(janitor)
library(openalexR)
library(kableExtra)

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

works_from_dois <- works_from_dois %>%
  mutate(doi = str_remove(doi, "https://doi.org/"))

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
  # Get number of authors (the one with missing DOI has 3)
  left_join(get_number_authors %>% select(doi, n_authors),
            by = "doi") %>%
  mutate(n_authors = case_when(is.na(doi) ~ 3,
                               TRUE ~ n_authors))


# To get predefined measures counts:
data_extraction <- data_extraction %>%
  rowwise() %>%
  mutate(agreement_in_statistical_significance =
           grepl("Agreement in statistical significance",
                 did_the_authors_measure_reproducibility),
         meta_analysis =
           grepl("Meta-analysis of original and replication study/studies",
                 did_the_authors_measure_reproducibility),
         agreement_in_effect_size =
           grepl("Agreement in effect size",
                 did_the_authors_measure_reproducibility),
         subjective_assessment =
           grepl("Subjective assessment",
                 did_the_authors_measure_reproducibility),
         none_of_the_above =
           grepl("None of the above",
                 did_the_authors_measure_reproducibility)) %>%
  ungroup()

# Some counts for a TABLE:

table_entry_count <- function(data, variable, name = NULL){
  if (is.null(name)) name <- variable

  if (is.logical(data %>% pull(variable))) {
    data %>%
      mutate(var = case_when(get(variable) ~ "Yes", TRUE ~ "No")) %>%
      count(var) %>%
      mutate(n = paste0(n, " (", round(n/sum(n)*100, 1), "%)"),
             v = name) %>%
      rename(" " = var)
  } else {
    data %>%
      count(get(variable)) %>%
      mutate(n = paste0(n, " (", round(n/sum(n)*100, 1), "%)"),
             v = name) %>%
      rename(" " = "get(variable)") %>%
      return()
  }
}

table_entry_numeric <- function(data, variable, name = NULL, big = ""){
  if (is.null(name)) name <- variable
  data %>%
    reframe(n = c(median(get(variable)),
                    paste0(min(get(variable)), " - ",
                           prettyNum(max(get(variable)), big.mark = big)))) %>%
    mutate(" " = c("Median", "Range"),
           v = name) %>%
    return()
}

tab <- data_extraction %>%
  table_entry_count("domain_openA", "Field of research") %>%
  bind_rows(
    data_extraction %>%
      mutate(
        did_the_authors_define_the_type_of_reproducibility_that_is_investigated =
          case_when(did_the_authors_define_the_type_of_reproducibility_that_is_investigated %in%
                      c("TRUE", "TRUE [Comment: Emulation]") ~ "Yes",
                    TRUE ~ "No")) %>%
      table_entry_count("did_the_authors_define_the_type_of_reproducibility_that_is_investigated",
                        "Authors defined reproducibility?")
  ) %>%
  bind_rows(data_extraction %>%
              # the many lab 5 would be different data and different analysis, as they
              # specificially test whether two different protocols (analyses) give different results
              mutate(
                even_if_defined_by_the_authors_infer_the_aspect_of_reproducibility_investigated =
                  case_when(doi == "10.1177/2515245920958687" ~
                              "Different data - different analysis",
                            !even_if_defined_by_the_authors_infer_the_aspect_of_reproducibility_investigated %in%
                              c("Different data - different analysis",
                                "Different data - same analysis",
                                "Same data - different analysis",
                                "Same data - same analysis") ~ "Combination",
                            TRUE ~ even_if_defined_by_the_authors_infer_the_aspect_of_reproducibility_investigated)) %>%
              table_entry_count("even_if_defined_by_the_authors_infer_the_aspect_of_reproducibility_investigated",
                                "Aspect of reproducibility")) %>%
  bind_rows(data_extraction %>%
              table_entry_count("type_of_project", "Type of project")) %>%
  bind_rows(data_extraction %>%
              table_entry_numeric("n_authors", "Number project authors")) %>%
  bind_rows(data_extraction %>%
              table_entry_numeric("cited_by_count",
                                  "Citation count", big = ",")) %>%
  bind_rows(data_extraction %>%
              table_entry_numeric("year", "Year of publication")) %>%
  bind_rows(data_extraction %>%
              table_entry_numeric("number_of_measures",
                                  "Number of measures used")) %>%
  bind_rows(data_extraction %>%
              table_entry_count("agreement_in_statistical_significance",
                                "Agreement in statistical significance")) %>%
  bind_rows(data_extraction %>%
              table_entry_count("agreement_in_effect_size",
                                "Agreement in effect size"))  %>%
  bind_rows(data_extraction %>%
              table_entry_count("meta_analysis",
                                "Meta-analaysis of study results")) %>%
  bind_rows(data_extraction %>%
              table_entry_count("subjective_assessment",
                                "Subjective assessment")) %>%
  bind_rows(data_extraction %>%
              table_entry_count("none_of_the_above",
                                "None of the predefined measures"))

tab %>%
  select(-v) %>%
  rename("N (%), unless otherwise indicated" = n) %>%
  kable("latex") %>%
  pack_rows(index = table(fct_inorder(tab$v)))

#
# data_extraction %>%
#   count(type_of_project, agreement_in_statistical_significance)
#
# data_extraction %>%
#   count(type_of_project, meta_analysis)
#
# data_extraction %>%
#   count(type_of_project, agreement_in_effect_size)
#
# data_extraction %>%
#   count(type_of_project, subjective_assessment)
#
# data_extraction %>%
#   count(type_of_project, none_of_the_above)

# data_extraction %>%
#   filter(!is.na(paste_description_of_each_of_the_measures_used)) %>%
#   group_by(type_of_project) %>%
#   count(paste_description_of_each_of_the_measures_used)


# data_extraction %>%
#   filter(did_the_paper_refer_to_other_papers_for_more_information_on_the_metric_s_used) %>%
#   pull(paste_the_doi_of_all_paper_s)
# +/- 14 additional papers




