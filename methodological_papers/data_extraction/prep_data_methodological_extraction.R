# Script to go from long to wide format - data extraction methodological papers
# author: R. Heyard

library(tidyverse)
library(here)

#### Load data ######
#####################
data_extract_raw <-
  read_csv(here("methodological_papers",
                "data_extraction",
                "Annotation_data_2024_09_10_Long_format_RAW.csv"))
# Note that "Annotation_data_2024_09_10_Long_format_RAW.csv" was not added to
# git but is available in the project's DRIVE folder:
# https://drive.google.com/file/d/1rjYNbKKxLRzQjVIHvr_z5GsluwgsgOXy/view?usp=drive_link

# Quick check - 97 papers with single extraction
data_extract_raw %>% select(StudyId) %>% distinct() %>% nrow()
data_extract_raw %>% group_by(StudyId) %>%
  summarise(n = n_distinct(InvestigatorName)) %>% count(n)

# Change to wide format:
data_extract_wide <- data_extract_raw %>%
  select(StudyId, Title, Authors, PublicationName, Year, Doi, InvestigatorName,
         Question, Answer, Comments) %>%
  pivot_wider(names_from = Question, values_from = c(Answer, Comments),
              names_glue = "{tools::toTitleCase(Question)}_{.value}")
data_extract_wide <- data_extract_wide %>%
  select(StudyId, Title, Authors, PublicationName, Year, Doi, InvestigatorName,
         "Type of Paper_Answer", "Type of Paper_Comments",
         "Design Purpose_Answer", "Design Purpose_Comments",
         "Name of Reproducibility (or Related Concept)_Answer",
         "Name of Reproducibility (or Related Concept)_Comments",
         "Definition of Reproducibility (or Related Concept)_Answer",
         "Definition of Reproducibility (or Related Concept)_Comments",
         "Type of Reproducibility (or Related Concept)_Answer",
         "Type of Reproducibility (or Related Concept)_Comments",
         "Purpose of Measure_Answer", "Purpose of Measure_Comments",
         "Number of Measures_Answer", "Number of Measures_Comments",
         "Type of Measure_Answer", "Type of Measure_Comments",
         "Type of Assessment_Answer", "Type of Assessment_Comments",
         "Name of Measure_Answer", "Name of Measure_Comments",
         "Implementation of Measure_Answer",
         "Implementation of Measure_Comments",
         "Data Input of Measure_Answer", "Data Input of Measure_Comments",
         "Assumptions or Prerequisites for Measure’s Usage_Answer",
         "Assumptions or Prerequisites for Measure’s Usage_Comments",
         "Limitation of Measure_Answer", "Limitation of Measure_Comments",
         "Equity, Diversity, and/or Inclusion_Answer",
         "Equity, Diversity, and/or Inclusion_Comments")

# Get rid of empty columns:
data_extract_wide <- data_extract_wide %>%
  select(-where( ~ all(is.na(.))))

# Store as csv:
data_extract_wide %>%
  write_csv(here("methodological_papers",
                 "data_extraction", "data_extraction_1009_clean.csv"))

