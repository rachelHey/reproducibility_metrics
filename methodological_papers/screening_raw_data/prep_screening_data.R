# Script to go from long to wide format - screening and annotation data
# author: R. Heyard

library(tidyverse)
library(here)
library(janitor)

#### Load data ######
#####################
screening_raw <- read_csv(here("methodological_papers/",
                               "screening_raw_data",
                               "Screening_20240702_long_format.csv"))
annotation_raw <- read_csv(here("methodological_papers/",
                                "screening_raw_data",
                                "Annotation_20240702_long_format.csv"))

# Note that "Screening_20240702_long_format.csv" and
# "Annotation_20240702_long_format.csv" were not added to git but are
# available in the project's DRIVE folder: XXXXXXXX

# Quick check
screening_raw %>% select(StudyId) %>% distinct() %>% nrow()
screening_raw %>% group_by(StudyId) %>%
  summarise(n = n_distinct(InvestigatorName)) %>% count(n)


#### Tidying Screening data ######
##################################
data_screening <- screening_raw %>%
  select(StudyId, Title, Authors, PublicationName, Abstract,
         Url, Year, Doi, ScreeningStatus) %>%
  distinct() %>%
  clean_names()

included_papers <- data_screening %>%
  filter(screening_status == "Included")

#### Tidying Annotation data ######
###################################
data_annotation <- annotation_raw %>%
  select(StudyId, Title, Abstract, InvestigatorName, Question, Answer,
         Comments) %>%
  group_by(StudyId) %>%
  mutate("Comments_merged" = paste0(Comments[!is.na(Comments)],
                                      collapse = "; ")) %>%
  select(-Comments) %>%
  pivot_wider(names_from = Question, values_from = Answer) %>%
  select(-InvestigatorName) %>%
  group_by(StudyId) %>%
  mutate(`Review paper` = any(`Review paper`)) %>%
  mutate(`Tutorial paper` = any(`Tutorial paper`)) %>%
  mutate(`Interesting application paper` =
           any(`Interesting application paper`)) %>%
  ungroup() %>%
  clean_names() %>%
  distinct()

#### Merging data ######
########################
data_screening_with_annotation <- data_screening %>%
  left_join(data_annotation %>%
              select(study_id, review_paper, tutorial_paper,
                            interesting_application_paper, comments_merged),
            by = "study_id") %>%
  mutate(review_paper = case_when(is.na(review_paper) ~ FALSE,
                                  TRUE ~ review_paper),
         tutorial_paper = case_when(is.na(tutorial_paper) ~ FALSE,
                                    TRUE ~ tutorial_paper),
         interesting_application_paper =
           case_when(is.na(interesting_application_paper) ~ FALSE,
                     TRUE ~ interesting_application_paper),
         comments_merged = case_when(is.na(comments_merged) ~ "",
                     TRUE ~ comments_merged))


# Store data:
write_csv(data_screening_with_annotation, here("methodological_papers",
                                               "screening_raw_data",
                                               "data_screening_clean.csv"))
