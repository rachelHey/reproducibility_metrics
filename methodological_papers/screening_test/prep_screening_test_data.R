# Script to go from long to wide format - for the screening test
# author: R. Heyard

library(tidyverse)
library(here)

#### Load data ######
#####################
data_screening_test_raw <-
  read_csv(here("methodological_papers",
                "screening_test",
                "raw_data_screening_test_24_05_29.csv"))
# TODO: Note that "raw_data_screening_test_24_05_29.csv" was not added to git but is available in
# the project's DRIVE folder:
# https://drive.google.com/file/d/1ErHJNxWDRn7pmsdTT6RnYwrVNZ2_CQkr/view?usp=drive_link

# Quick check
data_screening_test_raw %>% select(StudyId) %>% distinct() %>% nrow()
data_screening_test_raw %>% group_by(StudyId) %>%
  summarise(n = n_distinct(InvestigatorName)) %>% count(n)

#### Tidying data ######
########################
data_screening_test <- data_screening_test_raw %>%
  select(StudyId, Title, Abstract, InvestigatorName, Answer, Comments) %>%
  group_by(StudyId) %>%
  mutate("Comments (merged)" = paste0(Comments[!is.na(Comments)],
                                      collapse = "; ")) %>%
  mutate(Agreement = paste0(paste(table(Answer), names(table(Answer))),
                            collapse = "; ")) %>%
  ungroup() %>%
  select(-Comments) %>%
  pivot_wider(names_from = InvestigatorName, values_from = Answer) %>%
  arrange(Agreement)

data_screening_test %>%
  write_csv(here("methodological_papers",
                 "screening_test", "data_screening_test_clean.csv"))
