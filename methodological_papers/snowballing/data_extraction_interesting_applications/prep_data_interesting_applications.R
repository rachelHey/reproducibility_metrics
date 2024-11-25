# Script to go from long to wide format
# author: R. Heyard

library(tidyverse)
library(here)

#### Load data ######
#####################
data_extract_raw <- read_csv(here("methodological_papers",
                                  "snowballing",
                                  "data_extraction_interesting_applications",
                                  "Annotation_data_2024_08_02.csv"))
# Note that "Annotation_data_2024_08_02.csv" was not added to git but is
# available in the project's DRIVE folder:
# TODO: add link

# Quick check
data_extract_raw %>% select(StudyId) %>% distinct() %>% nrow()
data_extract_raw %>% group_by(StudyId) %>%
  summarise(n = n_distinct(InvestigatorName)) %>% count(n)

#### Tidying data ######
########################
# e.g. add comment behind answer AND delete [add details ...], - as described
# ... and - one of ..., (authors), from answer/question
data_extract <- data_extract_raw %>%
  mutate(Answer = paste0(Answer, ifelse(!is.na(Comments),
                                        paste0(" [Comment: ", Comments, "]"),
                                        "")))
# Change to wide format:
data_extract_wide <- data_extract %>%
  select(StudyId, InvestigatorName, Question, Answer) %>%
  pivot_wider(names_from = Question, values_from = Answer)

# Change ordering of questions:
data_extract_wide %>%
  left_join(data_extract_raw %>%
              select(StudyId, Title, Year, Doi) %>%
              distinct(),
            by = "StudyId") %>%
  select(Title, Year, Doi, InvestigatorName,
         "Did the authors define the type of reproducibility that is investigated?",
         "Aspect of reproducibility investigated (authors)",
         "Infer the aspect of reproducibility investigated",
         "How did the authors measure reproducibility, i.e., summarise the results?",
         "Did the paper refer to other papers for more information on the metric(s) used?",
         "Paste the doi of all paper(s).") %>%
  write_csv(here("methodological_papers",
                 "snowballing",
                 "data_extraction_interesting_applications",
                 "annotation_data_2024_08_02_clean.csv"))
