# Script to go from long to wide format
# author: R. Heyard

library(tidyverse)
library(here)

#### Load data ######
#####################
data_extract_raw <- read_csv(here("application_papers",
                                  "data_extraction",
                                  "rawdata_syrf_april11.csv"))
# Note that "rawdata_syrf_april11.csv" was not added to git but is available in
# the project's DRIVE folder:
# https://drive.google.com/file/d/1ErHJNxWDRn7pmsdTT6RnYwrVNZ2_CQkr/view?usp=drive_link

# Quick check
data_extract %>% select(StudyId) %>% distinct() %>% nrow()
data_extract %>% group_by(StudyId) %>%
  summarise(n = n_distinct(InvestigatorName)) %>% count(n)

#### Tidying data ######
########################
# e.g. add comment behind answer AND delete [add details ...], - as described
# ... and - one of ..., (authors), from answer/question
data_extract <- data_extract_raw %>%
  mutate(Answer = paste0(Answer, ifelse(!is.na(Comments),
                                        paste0(" [Comment: ", Comments, "]"),
                                        ""))) %>%
  rowwise() %>%
  mutate(Answer = gsub(" [add details in the comment cell]", "", Answer,
                       fixed = TRUE),
         Question = gsub(" - as described by the authors in the paper.", "", Question,
                       fixed = TRUE),
         Question = gsub(" - as described by the authors in the paper", "", Question,
                       fixed = TRUE),
         Question = gsub(" - one of the following", "", Question, fixed = TRUE),
         Question = gsub(" (authors)", "", Question, fixed = TRUE)) %>%
  ungroup()

## Some questions are answered more than once per paper. Those have to be merged
# Multiple answers are merged using (1), (2), ...:
multiple_answer <- data_extract %>%
  select(StudyId, InvestigatorName, Question, Answer) %>%
  group_by(StudyId, InvestigatorName, Question) %>%
  mutate(n = n()) %>% filter(n > 1) %>%
  select(-n) %>%
  ungroup() %>%
  group_by(StudyId, InvestigatorName, Question) %>%
  summarise(Answer = paste0("(", 1:n(), ") ", Answer, collapse = "")) %>%
  ungroup()

# Extract single answers
single_answer <- data_extract %>%
  select(StudyId, InvestigatorName, Question, Answer) %>%
  group_by(StudyId, InvestigatorName, Question) %>%
  mutate(n = n()) %>% filter(n == 1) %>%
  select(-n) %>%
  ungroup()

# To put them together again:
data_extract <- single_answer %>%
  bind_rows(multiple_answer) %>%
  arrange(StudyId, InvestigatorName)

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
         "Field of research",
         "Discipline",
         "Type of project",
         "Research question or aim",
         "Did the authors define the type of reproducibility that is investigated?",
         "Aspect of reproducibility investigated",
         "Even if defined by the authors, infer the aspect of reproducibility investigated",
         "Did the authors measure reproducibility",
         "Did they measure reproducibility using measures not in the previous list?",
         "Paste description of each of the measures used",
         "Did the paper refer to other papers for more information on the metric(s) used?",
         "Paste the doi of all paper(s)",
         "Did the authors discuss limitations or assumptions of the metric(s) used?",
         "Paste text on limitation/assumptions",
         "Did the authors discuss equity, diversity, and/or inclusion at any point?",
         "Paste text on EDI dimension discussion") %>%
  write_csv("data_extraction/data_extract_clean.csv")







