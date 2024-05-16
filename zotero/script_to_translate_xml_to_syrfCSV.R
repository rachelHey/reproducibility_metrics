library(dplyr)
library(XML)
source("zotero/functions.R")

# GET DATA FOR APPLICATION PAPERS READY FOR SYRF:
data <- read_xml("zotero/ReproducibilityMetrics.xml")
data <- data %>%
  filter(!is.na(author)) %>%
  filter(author != "Aert Robbie C. M. van; Assen Marcel A. L. M. van")

# data <- read_xml("zotero/export-data.xml")

write_for_syrf(data, filename="syrf_citations.csv")



data <- read_xml("zotero/truststudy.xml")
data <- data %>%
  filter(!is.na(author)) %>%
  filter(author != "Aert Robbie C. M. van; Assen Marcel A. L. M. van")

# data <- read_xml("zotero/export-data.xml")

write_for_syrf(data, filename="trust_syrf_citations.csv")

# --------------------------------

# GET DATA FOR METHODOLOGICAL PAPERS READY FOR SYRF:
# From scopus
data_scopus <- read_xml("zotero/scopus_20240513.xml")
# From ebsco
data_ebsco <- read_xml("zotero/ebsco_20240513.xml")
# Merge them together:
all_methodological_papers <- data_scopus %>%
  bind_rows(data_ebsco)

# De-duplicate using DOI, if it is not NA
## 1. extract the ones without a DOI
all_methodological_papers <- all_methodological_papers %>%
  filter(is.na(doi)) %>%
  ## 2. Paste them together with the once with a DOI, after de-duplication:
  bind_rows(
    all_methodological_papers %>%
      filter(!is.na(doi)) %>%
      group_by(doi) %>% slice(1)
  ) %>%
  arrange(title)

# Save as CSV for SYRF:
write_for_syrf(all_methodological_papers,
               filename = "all_methodological_papers_20240513.csv")


# Extract random sample for screening test
# all_methodological_papers <- read.csv("all_methodological_papers_20240513.csv")
set.seed(5)
random_rows <- sample(x = 1:nrow(all_methodological_papers),
                      size = 20, replace = FALSE)
screening_test_papers <- all_methodological_papers[random_rows, ]
# write.csv(screening_test_papers, file = "screening_test_papers.csv")
write_for_syrf(screening_test_papers,
               filename = "screening_test_papers.csv")
