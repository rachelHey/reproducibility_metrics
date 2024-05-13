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
# Save as CSV for SYRF:
write_for_syrf(all_methodological_papers,
               filename="all_methodological_papers_20240513.csv")

