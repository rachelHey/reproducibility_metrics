# Script to produce Summary Table - data extraction application papers #
########################################################################
# author: R. Heyard

library(tidyverse)
library(here)
library(janitor)
library(openalexR)
library(kableExtra)
library(ggplot2)
library(scales) # for label_wrap()

#### Load data ######
#####################
data_extraction_merged <-
  read_csv("https://osf.io/download/ubmyt/") %>%
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
  mutate(doi = str_remove(doi, "https://doi.org/")) %>%
  distinct()

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
                               TRUE ~ n_authors)) %>%
  distinct()

works_from_dois <- works_from_dois %>%
  mutate(doi = str_remove(doi, "https://doi.org/")) %>%
  group_by(doi) %>%
  slice(1) %>%
  ungroup() %>%
  distinct()

# Merge meta-data to our data
data_extraction <- data_extraction_merged %>%
  # Take out retracted article
  filter(title != "High replicability of newly discovered social-behavioural findings is achievable") %>%
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

# Some count function for a TABLE:
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

# Produce that summary statistics table
tab <- tibble(" " = "Total records", n = paste0(nrow(data_extraction)), v = "") %>%
  bind_rows(
    data_extraction %>%
      table_entry_count("domain_openA", "Field of research (OpenAlex)")) %>%
  bind_rows(
    data_extraction %>%
      mutate(
        did_the_authors_define_the_type_of_reproducibility_that_is_investigated =
          case_when(did_the_authors_define_the_type_of_reproducibility_that_is_investigated %in%
                      c("TRUE", "TRUE [Comment: Emulation]") ~ "Yes",
                    TRUE ~ "No")) %>%
      table_entry_count("did_the_authors_define_the_type_of_reproducibility_that_is_investigated",
                        "Authors defined reproducibility?")) %>%
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
                                "Same data - same analysis") ~ "Combination*",
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
                                "Used none of the predefined measures"))

tab %>%
  select(-v) %>%
  rename("N (%), unless otherwise indicated" = n) %>%
  kable("latex", booktabs = TRUE,
        caption = "Summary of application paper data extraction.\\label{tab:application_table}") %>%
  kable_styling() %>%
  pack_rows(index = table(fct_inorder(tab$v))) %>%
  add_footnote("* Theses papers presented projects with several sub-projects looking at different aspects of reproducibility", notation="none")


# The project with both "Many Phenomena, Many studies; Many Phenomena, One Study":
data_extraction %>%
  filter(type_of_project == "Many Phenomena, Many studies; Many Phenomena, One Study")

# Opening the graphical device
pdf(here("plots_for_paper", "count_metrics.pdf"),
    width = 8, # The width of the plot in inches
    height = 5)

# Plot with number of metrics used
data_extraction %>%
  mutate(number_of_measures = round(number_of_measures, 0)) %>%
  count(number_of_measures) %>%
  ggplot(aes(x = number_of_measures, y = n)) +
  geom_col(color = "white",fill = "#B2C7E5") +
  geom_text(aes(label = paste0(n), vjust = -0.25)) +
  xlim(c(1, 12)) +
  # ylim(c(0, .4)) +
  labs(x = "Number of metrics used", y = "Count") +
  theme_bw() +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme(legend.position = "right",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(size = .2))

# Closing the graphical device
dev.off()

# Effort with 12 metrics
data_extraction %>%
  filter(number_of_measures == 12) %>%
  pull(doi)

# p-original stuff
data_extraction %>%
  filter(grepl("p-orig", did_the_authors_measure_reproducibility) |
           grepl("pori", did_the_authors_measure_reproducibility) |
           grepl("p_or", did_the_authors_measure_reproducibility) |
           grepl("p-orig", paste_description_of_each_of_the_measures_used) |
           grepl("pori", paste_description_of_each_of_the_measures_used) |
           grepl("p_or", paste_description_of_each_of_the_measures_used))

# Significance:
data_extraction %>%
  filter(agreement_in_statistical_significance) %>%
  pull(did_the_authors_measure_reproducibility)

# Effect size:
data_extraction %>%
  filter(agreement_in_effect_size) %>%
  pull(did_the_authors_measure_reproducibility)

# Meta-analysis:
data_extraction %>%
  filter(meta_analysis) %>%
  pull(did_the_authors_measure_reproducibility)

# Subjective assessment:
data_extraction %>%
  filter(subjective_assessment) %>%
  pull(did_the_authors_measure_reproducibility)

# To describe extra/special measures
data_extraction %>%
  filter(!is.na(paste_description_of_each_of_the_measures_used)) %>%
  pull(paste_description_of_each_of_the_measures_used)

# To describe limitations and assumptions
data_extraction %>%
  filter(!is.na(paste_text_on_limitation_assumptions)) %>%
  pull(paste_text_on_limitation_assumptions)

data_extraction %>%
  summarise(sum(!is.na(paste_text_on_limitation_assumptions))/n())


# Plot with type of project/reproducibility and type of agreement
long_data <- data_extraction %>%
  mutate(
    even_if_defined_by_the_authors_infer_the_aspect_of_reproducibility_investigated =
      case_when(doi == "10.1177/2515245920958687" ~
                  "Different data - different analysis",
                TRUE ~ even_if_defined_by_the_authors_infer_the_aspect_of_reproducibility_investigated)) %>%
  pivot_longer(agreement_in_statistical_significance:none_of_the_above,
               names_to = "type_of_agreement") %>%
  filter(value) %>%
  select(title, doi, type_of_project,
         even_if_defined_by_the_authors_infer_the_aspect_of_reproducibility_investigated,
         type_of_agreement) %>%
  mutate(type_of_agreement = factor(
    case_when(type_of_agreement == "agreement_in_effect_size" ~
                "Agreement in effect size",
              type_of_agreement == "agreement_in_statistical_significance" ~
                "Agreement in statistical significance",
              type_of_agreement == "meta_analysis" ~
                "Meta-analysis methodology",
              type_of_agreement == "none_of_the_above" ~ "Other",
              type_of_agreement == "subjective_assessment" ~
                "Subjective assessment"),
    levels = c("Agreement in effect size",
               "Agreement in statistical significance",
               "Meta-analysis methodology",
               "Subjective assessment", "Other"))) %>%
  rename(type_of_repro =
           even_if_defined_by_the_authors_infer_the_aspect_of_reproducibility_investigated)

# !!Note that combinations were split!!
# Opening the graphical device
pdf(here("plots_for_paper", "type_metric_repro.pdf"),
    width = 8, # The width of the plot in inches
    height = 5)
long_data %>%
  count(type_of_repro,
        type_of_agreement) %>%
  separate_longer_delim(type_of_repro, delim = ";") %>%
  mutate(type_of_repro = str_trim(type_of_repro)) %>%
  group_by(type_of_repro, type_of_agreement) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = type_of_agreement, y = type_of_repro)) +
  geom_point(aes(size = n, color = type_of_repro)) +
  scale_size_continuous(range = c(2, 15), breaks = c(1, 5, 10, 15, 20, 25),
                        "Count") +
  scale_color_manual(values = c("#44AA99", "#88CCEE",
                                "#DDCC77", "#CC6677"), guide = "none") +
  labs(x = "Type of Metric", y = "Type of reproducibility") +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_discrete(labels = label_wrap(20)) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(size = .2))

# Closing the graphical device
dev.off()
# !!Note that combinations were split!!
# Opening the graphical device
pdf(here("plots_for_paper", "type_metric_repro_bar.pdf"),
    width = 10, # The width of the plot in inches
    height = 5)
long_data %>%
  count(type_of_repro,
        type_of_agreement) %>%
  separate_longer_delim(type_of_repro, delim = ";") %>%
  mutate(type_of_repro = str_trim(type_of_repro)) %>%
  group_by(type_of_repro, type_of_agreement) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = type_of_agreement, y = n)) +
  geom_col(aes(fill = type_of_repro), position = "dodge", color = "white") +
  geom_text(aes(label = n), position = position_dodge2(.9), vjust =-0.1) +
  scale_fill_manual(values = c("#44AA99", "#88CCEE",
                                "#DDCC77", "#CC6677"), "Type of reproducibility") +
  labs(x = "Type of Metric", y = "Count") +
  scale_x_discrete(labels = label_wrap(15)) +
  # scale_y_discrete(labels = label_wrap(20)) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(size = .2))

# Closing the graphical device
dev.off()


pdf(here("plots_for_paper", "type_metric_project.pdf"),
    width = 8, # The width of the plot in inches
    height = 5)
long_data %>%
  count(type_of_project,
        type_of_agreement) %>%
  separate_longer_delim(type_of_project, delim = ";") %>%
  mutate(type_of_project = str_trim(type_of_project)) %>%
  group_by(type_of_project, type_of_agreement) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = type_of_agreement, y = type_of_project)) +
  geom_point(aes(size = n, color = type_of_project)) +
  scale_size_continuous(range = c(2, 10), breaks = c(1, 5, 10, 15),
                        "Count") +
  scale_color_manual(values = c("#44AA99", "#88CCEE",
                                "#DDCC77", "#CC6677"), guide = "none") +
  labs(x = "Type of Metric", y = "Type of project") +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_discrete(labels = label_wrap(18)) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(size = .2))

# Closing the graphical device
dev.off()

pdf(here("plots_for_paper", "type_metric_project_bar.pdf"),
    width = 10, # The width of the plot in inches
    height = 5)
long_data %>%
  count(type_of_project,
        type_of_agreement) %>%
  separate_longer_delim(type_of_project, delim = ";") %>%
  mutate(type_of_project = str_trim(type_of_project)) %>%
  group_by(type_of_project, type_of_agreement) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = type_of_agreement, y = n)) +
  geom_col(aes(fill = type_of_project), color = "white", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge2(.9), vjust =-0.1) +
  scale_fill_manual(values = c("#44AA99", "#88CCEE",
                               "#DDCC77", "#CC6677"), "Type of project") +
  labs(x = "Type of Metric", y = "Count") +
  scale_x_discrete(labels = label_wrap(15)) +
  # scale_y_discrete(labels = label_wrap(18)) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(size = .2))

# Closing the graphical device
dev.off()
