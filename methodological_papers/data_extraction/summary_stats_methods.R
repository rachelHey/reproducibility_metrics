# Script to produce Summary Statistics - data extraction methodological papers #
################################################################################
# author: R. Heyard

library(tidyverse)
library(here)
library(janitor)
library(openalexR)
library(kableExtra)
library(ggplot2)
library(scales) # for label_wrap()

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

### Table for methodological papers extraction: ##
##################################################
if (FALSE){
  #### Load data ######
  #####################
  data_extraction_methodological <-
    read_csv("https://osf.io/download/mt42q/") %>%
    clean_names()
  data_extraction_methodological <- data_extraction_methodological %>%
    filter(authors != "Kačmár Pavol ; Adamkovič Matúš")

  # Delete that paper that was in polish

  # One has no DOI - have to be added manually, later:
  # data_extraction_methodological %>% filter(is.na(doi))

  #### Get meta-data from OpenAlex ###
  ####################################
  works_from_dois <- oa_fetch(
    entity = "works",
    doi = data_extraction_methodological$doi,
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
    mutate(doi = str_remove(doi, "https://doi.org/"))

  # Merge meta-data to our data
  data_extraction_methodological <- data_extraction_methodological %>%
    select(-investigator) %>%
    mutate(doi = tolower(doi)) %>%
    left_join(get_firstdomain,
              by = "doi") %>%
    # Add "Physical Sciences" to the one with NA doi - as that is what it says on
    # OpenAlex when looking it up
    # And merge health and life sciences to one
    mutate(domain_openA = case_when(is.na(doi) ~ "Physical Sciences",
                                    domain_openA %in% c("Health Sciences",
                                                        "Life Sciences") ~
                                      "Health and Life Sciences",
                                    TRUE ~ domain_openA)) %>%
    # Add cited_by_count (the one with missing DOI was cited 43 times according to
    # OpenAlex (September 11)
    left_join(works_from_dois %>% select(doi, cited_by_count),
              by = "doi") %>%
    mutate(cited_by_count = case_when(is.na(doi) ~ 43,
                                      TRUE ~ cited_by_count)) %>%
    # Get number of authors (the one with missing DOI has 2)
    left_join(get_number_authors %>% select(doi, n_authors),
              by = "doi") %>%
    mutate(n_authors = case_when(is.na(doi) ~ 2,
                                 TRUE ~ n_authors))

  tab <- tibble(" " = "Total records",
                n = paste0(nrow(data_extraction_methodological)), v = "") %>%
    bind_rows(
      data_extraction_methodological %>%
        table_entry_count("domain_openA", "Field of research")) %>%
    bind_rows(
      data_extraction_methodological %>%
        table_entry_count("type_of_paper_answer",
                          "Type of paper"))

  tab %>%
    select(-v) %>%
    rename("N (%)" = n) %>%
    kable("latex", booktabs = TRUE,
          caption = "Summary of methodological papers included.\\label{tab:methodological_table}") %>%
    kable_styling() %>%
    pack_rows(index = table(fct_inorder(tab$v)))
}

### Table for reproducibility metrics extraction: ##
####################################################
reprometrics <-
  read_csv("https://osf.io/download/y3zf4/")


options(knitr.kable.NA = '')

# ---------------------------
# Table with all the metrics:
# --------------------------
# Function to replace quotes around questions with `` and ''
replace_quotes <- function(x) {
  gsub('\"([^\"]*)\"', "``\\1''", x)
}

tab_description <- reprometrics %>%
  mutate(Name = paste0(Name, ifelse(!is.na(`also called or related to`),
                                    paste0(" (", `also called or related to`, ")"),
                                    ""))) %>%
  select(`Type of metric`, Name, Description, `Question answered`,
         `Type of Reproducibility investigated`, `Scenario of application`,
         `Purpose of Metric`, `First mention in`, `Discussed in`, `Used in`) %>%
  mutate("Further reading" = paste0(ifelse(!is.na(`First mention in`),
                                           paste0("First mentioned in ",
                                                  `First mention in`, ". "), ""),
                                    ifelse(!is.na(`Discussed in`),
                                           paste0("Discussed in ",
                                                  `Discussed in`, ". "), ""),
                                    ifelse(!is.na(`Used in`),
                                           paste0("Used in ",
                                                  `Used in`, "."), "")),
         "Type of metric" =
           case_when(
             grepl("framework", `Type of metric`) ~ "A framework",
             grepl("Bland-Altman", Name) ~ "A graph",
             grepl("formula", `Type of metric`) ~ "A formula and/or statistical model",
             grepl("graph", `Type of metric`) ~ "A graph",
             grepl("model", `Type of metric`) ~ "A formula and/or statistical model",
             grepl("survey", `Type of metric`) ~ "A study, survey, or questionnaire",
             grepl("study", `Type of metric`) ~ "A study, survey, or questionnaire",
             TRUE ~ `Type of metric`),
         "Further reading" = str_trim(`Further reading`),
         "Type of Reproducibility investigated" =
           case_when(
             `Type of Reproducibility investigated` ==
               "Different data - same analysis; Different data - different analysis" ~
               "Different data - same/different analysis",
             `Type of Reproducibility investigated` ==
               "Same data - same analysis; Different data - same analysis" ~
               "Same/different data - same analysis",
             `Type of Reproducibility investigated` ==
               "Same data - same analysis; Same data - different analysis" ~
               "Same data - same/different analysis",
             `Type of Reproducibility investigated` %in%
               c("Same data - same analysis; Different data - same analysis; Same data - different analysis; Different data - different analysis",
                 "Same data - same analysis; Same data - different analysis; Different data - same analysis; Different data - different analysis") ~
               "Same/different data - same/different analysis",
             TRUE ~ `Type of Reproducibility investigated`),
         "Type of Reproducibility investigated" =
           factor(`Type of Reproducibility investigated`,
                  levels = c("Same data - same analysis",
                             "Same data - different analysis",
                             "Different data - same analysis",
                             "Different data - different analysis",
                             "Same data - same/different analysis",
                             "Different data - same/different analysis",
                             "Same/different data - same analysis",
                             "Same/different data - same/different analysis")),
         "Purpose of Metric" =
           case_when(
               grepl("quantify", `Purpose of Metric`) &
                 grepl("classify", `Purpose of Metric`) ~ "To quantify and classify",
               grepl("predict", `Purpose of Metric`) ~ "To quantify and predict",
               grepl("analyse", `Purpose of Metric`) |
                 grepl("describe", `Purpose of Metric`) |
                 grepl("explain", `Purpose of Metric`) ~ "To quantify and explain",
               grepl("quantify", `Purpose of Metric`) ~ "To quantify",
               grepl("classify", `Purpose of Metric`) ~ "To classify",
               TRUE ~ `Purpose of Metric`),
         "Question answered" = replace_quotes(`Question answered`)) %>%
  arrange(`Type of metric`, Name)

tab_description %>%
  select(-`Type of metric`, -`Type of Reproducibility investigated`,
         - `First mention in`, -`Discussed in`, - `Used in`) %>%
  rename("Research question" = "Question answered",
         "References" = "Further reading",
         "Name (also called/related to)" = "Name"
         ) %>%
  kable(booktabs = TRUE,  linesep = "", escape = FALSE, "latex",
        longtable = TRUE,
        caption = "Metrics table: Summary of the 50 identified metrics, ordered
        alphabetically and grouped by the type of metric: a formula or statistical
        model,  a framework, a graph, a study, survey or questionnaire, or an
        algorithm. The name and description of the metric is followed by one or
        several research questions summarising the type of question the metric
        can answer. The scenario of application gives insights into the type of
        project design needed to compute or use the metric. We then collapsed all
        the references for further reading, where have the metrics first been
        mentioned in relation to reproducibility, which papers discussed them
        further and which application papers demonstrate how to use
        them.\\label{tab:metrics_table}") %>%
  kable_styling(latex_options = c("repeat_header", "striped"),
                font_size = 8) %>%
  column_spec(1, color = "DarkBlue", width = "2.75cm") %>%
  column_spec(2, width = "9cm") %>%
  column_spec(3, width = "4.5cm") %>%
  column_spec(4, width = "2.5cm") %>%
  column_spec(5, width = "1.75cm") %>%
  column_spec(6, width = "2cm") %>%
  pack_rows(index =
              table(fct_inorder(tab_description$`Type of metric`)),
            latex_align = "c", indent = FALSE,
            background = "lightgray") %>%
  landscape(margin = NULL)


# -----------------------------------
# Summary table of extracted metrics:
# -----------------------------------

tab <- tibble(" " = "Total number of metrics", n = paste0(nrow(reprometrics)),
              v = "") %>%
  bind_rows(
    reprometrics %>%
      mutate(var =
               case_when(`Designed for Reproducibility` %in% c("No", "Unsure") ~
                           "No*",
                         TRUE ~ `Designed for Reproducibility`)) %>%
      table_entry_count("var", "Designed for reproducibility")) %>%
  bind_rows(
    reprometrics %>%
      mutate(
        var =
          case_when(
            `Type of Reproducibility investigated` ==
              "Different data - same analysis; Different data - different analysis" ~
              "Different data - same/different analysis",
            `Type of Reproducibility investigated` %in%
              c("Same data - same analysis; Different data - same analysis",
                "Different data - same analysis; Same data - same analysis") ~
              "Same/different data - same analysis",
            `Type of Reproducibility investigated` ==
              "Same data - same analysis; Same data - different analysis" ~
              "Same data - same/different analysis",
            `Type of Reproducibility investigated` %in%
              c("Same data - same analysis; Different data - same analysis; Same data - different analysis; Different data - different analysis",
                "Same data - same analysis; Same data - different analysis; Different data - same analysis; Different data - different analysis",
                "Different data - same analysis; Same data - different analysis", "Different data - same analysis; Same data - different analysis; Different data - different analysis") ~
              "Same/different data - same/different analysis",
            TRUE ~ `Type of Reproducibility investigated`)) %>%
      table_entry_count("var", "Type of reproducibility")) %>%
  bind_rows(
    reprometrics %>%
      mutate(
        var =
          case_when(
            grepl("framework", `Type of metric`) ~ "A framework",
            grepl("Bland-Altman", Name) ~ "A graph",
            grepl("formula", `Type of metric`) ~ "A formula and/or statistical model",
            grepl("graph", `Type of metric`) ~ "A graph",
            grepl("model", `Type of metric`) ~ "A formula and/or statistical model",
            grepl("survey", `Type of metric`) ~ "A study, survey, or questionnaire",
            grepl("study", `Type of metric`) ~ "A study, survey, or questionnaire",
            TRUE ~ `Type of metric`)) %>%
      table_entry_count("var", "Type of metric")) %>%
  bind_rows(reprometrics %>%
              mutate(
                var =
                  case_when(
                    grepl("quantify", `Purpose of Metric`) &
                      grepl("classify", `Purpose of Metric`) ~ "To quantify and classify",
                    grepl("predict", `Purpose of Metric`) ~ "To quantify and predict",
                    grepl("analyse", `Purpose of Metric`) |
                      grepl("describe", `Purpose of Metric`) |
                      grepl("explain", `Purpose of Metric`) ~ "To quantify and explain",
                    grepl("quantify", `Purpose of Metric`) ~ "To quantify",
                    grepl("classify", `Purpose of Metric`) ~ "To classify",
                    TRUE ~ `Purpose of Metric`)) %>%
  table_entry_count("var", "Purpose of metric")) %>%
  bind_rows(reprometrics %>%
            mutate(
              var =
                case_when(
                  grepl("ualitative", `Type of assessment`) &
                    grepl("uantitative", `Type of assessment`) ~ "Qualitative and quantitative",
                  grepl("ualitative", `Type of assessment`) ~ "Qualitative",
                  grepl("uantitative", `Type of assessment`) ~ "Quantitative",
                  TRUE ~ `Purpose of Metric`)) %>%
              table_entry_count("var", "Type of assessment")) %>%
  bind_rows(reprometrics %>%
            mutate(
              var =
                case_when(
                  grepl("Ready-to-use open", `Implementation of Metric`) |
                    grepl("Ready-to-use tool", `Implementation of Metric`) ~ "Ready-to-use open tool provided",
                  grepl("Ready-to-use closed tool", `Implementation of Metric`) ~ "Ready-to-use closed tool provided",
                  grepl("asy to implement", `Implementation of Metric`) |
                    grepl("an be implemented", `Implementation of Metric`) ~ "Easy to implement",
                  grepl("ard to implement", `Implementation of Metric`) |
                    grepl("nclear implementation", `Implementation of Metric`) ~ "Hard, costly or unclear implementation",
                  TRUE ~ `Implementation of Metric`)) %>%
              table_entry_count("var", "Implementation")) %>%
  bind_rows(
    reprometrics %>%
      mutate(
        var =
          case_when(
            grepl("Qualitative", `Data Input`) ~ "Qualitative data, surveys or questionnaires",
            grepl("Results - figures", `Data Input`) &
              grepl("Results - numbers and tables", `Data Input`) ~ "Results - figures, numbers and tables",
            grepl("Results - figures", `Data Input`) ~ "Results - figures",
            grepl("Results - numbers and table", `Data Input`) ~ "Results - number and tables",
            grepl("Text", `Data Input`) |
              grepl("design", `Data Input`) ~ "Text, meta-data, and information on design",
            grepl("Original raw data", `Data Input`) |
              grepl("Raw original data", `Data Input`) ~ "Original raw data, code, and/or software",
            TRUE ~ `Data Input`)) %>%
      table_entry_count("var", "Data Input"))


tab %>%
  select(-v) %>%
  rename("N (%)" = n) %>%
  kable("latex", booktabs = TRUE,
        caption = "Summary statistics of attributes of extracted reproducibility metrics.\\label{tab:metrics}") %>%
  kable_styling() %>%
  pack_rows(index = table(fct_inorder(tab$v))) %>%
  add_footnote("* includes unclear", notation="none")



# -------------------
# Metric description:
# -------------------
formulas <- reprometrics %>%
  mutate("Type of metric" =
  case_when(
    grepl("framework", `Type of metric`) ~ "FRAM",
    grepl("graph", `Type of metric`) ~ "GRAPH",
    grepl("formula", `Type of metric`) ~ "FORM",
    grepl("model", `Type of metric`) ~ "FORM",
    grepl("survey", `Type of metric`) ~ "STUDY",
    grepl("study", `Type of metric`) ~ "STUDY",
    grepl("algorithm", `Type of metric`) ~ "ALGO",
    TRUE ~ `Type of metric`)) %>%
  filter(`Type of metric` == "FORM")

formulas %>% count(`Purpose of Metric`)
formulas %>% filter(`Purpose of Metric` %in%
                      c("To predict",
                        "To quantify (continuous); To classify (binary, yes or no); To predict",
                        "To quantify (continuous); to predict"))
formulas %>% count(`Data Input`)
formulas %>% filter(`Data Input` %in% c("Raw original data", "Results - numbers and tables; Original raw data") )
formulas %>% count(`Type of assessment`)
formulas %>% count(`Type of Reproducibility investigated`)

frameworks <- reprometrics %>%
  mutate("Type of metric" =
           case_when(
             grepl("framework", `Type of metric`) ~ "FRAM",
             grepl("graph", `Type of metric`) ~ "GRAPH",
             grepl("formula", `Type of metric`) ~ "FORM",
             grepl("model", `Type of metric`) ~ "FORM",
             grepl("survey", `Type of metric`) ~ "STUDY",
             grepl("study", `Type of metric`) ~ "STUDY",
             grepl("algorithm", `Type of metric`) ~ "ALGO",
             TRUE ~ `Type of metric`)) %>%
  filter(`Type of metric` == "FRAM")

frameworks %>% count(`Data Input`)
frameworks %>% select(Name, `Data Input`)
frameworks %>% count(`Type of assessment`)
frameworks %>% count(`Type of Reproducibility investigated`)

