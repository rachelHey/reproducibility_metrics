# Function author: K. Hair.

#' Format DOIs in a DataFrame
#'
#' This function formats Digital Object Identifiers (DOIs) in a DataFrame by
#' converting them to uppercase and standardizing their representation. It
#' performs various replacements to ensure consistent formatting for DOIs.
#'
#' @import dplyr
#'
#' @param df A DataFrame containing a column named 'doi' with DOI strings.
#'
#' @return A DataFrame with the 'doi' column formatted for consistency.
#' @export
format_doi <- function(df){

  df$doi <- tolower(df$doi)
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("%28", "(", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("%29", ")", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("http://dx.doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("https://doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("https://dx.doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("http://doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi: ", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi:", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi", "", x)))
  return(df)
}

#' Format Columns for SOLES Search Data
#'
#' This function processes a dataframe to retain relevant columns required for SOLES and standardizes the case of
#' specific columns to ensure consistent formatting.
#'
#' @import dplyr
#'
#' @param df A dataframe to be formatted for SOLES search data.
#'
#' @return A formatted dataframe containing the required SOLES columns with standardized case.
#'
format_cols <- function(df){

  # cols required for soles
  x <- c("record_id", "accession", "author", "year", "journal", "doi", "title",
         "pages", "volume", "abstract", "isbn", "keywords",
         "secondarytitle", "url", "date", "issn", "pmid", "ptype",
         "source", "author_country", "number", "author_affiliation")

  title_case_cols <- c("author","journal", "secondarytitle", "author_country", "author_affiliation")

  sentence_case_cols <- c("title",
                          "abstract")

  lower_case_cols <- c("record_id", "doi",  "keywords", "ptype",
                       "source")

  df[x[!(x %in% colnames(df))]] = NA
  df <- df %>%
    select(all_of(x)) %>%
    mutate(across(all_of(sentence_case_cols), ~stringr::str_to_sentence(.))) %>%
    mutate(across(all_of(lower_case_cols), ~stringr::str_to_lower(.))) %>%
    mutate(across(all_of(title_case_cols), ~stringr::str_to_title(.))) %>%
    mutate_at(vars(x), ~ gsub(";", "; ", .))

  df$pages <- lapply(df$pages, function(x) gsub("--", "-", x))
  df$date  <-  format(Sys.Date(), "%d%m%y")

  cols_to_modify <-  c('author', 'title', 'year', 'journal', 'doi', 'number', 'pages', 'volume', 'isbn', 'issn')
  df['abstract'] <- lapply(df['abstract'], function(x) gsub("[Aa]bstract", "", x))
  df[cols_to_modify] <- lapply(df[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))
  df['abstract'] <- lapply(df['abstract'], function(x) trimws(x))
  return(df)
}

#' Read reference data from an XML file
#'
#' This function reads reference information from an XML file and returns a data frame
#' containing the extracted data.
#'
#' @param path The path to the XML file.
#' @param source The XML source, not used currently.
#'
#' @return A data frame containing the extracted reference information.
#'
#' @details This function uses XPath queries to extract specific fields from each record
#' in the XML file, such as author names, publication year, journal title, DOI, etc.
#' It then creates a data frame with these fields and returns it.
#'
#' @import XML
#' @import xml2
#' @import dplyr

#' @export
read_xml <- function(path, source){

  newdat <- XML::xmlParse(path)
  x <- XML::getNodeSet(newdat, "//record")
  xpath2 <- function(x, ...) {
    y <- XML::xpathSApply(x, ...)
    y <- gsub(",", "", y)
    ifelse(length(y) == 0, NA, paste(y, collapse = "; "))
  }

  newdat <- data.frame(author = sapply(x, xpath2, ".//author", xmlValue),
                       year = sapply(x, xpath2, ".//dates/year", xmlValue),
                       journal = sapply(x, xpath2, ".//periodical/full-title",  xmlValue),
                       doi = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
                       title = sapply(x, xpath2, ".//titles/title",  xmlValue),
                       pages = sapply(x, xpath2, ".//pages", xmlValue),
                       volume = sapply(x, xpath2, ".//volume", xmlValue),
                       number = sapply(x, xpath2, ".//number",  xmlValue),
                       abstract = sapply(x, xpath2, ".//abstract",  xmlValue),
                       keywords = sapply(x, xpath2, ".//keywords/keyword",  xmlValue),
                       record_id = sapply(x, xpath2, ".//rec-number", xmlValue),
                       isbn = sapply(x, xpath2, ".//isbn", xmlValue),
                       secondary_title = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
                       pmid = sapply(x, xpath2, ".//custom2", xmlValue),
                       label = sapply(x, xpath2, ".//label", xmlValue),
                       database = sapply(x, xpath2, ".//remote-database-name", xmlValue),
                       accession = sapply(x, xpath2, ".//accession-num", xmlValue),
                       url = sapply(x, xpath2, ".//urls/web-urls", xmlValue))


  }


#' Write reference data to a CSV file formatted for SYRF
#'
#' This function takes a data frame of reference information and writes it to a CSV file
#' formatted specifically for the Systematic Review Facility (SYRF).
#'
#' @param refs A data frame containing reference information.
#' @param filename The name of the file to write the data to.
#'
#' @return This function does not return anything explicitly. It writes the data frame
#' to a CSV file specified by `filename`.
#'
#' @details The function renames columns, adds additional columns required by SYRF,
#' handles missing URL columns, selects desired columns, and then writes the resulting
#' data frame to a CSV file with specified filename.
#'
#' @export
#' @import dplyr
write_for_syrf <- function(refs, filename){

  cols_to_modify <-  c('title', 'year', 'author', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label')
  refs[cols_to_modify] <- lapply(refs[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))


refs <- refs %>%
  rename(Authors = author, Title = title,
         Url = url, Abstract = abstract, Year = year, DOI = doi,
         PublicationName = journal) %>% mutate(AuthorAddress = "",
                                               AlternateName = "",
                                               ReferenceType = "",
                                               CustomId = record_id,
                                               Keywords = keywords,
                                               PdfRelativePath = "") %>%
  select(Title, Authors, PublicationName, AlternateName, PdfRelativePath, Abstract, Url, AuthorAddress, Year, DOI, ReferenceType,
         Keywords, CustomId)

write.csv(refs, filename,  row.names = F, quote = TRUE, na = "")
}

