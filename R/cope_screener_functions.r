url <- 'https://redcap.nyu.edu/api/'

#' COPE SCREENER PACKAGE
#' 
#' This function retrieves data from the COPE Screener REDCap project using the API.
#'
#' @param token The API token for the project
#' @param form The name of the REDCap form to retrieve data from
#' @param raw_v_label The label for raw data fields
#' @return A data frame with the cope_screener_survey form
#' @export
get_cope_screener <- function(token = token, form = 'cope_screener_survey', raw_v_label = 'raw') {
  record_filter = paste("[", form, "_complete]=2", sep = "")
  formData <- list(uri = url,
                   "token"=token,
                   content='record',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   'fields[0]'='record_id', # NOTE this is the subject ID for COPE, change if different
                   'forms[0]'=form,
                   rawOrLabel=raw_v_label,
                   rawOrLabelHeaders=raw_v_label,
                   exportCheckboxLabel='false',
                   exportSurveyFields='true',
                   exportDataAccessGroups='false',
                   returnFormat='csv',
                   filterLogic=record_filter)
  
  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  df[df == -888] = NA
  df[df == 888] = NA
  df[df == 8888] = NA
  df[df == -999] = NA
  df[df == 999] = NA
  df[df == 9999] = NA # NOTE values can be changed, or new ones added, to account for different missing data flags
  return (df)
}

#' This function retrieves cleaned data from the COPE Screener REDCap project using the API.
#'
#' @param token The API token for the project
#' @param min_date The minimum date you want to pull screener responses for. Default is all
#' @return A data frame with the cleaned screener responses for your specified date range
#' @export
get_cope_screener_clean <- function(token, min_date = '2020-01-01') {
  screener <- get_cope_screener(token)
  screener$cope_screener_survey_timestamp <- format(as.Date(screener$cope_screener_survey_timestamp), "%Y-%m-%d")
  screener <- dplyr::filter(screener, cope_screener_survey_timestamp > min_date)
  screener <- dplyr::filter(screener, rec_over_18 == 1)
  screener <- dplyr::select(screener,
                     rec_language_preference,rec_caregiver_name, rec_caregiver_email,
                     rec_phone_number, rec_source, rec_babydob, rec_over_18, rec_address_zipcode)
  screener <- dplyr::rename(screener,
                     language = rec_language_preference,
                     cg_name = rec_caregiver_name,
                     email = rec_caregiver_email,
                     phone = rec_phone_number,
                     source = rec_source, child_dob = rec_babydob, zipcode = rec_address_zipcode)
  screener$child_dob <- as.Date(screener$child_dob, format = "%Y-%m-%d")
  screener$child_age <- difftime(Sys.Date(),screener$child_dob, units = "days")
  screener$child_age <- as.numeric(screener$child_age)
  screener$source <- gsub("1", "social media ad", screener$source)
  screener$source <- gsub("2", "facebook group", screener$source)
  screener$source <- gsub("3", "participant referral", screener$source)
  screener$source <- gsub("4", "organization", screener$source)
  screener$source <- gsub("5", "other", screener$source)
  screener <- dplyr::mutate(screener,language = ifelse(stringr::str_detect(language, "spa"), "Spanish", language))
  screener <- dplyr::mutate(screener,
                            timepoint = ifelse(dplyr::between(child_age, 0, 60), "newborn", 
                                               ifelse(dplyr::between(child_age, 61, 227), "6m",
                                                      ifelse(dplyr::between(child_age, 228, 304), "9m",
                                                             ifelse(dplyr::between(child_age, 305,396), "12m",
                                                                    ifelse(dplyr::between(child_age, 397, 590), "18m",
                                                                           ifelse(dplyr::between(child_age, 398, 1034), "30m", "INELIGIBLE")))))))
  col_order <- c("cg_name", "email", "phone", "child_dob", "child_age", "timepoint",
                 "source", "language", "rec_over_18", "zipcode")
  screener <- screener[, col_order]
  screener <- dplyr::arrange(screener, child_age)
  return (screener)
}