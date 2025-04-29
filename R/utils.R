#' detects variables names in code
#'
#' @param codeintext a string vector to look for variables
#'
#' @return a vector with strings
#' @export
#'
#' @examples
#' string_to_check <- c(
#'   "ki_age> 1",
#'   "noo==\"mar\"",
#'   "sum(var1, var2, na.rm = T)",
#'   "oks != \"mar2\"",
#'   "oknospace!=\"mar2\""
#' )
#' detect_variable(string_to_check)
detect_variable <- function(codeintext) {
  var_vector <- codeintext %>%
    stringr::str_replace_all(pattern = "[:symbol:]", replace = " ") %>% # removes anything | ` =  + ^ ~ < > $
    stringr::str_replace_all(
      pattern = stop_words_replace %>% stringi::stri_c(collapse = "|"),
      replace = " "
    ) %>% # removes anything ! & %

    stringi::stri_split(regex = " ", omit_empty = T) %>%
    do.call(c, .) %>%
    stringr::str_subset(pattern = "\"", negate = T) %>% # removes anything that starts with a quote
    stringr::str_subset(pattern = "^[:digit:]+$", negate = T) %>% # removes anything that is only number
    stringr::str_subset(pattern = "[:alnum:]+(?=\\()", negate = T) %>% # removes functions, i.e. text(
    stringr::str_replace_all(pattern = "\\(|\\)", " ") %>% # removes parenthesis
    stringr::str_replace_all(pattern = "\\(|\\)", " ") %>%
    stringr::str_subset(
      pattern = stop_words_functions %>% stringi::stri_c(collapse = "|"),
      negate = T
    ) %>% # removes stop words from functions
    stringr::str_trim() %>%
    unique()

  var_vector[var_vector != ""]
}
stop_words_replace <- c("!", "&", "%")
stop_words_functions <- c("na.rm", "TRUE", "FALSE", "T", "F")


#' Verify if the Kobo survey dataframe is valid
#'
#' This function checks whether the provided Kobo survey dataframe meets certain criteria:
#' 1. It is a dataframe.
#' 2. It is not empty.
#' 3. It contains columns named "type" and "name".
#'
#' @param kobo_survey A dataframe representing the Kobo survey sheet.
#'
#' @return Logical. Returns `TRUE` if the dataframe meets all the criteria. Otherwise, it returns `FALSE`.
#'
#' @export
#'
#' @examples
#' # Assume df_valid is a valid Kobo survey dataframe
#' df_valid <- data.frame(type = c("integer", "selecte_one yesno"), name = c("age", "consent"))
#' verify_valid_survey(df_valid) # should return TRUE
#'
#' # Assume df_invalid lacks the required columns
#' df_invalid <- data.frame(column1 = c("integer", "selecte_one yesno"), column2 = c("age", "consent"))
#' verify_valid_survey(df_invalid) # should return FALSE
verify_valid_survey <- function(kobo_survey) {
  if (!is.data.frame(kobo_survey)) {
    return(F)
  }
  if (length(kobo_survey) == 0) {
    return(F)
  }
  if (!all(c("type", "name") %in% names(kobo_survey))) {
    return(F)
  }

  return(TRUE)
}

#' Verify if the Kobo choices dataframe is valid
#'
#' This function checks whether the provided Kobo choices dataframe meets certain criteria:
#' 1. It is a dataframe.
#' 2. It is not empty.
#' 3. It contains columns named "list_name" and "name".
#'
#' @param kobo_choices A dataframe representing the Kobo choices sheet.
#'
#' @return Logical. Returns `TRUE` if the dataframe meets all the criteria. Otherwise, it returns `FALSE`.
#'
#' @export
#'
#' @examples
#' # Assume df_valid_choices is a valid Kobo choices dataframe
#' df_valid_choices <- data.frame(list_name = c("ChoiceA", "ChoiceB"), name = c("Option1", "Option2"))
#' verify_valid_choices(df_valid_choices) # should return TRUE
#'
#' # Assume df_invalid_choices lacks the required columns
#' df_invalid_choices <- data.frame(column1 = c("ChoiceA", "ChoiceB"),
#'                                  column2 = c("Option1", "Option2"))
#' verify_valid_choices(df_invalid_choices) # should return FALSE
verify_valid_choices <- function(kobo_choices) {
  if (!is.data.frame(kobo_choices)) {
    return(F)
  }
  if (length(kobo_choices) == 0) {
    return(F)
  }
  if (!all(c("list_name", "name") %in% names(kobo_choices))) {
    return(F)
  }

  return(TRUE)
}


#' Create a Validation List for Data Entry
#'
#' This function generates a validation list to be used for data entry validation.
#' It combines predefined validation lists with dynamically formatted choices derived from
#' the `choices` and `tool` parameters. The resulting dataframe is intended to provide
#' structured and valid choices for various question types.
#'
#' @param choices A dataframe representing the Kobo choices sheet.
#'        Expected to have at least the columns `list_name` and `name`.
#' @param tool A dataframe representing the Kobo survey sheet.
#'        Expected to have at least the columns `type` and `name`.
#' @param others Whether 'Others' should be included in the validation list. Defaults to FALSE.
#'
#' @return A dataframe where each column corresponds to a choice list for a specific question.
#'         Each row contains a valid choice for the question.
#'
#' @examples
#' # Assume choices_df and tool_df are sample dataframes that fit the expected structure.
#' # validation_list <- create_validation_list(choices, tool)
#'
#' @export
create_validation_list <- function(choices, tool, others = F) {
  new_lists <- list(
    c("change_type_validation", "change_response;\nblank_response;\nremove_survey;\nno_action"),
    c("binaries_sm_options_lgl", "FALSE;\nTRUE"),
    c("binaries_sm_options_num", "0;\n1")
    # c("_duplicates_","-- keep the survey --;\n-- delete the survey --"),
    # c("_action_","-- confirm --;\n-- update --;\n-- delete --")
  ) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    stats::setNames(c("name", "choices"))

  choicelist <- new_lists %>%
    dplyr::bind_rows(create_formatted_choices(choices, tool) %>%
                       dplyr::select(name, choices))

  if (others) {

    extra = create_other_df(tool = tool, choices = choices)

    choicelist <- dplyr::bind_rows(
      choicelist,
      extra %>% dplyr::rename(name = other_question)
    )
  }

  choice_validation <- choicelist %>%
    unique() %>%
    data.table::transpose() %>%
    stats::setNames(.[1, ]) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate_all(~ stringr::str_split(., ";\n"))

  nrow_validation <- lapply(choice_validation, function(x) length(x[[1]])) %>%
    unlist() %>%
    max()

  data.val <- data.frame(matrix(NA, nrow = nrow_validation, ncol = 0))

  for (c in colnames(choice_validation)) {
    data.val <- data.val %>%
      dplyr::mutate(!!rlang::sym(c) := c(unlist(choice_validation[[c]]), rep(NA, nrow_validation - length(choice_validation[[c]][[1]]))))
  }

  return(data.val)
}


#' Format and filter Choices for 'select_one' Questions
#'
#' This function returns a dataframe containing 'select_one' questions
#' from the `tool` dataframe with their corresponding choices in the `choices` dataframe.
#'
#' @param choices A dataframe containing the choices sheet from the Kobo tool
#'        Expected to have at least the columns `list_name` and `name`.
#'
#' @param tool A dataframe containing the survey sheet from the Kobo tool
#'        Expected to have at least the columns `type` and `name`.
#'
#' @return A dataframe containing 'select_one' questions with their corresponding choices.
#'         The dataframe has columns from both the `tool` and `choices` dataframes.
#'
#' @examples
#' # formatted_choices <- create_formatted_choices(choices, tool)
#'
#' @export
create_formatted_choices <- function(choices, tool) {
  list.choices <- choices %>%
    dplyr::filter(!is.na(list_name)) %>%
    dplyr::group_by(list_name) %>%
    dplyr::mutate(
      choices = paste(name, collapse = ";\n"),
      choices.label = paste(!!rlang::sym(names(choices)[3]), collapse = ";\n")
    ) %>%
    dplyr::summarise(choices = choices[1], choices.label = choices.label[1])

  select.questions <- tool %>%
    dplyr::select(type, name) %>%
    dplyr::mutate(
      q.type = as.character(lapply(type, function(x) {
        return(stringr::str_split(x, " ")[[1]][1])
      })),
      list_name = as.character(lapply(type, function(x) {
        x.1 <- stringr::str_split(x, " ")[[1]]
        if (length(x.1) == 1) {
          return(NA)
        } else {
          return(x.1[2])
        }
      }))
    ) %>%
    dplyr::filter(q.type == "select_one") %>%
    dplyr::filter(!is.na(list_name) & list_name != "NA" & list_name != "group" & list_name != "repeat") %>%
    dplyr::left_join(list.choices, by = "list_name") %>%
    dplyr::filter(!is.na(choices))

  return(select.questions)
}



#' Generate excel range to be used for the data validation formula in excel
#'
#' This function returns an Excel cell range for a given column in a dataframe.
#' The range is formatted for use in Excel functions or formulas and is prefixed
#' with the sheet name 'validation_rules'.
#'
#' @param variable The name of the column/question for which the Excel range is to be generated.
#' @param data.val A dataframe containing the column specified by `variable`.
#'
#' @return A string representing the Excel cell range containing all choices for the column `variable`
#'         in the sheet 'validation_rules'.
#'
#' @examples
#' # Assume df is a sample dataframe with a column named 'consent'.
#' # range_string <- create_col_range('consent', df)
#'
#' @export
create_col_range <- function(variable, data.val) {
  column.number <- which(colnames(data.val) == variable)
  all <- expand.grid(LETTERS, LETTERS)
  all <- all[order(all$Var1, all$Var2), ]
  alphabet <- c(LETTERS, do.call("paste0", all))
  col.excel <- alphabet[column.number]
  nrow <- nrow(data.val %>% dplyr::filter(!is.na(!!rlang::sym(variable))))
  range.vect <- c("$", col.excel, "$2:$", col.excel, "$", (nrow + 1))
  range <- paste(range.vect, sep = "", collapse = "")
  value.sheet <- paste("'validation_rules'!")
  value <- paste(value.sheet, range, sep = "", collapse = "")
  return(value)
}

#' Coerce numeric values to character, without scientific noting and NA are kept as NA.
#'
#' @param x a value to convert
#'
#' @return x as character
#' @export
#'
#' @examples
#'
#' coerce_to_character(c("a", NA))
#' coerce_to_character(c(1, 10000000, NA))
#'
coerce_to_character <- function(x) {
  format(x, scientific = F, justify = "none", trim = T) %>%
    dplyr::na_if("NA")
}



#' Create 'Other' Question Mapping Table
#'
#' This function identifies text questions in an XLSForm tool that are used to capture "other" responses
#' (i.e., follow-up text entries when 'other' is selected from a parent select question), and constructs
#' a mapping between these "other" text questions and the choices of their parent select questions.
#'
#' It returns a data frame where each row represents an "other" text question, and the corresponding
#' answer choices (excluding the 'other' option itself) from the parent select question. This is subsequently used to define the drop down options.
#'
#' @param tool A data frame representing the `survey` sheet from an XLSForm. Must contain columns `type`, `name`, and `relevant`.
#' @param choices A data frame representing the `choices` sheet from an XLSForm. Must contain columns `list_name` and `name`.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{other_question}{The name of the "other" text question.}
#'   \item{choices}{A semicolon-separated string of answer choices from the parent select question (excluding "other").}
#' }
#'
#' @details
#' This function assumes that "other" questions are identifiable by a `relevant` expression ending in `'other')`,
#' and that the parent question is referenced inside curly brackets (`{}`) in the relevant column.
#'
#' The function will throw an error if no valid "other" question mappings are found.
#'
#' @examples
#' \dontrun{
#' survey <- readxl::read_excel("myform.xlsx", sheet = "survey")
#' choices <- readxl::read_excel("myform.xlsx", sheet = "choices")
#' create_other_df(tool = survey, choices = choices)
#' }
#'
#' @importFrom dplyr filter mutate select bind_rows left_join
#' @importFrom stringr str_detect str_extract str_remove_all str_split str_split_i
#' @export
create_other_df <- function(tool, choices) {

  # Step 1: Identify relevant 'other' text questions and their parent question names
  other_text_questions <- tool %>%
    dplyr::filter(type == "text", stringr::str_detect(relevant, "'other'\\)\\s*$")) %>%
    dplyr::mutate(
      parent_question = stringr::str_extract(relevant, "\\{([A-Za-z0-9_]+)\\}") %>%
        stringr::str_remove_all("\\{|\\}")
    )

  # Step 2: Get list names for the parent questions
  parent_questions <- tool %>%
    dplyr::filter(stringr::str_detect(type, "select")) %>%
    dplyr::mutate(list_name = stringr::str_split_i(type, " ", 2)) %>%
    dplyr::filter(name %in% other_text_questions$parent_question)

  # Step 3: Filter choices to only relevant list_names
  relevant_choices <- choices %>%
    dplyr::filter(list_name %in% parent_questions$list_name)

  # Step 4: Combine everything into wide format: other question ~ its 'other' answer options
  questions_and_answers <- parent_questions %>%
    # parent_questions has: name (parent) and list_name
    dplyr::left_join(
      other_text_questions %>%
        dplyr::select(other_question = name, parent_question),
      by = c("name" = "parent_question")
    ) %>%
    dplyr::left_join(
      choices %>% dplyr::rename(answer_name = name),
      by = "list_name"
    ) %>%
    dplyr::filter(answer_name != 'other') %>%
    dplyr::group_by(other_question) %>%
    dplyr::summarise(
      choices = paste(answer_name, collapse = ";\n"),
      .groups = "drop"
    )

  if (nrow(questions_and_answers) == 0) {
    stop("No 'other' questions with matching choices found.")
  }

  return(questions_and_answers)
}



