#' @name auto_detect_sm_parents
#' @rdname auto_detect_sm_parents
#' @title Detect select multiple parent columns
#' @description `auto_detect_sm_parents` is mean to detect select multiple parent columns in a way that does
#' not rely on the XLSForm as the input
#' @param dataset dataset to correct
#' @param sm_separator Separator for choice multiple questions. The default is "."
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a list of select multiple parent columns in data set.
#' @export
auto_detect_sm_parents <- function(dataset, sm_separator = ".") {
  sm_parents <-
    sub(glue::glue(".[^\\{sm_separator}]*$"),
        "",
        colnames(dataset))
  sm_parents <- data.frame(col_names = sm_parents[sm_parents != ""])
  select_multiple_detected <- sm_parents %>%
    dplyr::group_by(col_names) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(col_names)
  return(as.character(select_multiple_detected$col_names))
}
#' @name auto_sm_parent_child
#' @rdname auto_sm_parent_child
#' @title detect and group together select multiple parent and children columns
#' @description `auto_sm_parent_children` is mean to detect select multiple parent columns & children columns in a way that does
#' not rely on the XLSForm as the input
#' @param dataset dataset to correct
#' @param sm_separator Separator for choice multiple questions. The default is "."
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a data frame containing the the child select multiple columns alongside there parents and
#' a log with all changes recorded.
#' @export


auto_sm_parent_children <- function(dataset, sm_separator = ".") {
  sm_parents <- auto_detect_sm_parents(dataset, sm_separator)
  sm_child <- dataset %>%
    dplyr::select(dplyr::starts_with(glue::glue("{sm_parents}{sm_separator}"))) %>%
    colnames()
  dplyr::tibble(sm_parent = sub(glue::glue(".[^\\{sm_separator}]*$"), "", sm_child),
                sm_child)
}



#' This function recreates the columns for select multiple questions
#'
#' @param dataset data frame
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param kobo_survey Kobo survey sheet.
#' @param kobo_choices Kobo choices sheet.
#' @param sm_separator Separator for choice multiple questions. The default is "."
#' @param cleaning_log_to_append A cleaning log where to add the changes from this functions.
#' Names of the log from this function are  "uuid", "question", "change_type", "new_value",
#' "old_value", "comment". If the cleaning_log_to_append names are not matching, the only way is to
#' create without a cleaning_log_to_append, and rename the columns and then bind.
#'
#' @export
#' @examples
#' test_data <- dplyr::tibble(
#'   uuid = paste0("uuid_", 1:6),
#'   gender = rep(c("male", "female"), 3),
#'   reason = c(
#'     "xx,yy", "xx,zy",
#'     "zy", "xx,xz,zy",
#'     NA_character_, "xz"
#'   ),
#'   reason.xx = c(0, 1, 0, 1, 0, 0),
#'   reason.yy = c(1, 0, 0, 0, 1, 0),
#'   reason.xz = c(0, 0, 0, 1, 0, 1),
#'   reason.zy = c(0, 1, 1, 1, 0, 0),
#'   reason_zy = c(NA_character_, "A", "B", "C", NA_character_, NA_character_)
#' )
#' recreate_parent_column(dataset = test_data, uuid_column = "uuid", sm_separator = ".")
#'
recreate_parent_column <- function(dataset, uuid_column = "uuid", kobo_survey = NULL,
                                   kobo_choices = NULL, sm_separator = ".", cleaning_log_to_append = NULL) {
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(purrr)

  checked_data <- dataset
  initial_order <- names(dataset)

  if (is.null(kobo_survey)) {
    old_name <- names(dataset)
    number_of_separator <- max(str_count(names(dataset), fixed(sm_separator)), na.rm = TRUE)
    for (i in seq_len(number_of_separator)) {
      names(dataset) <- sub(paste0("(\\", sm_separator, ".*?)\\", sm_separator), "\\1_", names(dataset))
    }
    cols_order <- names(dataset)
    name_diff <- tibble(old_name = old_name, new_name = cols_order) %>% filter(old_name != new_name)
    if (nrow(name_diff) > 0) {
      warning("Column names were renamed due to multiple separators. See changes:")
      print(name_diff)
    }
    select_multiple <- auto_sm_parent_children(dataset, sm_separator = sm_separator)
  } else {
    sm_raw <- kobo_survey %>% filter(str_detect(type, "^select_multiple ")) %>%
      transmute(list_name = str_remove(type, "select_multiple "), sm_parent = name)
    sm_choices <- sm_raw %>% left_join(kobo_choices, by = "list_name", relationship = "many-to-many") %>%
      transmute(sm_parent, sm_child = paste0(sm_parent, sm_separator, name))
    missing <- setdiff(sm_choices$sm_child, names(dataset))
    if (length(missing) > 0) {
      warning("Some SM child columns not found in dataset:")
      print(missing)
    }
    select_multiple <- sm_choices %>% filter(sm_child %in% names(dataset))
  }

  if (nrow(select_multiple) == 0) {
    correction_log <- tibble(!!uuid_column := "all", comment = "No select_multiple questions detected")
    return(list(data_with_fix_concat = dataset, correction_parent_sm_log = correction_log))
  }

  reconstructed_list <- select_multiple %>% group_split(sm_parent) %>% map(function(df) {
    parent <- unique(df$sm_parent)
    child_cols <- df$sm_child
    long_df <- dataset %>%
      select(all_of(c(uuid_column, parent, child_cols))) %>%
      pivot_longer(cols = all_of(child_cols), names_to = "child", values_to = "value") %>%
      filter(value %in% c("1", 1, TRUE, "TRUE")) %>%
      mutate(child_clean = str_remove(child, paste0("^", fixed(parent), fixed(sm_separator)))) %>%
      group_by(uuid) %>%
      summarise(
        orig = list(unique(na.omit(.data[[parent]]))),
        new_tokens = list(child_clean),
        .groups = "drop"
      ) %>%
      mutate(
        orig_tokens = map(orig, ~ str_split(.x, "\\s+")[[1]]),
        same_set = map2_lgl(orig_tokens, new_tokens, ~ setequal(.x, .y)),
        new_final = map2_chr(same_set, seq_along(orig), ~ {
          if (.x) orig[[.y]] else paste(new_tokens[[.y]], collapse = " ")
        })
      ) %>%
      transmute(!!parent := new_final, !!uuid_column := .data[[uuid_column]])
  })


  final_parents <- reduce(reconstructed_list, full_join, by = uuid_column)
  parent_names <- unique(select_multiple$sm_parent)
  dataset_cleaned <- dataset %>% select(-any_of(parent_names)) %>% left_join(final_parents, by = uuid_column)
  dataset_cleaned <- dataset_cleaned %>% select(all_of(initial_order))

  correction_log <- create_cleaning_log(
    raw_dataset = checked_data,
    raw_dataset_uuid_column = uuid_column,
    clean_dataset = dataset_cleaned,
    clean_dataset_uuid_column = uuid_column
  )

  if ("comment" %in% names(correction_log)) {
    correction_log <- correction_log %>% mutate(
      comment = str_replace_all(comment, "An alteration was performed", "Parent column changed to match children columns"),
      comment = str_replace_all(comment, "changed to NA", "changed to NA with recreate_parent_column correction, all value are FALSE")
    )
  }

  if (!is.null(cleaning_log_to_append)) {
    return(list(
      data_with_fix_concat = dataset_cleaned,
      cleaning_log = bind_rows(cleaning_log_to_append, correction_log)
    ))
  } else {
    return(list(
      data_with_fix_concat = dataset_cleaned,
      correction_parent_sm_log = correction_log
    ))
  }
}
