
# Helpers -----------------------------------------------------------------

#' Creates numeric data from raw tables
#'
#' Tables containing raw data and NO, NO, NE, NA values are transfomred to numeric
#' with meta values carried in an table as attribute "data_meta"
#'
#' @param x `data.frame`, raw data table with columns `sector` and years.
#'
#' @return `data.frame` of same form, but with `numeric` values in year columns
#' @export
set_data_numeric <- function(x){

  stopifnot(`Provide a dataframe.` = inherits(x, "data.frame"))

  data_mask <- apply(x[,-1], MARGIN = 2, function(x){grepl("(NO|NO|NE)", x)})
  data_meta <- x[,-1]
  data_meta[!data_mask] <- NA_character_
  data_meta <- cbind(x[,1, drop = FALSE], data_meta)

  data_numeric <- x[,-1]
  data_numeric[data_mask] <- NA_real_
  data_numeric <- suppressWarnings(apply(data_numeric, MARGIN = 2, as.numeric, simplify = FALSE))
  data_numeric <- cbind(x[,1, drop = FALSE], data_numeric)

  attr(data_numeric, "data_meta") <- data_meta

  return(data_numeric)

}




#' Identify categories from CRF trend tables
#'
#' @param x character, "raw" categories read directly from CRF trend tables
#'
#' @return character, short-format categories (e.g., 1.A.2, 2.)
clean_categories <- function(x){

  crf_top <- ""
  crf_sub <- ""
  crf_sub_sub <- ""
  # crf_sub_sub_sub

  # grab front only
  xlvls <- sub("^(.*[.])([ ]+.*)", "\\1", x)
  xlvls <- sub("[.]", "", xlvls)


  categories <- vector("character", length(x))

  for(idx in seq_along(xlvls)){

    if(idx ==1){
      categories[idx] <- xlvls[idx]
      crf_top <- 1
    } else {


      if(xlvls[idx] %in% LETTERS){

        # crf-sub level A-B
        crf_sub <- xlvls[idx]
        crf_sub_sub <- ""


      } else if(xlvls[idx-1] %in% LETTERS &
                xlvls[idx] == 1){
        #  found start of new subcategory
        crf_sub_sub <- xlvls[idx]


      } else if(xlvls[idx-1] %in% LETTERS &
                xlvls[idx] != 1){
        # found start of new top category
        crf_sub <- ""

        crf_top <- xlvls[idx]
      } else {
        # must be sub_sub
        crf_sub_sub <- xlvls[idx]

      }
    }

    crf <- paste(
      crf_top,
      crf_sub,
      crf_sub_sub,
      ".",
      sep = ".",
      recycle0 = FALSE
    )

    categories[idx] <- crf

    categories <- gsub("[.]+", ".", categories)

  }

  return(categories)

}



#' Tabled CRF-Categories
#'
#' @param x character, "raw" categories from CRF table
#' @return data.frame with 5 columns
#'
clean_categories_df <- function(x){
  category_df <- lapply(
    strsplit(clean_categories(trends$sector), "[.]"),
    function(x) as.data.frame(matrix(data = x, nrow = 1, byrow = FALSE))
  ) %>%
    dplyr::bind_rows()

  if(ncol(category_df)<5){

    category_df <- dplyr::bind_cols(
      category_df,
      as.data.frame(matrix(NA, ncol = 5-ncol(category_df))),
      .name_repair = 'minimal') %>%
      setNames(nm = 1:5)

  }

  return(category_df)
}


#' Identify CRF-Category levels
#'
#' @param x character, CRF-categories in short-form (e.g., 1.A.2, or 2.) from \link(`clean_categories()`)
#'
#' @return ch
get_crf_level <- function(x){

  # count dots in sector string
  crf_level <- sapply(gregexpr("[.]",x), function(x) length(x))

  return(crf_level)



}

#' Extract meta based on Table name
#'
#' @param tablename
#'
#' @return
#' @export
#'
#' @examples
# get_meta <- function(tablename){
# ....
# }


# # data I/O --------------------------------------------------------------




## read: trends ------------------------------------------------------------


#' Extract trend data from CRF Table
#'
#' @param path character, single file path to CRF table.
#' @param table character, table (i.e., sheet) name
#'
#' @return data.frame of CRF values
#' @export
read_emission_trends <- function(path, table = "Table10s1", total = FALSE){

  # check if table exists in meta
  # check if table exists in xlsx

  # grab appropriate meta
  tab_meta <- crf_meta[['emission_trends']][names(crf_meta$emission_trends) == table][[1]]

  ## build up cells:


  ghg <-
    readxl::read_excel(
      path,
      sheet = table,
      range = tab_meta$cells$ghg_cell,
      col_names = FALSE,
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
    )[[1]]

  years <-
    readxl::read_excel(
      path,
      sheet = table,
      range = tab_meta$cells$header,
      col_names = FALSE,
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
    )[1, ] %>%
    as.character()

  if(total){
    totals <-
      readxl::read_excel(
        path,
        sheet = table,
        range = tab_meta$cells$total_row,
        col_names = FALSE,
        .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
      )

    total_name <- readxl::read_excel(
      path,
      sheet = table,
      range = tab_meta$cells$total_name,
      col_names = FALSE,
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
    )[[1]]

    crf_table <- cbind(data.frame(crf_category_desc = total_name), totals)
    names(crf_table)[-1] <- years

    crf_table <- set_data_numeric(crf_table)

    attr(crf_table, "measure") <- ghg



    return(crf_table)

  }

  row_names <-
    readxl::read_excel(
      path,
      sheet = table,
      range = tab_meta$cells$row_names,
      col_names = FALSE,
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
    )[,1, drop = TRUE]


  values <-
    readxl::read_excel(
      path,
      sheet = table,
      range = tab_meta$cells$values,
      col_names = FALSE,
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
    )


  crf_table <- cbind(row_names, values)
  colnames(crf_table) <- c('crf_category_desc', years)

  crf_table <- set_data_numeric(crf_table)

  crf_table <- cbind(
    crf_category = clean_categories(crf_table[['crf_category_desc']]),
    crf_level = get_crf_level(clean_categories(crf_table[['crf_category_desc']])),
    crf_table)



  attr(crf_table, "measure") <- ghg


  return(crf_table)
}


