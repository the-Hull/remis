## code to prepare `meta` dataset goes here



#' Supported Tables (sheets) from Inventory data
#'
#' Manually enter supported tables
#'
#' @return list of tables
supply_table_list <- function(){
  # Manually curated list
  tables <- list(
    emission_trends = c(
      "Table10s1",
      "Table10s2",
      "Table10s3",
      "Table10s4"
      # "Table10s5",
      # "Table10s6"
    )
  )
  return(tables)
}






#' Helper to generate a nested meta list
#'
#' @param tables
#'
#' @return empty `list` with names and position following supported tables
gen_meta_list <- function(tables){


  meta_list <- purrr::imap(
    tables,
    function(x, nm){

      out <- vector("list", length(x))
      names(out) <- x
      return(out)

    })

  return(meta_list)
}



#' Meta data
#'
#' Manually enter meta data for tables
#' Follow conventions based on existing example
#'
#' @return list of meta data attributes fo reach table.
supply_meta <- function(){


  t_attr <- list(


    #### Table10s1 -----------------------------------------------------------
    Table10s1 = list(
      table = "Table10s1",
      type = 'emission_trends',
      measure = 'time series',
      units = 'kt_co2_eq',
      ghg = 'co2',

      cells = list(
        ghg_cell = 'A2',
        header_type = 'years',
        header = 'B5:AG5',
        row_names = 'A8:A54',
        values = 'B8:AG54',
        total_row = 'B7:AG7',
        total_name = 'A7'
      ) # / cells

    ), # / Table10s1




    #### Table10s2 ---------------------------------------------------------------
    Table10s2 = list(
      table = "Table10s2",
      type = 'emission_trends',
      measure = 'time series',
      units = 'kt',
      ghg = 'co2',

      cells = list(
        ghg_cell = 'A2',
        header_type = 'years',
        header = 'B5:AG5',
        row_names = 'A7:A53',
        values = 'B7:AG53',
        total_row = 'B65:AG65',
        total_name = 'A65'
      ) # / cells

    ), # / Table10s2

    #### Table10s3 ---------------------------------------------------------------
    Table10s3 = list(
      table = "Table10s3",
      type = 'emission_trends',
      measure = 'time series',
      units = 'kt',
      ghg = 'ch4',

      cells = list(
        ghg_cell = 'A2',
        header_type = 'years',
        header = 'B5:AG5',
        row_names = 'A7:A53',
        values = 'B7:AG53',
        total_row = 'B55:AG55',
        total_name = 'A55'
      ) # / cells

    ), # / Table10s3


    #### Table10s4 ---------------------------------------------------------------
    Table10s4 = list(
      table = "Table10s4",
      type = 'emission_trends',
      measure = 'time series',
      units = 'kt',
      ghg = 'n2o',

      cells = list(
        ghg_cell = 'A2',
        header_type = 'years',
        header = 'B5:AG5',
        row_names = 'A7:A53',
        values = 'B7:AG53',
        total_row = 'A55:AG55',
        total_name = 'A55'
      ) # / cells

    ) # / Table10s4


  ) # / t_attr

  return(t_attr)
}


#' Fill meta list with meta data
#'
#' @param meta_list
#' @param table_attributes
#'
#' @return
#' @export
#'
#' @examples
populate_meta_list <- function(meta_list, table_attributes){

  # check if table in meta list

  for(tab in table_attributes){

    table_name <- tab[['table']]
    table_type <- tab[['type']]

    meta_list[[table_type]][[table_name]] <- tab

  }

  return(meta_list)

}


ml <- gen_meta_list(tables = supply_table_list())


crf_meta <- populate_meta_list(meta_list = ml, table_attributes = supply_meta())


usethis::use_data(crf_meta, internal = TRUE, overwrite = TRUE)
