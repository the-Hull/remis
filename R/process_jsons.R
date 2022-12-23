


# parse non-instances -----------------------------------------------------

#' Flatten UNFCCC DI info tables
#'
#' @param x response
#'
#' @return tibble
#' @export
flatten_json <- function(x){
  # identify nested column
  nested_cols <- colnames(x)[sapply(x, is.list)]
  print(nested_cols)
  flattened <- tidyr::unnest(x, cols = tidyselect::all_of(nested_cols), names_sep = "_", names_repair = "unique")
  return(flattened)
}



# parse instances ---------------------------------------------------------


#' Recursive unnest of instance lists
#'
#' Instance responses return lists after parsing. Each item corresponds to an
#' UNFCCC / Worldbank category (e.g., annex-I). This function parses the information,
#' i.e., `name` and `id` for each element and sub-element (e.g., LULUCF - Deforestation)
#' into a tibble reflecting the nestedness.
#'
#' @param x list, a category list from an instance response, such as
#' "api/dimension-instances/classification".
#' @param depth numeric, depth of nestedness.
#'
#' @return tibble with columns `id`, `level_x`, with x reflecting the nestedness,
#' and `name`. `id` is the DI API id, values in `level_x` are the corresponding name
#' for the id, or its parent, and name is the id's name.
#' @export
#'
flatten_dims <- function(x, depth = 1){

  nr <- length(x[['id']])


  info <- make_iddf(depth = depth, nrow = nr)


  info[,'id'] <- x[['id']]
  info[,1+depth] <- x[['name']]

  if('children' %in% names(x)){

    for(i in seq_along(x[['children']])){
      child <- x[['children']][[i]]

      new <- flatten_dims(x = child, depth = depth + 1)

      # carry the parent name
      new[,depth + 1] <- x[['name']][i]
      info <- dplyr::bind_rows(info, new)

    }
  }
  return(info)
}

# helpers -----------------------------------------------------------------

#' content placeholder
#'
#' Generate placeholder tibbles for
#'
#' @param nrow numeric, number of rows for the placeholder `tibble`.
#' Should be the same size as all non-children items in extraction list.
#' @param depth numeric, corresponds to depth in nested extraction list.
#'
#' @return tibble with columns `id`, `level_x`, with x reflecting the nestedness,
#' and `name`. `id` is the DI API id, values in `level_x` are the corresponding name
#' for the id, or its parent, and name is the id's name.
make_idtbl <- function(nrow, depth){

  out <- tibble::as_tibble(
    matrix(data = NA_character_,
           nrow = nrow,
           ncol = 1 + depth,
           byrow = TRUE),
    .name_repair = 'minimal')
  cn <- c('id', paste0("level_", seq_len(depth)))

  colnames(out) <- cn
  out <- mutate(out, id = as.integer(id))
  return(out)

}


#' Add 'name' column to unnested tables
#'
#' @param x data.frame/tibble with `level_x` columns from unnesting
#'
#' @return x with additional column `name`, or if nesting is only one level deep
#'  (i.e., `level_1`), the column is renamed
add_name_column <- function(x){

  if(ncol(x) == 2 & colnames(x)[2] == 'level_1'){
    names(x)[2] <- 'name'
    return(x)
  } else {
  column <- apply(x, MARGIN = 1, function(z) max(which(!is.na(z))))
  x$name <- vapply(seq_len(length(column)),
                      function(idx){
                        x[idx,column[idx], drop = TRUE]
                      }, FUN.VALUE = character(1))
  }
  return(x)
}
