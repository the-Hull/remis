flatten_json <- function(x){
  # identify nested column
  nested_cols <- colnames(x)[sapply(x, is.list)]
  print(nested_cols)
  flattened <- tidyr::unnest(x, cols = tidyselect::all_of(nested_cols), names_sep = "_", names_repair = "unique")
  return(flattened)
}


list_dims <- function(x, simplify = TRUE){
  out <- flatten_dims(x)
  if(simplify){
    out <- data.frame(id = do.call(rbind, out), name = names(out), row.names = NULL)
  }
  return(out)
}



# flatten_dims <- function(x, parent = list(), simplify = TRUE){
#   dict <- parent
#   dict[x[['name']]] <- x[['id']]
#
#   if('children' %in% names(x)){
#
#     print("found children")
#
#
#
#     for(i in seq_along(x[['children']])){
#       length(x['children'])
#       child <- x[['children']][[i]]
#       dict <- c(dict, flatten_dims(child))
#     }
#   }
#   return(dict)
# }

make_iddf <- function(nrow, depth){

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


flatten_dims <- function(x, depth = 1, simplify = TRUE){

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







meta <- list(
  api_base_url = "https://di.unfccc.int/api",
  # choose annex/parties/groups
    api_parties_annex = c(nonannex = "nonAnnexOne", annex = "annexOne"),
  api_parties_url = "api/parties/",
  # choose years
    api_years = "api/years/single",
  # get units
    api_unit = "conversion/fq",
  # collect variables for query here
    api_categories = "api/dimension-instances/category",
  api_classification = "api/dimension-instances/classification",
  api_measures = "api/dimension-instances/measure",
  api_gas = "api/dimension-instances/gas",
  # variable collection?
    api_variables = "api/variables/fq/annexOne"
  )
library(crul)
reqer <- crul::HttpClient$new(meta$api_base_url)

parties <- reqer$get(meta$api_parties_url)
parties_pretty <- jsonlite::fromJSON(parties$parse())
flatten_json(parties_pretty)

vars <- reqer$get(meta$api_variables)
vars_pretty <- jsonlite::fromJSON(vars$parse())
flatten_json(vars_pretty)

dimcat <- reqer$get(meta$api_categories)
dimcat_pretty <- jsonlite::fromJSON(dimcat$parse())
flatten_dims(dimcat_pretty$extData)
flatten_dims(dimcat_pretty$nonAnnexOne)
flatten_dims(dimcat_pretty$cad)

dimclass <- reqer$get(meta$api_classification)
dimclass_pretty <- jsonlite::fromJSON(dimclass$parse())
flatten_dims(dimclass_pretty$annexOne)


dimmeasure <- reqer$get(meta$api_measures)
dimmeasure_pretty <- jsonlite::fromJSON(dimmeasure$parse())
flatten_dims(dimmeasure_pretty$annexOne)




dimgas <- reqer$get(meta$api_gas)
dimgas_pretty <- jsonlite::fromJSON(dimgas$parse())
flatten_dims(dimgas_pretty$annexOne)


test <- flatten_dims(dimcat_pretty$cad)

column <- apply(test, MARGIN = 1, function(x) max(which(!is.na(x))))
test$name <- sapply(seq_len(length(column)),
                    function(idx){

                      test[idx,column[idx]]

})

purrr::map_df(dimmeasure_pretty, list_dims, .id = 'Category')
purrr::map_df(dimcat_pretty, list_dims, .id = 'Category')
