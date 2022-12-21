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



flatten_dims <- function(x, parent = list(), simplify = TRUE){
  # dict <- list()
  dict <- parent
  dict[x[['name']]] <- x[['id']]
  if('children' %in% names(x)){
    # if(!is.null(x['children'])){
    print("found children")
    for(i in seq_along(x[['children']])){
      length(x['children'])
      child <- x[['children']][[i]]
      # dict[child[['name']] <- child[['id']]
      dict <- c(dict, flatten_dims(child))
    }
  }
  return(dict)
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
  api_classification = "dimension-instances/classification",
  api_measures = "api/dimension-instances/measure",
  api_gas = "dimension-instances/gas",
  # variable collection?
    api_variables = "variables/fq/"
  )
library(crul)
reqer <- crul::HttpClient$new(meta$api_base_url)
parties <- reqer$get(meta$api_parties_url)
parties_pretty <- jsonlite::fromJSON(parties$parse())


dimcat <- reqer$get(meta$api_categories)
dimcat_pretty <- jsonlite::fromJSON(dimcat$parse())


dimmeasure <- reqer$get(meta$api_measures)
dimmeasure_pretty <- jsonlite::fromJSON(dimmeasure$parse())


purrr::map_df(dimmeasure_pretty, list_dims, .id = 'Category')
purrr::map_df(dimcat_pretty, list_dims, .id = 'Category')
