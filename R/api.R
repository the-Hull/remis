# api meta ----------------------------------------------------------------



meta <- list(
  api_base_url = "https://di.unfccc.int/",
  # choose annex/parties/groups
  parties = "api/parties/",
  # choose years
  years = "api/years/single",
  # get units
  # variables for query here
  categories = "api/dimension-instances/category",
  classification = "api/dimension-instances/classification",
  measures = "api/dimension-instances/measure",
  gas = "api/dimension-instances/gas",
  # variable collection?
  # variables = "api/variables/fq/"
  # units
  units = "api/conversion/fq"
)



# rem_init ----------------------------------------------------------------

rem_init <- function(base_url = meta$api_base_url){

  req <- crul::HttpClient$new(meta$api_base_url)

  responses <- lapply(meta[!grepl('base', names(meta))],

                      function(u){
                        cat(sprintf('parsing %s\n', u))
                            req_parse(x = req, url = u)
                      })


  # pre-process parties
  responses$parties <- flatten_json(responses$parties)

  # pre-process nested values
  nested_obs <- c('categories', 'classification', 'measures')


  # loop through the three selected lists (nested_obs)
  responses[nested_obs] <- lapply(
    responses[nested_obs],
    function(x){

      # loop through all categories (annexOne, nonannexOne, extData, cad, cadCP2)
     lapply(x, function(y){

       # unnest all children in lists
       # add a final 'name' column to tables
       add_name_column(flatten_dims(y))
       })
    })



  return(responses)
}


# helpers -----------------------------------------------------------------

#' Get and parse objects
#'
#' @param x response from
#' @param url character, api path for desired object
#'
#' @return list, parsed from json
#' @export
#'
#' @examples
req_parse <- function(x, url){

  obj <- reqer$get(url)
  obj_pretty <- jsonlite::fromJSON(obj$parse(encoding = 'UTF-8'))

  return(obj_pretty)

}
