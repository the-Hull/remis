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
  variables_annexone = "api/variables/fq/annexOne",
  # units
  units = "api/conversion/fq"
)



# rem_init ----------------------------------------------------------------

#' Initialize remis
#'
#' @param base_url character, base url for queries
#'
#' @return list
#' @export
#'
#' @examples
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

  responses$.req <- req


  return(responses)
}


# helpers -----------------------------------------------------------------

#' Get and parse objects
#'
#' @param x response from
#' @param url character, api path for desired object
#'
#' @return list, parsed from json
#'
req_parse <- function(x, url){

  obj <- x$get(url)
  obj_pretty <- jsonlite::fromJSON(obj$parse(encoding = 'UTF-8'))

  return(obj_pretty)

}


#' Variable selector
#'
#' Select variables based on categories, classifications, measures and gasses using
#' ids. Variable selection is based on the union of all supplied ids, non-intersecting.
#' If no ids are supplied, all variables are returned.
#'
#' @param vars data.frame of variable ids with corresponding cat/class/meas/gas ids
#' @param category_id integer
#' @param classification_id integer
#' @param measure_id integer
#' @param gas_id integer
#'
#' @return data.frame with same dims of `vars` if no ids are supplied, or with rows
#' resulting from union of ids, and columns of `vars`
select_vars <- function(
    vars,
    category_id = NULL,
    classification_id = NULL,
    measure_id = NULL,
    gas_id = NULL){

  nvars <- nrow(vars)


  # collect ids
  id_lists <- list(
    categoryId = category_id,
    classificationId = classification_id,
    measureId = measure_id,
    gasId = gas_id)

  # if all entries are null, provide all vars to download everything
  # if any entry is non-null, limit to respective ids
  null_entries <- unlist(sapply(id_lists, is.null))

  get_all_ids <- all(null_entries)

  # check if no ids supplied
  if(get_all_ids){

    cat(sprintf('No ids were supplied. Defaulting to selecting all %d\ variables.\n', nvars))
    return(vars)
  } else {

    id_lists_entries <- id_lists[!null_entries]
    masks <- lapply(seq_along(id_lists_entries),
                    function(idx){
                      category_mask <- vars[[names(id_lists_entries)[idx]]] %in% id_lists_entries[[idx]]
                      if(sum(category_mask) == 0 ){
                        stop(sprintf('Could not find any matching %ss. Please cross-check with the variable table or respective info table.', names(id_lists)[idx]))
                      }
                      return(category_mask)
                    }
    )

    masks <- do.call(cbind, masks)

    # find all rows with TRUE hits in mask
    vars_sub <- vars[rowSums(masks) > 0,]

    return(vars_sub)
  }

}




