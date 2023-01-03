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
  # units
  units = "api/conversion/fq",
  variables = list(annexOne = "api/variables/fq/annexOne",
                   nonAnnexOne = "api/variables/fq/nonAnnexOne")
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
                        if(class(u) == 'character'){
                          cat(sprintf('parsing %s\n', u))
                          get_parse(x = req, url = u)
                        } else if(class(u) == 'list'){

                          lapply(u, function(v){
                          cat(sprintf('parsing %s\n', v))
                          get_parse(x = req, url = v)
                          })

                        }
                      })


  # pre-process parties
  responses$parties <- flatten_json(responses$parties)
  ## clean names
  names(responses$parties)[names(responses$parties) == 'parties_id'] <- 'id'
  names(responses$parties)[names(responses$parties) == 'name'] <- 'name_code'
  names(responses$parties)[names(responses$parties) == 'parties_name'] <- 'name'

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

#' GET and parse objects
#'
#' @param x response from
#' @param url character, api path for desired object
#'
#' @return list, parsed from json
#'
get_parse <- function(x, url){

  obj <- x$get(url)
  obj_pretty <- jsonlite::fromJSON(obj$parse(encoding = 'UTF-8'))

  return(obj_pretty)

}


#' POST and parse objects
#'
#' @param x list, remis object
#' @param url character, api path for flex query
#'
#' @return data.frame, parsed from json
#'
post_parse <- function(x, url = 'api/records/flexible-queries/', query){

  # check if query is empty warn for large (!) download

  # replace all ids with existing ones?


  obj <- x$.req$post(
    path = url,
    body = query,
    encode = 'json'
    )

  # check if response is 200 or fail


  obj_pretty <- jsonlite::fromJSON(obj$parse(encoding = 'UTF-8'))

  return(obj_pretty)

}

#' Readable query results
#'
#' This function combines numeric/character responses from queries with their
#' respective id values from parties, years and variables.
#'
#' @param raw list, response from POST request
#' @param rms list, `remis` object
#'
#' @return data.frame
parse_raw <- function(raw, rms){


  nr <- nrow(raw)

  pts <- character(nr)
  yrs <- character(nr)

  # get parties
  parties <- get_names(raw$partyId, rms$parties)

  # get years
  years <- get_names(raw$yearId, rms$years)


  ## variables
  variables <- get_variables(raw$variable, rms)








  # id_cols <- c('partyId', 'yearId')
  #
  # rows <- vector('list', nrow(raw))
  #
  #
  # for(idx in seq_len(nrow(raw))){
  #
  #   rows[idx] <- list(sapply(raw[idx, id_cols],
  #          function(x) find_id(rms, id = x, verbose = FALSE))
  #   )
  #
  #
  # }
  #
  # return(rows)

  out <- cbind(
    data.frame(parties = parties, years = years),
    variables,
    data.frame(number_value = res[['numberValue']], string_value = res[['stringValue']]))

  return(out)

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
select_varid <- function(
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


#' Search through UNFCCC Ids
#'
#' @param x object from `rem_init()`
#' @param id integer
#'
#' @return character, name corresponding to id
find_id <- function(x, id, verbose = FALSE){
  # ensure that match is only length one

  nms <- c()

  # recurse through list and keep track of nesting
  while(!inherits(x, 'integer')){

    nm <- names(x)[grepl(id, x)][1]
    nms <- c(nms, nm)
    if(nm == 'id'){
      y <- x
    }
    x <- x[[nm]]
  }

  # subset 'name' column using id
  idl <- x == id
  nm_id <- y[['name']][idl]

  if(verbose){
    cat(paste(nms, collapse = " - "), sep = '\n')
  }

  return(nm_id)

}



#' Title
#'
#' @param ids
#' @param rms list, lower-level item from `remis` object
#'
#' @return
#' @export
#'
#' @examples
get_names <- function(ids, rms){


  un_id <- unique(ids)
  for(idx in seq_along(un_id)){

    names(un_id)[idx] <- find_id(rms, un_id[idx])
  }

  nms <- character(length(ids))

  for(idx in seq_along(ids)){

    nms[idx] <- names(un_id)[ids[idx] == un_id]
  }

  return(nms)
}

#' Extract ccmgu information based on variableId
#'
#' @param ids integer, variableId
#' @param rms list, `remis` object
#'
#' @return data.frame containing integer variableId and text description of each
#' corresponding ccmug id
get_variables <- function(ids, rms){

  un_id <- unique(ids)
  categories <- vector('character', length(un_id))
  classification <- vector('character', length(un_id))
  measures <- vector('character', length(un_id))
  gas <- vector('character', length(un_id))
  unit <- vector('character', length(un_id))

  for(idx in seq_along(un_id)){

    aon <- names(rms$variables)[grepl(un_id[idx], rms$variables)]

    # get variable row
    row_lgl <- rms$variables[[aon]][['variableId']] == un_id[idx]
    row <- rms$variables[[aon]][row_lgl, ]


    categories[idx] <- find_id(rms$categories, row$categoryId)
    classification[idx] <- find_id(rms$classification, row$classificationId)
    measures[idx] <- find_id(rms$measures, row$measureId)
    gas[idx] <- find_id(rms$gas, row$gasId)
    unit[idx] <- find_id(rms$units$units, row$unitId)

  }


  lookup <-
    data.frame(
      variable_id = un_id,
      reporting = aon,
      category = categories,
      classification = classification,
      measure = measures,
      gas = gas,
      unit = unit
    )

  variables <- vector('list', length(ids))



  for(idx in seq_along(ids)){

    variables[idx] <- list(lookup[ids[idx] == lookup[['variable_id']], ])

  }

  variables <- do.call(rbind, variables)




  return(variables)

}






# TODO: -------------
## remis_check() ...-------------
# ensure that objects have suitable remis format

