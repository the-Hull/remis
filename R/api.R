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
                          get_parse(rms = req, url = u)
                        } else if(class(u) == 'list'){

                          lapply(u, function(v){
                          cat(sprintf('parsing %s\n', v))
                          get_parse(rms = req, url = v)
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

  variable_ids <- unlist(sapply(responses$variables, `[[`, 'variableId'))
  responses$duplicate_variableIds <- variable_ids[duplicated(variable_ids)]

  return(responses)
}




#' Query UNFCCC DI with Flex Query API
#'
#' @param rms list, object from \link{`rem_init()`}
#' @param variable_ids integer, ids of interest
#' @param party_ids integer, ids of interest
#' @param year_ids integer, ids of interest
#' @param path character, default is 'api/records/flexible-queries/'
#' @param pretty logical, should raw results transformed to feature text
#' descriptions instead of ids?
#'
#' @return data.frame
#' @export
flex_query <- function(rms, variable_ids, party_ids, year_ids, path = 'api/records/flexible-queries/', pretty = TRUE){

  rem_check(rms)
  duplicate_check(rms, variable_id = variable_ids)




  # check if any are null.. allow for "variable_ids = 'all'" then download all?
  # warn for large dl and ask to proceed with input y/n
  # add `warn_large` argument as flag, set to FALSE for headless systems

  body <- list(
    variableIds = as.list(variable_ids),
    partyIds = as.list(party_ids),
    yearIds = as.list(year_ids)
    )



  result <- post_parse(rms = rms, path = path, body = body, pretty = pretty)

  if(is.list(result) & length(result) == 0){
    cat('No data submitted for any of the variables.')
    return(NULL)
  }


  return(result)

}



# helpers -----------------------------------------------------------------

#' GET and parse objects
#'
#' @param rms response from
#' @param url character, api path for desired object
#'
#' @return list, parsed from json
#'
get_parse <- function(rms, url){

  obj <- rms$get(url)
  obj_pretty <- jsonlite::fromJSON(obj$parse(encoding = 'UTF-8'))

  return(obj_pretty)

}





#' POST and parse objects
#'
#' @param rms list, remis object
#' @param path character
#' @param body list containing parties, years, variables
#' @param parse logical, should raw results transformed to feature text
#' descriptions instead of ids?
#'
#' @return data.frame, parsed from json
#'
post_parse <- function(rms, path = 'api/records/flexible-queries/', body, pretty = TRUE){

  # check if query is empty warn for large (!) download

  # replace all ids with existing ones?


  obj <- rms$.req$post(
    path = path,
    body = body,
    encode = 'json'
    )

  # check if response is 200 or fail
  if(obj$status_code != 200){
    stop(sprintf("POST query failed with status %d", obj$status_code))
  }

  obj <- jsonlite::fromJSON(obj$parse(encoding = 'UTF-8'))

    if(pretty){
    obj <- parse_raw(rms, obj)
  }

  return(obj)
}

#' Readable query results
#'
#' This function combines numeric/character responses from queries with their
#' respective id values from parties, years and variables.
#'
#' @param rms list, `remis` object
#' @param raw list, response from POST request
#'
#' @return data.frame
parse_raw <- function(rms, raw){

  # get parties
  parties <- get_names(rms$parties, raw$partyId)
  # get years
  years <- get_names(rms$years, raw$yearId)
  # get text variables
  variables <- get_variables(rms, raw$variableId)

  out <- cbind(
    data.frame(parties = parties, years = years),
    variables,
    data.frame(number_value = raw[['numberValue']], string_value = raw[['stringValue']]))

  return(out)
}


#' Variable selector
#'
#' Select variables based on categories, classifications, measures and gasses using
#' ids. Variable selection is based on the intersection of supplied ids (`union = FALSE`) or union
#' (only select variables where all supplied ccmug's are present).
#' If no ids are supplied, all variables are returned.
#'
#' @param vars data.frame of variable ids with corresponding cat/class/meas/gas ids
#' @param category_id integer
#' @param classification_id integer
#' @param measure_id integer
#' @param gas_id integer
#' @param unit_id integer
#' @param union logical, for `TRUE` only return variables where **all** supplied ccmug's are represented,
#'   for `FALSE` return every variable where any of the ccmug's is present.
#'
#' @export
#'
#' @return data.frame with same dims of `vars` if no ids are supplied, or with rows
#' resulting from union of ids, and columns of `vars`
select_varid <- function(
    vars,
    category_id = NULL,
    classification_id = NULL,
    measure_id = NULL,
    gas_id = NULL,
    unit_id = NULL,
    union = TRUE){


  nvars <- nrow(vars)


  # collect ids
  id_lists <- list(
    categoryId = category_id,
    classificationId = classification_id,
    measureId = measure_id,
    gasId = gas_id,
    unitId = unit_id)

  # if all entries are null, provide all vars to download everything
  # if any entry is non-null, limit to respective ids
  null_entries <- unlist(sapply(id_lists, is.null))

  get_all_ids <- all(null_entries)

  n_entries <- sum(!null_entries)

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

    if(!union){
      # find all rows with TRUE hits in mask
      vars_sub <- vars[rowSums(masks) > 0, ]
    } else if(union){
      vars_sub <- vars[rowSums(masks) == n_entries, ]
    }


    return(vars_sub)
  }

}


#' Search through UNFCCC Ids
#'
#' @param rms object from `rem_init()`
#' @param id integer
#'
#' @return character, name corresponding to id
find_id <- function(rms, id, verbose = FALSE){
  # ensure that match is only length one

  nms <- c()

  rms$duplicate_variableIds <- NULL

  # recurse through list and keep track of nesting
  while(!inherits(rms, 'integer')){

    if(any(grepl('units', names(rms)))){

      nm <- 'units'

    } else {
      # grepl doesn't find all items in lists - unlist and clean names
      nm <- names(unlist(rms))[grepl(id, unlist(rms))][1]
      # unlisted items have form "colname00" or "colname.00" or "colname.id00"
      # or "colnameid00"
      # clean with regex
      nm <- gsub("((?<=\\w)[.].+[0-9]+$)|([0-9]+$)", "", nm, perl = TRUE)
    }
    nms <- c(nms, nm)
    if(nm == 'id'){
      y <- rms
    }
    rms <- rms[[nm]]
  }

  # subset 'name' column using id
  idl <- rms == id
  nm_id <- y[['name']][idl]

  if(verbose){
    cat(paste(nms, collapse = " - "), sep = '\n')
  }

  return(nm_id)

}



#' Get name based on ids
#'
#' @param rms list, lower-level item from `remis` object, like `..$parties`
#' @param ids
#'
#' @return character, names corresponding to `ids`
#'
#' @examples
get_names <- function(rms, ids){


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
#' @param rms list, `remis` object
#' @param variable_id integer variableIds
#'
#' @export
#'
#' @return data.frame containing integer variableId and text description of each
#' corresponding ccmug id
get_variables <- function(rms, variable_id){

  rem_check(rms)

  expected_cols <- c(
    "variables" = "variableId",
    "categories" = "categoryId",
    "classification" = "classificationId",
    "measures" = "measureId",
    "gas" = "gasId",
    "units" =  "unitId")

  stopifnot("Please provide a data.frame or tbl with the following names:
             variableId, categoryId, classificationId, measureId, gasId, unitId" =
               all(colnames(variables) %in% expected_cols & ncol(variables)==length(expected_cols)))



  # create new and complete variable df based on variable ids
  var_un_id <- unique(variable_id)
  variables <- vector('list', length(var_un_id))

  for(vid in var_un_id){

    # extData variables are duplicated, i.e. listed in both annexOne and nonAnnexOne
    # Variable tables.
    # if aon = length 2, we assume it is extData and choose annexOne for time being
    aon <- names(rms[['variables']])[grepl(vid, rms[['variables']])]

    isextData <- FALSE
    if(length(aon) == 2){
      isextData <- TRUE
      aon <- aon[1]
    }
    vardf <- rms[['variables']][[aon]]
    # note that variable id's are not unique
    # thus vardf can have more than 1 row
    vardf <- vardf[vardf[['variableId']] == vid, ]

    if(isextData){
      aon <- 'extData'
    }

    vardf[['reporting']] <- aon
    variables[[vid]] <- vardf
  }
  variables <- do.call(rbind, variables)


  # lookup ccmugs and fill in variables
  for(idcat in expected_cols[-1]){

    un_id <- unique(variables[[idcat]])

    for(uid in un_id){

      id_pair <- setNames(
        object = uid,
        nm = find_id(
          rms[[names(grep(idcat, expected_cols, value = TRUE))]],
          uid
          )
        )

      # replace all values in variable column
      variables[[idcat]][variables[[idcat]] == id_pair]  <-  names(id_pair)

    }



  }


  variables <- merge(x = data.frame(variableId = variable_id),
        y = variables,
        by = 'variableId',
        all = TRUE)
    return(variables)

}



rem_check <- function(rms){

  stopifnot("Must supply a remis object." = {
    !inherits(rms, 'remis')
    any(names(rms)=='.req')
  })

}

duplicate_check <- function(rms, variable_id){

  found_duplicates <- variable_id %in% rms$duplicate_variable

  if(any(found_duplicates)){

    s <- paste(variable_id[found_duplicates], collapse = ", ")


    warning(
      sprintf("Found duplicated variableId ids: %s.\n Check results and possibly exclude unwanted observations.\n", s))

  }

}



# TODO: -------------

# clean_years <- function(years){

# Year: This field provides dropdown list for years from 1990 to the latest
# available year, as well as the base year. The base year, which is
#  applicable only to Annex I Parties, relates to the base year under the
# Climate Change Convention which is 1990 for most Parties.
# The other Parties that have a base year other than 1990 are:
# Bulgaria (1988), Hungary (average of 1985–1987), Poland (1988),
# Romania (1989) and Slovenia (1986).


# }

# check vars - use select_varid / text variables to
# filter final results!
