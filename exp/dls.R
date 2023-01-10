rem <- rem_init()

find_id(rem$years, 52)
get_names(rem$years, 52)


rem$categories$extData
select_varid(rem$variables$annexOne, category_id = 10504)


res <- rem$.req$post(
  path = 'api/records/flexible-queries/',
  body = list(
    partyIds = list(3),
    yearIds = list(33),
    variableIds = list(188990)
  ),
  encode = 'json'
)
res <- rem$.req$post(
  path = 'api/records/flexible-queries/',
  body = list(
    partyIds = list(3),
    yearIds = list(33),
    variableIds = list(17344)
  ),
  encode = 'json'
)
res <- rem$.req$post(
  path = 'api/records/flexible-queries/',
  body = list(
    partyIds = list(3),
    yearIds = list(33),
    variableIds = list(78278)
  ),
  encode = 'json'
)


# check
cat1A1a <- rem$categories$annexOne[grepl("1.A.1.a.", rem$categories$annexOne$name), ]

# choose id
cat1A1a$id[1]

# get variables
cat1A1a_variables <- get_variables(
  rms = rem,
  select_varid(vars = rem$variables$annexOne, cat1A1a$id[1])$variableId
)

#choose id:
var_id <- cat1A1a_variables %>%
  filter(classificationId == 'Total for category',
         gasId == 'Aggregate GHGs'
  ) %>%
  glimpse() %>%
  pull(variableId)

cat1A1a_agg_ghg <- flex_query(
  rms = rem,
  variable_ids = var_id,
  party_ids = 13,
  year_ids = rem$years$annexOne$id)


# CRF-Steckbrief Analyse --------------------------------------------------

# grab all level_6 GHG Emissions

## categories:

library(dplyr)
# library(remis)
rem <- rem_init()

cat_level6 <- rem$categories$annexOne %>%
  # exploit nested structure
  filter(is.na(level_7) & !is.na(level_6))

class_emissions <-
  rem$classification$annexOne[grepl("Total for category", rem$classification$annexOne$name), ]

meas_emissions <-
  rem$measures$annexOne[grepl("Net emissions/removals$", rem$measures$annexOne$name), ]

gas_emissions <-
  rem$gas$annexOne[1:4, ]


variables <- select_varid(vars =
  rem$variables$annexOne,
  category_id = cat_level6$id,
  classification_id = class_emissions$id,
  measure_id = meas_emissions$id,
  gas_id = gas_emissions$id,
  union = TRUE)

variables_text <- get_variables(rem, variables)

vars <- variables[1:52,]
nrow_vars <- nrow(vars)


batch_size <- 10
start_idx <- seq(1, nrow_vars, by = batch_size)

n_batches <- length(start_idx)
batches <- vector('list', n_batches)

for(idx in seq_along(start_idx)){

  st <- start_idx[idx]
  en <- start_idx[idx] + batch_size - 1

  if(en > nrow_vars){
    en <- nrow_vars
  }

  cat(sprintf("batch: %d, from %d to %d\n", idx, st, en))

  batches[idx] <-
    list(flex_query(
      rem,
      variable_ids = vars$variableId[st:en],
      party_ids = 13, # germany,
      year_ids = 62 # last inventory year
      ))
  Sys.sleep(.5)
}

overview <- flex_query()
# check why not working...

grepl(rem$categories$extData$id[3], rem)





select_varid(vars = rem$variables$annexOne,
             category_id = 9560,
             classification_id = 10510,
             measure_id = 10460,
             gas_id = 10469,
             unit_id = 5) %>%
  # dplyr::glimpse() %>%
  get_variables(rem, .)


select_varid(vars = rem$variables$annexOne,
             category_id = 9560,
             classification_id = 10827,
             measure_id = 10460,
             gas_id = 10469,
             unit_id = 5) %>%
  dplyr::glimpse() %>%
  get_variables(rem, .)

10450
10510
10460
10471
5


select_varid(vars = rem$variables$annexOne,
             category_id = 10450,
             classification_id = 10510,
             measure_id = 10460,
             gas_id = 10471,
             unit_id = 5) %>%
  dplyr::glimpse() %>%
  get_variables(rem, .)
10450
10820
10460
10471
5

select_varid(vars = rem$variables$annexOne,
             category_id = 10450,
             classification_id = 10820,
             measure_id = 10460,
             gas_id = 10471,
             unit_id = 5) %>%
  dplyr::glimpse() %>%
  get_variables(rem, .)
