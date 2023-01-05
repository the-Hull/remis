find_id(rem$years, 52)
get_names(rem$years, 52)


rem$categories$extData
select_varid(rem, 188990)


res <- rem$.req$post(
  path = 'api/records/flexible-queries/',
  body = list(
    partyIds = list(3),
    yearIds = list(33),
    variableIds = list(188990)
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
  filter(classification == 'Total for category',
         gas == 'Aggregate GHGs'
  ) %>%
  glimpse() %>%
  pull(variable_id)

cat1A1a_agg_ghg <- flex_query(
  rms = rem,
  variable_ids = var_id,
  party_ids = 13,
  year_ids = rem$years$annexOne$id)


# CRF-Steckbrief Analyse --------------------------------------------------

# grab all level_6 GHG Emissions

## categories:

libarry(dplyr)
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
  gas_id = gas_emissions$id)


vars <- variables[1:212,]
nrow_vars <- nrow(vars)


batch_size <- 50
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
      variable_ids = vars$variableId[2],
      party_ids = 13, # germany,
      year_ids = 62 # last inventory year
      ))
}

overview <- flex_query()
# check why not working...

grepl(rms$extData$id[4], rms, fixed = TRUE)
