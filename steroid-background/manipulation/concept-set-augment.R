# knitr::stitch_rmd(script="manipulation/te-ellis.R", output="stitched-output/manipulation/te-ellis.md") # dir.create("stitched-output/manipulation/", recursive=T)
rm(list = ls(all.names = TRUE))  # Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr            , quietly=TRUE)

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
# requireNamespace("testit"       ) # For asserting conditions meet expected patterns/conditions.
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("sqldf"       ) # For interfacing w/ SQLite
# requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")
requireNamespace("geosphere")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config    <- config::get()

# OuhscMunge::readr_spec_aligned("concept-sets/input/dexamethasone.csv")
col_types <- readr::cols_only(
  # `Concept Name`        = readr::col_character(),
  `concept_id`          = readr::col_integer(),
  # `Valid Start Date`    = readr::col_date(format = ""),
  # `Invalid Reason`      = readr::col_logical(),
  # `Vocabulary Id`       = readr::col_character(),
  # `Concept Code`        = readr::col_character(),
  # `Concept Class Id`    = readr::col_character(),
  # `Standard Concept`    = readr::col_character(),
  # `Concept Name_1`      = readr::col_character(),
  # `Domain Id`           = readr::col_character(),
  # `Valid End Date`      = readr::col_date(format = "")
)


sql_create <- c(
  "
    DROP TABLE IF EXISTS codeset_member;
  ",
  "
    CREATE TABLE codeset_member (
      codeset              varchar(255)  not null,
      concept_id           int           not null,
      primary key(codeset, concept_id)
    )
  "
)

sql_retrieve <-
  "
    SELECT
      csm.codeset
      ,c.concept_id
      ,c.concept_name
      ,c.standard_concept
      ,'' as standard_concept_caption
      ,c.invalid_reason
      ,'' as invalid_reason_caption
      ,c.concept_code
      ,c.domain_id
      ,c.vocabulary_id
      ,c.concept_class_id
    FROM  concept c
      inner join codeset_member csm on c.concept_id = csm.concept_id
    LIMIT 5
  "

# ---- load-data ---------------------------------------------------------------
# Read the CSVs
paths <- fs::dir_ls(config$directory_codeset_input)[1:3]

ds_csm <-
  paths |>
  {\(path)
    purrr::map_dfr(
      .x = path,
      ~readr::read_csv(
        file      = path,
        col_types = col_types
      ),
      .id = "source"
    )
  }() |>
  tidyr::drop_na(concept_id) |>
  dplyr::mutate(
    codeset = fs::path_ext_remove(fs::path_file(source)),
  ) |>
  dplyr::select(
    codeset,
    concept_id,#  = `Concept Id`,
  ) |>
  dplyr::distinct()


# Open connection
cnn <- DBI::dbConnect(drv=RSQLite::SQLite(), dbname=config$path_database)

# Create tables
sql_create %>%
  purrr::walk(~DBI::dbExecute(cnn, .))
purrr::walk(sql_create, ~DBI::dbExecute(cnn, .))
# DBI::dbListTables(cnn)

# Write to database
DBI::dbWriteTable(cnn, name='codeset_member', value=ds_csm, append=TRUE, row.names=FALSE)

ds <-
  DBI::dbGetQuery(cnn, sql_retrieve)


# Close connection
DBI::dbDisconnect(cnn); rm(cnn, sql_retrieve)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(") #Spit out columns to help write call ato `dplyr::rename()`.

ds2 <-
  ds |>
  tidyr::pack(concept = tidyr::everything())

l2 <-
  list(
    items =
      data.frame(
        concept               = ds2,
        isExcluded          = rep(FALSE   , nrow(ds2)),
        includeDescendants  = rep(TRUE    , nrow(ds2)),
        includeMapped       = rep(FALSE   , nrow(ds2))
      )
  )
str(l2)

# l <-
#   list(
#     items = ds
#   )
#
# l
#
# test <-
#   list(
#     list(
#       concept = list(
#         CONCEPT_ID               = ds$concept_id,
#         CONCEPT_NAME             = ds$concept_name,
#         STANDARD_CONCEPT         = ds$standard_concept,
#         STANDARD_CONCEPT_CAPTION = ds$standard_concept_caption,
#         INVALID_REASON           = ds$invalid_reason,
#         INVALID_REAON_CAPTION    = ds$invalid_reason_caption
#       )
#     ),
#     list(isExcluded = 'false'),
#     list(includeDescendants = 'true'),
#     list(includeMapped = 'false')
#   )
#
# View(test)
li <- list()
for (i in seq_len(nrow(ds))) {
  li[i] <- list(
    concept = ds[i, ]
    # isExcluded = 'false',
    # includeDescendants = 'true',
    # includeMapped = 'false'
  )
}

l <- list(items = li)
l
as.list(ds[1, ])

jsonlite::fromJSON(
  txt = "concept-sets/input/desired.json"
) |>
  str()


# ---- verify-values -----------------------------------------------------------
# Sniff out problems
# OuhscMunge::verify_value_headstart(ds2)

# checkmate::assert_character(ds2$zip_code         , any.missing=F , pattern="^\\d{5}$" , unique=T)
# checkmate::assert_integer(  ds2$distance_min     , any.missing=T , lower=0, upper= 200 )
# checkmate::assert_integer(  ds2$count_within_20  , any.missing=T , lower=0, upper= 500 )
# checkmate::assert_integer(  ds2$count_within_60  , any.missing=T , lower=0, upper=1000 )
# checkmate::assert_integer(  ds2$count_within_100 , any.missing=T , lower=0, upper=2000 )

# ---- specify-columns-to-write ------------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.
# ds_slim <-
#   ds2 %>%
#   # dplyr::slice(1:100) %>%
#   dplyr::select(
#     zip_code,
#     distance_min,
#     count_within_20,
#     count_within_60,
#     count_within_100,
#   )
# # ds_slim

# ---- save-to-disk -------------------------------------------------
# readr::write_csv(ds_slim, config$path_derived_zip_code)
jsonlite::write_json(
  x       = l2,
  path    = config$directory_codeset_output_try1,
  pretty  = FALSE
)
