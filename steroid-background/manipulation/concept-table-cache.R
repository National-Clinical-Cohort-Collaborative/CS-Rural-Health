# knitr::stitch_rmd(script="manipulation/mlm-scribe.R", output="stitched-output/manipulation/mlm-scribe.md")
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
# source("manipulation/osdh/ellis/common-ellis.R")
# base::source(file="dal/osdh/arch/benchmark-client-program-arch.R") #Load retrieve_benchmark_client_program

# ---- load-packages -----------------------------------------------------------
# import::from("magrittr", "%>%")
requireNamespace("DBI")
requireNamespace("tibble")
requireNamespace("readr"                      )  # remotes::install_github("tidyverse/readr")
requireNamespace("dplyr"                      )
requireNamespace("checkmate"                  )
requireNamespace("testit"                     )
requireNamespace("config"                     )
requireNamespace("OuhscMunge"                 )  # remotes::install_github("OuhscBbmc/OuhscMunge")

time_start <- Sys.time()
# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()
path_concept        <- "S:/BBMC/prairie-outpost/omop-1/downloads-athena/omop-v5/CONCEPT.csv"
path_ancestor       <- "S:/BBMC/prairie-outpost/omop-1/downloads-athena/omop-v5/CONCEPT_ANCESTOR.csv"
path_relationship   <- "S:/BBMC/prairie-outpost/omop-1/downloads-athena/omop-v5/CONCEPT_RELATIONSHIP.csv"

# OuhscMunge::readr_spec_aligned(path_concept)
col_types_concept <- readr::cols_only(
  concept_id	        = readr::col_integer(),
  concept_name	      = readr::col_character(),
  domain_id	          = readr::col_character(),
  vocabulary_id	      = readr::col_character(),
  concept_class_id    = readr::col_character(),
  standard_concept    = readr::col_character(),
  concept_code	      = readr::col_character(),
  valid_start_date    = readr::col_date("%Y%m%d"),
  valid_end_date	    = readr::col_date("%Y%m%d"),
  invalid_reason      = readr::col_character()
)

# OuhscMunge::readr_spec_aligned(path_ancestor)
col_types_ancestor <- readr::cols_only(
  ancestor_concept_id       = readr::col_integer(),
  descendant_concept_id     = readr::col_integer()
  # min_levels_of_separation  = readr::col_integer(),
  # max_levels_of_separation  = readr::col_integer()
)
sql_relationship <-
  "
    SELECT
      concept_id_1
      ,concept_id_2
      ,invalid_reason
    FROM cr
    WHERE
      relationship_id = 'Mapped from'
      and
      invalid_reason = ''
  "

# # OuhscMunge::readr_spec_aligned(path_relationship)
# col_types_relationship <- readr::cols_only(
#   concept_id_1         = readr::col_integer(),
#   concept_id_2         = readr::col_integer(),
#   relationship_id      = readr::col_character(),
#   # valid_start_date     = readr::col_date(),
#   # valid_end_date       = readr::col_date(),
#   invalid_reason       = readr::col_character(),
# )

# sql <-
#   "
#     SELECT
#       concept_id
#       ,concept_name      COLLATE SQL_Latin1_General_CP1_CI_AI as concept_name
#       ,domain_id
#       ,vocabulary_id
#       ,concept_class_id
#       ,standard_concept
#       ,concept_code
#       ,valid_start_date
#       ,valid_end_date
#       ,invalid_reason
#     FROM v6.concept
#     WHERE
#       domain_id in ('drug')
#     ORDER BY concept_id;
#   "


# ---- load-data ---------------------------------------------------------------
# cnn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = path_db)
# cnn_warehouse <- DBI::dbConnect(odbc::odbc(), dsn = config$dsn_omop)
# ds            <- DBI::dbGetQuery(cnn_warehouse, sql)
# DBI::dbDisconnect(cnn_warehouse); rm(cnn_warehouse, sql)
ds_concept      <- readr::read_tsv(path_concept, col_types = col_types_concept)
ds_ancestor     <- readr::read_tsv(path_ancestor, col_types = col_types_ancestor)
# system.time({
# ds_relationship <- readr::read_tsv(path_relationship, col_types = col_types_relationship, lazy = TRUE)
# })
# print(object.size(ds_relationship), units = "GB")



cr <- base::file(path_relationship) # Create an explicit connection.
system.time({
ds_relationship <-
  sqldf::read.csv.sql(
    sql         = sql_relationship,
    sep         = "\t",
    eol         = "\n",
    # na.strings  = "",
    # strip.white = TRUE,
    colClasses  = c("integer", "integer", "character", "character", "character", "character")
  )
# base::closeAllConnections() # I'd like to be more selective than 'All'.
})
base::close(cr)

rm(path_concept, path_ancestor, path_relationship)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds) # Help write `dplyr::select()` call.
ds_concept <-
  ds_concept |>
  dplyr::select(    # `dplyr::select()` drops columns not included.
    concept_id,
    concept_name,
    domain_id,
    vocabulary_id,
    concept_class_id,
    standard_concept,
    concept_code,
    valid_start_date,
    valid_end_date,
    invalid_reason,
  ) |>
  tidyr::drop_na(concept_name)

  # dplyr::mutate(
  #   concept_name  = iconv(concept_name,"WINDOWS-1252","UTF-8")
  # )

ds_ancestor <-
  ds_ancestor |>
  dplyr::filter(
    ancestor_concept_id   %in% ds_concept$concept_id,
    descendant_concept_id %in% ds_concept$concept_id
  )

ds_relationship <-
  ds_relationship |>
  dplyr::filter(
    concept_id_1   %in% ds_concept$concept_id,
    concept_id_2   %in% ds_concept$concept_id
  )

# ds_relationship <-
#   ds_relationship |>
#   dplyr::filter(relationship_id == 'Mapped from') |>
#   dplyr::filter(is.na(invalid_reason)) |>
#   dplyr::select(
#     -relationship_id,
#     -invalid_reason,
#   )

# print(object.size(ds_relationship), units = "GB")


# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_integer(  ds_concept$concept_id       , any.missing=F , lower=0, upper=2^31-1, unique=T)
checkmate::assert_character(ds_concept$concept_name     , any.missing=F )
checkmate::assert_character(ds_concept$domain_id        , any.missing=F , min.chars = 1 )
checkmate::assert_character(ds_concept$vocabulary_id    , any.missing=F , min.chars = 1 )
checkmate::assert_character(ds_concept$concept_class_id , any.missing=F , min.chars = 1 )
checkmate::assert_character(ds_concept$standard_concept , any.missing=T , pattern="^[CS]$"                                       )
checkmate::assert_character(ds_concept$concept_code     , any.missing=F , min.chars = 1 )
checkmate::assert_date(     ds_concept$valid_start_date , any.missing=F , lower=as.Date("1900-01-01"), upper=as.Date("2099-12-31") )
checkmate::assert_date(     ds_concept$valid_end_date   , any.missing=F , lower=as.Date("1900-01-01"), upper=as.Date("2099-12-31") )
checkmate::assert_character(ds_concept$invalid_reason   , any.missing=T , pattern="^[DU]$")

checkmate::assert_integer(  ds_ancestor$ancestor_concept_id     , any.missing=F , lower=0, upper=2^31-1, unique = FALSE)
checkmate::assert_integer(  ds_ancestor$descendant_concept_id   , any.missing=F , lower=0, upper=2^31-1, unique = FALSE)

combo <- paste(ds_ancestor$ancestor_concept_id, ds_ancestor$descendant_concept_id)
checkmate::assert_character(combo, any.missing = FALSE, unique = TRUE)
rm(combo)

# sum(!ds_ancestor$ancestor_concept_id   %in% ds_concept$concept_id)
# sum(!ds_ancestor$descendant_concept_id %in% ds_concept$concept_id)


# OuhscMunge::verify_value_headstart(ds_relationship)
checkmate::assert_integer(  ds_relationship$concept_id_1       , any.missing=F , lower=0, upper=2^31-1, unique=F)
checkmate::assert_integer(  ds_relationship$concept_id_2       , any.missing=F , lower=0, upper=2^31-1, unique=F)


# ---- specify-columns-to-upload -----------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.

ds_slim_concept <-
  ds_concept |>
  # dplyr::slice(1:100) |>
  dplyr::select(
    concept_id,
    concept_name,
    domain_id,
    vocabulary_id,
    concept_class_id,
    standard_concept,
    concept_code,
    valid_start_date,
    valid_end_date,
    invalid_reason
  )

ds_slim_ancestor <-
  ds_ancestor

ds_slim_relationship <-
  ds_relationship

rm(ds_concept, ds_ancestor, ds_relationship)
# ds_concept    <- ds_slim_concept
# ds_ancestor   <- ds_slim_ancestor
# ds_relationship <- ds_slim_relationship

# ---- save-to-db --------------------------------------------------------------
# If there's no PHI, a local database like SQLite fits a nice niche if
#   * the data is relational and
#   * later, only portions need to be queried/retrieved at a time (b/c everything won't need to be loaded into R's memory)
# cat(dput(colnames(ds)), sep = "\n")
sql_create <- c(
  "
    DROP TABLE IF EXISTS concept;
  ",
  "
    DROP TABLE IF EXISTS concept_ancestor;
  ",
  "
    DROP TABLE IF EXISTS concept_relationship_mapped_from;
  ",
  "
    CREATE TABLE concept (
      concept_id           int          primary key,
      concept_name         varchar(255) not null,
      domain_id            varchar(20)  not null,
      vocabulary_id        varchar(20)  not null,
      concept_class_id     varchar(20)  not null,
      standard_concept     varchar(1)       null,
      concept_code         varchar(50)  not null,
      valid_start_date     date         not null,
      valid_end_date       date         not null,
      invalid_reason       varchar(1)       null
    )
  ",
  "
    CREATE TABLE concept_ancestor (
      ancestor_concept_id    int   not null,
      descendant_concept_id  int   not null,
      primary key (ancestor_concept_id, descendant_concept_id),
      foreign key (ancestor_concept_id  ) references concept(concept_id),
      foreign key (descendant_concept_id) references concept(concept_id)
    )
  ",
  "
    CREATE TABLE concept_relationship_mapped_from (
      concept_id_1    int   not null,
      concept_id_2  int   not null,
      primary key (concept_id_1, concept_id_2),
      foreign key (concept_id_1) references concept(concept_id),
      foreign key (concept_id_1) references concept(concept_id)
    )
  "
)

# Remove old DB
if( file.exists(config$path_database) ) file.remove(config$path_database)

# Open connection
cnn <- DBI::dbConnect(drv=RSQLite::SQLite(), dbname=config$path_database)
result <- DBI::dbSendQuery(cnn, "PRAGMA foreign_keys=ON;") #This needs to be activated each time a connection is made. #http://stackoverflow.com/questions/15301643/sqlite3-forgets-to-use-foreign-keys
DBI::dbClearResult(result)
DBI::dbListTables(cnn)

# Create tables
sql_create |>
  purrr::walk(~DBI::dbExecute(cnn, .))

purrr::walk(sql_create, ~DBI::dbExecute(cnn, .))

DBI::dbListTables(cnn)

# Write to database
ds_slim_concept |>
  # dplyr::slice(1:100) |>
  {\(.x)
    dplyr::mutate_if(.x, ~inherits(.x, "Date"), as.character)
  }() |>
  {\(.d)
    DBI::dbWriteTable(cnn, name = "concept", value = .d, append = TRUE, row.names = FALSE)
  }()

ds_slim_ancestor |>
  # dplyr::slice(1:100) |>
  {\(.d)
    DBI::dbWriteTable(cnn, name = "concept_ancestor", value = .d, append = TRUE, row.names = FALSE)
  }()

ds_slim_relationship |>
  # dplyr::slice(1:100) |>
  {\(.d)
    DBI::dbWriteTable(cnn, name = "concept_relationship_mapped_from", value = .d, append = TRUE, row.names = FALSE)
  }()

# Close connection
DBI::dbDisconnect(cnn)

message(sprintf("Caching duration: %.2f min.", (difftime(Sys.time(), time_start, units = "min"))))
