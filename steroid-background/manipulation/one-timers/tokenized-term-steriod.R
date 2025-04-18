rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path


# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tm"        )
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("rlang"        ) # Language constructs, like quosures
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()


# Execute to specify the column types.  It might require some manual adjustment (eg doubles to integers).
# OuhscMunge::readr_spec_aligned(config$path_steroid_classification)
col_types <- readr::cols_only(
  # `concept_id`                                  = readr::col_double(),
  # `steroid_class`                               = readr::col_character(),
  `concept_name`                                = readr::col_character()
  # `condition_count`                             = readr::col_double(),
  # `patient_count`                               = readr::col_double(),
  # `nasal_spray`                                 = readr::col_logical(),
  # `inhaled_corticosteroid`                      = readr::col_logical(),
  # `oral_dexamethasone`                          = readr::col_logical(),
  # `oral_hydrocortisone`                         = readr::col_logical(),
  # `systemic_hydrocortisone`                     = readr::col_logical(),
  # `systemic_prednisolone`                       = readr::col_logical(),
  # `systemic_prednisone_and_methyprednisolone`   = readr::col_logical(),
  # `membership_count`                            = readr::col_double(),
  # `standard_concept`                            = readr::col_character(),
  # `invalid_reason`                              = readr::col_character(),
  # `concept_code`                                = readr::col_character(),
  # `vocabulary_id`                               = readr::col_character(),
  # `concept_class_id`                            = readr::col_character()
)

# ---- load-data ---------------------------------------------------------------
# Read the CSVs
ds <- readr::read_csv(config$path_steroid_classification  , col_types=col_types)

rm(col_types)


# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds) # Help write `dplyr::select()` call.


# ---- corpus ----------------------------------------------------------------
ds$concept_name[grepl("Pulmicort", ds$concept_name)]

corpus <-
  ds$concept_name |>
  # ds$concept_name[grepl("Pulmicort", ds$concept_name)] |>
  tolower() |>
  {\(x)
    gsub("[\\[\\]\\(\\)]", " ", x, perl = T)
  }() |>
  tm::VectorSource() |>
  tm::VCorpus() |>
  tm::tm_map(tm::stripWhitespace) |>
  tm::tm_map(tm::removeWords, tm::stopwords("english"))

corpus |>
  tm::DocumentTermMatrix() |>
  tm::findFreqTerms(lowfreq = 10) |>
  paste(collapse = "\n") |>
  cat()


# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_integer(  ds$subject_id , any.missing=F , lower=1001, upper=1200 , unique=T)
checkmate::assert_integer(  ds$county_id  , any.missing=F , lower=1, upper=77     )
checkmate::assert_numeric(  ds$gender_id  , any.missing=F , lower=1, upper=255     )
checkmate::assert_character(ds$race       , any.missing=F , pattern="^.{5,41}$"    )
checkmate::assert_character(ds$ethnicity  , any.missing=F , pattern="^.{18,30}$"   )

# ---- specify-columns-to-upload -----------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.

ds_slim <-
  ds %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(
    subject_id,
    county_id,
    gender_id,
    race,
    ethnicity,
  )

ds_slim

# ---- save-to-disk ------------------------------------------------------------
# If there's no PHI, a rectangular CSV is usually adequate, and it's portable to other machines and software.
# readr::write_csv(ds_slim, path_out_unified)
# readr::write_rds(ds_slim, path_out_unified, compress="gz") # Save as a compressed R-binary file if it's large or has a lot of factors.


# ---- save-to-db --------------------------------------------------------------
# If a database already exists, this single function uploads to a SQL Server database.
# OuhscMunge::upload_sqls_odbc(
#   d             = ds_slim,
#   schema_name   = "skeleton",         # Or config$schema_name,
#   table_name    = "subject",
#   dsn_name      = "skeleton-example", # Or config$dsn_qqqqq,
#   timezone      = config$time_zone_local, # Uncomment if uploading non-UTC datetimes
#   clear_table   = T,
#   create_table  = F
# ) # 0.012 minutes


# If there's no PHI, a local database like SQLite fits a nice niche if
#   * the data is relational and
#   * later, only portions need to be queried/retrieved at a time (b/c everything won't need to be loaded into R's memory)
# cat(dput(colnames(ds)), sep = "\n")
sql_create <- c(
  "
    DROP TABLE if exists subject;
  ",
  "
    CREATE TABLE `subject` (
      subject_id      int   primary key,
      county_id       int   not null,
      gender_id       float not null,
      race            float not null,
      ethnicity       float not null
    );
  "
)

# Remove old DB
# if( file.exists(path_db) ) file.remove(path_db)

# Create directory if necessary.
if (fs::dir_exists(fs::path_dir(path_db)))
  fs::dir_create(fs::path_dir(path_db))

# Open connection
cnn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = path_db)
result <- DBI::dbSendQuery(cnn, "PRAGMA foreign_keys=ON;") #This needs to be activated each time a connection is made. #http://stackoverflow.com/questions/15301643/sqlite3-forgets-to-use-foreign-keys
DBI::dbClearResult(result)
DBI::dbListTables(cnn)

# Create tables
sql_create %>%
  purrr::walk(~DBI::dbExecute(cnn, .))
DBI::dbListTables(cnn)

# Write to database
DBI::dbWriteTable(cnn, name='subject',            value=ds_slim,        append=TRUE, row.names=FALSE)

# Close connection
DBI::dbDisconnect(cnn)
