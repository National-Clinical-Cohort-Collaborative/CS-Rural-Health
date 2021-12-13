rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

# Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("rlang"        ) # Language constructs, like quosures
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("DBI"          ) # Database-agnostic interface
requireNamespace("RSQLite"      ) # Lightweight database for non-PHI data.
# requireNamespace("odbc"         ) # For communicating with SQL Server over a locally-configured DSN.  Uncomment if you use 'upload-to-db' chunk.
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()


paths <-
  c(
    "concept-sets/input/reviewed/nasal-spray.csv",
    "concept-sets/input/reviewed/inhaled-corticosteroid.csv",
    "concept-sets/input/reviewed/oral-dexamethasone.csv",
    "concept-sets/input/reviewed/oral-hydrocortisone.csv",
    "concept-sets/input/reviewed/systemic-hydrocortisone.csv",
    "concept-sets/input/reviewed/systemic-prednisolone.csv",
    "concept-sets/input/reviewed/systemic-prednosone-and-methyprednisolone.csv"
  ) |>
  rlang::set_names(
    {\(p)
      fs::path_ext_remove(fs::path_file(p))
    }
  )

read_reviewed <- function (path) {
  # OuhscMunge::readr_spec_aligned("concept-sets/input/dexamethasone.csv")
  col_types <- readr::cols_only(
    # `Concept Name`        = readr::col_character(),
    `concept_id`              = readr::col_integer(),
    `keep_entry_in_codeset`   = readr::col_logical()
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

  readr::read_csv(file = path, col_types = col_types, lazy = FALSE)
}

# ---- load-data ---------------------------------------------------------------
# dplyr::filter(dplyr::count(ds, concept_id), 2L <= n)
ds_lonely <-
  paths |>
  purrr::map_dfr(read_reviewed, .id = "file") |>
  # dplyr::filter(concept_id %in% c(792484L, 792486L, 792487L)) |> # all t
  # dplyr::filter(concept_id %in% c(739917L, 740264L, 789930L)) |> # all t
  # dplyr::filter(concept_id %in% c(792586L, 792587L)) |> # mixed t/f
  dplyr::group_by(concept_id) |>
  dplyr::filter(all(!keep_entry_in_codeset)) |>
  dplyr::ungroup()


  paths |>
  purrr::map_dfr(read_reviewed, .id = "file") |>
  tidyr::pivot_wider(
    id_cols = c(")
  )


  dplyr::group_by(concept_id) |>
  dplyr::filter(all(!keep_entry_in_codeset)) |>
  dplyr::ungroup()


    readr::read_csv(col_types = col_types, lazy = FALSE) |>
    tidyr::drop_na(concept_id) |>
    dplyr::mutate(
      is_excluded  = dplyr::coalesce(!keep_entry_in_codeset, TRUE), # Exclude if the value is missing
    ) |>
    dplyr::select(
      # codeset,
      concept_id,#  = `Concept Id`,
      is_excluded,
    ) |>
    dplyr::distinct()



  # purrr::map_dfr(jsonlite::read_json()


rm(col_types)

# Print the first few rows of each table, especially if you're stitching with knitr (see first line of this file).
#   If you print, make sure that the datasets don't contain any PHI.
#   A normal `data.frame` will print all rows.  But `readr::read_csv()` returns a `tibble::tibble`,
#   which prints only the first 10 rows by default.  It also lists the data type of each column.
ds

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds) # Help write `dplyr::select()` call.
ds <-
  ds %>%
  dplyr::select(    # `dplyr::select()` drops columns not included.
    subject_id,
    county_id,
    gender_id,
    race,
    ethnicity,
  ) %>%
  # dplyr::mutate(
  # )  %>%
  dplyr::arrange(subject_id) # %>%
  # tibble::rowid_to_column("subject_id") # Add a unique index if necessary

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
