rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

# Import only certain functions of a package into the search path.

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("rlang"        ) # Language constructs, like quosures
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
# requireNamespace("DBI"          ) # Database-agnostic interface
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
    "concept-sets/input/reviewed/systemic-prednisone-and-methyprednisolone.csv",

    # Plus one that hasn't been reviewed yet
    "concept-sets/input/pre-reviewed/wildcard-round-2.csv"
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

sql_retrieve <-
  "
    SELECT
      c.concept_id
      ,c.concept_name
      ,c.standard_concept
      -- ,'' as standard_concept_caption
      ,c.invalid_reason
      -- ,'' as invalid_reason_caption
      ,c.concept_code
      -- ,c.domain_id
      ,c.vocabulary_id
      ,c.concept_class_id
    FROM  concept c
      -- inner join codeset_member csm on c.concept_id = csm.concept_id
    WHERE c.concept_id = ?
    ORDER BY c.concept_id
    --LIMIT 5
  "
# OuhscMunge::readr_spec_aligned(config$path_concept_counts)
col_types_counts <- readr::cols_only(
  `concept_id`        = readr::col_integer(),
  `condition_count`   = readr::col_integer(),
  `patient_count`     = readr::col_integer()
)

# ---- load-data ---------------------------------------------------------------
# dplyr::filter(dplyr::count(ds, concept_id), 2L <= n)
ds_long <-
  paths |>
  purrr::map_dfr(read_reviewed, .id = "concept_set") |>
  # dplyr::filter(concept_id %in% c(792484L, 792486L, 792487L)) |>
  dplyr::mutate(
    concept_set = gsub("-", "_", concept_set),
  )

ds_concept_counts <- readr::read_csv(config$path_concept_counts, col_types = col_types_counts)

# Pull info from OMOP's concept table
cnn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = config$path_database)
ds_omop  <- DBI::dbGetQuery(cnn, sql_retrieve, params = list(unique(ds_long$concept_id)))
DBI::dbDisconnect(cnn); rm(cnn, sql_retrieve)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds) # Help write `dplyr::select()` call.
ds <-
  ds_long |>
  tidyr::pivot_wider(
    id_cols       = concept_id,
    names_from    = concept_set,
    values_from   = keep_entry_in_codeset
  ) |>
  {\(x)
    dplyr::mutate(
      x,
      membership_count = as.integer(rowSums(dplyr::select(x,
        nasal_spray,
        inhaled_corticosteroid,
        oral_dexamethasone,
        oral_hydrocortisone,
        systemic_hydrocortisone,
        systemic_prednisolone,
        systemic_prednisone_and_methyprednisolone
    ), na.rm = TRUE)))
  }() |>
  dplyr::left_join(ds_omop, by = "concept_id") |>
  dplyr::left_join(ds_concept_counts, by = "concept_id") |>
  dplyr::mutate(
    standard_concept  = dplyr::coalesce(standard_concept, "S"),
    steroid_class = dplyr::case_when(
      oral_dexamethasone                            ~ "systemic",
      oral_hydrocortisone                           ~ "systemic",
      systemic_hydrocortisone                       ~ "systemic",
      systemic_prednisolone                         ~ "systemic",
      systemic_prednisone_and_methyprednisolone     ~ "systemic",
      inhaled_corticosteroid                        ~ "inhaled",
      nasal_spray                                   ~ "nasal",
      TRUE                                          ~ "unclassified"
    ),
  ) |>
  dplyr::select(
    concept_id,
    steroid_class,
    concept_name,
    tidyselect::everything()
  ) |>
  dplyr::arrange(concept_id)


# ---- output-concepts-for-sql-where-clause ------------------------------------
ds |>
  # dplyr::filter(!is_excluded) |>
  dplyr::pull(concept_id) |>
  paste0(collapse = ", ", prefix = "") |>
  stringi::stri_wrap(
    initial = "WHERE concept_id in (\n  ",
    # exdent = ")\n",
    prefix = "  ",
    width = 201,
    cost_exponent = 2
  ) |>
  cat(
    ")\n",
    file = "data-public/derived/concepts-to-count.sql",
    sep = "\n"
  )




# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_integer(  ds$concept_id                                , any.missing=F , lower=1, upper=2^31 , unique=T)
checkmate::assert_character(ds$steroid_class                             , any.missing=F , pattern="^systemic|inhaled|nasal|unclassified$")
checkmate::assert_character(ds$concept_name                              , any.missing=F , pattern="^.{5,255}$"         )
checkmate::assert_logical(  ds$nasal_spray                               , any.missing=T                                )
checkmate::assert_logical(  ds$inhaled_corticosteroid                    , any.missing=T                                )
checkmate::assert_logical(  ds$oral_dexamethasone                        , any.missing=T                                )
checkmate::assert_logical(  ds$oral_hydrocortisone                       , any.missing=T                                )
checkmate::assert_logical(  ds$systemic_hydrocortisone                   , any.missing=T                                )
checkmate::assert_logical(  ds$systemic_prednisolone                     , any.missing=T                                )
checkmate::assert_logical(  ds$systemic_prednisone_and_methyprednisolone , any.missing=T                                )
checkmate::assert_integer(  ds$membership_count                          , any.missing=F , lower=0, upper=2             )
checkmate::assert_character(ds$standard_concept                          , any.missing=F , pattern="^C|S$"         )
checkmate::assert_character(ds$invalid_reason                            , all.missing=T)
checkmate::assert_character(ds$concept_code                              , any.missing=F , pattern="^.{4,11}$"          , unique=T)
checkmate::assert_character(ds$vocabulary_id                             , any.missing=F , pattern="^(?:ATC|RxNorm(?: Extension)?)$")
checkmate::assert_character(ds$concept_class_id                          , any.missing=F , pattern="^.{5,50}$"          )
checkmate::assert_integer(  ds$condition_count                           , any.missing=T , lower=20, upper=9999999      )
checkmate::assert_integer(  ds$patient_count                             , any.missing=T , lower=20, upper=999999       )

# View(ds[is.na(ds$standard_concept), ])
# ds$concept_code[!grepl("^.{4,11}$", ds$concept_code)]

# ---- specify-columns-to-upload -----------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.

ds_slim <-
  ds |>
  # dplyr::slice(1:100) |>
  dplyr::select(
    concept_id,
    steroid_class,
    concept_name,
    condition_count,
    patient_count,
    nasal_spray,
    inhaled_corticosteroid,
    oral_dexamethasone,
    oral_hydrocortisone,
    systemic_hydrocortisone,
    systemic_prednisolone,
    systemic_prednisone_and_methyprednisolone,
    membership_count,
    standard_concept,
    invalid_reason,
    concept_code,
    vocabulary_id,
    concept_class_id,
  )

ds_slim

# ---- save-to-disk ------------------------------------------------------------
# If there's no PHI, a rectangular CSV is usually adequate, and it's portable to other machines and software.
readr::write_csv(ds_slim, config$path_steroid_classification)
