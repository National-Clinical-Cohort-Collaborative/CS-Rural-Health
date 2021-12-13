# knitr::stitch_rmd(script="manipulation/te-ellis.R", output="stitched-output/manipulation/te-ellis.md") # dir.create("stitched-output/manipulation/", recursive=T)
rm(list = ls(all.names = TRUE))  # Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"       ) # For asserting conditions meet expected patterns/conditions.
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("odbc"        )
# requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config    <- config::get()

# OuhscMunge::readr_spec_aligned("concept-sets/input/dexamethasone.csv")
col_types <- readr::cols_only(
  # `Concept Name`        = readr::col_character(),
  `concept_id`              = readr::col_integer(),
  `keep_entry_in_codeset`   = readr::col_logical(),
  `comments`                = readr::col_character()
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

# OuhscMunge::readr_spec_aligned(config$path_concept_counts)
col_types_concept_count <- readr::cols_only(
  `concept_id`        = readr::col_integer(),
  `condition_count`   = readr::col_integer(),
  `patient_count`     = readr::col_integer()
)

sql_lu <- # 2.3M records
  "
    SELECT
      c.concept_id
      ,c.concept_name
      ,c.standard_concept
      ,c.invalid_reason
      ,c.concept_code
      ,c.domain_id
      ,c.vocabulary_id
      ,c.concept_class_id
    FROM  v6.concept c
    WHERE
      c.vocabulary_id in ('RxNorm', 'RxNorm Extension', 'ATC')
    ORDER BY c.concept_id
  "
paths <-
  c(
    # "concept-sets/input/nasal-spray.csv",
    "concept-sets/input/reviewed/inhaled-corticosteroid.csv",
    "concept-sets/input/reviewed/oral-dexamethasone.csv",
    "concept-sets/input/reviewed/oral-hydrocortisone.csv",
    "concept-sets/input/reviewed/systemic-hydrocortisone.csv",
    "concept-sets/input/reviewed/systemic-prednisolone.csv",
    "concept-sets/input/reviewed/systemic-prednisone-and-methyprednisolone.csv"
  )


# ---- load-data ---------------------------------------------------------------
cnn       <- DBI::dbConnect(odbc::odbc(), dsn = config$dsn_omop)
ds_lu     <- DBI::dbGetQuery(cnn, sql_lu)
DBI::dbDisconnect(cnn); rm(cnn, sql_lu)

ds_concept_count <- readr::read_csv(config$path_concept_counts, col_types = col_types_concept_count)
rm(col_types_concept_count)

# ---- loop-io -----------------------------------------------------------------
# Read each file, add the extra columns, then write back to the same file

for (p in paths) {
  message("Processesing ", p)
  # p <- paths[1]
  d <- readr::read_csv(p, col_types = col_types, lazy = FALSE)

  row_count   <- nrow(d)
  d <-
    d |>
    dplyr::left_join(ds_lu, by = "concept_id") |>
    dplyr::left_join(ds_concept_count, by = "concept_id") |>
    dplyr::select(
      concept_id,
      concept_name,
      keep_entry_in_codeset,
      comments,
      condition_count,
      patient_count,
      standard_concept,
      invalid_reason,
      concept_code,
      vocabulary_id,
      concept_class_id,
      domain_id
    )

  testit::assert("The row count should be constant.", row_count == nrow(d))
  # cat(paste0("    ", colnames(d), collapse=",\n"))

  checkmate::assert_integer(  d$concept_id               , any.missing=F , lower=1, upper=2^31     )
  checkmate::assert_logical(  d$keep_entry_in_codeset    , any.missing=F                           )
  checkmate::assert_character(d$comments                 , any.missing=T , pattern="^.{1,255}$"    )
  checkmate::assert_character(d$concept_name             , any.missing=F , pattern="^.{5,255}$"    )
  checkmate::assert_integer(  d$condition_count          , any.missing=T , lower=20, upper=9999999 )
  checkmate::assert_integer(  d$patient_count            , any.missing=T , lower=20, upper= 999999 )
  checkmate::assert_character(d$standard_concept         , any.missing=F , pattern="^C|S$"         )
  checkmate::assert_character(d$invalid_reason           , all.missing=T)
  checkmate::assert_character(d$concept_code             , any.missing=F , pattern="^.{4,15}$"     )
  checkmate::assert_character(d$domain_id                , any.missing=F , pattern="^Drug$"        )
  checkmate::assert_character(d$vocabulary_id            , any.missing=F , pattern="^ATC|RxNorm(?: Extension)?$"      )
  checkmate::assert_character(d$concept_class_id         , any.missing=F , pattern="^.{5,50}$"     )

  readr::write_csv(d, p, na = "")
}
