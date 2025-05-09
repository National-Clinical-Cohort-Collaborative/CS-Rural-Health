# knitr::stitch_rmd(script="manipulation/mlm-scribe.R", output="stitched-output/manipulation/mlm-scribe.md")
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
# source("manipulation/osdh/ellis/common-ellis.R")
# base::source(file="dal/osdh/arch/benchmark-client-program-arch.R") #Load retrieve_benchmark_client_program

# ---- load-packages -----------------------------------------------------------
# import::from("magrittr", "%>%")
requireNamespace("DBI")
requireNamespace("odbc")
requireNamespace("tibble")
requireNamespace("readr"                      )  # remotes::install_github("tidyverse/readr")
requireNamespace("dplyr"                      )
requireNamespace("checkmate"                  )
requireNamespace("testit"                     )
requireNamespace("config"                     )
requireNamespace("OuhscMunge"                 )  # remotes::install_github("OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()
path_in <- "./concept-sets/input/pre-reviewed/nasal-spray-pre-regex.csv"

# OuhscMunge::readr_spec_aligned(path_in)
col_types <- readr::cols_only(
  concept_id	               = readr::col_integer(),
  keep_entry_in_codeset      = readr::col_logical(),
  comments                   = readr::col_character(),
  concept_name	             = readr::col_character(),
  standard_concept           = readr::col_character(),
  standard_concept_caption   = readr::col_character(),
  invalid_reason             = readr::col_character(),
  invalid_reason_caption     = readr::col_character(),
  concept_code               = readr::col_integer(),
  domain_id	                 = readr::col_character(),
  vocabulary_id	             = readr::col_character(),
  concept_class_id           = readr::col_character(),
  concept_code	             = readr::col_character()
  # valid_start_date           = readr::col_date("%Y%m%d"),
  # valid_end_date	           = readr::col_date("%Y%m%d"),
)


# OuhscMunge::readr_spec_aligned(config$path_concept_counts)
col_types_concept_count <- readr::cols_only(
  `concept_id`        = readr::col_integer(),
  `condition_count`   = readr::col_integer(),
  `patient_count`     = readr::col_integer()
)

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
regex <- function (pattern, variable) {
  grepl(pattern, variable, ignore.case = TRUE)
}


# ---- load-data ---------------------------------------------------------------
# cnn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = path_db)
# cnn_warehouse <- DBI::dbConnect(odbc::odbc(), dsn = config$dsn_omop)
# ds            <- DBI::dbGetQuery(cnn_warehouse, sql)
# DBI::dbDisconnect(cnn_warehouse); rm(cnn_warehouse, sql)
ds <- readr::read_csv(path_in, col_types = col_types)

ds_concept_count <- readr::read_csv(config$path_concept_counts, col_types = col_types_concept_count, lazy = FALSE)

rm(col_types_concept_count)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds) # Help write `dplyr::select()` call.
ds <-
  ds |>
  dplyr::select(    # `dplyr::select()` drops columns not included.
    concept_id                           ,
    keep_entry_in_codeset                ,
    comments                             ,
    concept_name                         ,
    standard_concept                     ,
    standard_concept_caption             ,
    invalid_reason                       ,
    invalid_reason_caption               ,
    concept_code                         ,
    domain_id                            ,
    vocabulary_id                        ,
    concept_class_id                     ,
  ) |>
  tidyr::drop_na(concept_name) |>
  dplyr::mutate(
    keep_entry_in_codeset =
      dplyr::case_when(
        regex("\\bnasal\\b"            , concept_name) ~ "TRUE",
        regex("\\bflonase\\b"          , concept_name) ~ "TRUE",
        regex("\\bflovent\\b"          , concept_name) ~ "TRUE",
        regex("\\bfluticasone\\b"      , concept_name) ~ "TRUE",

        regex("\\btopical\\b"          , concept_name) ~ "FALSE",
        regex("\\binject"              , concept_name) ~ "FALSE",
        regex("\\binhal"               , concept_name) ~ "FALSE",
        regex("\\boral"                , concept_name) ~ "FALSE",
        regex("\\btablet\\b"           , concept_name) ~ "FALSE",
        regex("\\bcream\\b"            , concept_name) ~ "FALSE",
        regex("\\bophthalmic\\b"       , concept_name) ~ "FALSE",
        regex("\\beye\\b"              , concept_name) ~ "FALSE",
        regex("\\benema\\b"            , concept_name) ~ "FALSE",
        regex("\\botic\\b"             , concept_name) ~ "FALSE",
        regex("\\bsyringe\\b"          , concept_name) ~ "FALSE",
        regex("\\bimplant\\b"          , concept_name) ~ "FALSE",
        regex("\\bpaste\\b"            , concept_name) ~ "FALSE",
        regex("\\brectal\\b"           , concept_name) ~ "FALSE",
        regex(   "derm\\b"             , concept_name) ~ "FALSE",
        regex("\\bsinuva\\b"           , concept_name) ~ "FALSE",
        regex("\\bzytopic\\b"          , concept_name) ~ "FALSE",

        TRUE                                           ~ "--"
    )
  )
# Terms considered, but were not decisive:
# "actuat"

# dplyr::mutate(
#   concept_name  = iconv(concept_name,"WINDOWS-1252","UTF-8")
# )


# ---- join-with-counts --------------------------------------------------------
ds <-
  ds |>
  dplyr::left_join(ds_concept_count, by = "concept_id")


table(ds$keep_entry_in_codeset)

ds |>
  dplyr::group_by(keep_entry_in_codeset) |>
  dplyr::summarize(
    condition_count = sum(condition_count, na.rm = T),
    patient_count   = sum(patient_count  , na.rm = T),
  ) |>
  dplyr::ungroup()

# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_integer(  ds$concept_id               , any.missing=F , lower=config$omop_concept_min, upper=config$omop_concept_local , unique=T)
checkmate::assert_character(ds$keep_entry_in_codeset    , any.missing=F , pattern="^TRUE|FALSE|--$"   )
checkmate::assert_character(ds$comments                 , any.missing=T , pattern="^.{1,255}$"         )
checkmate::assert_integer(  ds$condition_count          , any.missing=T , lower=20, upper=9999999      )
checkmate::assert_integer(  ds$patient_count            , any.missing=T , lower=20, upper= 999999      )
checkmate::assert_character(ds$concept_name             , any.missing=F , pattern="^.{2,255}$"        , unique=T)
checkmate::assert_character(ds$standard_concept         , any.missing=F , pattern="^S$"                )
checkmate::assert_character(ds$invalid_reason           , any.missing=F , pattern="^NULL$"             )
checkmate::assert_integer(  ds$concept_code             , any.missing=F , lower=1, upper=40000000   , unique=T)
checkmate::assert_character(ds$domain_id                , any.missing=F , pattern="^Drug$"             )
checkmate::assert_character(ds$vocabulary_id            , any.missing=F , pattern="^RxNorm$"           )
checkmate::assert_character(ds$concept_class_id         , any.missing=F , pattern="^.{10,25}$"         )


# ---- specify-columns-to-upload -----------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.

ds_slim <-
  ds |>
  # dplyr::slice(1:100) |>
  dplyr::select(
    concept_id               ,
    concept_name             ,
    keep_entry_in_codeset    ,
    comments                 ,
    condition_count,
    patient_count,
    # standard_concept         , # Always "S"; add this column back if needed
    # invalid_reason           , # Always "NULL"; add this column back if needed
    concept_code             ,
    vocabulary_id            ,
    concept_class_id         ,
    domain_id                ,
  )

# ds_slim

# ---- save-to-db --------------------------------------------------------------
readr::write_csv(ds_slim, "concept-sets/input/pre-reviewed/nasal-spray.csv")
