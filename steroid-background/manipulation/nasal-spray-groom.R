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
path_in <- "./concept-sets/input/pre-reviewed/nasal-spray.csv"

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
  concept_code               = readr::col_number(),
  domain_id	                 = readr::col_character(),
  vocabulary_id	             = readr::col_character(),
  concept_class_id           = readr::col_character(),
  concept_code	             = readr::col_character()
  # valid_start_date           = readr::col_date("%Y%m%d"),
  # valid_end_date	           = readr::col_date("%Y%m%d"),
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


# ---- load-data ---------------------------------------------------------------
# cnn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = path_db)
# cnn_warehouse <- DBI::dbConnect(odbc::odbc(), dsn = config$dsn_omop)
# ds            <- DBI::dbGetQuery(cnn_warehouse, sql)
# DBI::dbDisconnect(cnn_warehouse); rm(cnn_warehouse, sql)
ds <- readr::read_csv(path_in, col_types = col_types)


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
        grepl('nasal',         concept_name, ignore.case = T) ~ TRUE,
        grepl('topical',       concept_name, ignore.case = T) ~ FALSE,
        grepl('\\binject.+$',  concept_name, ignore.case = T) ~ FALSE,
        grepl('inhal.+',       concept_name, ignore.case = T) ~ FALSE,
        grepl('oral',          concept_name, ignore.case = T) ~ FALSE,
        grepl('\\btablet\\b',  concept_name, ignore.case = T) ~ FALSE,
        grepl('\\bcream\\b',   concept_name, ignore.case = T) ~ FALSE,
        grepl('ophthalmic',    concept_name, ignore.case = T) ~ FALSE,
        grepl('\\beye\\b',     concept_name, ignore.case = T) ~ FALSE,
        grepl('\\benema\\b',   concept_name, ignore.case = T) ~ FALSE,
        grepl('\\botic\\b',    concept_name, ignore.case = T) ~ FALSE,
        grepl('\\bsyringe\\b', concept_name, ignore.case = T) ~ FALSE,
        grepl('\\bimplant\\b', concept_name, ignore.case = T) ~ FALSE,
        grepl('\\bpaste\\b',   concept_name, ignore.case = T) ~ FALSE,
        grepl('\\brectal\\b',  concept_name, ignore.case = T) ~ FALSE,
        grepl('derm\\b',       concept_name, ignore.case = T) ~ FALSE,
        grepl('\\bsinuva\\b',  concept_name, ignore.case = T) ~ FALSE,
        TRUE                                                  ~ TRUE
    )
  )|>
  dplyr::filter(keep_entry_in_codeset)

# dplyr::mutate(
#   concept_name  = iconv(concept_name,"WINDOWS-1252","UTF-8")
# )



# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_integer(  ds$concept_id               , any.missing=F , lower=789931, upper=46287627 , unique=T)
checkmate::assert_logical(  ds$keep_entry_in_codeset    , any.missing=F                                )
# checkmate::assert_character(ds$comments                 , any.missing=T , pattern="^.{NA,NA}$"         )
checkmate::assert_character(ds$concept_name             , any.missing=F , pattern="^.{10,115}$"        , unique=T)
checkmate::assert_character(ds$standard_concept         , any.missing=F , pattern="^.{1,1}$"           )
checkmate::assert_character(ds$standard_concept_caption , any.missing=T , pattern="^.{NA,NA}$"         )
checkmate::assert_character(ds$invalid_reason           , any.missing=F , pattern="^.{4,4}$"           )
checkmate::assert_character(ds$invalid_reason_caption   , any.missing=T , pattern="^.{NA,NA}$"         )
checkmate::assert_numeric(  ds$concept_code             , any.missing=F , lower=1, upper=40000000   , unique=T)
checkmate::assert_character(ds$domain_id                , any.missing=F , pattern="^.{4,4}$"           )
checkmate::assert_character(ds$vocabulary_id            , any.missing=F , pattern="^.{6,6}$"           )
checkmate::assert_character(ds$concept_class_id         , any.missing=F , pattern="^.{10,18}$"         )


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
    comments                 ,
    standard_concept         ,
    standard_concept_caption ,
    invalid_reason           ,
    invalid_reason_caption   ,
    concept_code             ,
    domain_id                ,
    vocabulary_id            ,
    concept_class_id         ,
  )

# ds_slim

# ---- save-to-db --------------------------------------------------------------
write.csv(ds_slim, file = "./concept-sets/input/pre-reviewed/nasal-spray-keyword-cleaned.csv")
