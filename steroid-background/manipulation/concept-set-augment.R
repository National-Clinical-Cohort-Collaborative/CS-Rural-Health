# knitr::stitch_rmd(script="manipulation/te-ellis.R", output="stitched-output/manipulation/te-ellis.md") # dir.create("stitched-output/manipulation/", recursive=T)
rm(list = ls(all.names = TRUE))  # Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
# library(magrittr            , quietly=TRUE)

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

sql_retrieve <-
  "
    SELECT
      c.concept_id
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
      -- inner join codeset_member csm on c.concept_id = csm.concept_id
    WHERE c.concept_id = ?
    ORDER BY c.concept_id
    --LIMIT 5
  "

paths <-
  c(
    "concept-sets/input/reviewed/nasal-spray.csv",
    "concept-sets/input/reviewed/inhaled-corticosteroid.csv",
    "concept-sets/input/reviewed/oral-dexamethasone.csv",
    "concept-sets/input/reviewed/oral-hydrocortisone.csv",
    "concept-sets/input/reviewed/systemic-hydrocortisone.csv",
    "concept-sets/input/reviewed/systemic-prednisolone.csv",
    "concept-sets/input/reviewed/systemic-prednosone-and-methyprednisolone.csv"
  )

# ---- load-data ---------------------------------------------------------------


# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(") #Spit out columns to help write call ato `dplyr::rename()`.


# ---- loop-io -----------------------------------------------------------------

for (p in paths) { # p <- paths[1]
  message("Processesing ", p)

  # Read from CSV
  ds_input <-
    p |>
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

  row_count <- nrow(ds_input)


  # Pull info from OMOP's concept table
  cnn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = config$path_database)
  ds_omop  <- DBI::dbGetQuery(cnn, sql_retrieve, params = list(ds_input$concept_id))
  DBI::dbDisconnect(cnn); rm(cnn)

  # Merge input with concept table
  ds <-
    ds_input |>
    dplyr::left_join(ds_omop, by = "concept_id") |>
    dplyr::mutate(
      standard_concept_caption  = dplyr::recode(
        standard_concept,
        "S"   = "Standard",
        "C"   = "Classification"
        # Add more as needed
      ),
      invalid_reason_caption = dplyr::if_else(!is.na(invalid_reason), "Invalid", "Valid")
    ) |>
    dplyr::arrange(concept_id)

  # Pack to produce JSON that's compliant with Enclave's version of OMOP.
  ds_packed <-
    ds |>
    dplyr::rename_all(toupper) |>
    tidyr::pack(concept = -c("IS_EXCLUDED")) |>
    dplyr::mutate(
      IS_EXCLUDED = as.logical(IS_EXCLUDED)
    )

  ds_items <-
    data.frame(
      concept             = ds_packed,
      includeDescendants  = rep(FALSE   , nrow(ds_packed)),
      includeMapped       = rep(FALSE   , nrow(ds_packed))
    ) |>
    dplyr::rename(
      `isExcluded` = `concept.IS_EXCLUDED`
    )

  # l2 <- list(items = ds_items)
  # str(l2)

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
  #     list(includeDescendants = 'true'),
  #     list(includeMapped = 'false')
  #   )
  #
  # View(test)
  # li <- list()
  # for (i in seq_len(nrow(ds))) {
  #   li[i] <- list(
  #     concept = ds[i, ]
  #     # includeDescendants = 'true',
  #     # includeMapped = 'false'
  #   )
  # }
  #
  # l <- list(items = li)
  # l
  # as.list(ds[1, ])

  # jsonlite::fromJSON(
  #   "concept-sets/input/pre-reviewed/desired.json"
  # ) |>
  # str()

  # ---- verify-values -----------------------------------------------------------
  # Sniff out problems
  # OuhscMunge::verify_value_headstart(ds)

  # checkmate::assert_character(ds$codeset                  , any.missing=F , pattern="^.{5,50}$"     )
  checkmate::assert_integer(  ds$concept_id               , any.missing=F , lower=1, upper=2^31     )
  checkmate::assert_character(ds$concept_name             , any.missing=F , pattern="^.{5,255}$"    )
  checkmate::assert_character(ds$standard_concept         , any.missing=F , pattern="^C|S$"         )
  checkmate::assert_character(ds$standard_concept_caption , any.missing=F , pattern="^Classification|Standard$"  )
  checkmate::assert_character(ds$invalid_reason           , all.missing=T)
  checkmate::assert_character(ds$invalid_reason_caption   , any.missing=F , pattern="^Valid$"       )
  checkmate::assert_character(ds$concept_code             , any.missing=F , pattern="^.{4,15}$"     )
  checkmate::assert_character(ds$domain_id                , any.missing=F , pattern="^Drug$"        )
  checkmate::assert_character(ds$vocabulary_id            , any.missing=F , pattern="^ATC|RxNorm(?: Extension)?$"      )
  checkmate::assert_character(ds$concept_class_id         , any.missing=F , pattern="^.{5,50}$"     )

  ds$concept_code[!grepl("^.{4,15}$", ds$concept_code)]

  # ---- specify-columns-to-write ------------------------------------------------
  # Print colnames that `dplyr::select()`  should contain below:
  #   cat(paste0("    ", colnames(ds), collapse=",\n"))

  ds_items <-
    ds_items |>
    dplyr::select(
      # concept.codeset
      concept           = concept.concept,
      isExcluded,
      includeDescendants,
      includeMapped,
    )

  # ---- save-to-disk -------------------------------------------------
  # readr::write_csv(ds_slim, config$path_derived_zip_code)

  file_name_base <- fs::path_ext_remove(fs::path_file(p))

  # Write concept set as OMOP-compliant json.
  jsonlite::write_json(
    x       = list(items = ds_items),
    path    = sprintf(config$directory_codeset_output_template, file_name_base),
    pretty  = TRUE
  )

  # Write as a WHERE clause of an Enclave SQL transform
  # sprintf(
  #   "WHERE concept_id in (\n  %s\n)\n",
  #   stringi::stri_wrap(paste(ds$concept_id, collapse = ", "), 40, 0)
  # ) |>
  # cat(
  # "WHERE concept_id in (\n",
  #   stringi::stri_wrap(paste(ds$concept_id, collapse = ", ", prefix = "++"), 80, 0),
  #    sep='\n',
  #   file = sprintf("concept-sets/%s.sql", file_name_base)
  # )

  ds$concept_id |>
    paste0(collapse = ", ", prefix = "") |>
    stringi::stri_wrap(
      initial = "WHERE concept_id in (\n  ",
      # exdent = ")\n",
      prefix = "  ",
      width = 121,
      cost_exponent = 2
    ) |>
    cat(
      ")\n",
      file = sprintf("concept-sets/%s.sql", file_name_base),
      sep = "\n"
    )


} # End loop of input file
