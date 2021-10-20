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

sql_lu <-
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
      c.vocabulary_id = 'RxNorm'
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
    "concept-sets/input/reviewed/systemic-prednosone-and-methyprednisolone.csv"
  )


# ---- load-data ---------------------------------------------------------------
cnn       <- DBI::dbConnect(odbc::odbc(), dsn = config$dsn_omop)
ds_lu     <- DBI::dbGetQuery(cnn, sql_lu)
DBI::dbDisconnect(cnn); rm(cnn, sql_lu)

ds_concept_count <- readr::read_csv(config$path_concept_counts, col_types = col_types_concept_count)
rm(col_types_concept_count)


# ---- loop-io -----------------------------------------------------------------
# Read each file, add the extra columns, then write back to the same file

# for (p in paths[1]) {
#
# }

p <- paths[1]



d <- readr::read_csv(p, col_types = col_types)

row_count   <- nrow(d)
d <-
  d |>
  dplyr::left_join(ds_lu) |>
  dplyr::left_join(ds_concept_count) |>
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
    domain_id,
    vocabulary_id,
    concept_class_id,
  )

testit::assert("The row count should be constant.", row_count == nrow(d))
# cat(paste0("    ", colnames(d), collapse=",\n"))


checkmate::assert_integer(  d$concept_id               , any.missing=F , lower=1, upper=2^31     )
checkmate::assert_character(d$concept_name             , any.missing=F , pattern="^.{5,255}$"    )
checkmate::assert_character(d$standard_concept         , any.missing=F , pattern="^C|S$"         )
checkmate::assert_character(d$invalid_reason           , all.missing=T)
checkmate::assert_character(d$concept_code             , any.missing=F , pattern="^.{4,15}$"     )
checkmate::assert_character(d$domain_id                , any.missing=F , pattern="^Drug$"        )
checkmate::assert_character(d$vocabulary_id            , any.missing=F , pattern="^ATC|RxNorm(?: Extension)?$"      )
checkmate::assert_character(d$concept_class_id         , any.missing=F , pattern="^.{5,50}$"     )


stop()

# # ---- tweak-data --------------------------------------------------------------
# # OuhscMunge::column_rename_headstart(") #Spit out columns to help write call ato `dplyr::rename()`.
#
# ds <-
#   ds |>
#   tibble::as_tibble() |>
#   dplyr::mutate(
#     standard_concept_caption  = dplyr::recode(
#       standard_concept,
#       "S"   = "Standard",
#       "C"   = "Classification"
#       # Add more as needed
#     ),
#     invalid_reason_caption = dplyr::if_else(!is.na(invalid_reason), "Invalid", "Valid")
#   )
#
#
# ds_packed <-
#   ds |>
#   dplyr::rename_all(toupper) |>
#   tidyr::pack(concept = -CODESET)
#   # tidyr::pack(concept = tidyr::everything())
#
# ds_items <-
#   data.frame(
#     concept             = ds_packed,
#     isExcluded          = rep(FALSE   , nrow(ds_packed)),
#     includeDescendants  = rep(TRUE    , nrow(ds_packed)),
#     includeMapped       = rep(FALSE   , nrow(ds_packed))
#   )
#
# # l2 <- list(items = ds_items)
# # str(l2)
#
# #
# # test <-
# #   list(
# #     list(
# #       concept = list(
# #         CONCEPT_ID               = ds$concept_id,
# #         CONCEPT_NAME             = ds$concept_name,
# #         STANDARD_CONCEPT         = ds$standard_concept,
# #         STANDARD_CONCEPT_CAPTION = ds$standard_concept_caption,
# #         INVALID_REASON           = ds$invalid_reason,
# #         INVALID_REAON_CAPTION    = ds$invalid_reason_caption
# #       )
# #     ),
# #     list(isExcluded = 'false'),
# #     list(includeDescendants = 'true'),
# #     list(includeMapped = 'false')
# #   )
# #
# # View(test)
# # li <- list()
# # for (i in seq_len(nrow(ds))) {
# #   li[i] <- list(
# #     concept = ds[i, ]
# #     # isExcluded = 'false',
# #     # includeDescendants = 'true',
# #     # includeMapped = 'false'
# #   )
# # }
# #
# # l <- list(items = li)
# # l
# # as.list(ds[1, ])
#
# # jsonlite::fromJSON(
# #   "concept-sets/input/pre-reviewed/desired.json"
# # ) |>
# # str()
#
#
# # ---- verify-values -----------------------------------------------------------
# # Sniff out problems
# # OuhscMunge::verify_value_headstart(ds)
#
# checkmate::assert_character(ds$codeset                  , any.missing=F , pattern="^.{5,50}$"     )
# checkmate::assert_integer(  ds$concept_id               , any.missing=F , lower=1, upper=2^31     )
# checkmate::assert_character(ds$concept_name             , any.missing=F , pattern="^.{5,255}$"    )
# checkmate::assert_character(ds$standard_concept         , any.missing=F , pattern="^C|S$"         )
# checkmate::assert_character(ds$standard_concept_caption , any.missing=F , pattern="^Classification|Standard$"  )
# checkmate::assert_character(ds$invalid_reason           , all.missing=T)
# checkmate::assert_character(ds$invalid_reason_caption   , any.missing=F , pattern="^Valid$"       )
# checkmate::assert_character(ds$concept_code             , any.missing=F , pattern="^.{4,15}$"     )
# checkmate::assert_character(ds$domain_id                , any.missing=F , pattern="^Drug$"        )
# checkmate::assert_character(ds$vocabulary_id            , any.missing=F , pattern="^ATC|RxNorm(?: Extension)?$"      )
# checkmate::assert_character(ds$concept_class_id         , any.missing=F , pattern="^.{5,50}$"     )
#
# ds$concept_code[!grepl("^.{4,15}$", ds$concept_code)]
#
# # ---- specify-columns-to-write ------------------------------------------------
# # Print colnames that `dplyr::select()`  should contain below:
# #   cat(paste0("    ", colnames(ds), collapse=",\n"))
#
# # Define the subset of columns that will be needed in the analyses.
# #   The fewer columns that are exported, the fewer things that can break downstream.
# # ds_slim <-
# #   ds2 %>%
# #   # dplyr::slice(1:100) %>%
# #   dplyr::select(
# #     zip_code,
# #     distance_min,
# #     count_within_20,
# #     count_within_60,
# #     count_within_100,
# #   )
# # # ds_slim
#
# # ---- save-to-disk -------------------------------------------------
# # readr::write_csv(ds_slim, config$path_derived_zip_code)
# # jsonlite::write_json(
# #   x       = l2,
# #   path    = config$directory_codeset_output_try1,
# #   pretty  = TRUE
# # )
#
# # ds_items |>
# #   tibble::as_tibble()
#
# names(paths) |>
#   purrr::walk(
#     {
#       \(.codeset)
#       ds_items |>
#         dplyr::filter(concept.CODESET == .codeset) |>
#         dplyr::select(
#           # concept.codeset
#           concept           = concept.concept,
#           isExcluded,
#           includeDescendants,
#           includeMapped,
#         ) |>
#         {
#           \(.items)
#           list(items = .items) # Nest in the "items" element
#         }() |>
#         jsonlite::write_json(
#           path    = sprintf(config$directory_codeset_output_template, .codeset),
#           pretty  = TRUE
#         )
#     }
#   )
#
#
# # items |>
# #   purrr::keep(function(x) x$concept.codeset == "inhaled-corticosteroid")
#
