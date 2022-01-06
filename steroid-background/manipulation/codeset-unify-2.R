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

sql_retrieve <-
  "
    SELECT
      ca.descendant_concept_id
      ,case
        when d.concept_name like '%drug implant%'   then 'systemic'
        when d.concept_name like '%inject%'         then 'systemic'
        when d.concept_name like '%oral%'           then 'systemic'
        when d.concept_name like '%pill%'           then 'systemic'
        when d.concept_name like '%syringe%'        then 'systemic'
        when d.concept_name like '%tablet%'         then 'systemic'

        when d.concept_name like '%inhal%'          then 'inhaled'

        when d.concept_name like '%nasal%'          then 'nasal'

        when d.concept_name like '%cream%'          then 'other'
        when d.concept_name like '%enema%'          then 'other'
        when d.concept_name like '%itch%'           then 'other'
        when d.concept_name like '%ointment%'       then 'other'
        when d.concept_name like '%ophthal%'        then 'other'
        when d.concept_name like '%otic%'           then 'other'
        when d.concept_name like '%pad%'            then 'other'
        when d.concept_name like '%paste%'          then 'other'
        when d.concept_name like '%rectal%'         then 'other'
        when d.concept_name like '%shampoo%'        then 'other'
        when d.concept_name like '%tape%'           then 'other'
        when d.concept_name like '%toothpaste%'     then 'other'
        when d.concept_name like '%topical%'        then 'other'
        when d.concept_name like '%vaginal%'        then 'other'
      end                        as guess
      ,d.concept_name            as descendent_name
      ,a.concept_name            as ancestor_name
      -- ,d.vocabulary_id
      ,ca.ancestor_concept_id
      -- ,ca.min_levels_of_separation
      -- ,ca.max_levels_of_separation
      -- ,a.concept_id   as ancestor_id
      -- ,d.concept_id   as descendent_id
    FROM v6.concept_ancestor ca
      inner join v6.concept a on ca.ancestor_concept_id = a.concept_id -- stands for ancestor
      inner join v6.concept d on ca.descendant_concept_id = d.concept_id -- stands for descendant
    WHERE
      -- d.standard_concept = 'S'
      -- and
      d.vocabulary_id != 'RxNorm Extension'  --d.vocabulary_id = 'RxNorm'
      and
      -- d.standard_concept = 'S'
      -- and
      a.concept_name in (          -- ancestor_concept_id
        'beclomethasone'           -- 1115572
        ,'budesonide'              -- 939259
        ,'ciclesonide'             -- 902938
        ,'cloprednol'              -- 19050907
        ,'cortisone'               -- 1507705
        ,'cortivazol'              -- 19061907
        ,'deflazacort'             -- 19086888
        ,'dexamethasone'           -- 1518254
        ,'fluticasone'             -- 1149380
        ,'hydrocortisone'          -- 975125
        ,'meprednisone'            -- 19009116
        ,'methylprednisolone'      -- 1506270
        ,'mometasone'              -- 905233
        ,'paramethasone'           -- 19027186
        ,'prednisolone'            -- 1550557
        ,'prednisone'              -- 1551099
        ,'prednylidene'            -- 19011127
        ,'triamcinolone'           -- 903963
      )
    ORDER BY
     a.concept_name
     ,d.concept_name

  "

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
