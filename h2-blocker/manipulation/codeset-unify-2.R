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
    with ingredient as (
      SELECT
        concept_id
      FROM concept
      WHERE
        concept_id in (  -- ancestor concept name
          997276           -- 'cimetidine'
          ,953076          -- 'famotidine'
          ,950696          -- 'nizatidine'
          ,961047          -- 'ranitidine'
        )
    )
    ,downstream as (
      SELECT
        cr.concept_id_2                              as concept_id
        ,cr.concept_id_1                             as ingredient_concept_id
      FROM concept_relationship_mapped_from  cr
        left  join concept c1 on cr.concept_id_1 = c1.concept_id
        left  join concept c2 on cr.concept_id_2 = c2.concept_id
      WHERE
        -- cr.invalid_reason is null
        -- and
        -- cr.relationship_id = 'Mapped from'
        -- and
        c1.concept_class_id = 'Ingredient'
        and
        c2.vocabulary_id != 'RxNorm Extension'
        --c2.vocabulary_id = 'RxNorm'
        --and
        --c2.standard_concept = 'S'
        and
        c1.concept_id in (SELECT i.concept_id FROM ingredient i)
        --and
        --c2.concept_name like '%cimet%'
    )
    ,descendant as (
      SELECT
        ca.descendant_concept_id                      as concept_id
        ,ca.ancestor_concept_id                       as ingredient_concept_id
      FROM concept_ancestor ca
        inner join concept a on ca.ancestor_concept_id = a.concept_id -- stands for ancestor
        inner join concept d on ca.descendant_concept_id = d.concept_id -- stands for descendant
      WHERE
        d.vocabulary_id != 'RxNorm Extension'
        --d.vocabulary_id = 'RxNorm'
        --and
        --d.standard_concept = 'S'
        and
        ca.ancestor_concept_id in (SELECT i.concept_id FROM ingredient i)
    )
    ,stack as (
      SELECT concept_id, ingredient_concept_id FROM downstream
      UNION
      SELECT concept_id, ingredient_concept_id FROM descendant
    )
    ,collapsed as (
      SELECT
        s.concept_id
        --,string_agg(s.ingredient_concept_id, ';')      as ingredient_concept_ids -- about 100 duplicates
        --,string_agg(i.concept_name, ';')               as ingredient_names
        ,group_concat(s.ingredient_concept_id, ';')    as ingredient_concept_ids
        ,group_concat(i.concept_name, ';')             as ingredient_names
        ,count(distinct s.ingredient_concept_id)       as ingredient_count
      FROM stack s
        inner join concept i on s.ingredient_concept_id = i.concept_id
      GROUP BY s.concept_id
    )
    -- ,cte as (
    SELECT
      co.concept_id
      --,case
      --  when c.concept_name like '%drug implant%'   then 'systemic'
      --  when c.concept_name like '%inject%'         then 'systemic'
      --  when c.concept_name like '%oral%'           then 'systemic'
      --  when c.concept_name like '%inhal%'          then 'inhaled'
      --  when c.concept_name like '%nasal%'          then 'nasal'
      --  when c.concept_name like '%cream%'          then 'other'
      --  when c.concept_name like '%enema%'          then 'other'
      --end                        as guess
      ,co.ingredient_concept_ids
      ,co.ingredient_names
      ,co.ingredient_count
      ,c.concept_name
      ,c.vocabulary_id
      ,c.concept_class_id
      ,c.standard_concept
    FROM collapsed co
      inner join concept c on co.concept_id = c.concept_id
    ORDER BY ingredient_names, c.concept_name
    -- )

    -- SELECT
    --   concept_id
    --   ,concept_name
    -- FROM cte
    -- WHERE guess is null
    -- ORDER BY concept_name

  "


# OuhscMunge::readr_spec_aligned(config$path_concept_counts)
col_types_counts <- readr::cols_only(
  `concept_id`        = readr::col_integer(),
  `condition_count`   = readr::col_integer(),
  `patient_count`     = readr::col_integer()
)

# ---- load-data ---------------------------------------------------------------
# dplyr::filter(dplyr::count(ds, concept_id), 2L <= n)

# ds_classified     <- readr::read_csv(config$path_steroid_classification , col_types = col_types_classified)
# ds_concept_counts <- readr::read_csv(config$path_concept_counts         , col_types = col_types_counts)
# rm(col_types_classified, col_types_counts)

system.time({
# Pull info from OMOP's concept & concept_ancestor tables
cnn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = config$path_database)
ds_omop  <- DBI::dbGetQuery(cnn, sql_retrieve)
DBI::dbDisconnect(cnn); rm(cnn, sql_retrieve)
})
# 451.94 with concept name; 1.4 sec with concept id

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds) # Help write `dplyr::select()` call.

checkmate::assert_integer(ds_omop$concept_id, any.missing = FALSE, unique = TRUE)
#
# table(table(ds_omop$concept_id))
#
# ds_omop |>
#   dplyr::group_by(concept_id) |>
#   dplyr::mutate(
#     concept_count = dplyr::n()
#   ) |>
#   dplyr::ungroup() |>
#   dplyr::filter(2L <= concept_count) |>
#   dplyr::arrange(concept_id) |>
#   View()

ds_missing_from_omop <-
  ds_classified |>
  dplyr::filter(vocabulary_id != "RxNorm Extension") |>
  dplyr::anti_join(ds_omop, by = "concept_id")


stop()

ds <-
  ds_omop |>
  dplyr::left_join(ds_classified, by = "concept_id") |>
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
ds_omop |>
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
