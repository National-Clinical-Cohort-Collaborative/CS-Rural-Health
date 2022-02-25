# knitr::stitch_rmd(script="manipulation/te-ellis.R", output="stitched-output/manipulation/te-ellis.md") # dir.create("stitched-output/manipulation/", recursive=T)
rm(list = ls(all.names = TRUE))  # Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
import::from("magrittr", "%>%")

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("sqldf"       ) # For interfacing w/ SQLite
# requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")
requireNamespace("geosphere")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config    <- config::get()

zip_paths <- fs::dir_ls(regexp = config$path_raw_zip_code_zcta_pattern, recurse = T, perl = T)

# figure_path <- 'stitched-output/manipulation/te/'

col_types_zcta <- readr::cols_only(
  GEOID           = readr::col_character(),
  # ALAND           = readr::col_double(),
  # AWATER          = readr::col_double(),
  # ALAND_SQMI      = readr::col_double(),
  # AWATER_SQMI     = readr::col_double(),
  INTPTLAT        = readr::col_double(),
  INTPTLONG       = readr::col_double()
)

col_types_hospital <- readr::cols_only( # OuhscMunge::readr_spec_aligned(config$path_raw_hospital)
  `OBJECTID`    = readr::col_integer(),
  `LATITUDE`    = readr::col_double(),
  `LONGITUDE`   = readr::col_double(),
  `TYPE`        = readr::col_character(),
  `STATUS`      = readr::col_character(),
  `BEDS`        = readr::col_integer()
)

# ---- load-data ---------------------------------------------------------------
# Read the CSVs
ds_zcta_census  <-
  readr::read_tsv(
    file        = zip_paths,
    col_types   = col_types_zcta,
    id          = "file_path"
  )
ds_hospital <- readr::read_csv(config$path_raw_hospital, col_types = col_types_hospital)

rm(col_types_zcta, col_types_hospital)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds_city) #Spit out columns to help write call ato `dplyr::rename()`.

ds_zcta_census <-
  ds_zcta_census %>%
  # dplyr::slice(1:200) %>%
  dplyr::select(    # `dplyr::select()` drops columns not mentioned.
    zip_code    = GEOID,
    long        = INTPTLONG,
    lat         = INTPTLAT,
    file_path
  ) %>%
  rematch2::bind_re_match(from = file_path, config$path_raw_zip_code_zcta_pattern) %>%
  dplyr::mutate(
    year  = as.integer(year),
    # zip_code    = sprintf("%05i", zip_code) # If the variable had been converted to an integer
  ) %>%
  tibble::as_tibble() %>%
  dplyr::group_by(zip_code) %>%
  dplyr::mutate(
    index_within_year   = dplyr::n() - dplyr::row_number(year),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(index_within_year == 0L) %>%
  dplyr::select(    # `dplyr::select()` drops columns not mentioned.
    zip_code,
    long,
    lat,
    year_last_existed = year,
  )

ds_hospital <-
  ds_hospital %>%
  # dplyr::slice(1:200) %>%
  dplyr::select(    # `dplyr::select()` drops columns not included.
    hospital_id   = OBJECTID,
    long          = LONGITUDE,
    lat           = LATITUDE,
    status        = STATUS,
    hospital_type = TYPE,
    bed_count     = BEDS,
  ) %>%
  dplyr::filter(status == "OPEN") %>%
  dplyr::filter(hospital_type %in% c("CRITICAL ACCESS", "GENERAL ACUTE CARE")) %>%
  dplyr::mutate(
    hospital_type  = factor(tolower(hospital_type)),
    bed_count      = dplyr::na_if(bed_count, -999L),
  ) %>%
  dplyr::select(
    hospital_id,
    long,
    lat,
    hospital_type,
    bed_count,
  )

# ggplot(ds_hospital, aes(x = bed_count, color = hospital_type)) +
#   geom_line(stat = "bin")
# table(ds_hospital$hospital_type, missing = is.na(ds_hospital$bed_count))
#                    missing
#                    FALSE TRUE
# critical access      989   21
# general acute care  4191   77

system.time({
ds_zcta_hospital <- # One row represents a [zip]-by-[hospital] combination
  "
    SELECT
      z.zip_code
      ,h.hospital_id
      ,h.hospital_type
      ,h.bed_count
      ,z.long    as z_long
      ,z.lat     as z_lat
      ,h.long    as h_long
      ,h.lat     as h_lat
    FROM ds_zcta_census z
      left  join ds_hospital h on
        z.lat  between h.lat  - 3 and h.lat  + 3
        and
        z.long between h.long - 4 and h.long + 4
  " %>%
  sqldf::sqldf() %>%
  tibble::as_tibble()
})

# ---- find-distance-to-city ---------------------------------------------------
message("Distance start time: ", Sys.time())
system.time({
ds_zcta_hospital_2 <- # One row represents a [zip]-by-[hospital] combination
  ds_zcta_hospital %>%
  # dplyr::slice(1:20000) %>%
  dplyr::mutate(
    distance_from_zip_code_to_hospital_in_miles =
      geosphere::distVincentyEllipsoid(
        p1  = cbind(.data$z_long, .data$z_lat),
        p2  = cbind(.data$h_long, .data$h_lat)
      ) * config$miles_per_m
  ) %>%
  dplyr::mutate(
    within_20mi  = (distance_from_zip_code_to_hospital_in_miles <=  20L),
    within_60mi  = (distance_from_zip_code_to_hospital_in_miles <=  60L),
    within_100mi = (distance_from_zip_code_to_hospital_in_miles <= 100L),
  )
}) #  1571.16 sec on i7 2th gen w/ 16GB

ds_zcta_hospital_type <- # One row per [zip] -by- [hospital type]
  ds_zcta_hospital_2 |>
  dplyr::group_by(zip_code, hospital_type) %>%
  dplyr::summarize(
    distance_min      = as.integer(round(min(distance_from_zip_code_to_hospital_in_miles))),
    hospital_count_within_20mi  = sum(within_20mi ),
    hospital_count_within_60mi  = sum(within_60mi ),
    hospital_count_within_100mi = sum(within_100mi),
  ) %>%
  dplyr::ungroup()

ds_zcta <-  # One row per [zip]
  ds_zcta_hospital_2 |>
  dplyr::group_by(zip_code) %>%
  dplyr::summarize(
    bed_count_within_20mi   = sum(bed_count * within_20mi , na.rm = TRUE),
    bed_count_within_60mi   = sum(bed_count * within_60mi , na.rm = TRUE),
    bed_count_within_100mi  = sum(bed_count * within_100mi, na.rm = TRUE),
  ) |>
  dplyr::ungroup()

# Pivot to one row per zip,a nd join with two other zip-grain datasets.
ds_wide <-
  ds_zcta_hospital_type %>%
  dplyr::mutate(
    hospital_type =
      dplyr::recode(
        hospital_type,
        "critical access"     = "critical",
        "general acute care"  = "acute"
      )
  ) %>%
  tidyr::pivot_wider(
    id_cols     = "zip_code",
    names_from  = "hospital_type",
    values_from = c(
      "distance_min",
      "hospital_count_within_20mi",
      "hospital_count_within_60mi",
      "hospital_count_within_100mi"
    )
  ) %>%
  dplyr::mutate(
    hospital_count_within_20mi_acute        = dplyr::coalesce(hospital_count_within_20mi_acute      , 0L),
    hospital_count_within_20mi_critical     = dplyr::coalesce(hospital_count_within_20mi_critical   , 0L),
    hospital_count_within_60mi_acute        = dplyr::coalesce(hospital_count_within_60mi_acute      , 0L),
    hospital_count_within_60mi_critical     = dplyr::coalesce(hospital_count_within_60mi_critical   , 0L),
    hospital_count_within_100mi_acute       = dplyr::coalesce(hospital_count_within_100mi_acute     , 0L),
    hospital_count_within_100mi_critical    = dplyr::coalesce(hospital_count_within_100mi_critical  , 0L),
  ) %>%
  dplyr::left_join( # join with the bed counts
    ds_zcta %>%
      dplyr::select(
        zip_code,
        bed_count_within_20mi,
        bed_count_within_60mi,
        bed_count_within_100mi,
      ),
    by = "zip_code"
  ) %>%
  dplyr::left_join( # join to get the last year of info for the zip
    ds_zcta_census %>%
      dplyr::select(
        zip_code,
        year_last_existed,
      ),
    by = "zip_code"
  )

# ---- verify-values -----------------------------------------------------------
# Sniff out problems
# OuhscMunge::verify_value_headstart(ds_wide)

checkmate::assert_character(ds_wide$zip_code                            , any.missing=F , pattern="^\\d{5}$"     , unique=T)
checkmate::assert_integer(  ds_wide$distance_min_acute                  , any.missing=T , lower=0, upper=500      )
checkmate::assert_integer(  ds_wide$distance_min_critical               , any.missing=T , lower=0, upper=500     )
checkmate::assert_integer(  ds_wide$hospital_count_within_20mi_acute    , any.missing=F , lower=0, upper=999     )
checkmate::assert_integer(  ds_wide$hospital_count_within_20mi_critical , any.missing=F , lower=0, upper=999     )
checkmate::assert_integer(  ds_wide$hospital_count_within_60mi_acute    , any.missing=F , lower=0, upper=999     )
checkmate::assert_integer(  ds_wide$hospital_count_within_60mi_critical , any.missing=F , lower=0, upper=999     )
checkmate::assert_integer(  ds_wide$hospital_count_within_100mi_acute   , any.missing=F , lower=0, upper=999     )
checkmate::assert_integer(  ds_wide$hospital_count_within_100mi_critical, any.missing=F , lower=0, upper=999     )
checkmate::assert_integer(  ds_wide$bed_count_within_20mi               , any.missing=F , lower=0, upper=99999   )
checkmate::assert_integer(  ds_wide$bed_count_within_60mi               , any.missing=F , lower=0, upper=99999   )
checkmate::assert_integer(  ds_wide$bed_count_within_100mi              , any.missing=F , lower=0, upper=99999   )
checkmate::assert_integer(  ds_wide$year_last_existed                   , any.missing=F , lower=2019, upper=2021 )

# combo <- paste(ds2$zip_code, ds2$hospital_type)
# checkmate::assert_character(combo, any.missing=F, unique=T) # The combination should be unique

# ---- specify-columns-to-write ------------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds_wide), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.
ds_slim <-
  ds_wide %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(
    zip_code,
    distance_min_acute,
    distance_min_critical,
    hospital_count_within_20mi_acute,
    hospital_count_within_20mi_critical,
    hospital_count_within_60mi_acute,
    hospital_count_within_60mi_critical,
    hospital_count_within_100mi_acute,
    hospital_count_within_100mi_critical,
    bed_count_within_20mi,
    bed_count_within_60mi,
    bed_count_within_100mi,
    year_last_existed,
  )

# ---- save-to-disk -------------------------------------------------
readr::write_csv(ds_slim, config$path_derived_zip_code_hospital)
