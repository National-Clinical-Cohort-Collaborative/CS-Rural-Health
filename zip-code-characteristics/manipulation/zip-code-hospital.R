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

zip_paths <- fs::dir_ls(regexp = config$path_raw_zip_code_zcta_pattern, recurse = T, perl = T)

# figure_path <- 'stitched-output/manipulation/te/'

# col_types_zcta <- readr::cols_only(
#   ZIP       = readr::col_integer(),
#   LAT       = readr::col_double(),
#   LONG      = readr::col_double()
# )
col_types_zcta <- readr::cols_only(
  GEOID           = readr::col_character(),
  # ALAND           = readr::col_double(),
  # AWATER          = readr::col_double(),
  # ALAND_SQMI      = readr::col_double(),
  # AWATER_SQMI     = readr::col_double(),
  INTPTLAT        = readr::col_double(),
  INTPTLONG       = readr::col_double()
)

col_types_hospital <- readr::cols_only( # OuhscMunge::readr_spec_aligned(config$col_types_hospital)
  `OBJECTID`        = readr::col_integer(),
  `LATITUDE`        = readr::col_double(),
  `LONGITUDE`       = readr::col_double()
)

# ---- load-data ---------------------------------------------------------------
# Read the CSVs
ds_zcta_latlong   <-
  readr::read_tsv(
    file        = zip_paths,
    col_types   = col_types_zcta,
    id          = "file_path"
  )
ds_hospital       <- readr::read_csv(config$path_raw_hospital       , col_types = col_types_hospital)

rm(col_types_zcta, col_types_hospital)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds_city) #Spit out columns to help write call ato `dplyr::rename()`.

ds_zcta_latlong <-
  ds_zcta_latlong %>%
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
    hospital_id           = `OBJECTID`,
    long                  = `LONGITUDE`,
    lat                   = `LATITUDE`,
  )


system.time({
ds <-
  "
    SELECT
      z.zip_code
      ,h.hospital_id
      ,z.long    as z_long
      ,z.lat     as z_lat
      ,h.long    as h_long
      ,h.lat     as h_lat
    FROM ds_zcta_latlong z
      left  join ds_hospital h on
        z.lat  between h.lat  - 3 and h.lat  + 3
        and
        z.long between h.long - 4 and h.long + 4
  " %>%
  sqldf::sqldf()
})

# +/-1  7,824,232
# +/-2 26,052,128

# ---- find-distance-to-city ---------------------------------------------------
message("Distance start time: ", Sys.time())
system.time({
ds2 <-
  ds %>%
  # dplyr::slice(1:2000) %>%
  dplyr::mutate(
    distance_from_zip_code_to_hospital_in_miles =
      geosphere::distVincentyEllipsoid(
        p1  = cbind(.data$z_long, .data$z_lat),
        p2  = cbind(.data$h_long, .data$h_lat)
      ) * config$miles_per_m
  ) %>%
  dplyr::group_by(zip_code) %>%
  dplyr::summarize(
    distance_min      = as.integer(round(min(distance_from_zip_code_to_hospital_in_miles))),
    count_within_20   = sum(distance_from_zip_code_to_hospital_in_miles <=  20L),
    count_within_60   = sum(distance_from_zip_code_to_hospital_in_miles <=  60L),
    count_within_100  = sum(distance_from_zip_code_to_hospital_in_miles <= 100L),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    zip_code_prefix_3  = substr(zip_code, 1, 3)
  )
}) #  1904.72 sec on i7 2th gen w/ 16GB

# ---- verify-values -----------------------------------------------------------
# Sniff out problems
# OuhscMunge::verify_value_headstart(ds2)

checkmate::assert_character(ds2$zip_code         , any.missing=F , pattern="^\\d{5}$" , unique=T)
checkmate::assert_integer(  ds2$distance_min     , any.missing=T , lower=0, upper= 400 )
checkmate::assert_integer(  ds2$count_within_20  , any.missing=T , lower=0, upper= 500 )
checkmate::assert_integer(  ds2$count_within_60  , any.missing=T , lower=0, upper=1000 )
checkmate::assert_integer(  ds2$count_within_100 , any.missing=T , lower=0, upper=2000 )

# ---- specify-columns-to-write ------------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.
ds_slim <-
  ds2 %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(
    zip_code,
    distance_min,
    count_within_20,
    count_within_60,
    count_within_100,
  )
# ds_slim

# ---- save-to-disk -------------------------------------------------
readr::write_csv(ds_slim, config$path_derived_zip_code_hospital)
