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
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")
requireNamespace("geosphere")
# requireNamespace("tidycensus")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config    <- config::get()
max_pop   <- 200000 # The largest zip code in 2018 is 122814

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

col_types_city <- readr::cols_only( # OuhscMunge::readr_spec_aligned(config$path_raw_city)
  `city_ascii`      = readr::col_character(),
  `lat`             = readr::col_double(),
  `lng`             = readr::col_double()
)

# ---- load-data ---------------------------------------------------------------
# Read the CSVs
ds_zcta_latlong   <- readr::read_tsv(config$path_raw_zip_code_zcta  , col_types = col_types_zcta)
ds_city           <- readr::read_csv(config$path_raw_city           , col_types = col_types_city)

rm(col_types_zcta, col_types_city)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds_city) #Spit out columns to help write call ato `dplyr::rename()`.

ds_zcta_latlong <-
  ds_zcta_latlong |>
  # dplyr::slice(1:2000) |>
  dplyr::select(    # `dplyr::select()` drops columns not mentioned.
    zip_code    = GEOID,
    long        = INTPTLONG,
    lat         = INTPTLAT,
  )

ds_city <-
  ds_city |>
  # dplyr::slice(1:2000) |>
  dplyr::select(    # `dplyr::select()` drops columns not included.
    city                  = `city_ascii`,
    long                  = `lng`,
    lat                   = `lat`,
  )


ds <-
  "
    SELECT
      z.zip_code
      ,c.city    as c_city
      ,z.long    as z_long
      ,z.lat     as z_lat
      ,c.long    as c_long
      ,c.lat     as c_lat
    FROM ds_zcta_latlong z
      left  join ds_city c on
        z.lat  between c.lat  - 3 and c.lat  + 3
        and
        z.long between c.long - 4 and c.long + 4
  " |>
  sqldf::sqldf()

# +/-1  7,824,232
# +/-2 26,052,128

# ---- find-distance-to-city ---------------------------------------------------
ds2 <-
  ds |>
  # dplyr::slice(1:2000) |>
  dplyr::mutate(
    distance_from_zip_code_to_city_in_miles =
      geosphere::distVincentyEllipsoid(
        p1  = cbind(.data$z_long, .data$z_lat),
        p2  = cbind(.data$c_long, .data$c_lat)
      ) * config$miles_per_m
  ) |>
  dplyr::group_by(zip_code) |>
  dplyr::summarize(
    distance_min      = min(distance_from_zip_code_to_city_in_miles),
    count_within_20   = sum(distance_from_zip_code_to_city_in_miles <=  20),
    count_within_60   = sum(distance_from_zip_code_to_city_in_miles <=  60),
    count_within_100  = sum(distance_from_zip_code_to_city_in_miles <= 100),
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    zip_code_prefix_3  = substr(zip_code, 1, 3)
  )


# ---- verify-values -----------------------------------------------------------
# Sniff out problems
# OuhscMunge::verify_value_headstart(ds2)

checkmate::assert_character(ds2$zip_code         , any.missing=F , pattern="^\\d{5}$" , unique=T)
checkmate::assert_numeric(  ds2$distance_min     , any.missing=T , lower=0, upper=200 )
checkmate::assert_integer(  ds2$count_within_20  , any.missing=T , lower=0, upper=200 )
checkmate::assert_integer(  ds2$count_within_60  , any.missing=T , lower=0, upper=200 )
checkmate::assert_integer(  ds2$count_within_100 , any.missing=T , lower=0, upper=200 )

# ---- specify-columns-to-write ------------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.
ds_slim <-
  ds2 |>
  # dplyr::slice(1:100) |>
  dplyr::select(
    zip_code,
    distance_min,
    count_within_20,
    count_within_60,
    count_within_100,
  )
# ds_slim

# ---- save-to-disk -------------------------------------------------
readr::write_csv(ds_slim, config$path_derived_zip_code)
