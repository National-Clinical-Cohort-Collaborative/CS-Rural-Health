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
# requireNamespace("rlang"        ) # Language constructs, like quosures
# requireNamespace("testit"       ) # For asserting conditions meet expected patterns/conditions.
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
# requireNamespace("odbc"         ) # For communicating with SQL Server over a locally-configured DSN.  Uncomment if you use 'upload-to-db' chunk.
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")
requireNamespace("geosphere")
requireNamespace("tidycensus")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config    <- config::get()
max_pop   <- 200000 # The largest zipcode in 2018 is 122814

# tidycensus::census_api_key(config$api_key_census, install = TRUE)
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

# col_types_ruca <- readr::cols_only( # OuhscMunge::readr_spec_aligned(config$path_zipcode_ruca)
#   ZIPCODEN      = readr::col_integer(),
#   # ZIPCODEA      = readr::col_integer(),
#   ZIPTYPE       = readr::col_integer(),
#   # RESZIPN       = readr::col_integer(),
#   # RESZIPA       = readr::col_integer(),
#   RUCA30        = readr::col_double()
#   # `State-County FIPS Code`                        = readr::col_character(),
#   # `Select State`                                  = readr::col_character(),
#   # `Select County`                                 = readr::col_character(),
#   # `State-County-Tract FIPS Code`                  = readr::col_character(),
#   # `Primary RUCA Code 2010`                        = readr::col_double(),
#   # `Secondary RUCA Code, 2010 (see errata)`        = readr::col_double(),
#   # `Tract Population, 2010`                        = readr::col_number(),
#   # `Land Area (square miles), 2010`                = readr::col_double(),
#   # `Population Density (per square mile), 2010`    = readr::col_character()
# )
#
# col_types_census_variable <- readr::cols_only( # OuhscMunge::readr_spec_aligned(config$path_census_variable)
#   `variable_code`             = readr::col_character(),
#   `desired`                   = readr::col_logical(),
#   `variable_name`             = readr::col_character()
#   # `variable_name_original`  = readr::col_character()
#   # `concept`                 = readr::col_character()
# )

# Peek at the available ACS Census variables
# v18 <- tidycensus::load_variables(2018, "acs5", cache = TRUE)
# View(v18); readr::write_csv(v18, "census_variable_temp.csv")
# population_total  <- "B01003_001"
# ds_variable <-
#   tibble::tibble(
#     ~variable_code, ~variable_name,
#     "B01003_001"  , "population_total",
#   )

# ---- load-data ---------------------------------------------------------------
# Read the CSVs
ds_zcta_latlong  <- readr::read_tsv(config$path_zipcode_zcta  , col_types=col_types_zcta)
# ds_ruca          <- readr::read_csv(config$path_zipcode_ruca  , col_types=col_types_ruca)
# ds_variable      <- readr::read_csv(config$path_census_variable  , col_types=col_types_census_variable)
#
# ds_zcta_variable <- tidycensus::get_acs(
#   geography   = "zcta",
#   variables   = ds_variable$variable_code[ds_variable$desired],
#   year        = 2018,
#   geometry    = FALSE,
#   cache_table = TRUE,
#   show_call   = FALSE
#   # summary_var = ds_variable$variable_name[ds_variable$desired],
#   # variables = population_total,
#   # state     = "OK",
# )

# Getting data from the 2014-2018 5-year ACS
# Basic population call ("B01003_001"): https://api.census.gov/data/2018/acs/acs5?get=B01003_001E%2CB01003_001M%2CNAME&for=zip%20code%20tabulation%20area%3A%2A
# Census API call: https://api.census.gov/data/2018/acs/acs5?get=B01003_001E%2CB01003_001M%2CB01001_002E%2CB01001_002M%2CNAME&for=zip%20code%20tabulation%20area%3A%2A

rm(col_types_zcta) #, col_types_ruca)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds_zcta_census) #Spit out columns to help write call ato `dplyr::rename()`.

# ds_zcta_variable2 <-
#   ds_zcta_variable %>%
#   dplyr::select(
#     zipcode       = GEOID,
#     estimate,
#     variable_code = variable,
#     # population    = `estimate`,
#     # name        = `NAME`,
#     # moe         = `moe`
#   )

ds_zcta_latlong <-
  ds_zcta_latlong %>%
  dplyr::select(    # `dplyr::select()` drops columns not mentioned.
    zip_code    = GEOID,
    lat         = INTPTLAT,
    long        = INTPTLONG,
  )

# ds_ruca <-
#   ds_ruca %>%
#   dplyr::select(    # `dplyr::select()` drops columns not mentioned.
#     zipcode     = ZIPCODEN,
#     zipcode_type= ZIPTYPE,
#     ruca        = RUCA30,
#   ) %>%
#   dplyr::mutate(
#     zipcode      = sprintf("%05i", zipcode),
#     zipcode_type = dplyr::recode(zipcode_type, `1` = "residential", `2` = "point"),
#     ruca_cut4    = as.character(cut(
#       ruca,
#       breaks = c(1, 4, 7, 10, Inf),
#       labels = c("metropolitan", "micropolitan", "small town", "rural"),
#       right  = FALSE
#     ))
#   ) #%>%
#   # dplyr::filter(type==1)


# # ---- census-widen ------------------------------------------------------------
# ds_zcta_census <-
#   ds_zcta_variable2 %>%
#   dplyr::mutate(
#     estimate  = as.integer(estimate)
#   ) %>%
#   dplyr::left_join(ds_variable, by = "variable_code") %>%
#   dplyr::select(zipcode, variable_name, estimate) %>%
#   tidyr::pivot_wider(
#     names_from  = "variable_name",
#     values_from = "estimate"
#   )


# ---- join --------------------------------------------------------------------
# ds <-
#   ds_zcta_census %>%
#   dplyr::full_join(ds_zcta_latlong, by = "zipcode") %>%
#   dplyr::full_join(ds_ruca, by = "zipcode") %>%
#   dplyr::mutate(
#     zipcode_prefix_3  = substr(zipcode, 1, 3)
#   )
ds <-
  ds_zcta_latlong %>%
  dplyr::mutate(
    zip_code_prefix_3  = substr(zip_code, 1, 3)
  )

# ---- find-distance-to-oucp ---------------------------------------------------
ds <-
  ds %>%
  dplyr::mutate(
    distance_from_zipcode_to_oucp_in_miles =
      geosphere::distVincentyEllipsoid(
        p1  = c(config$location_oucp_long, config$location_oucp_lat),
        p2  = cbind(.data$long, .data$lat)
      ) * config$miles_per_m
  )


# ---- verify-values -----------------------------------------------------------
# Sniff out problems
# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_character(ds$zipcode , any.missing=F , pattern="^\\d{5}$"    , unique=T)
checkmate::assert_character(ds$zipcode_prefix_3        , any.missing=F , pattern="^\\d{3}$"    )
checkmate::assert_numeric(  ds$lat     , any.missing=T , lower=-15, upper=72   )
checkmate::assert_numeric(  ds$long    , any.missing=T , lower=-177, upper=146 )
checkmate::assert_character(ds$zipcode_type, any.missing=T , pattern = "^(residential|point)$"     )
checkmate::assert_numeric(  ds$ruca    , any.missing=T , lower=1, upper=11     )
checkmate::assert_character(ds$ruca_cut4    , any.missing=T , pattern="^(metropolitan|micropolitan|small town|rural)$"   )
checkmate::assert_numeric(  ds$distance_from_zipcode_to_oucp_in_miles , any.missing=T , lower=0, upper=8000   )


checkmate::assert_integer(  ds$pop                                    , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male                               , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_00_04                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_00_09                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_10_14                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_15_17                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_18_19                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_20_20                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_21_21                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_22_24                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_25_29                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_30_34                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_35_39                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_40_44                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_45_49                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_50_54                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_55_59                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_60_61                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_62_64                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_65_66                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_67_69                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_70_74                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_75_79                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_80_84                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_male_85_plus                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female                             , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_00_04                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_00_09                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_10_14                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_15_17                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_18_19                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_20_20                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_21_21                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_22_24                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_25_29                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_30_34                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_35_39                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_40_44                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_45_49                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_50_54                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_55_59                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_60_61                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_62_64                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_65_66                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_67_69                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_70_74                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_75_79                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_80_84                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_female_85_plus                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_black                              , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_black_male                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_black_female                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_american_indian                    , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_american_indian_male               , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_american_indian_female             , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_asian                              , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_asian_male                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_asian_female                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_pacific                            , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_pacific_male                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_pacific_female                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_other                              , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_other_male                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_other_female                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_mixed                              , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_mixed_male                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_mixed_female                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_white                              , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_white_male                         , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_white_female                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_latino                             , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_latino_male                        , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_latino_female                      , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_not_citizen                        , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_foreign_born                       , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_poverty_ratio                      , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_poverty_ratio_0_1                  , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_poverty_ratio_1_2                  , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_poverty_ratio_2_plus               , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_000_000k                    , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_000_009k                    , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_010_014k                    , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_015_019k                    , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_020_024k                    , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_025_029                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_030_034                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_035_039                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_040_044                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_045_049                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_050_059                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_060_074                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_075_099                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_100_124                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_125_149                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_150_199                     , any.missing=T , lower=0, upper=max_pop)
checkmate::assert_integer(  ds$pop_income_200_plus                    , any.missing=T , lower=0, upper=max_pop)

# ---- specify-columns-to-write ------------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.
ds_slim <-
  ds %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(
    zipcode,
    zipcode_prefix_3,
    lat,
    long,
    zipcode_type,
    ruca,
    ruca_cut4,
    distance_from_zipcode_to_oucp_in_miles,

    pop                                    ,
    pop_male                               ,
    pop_female                             ,

    pop_male_00_04                         ,
    pop_male_00_09                         ,
    pop_male_10_14                         ,
    pop_male_15_17                         ,
    pop_male_18_19                         ,
    pop_male_20_20                         ,
    pop_male_21_21                         ,
    pop_male_22_24                         ,
    pop_male_25_29                         ,
    pop_male_30_34                         ,
    pop_male_35_39                         ,
    pop_male_40_44                         ,
    pop_male_45_49                         ,
    pop_male_50_54                         ,
    pop_male_55_59                         ,
    pop_male_60_61                         ,
    pop_male_62_64                         ,
    pop_male_65_66                         ,
    pop_male_67_69                         ,
    pop_male_70_74                         ,
    pop_male_75_79                         ,
    pop_male_80_84                         ,
    pop_male_85_plus                       ,

    pop_female_00_04                       ,
    pop_female_00_09                       ,
    pop_female_10_14                       ,
    pop_female_15_17                       ,
    pop_female_18_19                       ,
    pop_female_20_20                       ,
    pop_female_21_21                       ,
    pop_female_22_24                       ,
    pop_female_25_29                       ,
    pop_female_30_34                       ,
    pop_female_35_39                       ,
    pop_female_40_44                       ,
    pop_female_45_49                       ,
    pop_female_50_54                       ,
    pop_female_55_59                       ,
    pop_female_60_61                       ,
    pop_female_62_64                       ,
    pop_female_65_66                       ,
    pop_female_67_69                       ,
    pop_female_70_74                       ,
    pop_female_75_79                       ,
    pop_female_80_84                       ,
    pop_female_85_plus                     ,

    pop_black                              ,
    pop_black_male                         ,
    pop_black_female                       ,
    pop_american_indian                    ,
    pop_american_indian_male               ,
    pop_american_indian_female             ,
    pop_asian                              ,
    pop_asian_male                         ,
    pop_asian_female                       ,
    pop_pacific                            ,
    pop_pacific_male                       ,
    pop_pacific_female                     ,
    pop_other                              ,
    pop_other_male                         ,
    pop_other_female                       ,
    pop_mixed                              ,
    pop_mixed_male                         ,
    pop_mixed_female                       ,
    pop_white                              ,
    pop_white_male                         ,
    pop_white_female                       ,
    pop_latino                             ,
    pop_latino_male                        ,
    pop_latino_female                      ,

    pop_not_citizen                        ,
    pop_foreign_born                       ,

    pop_poverty_ratio                      ,
    pop_poverty_ratio_0_1                  ,
    pop_poverty_ratio_1_2                  ,
    pop_poverty_ratio_2_plus               ,

    pop_income_000_000k                    ,
    pop_income_000_009k                    ,
    pop_income_010_014k                    ,
    pop_income_015_019k                    ,
    pop_income_020_024k                    ,
    pop_income_025_029                     ,
    pop_income_030_034                     ,
    pop_income_035_039                     ,
    pop_income_040_044                     ,
    pop_income_045_049                     ,
    pop_income_050_059                     ,
    pop_income_060_074                     ,
    pop_income_075_099                     ,
    pop_income_100_124                     ,
    pop_income_125_149                     ,
    pop_income_150_199                     ,
    pop_income_200_plus                    ,
  )
ds_slim

# ---- save-to-db -------------------------------------------------
OuhscMunge::upload_sqls_odbc(
  d             = ds_slim,
  schema_name   = config$schema_lexis,
  table_name    = "dim_zipcode",
  # dsn_name      = "playground_1",
  dsn_name      = config$dsn_outpost,
  clear_table   = T,
  create_table  = F
)
