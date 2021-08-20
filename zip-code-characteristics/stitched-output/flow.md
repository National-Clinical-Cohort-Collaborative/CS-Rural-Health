



This report was automatically generated with the R package **knitr**
(version 1.33).


```r
# knitr::stitch_rmd(script="flow.R", output="stitched-output/flow.md")
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
```


```r
import::from("magrittr", "%>%")

requireNamespace("purrr")
```

```
## Loading required namespace: purrr
```

```r
requireNamespace("rlang")
# requireNamespace("checkmate")
# requireNamespace("fs")
requireNamespace("OuhscMunge") # remotes::install_github("OuhscBbmc/OuhscMunge")
```

```
## Loading required namespace: OuhscMunge
```

```r
# Allow multiple files below to have the same chunk name.
#    If the `root.dir` option is properly managed in the Rmd files, no files will be overwritten.
options(knitr.duplicate.label = "allow")

config        <- config::get()

# open log
if( interactive() ) {
  sink_log <- FALSE
} else {
  message("Creating flow log file at ", config$path_log_flow)

  if( !dir.exists(dirname(config$path_log_flow)) ) {
    # Create a month-specific directory, so they're easier to find & compress later.
    dir.create(dirname(config$path_log_flow), recursive=T)
  }

  file_log  <- file(
    description   = config$path_log_flow,
    open          = "wt"
  )
  sink(
    file    = file_log,
    type    = "message"
  )
  sink_log <- TRUE
}

# Typically, only `ds_rail` changes.  Everything else in this file is constant between projects.
ds_rail  <- tibble::tribble(
  ~fx         , ~path,

  # ETL (extract-transform-load) the data from the outside world.
  "run_r"     , "manipulation/zip-code.R"

  # Second-level manipulation on data inside the warehouse.
  # "run_sql" , "manipulation/inserts-to-normalized-tables.sql"

  # Scribes create analysis-ready rectangles.
  # "run_r"     , "manipulation/te-scribe.R",

  # Reports for human consumers.
  # "run_rmd"   , "analysis/car-report-1/car-report-1.Rmd",

  # Dashboards for human consumers.
  # "run_rmd" , "analysis/dashboard-1/dashboard-1.Rmd"
)

run_r <- function( minion ) {
  message("\nStarting `", basename(minion), "` at ", Sys.time(), ".")
  base::source(minion, local=new.env())
  message("Completed `", basename(minion), "`.")
  return( TRUE )
}
run_sql <- function( minion ) {
  message("\nStarting `", basename(minion), "` at ", Sys.time(), ".")
  OuhscMunge::execute_sql_file(minion, config$dsn_staging)
  message("Completed `", basename(minion), "`.")
  return( TRUE )
}
run_rmd <- function( minion ) {
  message("Pandoc available: ", rmarkdown::pandoc_available())
  message("Pandoc version: ", rmarkdown::pandoc_version())

  message("\nStarting `", basename(minion), "` at ", Sys.time(), ".")
  path_out <- rmarkdown::render(minion, envir=new.env())
  Sys.sleep(3) # Sleep for three secs, to let pandoc finish
  message(path_out)

  # Uncomment to save a dated version to a different location.
  #   Do this before the undated version, in case someone left it open (& locked it)
  # path_out_archive <- strftime(Sys.Date(), config$path_report_screen_archive)
  # if( !dir.exists(dirname(path_out_archive)) ) {
  #   # Create a month-specific directory, so they're easier to find & compress later.
  #   message("Creating subdirectory for archived eligibility reports: `", dirname(path_out_archive), "`.")
  #   dir.create(dirname(path_out_archive), recursive=T)
  # }
  # archive_successful <- file.copy(path_out, path_out_archive, overwrite=TRUE)
  # message("Archive success: ", archive_successful, " at `", path_out_archive, "`.")

  # Uncomment to copy the undated version to a different location.
  # If saving to a remote drive, this works better than trying to save directly from `rmarkdown::render()`.
  # To use this, you'll need a version of `run_rmd()` that's specialized for the specific rmd.
  # fs::file_copy(path_out, config$path_out_remote, overwrite = TRUE)

  return( TRUE )
}
run_python <- function( minion ) {
  message("\nStarting `", basename(minion), "` at ", Sys.time(), ".")
  # reticulate::use_python(Sys.which("python3"))
  reticulate::source_python(minion)
  # reticulate::source_python(minion, envir = NULL)
  message("Completed `", basename(minion), "`.")
  return( TRUE )
}

(file_found <- purrr::map_lgl(ds_rail$path, file.exists))
```

```
## [1] TRUE
```

```r
if( !all(file_found) ) {
  warning("--Missing files-- \n", paste0(ds_rail$path[!file_found], collapse="\n"))
  stop("All source files to be run should exist.")
}
```



```r
message("Starting flow of `", basename(base::getwd()), "` at ", Sys.time(), ".")
```

```
## Starting flow of `zip-code-characteristics` at 2021-08-19 23:41:50.
```

```r
warn_level_initial <- as.integer(options("warn"))
# options(warn=0)  # warnings are stored until the top–level function returns
# options(warn=2)  # treat warnings as errors

elapsed_duration <- system.time({
  purrr::map2_lgl(
    ds_rail$fx,
    ds_rail$path,
    function(fn, args) rlang::exec(fn, !!!args)
  )
})
```

```
## 
## Starting `zip-code.R` at 2021-08-19 23:41:50.
```

```
## Loading required namespace: readr
```

```
## Loading required namespace: tidyr
```

```
## Loading required namespace: checkmate
```

```
## Loading required namespace: sqldf
```

```
## Loading required namespace: geosphere
```

```
## Distance start time: 2021-08-19 23:41:51
```

```
## Completed `zip-code.R`.
```

```r
message("Completed flow of `", basename(base::getwd()), "` at ", Sys.time(), "")
```

```
## Completed flow of `zip-code-characteristics` at 2021-08-19 23:41:51
```

```r
elapsed_duration
```

```
##    user  system elapsed 
##    1.71    0.08    1.68
```

```r
options(warn=warn_level_initial)  # Restore the whatever warning level you started with.
```

```r
# close(file_log)
if( sink_log ) {
  sink(file = NULL, type = "message") # ends the last diversion (of the specified type).
  message("Closing flow log file at ", gsub("/", "\\\\", config$path_log_flow))
}

# bash: Rscript flow.R
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 4.1.0 Patched (2021-05-29 r80415)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows >= 8 x64 (build 9200)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices
## [4] utils     datasets  methods  
## [7] base     
## 
## other attached packages:
## [1] RSQLite_2.2.7  magrittr_2.0.1
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.7           
##  [2] pillar_1.6.1         
##  [3] compiler_4.1.0       
##  [4] tools_4.1.0          
##  [5] import_1.2.0         
##  [6] bit_4.0.4            
##  [7] lattice_0.20-44      
##  [8] evaluate_0.14        
##  [9] memoise_2.0.0        
## [10] lifecycle_1.0.0      
## [11] tibble_3.1.3         
## [12] checkmate_2.0.0      
## [13] pkgconfig_2.0.3      
## [14] rlang_0.4.11         
## [15] DBI_1.1.1            
## [16] parallel_4.1.0       
## [17] yaml_2.2.1           
## [18] xfun_0.24            
## [19] proto_1.0.0          
## [20] fastmap_1.1.0        
## [21] dplyr_1.0.7          
## [22] stringr_1.4.0        
## [23] knitr_1.33           
## [24] generics_0.1.0       
## [25] vctrs_0.3.8          
## [26] hms_1.1.0            
## [27] grid_4.1.0           
## [28] bit64_4.0.5          
## [29] tidyselect_1.1.1     
## [30] glue_1.4.2           
## [31] OuhscMunge_0.2.0.9014
## [32] sqldf_0.4-11         
## [33] R6_2.5.0             
## [34] fansi_0.5.0          
## [35] tcltk_4.1.0          
## [36] vroom_1.5.3          
## [37] gsubfn_0.7           
## [38] sp_1.4-5             
## [39] tidyr_1.1.3          
## [40] tzdb_0.1.2           
## [41] readr_2.0.0          
## [42] purrr_0.3.4          
## [43] blob_1.2.2           
## [44] backports_1.2.1      
## [45] ellipsis_0.3.2       
## [46] assertthat_0.2.1     
## [47] geosphere_1.5-10     
## [48] config_0.3.1         
## [49] utf8_1.2.2           
## [50] stringi_1.7.3        
## [51] cachem_1.0.5         
## [52] chron_2.3-56         
## [53] crayon_1.4.1
```

```r
Sys.time()
```

```
## [1] "2021-08-19 23:41:52 CDT"
```

