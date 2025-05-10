
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{soildiveragro}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{soildiveragro}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
soildiveragro::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-04-08 08:48:31 CEST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading soildiveragro
#> ── R CMD check results ─────────────────────────── soildiveragro 0.0.0.9000 ────
#> Duration: 37.5s
#> 
#> ❯ checking R files for non-ASCII characters ... WARNING
#>   Found the following file with non-ASCII characters:
#>     mod_prediction.R
#>   Portable packages must use only ASCII characters in their R code,
#>   except perhaps in comments.
#>   Use \uxxxx escapes for other characters.
#> 
#> ❯ checking for missing documentation entries ... WARNING
#>   Undocumented code objects:
#>     'model_data' 'pred_models' 'rotations' 'scales'
#>   Undocumented data sets:
#>     'model_data' 'pred_models' 'rotations' 'scales'
#>   All user-level objects in a package should have documentation entries.
#>   See chapter 'Writing R documentation files' in the 'Writing R
#>   Extensions' manual.
#> 
#> ❯ checking contents of 'data' directory ... WARNING
#>   Files not of a type allowed in a 'data' directory:
#>     'model_data.rds' 'pred_models.rds'
#>   Please use e.g. 'inst/extdata' for non-R data files
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking top-level files ... NOTE
#>   Non-standard files/directories found at top level:
#>     'analysis.pptx' 'model_data.rds' 'pred_models.rds' 'rotations.rds'
#>     'scales.rds'
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespace in Imports field not imported from: 'pkgload'
#>     All declared Imports should be used.
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'PedoClim (choose from list)'
#>   mod_prediction_server : <anonymous>: no visible global function
#>     definition for 'data'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'scales'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'value'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'm'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 's'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'name'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'x'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'rotations'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'tillage'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'fertilization'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'pesticides'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'pred_models'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'index'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'sq_yield'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'earth_prev'
#>   mod_prediction_server : <anonymous>: no visible global function
#>     definition for 'starts_with'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'model_data'
#>   mod_prediction_server : <anonymous>: no visible global function
#>     definition for 'everything'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'category'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'yield'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'chao1'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'bacteria'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'fungal'
#>   mod_prediction_server : <anonymous>: no visible binding for global
#>     variable 'nematode'
#>   Undefined global functions or variables:
#>     . PedoClim (choose from list) bacteria category chao1 data earth_prev
#>     everything fertilization fungal index m model_data name nematode
#>     pesticides pred_models rotations s scales sq_yield starts_with
#>     tillage value x yield
#>   Consider adding
#>     importFrom("utils", "data")
#>   to your NAMESPACE file.
#>   
#>   Found the following calls to data() loading into the global environment:
#>   File 'soildiveragro/R/mod_prediction.R':
#>     data(scales)
#>     data(rotations)
#>     data(pred_models)
#>     data(model_data)
#>     data(model_data)
#>     data(model_data)
#>     data(model_data)
#>     data(model_data)
#>     data(model_data)
#>   See section 'Good practice' in '?data'.
#> 
#> 0 errors ✔ | 3 warnings ✖ | 4 notes ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> soildiveragro Coverage: 23.92%
#> R/app_config.R: 0.00%
#> R/app_server.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/mod_prediction.R: 0.00%
#> R/mod_welcome.R: 0.00%
#> R/run_app.R: 0.00%
#> R/golem_utils_server.R: 100.00%
#> R/golem_utils_ui.R: 100.00%
```
