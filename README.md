
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WSC

<!-- badges: start -->

<!-- badges: end -->

## About this package

This package offers functions to process data according to the WSC
guidelines.

Functions rely on the existence of two global analysis plans:

  - The general WSC analysis plan (AP) than can be found
    [here](https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing)
    or as an object in the package (`WSC::WSC_AP`)
  - The WASH Insecurity Score (WIS) analysis plan that can be found
    [here](https://docs.google.com/spreadsheets/d/1UCr-G9gD6YZmiOHDoP95qiMkEqi9jMG3lfzzv7WCFnM/edit?usp=sharing)
    (in multiple sheets) or as an object in the package
    (`WSC::WIS_water`, `WSC::WIS_sanitation`, `WSC::WIS_final`)

To contextualise the analysis to the environment in which the WSC is
applied, users should create:

  - A context specific AP that links the indicators in the WSC AP to the
    datasets used in the context analysis. See an example
    [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing)
    or in `WSC::context_AP`.

The data is stored on googlesheets to ease the remote use of the
package, but the functions use `data.frames` as inputs.

## Installation

You can install the latest version of WSC from
[github](https://github.com/ElliottMess/WSC) with:

``` r
devtools::install_github("ElliottMess/WSC")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(WSC)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: googlesheets4
#> Loading required package: car
#> Loading required package: carData
#> 
#> Attaching package: 'car'
#> The following object is masked from 'package:dplyr':
#> 
#>     recode
#> Loading required package: stringr
#> Loading required package: srvyr
#> 
#> Attaching package: 'srvyr'
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> Loading required package: tidyr
## basic example code
```

## About the WSC

The WASH Severity Classification (WSC) is a new interagency global
initiative led by the [Global WASH Cluster](http://washcluster.net/),
[UNICEF](https://www.unicef.org/), and [IMPACT
Initiatives](impact-initiatives.org/). Developed at the global level
through a participatory process, the WSC project aims to develop a
standardized approach to classifying the severity of WASH needs and
vulnerabilities across contexts. For more information, contact
<wsc@reach-initiative.org>.

As the documentation relating to the WSC is still under development,
direct links to them are replaced by placeholder\_link.
