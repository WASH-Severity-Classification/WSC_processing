
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WSC <img src='man/figures/WSC_logo_EN.png' align="right" height="138.5" />

![R-CMD-check](https://github.com/ElliottMess/WSC/workflows/R-CMD-check/badge.svg)

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

## Main Functions

The package contains three main functions:

    1. WSC::score_WIS(): scores a dataset according to the calculation model.
    2. WSC::agg_score(): aggregates results at a specified administrative level
    3. WSC::twenty_rule(): applies the 20% rule to a specified datasets
    4. WSC::assign_hiAdmin_loAdmin(): assign results from a higher administrative level to a lower one in an uniform way (all lower units part of a higher administrative unit have the same value).
    5. WSC::score_df_AP(): Score dataset according to the Analysis Plan (AP) phases.
    6. WSC::scoring_var(): Score individual variables according to AP.

Working examples are provided for all the functions based on the
datasets documented within the package.

### 1\. score\_WIS

``` r
library(WSC)
library(knitr)

WIS_scored <- score_WIS(data = WSC::bfa_msna_2020, context_AP = WSC::context_AP, context = "bfa_2020",
         WSC_AP = WSC::WSC_AP, WIS_water = WSC::WIS_water, WIS_sanitation = WSC::WIS_sanitation,
         WIS_final = WSC::WIS_final)
#> Warning in r3c(scoring$key_water, WIS_water$key_water, WIS_water$score_water)
#> %>% : NAs introduced by coercion
#> Warning in r3c(scoring$key_sanit, WIS_sanitation$key_sanit,
#> WIS_sanitation$score_sanit) %>% : NAs introduced by coercion
#> Warning in scoring$score %>% as.numeric(): NAs introduced by coercion

kable(head(WIS_scored))
```

| admin1      | admin2     | admin3   | water\_source | time\_going\_water\_source | time\_queing\_water\_source | sufficiency\_of\_water | access\_to\_soap | uuid                                 | cluster\_id    | distance\_to\_water\_source | weights     | type\_of\_sanitation\_facility | sanitation\_facility\_sharing | water\_source\_dist | key\_water                      | key\_sanit                                       | water\_score | sanit\_score | key\_score | score | score\_final | admin0 |
| :---------- | :--------- | :------- | :------------ | :------------------------- | :-------------------------- | :--------------------- | :--------------- | :----------------------------------- | :------------- | :-------------------------- | :---------- | :----------------------------- | :---------------------------- | :------------------ | :------------------------------ | :----------------------------------------------- | -----------: | -----------: | :--------- | :---- | -----------: | :----- |
| centre\_est | koulpelogo | ouargaye | improved      | less\_30                   | less\_30                    | sufficient             | no\_soap         | f3227fe6-78ba-490b-9480-0f31750ac1f6 | BF480204\_pdi  | less\_30                    | 0.034512707 | open\_defec                    | NA                            | improved\_less\_30  | sufficient-/-improved\_less\_30 | open\_defec-/-NA-/-no\_soap                      |            2 |            4 | 2-/-4      | 3     |            3 | BFA    |
| centre\_est | koulpelogo | ouargaye | improved      | more\_30                   | less\_30                    | sufficient             | no\_soap         | e27b5949-728e-4913-b07f-c94a1b278ace | BF480204\_pdi  | more\_30                    | 0.034512707 | open\_defec                    | NA                            | improved\_more\_30  | sufficient-/-improved\_more\_30 | open\_defec-/-NA-/-no\_soap                      |            3 |            4 | 3-/-4      | 4     |            4 | BFA    |
| centre\_est | koulpelogo | ouargaye | improved      | less\_30                   | less\_30                    | sufficient             | no\_soap         | 127ce1fa-e05c-4b3e-9eca-8df58a4c61de | BF480204\_pdi  | less\_30                    | 0.034512707 | latrine\_nonhygienic           | shared\_less20                | improved\_less\_30  | sufficient-/-improved\_less\_30 | latrine\_nonhygienic-/-shared\_less20-/-no\_soap |            2 |            3 | 2-/-3      | 3     |            3 | BFA    |
| centre\_est | koulpelogo | ouargaye | improved      | less\_30                   | less\_30                    | sufficient             | soap             | 994088cf-ac1e-4174-9ecb-ff2201d98655 | BF480204\_pdi  | less\_30                    | 0.034512707 | latrine\_hygienic              | not\_shared                   | improved\_less\_30  | sufficient-/-improved\_less\_30 | latrine\_hygienic-/-not\_shared-/-soap           |            2 |            1 | 2-/-1      | 2     |            2 | BFA    |
| centre\_est | koulpelogo | ouargaye | improved      | less\_30                   | less\_30                    | sufficient             | no\_soap         | afff9552-7ae6-403c-b688-f501285458a2 | BF480204\_pdi  | less\_30                    | 0.034512707 | latrine\_hygienic              | shared\_20to50                | improved\_less\_30  | sufficient-/-improved\_less\_30 | latrine\_hygienic-/-shared\_20to50-/-no\_soap    |            2 |            4 | 2-/-4      | 3     |            3 | BFA    |
| sahel       | seno       | dori     | improved      | more\_30                   | more\_30                    | sufficient             | soap             | ec37cd9c-27a4-4a48-8ac5-ceef62c3cfc7 | BF560202\_host | more\_30                    | 0.574334461 | latrine\_hygienic              | shared\_less20                | improved\_more\_30  | sufficient-/-improved\_more\_30 | latrine\_hygienic-/-shared\_less20-/-soap        |            3 |            2 | 3-/-2      | 3     |            3 | BFA    |

### 2\. agg\_score

``` r
library(WSC)
library(knitr)

score_agg_admin2 <- agg_score(context = "bfa_2020", context_AP = WSC::context_AP,
          WSC_AP = WSC::WSC_AP, data = WSC::bfa_msna_2020)
#> Warning in r3c(scoring$key_water, WIS_water$key_water, WIS_water$score_water)
#> %>% : NAs introduced by coercion
#> Warning in r3c(scoring$key_sanit, WIS_sanitation$key_sanit,
#> WIS_sanitation$score_sanit) %>% : NAs introduced by coercion
#> Warning in scoring$score %>% as.numeric(): NAs introduced by coercion

kable(head(score_agg_admin2))
```

| admin2 | indicator  | choice |     value | context   |
| :----- | :--------- | :----- | --------: | :-------- |
| bale   | key\_score | 2-/-1  | 0.0483755 | bfa\_2020 |
| bale   | key\_score | 2-/-2  | 0.2598389 | bfa\_2020 |
| bale   | key\_score | 2-/-3  | 0.0125265 | bfa\_2020 |
| bale   | key\_score | 2-/-4  | 0.0592587 | bfa\_2020 |
| bale   | key\_score | 3-/-1  | 0.0686043 | bfa\_2020 |
| bale   | key\_score | 3-/-2  | 0.1557088 | bfa\_2020 |

### 3\. twenty\_rule

``` r
library(WSC)
library(knitr)

admin2_twenty_ruled <- twenty_rule(data = score_agg_admin2, col_score = "indicator",
            col_label = "choice", name_final_score = "score_final",
            col_agg = "admin2", col_value = "value")

kable(head(admin2_twenty_ruled))
```

| admin2     | indicator    | context   |  score\_1 |  score\_2 |  score\_3 |  score\_4 |  score\_5 | score\_final |
| :--------- | :----------- | :-------- | --------: | --------: | --------: | --------: | --------: | :----------- |
| bale       | score\_final | bfa\_2020 |        NA | 0.3824060 | 0.3978353 | 0.2045352 | 0.0152235 | 4            |
| bam        | score\_final | bfa\_2020 | 0.0120944 | 0.1990463 | 0.4323360 | 0.3504298 | 0.0060935 | 4            |
| banwa      | score\_final | bfa\_2020 | 0.0071037 | 0.2419910 | 0.5036909 | 0.2187994 | 0.0284149 | 4            |
| bazega     | score\_final | bfa\_2020 |        NA | 0.1791556 | 0.5628474 | 0.2579969 |        NA | 4            |
| bougouriba | score\_final | bfa\_2020 |        NA | 0.1117228 | 0.4725281 | 0.3882772 | 0.0274719 | 4            |
| boulgou    | score\_final | bfa\_2020 | 0.0444890 | 0.2459820 | 0.4317736 | 0.2777554 |        NA | 4            |

### 4\. assign\_hiAdmin\_loAdmin

``` r
library(WSC)
library(knitr)

admin1_admin2_agg <- assign_hiAdmin_loAdmin(HiAdmin_df = WSC::bfa_smart_2019_admin1, HiAdmin_name = "admin1",
                       HiAdmin_df_name = "smart_2019_admin1",
                       context = "bfa_2020", context_AP = WSC::context_AP,
                       WSC_AP = WSC::WSC_AP, LoAdmin_df = WSC::bfa_msna_2020, LoAdmin_name = "admin2")



kable(head(admin1_admin2_agg))
```

| admin2     | indicator | choice | value | context   |
| :--------- | :-------- | :----- | ----: | :-------- |
| koulpelogo | gam\_muac | NA     |   2.3 | bfa\_2020 |
| seno       | gam\_muac | NA     |   3.3 | bfa\_2020 |
| sanmatenga | gam\_muac | NA     |   2.1 | bfa\_2020 |
| boulkiemde | gam\_muac | NA     |   2.1 | bfa\_2020 |
| loroum     | gam\_muac | NA     |   1.2 | bfa\_2020 |
| yatenga    | gam\_muac | NA     |   1.2 | bfa\_2020 |

### 5\. score\_df\_AP

``` r
library(WSC)
library(knitr)

area_df <- score_df_AP(data = WSC::bfa_smart_2019_admin1, data_name = "smart_2019_admin1",
         data_type = "area",
         agg_level = "admin1", context = "bfa_2020", context_AP = WSC::context_AP,
         WSC_AP = WSC::WSC_AP)

hh_df <- score_df_AP(data = WSC::bfa_msna_2020, data_name = "msna_2020",
         data_type = "hh",
         agg_level = "admin1", context = "bfa_2020", context_AP = WSC::context_AP,
         WSC_AP = WSC::WSC_AP)



kable(head(hh_df))
```

| admin1              | indicator   | choice |     value | context   |
| :------------------ | :---------- | :----- | --------: | :-------- |
| boucle\_du\_mouhoun | rcsi\_score | 1      | 0.6779283 | bfa\_2020 |
| boucle\_du\_mouhoun | rcsi\_score | 2      | 0.2610477 | bfa\_2020 |
| boucle\_du\_mouhoun | rcsi\_score | 3      | 0.0610240 | bfa\_2020 |
| cascades            | rcsi\_score | 1      | 0.6445926 | bfa\_2020 |
| cascades            | rcsi\_score | 2      | 0.3327372 | bfa\_2020 |
| cascades            | rcsi\_score | 3      | 0.0226702 | bfa\_2020 |

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
