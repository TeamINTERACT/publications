---
title: "accel_metrics_2021_Fuller"
author: "Daniel Fuller"
date: "22/02/2021"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---



## Required packages

```r
library(tidyverse)
```

```
## ── Attaching packages ───────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ──────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)
library(naniar)
library(ggmap)
```

```
## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
```

```
## Please cite ggmap if you use it! See citation("ggmap") for details.
```

```r
library(ggthemes)
library(sf)
```

```
## Linking to GEOS 3.8.1, GDAL 3.1.1, PROJ 6.3.1
```

```r
library(rgeos)
```

```
## Loading required package: sp
```

```
## rgeos version: 0.5-5, (SVN revision 640)
##  GEOS runtime version: 3.8.1-CAPI-1.13.3 
##  Linking to sp version: 1.4-2 
##  Polygon checking: TRUE
```

```r
library(cancensus)
```

```
## Census data is currently stored temporarily.
## 
##  In order to speed up performance, reduce API quota usage, and reduce unnecessary network calls, please set up a persistent cache directory by setting options(cancensus.cache_path = '<path to cancensus cache directory>')
## 
##  You may add this option, together with your API key, to your .Rprofile.
```

```r
library(cowplot)
```

```
## 
## Attaching package: 'cowplot'
```

```
## The following object is masked from 'package:ggthemes':
## 
##     theme_map
```

```
## The following object is masked from 'package:ggmap':
## 
##     theme_nothing
```

```
## The following object is masked from 'package:lubridate':
## 
##     stamp
```

```r
library(ggspatial)
library(knitr)
library(corrr)

setwd("/Users/dfuller/Dropbox/Projects/INTERACT_github/publications/accel_metrics_2021_Fuller")
```

## Analysis Plan 

1. Health Survey wrangling
2. Accelerometer Metrics 
3. Accel Metrics + Health Survey
    * Physical activity (self-report)
    * Happiness
    * Social connectedness 

### Read in health data


```r
data <- read_csv("/Users/dfuller/Documents/INTERACT/data/health_clean.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   city_id = col_character(),
##   questionnaire_lang.x = col_character(),
##   date_of_survey = col_date(format = ""),
##   mode_used = col_character(),
##   mode_used_txt = col_character(),
##   cars_access_outside = col_character(),
##   cars_access_outside_txt = col_character(),
##   bike_access_options = col_character(),
##   bike_access_options_txt = col_character(),
##   tracking1_txt = col_character(),
##   house_tenure_txt = col_character(),
##   dwelling_type_txt = col_character(),
##   residence = col_date(format = ""),
##   gender_txt = col_character(),
##   sex_txt = col_logical(),
##   living_arrange = col_character(),
##   living_arrange_txt = col_character(),
##   group_id_mtl = col_character(),
##   group_id_mtl_txt = col_character(),
##   employment_txt = col_character()
##   # ... with 204 more columns
## )
```

```
## See spec(...) for full column specifications.
```

```
## Warning: 35032 parsing failures.
##  row            col           expected actual                                                      file
## 1156 sask_bus_pass  1/0/T/F/TRUE/FALSE     2  '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## 1156 bus_safe       1/0/T/F/TRUE/FALSE     2  '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## 1156 bus_reliable   1/0/T/F/TRUE/FALSE     2  '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## 1156 bus_convenient 1/0/T/F/TRUE/FALSE     2  '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## 1156 bus_freq_a     1/0/T/F/TRUE/FALSE     91 '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## .... .............. .................. ...... .........................................................
## See problems(...) for more details.
```

```r
data <- data %>% select(interact_id, city_id, date_of_survey, gwb_happiness, belonging, belonging_cat, pwb_wellbeing, education_recode, age, gender_recode2, income_recode, leisure_modpa_freq, work_modpa_freq, work_vigpa_freq, leisure_vigpa_freq, sit_weekday, sit_weekend)
```

### Happiness


```r
summary(data$gwb_happiness)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   4.500   5.500   5.252   6.300   7.000     114
```

```r
table(data$gwb_happiness)
```

```
## 
##   1 1.3 1.5 1.8   2 2.3 2.5 2.8   3 3.3 3.5 3.8   4 4.3 4.5 4.8   5 5.3 5.5 5.8 
##   3   3   3   7  11  17  28  27  27  46  62  58  74  83 105  98 137 146 166 187 
##   6 6.3 6.5 6.8   7 
## 186 172 115 102 109
```

### Sense of community belonging


```r
table(data$city_id, data$belonging_cat)
```

```
##            
##               0   1
##   Montréal  572 514
##   Saskatoon 158 142
##   Vancouver 122 200
##   Victoria   51 114
```

```r
ggplot(data, aes(belonging_cat)) + 
      geom_bar() + 
      facet_wrap(~ city_id)
```

```
## Warning: Removed 213 rows containing non-finite values (stat_count).
```

![](accel_metrics_analysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Read in accel metrics


```r
accel_metrics <- read_csv("/Users/dfuller/Documents/INTERACT/data/SaskatoonW1_three_init_fit_results.csv")
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   interact_id = col_double(),
##   x1 = col_double(),
##   x2 = col_double(),
##   x3 = col_double(),
##   x4 = col_double(),
##   m1 = col_double(),
##   m2 = col_double(),
##   m3 = col_double(),
##   a1 = col_double(),
##   a2 = col_double(),
##   a3 = col_double(),
##   r2 = col_double(),
##   mse = col_double(),
##   exp_var = col_double(),
##   aic = col_double(),
##   bic = col_double()
## )
```

### Join in accel metrics to health survey


```r
health_accel <- left_join(accel_metrics, data, by = "interact_id")
```

### Associations with self-report activity


```r
### Self report data has a -7 for NA values 

health_accel <- health_accel %>%
          mutate(across(where(is.numeric), ~na_if(., -7)))

### Creating a mod+vig variable

health_accel$all_mod_vig_freq <- health_accel$leisure_modpa_freq + health_accel$leisure_vigpa_freq +                                  health_accel$work_modpa_freq + health_accel$work_vigpa_freq

### Summary stats on Mod Vig variable

summary(health_accel$all_mod_vig_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    40.0   226.2   352.5   472.5   521.2  1545.0      63
```

```r
mod_vig_pa_histo <- ggplot(health_accel, aes(all_mod_vig_freq)) + 
                geom_histogram()
plot(mod_vig_pa_histo)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 63 rows containing non-finite values (stat_bin).
```

![](accel_metrics_analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Associations with self-report sedentary time


```r
### Weekdays
summary(health_accel$sit_weekday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    30.0   240.0   360.0   385.7   540.0   780.0       1
```

```r
sit_weekday_histo <- ggplot(health_accel, aes(sit_weekday)) + 
                geom_histogram()
plot(sit_weekday_histo)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 1 rows containing non-finite values (stat_bin).
```

![](accel_metrics_analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
### Weekend days
summary(health_accel$sit_weekend)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    60.0   240.0   360.0   361.7   480.0   960.0       1
```

```r
sit_weekend_histo <- ggplot(health_accel, aes(sit_weekend)) + 
                geom_histogram()
plot(sit_weekend_histo)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 1 rows containing non-finite values (stat_bin).
```

![](accel_metrics_analysis_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

### Correlations


```r
x <- health_accel %>%
       select(all_mod_vig_freq, sit_weekday, sit_weekend, x1, x2, x3, x4, m1, m2, m3, a1, a2, a2) %>%
       correlate() %>%    # Create correlation data frame (cor_df)
       rearrange() %>%  # rearrange by correlations
       shave() # Shave off the upper triangle for a clean result
```

```
## 
## Correlation method: 'pearson'
## Missing treated using: 'pairwise.complete.obs'
```

```r
fashion(x)
```

```
##                term all_mod_vig_freq   x1 sit_weekend   x3   m2   m1   x4
## 1  all_mod_vig_freq                                                      
## 2                x1             -.11                                     
## 3       sit_weekend              .58  .13                                
## 4                x3              .11 -.09         .04                    
## 5                m2             -.37  .10        -.02 -.11               
## 6                m1              .38 -.01         .13 -.09 -.06          
## 7                x4             -.09 -.01         .22  .12 -.08  .12     
## 8       sit_weekday             -.37  .13         .51  .26 -.10 -.10  .23
## 9                m3             -.57  .05         .18  .23 -.39 -.09  .51
## 10               x2             -.15  .23         .12 -.05  .24  .25  .17
## 11               a1             -.08 -.15         .12  .00 -.15  .28  .14
## 12               a2             -.26 -.24         .14 -.01 -.05  .24  .18
##    sit_weekday   m3   x2   a1 a2
## 1                               
## 2                               
## 3                               
## 4                               
## 5                               
## 6                               
## 7                               
## 8                               
## 9          .36                  
## 10         .20  .01             
## 11         .24  .12  .83        
## 12         .26  .19  .77  .95
```

### Correlation plot

```r
health_accel %>% 
  select(all_mod_vig_freq, sit_weekday, sit_weekend, x1, x2, x3, x4, m1, m2, m3, a1, a2, a2) %>%
  correlate() %>% 
  network_plot(min_cor = .2)
```

```
## 
## Correlation method: 'pearson'
## Missing treated using: 'pairwise.complete.obs'
```

![](accel_metrics_analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

# Physical Activity

### Reading in the physical activity data from SenseDoc
```{}
data_temp <- NULL

data_temp <- dir(path = "/Users/dfuller/Documents/INTERACT/data/sensors/", full.names = TRUE, pattern = "*_sd.csv", recursive = TRUE) %>%
   purrr::map(function(i){
     dfhx <- read.csv(i, header = TRUE)
     temp <- bind_rows(data_temp, dfhx)
   })

sd_data <- bind_rows(data_temp)
rm(data_temp)
table(sd_data$city)
```
