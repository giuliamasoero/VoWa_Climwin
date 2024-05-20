---
title: "Weather-sensitive developmental windows for nestling Alpine swifts"
subtitle: "Supporting Information and codes for the manuscript"

date: "Last compiled on 20 May 2024"
output:
  html_document:
    code_folding: hide
    toc: yes
    toc_depth: 4
    toc_float: yes
    number_sections: yes
    df_print: paged
    theme: cerulean
    collapsed: no
    keep_md: yes
editor_options: 
  chunk_output_type: inline
---



# Abstract and keywords


__Abstract__: The size and growth patterns of nestling birds are key determinants of their survival up to fledging and long-term fitness. However, because traits such as feathers, skeleton and body mass can follow different developmental trajectories, our understanding of the impact of adverse weather on development requires insights on trait-specific sensitive developmental windows. We analysed data from nestling Alpine swifts in Switzerland measured throughout growth up to the age of 50 days (i.e. fledging between 50 and 70 days), for wing length and body mass (2693 nestlings in 25 years) and sternum length (2447 nestlings in 22 years). We show that the sensitive developmental windows for wing and sternum length corresponded to the periods of trait-specific peak growth, which span almost the whole developmental period for wings and the first half for the sternum. Adverse weather conditions during these periods slowed down growth and reduced size. Although nestling body mass at 50 days showed the greatest inter-individual variation, this was explained by weather in the two days before measurement rather than during peak growth. Interestingly, the relationship between temperature and body mass was not linear, and the initial sharp increase in body mass associated with the increase in temperature was followed by a moderate drop on hot days, likely linked to heat stress. Nestlings experiencing adverse weather conditions during wing growth had lower survival rates up to fledging and fledged at later ages, presumably to compensate for slower wing growth. Overall, our results suggest that measures of feather growth and, to some extent, skeletal growth best capture the consequences of adverse weather conditions throughout the whole development of offspring, while body mass better reflects the short, instantaneous effects of weather conditions on their body reserves (i.e. energy depletion vs storage in unfavourable vs favourable conditions).

__Keywords__: Apodiformes, feathers, meteorological conditions, climate change, multi-trait, heat stress, offspring development, early-life conditions, _Tachymarptis (Apus) melba_


__Journal__: Ecology and Evolution


Full script, data and Supporting Information can also be found on the OSF page of the project: https://osf.io/2ndmk/ 


# Libraries and datasets


## Libraries

```r
# to install climwin:
# install.packages("climwin")

# data manipulation
library(dplyr)
library(lubridate)
# models
library(lme4)
library(lmerTest)
library(climwin)
# graphics
library(ggplot2)

knitr::opts_chunk$set(
  fig.path = "Figures/"
)
```


## The datasets

_Study system_

Data were collected between 1999 and 2023 in a Swiss population of Alpine swift. It is a long-distance migratory bird that breeds in colonies of a few to several hundred pairs in holes within cliffs or under the roofs of tall buildings. In Switzerland, Alpine swifts return to their breeding grounds from sub-Saharan Africa at the beginning of April (Meier et al. 2020) and start laying eggs between early May and June, with significant adaptive variations in laying dates depending on weather conditions (de Villemereuil et al. 2020). Females lay one clutch a year, with one to four eggs per clutch (modal clutch size is three). Both parents incubate the eggs for about 18 days and then feed their nestlings until fledging, which occurs around 55 days after hatching (range 50-76 days; (Bize et al. 2004) and this manuscript). After breeding, Alpine swifts migrate back to Africa in September (Meier et al. 2020).

Fieldwork was carried out in two Alpine swift colonies located in clock towers in the Swiss cities of Biel (60-100 breeding pairs) and Solothurn (40-55 breeding pairs), ca. 20km apart. Each year, both colonies were regularly visited to monitor egg laying and clutch size, to capture and measure adults, and to ring and measure nestlings. Nestlings were individually recognised by ringing them with numbered metal rings 10 to 15 days after hatching. Nestlings were measured regularly (usually every 5 to 10 days, on average five times in total) until fledging. At each measurement, we measured wing length with a ruler to the nearest 1 mm, sternum size with a calliper to the nearest 0.1 mm, and body mass with a digital scale to the nearest 0.1 g. The measure of sternum length provides an estimate of skeletal growth and size. Tarsus length has been commonly used in passerines, but it is difficult to measure in a repeatable way in a species with short and bulky tarsi, such as swifts. As nestlings are not ringed at hatching, the age of the nestlings in a brood is based on the hatching date of the first nestling; the last nestling is usually born on the same day or 1 day later. Therefore, measurements for a brood of three nestlings, for example, are taken when the first-hatched nestling reaches 50 days of age, the youngest one might be the same age or 1 day younger. Only nestlings that survived up to fledging were included in the statistical analyses. Sample sizes differ between traits, as wing length and body mass have been measured since 1999, while sternum length has been measured since 2003. 

The figure below shows how the measures are collected. 

A - Wing length is measured using a ruler specific for bird ringing with a bended part on one side to avoid movements of the wing. Data is recorded to the closest millimeter. 

B - Tail length is measured using a thin ruler positioned between the two outermost tail feathers. Data is recorded to the closest millimeter. Both sides are measured and the average is used for the analyses.

C - Sternum length is measured using a calliper. Breast feathers are kept open using cotton and ethanol. Calliper is positioned just above the belly on the lowest part of the sternum bone and then closed on the highest part of the sternum bone, close to the head. Data is recorded to the closest tenth of a millimeter.

D - Body mass is measured using a kitchen scale. Data is recorded to the closest tenth of a gram. 


![Figure 1: Measurement of the Alpine swifts: wing (A), tail (B), sternum (C), body mass (D). Drawings by Martina Cadin.](Figures/alltraits.png)



![Figure 2: Growth of Alpine swifts nestlings.](Figures/Fig.2.png)

<br/><br/>


```r
# loading the datasets
data_nestlings <- read.csv("Data/data_nestlings.csv",
  stringsAsFactors = TRUE, na = c("", "NA")
)
data_weather <- read.csv("Data/data_clim.csv",
  stringsAsFactors = TRUE, na = c("", "NA")
)
```

Nestling data - structure and summary


```r
str(data_nestlings)
```

```
## 'data.frame':	1655 obs. of  9 variables:
##  $ year            : int  2002 2002 2002 2002 2003 2003 2003 2003 2003 2003 ...
##  $ colony          : Factor w/ 1 level "Solothurn": 1 1 1 1 1 1 1 1 1 1 ...
##  $ nestcode_rearing: Factor w/ 775 levels "BT1999A1","BT1999A22",..: 96 96 95 95 142 142 142 155 155 155 ...
##  $ ring            : Factor w/ 1655 levels "ring_000003",..: 1094 189 883 1404 1069 1048 1350 158 295 174 ...
##  $ date_hatch      : Factor w/ 342 levels "01/06/03","01/06/04",..: 331 331 331 331 333 333 333 333 333 333 ...
##  $ mass_50         : num  106.2 97.9 105 111.3 119.7 ...
##  $ wing_50         : num  210 208 211 213 228 ...
##  $ sternum_50      : num  41.2 37.8 40.6 41.5 41.5 40.8 40.3 39.8 40.5 39.5 ...
##  $ brood_size      : int  2 2 2 2 3 3 3 3 3 3 ...
```

```r
summary(data_nestlings)
```

```
##       year            colony      nestcode_rearing          ring     
##  Min.   :1999   Solothurn:1655   BT1999C13:   3    ring_000003:   1  
##  1st Qu.:2005                    BT1999F36:   3    ring_000010:   1  
##  Median :2011                    BT1999H41:   3    ring_000012:   1  
##  Mean   :2011                    BT2000A72:   3    ring_000013:   1  
##  3rd Qu.:2017                    BT2000B34:   3    ring_000016:   1  
##  Max.   :2023                    BT2000D96:   3    ring_000017:   1  
##                                  (Other)  :1637    (Other)    :1649  
##     date_hatch      mass_50          wing_50        sternum_50   
##  14/06/17:  26   Min.   : 63.10   Min.   :151.0   Min.   :30.50  
##  04/06/09:  20   1st Qu.: 90.65   1st Qu.:209.0   1st Qu.:38.70  
##  27/05/11:  19   Median : 96.60   Median :216.0   Median :39.60  
##  06/06/09:  18   Mean   : 96.02   Mean   :213.4   Mean   :39.54  
##  08/06/14:  17   3rd Qu.:102.30   3rd Qu.:221.0   3rd Qu.:40.50  
##  06/06/03:  16   Max.   :127.30   Max.   :237.0   Max.   :44.00  
##  (Other) :1539                                    NA's   :177    
##    brood_size   
##  Min.   :1.000  
##  1st Qu.:2.000  
##  Median :3.000  
##  Mean   :2.698  
##  3rd Qu.:3.000  
##  Max.   :4.000  
## 
```

Weather data - structure and summary

```r
str(data_weather)
```

```
## 'data.frame':	9131 obs. of  5 variables:
##  $ year           : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
##  $ date           : Factor w/ 9131 levels "01/01/00","01/01/01",..: 25 325 625 925 1225 1525 1825 2125 2425 2725 ...
##  $ t_daily_mean   : num  -0.6 -0.04 5.16 7 3.96 1.3 4.48 6.56 3.08 1.08 ...
##  $ rain_daily     : num  0 4 3 1.2 0 0 0.4 9.55 1.4 0 ...
##  $ wind_daily_mean: num  0.82 1.02 4.78 2.08 0.98 1.04 3.22 4.74 4.02 3.82 ...
```

```r
summary(data_weather)
```

```
##       year            date       t_daily_mean       rain_daily    
##  Min.   :1999   01/01/00:   1   Min.   :-12.700   Min.   : 0.000  
##  1st Qu.:2005   01/01/01:   1   1st Qu.:  3.820   1st Qu.: 0.000  
##  Median :2011   01/01/02:   1   Median :  9.960   Median : 0.050  
##  Mean   :2011   01/01/03:   1   Mean   :  9.897   Mean   : 2.898  
##  3rd Qu.:2017   01/01/04:   1   3rd Qu.: 16.045   3rd Qu.: 3.000  
##  Max.   :2023   01/01/05:   1   Max.   : 27.340   Max.   :77.660  
##                 (Other) :9125                                     
##  wind_daily_mean 
##  Min.   : 0.380  
##  1st Qu.: 1.180  
##  Median : 1.700  
##  Mean   : 2.067  
##  3rd Qu.: 2.590  
##  Max.   :10.600  
## 
```

Preparing the variables

```r
# year of measurement as factor
data_nestlings$year_f <- as.factor(data_nestlings$year)
data_nestlings$ring <- as.factor(data_nestlings$ring)

# Date variables as dates
data_nestlings$date_hatch <- as.Date(data_nestlings$date_hatch, "%d/%m/%y")
data_weather$date <- as.Date(data_weather$date, "%d/%m/%y")

# The nestlings are measured at 50 days, this date is necessary for climwin
data_nestlings$date_50days <- data_nestlings$date_hatch + 50

# Hatching date as day of the year, to get the seasonal effect
data_nestlings$hatch_doy <- yday(data_nestlings$date_hatch)

# scaling variables for the analyses
data_nestlings$hatch_doy_sc <- scale(data_nestlings$hatch_doy)[, 1]
```



# Climwin analyses

## Weather data
To estimate the weather conditions during nestling development, we used meteorological data collected from five Swiss meteorological stations surrounding Biel and Solothurn (Bern-Zollikofen, Cressier, Grenchen, Koppigen, Wynau). Doing so allowed us to cover the whole foraging area of the swifts (up to 30 km in a single foraging trip) (Arn-Willi 1960) and to account for microenvironmental variations (i.e., strong weather events captured by one station only). Daily weather data were averaged across the five stations to obtain two variables: mean daily temperature (average air temperature at 2 m above ground for the whole day), daily precipitation (total rainfall for that day) and wind speed (daily mean of the wind speed scalar in m/s.).


![Figure 3: Map of Switzerland representing the location of the two Alpine swift colonies in Biel and Solothurn and the five meteorological stations Bern-Zollikofen, Cressier, Grenchen, Koppigen, Wynau.](Figures/Map.png)


[Base map] (https://commons.wikimedia.org/wiki/File:Switzerland_relief_location_map.jpg) credits to Eric Gaba [Wikimedia Commons user: Sting] (https://commons.wikimedia.org/wiki/User:Sting) and Wikimedia [Commons user: NordNordWest] (https://commons.wikimedia.org/wiki/User:NordNordWest). This file is licensed under the Creative Commons Attribution-Share Alike 3.0 Unported license.




## The climwin approach

The overall approach for the climwin analysis is to compare the support by the data for competing hypotheses and to formalize them into regression models (van de Pol et al., 2016). 

Competing models are based upon a baseline model (called also null model, a model without weather effects) and ranked using the deltaAICc, or the difference in terms of the Akaike Information Criterion values calculated for a small sample size between the candidate model and baseline model. 

Climwin presents the models using the deltaAICc value relative to the baseline model (AICc of the candidate model - AICc of the baseline model). Therefore, a model that is more supported than the baseline model will have a negative deltaAICc value. On the same hand the model with the best support from the data, usually with lowest AICc, will be shown as the model with lowest deltaAICc in the climwin output. 

The baseline model was a linear mixed model with the wing length of the nestlings at 50 days of age in relation to brood size, colony of hatching, hatching day, and exact age of measurement (range 45-55 days of age). The function slidingwin creates a candidate set of competing models testing windows of different lengths for the weather variable of interest, in this study the mean daily ambient temperature (°C).  
 
Non-linear effects of temperature on the traits were taken into account by checking for both linear and quadratic trends. This is mentioned in the climwin output as func = lin (only linear term) func = quad (linear and quadratic terms).

For the analyses we used an individual specific time window, based on the date when the nestlings had 50 days. We then looked for windows between this reference date and 50 days before.
 

### Base model

According to (van de Pol et al. 2016), we built a base model that includes variables that can affect the traits. As random effects, we included brood ID and year as a factor to account for non-independence among nestlings belonging to the same brood and nestlings hatched in the same year. 

__Baseline models__

Creating the subsets needed for the analyses

```r
data_nestlings_wing_50 <- subset(data_nestlings, !is.na(wing_50))
```



```r
wing_50_basemod <- lme4::lmer(
  wing_50 ~
    brood_size + hatch_doy_sc +
    (1 | year_f) + (1 | nestcode_rearing),
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+06)),
  data = data_nestlings_wing_50
)
```

### Finding the best window
Using the function slidingwin allows to search for the best climatic window 


```r
?slidingwin

wing_50_sw <- slidingwin(
  baseline = wing_50_basemod,
  xvar = list(
    temp_mean = data_weather$t_daily_mean
  ),
  stat = c("mean"),
  func = c("lin", "quad"),
  type = "relative", # the window is relative to the individual
  range = c(50, 0), # 0 is the date given in bdate, 50 is the number of days before that date
  cinterval = "day",
  cdate = data_weather$date,
  bdate = data_nestlings_wing_50$date_50days
)
save(wing_50_sw, file = "Output/wing_50_sw.rda")
```

### Investigating the models
Loading the models

```r
load(file = "Output/wing_50_sw.rda")
```

Best windows for linear and linear+quadratic models sorted by deltaAICc such that the best supported model is on top. 


```r
wing_50_mod_combos <- (wing_50_sw$combos)

wing_50_mod_combos[order(wing_50_mod_combos$DeltaAICc), ]
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["response"],"name":[1],"type":["chr"],"align":["left"]},{"label":["climate"],"name":[2],"type":["fct"],"align":["left"]},{"label":["type"],"name":[3],"type":["fct"],"align":["left"]},{"label":["stat"],"name":[4],"type":["fct"],"align":["left"]},{"label":["func"],"name":[5],"type":["fct"],"align":["left"]},{"label":["DeltaAICc"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["WindowOpen"],"name":[7],"type":["int"],"align":["right"]},{"label":["WindowClose"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"wing_50","2":"temp_mean","3":"relative","4":"mean","5":"lin","6":"-16.67","7":"44","8":"18","_rn_":"1"},{"1":"wing_50","2":"temp_mean","3":"relative","4":"mean","5":"quad","6":"-13.43","7":"44","8":"18","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

The linear term better explains the variation in the data (deltaAICc has the lowest value)


##### The best quadratic models

Six of the best windows for the linear models sorted by deltaAICc. To investigate any other tested hypothesis we can simply replace the number on the left or in the double square brackets with the corresponding list number.


```r
head(wing_50_sw[[1]]$Dataset)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["deltaAICc"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["WindowOpen"],"name":[2],"type":["int"],"align":["right"]},{"label":["WindowClose"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ModelBeta"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Std.Error"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["ModelBetaQ"],"name":[6],"type":["lgl"],"align":["right"]},{"label":["ModelBetaC"],"name":[7],"type":["lgl"],"align":["right"]},{"label":["ModelInt"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Function"],"name":[9],"type":["chr"],"align":["left"]},{"label":["Furthest"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["Closest"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["Statistics"],"name":[12],"type":["chr"],"align":["left"]},{"label":["Type"],"name":[13],"type":["chr"],"align":["left"]},{"label":["K"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["ModWeight"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["sample.size"],"name":[16],"type":["int"],"align":["right"]},{"label":["Randomised"],"name":[17],"type":["chr"],"align":["left"]}],"data":[{"1":"-16.66554","2":"44","3":"18","4":"2.300356","5":"0.4853081","6":"NA","7":"NA","8":"177.5048","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.04460104","16":"25","17":"no","_rn_":"1017"},{"1":"-16.30056","2":"43","3":"18","4":"2.207556","5":"0.4854057","6":"NA","7":"NA","8":"179.1969","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.03716133","16":"25","17":"no","_rn_":"972"},{"1":"-16.27774","2":"44","3":"17","4":"2.349050","5":"0.4854653","6":"NA","7":"NA","8":"176.5117","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.03673975","16":"25","17":"no","_rn_":"1018"},{"1":"-16.02761","2":"43","3":"17","4":"2.270356","5":"0.4855939","6":"NA","7":"NA","8":"177.9373","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.03242062","16":"25","17":"no","_rn_":"973"},{"1":"-15.70118","2":"42","3":"18","4":"2.092271","5":"0.4856366","6":"NA","7":"NA","8":"181.3211","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.02753830","16":"25","17":"no","_rn_":"928"},{"1":"-15.51546","2":"44","3":"16","4":"2.373877","5":"0.4856956","6":"NA","7":"NA","8":"176.0014","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.02509621","16":"25","17":"no","_rn_":"1019"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


The best windows (deltaAICc difference from the best model < 2) for the linear+quadratic models sorted by deltaAICc. All models with the lowest AICc present very comparable windows:


```r
wing_50_bestmod <- subset(
  wing_50_sw[[1]]$Dataset,
  deltaAICc < wing_50_sw[[1]]$Dataset$deltaAICc[1] + 2
)

wing_50_bestmod
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["deltaAICc"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["WindowOpen"],"name":[2],"type":["int"],"align":["right"]},{"label":["WindowClose"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ModelBeta"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Std.Error"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["ModelBetaQ"],"name":[6],"type":["lgl"],"align":["right"]},{"label":["ModelBetaC"],"name":[7],"type":["lgl"],"align":["right"]},{"label":["ModelInt"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Function"],"name":[9],"type":["chr"],"align":["left"]},{"label":["Furthest"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["Closest"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["Statistics"],"name":[12],"type":["chr"],"align":["left"]},{"label":["Type"],"name":[13],"type":["chr"],"align":["left"]},{"label":["K"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["ModWeight"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["sample.size"],"name":[16],"type":["int"],"align":["right"]},{"label":["Randomised"],"name":[17],"type":["chr"],"align":["left"]}],"data":[{"1":"-16.66554","2":"44","3":"18","4":"2.300356","5":"0.4853081","6":"NA","7":"NA","8":"177.5048","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.04460104","16":"25","17":"no","_rn_":"1017"},{"1":"-16.30056","2":"43","3":"18","4":"2.207556","5":"0.4854057","6":"NA","7":"NA","8":"179.1969","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.03716133","16":"25","17":"no","_rn_":"972"},{"1":"-16.27774","2":"44","3":"17","4":"2.349050","5":"0.4854653","6":"NA","7":"NA","8":"176.5117","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.03673975","16":"25","17":"no","_rn_":"1018"},{"1":"-16.02761","2":"43","3":"17","4":"2.270356","5":"0.4855939","6":"NA","7":"NA","8":"177.9373","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.03242062","16":"25","17":"no","_rn_":"973"},{"1":"-15.70118","2":"42","3":"18","4":"2.092271","5":"0.4856366","6":"NA","7":"NA","8":"181.3211","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.02753830","16":"25","17":"no","_rn_":"928"},{"1":"-15.51546","2":"44","3":"16","4":"2.373877","5":"0.4856956","6":"NA","7":"NA","8":"176.0014","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.02509621","16":"25","17":"no","_rn_":"1019"},{"1":"-15.48886","2":"42","3":"17","4":"2.161653","5":"0.4858755","6":"NA","7":"NA","8":"179.9379","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.02476466","16":"25","17":"no","_rn_":"929"},{"1":"-15.40742","2":"43","3":"16","4":"2.314008","5":"0.4858264","6":"NA","7":"NA","8":"177.0711","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.02377648","16":"25","17":"no","_rn_":"974"},{"1":"-15.12908","2":"45","3":"18","4":"2.268732","5":"0.4857290","6":"NA","7":"NA","8":"178.1513","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.02068747","16":"25","17":"no","_rn_":"1063"},{"1":"-14.97617","2":"42","3":"16","4":"2.217963","5":"0.4861232","6":"NA","7":"NA","8":"178.8324","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.01916473","16":"25","17":"no","_rn_":"930"},{"1":"-14.72299","2":"44","3":"14","4":"2.434881","5":"0.4859902","6":"NA","7":"NA","8":"174.7917","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.01688595","16":"25","17":"no","_rn_":"1021"},{"1":"-14.69158","2":"43","3":"14","4":"2.388576","5":"0.4861191","6":"NA","7":"NA","8":"175.6057","9":"lin","10":"50","11":"0","12":"mean","13":"relative","14":"0","15":"0.01662287","16":"25","17":"no","_rn_":"976"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

##### Windows plot

It's possible to extract the time windows of all the best supported models (i.e. multi-model inference). This panel shows the opening and closing points of the time windows that were best supported by the data, here those models that made up 95% model confidence set. 

```r
plotwin(wing_50_sw[[1]]$Dataset)
```

![](Figures/plotwin-1.png)<!-- -->


##### Delta plot
The variation in deltaAICc between time windows can be better investigated using the following plot:


```r
plotdelta(
  dataset =
    wing_50_sw[[1]]$Dataset, arrow = TRUE
)
```

![](Figures/plotdelta-1.png)<!-- -->

Warmer areas shows values with the lowest deltaAICc (i.e. "best models"). As explained by van de Pol et al., 2016, these deltaAICc landscapes of the different time windows shows multiple peaks (red areas) instead of a clear single peak. This can indicate the presence of multiple (e.g. possibly both long- and short-lag) weather signals within the same weather variable, but it can also occur due to collinearity or chance. 

The evidence for multiple signals can be therefore investigated by adding the best supported of the weather windows to the baseline model, and re-fitting all the different time windows again: this tests whether there is still strong model support for the second best (e.g. short-lag) weather window once the other best supported (e.g. long-lag) weather window has been accounted for in the baseline model (here in the Step 2).



##### Beta plot
This panel shows the model support (deltaAICc) for all fitted time windows tried, shown for each combination of Window open (y-axis) and Window close (x-axis). Models with the lowest deltaAICc (red) are the best supported (colours show the deltaAICc levels compared to the null model, see legend). Strongly supported windows will often be grouped together. 

```r
plotbetas(wing_50_sw[[1]]$Dataset, arrow = TRUE)
```

![](Figures/plotbetas-1.png)<!-- -->


##### Autocollinearity

Correlation between the mean temperature during the best supported time window and the mean temperature over all other time windows.


```r
autocoll <- autowin(
  reference = wing_50_sw[[1]],
  baseline = wing_50_basemod,
  xvar = list(temp_mean = data_weather$t_daily_mean),
  type = "relative",
  range = c(50, 0),
  stat = "mean",
  cdate = data_weather$date,
  bdate = data_nestlings_wing_50$date_50days,
  func = "quad",
  # cmissing = FALSE,
  cinterval = "day"
)

save(autocoll, file = "Output/wing_50_autocall.rda")
```


```r
load(file = "Output/wing_50_autocall.rda")
plotcor(autocoll, type = "A", arrow = TRUE)
```

![](Figures/plotcorr-1.png)<!-- -->


##### Randwin

Using randwin() to randomize the identity of the nestling, we are able to check if the window that was found before is actually important, or the relationship was just random.


```r
# Performing randamization to identify
# likelyhood of signals occurring by chance

wing_50_rand100 <- randwin(
  repeats = 100,
  baseline = wing_50_basemod,
  xvar = list(temp_mean = data_weather$t_daily_mean),
  stat = c("mean"),
  func = c("lin"),
  type = "relative", # relative to the individual
  range = c(50, 0),
  cinterval = "day",
  cdate = data_weather$date,
  bdate = data_nestlings_wing_50$date_50days,
  cmissing = FALSE
)
save(wing_50_rand100, file = "Output/wing_50_rand100.rda")
```


```r
load("Output/wing_50_rand100.rda")

climwin::pvalue(
  datasetrand = wing_50_rand100[[1]],
  dataset = wing_50_sw[[1]]$Dataset,
  metric = "AIC",
  sample.size = wing_50_sw[[1]]$Dataset$sample.size[1]
  # sample size = number of years
)
```

```
## [1] "<0.001"
```

This panel shows a histogram of all deltaAICc values from all models run on the randomized (in grey) and the deltaAICc of the best model fitted on the observed data (dashed line). The best climate window in the randomized data is by definition a false positive, as the randomization procedure removes any climate signal from the data. The percentage of randomizations that generates a deltaAICc value that is at least as low as the deltaAICc value of the best model fitted to the observed data (P value above) is a measure of how likely it is that one obtained the observed deltaAICc by chance and thus whether the signal is likely to be real or not. In our case we can say that the candidate signal is very unlikely to be a false positive: P <0.001. 

```r
climwin::plothist(
  datasetrand = wing_50_rand100[[1]],
  dataset = wing_50_sw[[1]]$Dataset
)
```

![](Figures/plothist-1.png)<!-- -->


##### Extracting the best window

The best window was obtaining averaging the opening and closing dates for all the best models (deltaAICc < 2). The best window opened 43.25 (mean; SD: 0.97) and closed 16.58 (mean; SD: 1.44) days before the 50 days measurement, which corresponds to the age of the nestling of 7 and 33 days old.


```r
wing_50_bestsw <- singlewin(
  baseline = wing_50_basemod,
  xvar = list(
    t_mean = data_weather$t_daily_mean
  ),
  stat = c("mean"),
  func = c("quad"),
  type = "relative",
  range = c(
    round(mean(wing_50_bestmod$WindowOpen), 0),
    round(mean(wing_50_bestmod$WindowClose), 0)
  ),
  cinterval = "day",
  cdate = data_weather$date,
  bdate = data_nestlings_wing_50$date_50days
)
```

```
## fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
```

```r
# adding the climatic variable to the wing_50 dataset
data_nestlings_wing_50$t_mean <-
  wing_50_bestsw$BestModelData$climate
```



Climatic models, quadratic effects are kept even if not significant, if present in the best model suggested by climwin.


```r
wing_50_climmod <- lmer(
  wing_50 ~
    brood_size +
    hatch_doy_sc +
    t_mean +
    (1 | year_f) + (1 | nestcode_rearing),
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+06)),
  data = data_nestlings_wing_50
)

summary(wing_50_climmod)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: wing_50 ~ brood_size + hatch_doy_sc + t_mean + (1 | year_f) +  
##     (1 | nestcode_rearing)
##    Data: data_nestlings_wing_50
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+06))
## 
## REML criterion at convergence: 12161.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.0597 -0.3787  0.1249  0.5511  2.7386 
## 
## Random effects:
##  Groups           Name        Variance Std.Dev.
##  nestcode_rearing (Intercept) 28.94    5.380   
##  year_f           (Intercept) 35.33    5.944   
##  Residual                     64.19    8.012   
## Number of obs: 1655, groups:  nestcode_rearing, 775; year_f, 25
## 
## Fixed effects:
##              Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)  177.9373     9.7918 207.5763  18.172  < 2e-16 ***
## brood_size    -3.0266     0.4856 927.3471  -6.233 6.95e-10 ***
## hatch_doy_sc  -3.5155     0.4694 658.1356  -7.489 2.24e-13 ***
## t_mean         2.2704     0.5076 216.4781   4.473 1.25e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) brd_sz htch__
## brood_size  -0.140              
## hatch_dy_sc  0.229  0.133       
## t_mean      -0.983  0.009 -0.253
```


### Figure


```r
clim_predict <-
  data.frame(effects::effect("t_mean",
    wing_50_climmod,
    partial.residuals = TRUE
  ))
ggplot(
  clim_predict,
  aes(t_mean, fit)
) +
  geom_point(
    data = data_nestlings_wing_50,
    aes(
      x = t_mean,
      y = wing_50
    ),
    size = 1, shape = 16, alpha = 0.2
  ) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_markdown()
  ) +
  geom_line(linewidth = 1.2) +
  xlab("Mean temperature (°C) between 7 and 33 days of age") +
  ylab("Wing at 50 days (mm)") +
  scale_y_continuous(
    limits = c(150, 240),
    breaks = c(seq(150, 300, 30))
  )
```

<div class="figure">
<img src="Figures/wing-temp-1.png" alt="Fig. 1 - Variation in wing lengths of 50-day-old nestling Alpine swifts in relation to temperature conditions encountered earlier in their development. Climatic windows are reported using the nestling’s age as a reference." width="50%" />
<p class="caption">Fig. 1 - Variation in wing lengths of 50-day-old nestling Alpine swifts in relation to temperature conditions encountered earlier in their development. Climatic windows are reported using the nestling’s age as a reference.</p>
</div>
