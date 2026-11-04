library(readxl)
library(dplyr)
library(broom)
library(car)


raw <- read_excel("Downloads/Survey Penggunaan Fitur \"Add yours\" Instagram dalam Konteks Isu Nasional-report.xlsx",
                  sheet = "Coding")


View(raw)


analysis <- data.frame(
  source = factor(
    as.character(raw[[18]]),
    levels = c("1", "2", "3", "4"),
    labels = c("Close_friend", "Weak_tie", "Media_organisation", "Influencer_KOL")
  ),
  age = as.numeric(raw[[4]]),
  gender = factor(as.character(raw[[3]]), levels = c("Laki-Laki", "Perempuan")),
  ses = factor(as.character(raw[[7]]), levels = c("Lower", "Middle", "Upper"))
)


table(analysis$source)

Close_friend           Weak_tie Media_organisation     Influencer_KOL 
57                 89                113                 41 


levels(analysis$source)
[1] "Close_friend"       "Weak_tie"           "Media_organisation"
[4] "Influencer_KOL"    
contrasts(analysis$source)
Weak_tie Media_organisation Influencer_KOL
Close_friend              0                  0              0
Weak_tie                  1                  0              0
Media_organisation        0                  1              0
Influencer_KOL            0                  0              1


analysis$seek_information <- ifelse(is.na(raw[[22]]), 0, 1)

analysis$discussed <- ifelse(is.na(raw[[23]]), 0, 1)

analysis$repost <- ifelse(is.na(raw[[24]]), 0, 1)

analysis$demonstration <- ifelse(is.na(raw[[28]]), 0, 1)

analysis$petition <- ifelse(is.na(raw[[29]]), 0, 1)

analysis$contacted_official <- ifelse(is.na(raw[[30]]), 0, 1)

analysis$changed_attitude <- ifelse(is.na(raw[[31]]), 0, 1)

analysis$encouraged_others <- ifelse(is.na(raw[[32]]), 0, 1)


table(analysis$seek_information)

0   1 
44 256 

table(analysis$changed_attitude)

0   1 
120 180 

table(analysis$discussed)

0   1 
138 162 

table(analysis$repost)

0   1 
178 122 

table(analysis$petition)

0   1 
167 133 

table(analysis$encouraged_others)

0   1 
194 106 

table(analysis$contacted_official)

0   1 
226  74 

table(analysis$demonstration)

0   1 
222  78 


model_seek <- glm(
  seek_information ~ source + age + gender + ses,
  data = analysis,
  family = binomial
)

model_attitude <- glm(
  changed_attitude ~ source + age + gender + ses,
  data = analysis,
  family = binomial
)

model_discussed <- glm(
  discussed ~ source + age + gender + ses,
  data = analysis,
  family = binomial
)

model_repost <- glm(
  repost ~ source + age + gender + ses,
  data = analysis,
  family = binomial
)

model_petition <- glm(
  petition ~ source + age + gender + ses,
  data = analysis,
  family = binomial
)

model_encouraged <- glm(
  encouraged_others ~ source + age + gender + ses,
  data = analysis,
  family = binomial
)

model_contact <- glm(
  contacted_official ~ source + age + gender + ses,
  data = analysis,
  family = binomial
)

model_demonstration <- glm(
  demonstration ~ source + age + gender + ses,
  data = analysis,
  family = binomial
)

tidy(
  model_seek,
  exponentiate = TRUE,
  conf.int = TRUE
)
# A tibble: 8 × 7
term               estimate std.error statistic p.value conf.low conf.high
<chr>                 <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
  1 (Intercept)           7.65     1.31       1.55    0.120   0.636     111.  
2 sourceWeak_tie        1.46     0.478      0.794   0.427   0.564       3.76
3 sourceMedia_organ…    1.18     0.442      0.383   0.702   0.484       2.78
4 sourceInfluencer_…    1.15     0.567      0.253   0.801   0.386       3.70
5 age                   1.00     0.0416     0.104   0.917   0.924       1.09
6 genderPerempuan       1.06     0.332      0.184   0.854   0.551       2.03
7 sesMiddle             0.408    0.659     -1.36    0.173   0.0908      1.31
8 sesUpper              0.596    0.666     -0.777   0.437   0.132       1.95

tidy(
  model_attitude,
  exponentiate = TRUE,
  conf.int = TRUE
)
# A tibble: 8 × 7
term               estimate std.error statistic p.value conf.low conf.high
<chr>                 <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
  1 (Intercept)           0.135    0.935     -2.14  0.0323    0.0211     0.835
2 sourceWeak_tie        0.770    0.355     -0.737 0.461     0.382      1.54 
3 sourceMedia_organ…    1.34     0.347      0.833 0.405     0.673      2.64 
4 sourceInfluencer_…    1.17     0.434      0.358 0.720     0.501      2.76 
5 age                   1.08     0.0309     2.61  0.00905   1.02       1.15 
6 genderPerempuan       1.17     0.246      0.626 0.531     0.720      1.89 
7 sesMiddle             0.698    0.402     -0.893 0.372     0.314      1.53 
8 sesUpper              1.27     0.399      0.609 0.542     0.576      2.78 

tidy(
  model_discussed,
  exponentiate = TRUE,
  conf.int = TRUE
)
# A tibble: 8 × 7
term               estimate std.error statistic p.value conf.low conf.high
<chr>                 <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
  1 (Intercept)           0.449    0.899   -0.891     0.373   0.0760      2.61
2 sourceWeak_tie        0.759    0.344   -0.801     0.423   0.384       1.49
3 sourceMedia_organ…    1.00     0.332    0.00602   0.995   0.521       1.92
4 sourceInfluencer_…    1.11     0.418    0.255     0.799   0.491       2.54
5 age                   1.03     0.0296   1.07      0.286   0.974       1.09
6 genderPerempuan       1.13     0.236    0.517     0.605   0.711       1.79
7 sesMiddle             0.960    0.393   -0.104     0.917   0.443       2.08
8 sesUpper              1.16     0.387    0.385     0.700   0.540       2.49

tidy(
  model_repost,
  exponentiate = TRUE,
  conf.int = TRUE
)
# A tibble: 8 × 7
term               estimate std.error statistic p.value conf.low conf.high
<chr>                 <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
  1 (Intercept)          0.0296    1.02      -3.44  5.76e-4  0.00375     0.209
2 sourceWeak_tie       2.29      0.361      2.30  2.17e-2  1.14        4.70 
3 sourceMedia_organ…   1.11      0.349      0.310 7.56e-1  0.565       2.23 
4 sourceInfluencer_…   1.44      0.432      0.843 3.99e-1  0.616       3.38 
5 age                  1.05      0.0315     1.67  9.54e-2  0.991       1.12 
6 genderPerempuan      1.19      0.247      0.720 4.72e-1  0.737       1.94 
7 sesMiddle            3.19      0.499      2.32  2.01e-2  1.27        9.24 
8 sesUpper             4.21      0.492      2.92  3.47e-3  1.70       12.0  

tidy(
  model_petition,
  exponentiate = TRUE,
  conf.int = TRUE
)
# A tibble: 8 × 7
term               estimate std.error statistic p.value conf.low conf.high
<chr>                 <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
  1 (Intercept)           0.225    0.921    -1.62     0.106   0.0361      1.35
2 sourceWeak_tie        1.15     0.345     0.415    0.678   0.587       2.28
3 sourceMedia_organ…    0.750    0.334    -0.862    0.388   0.389       1.45
4 sourceInfluencer_…    1.15     0.416     0.334    0.738   0.508       2.60
5 age                   1.05     0.0303    1.49     0.136   0.986       1.11
6 genderPerempuan       0.805    0.238    -0.916    0.360   0.504       1.28
7 sesMiddle             1.03     0.403     0.0742   0.941   0.470       2.31
8 sesUpper              1.28     0.396     0.624    0.533   0.593       2.83

tidy(
  model_encouraged,
  exponentiate = TRUE,
  conf.int = TRUE
)
# A tibble: 8 × 7
term               estimate std.error statistic p.value conf.low conf.high
<chr>                 <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
  1 (Intercept)           0.305    0.977     -1.22   0.224    0.0433     2.02 
2 sourceWeak_tie        0.630    0.351     -1.32   0.188    0.316      1.25 
3 sourceMedia_organ…    0.415    0.345     -2.55   0.0108   0.210      0.815
4 sourceInfluencer_…    0.888    0.417     -0.286  0.775    0.390      2.01 
5 age                   1.02     0.0317     0.614  0.539    0.959      1.09 
6 genderPerempuan       0.894    0.250     -0.448  0.654    0.548      1.46 
7 sesMiddle             1.59     0.460      1.00   0.315    0.665      4.12 
8 sesUpper              2.07     0.450      1.62   0.106    0.888      5.29 

tidy(
  model_contact,
  exponentiate = TRUE,
  conf.int = TRUE
)
# A tibble: 8 × 7
term               estimate std.error statistic p.value conf.low conf.high
<chr>                 <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
  1 (Intercept)           0.340    1.05     -1.03    0.305    0.0416     2.59 
2 sourceWeak_tie        0.965    0.384    -0.0926  0.926    0.456      2.07 
3 sourceMedia_organ…    0.910    0.370    -0.254   0.799    0.444      1.91 
4 sourceInfluencer_…    0.340    0.565    -1.91    0.0562   0.102      0.974
5 age                   1.00     0.0347    0.0661  0.947    0.937      1.07 
6 genderPerempuan       0.858    0.273    -0.559   0.576    0.502      1.47 
7 sesMiddle             0.867    0.477    -0.299   0.765    0.350      2.31 
8 sesUpper              1.42     0.461     0.761   0.447    0.595      3.70 

tidy(
  model_demonstration,
  exponentiate = TRUE,
  conf.int = TRUE
)
# A tibble: 8 × 7
term               estimate std.error statistic p.value conf.low conf.high
<chr>                 <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
  1 (Intercept)           0.335    1.05      -1.04   0.298    0.0408     2.56 
2 sourceWeak_tie        0.619    0.372     -1.29   0.197    0.298      1.29 
3 sourceMedia_organ…    0.505    0.361     -1.89   0.0582   0.248      1.03 
4 sourceInfluencer_…    0.287    0.527     -2.37   0.0178   0.0947     0.769
5 age                   1.03     0.0349     0.768  0.443    0.960      1.10 
6 genderPerempuan       0.542    0.271     -2.26   0.0240   0.317      0.920
7 sesMiddle             1.13     0.472      0.266  0.790    0.463      3.00 
8 sesUpper              1.20     0.466      0.388  0.698    0.496      3.15 