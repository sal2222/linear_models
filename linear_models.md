Linear Models
================

Examples
--------

AirBnB

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(boro = neighbourhood_group,
         neighborhood = neighbourhood) %>% 
  filter(boro != "Staten Island") %>% 
  select(price, stars, boro, neighborhood, room_type)
```

Fit a model for price as an outcome

``` r

fit = lm(price ~ stars + boro, data = nyc_airbnb)
```

Look at summaries

``` r
summary(fit)
## 
## Call:
## lm(formula = price ~ stars + boro, data = nyc_airbnb)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -169.8  -64.0  -29.0   20.2 9870.0 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    -70.414     14.021  -5.022 5.14e-07 ***
## stars           31.990      2.527  12.657  < 2e-16 ***
## boroBrooklyn    40.500      8.559   4.732 2.23e-06 ***
## boroManhattan   90.254      8.567  10.534  < 2e-16 ***
## boroQueens      13.206      9.065   1.457    0.145    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 181.5 on 30525 degrees of freedom
##   (9962 observations deleted due to missingness)
## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16
summary(fit)$coef
##                Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)   -70.41446  14.020697 -5.022180 5.137589e-07
## stars          31.98989   2.527500 12.656733 1.269392e-36
## boroBrooklyn   40.50030   8.558724  4.732049 2.232595e-06
## boroManhattan  90.25393   8.567490 10.534465 6.638618e-26
## boroQueens     13.20617   9.064879  1.456850 1.451682e-01
coef(fit)
##   (Intercept)         stars  boroBrooklyn boroManhattan    boroQueens 
##     -70.41446      31.98989      40.50030      90.25393      13.20617
# fitted.values(fit)
```

Look at better summaries...

``` r

fit %>% 
  broom::tidy()
## # A tibble: 5 x 5
##   term          estimate std.error statistic  p.value
##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)      -70.4     14.0      -5.02 5.14e- 7
## 2 stars             32.0      2.53     12.7  1.27e-36
## 3 boroBrooklyn      40.5      8.56      4.73 2.23e- 6
## 4 boroManhattan     90.3      8.57     10.5  6.64e-26
## 5 boroQueens        13.2      9.06      1.46 1.45e- 1

fit %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value) %>% 
  mutate(term = str_replace(term, "^boro", "Boro: ")) %>% 
  knitr::kable(digits = 3)
```

| term            |  estimate|  p.value|
|:----------------|---------:|--------:|
| (Intercept)     |   -70.414|    0.000|
| stars           |    31.990|    0.000|
| Boro: Brooklyn  |    40.500|    0.000|
| Boro: Manhattan |    90.254|    0.000|
| Boro: Queens    |    13.206|    0.145|

Look at other summaries...

``` r
fit %>% 
  broom::glance()
## # A tibble: 1 x 11
##   r.squared adj.r.squared sigma statistic   p.value    df  logLik    AIC
## *     <dbl>         <dbl> <dbl>     <dbl>     <dbl> <int>   <dbl>  <dbl>
## 1    0.0342        0.0341  182.      271. 6.73e-229     5 -2.02e5 4.04e5
## # ... with 3 more variables: BIC <dbl>, deviance <dbl>, df.residual <int>

fit %>% 
  broom::glance() %>% 
  select(r.squared, adj.r.squared)
## # A tibble: 1 x 2
##   r.squared adj.r.squared
## *     <dbl>         <dbl>
## 1    0.0342        0.0341
```

Be carefu with factor order...

``` r
nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(boro = fct_infreq(boro),  # in order of frequency
         room_type = fct_infreq(room_type))

fit = lm(price ~ stars + boro, data = nyc_airbnb)

fit %>% 
  broom::tidy()
## # A tibble: 5 x 5
##   term         estimate std.error statistic   p.value
##   <chr>           <dbl>     <dbl>     <dbl>     <dbl>
## 1 (Intercept)      19.8     12.2       1.63 1.04e-  1
## 2 stars            32.0      2.53     12.7  1.27e- 36
## 3 boroBrooklyn    -49.8      2.23    -22.3  6.32e-109
## 4 boroQueens      -77.0      3.73    -20.7  2.58e- 94
## 5 boroBronx       -90.3      8.57    -10.5  6.64e- 26
```

Diagnostics
-----------

Add residuals to dataset

``` r
modelr::add_residuals(nyc_airbnb, fit)
## # A tibble: 40,492 x 6
##    price stars boro  neighborhood room_type        resid
##    <int> <dbl> <fct> <chr>        <fct>            <dbl>
##  1    99   5   Bronx City Island  Private room      9.47
##  2   200  NA   Bronx City Island  Private room     NA   
##  3   300  NA   Bronx City Island  Entire home/apt  NA   
##  4   125   5   Bronx City Island  Entire home/apt  35.5 
##  5    69   5   Bronx City Island  Private room    -20.5 
##  6   125   5   Bronx City Island  Entire home/apt  35.5 
##  7    85   5   Bronx City Island  Entire home/apt  -4.53
##  8    39   4.5 Bronx Allerton     Private room    -34.5 
##  9    95   5   Bronx Allerton     Entire home/apt   5.47
## 10   125   4.5 Bronx Allerton     Entire home/apt  51.5 
## # ... with 40,482 more rows

modelr::add_predictions(nyc_airbnb, fit)
## # A tibble: 40,492 x 6
##    price stars boro  neighborhood room_type        pred
##    <int> <dbl> <fct> <chr>        <fct>           <dbl>
##  1    99   5   Bronx City Island  Private room     89.5
##  2   200  NA   Bronx City Island  Private room     NA  
##  3   300  NA   Bronx City Island  Entire home/apt  NA  
##  4   125   5   Bronx City Island  Entire home/apt  89.5
##  5    69   5   Bronx City Island  Private room     89.5
##  6   125   5   Bronx City Island  Entire home/apt  89.5
##  7    85   5   Bronx City Island  Entire home/apt  89.5
##  8    39   4.5 Bronx Allerton     Private room     73.5
##  9    95   5   Bronx Allerton     Entire home/apt  89.5
## 10   125   4.5 Bronx Allerton     Entire home/apt  73.5
## # ... with 40,482 more rows


modelr::add_residuals(nyc_airbnb, fit) %>% 
  ggplot(aes(x = boro, y = resid)) + geom_boxplot()
## Warning: Removed 9962 rows containing non-finite values (stat_boxplot).
```

<img src="linear_models_files/figure-markdown_github/unnamed-chunk-8-1.png" width="90%" />

``` r

modelr::add_residuals(nyc_airbnb, fit) %>% 
  ggplot(aes(x = stars, y = resid)) + geom_point()
## Warning: Removed 9962 rows containing missing values (geom_point).
```

<img src="linear_models_files/figure-markdown_github/unnamed-chunk-8-2.png" width="90%" />

``` r

nyc_airbnb %>% 
  modelr::add_predictions(fit) %>% 
  ggplot(aes(x = stars, y = pred, color = boro)) + geom_point()
## Warning: Removed 9962 rows containing missing values (geom_point).
```

<img src="linear_models_files/figure-markdown_github/unnamed-chunk-8-3.png" width="90%" />
