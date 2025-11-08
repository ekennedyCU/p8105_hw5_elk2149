Homework 5
================
Evan Kennedy, ELK2149
2025-11-8

***Problem 1***

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)

birthdays = sample(1:365, 5, replace = TRUE)

repeated_bday = length(unique(birthdays)) < 5

repeated_bday
```

    ## [1] FALSE

put this in a function

``` r
bday_sim = function(n_room) {

    birthdays = sample(1:365, n_room, replace = TRUE)

    repeated_bday = length(unique(birthdays)) < n_room

    repeated_bday
    
}

bday_sim(5)
```

    ## [1] FALSE

``` r
bday_sim_results = 
  expand_grid(
    bdays = 2:50,
    iter = 1:10000
  ) |> 
  mutate(
    result = map_lgl(bdays, bday_sim)
  ) |> 
  group_by(
    bdays
    ) |> 
  summarize(
    prob_repeat = mean(result)
  )
  
bday_sim_results
```

    ## # A tibble: 49 × 2
    ##    bdays prob_repeat
    ##    <int>       <dbl>
    ##  1     2      0.0027
    ##  2     3      0.0074
    ##  3     4      0.0168
    ##  4     5      0.0257
    ##  5     6      0.0384
    ##  6     7      0.055 
    ##  7     8      0.0742
    ##  8     9      0.0938
    ##  9    10      0.118 
    ## 10    11      0.142 
    ## # ℹ 39 more rows

\#plot this

``` r
bday_sim_results |> 
  ggplot(aes(x = bdays, y = prob_repeat)) +
  geom_point() +
  geom_line()
```

![](Homework-5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#the probability of having two people with the same birthday does not
increase at a linear pace as the group size increased from 2 to 50
people.

***Problem 2***

``` r
n = 30
sd = 5
u = 0
a = 0.05

run_one = function(u, n, sd) {
  x = rnorm(n, mean = u, sd = sd)
  t_test = t.test(x, mu = 0)
  tibble(
    sample_mean  = mean(x),
    p_value = tidy(t_test)$p.value
  )
}

simulation = 
  expand_grid(iter = 1:5000) |> 
  mutate(out = map(iter, ~ run_one(u = u, n = n, sd = sd))) |>
  unnest(out)


estimated_type1_error <-
  simulation |>
  summarize(type1 = mean(p_value < a)) |>
  pull(type1)

estimated_type1_error
```

    ## [1] 0.0466

``` r
simulation_1_6 = 
  expand_grid(
    u = 1:6,
    iter = 1:5000
  ) |>
  mutate(out = map(u, ~ run_one(u = .x, n = n, sd = sd))) |>
  unnest(out)

power_results = 
  simulation_1_6 |>
  mutate(reject = p_value < a) |>
  group_by(u) |>
  summarize(
    power = mean(reject)
  )

power_results
```

    ## # A tibble: 6 × 2
    ##       u power
    ##   <int> <dbl>
    ## 1     1 0.182
    ## 2     2 0.554
    ## 3     3 0.891
    ## 4     4 0.990
    ## 5     5 0.999
    ## 6     6 1
