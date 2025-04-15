viz_part2
================
Ze Li
2023-10-03

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(patchwork)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

#theme_set(theme_minimal() + theme(legend.position = "bottom"))

#options(
#  ggplot2.continuous.colour = "viridis",
#  ggplot2.continuous.fill = "viridis"
#)

#scale_colour_discrete = scale_colour_viridis_d
#scale_fill_discrete = scale_fill_viridis_d
```

Get the data for plotting today.

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2023-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## using cached file: /Users/zeze/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-10-09 17:57:46.959077 (0.343)

    ## file min/max dates: 2021-01-01 / 2023-10-31

    ## using cached file: /Users/zeze/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2023-10-09 17:57:47.880737 (0.282)

    ## file min/max dates: 2021-01-01 / 2023-10-31

    ## using cached file: /Users/zeze/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-10-09 17:57:48.352621 (0.122)

    ## file min/max dates: 2021-01-01 / 2023-10-31

This results in a dataframe with 3102 observations on six variables.

## Same plot from last time

### rename variables & scale

``` r
weather_df |>
  #filter(tmax >= 20, tmax <= 30) |>
  ggplot(aes(x = tmin, y = tmax, color = name))+
  geom_point(alpha= .5) +
  labs(
    title = "Temperature plot",
    x = "Min daily temp (Degrees C)",
    y = "Max daily temp (Degrees C)",
    color = "Location",
    caption = "Max vs Min daily temp in three locations; data from rnoaa"
  ) +
  scale_x_continuous(
    breaks = c(-15,0,15),
    labels = c("-15", "0", "15")
  ) +
  scale_y_continuous(
    position = "right",
    trans = "sqrt", # transformation
    #limits = c(20,30)
  )
```

    ## Warning in self$trans$transform(x): NaNs produced

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Removed 259 rows containing missing values (`geom_point()`).

<img src="viz_part2_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

### colors

what about colors…

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax, color = name))+
  geom_point(alpha= .5) +
  labs(
    title = "Temperature plot",
    x = "Min daily temp (Degrees C)",
    y = "Max daily temp (Degrees C)",
    color = "Location",
    caption = "Max vs Min daily temp in three locations; data from rnoaa"
  ) +
  #scale_color_hue(h = c(150,200))
  viridis::scale_color_viridis(discrete = TRUE)
```

    ## Warning: Removed 93 rows containing missing values (`geom_point()`).

<img src="viz_part2_files/figure-gfm/scale_color_hue; viridis-1.png" width="90%" />

## Themes

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax, color = name))+
  geom_point(alpha= .5) +
  labs(
    title = "Temperature plot",
    x = "Min daily temp (Degrees C)",
    y = "Max daily temp (Degrees C)",
    color = "Location",
    caption = "Max vs Min daily temp in three locations; data from rnoaa"
  ) +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme_minimal() + # bw & without box lines
  #theme_classic() + # no grid line (recommend with grid line)
  #theme_bw() +
  theme(legend.position = "bottom") # legend bottom should be lower than bw (bw will reset)
```

    ## Warning: Removed 93 rows containing missing values (`geom_point()`).

<img src="viz_part2_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

## data arguments …

``` r
weather_df |>
  ggplot(aes(x = date, y = tmax, color = name)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 93 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 93 rows containing missing values (`geom_point()`).

<img src="viz_part2_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
nyc_weather_df = 
  weather_df |>
  filter(name == "CentralPark_NY")

hawaii_weather_df = 
  weather_df |>
  filter(name == "Molokai_HI")

ggplot(nyc_weather_df, aes(x = date, y = tmax, color = name)) +
  geom_point() +
  geom_line(data = hawaii_weather_df)
```

    ## Warning: Removed 25 rows containing missing values (`geom_point()`).

    ## Warning: Removed 25 rows containing missing values (`geom_line()`).

<img src="viz_part2_files/figure-gfm/unnamed-chunk-5-2.png" width="90%" />

## `patchwork`

``` r
weather_df |>
  ggplot(aes(x = date, y = tmax, color = name)) +
  geom_point() +
  facet_grid(. ~ name)
```

    ## Warning: Removed 93 rows containing missing values (`geom_point()`).

<img src="viz_part2_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
ggp_temp_scatter =
  weather_df |>
  ggplot(aes(x = tmin, y = tmax, color = name)) +
  geom_point(alpha = .5) +
  theme(legend.position = "none")

ggp_prcp_density = 
  weather_df |>
  filter(prcp > 25) |>
  ggplot(aes(x = prcp, fill = name)) +
  geom_density(alpha = .5) +
  theme(legend.position = "none")

ggp_tmax_date = 
  weather_df |>
  ggplot(aes(x = date, y = tmax, color = name)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom")

(ggp_temp_scatter + ggp_prcp_density) / ggp_tmax_date
```

    ## Warning: Removed 93 rows containing missing values (`geom_point()`).

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 93 rows containing non-finite values (`stat_smooth()`).
    ## Removed 93 rows containing missing values (`geom_point()`).

<img src="viz_part2_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

## data manipulation

``` r
weather_df |>
  mutate(
    name = fct_relevel(name, c("Molokai_HI","CentralPark_NY","Waterhole_WA"))
  ) |>
  ggplot(aes(x = name, y = tmax)) +
  geom_boxplot()
```

    ## Warning: Removed 93 rows containing non-finite values (`stat_boxplot()`).

<img src="viz_part2_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

``` r
weather_df |>
  mutate(
    name = fct_reorder(name, tmax)
  ) |>
  ggplot(aes(x = name, y = tmax, fill = name)) +
  geom_violin()
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `name = fct_reorder(name, tmax)`.
    ## Caused by warning:
    ## ! `fct_reorder()` removing 93 missing values.
    ## ℹ Use `.na_rm = TRUE` to silence this message.
    ## ℹ Use `.na_rm = FALSE` to preserve NAs.

    ## Warning: Removed 93 rows containing non-finite values (`stat_ydensity()`).

<img src="viz_part2_files/figure-gfm/unnamed-chunk-8-2.png" width="90%" />

## complicate FAS plot

``` r
litter_data = 
  read_csv("data/FAS_litters.csv") |>
  janitor::clean_names() |>
  separate(group, into = c("dose", "day_of_tx"), sep = 3)
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pup_data = 
  read_csv("data/FAS_pups.csv") |>
  janitor::clean_names() #|>
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
  #mutate(
  #  sex = 
  #    case_match(
  #      sex, 
  #      1 ~ "male", 
  #      2 ~ "female"))

fas_data = 
  left_join(pup_data, litter_data, by = "litter_number") 

fas_data |> 
  select(dose, day_of_tx, starts_with("pd")) |> 
  pivot_longer(
    pd_ears:pd_walk,
    names_to = "outcome", 
    values_to = "pn_day") |> 
  drop_na() |> 
  mutate(outcome = fct_reorder(outcome, pn_day)) |> 
  ggplot(aes(x = dose, y = pn_day)) + 
  geom_violin() + 
  facet_grid(day_of_tx ~ outcome)
```

<img src="viz_part2_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />
