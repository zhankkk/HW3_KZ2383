hw3
================

Problem 2

load dataset accel\_data and transpose dataset from wide to long; recode
Monday to Friday as week and Saturday to Sunday as weekend

``` r
  setwd("/Users/zhankeming/Dropbox/Mac/Desktop/columbia/columbia 3rd/data science/hw3")

  accel=read.csv("./data/accel_data.csv")%>%
    janitor::clean_names()%>%
    pivot_longer(
    activity_1:activity_1440,
    names_to = "minute",
    names_prefix = "activity_", 
    values_to = "activity_count"
    ) %>% 
    mutate(
    minute = as.numeric(minute),
    day = factor(day),
    day = fct_relevel(day, "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
    "Friday", "Saturday"),
    weekend = as.numeric(day %in% c("Saturday", "Sunday")),
    weekend=recode(weekend, `1`="weekend", `0`="weekday"))
```

The new data set accel displayed activity count based on week and day
variable. Specifically, weekday and weekend activities are collected for
better comparison. An unusually low activity count is observed for
Saturday as majority counts for each minute is as low as 1.This
distribution may be caused by mistake as it doesn’t seem to be
reasonable. Considering trend for activity counts for the remainng part,
there is no obvious trend without help of graphical analysis.

Display total activity counts per day with each day and week identified.

``` r
accel %>% 
  group_by(day, week) %>% 
  summarize(total_act = sum(activity_count)) %>% 
  pivot_wider(
    names_from = day, 
    values_from = total_act)
```

    ## `summarise()` has grouped output by 'day'. You can override using the `.groups`
    ## argument.

    ## # A tibble: 5 × 8
    ##    week Sunday  Monday Tuesday Wednesday Thursday  Friday Saturday
    ##   <int>  <dbl>   <dbl>   <dbl>     <dbl>    <dbl>   <dbl>    <dbl>
    ## 1     1 631105  78828. 307094.   340115.  355924. 480543.   376254
    ## 2     2 422018 295431  423245    440962   474048  568839    607175
    ## 3     3 467052 685910  381507    468869   371230  467420    382928
    ## 4     4 260617 409450  319568    434460   340291  154049      1440
    ## 5     5 138421 389080  367824    445366   549658  620860      1440

Activity counts generally fluctuates over the week with no apparent
trend. Counts during weekdays are reletively higher than that during
weekend. For week 4 and 5, activity counts for Saturday are unreasonably
low, potentially caused by mistake in collecting counts.

``` r
accel %>% 
  ggplot(aes(x = minute, y = activity_count, group = day_id, color = day)) + 
  geom_line(alpha = .3) + 
  geom_smooth(aes(group = day), se = FALSE)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](hw3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggsave("accel.png")
```

    ## Saving 7 x 5 in image
    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

Activity count is plotted bu each day every week and geom smooth graph
is also added to display a more lucid and continuous trend. Obvious
peaks are detected at Sunday morning and Friday late evening (midnight).
There are few smaller peaks , for instance Saturday late evening
displayed a very slight peak compared with other time of the day.

problem 3

load data set ny\_noaa

``` r
library(p8105.datasets)
data("ny_noaa")
```

clean the data;seperate year,month and day

``` r
ny_noaa = 
  ny_noaa %>% 
  janitor::clean_names()%>%
  separate(date, into = c("year", "month", "day"), convert = TRUE) %>% 
  mutate(
    tmax = as.numeric(tmax),
    tmin = as.numeric(tmin),
    snow = round(snow))


ny_noaa
```

    ## # A tibble: 2,595,176 × 9
    ##    id           year month   day  prcp  snow  snwd  tmax  tmin
    ##    <chr>       <int> <int> <int> <int> <dbl> <int> <dbl> <dbl>
    ##  1 US1NYAB0001  2007    11     1    NA    NA    NA    NA    NA
    ##  2 US1NYAB0001  2007    11     2    NA    NA    NA    NA    NA
    ##  3 US1NYAB0001  2007    11     3    NA    NA    NA    NA    NA
    ##  4 US1NYAB0001  2007    11     4    NA    NA    NA    NA    NA
    ##  5 US1NYAB0001  2007    11     5    NA    NA    NA    NA    NA
    ##  6 US1NYAB0001  2007    11     6    NA    NA    NA    NA    NA
    ##  7 US1NYAB0001  2007    11     7    NA    NA    NA    NA    NA
    ##  8 US1NYAB0001  2007    11     8    NA    NA    NA    NA    NA
    ##  9 US1NYAB0001  2007    11     9    NA    NA    NA    NA    NA
    ## 10 US1NYAB0001  2007    11    10    NA    NA    NA    NA    NA
    ## # … with 2,595,166 more rows

Snowfall is rounded to the nearest integer and the most commonly
observed value is 0 as it generally only snows in winter when
temperature is low.

Make a two-panel plot showing the average max temperature in January and
in July in each station across years. Identify observable structure and
existing outlier

``` r
ny_noaa %>% 
  group_by(id, year, month) %>% 
  filter(month %in% c(1, 7)) %>% 
  summarize(mean_tmax = mean(tmax, na.rm = TRUE, color = id)) %>% 
  ggplot(aes(x = year, y = mean_tmax, group = id)) +
  geom_point() +geom_path() +
  facet_grid(~month) +
  labs(title = "Average maximum temperature for January and July")
```

    ## `summarise()` has grouped output by 'id', 'year'. You can override using the
    ## `.groups` argument.

    ## Warning: Removed 5970 rows containing missing values (geom_point).

    ## Warning: Removed 5931 row(s) containing missing values (geom_path).

![](hw3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggsave("ny_noaa.png")
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 5970 rows containing missing values (geom_point).
    ## Removed 5931 row(s) containing missing values (geom_path).

Mean maximum temperature in January is much lower compared with that of
July. For each month, tmax fluctuates within similar range with several
observable outlier. For January, 1982 and 2005 had a relatively low
temperature while the relatively low temperature for July is in year
1988. Despite these obviously different values, other values
approximately fall within similar range for both January and July.

Make a two-panel plot showing (i) tmax vs tmin for the full data set and
(ii) make a plot showing the distribution of snowfall values greater
than 0 and less than 100 separately by year.

``` r
hex = 
  ny_noaa %>% 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_hex()

ridge = 
  ny_noaa %>% 
  filter(snow < 100, snow > 0) %>%
  ggplot(aes(x = snow, y = as.factor(year))) + geom_density_ridges()

hex + ridge
```

    ## Warning: Removed 1136276 rows containing non-finite values (stat_binhex).

    ## Picking joint bandwidth of 3.76

![](hw3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("hex_ridge.png")
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 1136276 rows containing non-finite values (stat_binhex).

    ## Picking joint bandwidth of 3.76

Hexagon plot is applied for showing distribution of tmax and tmin with
potential density of data distribution indicated;for ridge plot, it
lucidly illustrates change in snowfall values during each year in a
continous manner.

For the hexagon plot there is some variability while the majority of the
data gather in the center of the distribution. A large proportion of
tmax seem to be less than tmin and this is a bit confusing and could be
caused by data recording error.

For the ridge plot, there is a multimodal density of snowfall within a
given year with around 4 peaks. Most stations see between 0 and 35 mm of
snow in a year. Most common snowfall values are around 35 while very few
of them are as high as 80 for most stations.
