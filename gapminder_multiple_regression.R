library(tidyverse)
library(gapminder)

View(gapminder
     )

dat_gapminder <- gapminder::gapminder |>
  janitor::clean_names()

head(dat_gapminder)


dat_gapminder |>
  nest(.by = continent) |>
  mutate(
    lin_mod = map(
      data,
      ~ lm(life_exp ~ year, data = .x)
    )
  )

dat_gapminder |>
  nest(.by = continent) |>
  mutate(
    lin_mod = map(
      data,
      \(x) lm(life_exp ~ year, data = x)
    ),
    coeffs = map(
      lin_mod,
      coefficients
    ),
    slope = map_dbl(coeffs, \(x) x[2]),
    intercept = map_dbl(coeffs, \(x) x[1])
  )
