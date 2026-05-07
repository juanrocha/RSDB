library(tidyverse)
library(patchwork)

dat <- read_csv("assets/rsdb_clean_260219.csv")

a <- dat |> 
    select(creation_date) |> 
    mutate(date = as_datetime(creation_date)) |> 
    mutate(year = year(date)) |> 
    ggplot() + 
    geom_bar(aes(x = year)) +
    labs(tag = "a.", title = "Case studies per year") +
    theme_light(base_size = 7)


b <- dat |> 
    ggplot(aes(y=type)) +
    geom_bar() +
    labs(tag = "b.", title = "Case studies per regime shift",
         y = "Regime shift type", x = "Case study count") +
    theme_light(base_size = 7)

ggsave(
    filename = "assets/cases_update_260219.png", device = "png",
    plot = (a+b), width = 6, height = 3, bg = "white"
)

a+b

a <- dat |> 
    group_by(location_continent_or_ocean) |> 
    summarize(n = n()) |> 
    filter(n > 20) |> 
    arrange(n) |> 
    mutate(location_continent_or_ocean = as_factor(location_continent_or_ocean)) |> 
    ggplot(aes(y = location_continent_or_ocean, x = n)) +
    geom_col()  +
    labs(tag = "a.", title = "Case studies per continent or ocean",
         y = "Location", x = "Case study count",
         caption = "Only places with > 20 cases shown") +
    theme_light(base_size = 7)

b <- dat |> 
    group_by(location_countries) |> 
    summarize(n = n()) |> 
    filter(n > 20) |> 
    arrange(n) |> 
    mutate(location_countries = as_factor(location_countries)) |> 
    ggplot(aes(y = location_countries, x = n)) +
    geom_col()  +
    labs(tag = "b.", title = "Case studies per country",
         y = "Location", x = "Case study count",
         caption = "Only places with > 20 cases shown") +
    theme_light(base_size = 7)


ggsave(
    filename = "assets/cases_bias_260219.png", device = "png",
    plot = (a+b), width = 6, height = 3, bg = "white"
)

dat |> 
    pull(year_or_duration) |> unique()
