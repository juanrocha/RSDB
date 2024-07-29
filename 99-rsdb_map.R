library(tidyverse)
library(leaflet)
library(htmlwidgets)


## Most recent file:
dat <- read_csv("~/Documents/Projects/regimeshifts/rsdb/RSDB.csv")

### Old version of cases from research assisstants [J240711]
dat0 <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1tpUh0D04kK5aQgDvWs4zDu5nHJvBcEsjPMfF36WMZFE/edit#gid=0",
    sheet = 3, col_types = "c") |> # Carla and others
    janitor::clean_names()

dat1 <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1YWjfGpQQgLBkahMM1MBzRH6EBpaJvH24i7RbMJFE0LQ/edit#gid=0",
    sheet = 1, col_types = "c") |> # Romain
    janitor::clean_names()

dat2 <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1YWjfGpQQgLBkahMM1MBzRH6EBpaJvH24i7RbMJFE0LQ/edit#gid=0",
    sheet = 2, col_types = "c") |> # Rodrigo
    janitor::clean_names()

dat3 <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1YWjfGpQQgLBkahMM1MBzRH6EBpaJvH24i7RbMJFE0LQ/edit#gid=0",
    sheet = 3, col_types = "c") |> # Mathis
    janitor::clean_names()

dat4 <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1YWjfGpQQgLBkahMM1MBzRH6EBpaJvH24i7RbMJFE0LQ/edit#gid=0",
    sheet = 4, col_types = "c") |> # Jonas
    janitor::clean_names()

dat5 <- read_csv("~/Documents/Projects/regimeshifts/rsdb-scripts/new_cases/2024_05_13_tree_die_off/tree_die_off_cases.csv", col_types = "ccccc") |> janitor::clean_names()

## alternatively
load("assets/cases_db.Rda") # Old version

# dat <- read_csv("~/Downloads/RSDB_cases_210917 - Sheet1.csv") |> 
#     janitor::clean_names()

# dat <- readxl::read_xlsx("~/Downloads/RSDB_cases_210917.xlsx") |> 
#     janitor::clean_names()

# dat |> select(longitude, latitude)
# skimr::skim(dat)
# 
# dat <- dat |>
#     filter(!is.na(id)) 

# Now working
names(dat5) %in% names(dat1) |> all()
# 
# key_cols <- c("type_of_regime_shift", "longitude", "latitude")
# 
dat <- dat0  |> 
    bind_rows(dat1 |> select(-x47) ) |>
    bind_rows(dat2 ) |>
    bind_rows(dat3 ) |> 
    bind_rows( dat5 |> mutate(across(where(is.double), as.character)) )
 
coord_rs <- read.csv2(
    file = '~/Documents/Projects_old/Cascading Effects/data/case_coords.csv', dec = ".") |> 
    as_tibble()

rs_types <- read.csv(
    file = "~/Documents/Projects_old/Cascading Effects/data/rs_coords_paper.csv", dec = ".") |>
    as_tibble() |> 
    rename(type = name, long = x, lat = y) |> select(-X) |> 
    mutate(type = str_to_sentence(type)) |> 
    mutate(type = case_when(
        type == "Desertification" ~ "Dryland degradation",
        TRUE ~ type
    ))

rs_types

all(coord_rs$place %in% dat$case_study_name)

dat <- dat |> 
    left_join(coord_rs, by = c("case_study_name" = "place")) |> 
    mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude),
           lon = as.numeric(lon), lat = as.numeric(lat)) |> 
    #select(case_study_name, longitude,latitude, lon, lat) |> 
    mutate(
        longitude = case_when(
            is.na(longitude) ~ lon,
            TRUE ~ longitude
        ),
        latitude = case_when(
            is.na(latitude) ~ lat, 
            TRUE ~ latitude
        )
    ) |> select(-lon, -lat) |> 
    rename(long = longitude, lat = latitude)

world <- ggplot(map_data("world"), aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
                 fill = "#f9f9f9", linewidth = 0.1) +
    #coord_map(projection = "mercator" ) #
    coord_quickmap() 

lvls <- levels(as.factor(dat$regime_shift_type_value))

## Correct names: 
dat <- dat |> rename(type = regime_shift_type_value) |>
    mutate(type = case_when(
        type == "Coniferous to deciduous boreal forest" ~ "Coniferous to deciduous forest",
        type == "Fisheries collapse, Marine food webs" ~ "Fisheries collapse",
        type == "Forest to savannas" ~ "Forest to savanna",
        type == "Kelp transitions" ~ "Kelps transitions",
        type == "Mangrove transitions" ~ "Mangroves transitions",
        type == "Costal marine eutrophication" ~ "Marine eutrophication" ,
        type == "Marine food webs" ~ "Marine foodwebs",
        type == "Thermokarst lake to terrestrial ecosystem" ~ "Thermokarst lakes",
        type == "Marine food webs" ~ "Marine foodwebs",
        type == "River channel position" ~ "River channel change",
        type == "Desertification" ~ "Dryland degradation",
        type == "Proposed & new type" ~  "Unclassified",
        type == "Unknown" ~ "Unclassified",
        type == "To be determined" ~ "Unclassified",
        is.na(type) ~ "Unclassified",
        TRUE ~ type
    )) |>
    mutate(type = str_to_sentence(type))


world + 
    geom_point(data = rs_types, aes(x = long, y = lat, color = type),
               size = 4, alpha = 0.5, show.legend = TRUE) +
    geom_point(
        data = dat |> select(type , long, lat ), 
               aes(x = long, y = lat, color = type), 
        alpha = 0.4, size = 0.5, show.legend = TRUE) +
    scale_colour_hue(
        "Regime shift type", guide = guide_legend(title.position = "top")) +
    labs(title = "Regime shifts documented in the world",
         subtitle = "Big dots are regime shift types, small dots ~3000 case studies",
         caption = "Data source: The regime shifts database (www.regimeshifts.org)") +
    theme_void(base_size = 12) +
    theme(legend.position = "bottom")

# 
ggsave(
    file = "rsdb_map_240717.png", device = "png", width = 12, height = 9,
    bg = "white", dpi = 500
)

# dat |> 
#     ggplot(aes(threshold_variable, threshold_value)) +
#     geom_point(aes(color = type)) +
#     geom_boxplot(aes(fill=type), alpha = 0.5) +
#     facet_wrap(~type)

dat |> 
    select(threshold_variable, threshold_value) |> 
    unique() |> 
    print(n=200)

## Create suitable file names (it was Rmd but when compiled that is the link on the maps)
dat <- dat |> select(-id) |> 
    rownames_to_column(var = "id") |> 
    mutate(
        links = paste0(
            "cs", id , "_",
            str_to_lower(type) |> 
                str_replace_all(pattern = " ", replacement = "_") ,
            ".html"
        )
    ) 

## there are RS for which we do not have case studies


dat <- dat |> 
    mutate(popups = paste("<b><a href=", "'", links, "'", ">", case_study_name, "</a></b>", sep = "")) |> 
    mutate(type = as.factor(type))

save(dat, file = "assets/cases_db.Rda")

pal <- colorFactor(palette = scales::hue_pal()(length(levels(dat$type))), # number of regime shifts in the map
                   domain = levels(dat$type), na.color = "grey90")

map_rsdb <- leaflet(dat) |> 
    addTiles("https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") |>
    setView(80,0, zoom = 1) |> 
    addCircleMarkers(~long, ~lat, radius = 1, popup = ~popups, color = ~pal(type)) |> 
    addLegend("bottomright", pal = pal, values = ~type, title = "Regime shift")

map_rsdb

saveWidget(
    widget = map_rsdb, 
    file = "assets/rsdb2.html",
    selfcontained = FALSE,
    libdir = "site_libs",
    background = "white"
)

save(map_rsdb, file = "assets/map_rsdb.Rda")


rs_types



#### Old map with links to RSDB1 ####
## load data:
rsdb <- read.csv(file = "https://www.dropbox.com/s/34zdewrbrrf35f7/161025_RegimeShiftsDatabase.CSV?dl=1",
                 header = TRUE, sep = ",")

cases <- read.csv(file = 'https://www.dropbox.com/s/4j9jqx3iodw1fkl/Regime%20Shifts%20Database%20Case%20Studies_170120.CSV?dl=1',
                  header = T, sep = ',', stringsAsFactors = F )

rsdb <- rsdb [-2,] # delete for know Invasive floating plants... seems to be duplicated.
#rs <- as.character(rsdb[,3]) %>% sort()

# coords <- geocode(cases$Case.study.name)

# Missing coordinates were completed manually using Diaz supp material and GoogleEarth.
# cases$Case.study.name[which(is.na(coords$lon))]

# coords$place <- cases$Case.study.name
# str(coords)
# write.csv(coords, file = 'case_coords.csv' )
coords <- read.csv2("https://www.dropbox.com/s/csk2fret7d39t1l/case_coords.csv?dl=1", dec = '.')
coord_rs <- read.csv(file = 'https://www.dropbox.com/s/7vj2a0jqwetdaxm/rs_coords_paper.csv?dl=1', dec = '.') %>% select(-X)

cases <- cbind(cases, coords)
#rs <- cbind(rs, coord_rs)
#rs$rs <- as.factor(rs$rs)
cases$Type.of.regime.shift <- as.factor(cases$Type.of.regime.shift)
names(cases)[7] <- 'rs'


cases <- cases %>%
    rename(name = Case.study.name) %>%
    select(3,7,35,36) %>%
    as_tibble()

rs <- coord_rs %>%
    mutate(rs = name) %>%
    select(-place) %>%
    as_tibble() %>%
    rename(lon = x, lat = y)


df <- full_join(rs, cases)

df <- df %>%
    mutate(type = ifelse(rs == name, "rs", "case"),
           rs = forcats::as_factor(rs))

## Load urls dataset
load(url("https://www.dropbox.com/s/jlisr2phboybgfi/regimeShifts_url.RData?dl=1"))


df_rs <- df_rs %>%
    rename(case_names = rs_names, case_url = rs_url)

## the problem is different regime shift names:
df_rs$case_names[!df_rs$case_names %in% df$name][-4] <- c(
    as.character(rs$name)[c(3,4,16,5,6,10,11,12,18,14,15,17,21,22,25,9,28,29,30)]
)


urls <- bind_rows(df_case2, df_case, df_rs)

## correct manually misspelled names so the join works:
df$name[!df$name %in% urls$case_names][3:12] <- urls$case_names[!urls$case_names %in% df$name][c(4,6,7,1,10,9,5,2,3,11)]

df <- left_join(df, urls, by = c("name" = "case_names"))

df <- df %>%
    mutate(., popups = paste("<b><a href=", "'", case_url, "'", ">", name, "</a></b>", sep = ""))

l <- leaflet() %>%
    addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
    #addTiles() %>%
    addCircleMarkers(
        data = df %>% filter(type == "rs"),
        radius = 10,
        col = "orange",
        stroke = FALSE, opacity = 0,
        #clusterOptions = markerClusterOptions(),
        popup = df %>% filter(type == "rs") %>% pull(popups)
    ) %>%
    addCircleMarkers(
        data = df %>% filter(type == "case"),
        radius =  3,
        col = "blue",
        stroke = FALSE, opacity = 0,
        # clusterOptions = markerClusterOptions(
        #     color = "blue", opacity = 1),
        popup = df %>% filter(type == "case") %>% pull(popups)
    )

frameWidget(l, width = "100%")