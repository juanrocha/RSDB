library(tidyverse)

# load datasets and check name compatibility
load("assets/rsdb_images.Rda")

rs_dat <- read_csv("assets/generic_types_RSDB_new.csv") |> 
    janitor::clean_names()
# correct names and add links
rs_dat <- rs_dat |> 
    mutate(regime_shift_name = case_when(
        regime_shift_name == "Moonson" ~ "Indian summer monsoon",
        regime_shift_name == "Floating plants" ~ "Submerged to floating plants", 
        .default = regime_shift_name
    )) |> # the following should be CS of floating plants
    filter(regime_shift_name != "Invasive floating to invasive submerged plant dominance") |>
    mutate(filename = str_replace(filename, "\\.Rmd", "\\.html")) |> 
    # create links here, so there is links for RS without cases
    mutate(href = paste0("https://regimeshifts.netlify.app/", filename))

df_img <- df_img |> 
    filter(str_detect(href, "rs/rs-")) |> 
    mutate(name = str_to_sentence(name))

df_img$name[!df_img$name %in% rs_dat$regime_shift_name] |> unique()

## Correct names: 
df_img <- df_img |> 
    # type is the short name for plotting and matching, name is the long name
    mutate(type = name) |>
    mutate(type = case_when(
        type == "Coniferous to deciduous boreal forest" ~ "Coniferous to deciduous forest",
        type == "Coastal marine eutrophication" ~ "Marine eutrophication" ,
        type == "Forest to savannas" ~ "Forest to savanna",
        type == "Mangrove transitions" ~ "Mangroves transitions",
        type == "River channel position" ~ "River channel change",
        type == "Salt marsh to tidal flat" ~ "Salt marshes to tidal flats" ,
        type == "Thermokarst lake to terrestrial ecosystem" ~ "Thermokarst lakes",
        type == "Tundra to boreal forest" ~ "Tundra to forest",
        type == "West antarctic ice sheet collapse" ~ "West Antarctic ice sheet collapse",
        TRUE ~ type
    ))

df_img$type[!df_img$type %in% rs_dat$regime_shift_name] |> unique() # correct names

df_img <- df_img |> 
    select(-href) |> 
    left_join(rs_dat |> select(type = regime_shift_name, href))

### save to CSV so you can prioritize manually the pictures intead of a random sample
write_csv(df_img, file = "assets/rs_images.csv")


