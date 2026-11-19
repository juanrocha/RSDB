library(tidyverse)
library(fs)

df_img <- read_csv(file = "assets/rs_images.csv")
df_new <- readxl::read_xlsx("assets/img/rs_images.xlsx")
p <- df_img$priority

df_img <- df_img |> 
    select(-onclick, -downlink) |> 
    rename(caption = title, attribution = caption)

df_new <- df_new |> 
    filter(!is.na(file)) |> 
    select(-source_row, -code_name, -file, -downlink, -image_name) |> 
    rename(name = type, file = rs_name, caption = description)

df_img <- bind_rows(df_img, df_new) 


# load datasets and check name compatibility
# load("assets/rsdb_images.Rda") # this one links to all the >400 imgs that the old RSDB had
# df_img

rs_dat <- read_csv("assets/generic_types_RSDB_261129.csv") |> 
    janitor::clean_names()


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
        type == "Arctic sea ice loss" ~ "Arctic sea-ice loss",
        type == "Sprawling versus compact cities" ~ "Sprawling vs compact city",
        type == "River delta accretion and recession" ~ "River deltas" ,
        type == "Dryland degradation (or desertification)" ~ "Dryland degradation",
        type == "Carbon sink to carbon source transition" ~ "Tropical forest sink to source",
        TRUE ~ type
    )) 

df_img$type[!df_img$type %in% rs_dat$regime_shift_name] |> unique() # correct names

df_img <- df_img |> 
    select(-href) |> 
    left_join(rs_dat |> select(type = regime_shift_name, href))

#df_img$priority <- p

df_img <- df_img |> arrange(name)


### save to CSV so you can prioritize manually the pictures intead of a random sample
write_csv(df_img, file = "assets/rs_images_261119.csv")

## correct links:
df_img <- read_csv2(file = "assets/rs_images.csv")
p <- df_img$priority
df_img <- read_csv2(file = "assets/rs_images.csv")

## Move image files to image folder
df_img |> 
    filter(str_starts(file, "img"))

fls <- dir_ls(path = "~/Documents/Projects/Website/WebPage/static/img")

missing_pics <- tibble(path = fls) |> 
    mutate(file = str_remove_all(
        path, pattern = "/Users/juanrocha/Documents/Projects/Website/WebPage/static/")) |> 
    select(path, file) |> 
    filter(file %in% df_img$file)|> 
    mutate(destination = str_c("/Users/juanrocha/Documents/Projects/RSDB/assets/", file))

fs::file_copy(path = missing_pics$path, new_path = missing_pics$destination)
