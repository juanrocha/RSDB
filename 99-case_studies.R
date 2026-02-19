# create one markdwon document for each case study
library(tidyverse)
library(tictoc)
library(fs)
load("assets/cases_db.Rda")


## For cleaning old versions
fls <- dir_ls()

fls |> str_subset(pattern = "^cs") |> 
    file_delete()



## Notes
# The case study dataset does not have RS analysis but some CS do 
# have on the website. Where do we get that info from?
dat
i = 2

# remove: () in deserts.
dat <- dat |> 
    mutate(
        ecosystem_type = str_remove(ecosystem_type, pattern = "\\(below ~500mm rainfall\\/year\\)")) #|> pull(ecosystem_type) |> unique()

# template text for case studies without regime shift analysis
cs_txt <- function(i, dat){
    txt <- c(
#yaml
"---
output: html_document
params:
  j: ",i, "\n---\n\n\n",
# load libs
"```{r include = FALSE}
library(tidyverse)
library(leaflet)
load('assets/cases_db.Rda')
x <- dat$id[params$j]
cs <- dat |> filter(id== x)
knitr::opts_chunk$set(echo = FALSE, results = 'asis')
```\n\n\n",
# title and authors
"# ", dat$case_study_name[i], "\n\n",
"**Main contributors**: ", dat$main_contributors[i], "\n\n",
"**Other contributors**:", dat$other_contributors[i], "\n\n",
"Last update: ", as.character(dat$creation_date[i]) , "\n\n",
## Map
"```{r map, out.width='100%', out.height='300px', results = 'markup'}\n
m <- leaflet(cs ) |> 
    addTiles('https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') |>
    addMarkers(cs$long, cs$lat) |> 
    setView(cs$long, cs$lat, zoom = 4)
m
```\n\n",
# summary
"### Summary\n\n",
dat$summary[i], "\n\n",
"**Type of regime shift**: ", ifelse(dat$type[i] == "Unclassified", dat$regime_shift_type_other[i], as.character(dat$type[i]) ) , "\n\n",
"**Ecosystem type**: \n\n", dat$ecosystem_type[i] |>
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Land uses**:\n\n", dat$land_uses[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(),"\n\n" ,
"**Spatial scale**:\n",
  dat$spatial_scale[i] |>
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(),"\n\n" ,
"**Continent or Ocean**:\n", 
  dat$location_continent_or_ocean[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(),"\n\n" ,
"**Region**: ", dat$location_region[i], "\n\n",
"**Countries**: ", dat$location_countries[i], "\n\n",
"{.tabset}\n--------------------------------------------------\n\n",

# categorical attributes
"### Categorical attributes\n\n",
"::::{style='display: flex;'}\n\n",
":::{style='width: 50%;'}\n\n",
"#### Impacts\n\n",
"**Ecosystem type**:'\n\n", 
  dat$ecosystem_type[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Key ecosystem processes**:\n\n",
  dat$impacts_on_key_ecosystem_processes[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
# "**Biodiversity**:\n\n",
#   dat$impacts_biodiversity[i] |> 
#       str_split(pattern = ", ") |> 
#       map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Provisioning services**:\n\n",
  dat$impacts_on_provisioning_services[i] |>
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Regulating services**:\n\n",
  dat$impacts_on_regulating_services[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist() , "\n\n",
"**Cultural services**:\n\n",
  dat$impacts_on_cultural_services[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Human well-being**:\n\n",
  dat$impacts_on_human_well_being[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist() , "\n\n",
":::\n\n:::{style='width: 50%;'}\n\n",
"#### Drivers\n\n",
"**Key drivers**:\n\n",
  dat$key_direct_drivers[i] |>
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist() , "\n\n",
"**Land use**:\n\n",
  dat$land_uses[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"#### Key attributes\n\n",
"**Spatial scale**:\n\n",
  dat$spatial_scale[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Time scale**:\n\n",
  dat$time_scale[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Reversibility**:\n\n",
  dat$reversibility[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Evidence**:\n\n",
  dat$sources_of_evidence[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Confidence: existence of the regime shift**\n\n",
  dat$confidence_of_existence[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Confidence: mechanisms underlying the regime shift**\n\n",
  dat$confidence_of_mechanism[i] |> 
      str_split(pattern = ", ") |> 
      map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
":::\n\n::::\n\n",
"### Detail info and key references\n\n",
"#### Alternative regimes\n\n",
  dat$alternate_regimes[i] |> 
      str_remove_all(pattern = "\\<br \\/\\>"), "\n\n" ,
"#### Drivers and causes of the regime shift\n\n",
  dat$drivers_and_causes[i] |> 
      str_remove_all(pattern = "\\<p\\>\\&nbsp\\;\\<\\/p\\>"),
  "\n\n",
"#### Impacts on ecosystem services and human well-being\n\n",
  dat$impacts_on_ecosystem_services_and_human_well_being[i], 
  "\n\n",
"#### Management options\n\n",
  dat$management_options[i], "\n\n",
"#### Key references\n\n", "<small>\n\n", 
  dat$references[i] |> 
      str_split(pattern = "\\(\\#\\d{1,}\\) ") |> 
      map(~str_c("- ", ., "\n")) |> 
      unlist() |> 
      str_remove(pattern = "- \\\n") ,
"\n", "</small>", "\n\n"
)
    return(txt)
}

# test
txt2 <- cs_txt(1, dat)

## Create suitable file names
dat <- dat |> 
    mutate(
        filename = paste0(
            "cs", id , "_",
            str_to_lower(type) |> 
                str_replace_all(pattern = " ", replacement = "_") ,
            ".Rmd"
        )
    ) 

## Create one markdown file for every case study

i = 1:length(dat$id)

tic()
for (i in seq_along(dat$id)){
    txt <- cs_txt(i, dat)
    capture.output(
        cat(txt, sep = ""),
        file = dat$filename[i],
        append = FALSE
    )
}
toc() # 41s ~ 3000 case studies


## For cleaning old versions
fls <- dir_ls()

fls |> str_subset(pattern = "^cs") |> 
    file_delete()


## Some visualizations for 

