# create one markdwon document for each generic regime shift
library(tidyverse)
library(tictoc)
library(fs)


## For cleaning old versions
fls <- dir_ls()

fls |> str_subset(pattern = "^rs") |> 
    file_delete()


rs_dat <- read_csv("assets/generic_types_RSDB_new.csv") |> 
    janitor::clean_names()

# correct Monsoon, not moonson
rs_dat <- rs_dat |> 
    mutate(regime_shift_name = case_when(
        regime_shift_name == "Moonson" ~ "Indian summer monsoon",
        regime_shift_name == "Floating plants" ~ "Submerged to floating plants", 
        .default = regime_shift_name
    )) |> # the following should be CS of floating plants
    filter(regime_shift_name != "Invasive floating to invasive submerged plant dominance")

## change name so the rest of the scipt works:
dat <- rs_dat

rs_txt <- function(i, dat){
    txt <- c(

#yaml
"---
output: 
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true

params:
  j: ",i, 
"\n---\n\n\n",
# load libs
"```{r include = FALSE}
library(tidyverse)
library(leaflet)

load('assets/cases_db.Rda')
cases <- dat
dat <- read_csv('assets/generic_types_RSDB_new.csv') |> 
    janitor::clean_names()

x <- dat$id[params$j]
rs <- dat |> filter(id== x)
knitr::opts_chunk$set(echo = FALSE, results = 'asis')
```\n\n\n", 
# title and authors
"# ", dat$regime_shift_name[i], "\n\n",
"**Main contributors**: ", dat$main_contributors[i], "\n\n",
"**Other contributors**: ", dat$other_contributors[i], "\n\n",
"Last update: ", as.character(dat$date[i]) , "\n\n",
## Map
"```{r map, out.width='100%', out.height='300px', results = 'markup'}\n


cs <- cases |> 
    filter(type == rs$regime_shift_name) 

m <- leaflet(cs) |> 
    addTiles('https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') |> 
    addCircleMarkers(
        lng = ~long, lat=~lat, radius = 2, 
        color = 'orange', 
        popup = ~popups) |> 
    setView(5,10, zoom = 2) 
m
```\n\n",
# summary
"### Summary\n\n",
dat$summary[i], "\n\n",

# categorical attributes
"### Categorical attributes\n\n",
"::::{style='display: flex;'}\n\n",
":::{style='width: 50%;'}\n\n",
"#### Impacts\n\n",
"**Ecosystem type**:'\n\n", 

dat$impacts_ecosystem_type[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist() , "\n\n",
"**Key ecosystem processes**:\n\n",
dat$impacts_key_ecosystem_processes[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist() , "\n\n",
"**Biodiversity**:\n\n",
dat$impacts_biodiversity[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Provisioning services**:\n\n",
dat$impacts_provisioning_services[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Regulating services**:\n\n",
dat$impacts_regulating_services[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist() , "\n\n",
"**Cultural services**:\n\n",
dat$impacts_cultural_services[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Human well-being**:\n\n",
dat$impacts_human_well_being[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist() , "\n\n",
"**Links to other regime shifts**:\n\n",
dat$links_to_other_regime_shifts[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
":::\n\n:::{style='width: 50%;'}\n\n",
"#### Drivers\n\n",
"**Key drivers**:\n\n",
dat$drivers_key_direct_drivers[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist() , "\n\n",
"**Land use**:\n\n",
dat$drivers_land_use[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"#### Key attributes\n\n",
"**Spatial scale**:\n\n",
dat$key_attributes_typical_spatial_scale[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Time scale**:\n\n",
dat$key_attributes_typical_time_scale[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Reversibility**:\n\n",
dat$key_attributes_reversibility[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Evidence**:\n\n",
dat$key_attributes_evidence[i] |> str_split(pattern = ", ") |> 
    map(~str_c("- ", ., "\n")) |> unlist(), "\n\n",
"**Confidence: existence of the regime shift**\n\n",
dat$key_attributes_confidence_existence_of_rs[i] %>% str_c("- ", . , "\n"), "\n\n",
"**Confidence: mechanisms underlying the regime shift**\n\n",
dat$key_attributes_confidence_mechanism_underlying_rs[i]  %>% str_c("- ", . , "\n"), "\n\n",
":::\n\n::::\n\n", 

# "{.tabset}\n--------------------------------------------------\n\n",

"### Detail information\n\n",
"#### Alternative regimes\n\n",
dat$alternate_regimes[i] |> str_remove_all(pattern = "\\<br \\/\\>"), "\n\n" ,
"#### Drivers and causes of the regime shift\n\n",
dat$drivers_and_causes_of_the_regime_shift[i] |> 
    str_remove_all(pattern = "\\<p\\>\\&nbsp\\;\\<\\/p\\>"), "\n\n",
"#### Impacts on ecosystem services and human well-being\n\n", 

dat$impacts_on_ecosystem_services_and_human_well_being[i], "\n\n",
"#### Management options\n\n",
dat$management_options[i], "\n\n",

"### Regime shift Analysis\n\n",

## cld
"```{r cld, error = FALSE, message = FALSE, out.width='80%'}\n
source('tools.R')
load('assets/clds.Rda')

if (rs$regime_shift_name %in% clds$regime_shift) {
    gg <- rs_net(clds, rs$regime_shift_name) |> 
    plot_net()
    girafe(ggobj = gg)}
```\n\n",

# rs analysis
"```{r}\n
load('assets/rs_analysis_text.Rda')

if (is.null(rs_analysis[[rs$regime_shift_name]]))  'This regime shift does not have a feedback analysis yet' else 
    rs_analysis[[rs$regime_shift_name]] |> writeLines()
```\n\n",


"## Citation\n\n ",

"Acknowledge this review as:\n\n",
    
    
"<small> ", 
str_c(dat$main_contributors[i], ", ",  dat$other_contributors[i], ". ", dat$regime_shift_name[i], '. In: Regime Shift Database, www.regimeshifts.org. Last revised: ', dat$date[i]), "</small>", "\n\n", 


"### References\n\n", 

"<small>\n\n", 
dat$references[i] |> 
    str_split(pattern = "\\(\\#\\d{1,}\\) ") |> 
    map(~str_c("- ", ., "\n")) |> 
    unlist() |> 
    str_remove(pattern = "- \\\n"),
"\n", "</small>", "\n\n"
    )
    return(txt)
}


# test
txt2 <- rs_txt(1, dat)

## Create suitable file names
dat <- dat |> 
    arrange(date) |> 
    mutate(id = row_number()) |> 
    mutate(
        filename = paste0(
            "rs", id , "_",
            str_to_lower(regime_shift_name) |> 
                str_replace_all(pattern = " ", replacement = "_") ,
            ".Rmd"
        )
    ) 

dat$filename

write_csv(dat, file = "assets/generic_types_RSDB_new.csv")

## Create one markdown file for every regime shift

tic()
for (i in seq_along(dat$id)){
    txt <- rs_txt(i, dat)
    capture.output(
        cat(txt, sep = ""),
        file = dat$filename[i],
        append = FALSE
    )
}
toc() #27s all case studies!



