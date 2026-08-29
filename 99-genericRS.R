# create one markdwon document for each generic regime shift
library(tidyverse)
library(tictoc)
library(fs)


## For cleaning old versions
# fls <- dir_ls()
# 
# fls |> str_subset(pattern = "^rs") |> 
#     file_delete()


rs_dat <- read_csv("assets/generic_types_RSDB_new.csv") |> 
    janitor::clean_names()

pblm <- c("key_attributes_typical_spatial_scale", "drivers_key_direct_drivers", "drivers_land_use",
          "impacts_human_well_being")

rs_dat <- rs_dat |> 
    mutate(across(.cols = all_of(pblm), 
                  \(x) str_replace_all(x, pattern = ", ", replacement = " or "))) |> 
    mutate(impacts_ecosystem_type = str_remove(
        impacts_ecosystem_type, pattern = "\\(below ~500mm rainfall\\/year\\)"))


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

## function to produce carousel code
img <- read_csv2("assets/rs_images.csv")

txt_img <- function(i, dat, img){
    img <- img |> filter(type == dat$regime_shift_name[i])

    glue::glue_data(img, " |>
    bs_append(
        content = bs_carousel_image(
            src = paste0(w, '{file}'), alt = '{type}'),
        caption = bs_carousel_caption(title = '{type}'))")

}
## TODO: Fix attribution later, it's not consistently reported, so I need to edit img manually.
txt_img(3, dat,img)

rs_txt <- function(i, dat, img){
    txt <- c(

#yaml
"---
output: 
  html_document:
    toc: false
    toc_depth: 3
    toc_float: true
bibliography: ",
paste0('"','assets/bibliography/rs', i, '.bib','"', '\n'),
"params:
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

"::::{style='display: flex;'}\n\n",
":::{style='width: 50%;'}\n\n",

# title and authors
"# ", dat$regime_shift_name[i], "\n\n\n",
"- **Main contributors**: ", dat$main_contributors[i], "\n\n",
"- **Other contributors**: ", dat$other_contributors[i], "\n\n",
"- Last update: ", as.character(dat$date[i]) , "\n\n",
# summary
dat$summary[i], "\n\n",


":::\n\n:::{style='width: 5%;'}\n\n",
":::\n\n:::{style='width: 45%;'}\n\n",

## Carousel
"```{r carousel, out.width='500px', out.height='100px', results = 'asis'}\n\n

library(htmltools)
library(bsplus)

w <- 'https://www.juanrocha.se/'
bs_carousel(id = 'rs",i, "', use_indicators = TRUE)", txt_img(i, dat,img),
"\n\n",

"```\n\n",
":::\n::::\n\n",

"## Evidence\n\n",


## Map
"```{r map, out.width='100%', out.height='300px', results = 'markup'}\n


cs <- cases |> 
    filter(type == rs$regime_shift_name) 

m <- leaflet(cs) |> 
    addTiles(paste0('https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png?=key=', keyring::key_get('carto-maps'))) |> 
    addCircleMarkers(
        lng = ~long, lat=~lat, radius = 2, 
        color = 'orange', 
        popup = ~popups) |> 
    setView(5,10, zoom = 2) 
m
```\n\n",
"**Fig 1 | Empirical evidence.** The databse currently documents `r nrow(cs)` cases of `r str_to_lower(rs$regime_shift_name)` around the world. Each dot in the map is coded from a scientific article documenting a place undergoing this regime shift. You can learn more from each case by hovering and clicking on the case of interest.\n\n",

"## Analysis {.tabset .tabset-pills}\n\n",

"### General information\n\n",
"#### Alternative regimes\n\n",
dat$alternate_regimes[i] |> str_remove_all(pattern = "\\<br \\/\\>"), "\n\n" ,
"#### Drivers and causes of the regime shift\n\n",
dat$drivers_and_causes_of_the_regime_shift[i] |> 
    str_remove_all(pattern = "\\<p\\>\\&nbsp\\;\\<\\/p\\>"), "\n\n",
"#### Impacts on ecosystem services and human well-being\n\n", 

dat$impacts_on_ecosystem_services_and_human_well_being[i], "\n\n",
"#### Management options\n\n",
dat$management_options[i], "\n\n",

"### In-depth analysis\n\n",


## cld
"```{r cld, error = FALSE, message = FALSE, out.width='80%'}\n
#| fig.cap = '**Fig 2| Causal diagram.** Hover over the variables to see their names, orange dots are drivers, blue dots are variables inside feedback mechanisms. Red arrows represent positive causal relationships while blue arrows represent negative ones. Key feedbacks are described below.'

library(ggiraph)
library(htmlwidgets)
source('tools.R')
load('assets/clds.Rda')

if (rs$regime_shift_name %in% clds$regime_shift) {
    gg <- rs_net(clds, rs$regime_shift_name) |> 
    plot_net()
    # create the html obj: sizing not working
    girafe(ggobj = gg) |> 
        girafe_options(
            opts_tooltip(opacity = 0.7), opts_zoom(min =0.5, max =2),
        sizingPolicy(defaultWidth='100px', defaultHeight='50px'),
        opts_hover(css = 'fill:red;stroke:orange;r:5pt;'))
    }


```\n\n",

# rs analysis
"```{r}\n
load('assets/rs_analysis_text.Rda')

if (is.null(rs_analysis[[rs$regime_shift_name]]))  'This regime shift does not have a feedback analysis yet' else 
    rs_analysis[[rs$regime_shift_name]] |> writeLines()
```\n\n",


"### Categorical summary\n\n",

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



"### References\n\n",
"::: {#refs}\n
:::\n",


"### References old [Delete]\n\n", 

"<small>\n\n", 
dat$references[i] |> 
    str_split(pattern = "\\(\\#\\d{1,}\\) ") |> 
    map(~str_c("- ", ., "\n")) |> 
    unlist() |> 
    str_remove(pattern = "- \\\n"),
"\n", "</small>", "\n\n",

"## Citation\n",

"Acknowledge this review as:\n\n",
    
    
"<tiny> \n``` {style='color: gray;'}\n", 
str_c(dat$main_contributors[i], ", ",  dat$other_contributors[i], ". ", dat$regime_shift_name[i], '. In: Regime Shift Database, www.regimeshifts.org. Last revised: ', dat$date[i]),"\n```\n\n",

"</tiny>", "\n\n", "BibTeX citation:\n\n<tiny>",
"<tiny> \n``` {style='color: gray;'}\n", 
"@misc{\n",
"  author = {", 
str_c(dat$main_contributors[i] |> str_replace_all(", ", " and "),
    dat$other_contributors[i] |> str_replace_all(", ", " and ")),"},\n",
"  title = {", dat$regime_shift_name[i], "},\n",
"  url = {www.regimeshifts.org},\n",
"  howpublished = {Regime Shifts Database},\n",
"  publisher = {Stockholm Resilience Centre},\n",
"  institution = {Stockholm University}\n",
"}\n```\n\n",
"</tiny>"
)

    return(txt)
}


# test
txt2 <- rs_txt(1, dat, img)

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

#write_csv(dat, file = "assets/generic_types_RSDB_new.csv")

## Create one markdown file for every regime shift

tic()
for (i in seq_along(dat)){
    txt <- rs_txt(i, dat, img)
    capture.output(
        cat(txt, sep = ""),
        file = dat$filename[i],
        append = FALSE
    )
}
toc() #0.5s all RS!



