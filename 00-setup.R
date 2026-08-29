library(rmarkdown)
library(tictoc)
library(fs)
library(tidyverse, quietly = TRUE)
## Create a website template
#rmarkdown::default_site_generator()

fls <- dir_ls()

rs_only <- fls |> str_subset(pattern = "^cs|^_", negate = TRUE) |> 
    str_subset("\\.R$|\\.css", negate = TRUE) |> 
    str_subset("\\.Rmd")

## Serve all pages
tic()
walk(rs_only, render_site)
toc() 
# 144.224 sec elapsed (2.5min) elapsed, basic website with 30 generic regime shifts
# including all case studies 3711.352 sec elapsed = 1hr

tic()
render_site()
toc()

## serve all pages in parallel: I don't tiknk it is working
library(future)
library(furrr)

plan(multisession, workers = 10)

tic()
future_walk(.x = here::here(), .f = render_site, .progress = TRUE)
toc() # 50min, ~3800 webpages

plan(sequential)

tic()
rmarkdown::clean_site()
toc()