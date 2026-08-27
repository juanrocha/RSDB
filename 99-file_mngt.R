library(tidyverse)
library(fs)

dat <- dir_info()

# regime shifts files
dat |>
    filter(str_detect(path, "^rs")) |>
    filter(str_detect(path, "Rmd$")) |>
    select(path) |> arrange() |> print(n=40)

# create bibliography files
dat |>
    filter(str_detect(path, "^rs")) |>
    filter(str_detect(path, "Rmd$")) |> 
    separate(path, into = c("key", "rest"), remove = FALSE) |> 
    mutate(key = str_c(key, ".bib")) |> 
    pull(key) |> 
    file_create()

## see the refs to format text
rs <- read_csv(file = "assets/generic_types_RSDB_new.csv")
rs |> 
    filter(regime_shift_name == "Coral transitions") |> 
    select(references, new_ref_format) |> pull(new_ref_format) |> #jsonlite::parse_json()
    jsonlite::fromJSON() |> as_tibble() |> select(doi, authors, title)



#### Danger zone ####
# clean up, delete files so they can be compiled again if new versions come or
# corrections need to be made

# remove all case studies: files starting with cs+number
dat |> 
    filter(str_detect(path, "^cs\\d{1}")) |> 
    filter(str_detect(path, "Rmd$")) |> 
    pull(path) |> 
    file_delete()

# clean up the _site folder
fls <- dir_info("_site/")
fls |> filter(str_detect(path, "bib$")) |> 
    pull(path) |> 
    file_delete()
