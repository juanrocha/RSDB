library(tidyverse)
library(fs)

load("assets/cases_db.Rda") # version for PECS, has 3015 cases
dat2 <- read_csv("assets/rsdb_clean_240824.csv") # has a few extra cases by Romain
# mainly AMOC. Needs reviewing by me.
dat2
dat

dat2[!dat2$case_study_name %in% dat$case_study_name , ] |> select(case_study_name, type)

## Produce a dataset with DOIs classified and non-classified

dat2 |> 
    select(type, doi) |> 
    mutate(doi = str_split(doi, pattern = "\\(\\#\\d{1,}\\) ")) |>
    unnest(doi) |> 
    mutate(doi = str_trim(doi, "both")) |> 
    filter(doi != "No link found", doi != "") |> unique() |> 
    write_csv("assets/teaching_doi.csv")

dat2 |> 
    select(type, references) |> 
    mutate(references = str_split(references, pattern = "\\(\\#\\d{1,}\\) ")) |>
    unnest(references) |> 
    mutate(references = str_trim(references, "both")) |> 
    filter(references != "") |> unique() |> 
    write_csv("assets/teaching_refs.csv")


## CLDs
load("assets/clds.Rda")

write_csv(clds, "assets/teaching_clds.csv")


p <- dat |> 
    ggplot(aes(type)) + 
    geom_bar() + 
    #scale_y_log10() + 
    coord_flip() +
    theme_light(base_size = 7)

q <- p + scale_y_log10()


ggsave(plot = q, 
       filename = "teaching_current_status_log.png", path = "assets/", dpi = 500,
           width = 4, height = 3.5)
 