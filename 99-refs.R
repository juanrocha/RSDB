library(rcrossref)
library(tidyverse)

load("assets/cases_db.Rda")

df_refs <- dat |> 
    select(id, doi, references)

df_refs <- df_refs |> 
    mutate(across(.cols = doi:references, \(x) str_remove( x, pattern = "\\(#1\\) "))) |> 
        mutate(across(doi:references, \(x) str_split(x, pattern = "\\(\\#\\d{1,}\\) "))) |>
        unnest(cols = c(doi, references)) |>
        # only query the missing ones for now 
        filter(is.na(references))|> 
    mutate(is_doi = str_detect(doi, "doi.org")) |> 
    filter(is_doi) |> 
    mutate(doi = str_remove(doi, pattern = "^https://doi.org/|http://dx.doi.org/")) 

df_refs |> pull(doi) |> unique()  # remove duplicates

df_refs <- df_refs |> 
    mutate(doi = str_remove(doi, "\\; (.*)")) |> 
    mutate(doi = str_trim(doi, "both"))  |> 
    group_by(doi) |> 
    nest()

df_refs |> unnest(data) |> ungroup() 


citations <- map(df_refs$doi, cr_cn, format = "text", style = "apa")

df_refs <- df_refs |> 
    add_column(citations = citations) |> 
    unnest(c(data, citations)) |> 
    select(-is_doi, -references) |> 
    rename(references = citations) |> 
    ungroup()

dat <- dat |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id)

df_refs <- df_refs |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id)

dat$references[df_refs$id] <- df_refs$references

dat |> 
    select(id, doi, references) |> 
    filter(is.na(references))

save(dat, file = "assets/cases_db.Rda") # re-save data with citations
