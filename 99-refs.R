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
    mutate(doi = str_trim(doi, "both")) 

# stupidly I did this whitout unique()
citations <- map(df_refs$doi, cr_cn, format = "text", style = "apa")

df_refs <- df_refs |> 
    add_column(citations = citations) |> 
    unnest(citations)

dat <- dat |> left_join(df_refs |> select(-doi)) |> 
    # select(doi, references, citations) |> 
    # filter(!is.na(citations)) |> 
    mutate(references = case_when(
        is.na(references) ~ citations,
        .default = references
    )) 

save(dat, file = "assets/cases_db.Rda") # re-save data with citations
