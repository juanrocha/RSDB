library(rcrossref)
library(tidyverse)
library(tictoc)


load("assets/cases_db.Rda")

df_refs <- dat |> 
    select(id, doi, reference_links, references)

## There is two cases: missing ref with doi
df_refs |> filter(is.na(references), !is.na(doi)) #none
# missing ref but link provided: 1905 cases
df_refs |> filter(is.na(references), !is.na(reference_links))

# is there more than one ref per case?
df_refs <- df_refs |> 
    filter(is.na(references), !is.na(reference_links)) |> 
    mutate(reference_links = str_split(
        reference_links, "; "
    )) |> 
    unnest(reference_links) |> 
    mutate(reference_links = str_split(
        reference_links, "\\(\\#\\d{1,}\\)"
    )) |> 
    unnest(reference_links) |> 
    filter(reference_links != "") |>
    # filter(str_detect(reference_links, "\\(\\#\\d{1,}")) 
    mutate(reference_links = str_remove(reference_links, pattern = "\\(\\#\\d{1,}\\)")) |> 
    mutate(reference_links = str_trim(reference_links, "both")) 
    
## keep only dois
df_refs <- df_refs |> 
    mutate(is_doi = str_detect(reference_links, "doi.org")) |>
    filter(is_doi) |> 
    mutate(doi = str_remove(reference_links, pattern = "^https://doi.org/|http://dx.doi.org/|https://doi-org.ezp.sub.su.se/")) |> # remove entries with spaces, doi do not have space: one error case by Noelia Morell 
    filter(!str_detect(doi, " "))

df_refs |> pull(doi) |> unique()  # remove duplicates

df_refs <- df_refs |> 
    mutate(doi = str_trim(doi, "both"))  |> 
    group_by(doi) |> 
    nest()

df_refs |> unnest(data) |> ungroup() 

safe_doi <- safely(cr_cn)

tic()
citations <- map(
    df_refs$doi, safe_doi, 
    format = "text", style = "apa", 
    .progress = TRUE)
toc()

citations <- transpose(citations)
is_ok <- map(citations$error, is.null)

which(!unlist(is_ok))


df_refs <- df_refs |> 
    add_column(citations = citations$result) |> 
    unnest(c(data, citations)) |> 
    select(-is_doi, -references) |> 
    rename(references = citations) |> 
    ungroup()

## there is more than one ref per paper, I need to combine them on one field
df_refs |> group_by(id) |> summarize(n=n()) |> arrange(desc(n))

df_refs <- df_refs |>
     group_by(id) |> select(-doi, -reference_links) |> 
     summarize(refs = glue::glue_collapse(references, sep ="\n"))


dat <- dat |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id)

df_refs <- df_refs |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id)

dat$references[df_refs$id] <- df_refs$refs

# 30 entries still missing
dat |> 
    select(id, doi, references) |> 
    filter(is.na(references))

save(dat, file = "assets/cases_db.Rda") # re-save data with citations


## leftovers
## old way to recover refs:
# df_refs <- df_refs |> 
#     mutate(across(.cols = doi:references, \(x) str_remove( x, pattern = "\\(#1\\) "))) |> 
#     mutate(across(doi:references, \(x) str_split(x, pattern = "\\(\\#\\d{1,}\\) "))) |>
#     unnest(cols = c(doi, references)) |>
#     # only query the missing ones for now 
#     filter(is.na(references))|> 
#     mutate(is_doi = str_detect(doi, "doi.org")) |> 
#     filter(is_doi) |> 
#     mutate(doi = str_remove(doi, pattern = "^https://doi.org/|http://dx.doi.org/")) 
# 
# df_refs |> pull(doi) |> unique() 