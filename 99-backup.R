# A script to back up the database in a similar way as I webscrapped the old website
# Option 1: do it from Rmd files
# Option 2: do it from the website (using html tags)
library(tidyverse)
library(fs)

## The goal is to recreate a data frame or json file with the following structure:
rs_dat <- read_csv2("assets/generic_types_RSDB_new.csv") |> 
    janitor::clean_names()
vars <- names(rs_dat)

fls <- dir_ls("~/Documents/Projects/RSDB/") |> 
    str_subset(pattern = "/rs") |> str_subset("Rmd$")
fls # 30 Rmd files with Regime shift reviews

txt <- read_lines(fls[11])

## tools: helper functions
txt_anchors <- function(txt, clue1, clue2){
    a1 <- which(txt |> str_detect(clue1)) + 1 # first anchor
    aa <- which(txt |> str_detect(clue2)) # range
    a2 <- aa[aa > a1][1] - 1 # second anchor
    return(txt[a1:a2])
}


## make of this a named list, then a df, and then bind_rows for a flatten csv.
## only run it when all RS drafts are clean

id <- txt |> str_subset("  j: ") |> str_trim("both") |> str_remove("j\\: ") #id
#date
txt |> str_subset("Last update") |> str_remove("-   Last update: ")
# regime_shift_name
txt |> str_subset("^# ") |> head(1) |> str_remove("# ")
# main_contributors
txt |> str_subset("Main contributors") |> str_remove("-   \\*\\*Main contributors\\*\\*: ")
# other_contributors
txt |> str_subset("Other contributors") |> str_remove("-   \\*\\*Other contributors\\*\\*: ")
# summary: I need anchors
txt_anchors(txt, "Last update","\\:\\:\\:" ) |> str_c(collapse = " ") |> str_trim()
# drivers_key_direct_drivers
txt_anchors(txt, "\\*\\*Key drivers\\*\\*:", "\\*\\*Land use\\*\\*:"  ) |> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# drivers_land_use
txt_anchors(txt, "\\*\\*Land use\\*\\*:" , "#### Key attributes" ) |> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# impacts_ecosystem_type
txt_anchors(txt,  "\\*\\*Ecosystem type\\*\\*:'", "\\*\\*Key ecosystem processes\\*\\*:"  )|> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# impacts_key_ecosystem_processes
txt_anchors(txt,"\\*\\*Key ecosystem processes\\*\\*:","\\*\\*Biodiversity\\*\\*:" )|> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# impacts_biodiversity
txt_anchors(txt, "\\*\\*Biodiversity\\*\\*:", "\\*\\*Provisioning services\\*\\*:" )|> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# impacts_provisioning_services
txt_anchors(txt,  "\\*\\*Provisioning services\\*\\*:","\\*\\*Regulating services\\*\\*:")|> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# impacts_regulating_services
txt_anchors(txt, "\\*\\*Regulating services\\*\\*:","\\*\\*Cultural services\\*\\*:" )|> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# impacts_cultural_services
txt_anchors(txt, "\\*\\*Cultural services\\*\\*:" , "\\*\\*Human well-being\\*\\*:") |> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# impacts_human_well_being
txt_anchors(txt, "\\*\\*Human well-being\\*\\*:", "\\*\\*Links to other regime shifts\\*\\*:" )|> 
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# key_attributes_typical_spatial_scale
txt_anchors(txt, "\\*\\*Spatial scale\\*\\*:", "\\*\\*Time scale\\*\\*:" ) |>
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# key_attributes_typical_time_scale
txt_anchors(txt, "\\*\\*Time scale\\*\\*:" , "\\*\\*Reversibility\\*\\*:" )|>
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# key_attributes_reversibility
txt_anchors(txt, "\\*\\*Reversibility\\*\\*:", "\\*\\*Evidence\\*\\*:" ) |>
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# key_attributes_evidence
txt_anchors(txt, "\\*\\*Evidence\\*\\*:",
            "\\*\\*Confidence: existence of the regime shift\\*\\*" )|>
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# key_attributes_confidence_existence_of_rs
txt_anchors(txt, "\\*\\*Confidence: existence of the regime shift\\*\\*",
            "\\*\\*Confidence: mechanisms underlying the regime shift\\*\\*")|>
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# key_attributes_confidence_mechanism_underlying_rs
txt_anchors(txt, "\\*\\*Confidence: mechanisms underlying the regime shift\\*\\*",
            "\\:\\:\\:")|>
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# links_to_other_regime_shifts
txt_anchors(txt, "\\*\\*Links to other regime shifts\\*\\*:", "\\:\\:\\:")|>
    str_subset("-   ") |> str_replace("-   ", " | ") |> str_c(collapse = "")
# alternate_regimes: cat() recovers the line formating for Rmd
txt_anchors(txt, "#### Alternative regimes" , "#### Drivers and causes of the regime shift" ) |> 
    str_c(collapse = "\n") 
# drivers_and_causes_of_the_regime_shift
txt_anchors(txt,"#### Drivers and causes of the regime shift",
            "#### Impacts on ecosystem services and human well-being") |> 
    str_c(collapse = "\n")
# how_the_regime_shift_works: BUG! Corrected now
txt_anchors(txt,"#### How the regime shift works",
            "#### Drivers and causes of the regime shift" ) |> 
    str_c(collapse = "\n")
# impacts_on_ecosystem_services_and_human_well_being
txt_anchors(txt, "#### Impacts on ecosystem services and human well-being",
            "#### Management options" )|> 
    str_c(collapse = "\n")
# management_options
txt_anchors(txt, "#### Management options", "### In-depth analysis" )|> 
    str_c(collapse = "\n")
# references
read_lines(paste0("assets/bibliography/rs",id,".bib")) |> str_c(collapse = "\n") 
# rs_analysis
txt_anchors(txt,"#     rs_analysis[[rs$regime_shift_name]] |> writeLines()",
            "### Categorical summary" ) |> str_remove("```") |> 
    str_c(collapse = "\n")
# citation_text
txt_anchors(txt, "Acknowledge this review as:" ,  "BibTeX citation:"  ) |> 
    str_remove("<tiny>|<\\/tiny>") |> str_subset("```", negate = TRUE) |>
    str_c(collapse = "")
# citation_bibtex
txt_anchors(txt, "BibTeX citation:", "<\\/tiny>") |> 
    str_remove_all("<tiny>|<\\/tiny>") |> str_subset("```", negate = TRUE) |>
    str_c(collapse = "\n") |> str_remove("\n\n\n")





## test
# l <- list(1,2,3)
# names(l) <- c("a","b","c")
# l |> as_tibble()
