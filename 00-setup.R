library(rmarkdown)
library(tictoc)
## Create a website template
#rmarkdown::default_site_generator()

## Serve all pages
tic()
rmarkdown::render_site()
toc() # 225.253 sec (3.75min) elapsed, basic website with 31 generic regime shifts


tic()
rmarkdown::clean_site()
toc()