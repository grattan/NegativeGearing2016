

# First remove all but the CGT_parent
vapply(dir(path = ".", pattern = "\\.tex$"), function(x) {
  !grepl("parent", x) && file.remove(x)
}, FALSE)

install.packages("hutils", repos = "https://cran.rstudio.com")
install.packages("grattan", repos = "https://cran.rstudio.com", quiet = TRUE)
install.packages("TeXCheckR", repos = "https://cran.rstudio.com", dependencies = TRUE)

devtools::install_github(paste0("hughparsonage/", c("hutilscpp", "grattanCharts")), 
                         quick = TRUE,
                         quiet = TRUE)
if (!requireNamespace("taxstats", quietly = TRUE) || 
    !requireNamespace("taxstats1516", quietly = TRUE)) {
  install.packages(c("taxstats", "taxstats1516"),
                   repos = "https://hughparsonage.github.io/tax-drat",
                   type = "source")
}

packages_ <-
  c("bindrcpp", "hutils", "ggrepel", "testthat", 
    "magrittr", "tidyr", "usethis", "devtools", "expm", "Hmisc", 
    "Formula", "lattice", "foreign", "survey", "survival", "Matrix", 
    "grid", "zoo", "httr", "rsdmx", "readr", "openxlsx", "readxl", 
    "xtable", "grattan", "directlabels", "scales", "ggplot2", "gridExtra", 
    "dplyr", "haven", "devEMF", "knitr", 
    "showtext", "sysfonts",
    "data.table", "sessioninfo")

# Remove already installed packages
packages_ <- packages_[!nzchar(find.package(packages_))]

if (length(packages_)) {
  install.packages(packages_, repos = "https://cran.rstudio.com")
}

# Wait until we work out hilda
# knitr::knit("CGT_and_neg_gearing_parent.Rnw")






