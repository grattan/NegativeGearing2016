
if (!requireNamespace("devtools", quietly = TRUE) ||
    !requireNamespace("hunspell", quietly = TRUE) || 
    !requireNamespace("haven", quietly = TRUE)) {
  system("sudo apt-get install -y r-cran-devtools r-cran-rcpp r-cran-bh r-cran-stringi r-cran-hunspell r-cran-survey r-cran-dplyr r-cran-tidyr r-cran-readr r-cran-ggplot2 r-cran-ggrepel r-cran-haven r-cran-survival")
}
