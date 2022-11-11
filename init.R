# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("ggplot2", "tidyverse", "ggwordcloud", "tidytext", "textdata", "tidyquant", "tidyr", "stringr", "plotly", "shiny", "shinythemes", "gtrendsR", "lubridate", "data.table", "stopwords", "qdapRegex", "ggiraph", "ggthemes")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
