# List of required packages
packages <- c("shiny", "tidyr", "DT", "ggplot2", "dplyr", "purrr", "scales")

# Check if packages are installed and install if not
invisible(lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}))

# Invoke libraries
library(shiny)
library(tidyr)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)
library(scales)
