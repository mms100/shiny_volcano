# shiny_volcano
This app makes volcano plots from DEGs tables and lets you tweak them interactively

# prerequisite packages to be installed in R studio

**#Install CRAN packages**

install.packages(c("shiny", "DT", "plotly"))

**#Install Bioconductor and EnhancedVolcano if not already installed**

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("EnhancedVolcano")


# How it works

after installing all needed packages download the app.R file and open it in Rstudio and press > Run App



