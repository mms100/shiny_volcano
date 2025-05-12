# shiny_volcano
This app makes volcano plots from DEGs tables and lets you tweak them interactively

**#prerequisite packages to be installed in R studio
**
install.packages(c("shiny", "DT", "plotly"))
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("EnhancedVolcano")
