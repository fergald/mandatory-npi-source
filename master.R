library(RStata)
library(stringr)

path_script <- str_c(getwd(), "/script/")
path_stata <- "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp"
version_stata <- 16

# Initial data processing for the additional 5 countries (England, Germany, Netherlands, Spain, Sweden)
source(str_c(path_script, "npi_processing.R"))


# Inidividual country regression
stata(
  src = str_c(path_script, "master_pt1.do"),
  stata.path = path_stata,
  stata.version = version_stata
)

# Get the comparison countries paired with the reference countries (SWE, KOR)
source(str_c(path_script, "pairing_countries.R"))

# Paired regression
stata(
  src = str_c(path_script, "master_pt2.do"),
  stata.path = path_stata,
  stata.version = version_stata
)

# Collect regression results and plot
source(str_c(path_script, "npi_figures.R"))
