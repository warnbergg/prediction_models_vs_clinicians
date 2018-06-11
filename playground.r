## This is a file for testing

## Source all functions (remove when turned into package)
files <- list.files("./R", pattern = ".r$", full.names = TRUE)
for (f in files) source(f)
## Set parameters that are default in make.study
data_path =  c("../data/data_can_prediction_models_vs_clinicians/sample.csv")
bs_samples = 4
## Import data
data <- read.csv(data_path, stringsAsFactors = FALSE)
