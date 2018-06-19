## This is a file for testing
## Source all functions (remove when turned into package)
files <- list.files("./R", pattern = ".r$", full.names = TRUE)
for (f in files) source(f)
## Set parameters and initialize results list
data_path =  c("../data/sample.csv")
bs_samples = 4
results = list()
## Import data
study_data <- read.csv(data_path, stringsAsFactors = FALSE)
## Get data dictionary
data_dictionary <- SupaLarna::get.data.dictionary()
## Keep relevant variables
study_data <- SupaLarna::keep.relevant.variables(study_data,
                                                 data_dictionary = data_dictionary)
## Define 999 as missing
study_data[study_data == 999] <- NA
## Create complete case dataset
study_data <- complete.dataset(study_data)
## Prepare study_data using the data dictionary, i.e
## transform variables to factors
study_data <- SupaLarna::prepare.study.data(study_data,
                                            data_dictionary,
                                            is_seqn = FALSE)
## Define variabels to transform from factor to numeric
factors_to_numeric <- c("egcs",
                        "mgcs",
                        "vgcs",
                        "avpu")
## Transform some factors to numeric
study_data[, factors_to_numeric] <- lapply(study_data[, factors_to_numeric],
                                            function(comp) as.numeric(comp))
## Collapse gcs
study_data$gcs <- with(study_data, egcs + mgcs + vgcs)
## ## Set patients to dead if dead at discharge or at 24 hours
## and alive if coded alive and admitted to other hospital
study_data <- SupaLarna::set.to.outcome(study_data)
## Exclude those with missing data in outcome and triage category
## as well as those who did not provide consent to inclusion
study_data <- SupaLarna::apply.exclusion.criteria(study_data)
## Collapse mechanism of injury
study_data <- SupaLarna::collapse.moi(study_data)
## Generate sample characterstics table (to be inserted)
## Define model_names
model_names <- c("RTS",
                 "GAP",
                 "KTS",
                 "gerdin")
## Generate model predictions
predictions <- generate.model.predictions(study_data,
                                          model_names,
                                          n_cores = 4,
                                          write_to_disk = TRUE,
                                          gridsearch_parallel = TRUE,
                                          )
## Generate boostrap samples
samples <- SupaLarna::generate.bootstrap.samples(study_data,
                                                 bs_samples = 3)
## Generate predictions on bootstrap samples
bootstrap_predictions <- SupaLarna::generate.predictions.bssamples(samples,
                                                                   prediction_func = "generate.model.predictions",
                                                                   parallel = TRUE,
                                                                   n_cores = 4,
                                                                   log = TRUE,
                                                                   boot = TRUE,
                                                                   write_to_disk = TRUE)
## Create list of names of binned and contionous predictions
binned_and_continous <- list(names = list(cut_models = unlist(lapply(model_names,
                                                                        paste0, "_cut")),
                                          con_models = unlist(lapply(model_names,
                                                                     paste0, "_con"))))
## Intialize analysis list
analysis_lst <- list()
analysis_lst$AUROCC <- lapply(binned_and_continous$names,
                              function(names)
                                  SupaLarna::generate.confidence.intervals(
                                                 predictions,
                                                 model_names = names,
                                                 the_func = SupaLarna::model.review.AUROCC,
                                                 samples = bootstrap_predictions,
                                                 diffci_or_ci = "ci",
                                                 outcome_name = "outcome_cut"))
analysis_lst$reclassification <- SupaLarna::generate.confidence.intervals(
                                                predictions,
                                                model_names = binned_and_continous$names$cut_models,
                                                the_func = SupaLarna::model.review.reclassification,
                                                samples = bootstrap_predictions,
                                                diffci_or_ci = "ci",
                                                outcome_name = "outcome_cut"
                                            )



