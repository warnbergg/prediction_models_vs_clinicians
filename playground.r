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
## Collapse mechanism of injury
study_data <- SupaLarna::collapse.moi(study_data)
## Set study_data, i.e. remove patients arriving prior to one month
## before the dataset were created, remove patients before 2016-07-28
## when hospital collected tc. Also, complete dataset for analysis and
## "all" tbl for tbl one.
cc_and_all <- set.data(study_data)
all <- cc_and_all$all
study_data <- cc_and_all$cc
## Generate table of sample characteristics
tables <- generate.tbl.one(all, data_dictionary)
## Generate sample characterstics table (to be inserted)
## Define model_names
model_names <- c("RTS",
                 "GAP",
                 "KTS",
                 "gerdin")
##Initialize cut_points_lst
results$cut_points_lst <- list()
## Generate model predictions
predictions <- generate.model.predictions(study_data,
                                          model_names,
                                          n_cores = 4,
                                          write_to_disk = TRUE,
                                          gridsearch_parallel = TRUE)
## Rename cut_points according to model_names
names(results$cut_points_lst) <- model_names
## Generate boostrap samples
samples <- SupaLarna::generate.bootstrap.samples(study_data,
                                                 bs_samples = 3)
## Generate predictions on bootstrap samples
bootstrap_predictions <- SupaLarna::generate.predictions.bssamples(
                                        samples,
                                        prediction_func = "generate.model.predictions",
                                        parallel = TRUE,
                                        n_cores = 4,
                                        log = TRUE,
                                        boot = TRUE,
                                        write_to_disk = TRUE)
## Define names of models
suffixes <- c("_cut", "_con")
models_w_suffixes <- unlist(lapply(suffixes,
                                   function(suffix) paste0(model_names, suffix)))
## Intialize analysis list
analysis_lst <- list()
analysis_lst$AUROCC <- SupaLarna::generate.confidence.intervals(
                                      predictions,
                                      model_names = models_w_suffixes,
                                      the_func = SupaLarna::model.review.AUROCC,
                                      samples = bootstrap_predictions,
                                      diffci_or_ci = "ci",
                                      outcome_name = "outcome")
analysis_lst$reclassification <- SupaLarna::generate.confidence.intervals(
                                                predictions,
                                                model_names = grep("_cut",
                                                                   models_w_suffixes,
                                                                   value = TRUE),
                                                the_func = SupaLarna::model.review.reclassification,
                                                samples = bootstrap_predictions,
                                                diffci_or_ci = "ci",
                                                outcome_name = "outcome")
## Append analysis_lst to results
results$Analysis <- analysis_lst
## Save results to disk
saveRDS(results, file = "results.rds")
## Save plots to disk
## ROC-curves
SupaLarna::create.ROCR.plots(study_sample = predictions,
                             outcome_name = "outcome",
                             split_var = "con",
                             train_test = FALSE,
                             ROC_or_precrec = "ROC",
                             device = "pdf",
                             models = models_w_suffixes,
                             pretty_names = c("RTS_cat",
                                              "GAP_cat",
                                              "KTS_cat",
                                              "Gerdin_cat",
                                              "RTS_con",
                                              "GAP_con",
                                              "KTS_con",
                                              "Gerdin_con"))
## Precision/recall curves
SupaLarna::create.ROCR.plots(study_sample = predictions,
                             outcome_name = "outcome",
                             split_var = "con",
                             train_test = FALSE,
                             ROC_or_precrec = "prec_rec",
                             device = "pdf",
                             models = models_w_suffixes,
                             pretty_names = models_w_suffixes)
