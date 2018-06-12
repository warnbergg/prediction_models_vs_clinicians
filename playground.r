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
## Create table of sample characteristics
tables <- generate.sample.characteristics.tbl(study_data,
                                              data_dictionary)
## Add tables and table characteristics to results
results$table_of_sample_characteristics <- tables$formatted
results$raw_table_of_sample_characteristics <- tables$raw
results$n_training_sample <- nrow(prepped_sample$sets$x_train)
results$n_test_sample <- nrow(prepped_sample$sets$x_review)n
## Define model names and characteristics list
model_names <- c("RTS",
                 "GAP",
                 "KTS",
                 "gerdin")
models <- list(model_names = model_names,
               modelling_names = unlist(lapply(model_names,
                                               function(name) paste0("model.", name))),
               by_seqs = list(RTS = 0.5,
                              GAP = 1,
                              gerdin = 0.01,
                              KTS = 1))
## Generate model scores
preds <- lapply(setNames(models$modelling_names, nm = model_names),
                function(func_name)
                {
                    fun <- get(func_name)
                    fun(study_data)
                }
                )
outcome <- study_data$s30d; levels(outcome) <- c(0,1)
## Bin model predictions
binned_preds <- lapply(setNames(models$model_names, nm = models$model_names),
                       function(model_name) bin.models(preds[[model_name]],
                                                       outcome,
                                                       models$by_seqs[[model_name]]))
str(binned_preds)
## Convert to numeric preds
num_preds <- lapply(binned_preds,
                    function(pred) {
                        levels(pred) <- c("1","2","3","4")
                        as.numeric(pred)
                    }
                    )
## Add outcome and tc to preds list
num_preds$s30d <- as.numeric(as.character(outcome))
num_preds$tc <- as.numeric(study_data$tc)
## Save binned preds to disk
saveRDS(num_preds, "num_preds")
## List analysis
test <- SupaLarna::model.review.AUROCC(num_preds,
                                       which_preds = "gerdin",
                                       outcome_name = "s30d")
