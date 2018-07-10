## This is a file for testing
## Source all functions (remove when turned into package)
files <- list.files("./R", pattern = ".r$", full.names = TRUE)
for (f in files) source(f)
## Set parameters and initialize results list
data_path <- c("../data/sample.csv")
bs_samples <- 4
results <- list()
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
## Append samples to results
results$samples <- cc_and_all
## Generate table of sample characteristics and save to disk
tables <- generate.tbl.one(all, data_dictionary)
## Append tables to results
results$tables <- tables
## Generate sample characterstics table (to be inserted)
## Define model_names
model_names <- c("RTS",
                 "GAP",
                 "KTS",
                 "gerdin")
## Define pretty model names
pretty_model_names <- c("RTS",
                        "GAP",
                        "KTS",
                        "Gerdin et al.")
## Initialize cut_points_lst
results$cut_points_lst <- list()
## Generate model predictions
predictions <- generate.model.predictions(study_data,
                                          model_names,
                                          n_cores = 4,
                                          write_to_disk = TRUE,
                                          gridsearch_parallel = TRUE)
## Rename cut_points according to model_names
names(results$cut_points_lst) <- pretty_model_names
## Save cut points table
cut_points_table <- generate.cut.points.table(cut_points = results$cut_points_lst)
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
## Define suffixes to be added to models
suffixes <- c("_CUT", "_CON")
## Define clinicians labels for regular and pretty names
clinicians_names <- c("tc", "Clinicians Priority Level")
## Create names list for loop
lst_w_names <- setNames(list(model_names, pretty_model_names),
                        nm = c("names", "pretty_names"))
## Paste suffixes to names, and bind triage category
names_lst <- lapply(setNames(seq_along(lst_w_names), nm = names(lst_w_names)),
                    function (model_lst, names, i){
                        ## Paste suffixes to both pretty and non-pretty model
                        ## names
                        lst <- lapply(suffixes, function(suffix){
                            paste0(model_lst[[i]], suffix)
                        })
                        ## Unlist to vector and bind tc to models
                        new_names <- c(unlist(lst), clinicians_names[i])

                        return (new_names)
                    }, model_lst = lst_w_names, names = names(lst_w_names))
## Add AUC_ci and AUC_diff list
AUC_ci <- list(models = names_lst$names,
               ci_type = "ci")
AUC_diff <- list(models = names_lst$names,
                 ci_type = "diff")
AUC_lst <- setNames(list(AUC_ci, AUC_diff), nm = c("AUC (95 \\%)",
                                                   "95 \\% CI on AUC-difference"))
## Intialize analysis list
analysis_lst <- list()
analysis_lst$AUROCC <- lapply(AUC_lst, function(AUC){
    if (AUC$ci_type == "ci"){
        SupaLarna::generate.confidence.intervals(
                       predictions,
                       model_names = AUC$models,
                       the_func = SupaLarna::model.review.AUROCC,
                       samples = bootstrap_predictions,
                       diffci_or_ci = AUC$ci_type,
                       outcome_name = "outcome")
    } else {
        lapply(setNames(nm = AUC$models), function (model_name){
           SupaLarna::generate.confidence.intervals(
                          predictions,
                          model_names = c(model_name,
                                          "tc"),
                          the_func = SupaLarna::model.review.AUROCC,
                          samples = bootstrap_predictions,
                          diffci_or_ci = AUC$ci_type,
                          outcome_name = "outcome")})
    }
})
analysis_lst$reclassification <- SupaLarna::generate.confidence.intervals(
                                                predictions,
                                                model_names = grep("_CUT",
                                                                   names_lst$names,
                                                                   value = TRUE),
                                                the_func = SupaLarna::model.review.reclassification,
                                                samples = bootstrap_predictions,
                                                diffci_or_ci = "ci",
                                                outcome_name = "outcome")
## Append analysis list to results
results$Analysis <- analysis_lst
## Initialize list for estimate tables
auc_table <- list(table_data = t(do.call(rbind, lapply(analysis_lst$AUROCC,
                                                       generate.estimate.table,
                                                       pretty_names = names_lst$pretty_names))),
                  label = "auc",
                  caption = "AUC estimates and difference of model and clinicians AUC, both with corresponding confidence interval (95 \\%)",
                  file_name = "auc_estimates_table.tex",
                  san_col = function (word) {word},
                  san_row = function (word) {word})
reclassification_table <- list(table_data = generate.estimate.table(analysis_lst$reclassification,
                                                                    pretty_names = names_lst$pretty_names[grep("_CUT", names_lst$pretty_names)]),
                               label = "reclassification",
                               caption = "Estimates of reclassification and corresponding confidence intervals (95 \\%)",
                               file_name = "reclassification_estimates_table.tex",
                               san_col = function (word) {word},
                               san_row = NULL)
## Add tables
table_lst <- list(auc_table = auc_table,
                  reclassification_table = reclassification_table)
## Save results tables
for (lst in table_lst){
    with(lst, make.and.save.xtable(table_data = table_data,
                                   caption = caption,
                                   label = label,
                                   file_name = file_name,
                                   san_col = san_col,
                                   san_row = san_row))
}
## Save estimate tables to results
results$estimate_tables <- table_lst
## Save results to disk
saveRDS(results, file = "results.rds")
## Save plots to disk
## ROC-curves
SupaLarna::create.ROCR.plots(study_sample = predictions,
                             outcome_name = "outcome",
                             split_var = "CON",
                             train_test = FALSE,
                             ROC_or_precrec = "ROC",
                             device = "pdf",
                             models = names_lst$names,
                             pretty_names = names_lst$pretty_names,
                             subscript = TRUE)
## Precision/recall curves
SupaLarna::create.ROCR.plots(study_sample = predictions,
                             outcome_name = "outcome",
                             split_var = "CON",
                             train_test = FALSE,
                             ROC_or_precrec = "prec_rec",
                             device = "pdf",
                             models = names_lst$names,
                             pretty_names = names_lst$pretty_names,
                             subscript = TRUE)
