# Covid19AbOmic

The Rscripts folder contains the codes we used for the analysis

The data is contained in the dataset folder

The output folder contains results used for plotting


Covid19AbOmic codes:
1. LassoRF_SVM.R
-- This script calculates the AUC (survivors vs non-survivors) for number of replication in a (LASSO+SVM) cross-validation framework.
2. LassoRF_SVM_PermutedModel.R
-- This script calculates the AUC (survivors vs non-survivors) for the permuted/shuffled data for number of replication in a (LASSO+SVM) cross-validation framework.
3. Lasso_3_levels.R
-- This script calculates the AUC (survivors vs non-survivors vs healthy-control) for number of replication in a (LASSO+SVM) cross-validation framework.
4. Lasso_3_levels_Permuted.R
-- This script calculates the AUC (survivors vs non-survivors vs healthy-control) for the permuted/shuffled data for number of replication in a (LASSO+SVM) cross-validation framework.
5. Plot_Box_AUC_fcProfile.R
-- This script plots all the actual vs permuted AUC plots
6. Plot_PLS_LassoSelected_Features.R
-- This script plots the partial least square regression on the lasso selected features
7. plot_box_ICU_admission.R
-- This script plots the distribution of ICU admission of survivors and non-survivors.
8. plot_Other_ARDS.R
-- This script plots other ARDS patients LASSO selected features.
9. polarplots coloured by NSSH.r
-- This script plots the polarplots of the canonical and non-canonical features.
