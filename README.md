# Covid19AbOmic

The Rscripts folder contains the codes we used for the analysis

The data is contained in the dataset folder

The output folder contains results used for plotting


Covid19AbOmic codes:
1. LassoRF_SVM.R
-- This script calculates the AUC for number of replication in a (LASSO+SVM) cross-validation framework.
2. LassoRF_SVM_PermutedModel.R
-- This script calculates the AUC for the permuted/shuffled data for number of replication in a (LASSO+SVM) cross-validation framework.
3. Plot_Box_AUC_fcProfile.R
-- This script plots all the actual vs permuted AUC plots
4. Plot_PLS_LassoSelected_Features.R
-- This script plots the partial least square regression on the lasso selected features
5. plot_box_ICU_admission.R
-- This script plots the distribution of ICU admission of survivors and non-survivors.
6. plot_Other_ARDS.R
-- This script plots other ARDS patients LASSO selected features.
7. polarplots coloured by NSSH.r
-- This script plots the polarplots of the canonical and non-canonical features.
