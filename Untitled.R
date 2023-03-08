library(haven)
library(corrplot)
library(psych)
library(corrplot)
library("psych")
library(ggplot2)
library(car)
library(xtable)

# Disable scientific notation
options(scipen=999)

# From https://www.franciscowilhelm.com/post/exploratory-factor-analysis-table/
print_lm_analysis <- function(fa, task, slicing, rv) {
     fa_with_rv <- cbind(fa$scores, task, slicing, rv)
     model = lm(rv~., as.data.frame(fa_with_rv))
     summary(model)
}

# Load dataset
path <- file.path("./", "", "Datos_SPSS.sav")
full_data <- read_sav(path)

# Not all variables are necessary
reduced_data <- full_data[full_data$UNIT_TESTING_FRAMEWORK2_ADAPTED == 3, c(6, 12, seq(26, 49), seq(59, 62), seq(67, 70), seq(82, 87))]

# We use complete cases only (this avoid problems with the correlation matrices)
reduced_data <- reduced_data[complete.cases(reduced_data), ]

data_for_correlation_likert <- reduced_data[, c(seq(1, 2), 6, 7, 8, 12, 13, 14, 15, seq(22, 25), 35, 36, 37, 38, 39, 40)]
data_for_correlation_years <- reduced_data[, c(seq(1, 2), 3, 4, 5, 9, 10, 11, 15, 16, 17, 18, 19, 20, 21, seq(22, 25))]

# We create four datasets, containing variables measured on the same scales and abstraction levels
dataset1_simplified_likert <- reduced_data[, c(seq(1, 2), 6, 12, 15, seq(22, 25), 35, 38)]
dataset2_simplified_years  <- reduced_data[, c(seq(1, 2), 3, 9, 15, 16, 19, seq(22, 25))]
dataset3_detailed_likert   <- reduced_data[, c(seq(1, 2), 7,  8, 13, 14, 15, seq(22, 25), 36, 37, 39, 40)]
dataset4_detailed_years    <- reduced_data[, c(seq(1, 2), 4,  5, 10, 11, 15, 17, 18, 20, 21, seq(22, 25))]

# EFA analyses
dataset1_simplified_likert_nfact <- fa.parallel(dataset1_simplified_likert, fa = "fa")$nfact
dataset2_simplified_years_nfact <- fa.parallel(dataset2_simplified_years, fa = "fa")$nfact
dataset3_detailed_likert_nfact <- fa.parallel(dataset3_detailed_likert, fa = "fa")$nfact
dataset4_detailed_years_nfact <- fa.parallel(dataset4_detailed_years, fa = "fa")$nfact

fa_dataset1_simplified_likert <- fa(r=dataset1_simplified_likert, 
                                    nfactors = dataset1_simplified_likert_nfact, 
                                    fm="ols", # principal axis factoring
                                    max.iter=100, # 50 is the default
                                    rotate="Promax")

fa_dataset2_simplified_years <- fa(r=dataset2_simplified_years, 
                                   nfactors = dataset2_simplified_years_nfact, 
                                   fm="ols", # principal axis factoring
                                   max.iter=100, # 50 is the default
                                   rotate="Promax")

fa_dataset3_detailed_likert <- fa(r=dataset3_detailed_likert, 
                                  nfactors = dataset3_detailed_likert_nfact, 
                                  fm="ols", # principal axis factoring
                                  max.iter=100, # 50 is the default
                                  rotate="Promax")

fa_dataset4_detailed_years <- fa(r=dataset4_detailed_years, 
                                 nfactors = dataset4_detailed_years_nfact, 
                                 fm="ols", # principal axis factoring
                                 max.iter=100, # 50 is the default
                                 rotate="Promax")


# Factor graph
fa.diagram(fa_dataset1_simplified_likert, digits=2)




# does any of the latent variables predict the data?
qlty_itld <- reduced_data$QLTY_ITL
prod_itld <- reduced_data$PROD_ITL
qlty_tdd <- reduced_data$QLTY_TDD1
prod_tdd <- reduced_data$PROD_TDD1

print("----------------------------------------------------")

print_lm_analysis(fa_dataset1_simplified_likert, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_itld)
print_lm_analysis(fa_dataset1_simplified_likert, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, prod_itld)
print_lm_analysis(fa_dataset1_simplified_likert, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_tdd)
print_lm_analysis(fa_dataset1_simplified_likert, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_tdd)

print("----------------------------------------------------")

print_lm_analysis(fa_dataset2_simplified_years, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_itld)
print_lm_analysis(fa_dataset2_simplified_years, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, prod_itld)
print_lm_analysis(fa_dataset2_simplified_years, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_tdd)
print_lm_analysis(fa_dataset2_simplified_years, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_tdd)

print("----------------------------------------------------")

print_lm_analysis(fa_dataset3_detailed_likert, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_itld)
print_lm_analysis(fa_dataset3_detailed_likert, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, prod_itld)
print_lm_analysis(fa_dataset3_detailed_likert, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_tdd)
print_lm_analysis(fa_dataset3_detailed_likert, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_tdd)

print("----------------------------------------------------")

print_lm_analysis(fa_dataset4_detailed_years, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_itld)
print_lm_analysis(fa_dataset4_detailed_years, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, prod_itld)
print_lm_analysis(fa_dataset4_detailed_years, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_tdd)
print_lm_analysis(fa_dataset4_detailed_years, reduced_data$TASK_ITL, reduced_data$SLICED_ITL_DUMMY, qlty_tdd)

