### Relations among Phonological Processing Skills and Mathematics in Children: A Meta-analysis ###
# Purpose: double check Amy's analysis and some extra analysis required by the reviewer

library(metafor)
library(tidyverse)
library(multcomp)
library(corrr)


# Since we need to break down PM and RAN into digit and non-digit and "final" don't have such info
# Creating functions for converting correlation to long format 

cor_to_long <- function(cor, xs, ys=c('math_fluency', 'math_accuracy')){
  # Helper function for compiling one correlation table to pair-wise correlation
  # Since we have 2 DVs categories, fluency vs. math_acc and 
  # basic, cal, vs. word problem. To avoid duplication, I will just get all DVs pair
  # with math_fluency and accuracy. 
  
  long_cor <- cor %>%
    as_cordf() %>% 
    stretch() %>%
    filter(x %in% xs) %>%
    filter(y %in% ys) %>%
    na.omit()
  
}

get_long_cor <- function(df, x){
  # Get entire data set that contain x vs. all DVs (all 5 maths)
  output <- data.frame()
  ids<- df$between_df$id
  
  for (id in ids){
    this_long_cor <- cor_to_long(df$correlations[[id]], x)
    
    if (nrow(this_long_cor) > 0) {
      # Merge into main data set
      output <- rbind(output, cbind(id, this_long_cor))
    }
    
  }
  
  output <- output %>% left_join(df$between_df, by = "id")
  output <- escalc("ZCOR", ri = r, ni = n, data = output, slab = study)
  return(output)
}


# Ingest data 
df <- readRDS('tables/df.rds')
dat <- read.csv('main_data_201224.csv')
dat_bias <- readRDS("tables/dat_bias.rds")


#### Reject outlier (It run quite long, I have save the results in dat_bias.rds) ####

# Ignore study structure
# dat <- escalc("ZCOR", ri = r, ni = n, data = dat, slab = study)
# res_uv <- rma(yi, vi, data = dat)
# inf_uv <- influence(res_uv)

# Save univariate influence plot
# png("influence_univariate.png", width = 1000)
# plot(inf_uv)
# dev.off()

# Which studies are outliers?
# id_outliers <- which(inf_uv$inf$inf == "*")
# id_outliers

#### Egger's test for publication bias ####
# Reference for Egger test on rma.mv: https://dylancraven.github.io/MetaAnalysis_Course/
# - Basically, just add indicator in mod is equivalent to Egger's test
# - Indicator can be: vi, standard error: sqrt(vi)... see regtest() for details
# - Related discussion in Cross-validated: https://stats.stackexchange.com/questions/155693/metafor-package-bias-and-sensitivity-diagnostics
# egger_mv <- rma.mv(yi, vi, mod = I(sqrt(vi)), random = ~ 1 | study, data = dat, slab = study)
# summary(egger_mv)

# Results of Egger's test
# To test for publication bias, sampling standard error was used as the indicator in the Egger's regression test, 
# result indicates the publication bias is not significant, z=-0.11, p=0.92

# dat_bias <- dat[-id_outliers, ]
# dat_bias <- escalc("ZCOR", ri = r, ni = n, data = dat_bias)
# names(dat_bias)[1] <- 'id'
# saveRDS(dat_bias, "dat_bias.rds")


#### Table 1 ####
# see http://www.metafor-project.org/doku.php/analyses:konstantopoulos2011

# Subsetting data in each categorization
dat_math11 <- dat_bias %>%  filter(math1 == 1)  # Basic number knowledge
dat_math12 <- dat_bias %>%  filter(math1 == 2)  # Calculation
dat_math13 <- dat_bias %>%  filter(math1 == 3)  # Word problem
dat_math21 <- dat_bias %>%  filter(math2 == 1)  # Accuracy
dat_math22 <- dat_bias %>%  filter(math2 == 2)  # Fluency
dat_pp1 <- dat_bias %>% filter(PP == 1)  # PA
dat_pp2 <- dat_bias %>% filter(PP == 2)  # RAN
dat_dran <- get_long_cor(df, 'dran')
dat_ndran <- get_long_cor(df, 'ndran')
dat_pp3 <- dat_bias %>% filter(PP == 3)  # PM
dat_dpm <- get_long_cor(df, 'dpm')  
dat_ndpm <- get_long_cor(df, 'ndpm')
dat_sample1 <- dat_bias %>%  filter(sample == 1)  # TD
dat_sample2 <- dat_bias %>%  filter(sample == 2)  # Atypical
dat_sample4 <- dat_bias %>% filter(sample == 4)  # other diseases
dat_grade1 <- dat_bias %>%  filter(grade == 1)  # K1-K3
dat_grade2 <- dat_bias %>%  filter(grade == 2)  # G1-G3
dat_grade3 <- dat_bias %>%  filter(grade == 3)  # G4-G6
dat_design1 <- dat_bias %>%  filter(design == 1)  # Concurrent
dat_design2 <- dat_bias %>%  filter(design == 2)  # Longitudinal

df_list_table1 <- list(dat_bias, dat_math11, dat_math12, dat_math13,
                       dat_math21, dat_math22, dat_pp1, dat_pp2, dat_dran, 
                       dat_ndran, dat_pp3, dat_dpm, dat_ndpm, 
                       dat_sample1, dat_sample2, dat_sample4,
                       dat_grade1, dat_grade2,
                       dat_grade3, dat_design1, dat_design2)

name_list_table1 <- c("Overall",
                      "Math1: Basic number knowledge",
                      "Math1:Calculation", 
                      "Math1:Word problem",
                      "Math2: Accuracy", 
                      "Math2: Fluency",
                      "PP: PA", 
                      "PP: RAN", 
                      "PP (New): DRAN",
                      "PP (New): NDRAN",
                      "PP: PM",
                      "PP (New): DPM",
                      "PP (New): NDPM",
                      "Sample: TD", "Sample: Atypical", "Sample: Other",
                      "Grade: K1-K3", "Grade: G1-G3", "Grade: G4-G6",
                      "Design: Concurrent", "Design: Longitudinal")

extract_table1_stat <- function(df) {
  # This function can return a row in table 1
  # k, r, 95%CI lower-bound, upper-bound, and sigma^2 (multivariate version of Tau^2)
  res <- rma.mv (yi, vi, random = ~ 1 | study, data = df)
  print(summary(res))
  rpred <- predict(res, transf = transf.ztor)
  k <- res$k
  r <- round(rpred$pred, 2)
  lb <- round(rpred$ci.lb, 2)
  ub <- round(rpred$ci.ub, 2)
  s2 <- round(res$sigma2, 2)
  return(data.frame(k, r, lb, ub, s2))
  
}


results1 <- lapply(df_list_table1, extract_table1_stat) %>% bind_rows()
results1$name <- name_list_table1
write.csv(results1, "table1_201231.csv")

#### Table 2 (With no covariates) ####

# Create pairwise dataframe
datpair_math1_12 <- rbind(dat_math11, dat_math12) %>% mutate(mod_var = math1)
datpair_math1_23 <- rbind(dat_math12, dat_math13) %>% mutate(mod_var = math1)
datpair_math1_13 <- rbind(dat_math11, dat_math13) %>% mutate(mod_var = math1)
datpair_math2 <- rbind(dat_math21, dat_math22) %>% mutate(mod_var = math2)
datpair_pp_12 <- rbind(dat_pp1, dat_pp2) %>% mutate(mod_var = PP)
datpair_pp_23 <- rbind(dat_pp2, dat_pp3) %>% mutate(mod_var = PP)
datpair_pp_13 <- rbind(dat_pp1, dat_pp3) %>% mutate(mod_var = PP)
datpair_ran <- rbind(dat_dran, dat_ndran) %>% mutate(mod_var = x)
datpair_pm <- rbind(dat_dpm, dat_ndpm) %>% mutate(mod_var = x)
datpair_sample_12 <- rbind(dat_sample1, dat_sample2) %>% mutate(mod_var = sample)
datpair_sample_14 <- rbind(dat_sample1, dat_sample4) %>% mutate(mod_var = sample)
datpair_sample_24 <- rbind(dat_sample2, dat_sample4) %>% mutate(mod_var = sample)

datpair_grade_12 <- rbind(dat_grade1, dat_grade2) %>% mutate(mod_var = grade)
datpair_grade_23 <- rbind(dat_grade2, dat_grade3) %>% mutate(mod_var = grade)
datpair_grade_13 <- rbind(dat_grade1, dat_grade3) %>% mutate(mod_var = grade)
datpair_design <- rbind(dat_design1, dat_design2) %>% mutate(mod_var = design)

df_list_table2 <- list(datpair_math1_12, datpair_math1_23, datpair_math1_13,
                       datpair_math2, datpair_pp_12, datpair_pp_23, datpair_pp_13,
                       datpair_ran, datpair_pm,
                       datpair_sample_12, datpair_sample_14, datpair_sample_24, 
                       datpair_grade_12, datpair_grade_23, datpair_grade_13,
                       datpair_design)


name_list_table2 <- c("Math1: Basic vs. Calculation", 
                      "Math1: Calculation vs. Word",
                      "Math1: Basic vs. Word",
                      "Math2: Accuracy vs. Fluency",
                      "PP: PA vs. RAN",
                      "PP: RAN vs. PM",
                      "PP: PA vs. PM",
                      "RAN: Digit vs. Non-digit",
                      "PM: Digit vs. Non-digit",
                      "Sample: TD vs. Atypical",
                      "Sample: TD vs. Other",
                      "Sample: Atypical vs. Other",
                      "Grade: K1-K3 vs. G1-G3",
                      "Grade: G1-G3 vs. G4-G6",
                      "Grade: K1-K3 vs. G4-G6",
                      "Design: Concurrent vs. Longitudinal",
                      "Age")

# Function for pairwise statistics (With no covariate)
extract_table2_stat <- function(df) {
  modCat <- rma.mv (yi, vi, random = ~ 1 | study, mods = ~ (as.factor(mod_var)), data = df)
  print(summary(modCat))
  beta <- round(modCat$beta[2], 2)
  se <- round(modCat$se[2], 2)
  z <- round(modCat$zval[2], 2)
  p <- modCat$pval[2]
  lb <- round(modCat$ci.lb[2], 2)
  ub <- round(modCat$ci.ub[2], 2)
  return(data.frame(beta, se, z, p, lb, ub))
  
}

results2 <- lapply(df_list_table2, extract_table2_stat)

# Extra age as continuous regressor
modAge <-  rma.mv (yi, vi, random = ~ 1 | study, mods = age, data = dat_bias)
beta <- round(modAge$beta[2], 2)
se <- round(modAge$se[2], 2)
z <- round(modAge$zval[2], 2)
p <- modAge$pval[2]
lb <- round(modAge$ci.lb[2], 2)
ub <- round(modAge$ci.ub[2], 2)
results_age <- data.frame(beta, se, z, p, lb, ub)

results2 <- bind_rows(results2, results_age) 
results2$name <- name_list_table2
write.csv(results2, "table2_201231.csv")

#### Table 3 (With no covariates) ####

# Basically rerun table 2 with PP==1
# It will reuse the dataframes created in Table 2, must be run in sequence

extract_table345_stat <- function(pp_filter, fun) {
  # A slight modification of whole table 2 routine...
  
  # Create pairwise data frame with pp_filter
  datpair_math1_12 <- datpair_math1_12 %>% filter(PP == pp_filter)
  datpair_math1_23 <- datpair_math1_23 %>% filter(PP == pp_filter)
  datpair_math1_13 <- datpair_math1_13 %>% filter(PP == pp_filter)
  datpair_math2 <- datpair_math2 %>% filter(PP == pp_filter)
  datpair_sample_12 <- datpair_sample_12 %>% filter(PP == pp_filter)
  datpair_sample_24 <- datpair_sample_24 %>% filter(PP == pp_filter)
  datpair_sample_14 <- datpair_sample_14 %>% filter(PP == pp_filter)
  datpair_grade_12 <- datpair_grade_12 %>% filter(PP == pp_filter)
  datpair_grade_23 <- datpair_grade_23 %>% filter(PP == pp_filter)
  datpair_grade_13 <- datpair_grade_13 %>% filter(PP == pp_filter)
  datpair_design <- datpair_design %>% filter(PP == pp_filter)
  
  df_list <- list(
    datpair_math1_12,
    datpair_math1_23,
    datpair_math1_13,
    datpair_math2,
    datpair_sample_12,
    datpair_sample_24,
    datpair_sample_14,
    datpair_grade_12,
    datpair_grade_23,
    datpair_grade_13,
    datpair_design
  )
  
  name_list <- c("Math1: Basic vs. Calculation", 
                 "Math1: Calculation vs. Word",
                 "Math1: Basic vs. Word",
                 "Math2: Accuracy vs. Fluency",
                 "Sample: TD vs. Atypical",
                 "Sample: TD vs. Other",
                 "Sample: Atypical vs. Other",
                 "Grade: K1-K3 vs. G1-G3",
                 "Grade: G1-G3 vs. G4-G6",
                 "Grade: K1-K3 vs. G4-G6",
                 "Design: Concurrent vs. Longitudinal",
                 "Age")
  
  results <- lapply(df_list, fun)
  
  # Extra age as continuous regressor
  modAge <- rma.mv(yi, vi, random = ~ 1 | study, mods = age,
                   data = dat_bias %>% filter(PP == pp_filter))
  print(summary(modAge))
  
  beta <- round(modAge$beta[2], 2)
  se <- round(modAge$se[2], 2)
  z <- round(modAge$zval[2], 2)
  p <- modAge$pval[2]
  lb <- round(modAge$ci.lb[2], 2)
  ub <- round(modAge$ci.ub[2], 2)
  results_age <- data.frame(beta, se, z, p, lb, ub)
  out <- bind_rows(results, results_age)
  out$name <- name_list
  return(out)
  
}

extract_table345_stat(1, extract_table2_stat) %>% write.csv("table3_201231.csv")


#### Table 4 ####
extract_table345_stat(2, extract_table2_stat) %>% write.csv("table4_201231.csv")


#### Table 5 ####
extract_table345_stat(3, extract_table2_stat) %>% write.csv("table5_201231.csv")


