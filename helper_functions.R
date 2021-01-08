library(tidyverse)
library(corrr)

clean_df <- function(cor_df, main_df) {
    # Collect between study information
    between_info_from_cor <- cor_df %>%
        select(id, study, n) %>%
        filter(id != "")
    
    between_info_from_main <- main_df %>%
        select(id, age, grade, sample, design) %>%
        distinct() %>%
        group_by(id) %>%
        summarise_all(mean, na.rm = T)
    
    all_info <- between_info_from_cor %>%
        left_join(between_info_from_main, by = 'id')
    # Caution: left_join must be use to maintain id sort consistency between cor & main
    
    # Collect correlation matrices
    # Trim non-correlation columns (as.character is needed for factor, just in case)
    cor_df <- cor_df[, 5:ncol(cor_df)] %>%
        mutate(across(everything(), as.character)) %>%
        mutate(across(everything(), as.numeric))
    
    # Check row is divisible by no. of variable
    n_lines <- nrow(cor_df)
    n_var <- ncol(cor_df)
    stopifnot(n_lines %% n_var == 0)
    n <- n_lines / n_var
    
    # Split to a list
    correlation_list <- split(cor_df, rep(1:n, each = n_var))
    
    # Labeling rows
    labeling <- function(x) {
        var_names <- c(
            'pa',
            'dpm',
            'ndpm',
            'dran',
            'ndran',
            'math_fluency',
            'math_accuracy',
            'number_knowledge',
            'calculation',
            'word_problem',
            'viq',
            'nviq',
            'vocab',
            'ef',
            'age'
        )
        dimnames(x) <- list(var_names, var_names)
        return(x)
    }
    correlation_list <- lapply(correlation_list, labeling)
    
    # Copy lower triangle to upper triangle
    copyl2u <- function(m) {
        m[upper.tri(m)] <- t(m)[upper.tri(m)]
        return(m)
    }
    
    correlation_list <- lapply(correlation_list, copyl2u)
    
    # Convert to matrix
    correlation_list <- lapply(correlation_list, as.matrix)
    
    # Attach study ids to output
    names(correlation_list) <- all_info$id
    
    return(list(correlations = correlation_list,
                between_df = all_info))
}

csv_to_meta_df <-
    function(main_csv,
             correlation_csv,
             output_meta_df = "df.rds") {
        main_df <- read.csv(main_csv)
        names(main_df)[1] <- 'id'
        
        cor_df <- read.csv(correlation_csv)
        names(cor_df)[1:3] <- c('study', 'n', 'id')
        
        df <- clean_df(cor_df, main_df)
        
        if (output_meta_df != FALSE) {
            saveRDS(df, file = output_meta_df)
        }
        
        return(df)
    }


remove_outlier_from_main_csv <- function(main_csv, save_file="dat_no_outlier.rds"){
    #### Reject outlier (It need to run for long time, a few hours on a laptop) ####
    
    # Ignore study structure
    dat <- read.csv(main_csv)
    dat <- escalc("ZCOR", ri = r, ni = n, data = dat, slab = study)
    res_uv <- rma(yi, vi, data = dat)
    inf_uv <- influence(res_uv)
    
    # Save univariate influence plot
    png("influence_univariate.png", width = 1000)
    plot(inf_uv)
    dev.off()
    
    # Which studies are outliers?
    id_outliers <- which(inf_uv$inf$inf == "*")
    
    dat_no_outlier <- dat[-id_outliers, ]
    dat_no_outlier <- escalc("ZCOR", ri = r, ni = n, data = dat_no_outlier)
    names(dat_no_outlier)[1] <- 'id'
    saveRDS(dat_no_outlier, save_file)
    return(dat_no_outlier)
    
}





aggregate_cor <- function(cor, sel_vars) {
    
    n <- length(sel_vars)
    
    long_cor <- cor %>%
        as_cordf() %>%
        stretch() %>%
        mutate(x_group = case_when(
            x %in% c('dpm', 'ndpm') ~ 'pm',
            x %in% c('dran', 'ndran') ~ 'ran',
            TRUE ~ as.character(x)
        )) %>%
        mutate(y_group = case_when(
            y %in% c('dpm', 'ndpm') ~ 'pm',
            y %in% c('dran', 'ndran') ~ 'ran',
            TRUE ~ as.character(y)
        )) %>%
        group_by(x_group, y_group) %>%
        summarise(r = mean(r, na.rm = T), .groups = "drop")
    
    get_cor <- function(cor, x, y) {
        cor$r[(cor$x_group==x) & (cor$y_group==y)]
    }
    
    # Reconstruct correlation matrix form aggregated long format
    cor <- diag(x = n)
    dimnames(cor) <- list(sel_vars, sel_vars)
    for (x in sel_vars) {
        for (y in sel_vars) {
            cor[x, y] <- get_cor(long_cor, x, y)
        }
    }
    
    # Tidy matrix format
    diag(cor) <- 1
    cor[is.nan(cor)] <- NA
    
    
    # Check if all DV (sel_vars[1]) related correlation are all missing
    max_miss <- n - 1
    if (sum(is.na(cor[sel_vars[1], ])) == max_miss) {
        return(NULL)
    } else {
        return(cor)
    }
    
}

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


extract_table1_stat <- function(df) {
    # This function can return a row in table 1
    # k, r, 95%CI lower-bound, upper-bound, and sigma^2 (multivariate version of Tau^2)
    res <- rma.mv (yi, vi, random = ~ 1 | study, data = df)
    print(summary(res))
    rpred <- predict(res, transf = transf.ztor)
    k <- res$k
    r <- round(rpred$pred, 3)
    lb <- round(rpred$ci.lb, 3)
    ub <- round(rpred$ci.ub, 3)
    s2 <- round(res$sigma2, 3)
    return(data.frame(k, r, lb, ub, s2))
}


# Function for pairwise statistics
extract_table_stat_categorical_mod <- function(df) {
    # Assume the moderator is mod_var
    modCat <-
        rma.mv (
            yi,
            vi,
            random = ~ 1 | study,
            mods = ~ (as.factor(mod_var)),
            data = df
        )
    print(summary(modCat))
    beta <- round(modCat$beta[2], 2)
    se <- round(modCat$se[2], 2)
    z <- round(modCat$zval[2], 2)
    p <- modCat$pval[2]
    lb <- round(modCat$ci.lb[2], 2)
    ub <- round(modCat$ci.ub[2], 2)
    return(data.frame(beta, se, z, p, lb, ub))
    
}

extract_table_state_continous_mod  <- function(df) {
    # Assume the continuous factor is age
    modAge <- rma.mv(yi,
                     vi,
                     random = ~ 1 | study,
                     mods = age,
                     data = df)
    print(summary(modAge))
    beta <- round(modAge$beta[2], 2)
    se <- round(modAge$se[2], 2)
    z <- round(modAge$zval[2], 2)
    p <- modAge$pval[2]
    lb <- round(modAge$ci.lb[2], 2)
    ub <- round(modAge$ci.ub[2], 2)
    return(data.frame(beta, se, z, p, lb, ub))
}


prep_input <- function(df, vars){
    ####
    parsed_cor <- lapply(df$correlations, aggregate_cor, sel_vars=vars)
    non_empty_idx <- lapply(parsed_cor, length) > 0
    this_cor <- parsed_cor[non_empty_idx]
    this_n <- df$between_df$n[non_empty_idx]
    return(list(cor=this_cor, n=this_n))
}

run_model <- function(df, dv, ivs, verbose=F, method='two_stage'){
    
    # Prep Data
    observed_variables <- c(dv, ivs)
    parsed_cor <- lapply(df$correlations, aggregate_cor, sel_vars=observed_variables)
    non_empty_idx <- lapply(parsed_cor, length) > 0
    this_cor <- parsed_cor[non_empty_idx]
    this_n <- df$between_df$n[non_empty_idx]
    
    # Model specification
    iv_spec <- paste(ivs, collapse = " + ")
    lavaan_spec <- paste(dv, '~', iv_spec, sep=' ')
    print(lavaan_spec)
    RAM_spec <- lavaan2RAM(lavaan_spec, obs.variables=observed_variables)
    
    
    if (method=='one_stage'){
        this_df <- Cor2DataFrame(this_cor, this_n)
        fit <- osmasem(RAM=RAM_spec, data=this_df)
    } else if (method=='two_stage'){
        this_df <- tssem1(this_cor, this_n, method="REM") %>%
            rerun(autofixtau2 = TRUE)
        fit <- tssem2(this_df, RAM=RAM_spec, intervals.type="z") %>%
            rerun()
    }
    
    if (verbose) print(summary(fit))
    
    return(list(this_df=this_df, 
                lavaan_spec=lavaan_spec, 
                RAM_spec=RAM_spec, 
                fit=fit,
                summary=summary(fit)))
} 