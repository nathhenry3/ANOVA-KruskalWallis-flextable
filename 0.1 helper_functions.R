# Helper functions for Wellbeing survey

## demographise_anova() creates wellbeing tables filtered by demographic, using ANOVA tests. (This function has now been deprecated.) 
# group_1 - the other variable to group the data by (along with Semester)
# summ_1 - wellbeing variable to summarise (i.e. Q5.1, Q5.2, Q5.4)
# tab_title - flextable title
# All names must be in quotes, e.g. demographise_anova(group_1='Q5.6', ...)

demographise_anova <- function(data_set=comb_data, group_1, summ_1, tab_title) {
  
  # Filter data_set for NAs in group_1
  data_set %<>% filter(!is.na(!!sym(group_1)))
  
  # Get number of levels of group_1
  nlev <- nlevels(data_set[[group_1]])
  
  # Prepare variables for loop
  res_aov <- list()
  aov_p <- list()
  aov_welch <- list()
  less_3 <- list()
  
  # Prep formula
  aov_formula <- formula(paste0('`', summ_1, '` ~ Semester'))
  
  # Perform one-way ANOVA to test for differences in group means, for each level in group_1
  # Also check for homogeneity of variances; if not homogeneous, use Welch one-way test
  for (i in 1:nlev) {
    
    # Check if either semester group has < 3 observations. If so, cancel test.
    if (nrow(data_set[data_set[[group_1]] == levels(data_set[[group_1]])[i][1] & data_set[['Semester']] == 'S1 2021', ]) < 3 | 
        nrow(data_set[data_set[[group_1]] == levels(data_set[[group_1]])[i][1] & data_set[['Semester']] == 'S2 2020', ]) < 3) {
      
      less_3[i] <- TRUE
      
    } else {
      
      less_3[i] <- FALSE
      
      res_aov[[i]] <- aov(aov_formula, 
                          data_set[data_set[[group_1]] == levels(data_set[[group_1]])[i][1], ])
      
      levene_p <- leveneTest(aov_formula,
                             data_set[data_set[[group_1]] == levels(data_set[[group_1]])[i][1], ])
      
      # If levene's test reveals homogeneity of variance, use p-value from aov(). Otherwise use p-value from welch test
      if (levene_p[['Pr(>F)']][1] > 0.05) {
        aov_p[i] <- round(summary(res_aov[[i]])[[1]][['Pr(>F)']][[1]], 
                          3)
        
        # Pass the type of test and number of observations to footer in flex later on...
        aov_welch[i] <- FALSE
        
      } else {
        
        # Welch test
        res_aov[[i]] <- oneway.test(aov_formula, 
                                    data_set[data_set[[group_1]] == levels(data_set[[group_1]])[i][1], ])
        
        aov_p[i] <- round(res_aov[[i]]$p.value, 
                          3)
        
        aov_welch[i] <- TRUE
      }
    }
  }
  
  # Create flextable
  flex <- data_set %>% 
    group_by(Semester, !!sym(group_1), .drop=FALSE) %>% 
    summarise(Mean = round(mean(!!sym(summ_1), na.rm=TRUE), 
                           2), 
              SD = round(sd(!!sym(summ_1), na.rm=TRUE),
                         2),
              n = n()) %>%
    flextable() %>% 
    merge_at(i=1:nlev, j=1) %>% 
    merge_at(i=(nlev+1):(2*nlev), j=1) %>% 
    autofit(add_w=0.5) %>% 
    set_caption(tab_title) %>%  
    theme_vanilla()
  
  for (b in 1:nlev) {
    # Add ANOVA results as footer statements for each level in group_1, and state whether significant result or not
    flex %<>% 
      add_footer(top=FALSE, 
                 # If not enough results for anova, then state that in footer. Otherwise print p-value, significance, and type of test used. 
                 'Semester' = ifelse(less_3[b] == FALSE,
                                     paste0(b, 
                                            ') One-way ANOVA comparison between semesters for "',
                                            levels(data_set[[group_1]])[b][1],
                                            '" produces p-value of ', 
                                            aov_p[b], 
                                            ifelse(aov_p[b] <= 0.05, 
                                                   ' - difference between semesters is significant', 
                                                   ' - difference between semesters is not significant'),
                                            ifelse(aov_welch[b] == TRUE,
                                                   ' (NOTE: non-homogeneous variances detected, so Welch test used)',
                                                   '')),
                                     paste0(b, 
                                            ') Not enough observations to perform ANOVA for "',
                                            levels(data_set[[group_1]])[b][1],
                                            '"'))) %>%
      merge_at(i=b, j=1:5, part='footer')
  }
  
  flex
}

#############################################################################################333333

