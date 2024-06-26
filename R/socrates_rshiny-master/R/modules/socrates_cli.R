#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# A. Run SOCRATES functions via R(studio)
# B. Run a remote SOCRATES UI
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

################################################################### #
# Run SOCRATES functions via R(studio) ----
################################################################### #

# note: set your work directory to the main SOCRATES repo folder to 
#       enable the relative file paths
# setwd('../..')

# clear workspace
rm(list=ls())

# load functions and options
source('R/socrates_main.R')
source('R/load_config_base.R') # re-load settings (without CoMix-based selection)

# use the UI defined lists
opt_country
opt_day_type
opt_touch
opt_duration
opt_gender

# aggregate the input parameters in a list
input <- list(age_breaks_num = c(0,12,18,40,60),
                country     = opt_country[3],
                daytype     = opt_day_type[1],
                touch       = opt_touch[[1]],
                duration    = opt_duration[[1]],
                gender      = opt_gender[1],
                cnt_location = opt_location,
                cnt_matrix_features   = opt_matrix_features,
                bool_transmission_param = FALSE,
                bool_reciprocal = TRUE,
                bool_suppl_professional_cnt = TRUE,
                wave = 3,
                cnt_reduction = data.frame(Transport=0,Leisure=0,Otherplace=0) # no reductions for this example
  )
  
# include age breaks as text (required for SOCRATES)
input$age_breaks_text = paste(input$age_breaks_num,sep=',')

# set age-specifific susceptibility and/or infectiouness (using the given age breaks)
input$age_susceptibility_text = paste(rep(1,length(input$age_breaks_num)),collapse=',')
input$age_infectiousness_text = input$age_susceptibility_text

# attach all atributes from the input list, so you can use their names directly
attach(input,warn.conflicts = F)

## 1. GET SURVEY OBJECT AND CREATE CONTACT MATRICES (manually) ----
################################################################### #
# get survey object  
survey_obj <- get_survey_object(country,
                              daytype,
                              touch,
                              duration,
                              gender,
                              cnt_location,
                              bool_reciprocal,
                              bool_suppl_professional_cnt = TRUE,
                              bool_hhmatrix_selection = FALSE,
                              wave = wave)

# inspect survey object
lapply(survey_obj,dim)# head(survey$contacts)
summary(survey_obj$contacts)

# generate default contact matrix, based on the survey object
cnt_obj <- contact_matrix(survey_obj, 
                          age.limits = c(0,18,60))

# inspect contact matrix object
lapply(cnt_obj,dim)
table(is.na(cnt_obj$matrix))
sum(cnt_obj$participants$contacts_reported)

# get symmetric contact matrix
matrix_out <- contact_matrix(survey_obj, 
                             age.limits = age_breaks_num,
                             symmetric  = TRUE,
                             quiet      = TRUE,
                             weigh.dayofweek = TRUE,
                             weigh.age       = TRUE,
                             weight.threshold = weight_threshold)
names(matrix_out)

# inspect contact matrix using internal function(s)  
plot_cnt_matrix(matrix_out$matrix)

## 2. run the SOCRATES analysis, which is printed in the UI ----
################################################################### #

# run the principal function 
socrates_out <- run_social_contact_analysis(country,
                                            daytype,
                                            touch,
                                            duration,
                                            gender,
                                            cnt_location,
                                            cnt_matrix_features,
                                            age_breaks_text,
                                            weight_threshold,
                                            bool_transmission_param,
                                            age_susceptibility_text,
                                            age_infectiousness_text,
                                            wave = wave,
                                            cnt_reduction = cnt_reduction)

# inspect socrates object
names(socrates_out)
socrates_out[1:6]
plot_cnt_matrix(socrates_out$matrix)



################################################################### #
# B. Run a remote SOCRATES UI  ----
################################################################### #

if(0==1){ # never executed...
  # load package
  library('shiny')
  
  # run UI via GitHub ----
  runGitHub('socrates_rshiny','lwillem')
  
  # run UI via URL (of GitHub repo) ----
  runUrl('https://github.com/lwillem/socrates_rshiny/archive/master.tar.gz')
}

