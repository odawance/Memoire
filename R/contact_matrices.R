#___________________________________________________________________________
# DAWANCE Oceane (UNamur)
#
# Ce fichier fait partie de mon memoire.
#
# => GENERER ET REPRESENTER GRAPHIQUEMENT LES MATRICES DES CONTACTS SOCIAUX
#___________________________________________________________________________

path_to_socrates <- "C:/Users/Oceane/OneDrive - Université de Namur/Memoire/Q2/socrates_rshiny-master"
setwd(path_to_socrates)
source('R/socrates_main.R')
source('R/load_config_base.R')

#-----
# Generation des matrices des contacts sociaux (transposees)
#-----

contact_matrices_list <- function(Age_breaks, first_wave, last_wave){
  
  K_list = vector("list", length = first_wave-1)
  
  for (i in first_wave:last_wave){
    survey_obj <- get_survey_object(country="Belgium 2020 CoMix (Coletti 2020)",
                                    daytype="All contacts",
                                    touch="All contacts",
                                    duration="All contacts",
                                    gender="All",
                                    cnt_location=opt_location,
                                    bool_reciprocal=TRUE,
                                    bool_suppl_professional_cnt=TRUE,
                                    bool_hhmatrix_selection=FALSE,
                                    wave=i,
                                    quiet = TRUE)
    
    matrix_out <- contact_matrix(survey_obj,
                                 age.limits = Age_breaks,
                                 symmetric  = TRUE,
                                 quiet      = TRUE,
                                 weigh.dayofweek = TRUE,
                                 weigh.age = TRUE,
                                 weight.threshold = weight_threshold,
                                 estimated.contact.age = 'sample',
                                 missing.contact.age = 'sample')
    
    K_list[[i]] <- t(matrix_out$matrix)
    
  }
  
  # if (length(Age_breaks)==3){
  #   saveRDS(K_list, file = "contact_matrices_0-20-60.rds")    
  # }else{
  #   saveRDS(K_list, file = "contact_matrices_0-6-12-18-30-40-50-60-70-80.rds")    
  # }
  return(K_list)
}

# print(contact_matrices_list(c(0,20,60), 9, 43))
# contact_matrices_copie <- readRDS("contact_matrices_0-20-60.rds")
# print(contact_matrices_list(c(0,6,12,18,30,40,50,60,70,80), 9, 43))
# contact_matrices_copie <- readRDS("contact_matrices_0-6-12-18-30-40-50-60-70-80.rds")

#-----
# Representation graphique des matrices
#-----

plot_matrices <- function(mij,wave,plot_title_extra = ''){
  
  mij=mij[[wave]]
  if(all(is.na(mij))){
    return(NA)
  }
  redc <- rev(heat.colors(100))
  par(mar=c(5, 6, 2, 2),mgp=c(3,0.5,0))
  p <- simage(s = mij, 
              ylab="Age des participants",
              xlab="Age des contacts", 
              legend.width=1,
              slim=c(min(mij,na.rm=T), max(mij,na.rm=T)), 
              #slim=c(0, 7.29), #3 
              #slim=c(0, 6.15), #10
              cex.lab=1,
              cex.main=1, 
              las=0.1,
              col=redc, 
              main=paste("Vague ",wave), 
              xaxt="n", 
              yaxt="n")
  #mtext("Age des participants", side=2, line=4, cex=1.2)
  mtext("Age des contacts", side=1, line=2, cex=1.2)
  #mtext("Age des contacts", side=1, line=4, cex=1.2)
  # set axis 
  plt_ticks <- seq(0,1,1/2)
  axis(2, at=plt_ticks, labels = c('[0,20[','[20,60[','60+'),cex.axis=0.9,tick = FALSE,las=1)
  axis(1, at=plt_ticks, labels = c('[0,20[','[20,60[','60+'),cex.axis=0.9,tick = FALSE)
  #plt_ticks <- seq(0,1,1/9)
  #axis(2, at=plt_ticks, labels = c('[0,6[','[6,12[','[12,18[','[18,30[','[30,40[','[40,50[','[50,60[','[60,70[','[70,80[','80+'),cex.axis=0.7,tick = FALSE,las=1)
  #axis(1, at=plt_ticks, labels = c('[0,6[','[6,12[','[12,18[','[18,30[','[30,40[','[40,50[','[50,60[','[60,70[','[70,80[','80+'),cex.axis=0.7,tick = FALSE,las=2)
  
  # format results (rounding/scientific)
  if(any(max(mij,na.rm=T)>1)){
    mij <- round(mij,digits=format_num_digits)
  } else{
    mij <- format(mij,digits = format_num_digits)
  }
  # get grid centers and add value
  e_grid <- expand.grid(plt_ticks,plt_ticks)
  text(e_grid, labels = mij, cex=1)
  #text(e_grid, labels = mij, cex=0.7)
}

# contact_matrices_3 <- readRDS("contact_matrices_0-20-60.rds")
# pdf("contact_matrix_vague9.pdf", width=8.27/2,height=11.69/4)
# plot_matrices(contact_matrices_3,9)
# dev.off()
# 
# pdf("contact_matrices_3.pdf", width=8.27,height=11.69)
# par(mfrow = c(4,2))
# for (i in 12:43){
#   plot_matrices(contact_matrices_3,i)
# }  
# dev.off()
# 
# contact_matrices_10 <- readRDS("contact_matrices_0-6-12-18-30-40-50-60-70-80.rds")
# pdf("contact_matrices_10.pdf", width=8.27,height=11.69)
# par(mfrow = c(4,2))
# for (i in 12:43){
#   plot_matrices(contact_matrices_10,i)
# }  
# dev.off()
# 
# pdf("contact_matrix_vagues28_32.pdf", height=8.27,width=11.69)
# par(mfrow = c(2,2))
# plot_matrices(contact_matrices_3,28)
# plot_matrices(contact_matrices_10,28)
# plot_matrices(contact_matrices_3,32)
# plot_matrices(contact_matrices_10,32)
# dev.off()