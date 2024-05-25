#________________________________________________________________________________________________________________
# DAWANCE Oceane (UNamur)
#
# Ce fichier fait partie de mon memoire (chapitres 3 et 4).
#
# => CALCULER ET REPRESENTER GRAPHIQUEMENT LES TAUX D'AMORTISSEMENT
#    REPRESENTER GRAPHIQUEMENT LA CONVERGENCE DE L'INCIDENCE RELATIVE ET CALCULER QUAND LES SEUILS SONT ATTEINTS
#    REPRESENTER GRAPHIQUEMENT LES EQUILIBRES ET CALCULER LA DISTANCE QUI LES SEPARE
#________________________________________________________________________________________________________________

library(RColorBrewer)

path_to_socrates <- "C:/Users/Oceane/OneDrive - Université de Namur/Memoire/Q2/socrates_rshiny-master"
setwd(path_to_socrates)
source('R/socrates_main.R')
source('R/load_config_base.R')

#-----
# Taux d'amortissement
#-----

rapport_vp <- function(Age_breaks, first_wave, last_wave) {
  
  color <- brewer.pal(10, "Paired")
  set.seed(2024)
  rapports_mean=c()
  rapports_IC1=c()
  rapports_IC2=c()
  
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
    
    matrix_out_bs <- contact_matrix(survey_obj,
                                    age.limits = Age_breaks,
                                    symmetric  = TRUE,
                                    quiet      = TRUE,
                                    weigh.dayofweek = TRUE,
                                    weigh.age = TRUE,
                                    weight.threshold = weight_threshold,
                                    estimated.contact.age = 'sample',
                                    missing.contact.age = 'sample',
                                    n=200
    )
    
    rapports_matrice <- lapply(matrix_out_bs$matrices, function(mat) {
      mat$matrix[is.na(mat$matrix)] <- 0
      vp <- eigen(mat$matrix)
      return(vp$values[1] / Mod(vp$values[2]))
    })
    rapports_mean <- c(rapports_mean, mean(unlist(rapports_matrice)))
    IC = Mod(quantile(unlist(rapports_matrice), probs = c(0.025, 0.975)))
    rapports_IC1 <- c(rapports_IC1,IC[1])
    rapports_IC2 <- c(rapports_IC2,IC[2])
  }
  
  y=round(max(rapports_IC2))+1
  plot(1, xlim=c(first_wave,last_wave), ylim=c(1, y), type = "n", xlab = "Vague", ylab = "Taux d’amortissement", cex.lab = 1.5, cex.axis = 1.5)
  x = first_wave:last_wave
  polygon(c(x, rev(x)), c(rapports_IC1, rev(rapports_IC2)), col = color[9], border = NA)
  arrows(x0=x, y0=rapports_IC1, x1=x, y1=rapports_IC2, code=3, angle=90, length=0.05)
  lines(x, rapports_mean, type = "b", pch = 19, col = color[10])
  points(12, 1, pch=17, col = color[2], cex = 1.2) 
  points(c(16,43), c(1,1), pch=17, col = color[1], cex = 1.2)
  lines(c(19, 20), c(1,1), type = "b", pch = 17, col = color[4], cex = 1.2) 
  lines(c(26, 29), c(1,1), type = "b", pch = 17, col = color[3], cex = 1.2)
  points(34, 1, pch=17, col = color[6], cex = 1.2)
  lines(c(38, 39), c(1,1), type = "b", pch = 17, col = color[2], cex = 1.2)
  
  legende_texte <- c("Vacances d'hiver", "Conge de detente", "Vacances de printemps", "Vacances d'ete", "Conge d'automne")
  legende_couleurs <- c(color[2],color[1],color[4],color[3],color[6])
  legend("topright", legend=legende_texte, col=legende_couleurs, pch=17, cex = 1.5)
  
  #saveRDS(list(rapports_mean, rapports_IC1, rapports_IC2), file = "rapports_0-20-60.rds")
  #saveRDS(list(rapports_mean, rapports_IC1, rapports_IC2), file = "rapports_0-6-12-18-30-40-50-60-70-80.rds")
  
  return(rapports_mean)
  
}

# pdf("rapports_0-20-60.pdf", height=8.27,width=11.69)
# print(rapport_vp(c(0,20,60), 12, 43))
# rapports_copie <- readRDS("rapports_0-20-60.rds")
# dev.off()
# 
# pdf("rapports_0-6-12-18-30-40-50-60-70-80.pdf", height=8.27,width=11.69)
# print(rapport_vp(c(0,6,12,18,30,40,50,60,70,80), 12, 43))
# rapports_copie <- readRDS("rapports_0-6-12-18-30-40-50-60-70-80.rds")
# dev.off()

#-----
# Intervalles de confiance a 95%
#-----

IC <- function(x) {
  return(quantile(x, probs = c(0.025, 0.975)))
}

#-----
# Convergence de l'incidence (conditions initiales aleatoires)
#-----

convergence_random <- function(Age_breaks, wave, number, iterations) {
  
  n <- length(Age_breaks)
  color <- brewer.pal(n, "Paired")
  if (n==3){
    plot(1, type = "n", xlim = c(0, iterations), ylim = c(0, 0.8), 
         xlab = "Iteration", ylab = "Incidence relative", main = paste("Vague", wave), cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)  
    contact_matrices <- readRDS("contact_matrices_0-20-60.rds")
    H = diag(c((0.54+0.55+0.56+0.59)/4, (0.7+0.76+0.9+0.99)/4, (0.99+0.99)/2))
    A = diag(c((0.182+0.55+0.603+1)/4, (1.172+1.009+0.88+0.869)/4, (0.846+0.805)/2))
  }else{
    plot(1, type = "n", xlim = c(0, iterations), ylim = c(0, 0.3), 
         xlab = "Iteration", ylab = "Incidence relative", main = paste("Vague", wave), cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
    contact_matrices <- readRDS("contact_matrices_0-6-12-18-30-40-50-60-70-80.rds")
    H = diag(c(0.54, 0.55, 0.56, 0.59, 0.7, 0.76, 0.9, 0.99, 0.99, 0.99))
    A = diag(c(0.182, 0.55, 0.603, 1, 1.172, 1.009, 0.88, 0.869, 0.846, 0.805))
  }
  
  K <- contact_matrices[[wave]]
  #K <- A%*%contact_matrices[[wave]]%*%H
  vec_p <- eigen(K)$vectors[,1]
  equilibre <- vec_p/sum(vec_p)
  
  seuils_iterations_L1 = matrix(0, nrow = number, ncol = 3)
  seuils_iterations_hellinger = matrix(0, nrow = number, ncol = 3)
  
  for (i in 1:number){
    
    error_L1 = c()
    error_hellinger = c()
    
    nombres_aleatoires <- runif(n)
    proportions_initiales <- nombres_aleatoires/sum(nombres_aleatoires)
    error_L1 <- c(error_L1,sum(abs(proportions_initiales - equilibre))/n)
    error_hellinger <- c(error_hellinger,sqrt(sum((sqrt(proportions_initiales) - sqrt(equilibre))^2))/sqrt(2))
    
    resultat <- K%*%proportions_initiales
    resultat_normalise <- resultat/sum(resultat)
    resultat_final <- cbind(proportions_initiales,resultat_normalise)
    error_L1 <- c(error_L1,sum(abs(resultat_normalise - equilibre))/n)
    error_hellinger <- c(error_hellinger,sqrt(sum((sqrt(resultat_normalise) - sqrt(equilibre))^2))/sqrt(2))
    
    for (j in 1:(iterations-1)){
      
      resultat <- K%*%resultat_normalise
      resultat_normalise <- resultat/sum(resultat)
      resultat_final <- cbind(resultat_final,resultat_normalise)
      error_L1 <- c(error_L1,sum(abs(resultat_normalise - equilibre))/n)
      error_hellinger <- c(error_hellinger,sqrt(sum((sqrt(resultat_normalise) - sqrt(equilibre))^2))/sqrt(2))
      
    }
    
    for (j in 1:n) {
      x_values <- seq(0, length.out = length(resultat_final[j,]))
      lines(x_values, resultat_final[j,], col = color[j])
    }
    
    seuils <- c(1e-2, 1e-3, 1e-4)
    for (j in seq(1, length.out = length(seuils))) {
      
      for (k in 1:(iterations+1)) {
        if (error_L1[k] <= seuils[j]) {
          seuils_iterations_L1[i,j] = k-1
          break
        }
      }
      
      for (k in 1:(iterations+1)) {
        if (error_hellinger[k] <= seuils[j]) {
          seuils_iterations_hellinger[i,j] = k-1
          break
        }
      }
      
    }
    
  }
  
  mediane_seuils_iterations_L1 <- apply(seuils_iterations_L1, 2, median)
  print(mediane_seuils_iterations_L1)
  abline(v = mediane_seuils_iterations_L1, lty = "dashed", lwd=1.5, col="darkgrey")
  IC_seuils_iterations_L1 <- apply(seuils_iterations_L1, 2, IC)
  print(IC_seuils_iterations_L1)
  
  mediane_seuils_iterations_hellinger <- apply(seuils_iterations_hellinger, 2, median)
  print(mediane_seuils_iterations_hellinger)
  IC_seuils_iterations_hellinger <- apply(seuils_iterations_hellinger, 2, IC)
  print(IC_seuils_iterations_hellinger)
  
  intervals=c()
  for (i in 1:(n-1)) {
    intervals <- c(intervals, paste("[",Age_breaks[i],",",Age_breaks[i+1],"["))
  }
  intervals <- c(intervals,paste(Age_breaks[n],"+"))
  if (n==3){
    legend("topright", legend = intervals, fill = color, bg="white", cex = 1.5)
  }else{
    legend("topright", legend = intervals, fill = color, ncol=2, bg="white", cex = 1.5)
  }
  
  return(list(mediane_seuils_iterations_L1, IC_seuils_iterations_L1, mediane_seuils_iterations_hellinger, IC_seuils_iterations_hellinger))
  
}

# convergence_random(c(0,20,60), 12, 200, 15)
# windows() ; convergence_random(c(0,6,12,18,30,40,50,60,70,80), 12, 200, 20)
# 
# seuils_convergence_random = vector("list", length = 11)
# pdf("convergence_random_0-20-60.pdf", height=8.27,width=11.69)
# for (i in 12:43){
#   seuils_convergence_random[[i]] = convergence_random(c(0,20,60), i, 200, 15)
# }  
# dev.off()
# saveRDS(seuils_convergence_random, file = "seuils_iterations_0-20-60.rds")
# readRDS("seuils_iterations_0-20-60.rds")
# 
# seuils_convergence_random = vector("list", length = 11)
# pdf("convergence_random_0-6-12-18-30-40-50-60-70-80.pdf", height=8.27,width=11.69)
# for (i in 12:43){
#   seuils_convergence_random[[i]] = convergence_random(c(0,6,12,18,30,40,50,60,70,80), i, 200, 20)
# }  
# dev.off()
# saveRDS(seuils_convergence_random, file = "seuils_iterations_0-6-12-18-30-40-50-60-70-80.rds")
# readRDS("seuils_iterations_0-6-12-18-30-40-50-60-70-80.rds")
# 
# seuils_convergence_random = vector("list", length = 11)
# pdf("convergence_random_0-20-60_AH.pdf", height=8.27,width=11.69)
# for (i in 12:43){
#   seuils_convergence_random[[i]] = convergence_random(c(0,20,60), i, 200, 15)
# }  
# dev.off()
# saveRDS(seuils_convergence_random, file = "seuils_iterations_0-20-60_AH.rds")
# readRDS("seuils_iterations_0-20-60_AH.rds")
# 
# seuils_convergence_random = vector("list", length = 11)
# pdf("convergence_random_0-6-12-18-30-40-50-60-70-80_AH.pdf", height=8.27,width=11.69)
# for (i in 12:43){
#   seuils_convergence_random[[i]] = convergence_random(c(0,6,12,18,30,40,50,60,70,80), i, 200, 20)
# }  
# dev.off()
# saveRDS(seuils_convergence_random, file = "seuils_iterations_0-6-12-18-30-40-50-60-70-80_AH.rds")
# readRDS("seuils_iterations_0-6-12-18-30-40-50-60-70-80_AH.rds")

#-----
# Convergence de l'incidence (transition entre 2 vagues)
#-----

convergence_equilibre <- function(Age_breaks, wave1, wave2, iterations) {
  
  n <- length(Age_breaks)
  if (n==3){
    contact_matrices <- readRDS("contact_matrices_0-20-60.rds")
    H = diag(c((0.54+0.55+0.56+0.59)/4, (0.7+0.76+0.9+0.99)/4, (0.99+0.99)/2))
    A = diag(c((0.182+0.55+0.603+1)/4, (1.172+1.009+0.88+0.869)/4, (0.846+0.805)/2))
  }else{
    contact_matrices <- readRDS("contact_matrices_0-6-12-18-30-40-50-60-70-80.rds")
    H = diag(c(0.54, 0.55, 0.56, 0.59, 0.7, 0.76, 0.9, 0.99, 0.99, 0.99))
    A = diag(c(0.182, 0.55, 0.603, 1, 1.172, 1.009, 0.88, 0.869, 0.846, 0.805))
  }
  
  K1 <- contact_matrices[[wave1]]
  #K1 <- A%*%contact_matrices[[wave1]]%*%H
  vec_p1 <- eigen(K1)$vectors[,1]
  equilibre1 <- vec_p1/sum(vec_p1)
  K2 <- contact_matrices[[wave2]]
  #K2 <- A%*%contact_matrices[[wave2]]%*%H
  vec_p2 <- eigen(K2)$vectors[,1]
  equilibre2 <- vec_p2/sum(vec_p2)
  
  seuils_iterations_L1 = c(0,0,0)
  seuils_iterations_hellinger = c(0,0,0)
  
  error_L1 = c()
  error_hellinger = c()
  
  proportions_initiales <- equilibre1
  error_L1 <- c(error_L1,sum(abs(proportions_initiales - equilibre2))/n)
  error_hellinger <- c(error_hellinger,sqrt(sum((sqrt(proportions_initiales) - sqrt(equilibre2))^2))/sqrt(2))
  
  resultat <- K2%*%proportions_initiales
  resultat_normalise <- resultat/sum(resultat)
  resultat_final <- cbind(proportions_initiales,resultat_normalise)
  error_L1 <- c(error_L1,sum(abs(resultat_normalise - equilibre2))/n)
  error_hellinger <- c(error_hellinger,sqrt(sum((sqrt(resultat_normalise) - sqrt(equilibre2))^2))/sqrt(2))
  
  for (j in 1:(iterations-1)){
    
    resultat <- K2%*%resultat_normalise
    resultat_normalise <- resultat/sum(resultat)
    resultat_final <- cbind(resultat_final,resultat_normalise)
    error_L1 <- c(error_L1,sum(abs(resultat_normalise - equilibre2))/n)
    error_hellinger <- c(error_hellinger,sqrt(sum((sqrt(resultat_normalise) - sqrt(equilibre2))^2))/sqrt(2))
    
  }
  
  seuils <- c(1e-2, 1e-3, 1e-4)
  for (j in seq(1, length.out = length(seuils))) {
    
    for (k in 1:(iterations+1)) {
      if (error_L1[k] <= seuils[j]) {
        seuils_iterations_L1[j] = k-1
        break
      }
    }
    
    for (k in 1:(iterations+1)) {
      if (error_hellinger[k] <= seuils[j]) {
        seuils_iterations_hellinger[j] = k-1
        break
      }
    }
    
  }
  print('L1')
  print(seuils_iterations_L1)
  print('Hellinger')
  print(seuils_iterations_hellinger)
}

# convergence_equilibre(c(0,20,60), 12, 13, 20)
# convergence_equilibre(c(0,6,12,18,30,40,50,60,70,80), 12, 13, 20)
# 
# for (i in 12:42) {
#   convergence_equilibre(c(0,20,60), i, i+1, 20)
# }
# for (i in 12:42) {
#   convergence_equilibre(c(0,6,12,18,30,40,50,60,70,80), i, i+1, 20)
# }

#-----
# Graphique des equilibres
#-----

plot_equilibres <- function(Age_breaks, first_wave, last_wave) {
  
  n=length(Age_breaks)
  if (n==3){
    contact_matrices <- readRDS("contact_matrices_0-20-60.rds")
    plot(NULL, xlim = c(first_wave,last_wave), ylim = c(-0.02,0.7), #c(min(unlist(equilibres)),max(unlist(equilibres))), 
         xlab = "Vague", ylab = "Equilibre", cex.lab = 1.5, cex.axis = 1.5)
  }else{
    contact_matrices <- readRDS("contact_matrices_0-6-12-18-30-40-50-60-70-80.rds")
    plot(NULL, xlim = c(first_wave,last_wave), ylim = c(-0.02,0.4), #c(min(unlist(equilibres)),max(unlist(equilibres))), 
         xlab = "Vague", ylab = "Equilibre", cex.lab = 1.5, cex.axis = 1.5)
  }
  
  color <- brewer.pal(10, "Paired")
  abline(v = first_wave:last_wave, lty = "dashed", col="lightgrey")
  points(12, -0.02, pch = 17, col = color[2], cex = 1.2) 
  points(c(16,43), c(-0.02,-0.02), pch = 17, col = color[1], cex = 1.2)
  lines(c(19, 20), c(-0.02,-0.02), type = "b", pch = 17, col = color[4], cex = 1.2) 
  lines(c(26, 29), c(-0.02,-0.02), type = "b", pch = 17, col = color[3], cex = 1.2)
  points(34, -0.02, pch = 17, col = color[6], cex = 1.2)
  lines(c(38, 39), c(-0.02,-0.02), type = "b", pch = 17, col = color[2], cex = 1.2)
  
  equilibres = list()
  for (wave in first_wave:last_wave){
    K <- contact_matrices[[wave]]
    vec_p <- eigen(K)$vectors[,1]
    equilibres[[wave-11]] <- vec_p/sum(vec_p)
  }  
  
  for (j in seq_along(Age_breaks)) {
    eq <- sapply(equilibres, `[`, j)
    lines(first_wave:last_wave, eq, type = "b", pch = 19, col = color[j])
  }
  
  intervals=c()
  for (i in 1:(n-1)) {
    intervals <- c(intervals, paste("[",Age_breaks[i],",",Age_breaks[i+1],"["))
  }
  intervals <- c(intervals,paste(Age_breaks[n],"+"))
  if (n==3){
    legend("topright", legend = intervals, col = color, cex = 1.5, pch=19, bg="white")
  }else{
    legend("topright", legend = intervals, col = color, cex = 1.5, pch=19, ncol=2, bg="white")
  }
  
  distances_L1 = c()
  for (i in 1:(length(equilibres) - 1)) {
    distances_L1 <- c(distances_L1,sum(abs(equilibres[[i]] - equilibres[[i+1]]))/n)
  }
  
  couleurs <- colorRampPalette(c("mistyrose2", "red4"))(100) 
  for (i in 1:(length(equilibres) - 1)) {
    couleur_index <- round((distances_L1[[i]] - min(distances_L1)) / (max(distances_L1) - min(distances_L1)) * 99) + 1
    if (n==3){
      text(i+11.5,0.78,sprintf("%.3f", distances_L1[[i]]),xpd=T,srt=90,col = couleurs[couleur_index], cex = 1.5)
    }else{
      text(i+11.5,0.445,sprintf("%.3f", distances_L1[[i]]),xpd=T,srt=90,col = couleurs[couleur_index], cex = 1.5)
    }
  }
  
  return(distances_L1)
}

# pdf("equilibres_0-20-60.pdf", height=8.27,width=11.69)
# print(plot_equilibres(c(0,20,60), 12, 43))
# dev.off()
# pdf("equilibres_0-6-12-18-30-40-50-60-70-80.pdf", height=8.27,width=11.69)
# print(plot_equilibres(c(0,6,12,18,30,40,50,60,70,80), 12, 43))
# dev.off()
