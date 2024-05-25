#____________________________________________________________________________________________________________________
# DAWANCE Oceane (UNamur)
#
# Ce fichier fait partie de mon memoire (chapitre 5).
#
# => MODELE SIHR A 3 COMPARTIMENTS CALIBRE SUR DES DONNEES COVID-19 BELGES DE 2020
#    REPRESENTER GRAPHIQUEMENT LA CONVERGENCE DE LA PREVALENCE (RELATIVE) ET CALCULER QUAND LES SEUILS SONT ATTEINTS
#____________________________________________________________________________________________________________________

path_to_data <- "C:/Users/Oceane/OneDrive - Université de Namur/Memoire/Q2"
path_to_socrates <- "C:/Users/Oceane/OneDrive - Université de Namur/Memoire/Q2/socrates_rshiny-master"

#-----
# Modele
#-----

### Packages necessaires

library(deSolve) # pour la fonction ode
library(mcmc)
library(RColorBrewer)
library(scales)

### Donnees

## Jeu de donnees sur les hospitalisations

setwd(path_to_data)
hospitalisation <- read.table("covid-hospitalizations.csv", header=TRUE, sep=",")
hospitalisation <- hospitalisation[hospitalisation$entity == "Belgium",]
hospitalisation <- hospitalisation[substr(hospitalisation$date,1,4) == "2020",]
hospitalisation <- hospitalisation[-(1:24), ]
#View(hospitalisation)

## Selection des donnees d'admissions a l'hopital et d'occupations des hopitaux

A <- hospitalisation[hospitalisation$indicator == "Weekly new hospital admissions",]
A$value <- round(A$value/7) # Approximation des admissions journalieres
# Approximation de la repartition [0,20[, [20,60[, 60+ par mois
A_03 <- A[substr(A$date,6,7) == "03",]
A_03 = list(round(c(A_03[,5])*(0.9/100+0.5/100)),round(c(A_03[,5])*(5.9/100+26.1/100)),round(c(A_03[,5])*(41.6/100+25/100)))
A_04 <- A[substr(A$date,6,7) == "04",]
A_04 = list(round(c(A_04[,5])*(1/100+0.9/100)),round(c(A_04[,5])*(6.3/100+22/100)),round(c(A_04[,5])*(35.5/100+34.3/100)))
A_05 <- A[substr(A$date,6,7) == "05",]
A_05 = list(round(c(A_05[,5])*(1.4/100+1.5/100)),round(c(A_05[,5])*(8.6/100+17.3/100)),round(c(A_05[,5])*(29.9/100+41.3/100)))
A_06 <- A[substr(A$date,6,7) == "06",]
A_06 = list(round(c(A_06[,5])*(1.8/100+1.6/100)),round(c(A_06[,5])*(11.7/100+14.8/100)),round(c(A_06[,5])*(33.4/100+36.7/100)))
A_07 <- A[substr(A$date,6,7) == "07",]
A_07 = list(round(c(A_07[,5])*(3.8/100+2.8/100)),round(c(A_07[,5])*(11.5/100+28.7/100)),round(c(A_07[,5])*(32.1/100+21/100)))
A_08 <- A[substr(A$date,6,7) == "08",]
A_08 = list(round(c(A_08[,5])*(2.4/100+2.8/100)),round(c(A_08[,5])*(12/100+26/100)),round(c(A_08[,5])*(34.7/100+22.1/100)))
A_09 <- A[substr(A$date,6,7) == "09",]
A_09 = list(round(c(A_09[,5])*(2.7/100+1.1/100)),round(c(A_09[,5])*(7.8/100+19.4/100)),round(c(A_09[,5])*(37.1/100+31.9/100)))
A_10 <- A[substr(A$date,6,7) == "10",]
A_10 = list(round(c(A_10[,5])*(1.9/100+0.6/100)),round(c(A_10[,5])*(6.3/100+20.2/100)),round(c(A_10[,5])*(41/100+29.9/100)))
A_11 <- A[substr(A$date,6,7) == "11",]
A_11 = list(round(c(A_11[,5])*(1/100+0.9/100)),round(c(A_11[,5])*(6/100+17.9/100)),round(c(A_11[,5])*(39.5/100+34.7/100)))
A_12 <- A[substr(A$date,6,7) == "12",]
A_12 = list(round(c(A_12[,5])*(0.9/100+1.3/100)),round(c(A_12[,5])*(5.4/100+14.4/100)),round(c(A_12[,5])*(34.8/100+43.1/100)))
A = Map(c, A_03, A_04, A_05, A_06, A_07, A_08, A_09, A_10, A_11, A_12)

O <- hospitalisation[hospitalisation$indicator == "Daily hospital occupancy",]
O_03 <- O[substr(O$date,6,7) == "03",]
O_03 = list(round(c(O_03[,5])*(0.9/100+0.5/100)),round(c(O_03[,5])*(5.9/100+26.1/100)),round(c(O_03[,5])*(41.6/100+25/100)))
O_04 <- O[substr(O$date,6,7) == "04",]
O_04 = list(round(c(O_04[,5])*(1/100+0.9/100)),round(c(O_04[,5])*(6.3/100+22/100)),round(c(O_04[,5])*(35.5/100+34.3/100)))
O_05 <- O[substr(O$date,6,7) == "05",]
O_05 = list(round(c(O_05[,5])*(1.4/100+1.5/100)),round(c(O_05[,5])*(8.6/100+17.3/100)),round(c(O_05[,5])*(29.9/100+41.3/100)))
O_06 <- O[substr(O$date,6,7) == "06",]
O_06 = list(round(c(O_06[,5])*(1.8/100+1.6/100)),round(c(O_06[,5])*(11.7/100+14.8/100)),round(c(O_06[,5])*(33.4/100+36.7/100)))
O_07 <- O[substr(O$date,6,7) == "07",]
O_07 = list(round(c(O_07[,5])*(3.8/100+2.8/100)),round(c(O_07[,5])*(11.5/100+28.7/100)),round(c(O_07[,5])*(32.1/100+21/100)))
O_08 <- O[substr(O$date,6,7) == "08",]
O_08 = list(round(c(O_08[,5])*(2.4/100+2.8/100)),round(c(O_08[,5])*(12/100+26/100)),round(c(O_08[,5])*(34.7/100+22.1/100)))
O_09 <- O[substr(O$date,6,7) == "09",]
O_09 = list(round(c(O_09[,5])*(2.7/100+1.1/100)),round(c(O_09[,5])*(7.8/100+19.4/100)),round(c(O_09[,5])*(37.1/100+31.9/100)))
O_10 <- O[substr(O$date,6,7) == "10",]
O_10 = list(round(c(O_10[,5])*(1.9/100+0.6/100)),round(c(O_10[,5])*(6.3/100+20.2/100)),round(c(O_10[,5])*(41/100+29.9/100)))
O_11 <- O[substr(O$date,6,7) == "11",]
O_11 = list(round(c(O_11[,5])*(1/100+0.9/100)),round(c(O_11[,5])*(6/100+17.9/100)),round(c(O_11[,5])*(39.5/100+34.7/100)))
O_12 <- O[substr(O$date,6,7) == "12",]
O_12 = list(round(c(O_12[,5])*(0.9/100+1.3/100)),round(c(O_12[,5])*(5.4/100+14.4/100)),round(c(O_12[,5])*(34.8/100+43.1/100)))
O = Map(c, O_03, O_04, O_05, O_06, O_07, O_08, O_09, O_10, O_11, O_12)

## Visualisation des donnees d'occupation journaliere des hopitaux

color <- brewer.pal(12, "Paired")
nbre_jours = length(O[[1]])

#pdf("graph_donnees_3_2.pdf", height=8.27,width=11.69)
par(mfrow = c(2,3))
plot(1:nbre_jours, O[[1]], ylim=c(0, 200), xlab="Nombre de jours depuis le 21 mars 2020", ylab="Nombre de personnes hospitalisees",
     col=color[2], cex.lab = 1.5, cex.axis = 1.5)#, main="Occupation journaliere des hopitaux\n ([0,20[)")
abline(v = c(-2,45,103,131,165,226), lty = "dashed",lwd=3, col=c(color[12],color[8],color[7],color[5],color[9],color[10]))
legend("topleft", legend = "Donnees ([0,20[)", col = color[2], pch = 1, cex = 1.5,bg="white")
plot(1:nbre_jours, O[[2]], ylim=c(0, 2000), xlab="Nombre de jours depuis le 21 mars 2020", ylab="Nombre de personnes hospitalisees",
     col=color[2], cex.lab = 1.5, cex.axis = 1.5)#, main="Occupation journaliere des hopitaux \n ([20,60[)")
abline(v = c(-2,45,103,131,165,226), lty = "dashed",lwd=3, col=c(color[12],color[8],color[7],color[5],color[9],color[10]))
legend("topleft", legend = "Donnees ([20,60[)", col = color[2], pch = 1, cex = 1.5,bg="white")
plot(1:nbre_jours, O[[3]], ylim=c(0, 6000), xlab="Nombre de jours depuis le 21 mars 2020", ylab="Nombre de personnes hospitalisees",
     col=color[2], cex.lab = 1.5, cex.axis = 1.5)#, main="Occupation journaliere des hopitaux \n (60+)")
abline(v = c(-2,45,103,131,165,226), lty = "dashed",lwd=3, col=c(color[12],color[8],color[7],color[5],color[9],color[10]))
legend("topleft", legend = "Donnees (60+)", col = color[2], pch = 1, cex = 1.5,bg="white")

plot(1:nbre_jours, A[[1]], ylim=c(0, 20), xlab="Nombre de jours depuis le 21 mars 2020", ylab="Nombre de personnes admises",
     col=color[2], cex.lab = 1.5, cex.axis = 1.5)#, main="Occupation journaliere des hopitaux\n ([0,20[)")
abline(v = c(-2,45,103,131,165,226), lty = "dashed",lwd=3, col=c(color[12],color[8],color[7],color[5],color[9],color[10]))
legend("topleft", legend = "Donnees ([0,20[)", col = color[2], pch = 1, cex = 1.5,bg="white")
plot(1:nbre_jours, A[[2]], ylim=c(0, 200), xlab="Nombre de jours depuis le 21 mars 2020", ylab="Nombre de personnes admises",
     col=color[2], cex.lab = 1.5, cex.axis = 1.5)#, main="Occupation journaliere des hopitaux \n ([20,60[)")
abline(v = c(-2,45,103,131,165,226), lty = "dashed",lwd=3, col=c(color[12],color[8],color[7],color[5],color[9],color[10]))
legend("topleft", legend = "Donnees ([20,60[)", col = color[2], pch = 1, cex = 1.5,bg="white")
plot(1:nbre_jours, A[[3]], ylim=c(0, 600), xlab="Nombre de jours depuis le 21 mars 2020", ylab="Nombre de personnes admises",
     col=color[2], cex.lab = 1.5, cex.axis = 1.5)#, main="Occupation journaliere des hopitaux \n (60+)")
abline(v = c(-2,45,103,131,165,226), lty = "dashed",lwd=3, col=c(color[12],color[8],color[7],color[5],color[9],color[10]))
legend("topleft", legend = "Donnees (60+)", col = color[2], pch = 1, cex = 1.5,bg="white")
#dev.off()

### Modele

## Initialisations

# Approximation de la taille de la population ([0,20[, [20,60[, 60+)
N = c(2312040+(127025+130.257), 6976123-(127025+130.257+153630+148660+144335+140242+136872), 2204478+(153630+148660+144335+140242+136872)) 

# Nombre de reproduction de base
R_0 = c(0.65,(0.79+0.98)/2,1.37,0.73,1.7,1)

# Parametres pour chaque classe d'age
gamma = c(1/8, 1/9, 1/10) # Inverse du temps moyen avant guerison ou deces

setwd(path_to_socrates)
contact_matrices <- readRDS("contact_matrices_0-20-60.rds")

contact_matrix <- contact_matrices[[9]]
#H_diag = diag(c((0.54+0.55+0.56+0.59)/4, (0.7+0.76+0.9+0.99)/4, (0.99+0.99)/2))
#A_diag = diag(c((0.182+0.55+0.603+1)/4, (1.172+1.009+0.88+0.869)/4, (0.846+0.805)/2))
#contact_matrix <- A_diag%*%contact_matrices[[9]]%*%H_diag
vp = eigen(contact_matrix/gamma)
vp_principale = max(vp$values)

beta1 = R_0[1]/vp_principale*c(1,1,1) # Taux de transmission
beta2 = R_0[2]/vp_principale*c(1,1,1)
beta3 = R_0[3]/vp_principale*c(1,1,1)
beta4 = R_0[4]/vp_principale*c(1,1,1)
beta5 = R_0[5]/vp_principale*c(1,1,1)
beta6 = R_0[6]/vp_principale*c(1,1,1)
p = c(0.000333, 0.0048, 0.01) # Taux d'hospitalisation
delta = c(1/15, 1/17, 1/12) # Inverse du temps moyen de l'hospitalisation
param = list(beta1, beta2, beta3, beta4, beta5, beta6, p, delta, gamma)

# Compartiments initiaux pour chaque classe d'age
I0 = c(A[[1]][1]/(p[1]*gamma[1]),A[[2]][1]/(p[2]*gamma[2]),A[[3]][1]/(p[3]*gamma[3]))
H0 = c(O[[1]][1],O[[2]][1],O[[3]][1]) 
R0 = c(0,0,0)
S0 = c(N[1] - I0[1] - H0[1] - R0[1],N[2] - I0[2] - H0[2] - R0[2],N[3] - I0[3] - H0[3] - R0[3])  
CI = list(S0, I0, H0)

## Modele SIHR

t = 0:nbre_jours

vp_list = vector("list", length = 43)
for (i in 9:43){
  vp_list[[i]] <- eigen(contact_matrices[[i]])$values[1]  
  #vp_list[[i]] <- eigen(A_diag%*%contact_matrices[[i]]%*%H_diag)$values[1]  
}


SIHR = function(t, X, P, wave1=1, wave2=1, vp_list=vector("list", length = 43)){
  
  S1 = X[1]
  S2 = X[2]
  S3 = X[3]
  I1 = X[4]
  I2 = X[5]
  I3 = X[6]
  H1 = X[7]
  H2 = X[8]
  H3 = X[9]
  AH1 = X[10]
  AH2 = X[11]
  AH3 = X[12]
  
  if (is.null(vp_list[[9]])) {
    contact_matrix <- contact_matrices[[9]]}
  else{
    contact_matrix <- if (t >= 0 && t <= 286) {contact_matrices[[9]]}
    #contact_matrix <- if (t >= 0 && t <= 286) {A_diag%*%contact_matrices[[9]]%*%H_diag}
    else if (t > 286 && t <= 357) {contact_matrices[[wave1]]*vp_list[[9]]/vp_list[[wave1]]} #70
    #else if (t > 286 && t <= 357) {(A_diag%*%contact_matrices[[wave1]]%*%H_diag)*vp_list[[9]]/vp_list[[wave1]]} #70
    else if (t > 357 && t <= 4270) {contact_matrices[[wave2]]*vp_list[[9]]/vp_list[[wave2]]}
    #else if (t > 357 && t <= 4270) {(A_diag%*%contact_matrices[[wave2]]%*%H_diag)*vp_list[[9]]/vp_list[[wave2]]}
  }
  
  beta <- if (t >= 0 && t <= 44) {P[[1]]}
  else if (t > 44 && t <= 102) {P[[2]]}
  else if (t > 102 && t <= 130) {P[[3]]}
  else if (t > 130 && t <= 164) {P[[4]]}
  else if (t > 164 && t <= 225) {P[[5]]}
  else if (t > 225 && t <= 286) {P[[6]]}
  else {P[[6]]*0.7}
    
  p = P[[7]] 
  delta = P[[8]]
  gamma = P[[9]]
  
  dS1 = -(beta[1])*S1*(contact_matrix[1,1]*I1/(N[1])+contact_matrix[2,1]*I2/(N[2])+contact_matrix[3,1]*I3/(N[3]))
  dS2 = -(beta[2])*S2*(contact_matrix[1,2]*I1/(N[1])+contact_matrix[2,2]*I2/(N[2])+contact_matrix[3,2]*I3/(N[3]))
  dS3 = -(beta[3])*S3*(contact_matrix[1,3]*I1/(N[1])+contact_matrix[2,3]*I2/(N[2])+contact_matrix[3,3]*I3/(N[3]))
  dI1 = (beta[1])*S1*(contact_matrix[1,1]*I1/(N[1])+contact_matrix[2,1]*I2/(N[2])+contact_matrix[3,1]*I3/(N[3]))-(gamma[1])*I1
  dI2 = (beta[2])*S2*(contact_matrix[1,2]*I1/(N[1])+contact_matrix[2,2]*I2/(N[2])+contact_matrix[3,2]*I3/(N[3]))-(gamma[2])*I2
  dI3 = (beta[3])*S3*(contact_matrix[1,3]*I1/(N[1])+contact_matrix[2,3]*I2/(N[2])+contact_matrix[3,3]*I3/(N[3]))-(gamma[3])*I3
  dH1 = p[1]*(gamma[1])*I1-(delta[1])*H1
  dH2 = p[2]*(gamma[2])*I2-(delta[2])*H2
  dH3 = p[3]*(gamma[3])*I3-(delta[3])*H3
  dAH1 = p[1]*(gamma[1])*I1
  dAH2 = p[2]*(gamma[2])*I2
  dAH3 = p[3]*(gamma[3])*I3
  dX=c(dS1,dS2,dS3,dI1,dI2,dI3,dH1,dH2,dH3,dAH1,dAH2,dAH3) 
  
  return(list(dX))
}

CI_X = c(CI[[1]][1],CI[[1]][2],CI[[1]][3],CI[[2]][1],CI[[2]][2],CI[[2]][3],CI[[3]][1],CI[[3]][2],CI[[3]][3],0,0,0)
param_X = list(c(param[[1]][1], param[[1]][2], param[[1]][3]),c(param[[2]][1], param[[2]][2], param[[2]][3]),
               c(param[[3]][1], param[[3]][2], param[[3]][3]),c(param[[4]][1],param[[4]][2], param[[4]][3]),
               c(param[[5]][1], param[[5]][2], param[[5]][3]),c(param[[6]][1], param[[6]][2], param[[6]][3]),
               c(param[[7]][1], param[[7]][2], param[[7]][3]),c(param[[8]][1], param[[8]][2], param[[8]][3]),c(param[[9]][1], param[[9]][2], param[[9]][3]))
X = ode(CI_X,t,SIHR,param_X, method="bdf")
plot(X)

## Fonction de log-vraisemblance

log_likelihood=function(log_theta){
  
  theta = exp(log_theta)
  
  if(theta[1] > N[1]/30 || theta[2] > N[2]/30 || theta[3] > N[3]/20 || theta[4] > 50 || theta[5] > 1000 || theta[6] > 2000) {return(-Inf)}
  
  if (theta[25] > 1 || theta[26] > 1 || theta[27] > 1 || theta[28] > 1 || theta[29] > 1 || theta[30] > 1 || theta[31] > 1 || theta[32] > 1 || theta[33] > 1) {return(-Inf)}
  
  CI = c(N[1]-theta[1]-theta[4],N[2]-theta[2]-theta[5],N[3]-theta[3]-theta[6],theta[1],theta[2],theta[3],theta[4],theta[5],theta[6],0,0,0)
  param = list(c(theta[7],theta[8],theta[9]),c(theta[10],theta[11],theta[12]),
               c(theta[13],theta[14],theta[15]),c(theta[16],theta[17],theta[18]),
               c(theta[19],theta[20],theta[21]),c(theta[22],theta[23],theta[24]),
               c(theta[25],theta[26],theta[27]),c(theta[28],theta[29],theta[30]),c(theta[31],theta[32],theta[33]))
  
  X = ode(CI,t,SIHR,param, method="bdf") # Resolution du systeme d'EDO
  
  o1 = X[,8] # Charge hospitaliere theorique : H(t)
  o2 = X[,9]
  o3 = X[,10]
  a1 = c(3,diff(X[,11])) # Admissions
  a2 = c(59,diff(X[,12]))
  a3 = c(121,diff(X[,13]))
  
  logL_O1 = dpois(O[[1]],o1,log=T)
  logL_O2 = dpois(O[[2]],o2,log=T)
  logL_O3 = dpois(O[[3]],o3,log=T)
  logL_A1 = dpois(A[[1]],a1,log=T)
  logL_A2 = dpois(A[[2]],a2,log=T)
  logL_A3 = dpois(A[[3]],a3,log=T)
  logL = sum(c(logL_O1[nbre_jours],logL_O2[nbre_jours],logL_O3[nbre_jours],logL_O1[nbre_jours/2],logL_O2[nbre_jours/2],logL_O3[nbre_jours/2],logL_A1,logL_A2,logL_A3))
  
  return(logL)
}

theta0 = log(c(CI[[2]][1],CI[[2]][2],CI[[2]][3],CI[[3]][1],CI[[3]][2],CI[[3]][3],
               param[[1]][1], param[[1]][2], param[[1]][3], param[[2]][1], param[[2]][2], param[[2]][3], 
               param[[3]][1], param[[3]][2], param[[3]][3], param[[4]][1], param[[4]][2], param[[4]][3], 
               param[[5]][1], param[[5]][2], param[[5]][3], param[[6]][1], param[[6]][2], param[[6]][3], 
               param[[7]][1], param[[7]][2], param[[7]][3], param[[8]][1], param[[8]][2], param[[8]][3], param[[9]][1], param[[9]][2], param[[9]][3]))
log_likelihood(theta0)

## Optimisation de la log-vraisemblance

time_opt <- system.time(opt <- list(optim(theta0,log_likelihood,control=list(fnscale=-1))))
opt[[1]]$par = exp(opt[[1]]$par)

## Resultats de l'optimisation

S01 = N[1]-opt[[1]]$par[1]-opt[[1]]$par[4];
S02 = N[2]-opt[[1]]$par[2]-opt[[1]]$par[5];
S03 = N[3]-opt[[1]]$par[3]-opt[[1]]$par[6];
I01 = opt[[1]]$par[1];
I02 = opt[[1]]$par[2];
I03 = opt[[1]]$par[3];
H01 = opt[[1]]$par[4];
H02 = opt[[1]]$par[5];
H03 = opt[[1]]$par[6];
CI_opt = c(S01,S02,S03,I01,I02,I03,H01,H02,H03,0,0,0)

beta1 = c(opt[[1]]$par[7],opt[[1]]$par[8],opt[[1]]$par[9]);
beta2 = c(opt[[1]]$par[10],opt[[1]]$par[11],opt[[1]]$par[12]);
beta3 = c(opt[[1]]$par[13],opt[[1]]$par[14],opt[[1]]$par[15]);
beta4 = c(opt[[1]]$par[16],opt[[1]]$par[17],opt[[1]]$par[18]);
beta5 = c(opt[[1]]$par[19],opt[[1]]$par[20],opt[[1]]$par[21]);
beta6 = c(opt[[1]]$par[22],opt[[1]]$par[23],opt[[1]]$par[24]);
p = c(opt[[1]]$par[25],opt[[1]]$par[26],opt[[1]]$par[27]);
delta = c(opt[[1]]$par[28],opt[[1]]$par[29],opt[[1]]$par[30]);
gamma = c(opt[[1]]$par[31],opt[[1]]$par[32],opt[[1]]$par[33]);
param_opt = list(beta1,beta2,beta3,beta4,beta5,beta6,p,delta,gamma) 

## Simulation du modele pour les conditions initiales et parametres estimes

T = nbre_jours
t = 1:T
X_opt = ode(CI_opt,t,SIHR,param_opt, method="bdf")

## Visualisation des resultats optimaux

par(mfrow = c(1,3))

#plot(t, O[[1]], type = "p", ylim = c(0, 200), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes hospitalisees", col = "blue")#, main = "Modele SIHR ([0,20[)"
#lines(X_opt[,1],X_opt[,8], col = "red", lwd = 2)
plot(t, A[[1]], type = "p", ylim = c(0, 20), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes admises", col = "blue")#, main = "Modele SIHR ([0,20[)"
lines(X_opt[,1],c(3,diff(X_opt[,11])), col = "red", lwd = 2)
legend("topleft", legend = c("Donnees ([0,20[)", "Modele ([0,20[)"), col = c("blue", "red"), lty = c(0,1), lwd = c(1,2), pch = c(1,NA))

#plot(t, O[[2]], type = "p", ylim = c(0, 3000), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes hospitalisees", col = "blue")#, main = "Modele SIHR ([20,60[)"
#lines(X_opt[,1],X_opt[,9], col = "red", lwd = 2)
plot(t, A[[2]], type = "p", ylim = c(0, 300), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes admises", col = "blue")#, main = "Modele SIHR ([20,60[)"
lines(X_opt[,1],c(59,diff(X_opt[,12])), col = "red", lwd = 2)
legend("topleft", legend = c("Donnees ([20,60[)", "Modele ([20,60[)"), col = c("blue", "red"), lty = c(0,1), lwd = c(1,2), pch = c(1,NA))

#plot(t, O[[3]], type = "p", ylim = c(0, 6000), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes hospitalisees", col = "blue")#, main = "Modele SIHR (60+)"
#lines(X_opt[,1],X_opt[,10], col = "red", lwd = 2)
plot(t, A[[3]], type = "p", ylim = c(0, 600), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes admises", col = "blue")#, main = "Modele SIHR (60+)"
lines(X_opt[,1],c(121,diff(X_opt[,13])), col = "red", lwd = 2)
legend("topleft", legend = c("Donnees (60+)", "Modele (60+)"), col = c("blue", "red"), lty = c(0,1), lwd = c(1,2), pch = c(1,NA))

## MCMC

theta_opt = log(c(CI_opt[4],CI_opt[5],CI_opt[6],CI_opt[7],CI_opt[8],CI_opt[9],
                  param[[1]][1], param[[1]][2], param[[1]][3], param[[2]][1], param[[2]][2], param[[2]][3], 
                  param[[3]][1], param[[3]][2], param[[3]][3], param[[4]][1], param[[4]][2], param[[4]][3], 
                  param[[5]][1], param[[5]][2], param[[5]][3], param[[6]][1], param[[6]][2], param[[6]][3], 
                  param[[7]][1], param[[7]][2], param[[7]][3], param[[8]][1], param[[8]][2], param[[8]][3], 
                  param[[9]][1], param[[9]][2], param[[9]][3]))
time_out <- system.time(out <- metrop(log_likelihood, theta_opt, 100000, scale=0.0015))
out$accept
out$batch = exp(out$batch)

#theta_out = log(out$batch[nrow(out$batch),])
#time_out <- system.time(out <- metrop(log_likelihood, theta_out, nbatch=100000, blen=10, scale=0.0005))
#out$batch = exp(out$batch)

indices=(nrow(out$batch) - 4999):nrow(out$batch)
indices=indices[seq(1, length(indices), by = 50)]
out_select <- out$batch[indices, ]
#saveRDS(out_select, file = "out_select01_AH.rds")
out_select <- readRDS("out_select01.rds")
out_select <- readRDS("out_select01_AH.rds")

results = function(out,t=1:286,wave1=1,wave2=1,vp_list=vector("list", length = 43)){
  
  CI_out = c(N[1]-out[1]-out[4],N[2]-out[2]-out[5],N[3]-out[3]-out[6],out[1],out[2],out[3],out[4],out[5],out[6],0,0,0);
  param_out = list(c(out[7],out[8],out[9]),c(out[10],out[11],out[12]),c(out[13],out[14],out[15]),
                   c(out[16],out[17],out[18]),c(out[19],out[20],out[21]),c(out[22],out[23],out[24]),
                   c(out[25],out[26],out[27]),c(out[28],out[29],out[30]),c(out[31],out[32],out[33]));
  
  X_out = ode(CI_out,t,SIHR,param_out,wave1=wave1,wave2=wave2,vp_list=vp_list, method="bdf");
  
  return(X_out)
}

resume <- function(matrice) {
  resultats <- apply(matrice, 1, function(ligne) {
    moyenne <- mean(ligne)
    quantile_025 <- quantile(ligne, probs = 0.025)
    quantile_975 <- quantile(ligne, probs = 0.975)
    
    c(moyenne = moyenne, quantile_025 = quantile_025, quantile_975 = quantile_975)
  })
  return(resultats)
}

#pdf("mcmc1.pdf", height=8.27,width=11.69)
par(mfrow = c(2,3))

plot(t, O[[1]], type = "p", ylim = c(0, 200), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes hospitalisees", col = color[2], cex.lab = 1.5, cex.axis = 1.5)#, main = "Modele SIHR ([0,20[)"
X_out_list <- list()
H1_out <- c()
for (i in 1:nrow(out_select)) {
  X_out_list[[i]] = results(out_select[i, ])
  X_out = X_out_list[[i]]
  H1_out <- cbind(H1_out, X_out[,8]) 
}

legend("topleft", legend = c("Donnees ([0,20[)", "Modele ([0,20[)"), col = c(color[2], color[6]), lty = c(0,1), lwd = c(1,2), pch = c(1,NA), cex = 1.5)

polygon(c(t, rev(t)), c(resume(H1_out)[2,], rev(resume(H1_out)[3,])), col = color[5], border = NA)
lines(resume(H1_out)[1,], col = color[6], lwd = 2)

plot(t, O[[2]], type = "p", ylim = c(0, 2000), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes hospitalisees", col = color[2], cex.lab = 1.5, cex.axis = 1.5)#, main = "Modele SIHR ([20,60[)", col = "blue")
H2_out <- c()
for (i in 1:nrow(out_select)) {
  X_out = X_out_list[[i]]
  H2_out <- cbind(H2_out, X_out[,9]) 
}

legend("topleft", legend = c("Donnees ([20,60[)", "Modele ([20,60[)"), col = c(color[2], color[6]), lty = c(0,1), lwd = c(1,2), pch = c(1,NA), cex = 1.5)

polygon(c(t, rev(t)), c(resume(H2_out)[2,], rev(resume(H2_out)[3,])), col = color[5], border = NA)
lines(resume(H2_out)[1,], col = color[6], lwd = 2)

plot(t, O[[3]], type = "p", ylim = c(0, 6000), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes hospitalisees", col = color[2], cex.lab = 1.5, cex.axis = 1.5)#, main = "Modele SIHR (60+)", col = "blue")
H3_out <- c()
for (i in 1:nrow(out_select)) {
  X_out = X_out_list[[i]]
  H3_out <- cbind(H3_out, X_out[,10])
}

legend("topleft", legend = c("Donnees (60+)", "Modele (60+)"), col = c(color[2], color[6]), lty = c(0,1), lwd = c(1,2), pch = c(1,NA), cex = 1.5)

polygon(c(t, rev(t)), c(resume(H3_out)[2,], rev(resume(H3_out)[3,])), col = color[5], border = NA)
lines(resume(H3_out)[1,], col = color[6], lwd = 2)

plot(t, A[[1]], type = "p", ylim = c(0, 20), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes admises", col = color[2], cex.lab = 1.5, cex.axis = 1.5)
X_out_list <- list()
H1_out <- c()
for (i in 1:nrow(out_select)) {
  X_out_list[[i]] = results(out_select[i, ])
  X_out = X_out_list[[i]]
  H1_out <- cbind(H1_out, c(3,diff(X_out[,11]))) 
}
legend("topleft", legend = c("Donnees ([0,20[)", "Modele ([0,20[)"), col = c(color[2], color[6]), lty = c(0,1), lwd = c(1,2), pch = c(1,NA), cex = 1.5)
polygon(c(t, rev(t)), c(resume(H1_out)[2,], rev(resume(H1_out)[3,])), col = color[5], border = NA)
lines(resume(H1_out)[1,], col = color[6], lwd = 2)

plot(t, A[[2]], type = "p", ylim = c(0, 300), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes admises", col = color[2], cex.lab = 1.5, cex.axis = 1.5)
H2_out <- c()
for (i in 1:nrow(out_select)) {
  X_out = X_out_list[[i]]
  H2_out <- cbind(H2_out, c(59,diff(X_out[,12]))) 
}
legend("topleft", legend = c("Donnees ([20,60[)", "Modele ([20,60[)"), col = c(color[2], color[6]), lty = c(0,1), lwd = c(1,2), pch = c(1,NA), cex = 1.5)
polygon(c(t, rev(t)), c(resume(H2_out)[2,], rev(resume(H2_out)[3,])), col = color[5], border = NA)
lines(resume(H2_out)[1,], col = color[6], lwd = 2)

plot(t, A[[3]], type = "p", ylim = c(0, 600), xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Nombre de personnes admises", col = color[2], cex.lab = 1.5, cex.axis = 1.5)
H3_out <- c()
for (i in 1:nrow(out_select)) {
  X_out = X_out_list[[i]]
  H3_out <- cbind(H3_out, c(121,diff(X_out[,13]))) 
}
legend("topleft", legend = c("Donnees (60+)", "Modele (60+)"), col = c(color[2], color[6]), lty = c(0,1), lwd = c(1,2), pch = c(1,NA), cex = 1.5)
polygon(c(t, rev(t)), c(resume(H3_out)[2,], rev(resume(H3_out)[3,])), col = color[5], border = NA)
lines(resume(H3_out)[1,], col = color[6], lwd = 2)

#dev.off()

par(mfrow = c(1,1))

#-----
# Intervalles de confiance a 95%
#-----

IC <- function(x) {
  return(quantile(x, probs = c(0.025, 0.975)))
}

#-----
# Convergence de la prevalence (relative)
#-----

convergence_jours <- function(Age_breaks, day, end, wave1, wave2) {
  
  X_proj_list <- list()
  color <- brewer.pal(3, "Paired")
  plot(1, xlim=c(0,day+end), ylim=c(0,0.9), type = "n", xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Prevalence relative", main = paste("Transition de la vague", wave1, "a la vague", wave2), cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
  #plot(1, xlim=c(0,day+end), ylim=c(0,150000), type = "n", xlab = paste("Nombre de jours depuis le 21 mars 2020"), ylab = "Prevalence", main = paste("Transition de la vague", wave1, "a la vague", wave2), cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
  intervals=c()
  for (i in 1:(length(Age_breaks)-1)) {
    intervals <- c(intervals, paste("[",Age_breaks[i],",",Age_breaks[i+1],"["))
  }
  intervals <- c(intervals,paste(Age_breaks[length(Age_breaks)],"+"))
  legend("topleft", legend = intervals, fill = color, cex = 1.5)
  #legend("topright", legend = intervals, fill = color, cex = 1.5)
  
  seuils_jours_L1 = matrix(0, nrow = nrow(out_select), ncol = 3)
  seuils_jours_hellinger = matrix(0, nrow = nrow(out_select), ncol = 3)
  
  I1_out <- c()
  I2_out <- c()
  I3_out <- c()
  
  for (i in 1:nrow(out_select)) {
    
    t = 0:(day+end)
    X_proj_list[[i]] = results(out_select[i, ],t,wave1,wave2,vp_list)
    X_proj = X_proj_list[[i]]
    X_proj_n = t(X_proj[,5:7])
    X_proj_n = t(X_proj_n/colSums(X_proj_n)[col(X_proj_n)])
    #X_proj_n = t(X_proj_n)
    I1_out = cbind(I1_out,X_proj_n[,1])
    I2_out = cbind(I2_out,X_proj_n[,2])
    I3_out = cbind(I3_out,X_proj_n[,3])
    
    equilibre = X_proj_n[day+end,]
    
    error_L1 = c()
    error_hellinger = c()
    
    for (j in (day+(end/2)):(day+end)){
      error_L1 <- c(error_L1,sum(abs(X_proj_n[j,] - equilibre))/3)
      error_hellinger <- c(error_hellinger,sqrt(sum((sqrt(X_proj_n[j,]) - sqrt(equilibre))^2))/sqrt(2))
    }
    
    seuils <- c(1e-2, 1e-3, 1e-4)
    for (j in seq(1, length.out = length(seuils))) {
      for (k in 1:(end+1)) {
        if (error_L1[k] <= seuils[j]) {
          seuils_jours_L1[i,j] = k-1
          break
        }
      }
      for (k in 1:(end+1)) {
        if (error_hellinger[k] <= seuils[j]) {
          seuils_jours_hellinger[i,j] = k-1
          break
        }
      }
    }
    
  }
  
  mediane_seuils_jours_L1 <- apply(seuils_jours_L1, 2, median)
  print(mediane_seuils_jours_L1)
  abline(v = day+(end/2)+mediane_seuils_jours_L1[1:2], lty = "dashed", lwd=1.5, col="darkgrey")
  abline(v = day+(end/2), lwd=1.5, col="darkgrey")
  IC_seuils_jours_L1 <- apply(seuils_jours_L1, 2, IC)
  print(IC_seuils_jours_L1)
  
  mediane_seuils_jours_hellinger <- apply(seuils_jours_hellinger, 2, median)
  print(mediane_seuils_jours_hellinger)
  IC_seuils_jours_hellinger <- apply(seuils_jours_hellinger, 2, IC)
  print(IC_seuils_jours_hellinger)
  
  polygon(c(t, rev(t)), c(resume(I1_out)[2,], rev(resume(I1_out)[3,])), col = alpha(color[1],alpha=0.5), border = NA)
  lines(resume(I1_out)[1,], col = color[1], lwd = 2)
  polygon(c(t, rev(t)), c(resume(I2_out)[2,], rev(resume(I2_out)[3,])), col = alpha(color[2],alpha=0.5), border = NA)
  lines(resume(I2_out)[1,], col = color[2], lwd = 2)
  polygon(c(t, rev(t)), c(resume(I3_out)[2,], rev(resume(I3_out)[3,])), col = alpha(color[3],alpha=0.5), border = NA)
  lines(resume(I3_out)[1,], col = color[3], lwd = 2)
  
  return(list(mediane_seuils_jours_L1, IC_seuils_jours_L1, mediane_seuils_jours_hellinger, IC_seuils_jours_hellinger))
  
}

# convergence_jours(c(0,20,60), 287, 140, 22, 23)
# 
# seuils_jours = vector("list", length = 11)
# pdf("convergence_jours.pdf", height=8.27,width=11.69)
# for (wave in 12:42){
#   seuils_jours[[wave]] = convergence_jours(c(0,20,60), 287, 140, wave, wave+1)
# } 
# dev.off()
# saveRDS(seuils_jours, file = "seuils_jours_0-20-60_AH.rds")
# readRDS("seuils_jours_0-20-60_AH.rds")