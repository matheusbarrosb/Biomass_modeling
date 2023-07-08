#-------------------------------------------------------------------------------
# FUNCTIONS TO MODEL BIOMASS ACCUMULATION
#-------------------------------------------------------------------------------

set.seed(2023)


pred_B <- function(GR = NULL, N0 = NULL, M_ref = NULL, L_ref = NULL,
                   Lm = NULL, K = NULL,  Linf = NULL, t0 = NULL,
                   a = NULL, b = NULL, L0 = NULL, K_sup = NULL,
                   t.steps = NULL, plot = FALSE) {
  
  # growth is assumed to follow a linear trajectory at the juvenile phase
  # and a von Bertalanffy type after maturation
  L <- rep(NA, t.steps)
  L_prior <- rep(NA, t.steps)
  for(t in 2:length(L)) {
    
    L_prior[1] <- L0
    L_prior[t] <- L_prior[t-1]+GR
    
  }
  
  for (t in 1:length(L)) {
    
    L[t] <- ifelse(L_prior[t] < Lm,
                   L_prior[t]*1,
                   (Linf*(1-exp(- (K/365) *(t- (t0/365))))))
    
  }
  
  
  L_VB <- rep(NA, t.steps)
  for(t in 1:length(L_VB)) {
    
    L_VB[t] <- Linf*(1-exp(-(K/365)*(t - (t0/365))))
    
  }
  
  breakpoint <- c(which.min(abs(Lm-L_prior))) # find value closest to breakpoint Lm
  corr.point <- c(which.min(abs(L_prior[breakpoint]-L_VB)))
  
  L_corr <- rep(NA, t.steps)
  for(t in 1:length(L_corr)) {
    
    #apply a correction factor to make VB growth continue from the last point
    L_corr[t] <- ifelse(L_prior[t] < Lm,
                        L_prior[t],
                        L_VB[t] + (Lm - L_VB[breakpoint+1] ))
    
  }
  
  # estimation of mortality
  # mortality is size-dependent and is calculated based on known values for a given length
  M <- rep(NA, t.steps)
  for(t in 1:length(M)) {
    
    M[t] <- (M_ref/365)*(L_ref/L_corr[t])
    
  }
  
  # estimation of numbers at age
  N <- rep(NA, t.steps)
  for(t in 2:length(N)) {
    
    N[1] <- N0
    N[t] <- N[t-1]*exp(-M[t])
    
  }
  
  
  # calculate biomass at size
  B <- rep(NA, t.steps)
  for(t in 1:length(B)) {
    
    B[t] <- (a*L_corr[t])^b
    
  }
  
  # calculate cohort biomass
  CB <-rep(NA, t.steps)
  for(t in 1:length(CB)) {
    
    CB[t] <- N[t] * B[t]
    
  }
  
  P_acc <- cumsum(CB) # derived quantities
  GP_max <- max(P_acc)
  
  
  P_accK <- rep(NA, t.steps)
  for(t in 1:length(P_accK)) {
    
    P_accK[t] <- P_acc[t]*(1-P_acc[t]/K_sup)
    
  }
  
  if (plot == TRUE) {
    
    plot(P_acc, xlab = "Time (days)",
         ylab = "Gross production", type = "l")
    
  }
  
  vars <- data.frame(L_VB, L_corr, N, B, CB, P_acc)
  names(vars) <- c("LVB", "Pred_size", "Nt", "B", "CB", "GP")
  
  output <- list(vars, GP_max)
  names(output) <- c("sim_values", "final_GP")
  
  return(output)
  
}



a <- pred_B(GR = 0.5, M_ref = 1.6, L_ref = 131.5, Lm = 131.5, K = 0.33, Linf = 219,
            t0 = -1.1, t.steps = 365*10, L0 = 20, N0 = 0.4, a = 0.0064, b = 2.6, plot = TRUE, K_sup = 10)


