#-------------------------------------------------------------------------------
# SENSITIVITY ANALYSES
#-------------------------------------------------------------------------------

sensitivity <- function(par_to_vary, input_range = c(0,100), baseline = list(mean, low, up),
                        par_unc, GR.mu = NULL, GR.sd = NULL, N0.mu = NULL, N0.sd = NULL,
                        M_ref.mu = NULL, M_ref.sd = NULL, L_ref.mu = NULL, L_ref.sd = NULL,
                        Lm.mu = NULL, Lm.sd = NULL, K.mu = NULL, K.sd = NULL, 
                        Linf.mu = NULL, Linf.sd = NULL, t0.mu = NULL, t0.sd = NULL,
                        lwa.mu = NULL, lwa.sd = NULL, lwb.mu = NULL, lwb.sd = NULL,
                        L0.mu = NULL, L0.sd = NULL, N_sim = 1000, N_ite = 1000) {
  
  # random number generation for reproducible results
  set.seed(13)
  
  # setup error messages
  if (par_to_vary != "GR" && par_to_vary != "K" && par_to_vary != "Linf" && par_to_vary != "Lm") 
  {stop("Please specify which parameter to perform the sensitivity analysis on. Options are 'GR'(growth rate in mm/day), 'K' (von Bertalanffy growth rate), 'Linf' (von Bertalanffy asymptotic length), and 'Lm' (size at maturation) ")}

  if (is.list(par_to_vary)) {stop("Only one parameter to vary can be specified")}
  
  if (is.null(range)) {stop("Please specify the range of variation in the specific parameter")}
  
  if (!(is.list(baseline))) {stop("Baseline parameter values should be a list in format list(mean, lower, upper)")}

  # make a table to store values whithin the parameter range, with size = N_sim
  range <- seq(from = input_range[1], to = input_range[2], length.out = N_sim)
  
  # running time
  start.time <- Sys.time()
  
  # setup simulations
  out <- NULL
  if (par_to_vary == "GR") {
  for (i in 1:N_sim) {
      out[[i]] <- stoch_P(GR.mu = range[i], GR.sd = GR.sd, N0.mu = N0.mu, N0.sd = N0.sd,
                        M_ref.mu = M_ref.mu, M_ref.sd = M_ref.sd, L_ref.mu = L_ref.mu, L_ref.sd = L_ref.sd,
                        Lm.mu = Lm.mu, Lm.sd = Lm.sd, K.mu = K.mu, K.sd = K.sd, Linf.mu = Linf.mu, Linf.sd = Linf.sd,
                        t0.mu = t0.mu, t0.sd = t0.sd, lwa.mu = lwa.mu, lwa.sd = lwa.sd, lwb.mu = lwb.mu, lwb.sd = lwb.sd,
                        L0.mu = L0.mu, L0.sd = L0.sd, N = N_ite, N_years = 10, K_sup.mu = 100, K_sup.sd = 1, output.type = "basic")
      }
  } else if (par_to_vary == "K") {
    for (i in 1:N_sim) {
      out[[i]] <- stoch_P(GR.mu = GR.mu, GR.sd = GR.sd, N0.mu = N0.mu, N0.sd = N0.sd,
                       M_ref.mu = M_ref.mu, M_ref.sd = M_ref.sd, L_ref.mu = L_ref.mu, L_ref.sd = L_ref.sd,
                       Lm.mu = Lm.mu, Lm.sd = Lm.sd, K.mu = range[i], K.sd = K.sd, Linf.mu = Linf.mu, Linf.sd = Linf.sd,
                       t0.mu = t0.mu, t0.sd = t0.sd, lwa.mu = lwa.mu, lwa.sd = lwa.sd, lwb.mu = lwb.mu, lwb.sd = lwb.sd,
                       L0.mu = L0.mu, L0.sd = L0.sd, N = N_ite, N_years = 10, K_sup.mu = 100, K_sup.sd = 1, output.type = "basic")
      }
  } else if (par_to_vary == "Linf") {
    for (i in 1:N_sim) {
      out[[i]] <- stoch_P(GR.mu = GR.mu, GR.sd = GR.sd, N0.mu = N0.mu, N0.sd = N0.sd,
                        M_ref.mu = M_ref.mu, M_ref.sd = M_ref.sd, L_ref.mu = L_ref.mu, L_ref.sd = L_ref.sd,
                        Lm.mu = Lm.mu, Lm.sd = Lm.sd, K.mu = K.mu, K.sd = K.sd, Linf.mu = range[i], Linf.sd = Linf.sd,
                        t0.mu = t0.mu, t0.sd = t0.sd, lwa.mu = lwa.mu, lwa.sd = lwa.sd, lwb.mu = lwb.mu, lwb.sd = lwb.sd,
                        L0.mu = L0.mu, L0.sd = L0.sd, N = N_ite, N_years = 10, K_sup.mu = 100, K_sup.sd = 1, output.type = "basic")
      }
  } else if (par_to_vary == "Lm") {
    for (i in 1:N_sim) {
      out[[i]] <- stoch_P(GR.mu = GR.mu, GR.sd = GR.sd, N0.mu = N0.mu, N0.sd = N0.sd,
                        M_ref.mu = M_ref.mu, M_ref.sd = M_ref.sd, L_ref.mu = L_ref.mu, L_ref.sd = L_ref.sd,
                        Lm.mu = range[i], Lm.sd = Lm.sd, K.mu = K.mu, K.sd = K.sd, Linf.mu = Linf.mu, Linf.sd = Linf.sd,
                        t0.mu = t0.mu, t0.sd = t0.sd, lwa.mu = lwa.mu, lwa.sd = lwa.sd, lwb.mu = lwb.mu, lwb.sd = lwb.sd,
                        L0.mu = L0.mu, L0.sd = L0.sd, N = N_ite, N_years = 10, K_sup.mu = 100, K_sup.sd = 1, output.type = "basic")
      }
  }
  
  # PLAYING WITH OUTPUTS
  GP <- NULL
  mean_GP <- NULL
  low_GP <- NULL
  high_GP <- NULL
  for (i in 1:N_sim) {
  
     mean_GP[i] <- out[[i]][1]
     low_GP[i] <- out[[i]][2]
     high_GP[i] <- out[[i]][3]
      
    }

  GP_df <- data.frame(range, mean_GP, low_GP, high_GP)
  
  # convert baseline parameter value to GP first
  baseline_GP <- NULL
  if (par_to_vary == "GR") {
  
    baseline_GP <- stoch_P(GR.mu = baseline$mean, GR.sd = ((baseline$up - baseline$low)/3.92), N0.mu = N0.mu, N0.sd = N0.sd,
                        M_ref.mu = M_ref.mu, M_ref.sd = M_ref.sd, L_ref.mu = L_ref.mu, L_ref.sd = L_ref.sd,
                        Lm.mu = Lm.mu, Lm.sd = Lm.sd, K.mu = K.mu, K.sd = K.sd, Linf.mu = Linf.mu, Linf.sd = Linf.sd,
                        t0.mu = t0.mu, t0.sd = t0.sd, lwa.mu = lwa.mu, lwa.sd = lwa.sd, lwb.mu = lwb.mu, lwb.sd = lwb.sd,
                        L0.mu = L0.mu, L0.sd = L0.sd, N = N_ite, N_years = 10, K_sup.mu = 100, K_sup.sd = 1, output.type = "basic")
  
  } else if (par_to_vary == "K") {
      
    baseline_GP <- stoch_P(GR.mu = GR.mu, GR.sd = GR.sd, N0.mu = N0.mu, N0.sd = N0.sd,
                           M_ref.mu = M_ref.mu, M_ref.sd = M_ref.sd, L_ref.mu = L_ref.mu, L_ref.sd = L_ref.sd,
                           Lm.mu = Lm.mu, Lm.sd = Lm.sd, K.mu = baseline$mean, K.sd = ((baseline$up - baseline$low)/3.92), Linf.mu = Linf.mu, Linf.sd = Linf.sd,
                           t0.mu = t0.mu, t0.sd = t0.sd, lwa.mu = lwa.mu, lwa.sd = lwa.sd, lwb.mu = lwb.mu, lwb.sd = lwb.sd,
                           L0.mu = L0.mu, L0.sd = L0.sd, N = N_ite, N_years = 10, K_sup.mu = 100, K_sup.sd = 1, output.type = "basic")
    
  } else if (par_to_vary == "Linf") {
    
    baseline_GP <- stoch_P(GR.mu = GR.mu, GR.sd = GR.sd, N0.mu = N0.mu, N0.sd = N0.sd,
                           M_ref.mu = M_ref.mu, M_ref.sd = M_ref.sd, L_ref.mu = L_ref.mu, L_ref.sd = L_ref.sd,
                           Lm.mu = Lm.mu, Lm.sd = Lm.sd, K.mu = K.mu, K.sd = K.sd, Linf.mu = baseline$mean, Linf.sd = ((baseline$up - baseline$low)/3.92),
                           t0.mu = t0.mu, t0.sd = t0.sd, lwa.mu = lwa.mu, lwa.sd = lwa.sd, lwb.mu = lwb.mu, lwb.sd = lwb.sd,
                           L0.mu = L0.mu, L0.sd = L0.sd, N = N_ite, N_years = 10, K_sup.mu = 100, K_sup.sd = 1, output.type = "basic")
    
  } else if (par_to_vary == "Lm") {
    
    baseline_GP <- stoch_P(GR.mu = GR.mu, GR.sd = GR.sd, N0.mu = N0.mu, N0.sd = N0.sd,
                           M_ref.mu = M_ref.mu, M_ref.sd = M_ref.sd, L_ref.mu = L_ref.mu, L_ref.sd = L_ref.sd,
                           Lm.mu = baseline$mean, Lm.sd = ((baseline$up - baseline$low)/3.92), K.mu = K.mu, K.sd = K.sd, Linf.mu = Linf.mu, Linf.sd = Linf.sd,
                           t0.mu = t0.mu, t0.sd = t0.sd, lwa.mu = lwa.mu, lwa.sd = lwa.sd, lwb.mu = lwb.mu, lwb.sd = lwb.sd,
                           L0.mu = L0.mu, L0.sd = L0.sd, N = N_ite, N_years = 10, K_sup.mu = 100, K_sup.sd = 1, output.type = "basic")
    
  }
  
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,2)
  time.taken
  
  baseline_GP_mean <- baseline_GP[1] # get mean
  baseline_GP_low <- baseline_GP[2] # get lower value
  baseline_GP_up <- baseline_GP[3] # get upper value
  
  # convert to relative difference
  GP_df$mean_rel <- -((baseline_GP_mean - GP_df$mean_GP)/GP_df$mean_GP)*100
  GP_df$low_rel <- -((baseline_GP_low - GP_df$low_GP)/GP_df$low_GP)*100
  GP_df$up_rel <- -((baseline_GP_up - GP_df$high_GP)/GP_df$high_GP)*100
  
  output <- list(time.taken, GP_df)
  names(output) <- c("Run time", "Sensitivity output")
  
  # plotting
  par(mfrow = c(1,1))
  plot(GP_df$mean_rel ~ GP_df$range, type = "l",
       main = "Sensitivity output",
       xlab = "Range", ylab = "Sensitivity (% change)")
  abline(v = baseline$mean, col = "red", lty = 2, lwd = 2)
  lines(GP_df$low_rel ~ GP_df$range, lty = 2)
  lines(GP_df$up_rel ~ GP_df$range, lty = 2)
  legend("bottomright", legend=c("Baseline", "Mean", "95% C.I."),
         col=c("red", "black", "black"), lty=2:1, cex=0.8)
  
  return(output)
  
}

a <- sensitivity(par_to_vary = "K", input_range = c(0.25,0.4), 
                 baseline = list(mean = 0.33, low = 0.3, up = 0.35),
              GR.mu = 0.45, GR.sd = 0.03, N0.mu = 1, N0.sd = 0.01,
             M_ref.mu = 1.6, M_ref.sd = 0.05, L_ref.mu = 120, L_ref.sd = 5,
             Lm.mu = 120, Lm.sd = 0, K.mu = 0.33, K.sd = 0.1, Linf.mu = 200,
             Linf.sd = 30, t0.mu = 0, t0.sd = 0, lwa.mu = 0.005,
             lwa.sd = 0.000000001, lwb.mu = 3.25, lwb.sd = 0.1,
             L0.mu = 20, L0.sd = 0,
             N_sim = 100, N_ite = 500)


a

plot(a$mean_GP ~ a$range)

start.time <- Sys.time()

# Your R code here
result <- sum(1:10000)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken













