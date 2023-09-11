require(rstan)
rstan_options(auto_write = TRUE)

# WRITE STAN MODEL -------------------------------------------------------------

write("data {

int<lower=0> N_obs;         // number of observations
int<lower=0> N_sites;       // number of sites
int<lower=1> site[N_obs];   // variable for random effects
real age [N_obs];           // predictor variable
real TL [N_obs];            // response variable

}

parameters {

real<lower=0>alpha;                 // population intercept, fixed
real beta_site[N_sites];            // random slopes
real<lower=0> sigma_rd[N_sites];    // random slope SDs
real<lower=0> sigma_alpha;          // SD for fixed intercept
real<lower=0> sigma;                // observation SD

}

transformed parameters  {

real mu[N_obs];

for (i in 1:N_obs) {
   
mu[i] = alpha + beta_site[site[i]]*age[i];

}

}
 
model {

alpha ~ normal(5, sigma_alpha);
sigma_alpha ~ uniform(0,10)
beta_site ~ normal(0.5, sigma_rd);

for (i in 1:N_sites) {

sigma_rd[i] ~ uniform(0,10)

}

for (i in 1:N_obs) {

TL[i] ~ normal(mu[i], sigma); // LIKELIHOOD

}

}

generated quantities {

vector[N_obs] y_hat;

for (i in 1:N_obs) {

y_hat[i] = alpha + beta_site[site[i]]*age[i]; // POSTERIOR PREDICTIVE 

}

}
",
"
modelstring.stan
")


stanc('modelstring.stan')

# WRANGLE DATA -----------------------------------------------------------------
OM <- na.exclude(OM)
TL <- OM$TL
age <- OM$Age
site <- as.numeric(as.factor(OM$Site))
G <- OM$TL/OM$Age

plot(G ~ TL)

datalist <- list(N_obs = length(TL), N_sites = length(unique(site)),
                 TL = TL, age = age, site = site)

res <- stan(file = 'modelstring.stan', data = datalist, pars = c("beta_site","alpha"),
                chains = 3, iter = 10000, warmup = 1000, thin = 1)

stan_dens(res, pars = "beta_site")

plot(res, pars = "beta_site")

traceplot(res, pars = c("beta_site"), inc_warmup = TRUE)

library(bayesplot)

color_scheme_set("teal")

mcmc_areas(res, pars = c("beta_site[1]", "beta_site[2]",
                         "beta_site[3]", "beta_site[4]",
                         "beta_site[5]"), prob = 0.95,
           point_est = "median") +
  scale_y_discrete(expand = c(0, 0), labels = c("beta_site[1]" = "AM-C",
                                                "beta_site[2]" = "DR-C",
                                                "beta_site[3]" = "LB-C",
                                                "beta_site[4]" = "PaP-T",
                                                "beta_site[5]" = "SA-T")) +
  xlab("Growth rate (mm/day)")

pp_check(datalist$TL, 
  rstan::extract(res, par = 'y_hat')$y_hat[1:95, ], 
  fun = 'dens_overlay')









