write("data {

int<lower=0> N_obs;         // number of observations
int<lower=0> N_sites;       // number of sites
int<lower=1> site[N_obs];   // categorical covariate
real weight [N_obs];        // response 
real TL [N_obs];            // predictor 

}

parameters {

real<lower=0> alpha;                // population intercept, fixed
real beta_site[N_sites];            // random slopes
real<lower=0> sigma_rd[N_sites];    // random slope SDs
real<lower=0> sigma_alpha;          // SD for fixed intercept
real<lower=0> sigma;                // observation SD

}

transformed parameters  {

real mu[N_obs];

for (i in 1:N_obs) {
   
  mu[i] = alpha * (TL[i]^beta_site[site[i]]);

}

}
 
model {

alpha ~ normal(0.5, sigma_alpha);
sigma_alpha ~ cauchy(0,1);
beta_site ~ normal(0.5, sigma_rd);

for (i in 1:N_sites) {

  sigma_rd[i] ~ cauchy(0,1);

}

for (i in 1:N_obs) {

  weight[i] ~ normal(mu[i], sigma); // LIKELIHOOD

}

}
",
"string.stan")


stanc('string.stan')


OM <- na.exclude(OM)
TL <- OM$TL
WW <- OM$WW
site <- as.numeric(as.factor(OM$Site))

plot(WW ~ TL)

datalist <- list(N_obs = length(TL), N_sites = length(unique(site)),
                 TL = TL, weight = WW, site = site)

res <- stan(file = 'string.stan', data = datalist, pars = c("beta_site","alpha"),
            chains = 3, iter = 10000, warmup = 1000, thin = 1, algorithm = "NUTS")


print(res)


stan_plot(res, pars = "beta_site")

