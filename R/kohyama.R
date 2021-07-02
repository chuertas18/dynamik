#' @title Kohyama
#' @description 
#' 
#' @param 
#' @param 
#' @param varT: Variable containing period information
#' @param var0 : Variable indicating the status of the individual: dead, survivor or recruits.
#' @param 
#' @return 
#' @export 
#' @examples kohyama(database=d0,model=3,var1="treat",var2="habitat",iterations=10,varT="period",varO="state")
# Model 1: no random effect (homogeneous)
# Model 2: one random effect
# Model 3: two-level nested random effects

database=d0
model=2
var1="treat"
var2="habitat"
iterations=500
varT="period"
varO="state"


kohyama<-function(database,model,var1,var2,iterations,varT,varO){

####################################################################################"
names(database)[names(database) == var1] <- "variable1"
names(database)[names(database) == var2] <- "variable2"
names(database)[names(database) == varT] <- "T"
names(database)[names(database) == varO] <- "O"


database$T=as.integer(database$T)
database$variable1=as.factor(database$variable1)
database$variable2=as.factor(database$variable2)
head(database)


database$O = factor(ifelse(database$O  == "survivors", "surv",
                    ifelse(database$O  == "recruits", "recr","dead")),
             levels = c('surv', 'dead', 'recr'))




table(database$O)
database=as.data.frame(database)


library(R2jags)
library(RColorBrewer)
library(gtools)
library(reshape2)
library(plyr)
library(ggplot2)
library(tidyr)




# Model 1: no random effect (homogeneous)
mod1 = "
model { for (i in 1:N)
{
  ### model likelihood
  # observation O (survivors  or dead)
  O[i] ~ dcat(p[i,])
  # probability of survival
  p[i, 1] = 1/(1 + p2_p1[i] + p3_p1[i])
  # probability of death
  p[i, 2] = p2_p1[i]/(1 + p2_p1[i] + p3_p1[i])
  # recruitment
  p[i, 3] = p3_p1[i]/(1 + p2_p1[i] + p3_p1[i])
  # death over survival
  p2_p1[i] = exp(m[i] * T_[i]) - 1
  # recruitment over survival
  p3_p1[i] = exp(r[i] * T_[i]) - 1
  # mortality
  m[i] = exp(log_m_hat)
  # recruitment
  r[i] = exp(log_r_hat)
}
# priors
log_m_hat = beta0_m
log_r_hat = beta0_r
beta0_m ~ dnorm(0, 0.0001)
beta0_r ~ dnorm(0, 0.0001)
}
"

# Model 2: one random effect
mod2 = "
model { for (i in 1:N)
{
  # model likelihood
  O[i] ~ dcat(p[i,])
  p[i, 1] = 1/(1 + p2_p1[i] + p3_p1[i])
  p[i, 2] = p2_p1[i]/(1 + p2_p1[i] + p3_p1[i])
  p[i, 3] = p3_p1[i]/(1 + p2_p1[i] + p3_p1[i])
  p2_p1[i] = exp(m[i] * T_[i]) - 1
  p3_p1[i] = exp(r[i] * T_[i]) - 1
  m[i] = exp(log_m_hat[lv1_idx[i]])
  r[i] = exp(log_r_hat[lv1_idx[i]])
}
# priors
for (j in 1:N_lv1)
{
  log_m_hat[j] = beta0j_m[j]
  log_r_hat[j] = beta0j_r[j]
  beta0j_m[j] ~ dnorm(beta0_m, pow(sigma_u0_m, -2))
  beta0j_r[j] ~ dnorm(beta0_r, pow(sigma_u0_r, -2))
}
beta0_m ~ dnorm(0, 0.0001)
beta0_r ~ dnorm(0, 0.0001)
sigma_u0_m ~ dt(0, pow(2.5, -2), 1)T(0,)
sigma_u0_r ~ dt(0, pow(2.5, -2), 1)T(0,)
}
"

# Model 3: two-level nested random effects
mod3 = "
model { for (i in 1:N)
{
  # model likelihood
  O[i] ~ dcat(p[i,])
  p[i, 1] = 1/(1 + p2_p1[i] + p3_p1[i])
  p[i, 2] = p2_p1[i]/(1 + p2_p1[i] + p3_p1[i])
  p[i, 3] = p3_p1[i]/(1 + p2_p1[i] + p3_p1[i])
  p2_p1[i] = exp(m[i] * T_[i]) - 1
  p3_p1[i] = exp(r[i] * T_[i]) - 1
  m[i] = exp(log_m_hat[lv2_idx[i]])
  r[i] = exp(log_r_hat[lv2_idx[i]])
}
# priors
for (k in 1:N_lv2)
{
  log_m_hat[k] = beta0jk_m[k]
  log_r_hat[k] = beta0jk_r[k]
  beta0jk_m[k] ~ dnorm(beta0j_m[lv1_idx[k]], pow(sigma_v0_m, -2))
  beta0jk_r[k] ~ dnorm(beta0j_r[lv1_idx[k]], pow(sigma_v0_r, -2))
}
for (j in 1:N_lv1)
{
  beta0j_m[j] ~ dnorm(beta0_m, pow(sigma_u0_m, -2))
  beta0j_r[j] ~ dnorm(beta0_r, pow(sigma_u0_r, -2))
}
beta0_m ~ dnorm(0, 0.0001)
beta0_r ~ dnorm(0, 0.0001)
sigma_u0_m ~ dt(0, pow(2.5, -2), 1)T(0,)
sigma_u0_r ~ dt(0, pow(2.5, -2), 1)T(0,)
sigma_v0_m ~ dt(0, pow(2.5, -2), 1)T(0,)
sigma_v0_r ~ dt(0, pow(2.5, -2), 1)T(0,)
}
"



# function for fitting the JAGS model
fit_jags = function(data, random = NULL, inits = NULL,
                    pars = c('log_m_hat', 'log_r_hat'),
                    n.iter = iterations, n.burnin = floor(n.iter/2),
                    n.thin = 1, n.chains = 4)
{
  if(length(random) > 2) {
    stop(paste('If you want to include more than two random effects,',
               'you need to modify the model.'))
  } else {
    jags_data = list('O' = as.numeric(data$O),
                     'N' = nrow(data),
                     'T_' = data$T)
    if (length(random) == 0) {
      model_string = mod1
      subpop = rep(1, jags_data$N)
    } else if (length(random) == 1) {
      model_string = mod2
      jags_data$lv1_idx = factor(data[, random[1]])
      jags_data$N_lv1 = length(levels(factor(data[, random[1]])))
    } else if (length(random) == 2) {
      pars = c(pars, 'beta0j_m', 'beta0j_r')
      model_string = mod3
      jags_data$lv2_idx = factor(paste(data[, random[1]],
                                       data[, random[2]], sep = '/'))
      jags_data$lv1_idx = factor(gsub('(.*)/(.*)', '\\1',
                                      levels(jags_data$lv2_idx)),
                                 levels = levels(factor(data[, random[1]])))
      jags_data$N_lv2 = length(levels(jags_data$lv2_idx))
      jags_data$N_lv1 = length(levels(jags_data$lv1_idx))
    }
    cat(model_string, file = (model_tmp = tempfile()))
    jags_out = jags(model.file = model_tmp, data = jags_data,
                    parameters.to.save = pars, inits = inits,
                    n.iter = n.iter, n.burnin = n.burnin,
                    n.thin = n.thin, n.chains = n.chains)
    return(list('jags_out' = jags_out, 'jags_data' = jags_data))
  }
}

# function for calculating density-weighted means of vital rates
get_weighted_mean = function(obj){
  jags_out = obj$jags_out
  data = obj$jags_data
  post = as.matrix(as.mcmc(jags_out))
  post = post[, mixedorder(colnames(post))]
  if ('lv2_idx' %in% names(data)) {
    subpop = data$lv2_idx
  } else if ('lv1_idx' %in% names(data)) {
    subpop = data$lv1_idx
  } else {
    subpop = rep(1, data$N)
  }
  N_subpop = length(unique(subpop))
  N0 = as.numeric(tapply(data$O != 3, subpop, sum))
  NT = as.numeric(tapply(data$O != 2, subpop, sum))
  NsT = as.numeric(tapply(data$O == 1, subpop, sum))
  m = exp(post[, grep('^log_m_hat', colnames(post))])
  r = exp(post[, grep('^log_r_hat', colnames(post))])
  if (0 %in% NsT) {
    N0 = N0[NsT != 0]
    NT = NT[NsT != 0]
    m = m[, NsT != 0]
    r = r[, NsT != 0]
    warning(paste('Excluding subpopulation without any survivor that prevents',
                  'calculation of Nw and population-wide calculation of Nw',
                  'and population-wide mean vital rates.'))
  }
  g = r - m
  # period-mean density
  Nw = ifelse(NT != N0, (NT-N0)/log(NT/N0), N0)
  Wmean_r = colSums(t(r) * Nw)/sum(Nw)
  Wmean_m = colSums(t(m) * Nw)/sum(Nw)
  Wmean_g = colSums(t(g) * Nw)/sum(Nw)
  return(list('r' = Wmean_r, 'm' = Wmean_m, 'g' = Wmean_g))
}

# Run JAGS
# NOTE: The number of iterations used here is reduced to make the example run quick.
if (model==1) {

  # function for fitting the JAGS model
  fit1 = fit_jags(data=database, random = NULL, n.iter = iterations, n.thin = 1)

  # Check convergence
  jags_out = fit1$jags_out
  # posterior summary
  print(summary(as.mcmc(jags_out)))
  # Gelman and Rubin's convergence diagnostic
  print(gelman.diag(as.mcmc(jags_out)))
  # traceplot
  traceplot(jags_out, col = brewer.pal(jags_out$BUGSoutput$n.chains, 'PuOr'))

  # Calculate density-weighted means of vital rates for the entire population
  res1 = get_weighted_mean(fit1)

  # Plot density-weighted means of vital rates
  nd1 = data.frame('mcmc.id' = 1:fit1$jags_out$BUGSoutput$n.sims)
  for(i in 1:1){
    res = get_weighted_mean(get(sprintf('fit%i', i)))
    nd1[, sprintf('m_%i', i)] = res$m
    nd1[, sprintf('r_%i', i)] = res$r
    nd1[, sprintf('g_%i', i)] = res$g
  }
  nd1 = melt(nd1, id.vars = "mcmc.id")
  nd1[, 4:5] = t(matrix(unlist(strsplit(as.character(nd1$variable), '_')), nrow = 2))
  names(nd1)[4:5] = c('Parameter', 'Group')
  nd1 = ddply(nd1, .(variable), summarize,
              median = median(value),
			  mean = mean(value),
              ymax = HPDinterval(as.mcmc(value))[1],
              ymin = HPDinterval(as.mcmc(value))[2],
              Parameter = unique(Parameter),
              Group = unique(Group))
  nd1$period<-unique(database$per)
  return(nd1)


} else if (model==2) {

  # function for fitting the JAGS model
  fit2 = fit_jags(data=database, random = "variable1", n.iter = iterations, n.thin = 1)

  # Check convergence
  jags_out = fit2$jags_out
  # posterior summary
  # print(summary(as.mcmc(jags_out)))
  # # Gelman and Rubin's convergence diagnostic
  # print(gelman.diag(as.mcmc(jags_out)))
  # # traceplot
  # traceplot(jags_out, col = brewer.pal(jags_out$BUGSoutput$n.chains, 'PuOr'))

  # Calculate density-weighted means of vital rates for the entire population
  res2 = get_weighted_mean(fit2)

  # Plot m vs r for each Family MODEL 2
  post = as.matrix(as.mcmc(fit2$jags_out))
  post = post[, mixedorder(colnames(post))]
  m = exp(post[, grep('log_m_hat', colnames(post))])
  r = exp(post[, grep('log_r_hat', colnames(post))])


  nd2 = data.frame('var1' = levels(database$variable1),
                   'N0' = tapply(database$O!="recr", database$variable1, sum),
                   'm_med' = apply(m, 2, median),
				   'm_mean' = apply(m, 2, mean),
                   'm_lower' = HPDinterval(as.mcmc(m))[,1],
                   'm_upper' = HPDinterval(as.mcmc(m))[,2],
                   'r_med' = apply(r, 2, median),
                   'r_lower' = HPDinterval(as.mcmc(r))[,1],
                   'r_upper' = HPDinterval(as.mcmc(r))[,2]
  )

  nd2$period<-unique(database$per)
  return(nd2)
  
} else if (model==3) {

  # function for fitting the JAGS model
  fit3 = fit_jags(data=database, random = c("variable1", "variable2"), n.iter = iterations, n.thin = 1)

  # Check convergence
  jags_out = fit3$jags_out
  # posterior summary
  print(summary(as.mcmc(jags_out)))
  # Gelman and Rubin's convergence diagnostic
  #print(gelman.diag(as.mcmc(jags_out)))
  # traceplot
  #traceplot(jags_out, col = brewer.pal(jags_out$BUGSoutput$n.chains, 'PuOr'))

  # Calculate density-weighted means of vital rates for the entire population
  res3 = get_weighted_mean(fit3)


  # Plot m vs r for each Family MODEL 2
  post = as.matrix(as.mcmc(fit3$jags_out))
  post = post[, mixedorder(colnames(post))]
  m = exp(post[, grep('log_m_hat', colnames(post))])
  r = exp(post[, grep('log_r_hat', colnames(post))])




  nd3 = data.frame('id' = levels(fit3$jags_data$lv2_idx),
                   'var'= levels(fit3$jags_data$lv2_idx),
                   'm_med' = apply(m, 2, median),
				   'm_mean' = apply(m, 2, mean),
                   'm_lower' = HPDinterval(as.mcmc(m))[,1],
                   'm_upper' = HPDinterval(as.mcmc(m))[,2],
                   'r_med' = apply(r, 2, median),
                   'r_lower' = HPDinterval(as.mcmc(r))[,1],
                   'r_upper' = HPDinterval(as.mcmc(r))[,2]
  )

  nd3<-separate(nd3,id,into=c( "var1", "var2" ),sep="/")
  nd3$period<-unique(database$per)
  return(nd3)


}
else {
  print("Error in model selection")
}

}
