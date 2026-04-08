## ----chapter4-init-stanza, include = FALSE, cache = FALSE, echo = FALSE-------
library(bnlearn)
library(circular)
library(rstan)


## ----ae-two-submodels---------------------------------------------------------
dag = model2network("[I][A][S|I:A][O][B][W|S:O:B]")
dsep(dag, x = "A", y = "W", z = "S")
dsep(dag, x = "I", y = "O", z = "S")


## ----age-distribution-quantiles-----------------------------------------------
round(100 * qbeta(0.25, 1.1, 1.5))
round(100 * qbeta(0.80, 1.1, 1.5))


## ----figure-for-age-distribution, echo = FALSE, results = "hide", warning = FALSE----
local({
pdf("figures/distributions.pdf", width = 6, height = 8)
par(mfrow = c(3, 1), mar = c(4.75, 4, 0.1, 0.1))
hist(round(100 * rbeta(10^6, shape1 = 1.1, shape2 = 1.5)),
  xlab = "Age", ylab = "probability", main = "", freq = FALSE,
  xlim = c(0, 100), axes = FALSE)
axis(2, label = FALSE)
axis(1)
hist(rbeta(10^6, shape1 = 16, shape2 = 2),
  xlab = "Occupancy Rate", ylab = "probability", main = "", freq = FALSE,
  xlim = c(0, 1), axes = FALSE)
axis(2, label = FALSE)
axis(1, at = c(0, 0.5, 0.75, 0.85, 0.95, 1))
hist(as.numeric(rvonmises(10000, 0.0001 - pi, 2) / (2 * pi) * 24),
  xlab = "Time of the day", ylab = "probability",  main = "", freq = FALSE,
  xlim = c(0, 24), axes = FALSE)
axis(2, label = FALSE)
axis(1, at = c(0, 4, 8, 12, 16, 20, 24))
dev.off()
})


## ----load-rstan---------------------------------------------------------------
library(rstan)


## ----rstan-model-definition---------------------------------------------------
stancode <- 'data {
  vector[2] Ap; // shape parameters for the beta distribution.
  vector[4] Ip; // probabilities for incident types.
  vector[6] Sp; // regression coefficients, logistic regression.
  vector[2] Op; // parameters for the beta distribution.
  vector[2] Tp; // parameters for the von Mises distribution.
  vector[5] Wp; // regression coefficients, log-linear regression.
}
generated quantities {
  real A;
  int I;
  real S;
  real O;
  real W;
  real T;
  A = ceil(beta_rng(Ap[1], Ap[2]) * 100);
  I = categorical_rng(Ip);
  S = 2 + binomial_rng(10, inv_logit(Sp[1] + A * Sp[2] + Sp[2 + I]));
  O = beta_rng(Op[1], Op[2]);
  T = (von_mises_rng(Tp[1], Tp[2]) + pi()) / (2 * pi()) * 24;
  W = lognormal_rng(Wp[1] + O * Wp[2] + (12 - S) * Wp[3] +
                    fmax(6 - fabs(T - 12), 0) * Wp[4], Wp[5]);
}'


## ----rstan-load-model---------------------------------------------------------
data.model <- stan_model(model_code = stancode)


## ----rstan-generate-----------------------------------------------------------
params <- list(
  Ap = c(1.1, 1.5),
  Ip = c(0.075, 0.50, 0.25, 0.175),
  Sp = c(7, -0.05, 0, -4, -3, -1),
  Op = c(12, 2),
  Tp = c(0.001, 2),
  Wp = c(log(20), 2 * log(2), -0.5 * log(2), 0.25 * log(2), 1)
)
fit <- sampling(data.model, algorithm = "Fixed_param",
         data = params, thin = 25, iter = 50000, seed = 42)
nodes <- c("A", "I", "S", "O", "T", "W")
aewait <- as.data.frame(extract(fit)[nodes])


## ----stan-save-observations, echo = FALSE-------------------------------------
if (!file.exists("aewait.rds")) {

  aewait$I <- factor(aewait$I, labels = c("domestic", "road", "work", "other"))
  saveRDS(aewait, file = "aewait.rds")

} else {

  aewait <- readRDS("aewait.rds")

}#ELSE


## ----stan-generated-data------------------------------------------------------
aewait$I <- factor(aewait$I,
              labels = c("domestic", "road", "work", "other"))
head(aewait)


## ----stan-trauma-scores-summary-----------------------------------------------
S.cdf <- ecdf(aewait$S)
S.cdf(c(3, 7, 10, 11, 12))


## ----stan-waiting-times-summary-----------------------------------------------
W.cdf <- ecdf(aewait$W)
W.cdf(c(10, 30, 60, 120, 180, 240))


## ----stan-waiting-times-critical----------------------------------------------
nS <- length(which(aewait$S <= 3))
length(which((aewait$S <= 3) & (aewait$W < 10))) / nS
length(which((aewait$S <= 3) & (aewait$W < 30))) / nS


## ----stan-waiting-times-fine--------------------------------------------------
nW <- length(which(aewait$W > 240))
length(which((aewait$S >= 10) & (aewait$W > 240))) / nW


## ----stan-posterior-parameters-model------------------------------------------
stancode <- 'data {
  int<lower=1> n;
  vector<lower=0,upper=1>[n] A;
  int<lower=1,upper=4> I[n];
  matrix[n, 4] Im;
  int<lower=0,upper=10> S[n];
  vector<lower=0,upper=10>[n] Scomp;
  vector<lower=0,upper=1>[n] O;
  vector<lower=-pi(),upper=pi()>[n] T;
  vector<lower=0,upper=6>[n] Ttri;
  vector<lower=0>[n] W;
}
parameters {
  real<lower=0> Ap[2];
  simplex[4] Ip;
  vector[4] Sbeta;
  real Sa;
  real<lower=0> Op[2];
  real<lower=-pi(),upper=pi()> Tmu;
  real<lower=0> Ts;
  real Wp[4];
  real<lower=0> Ws;
}
model {
  A ~ beta(Ap[1], Ap[2]);
  I ~ categorical(Ip);
  S ~ binomial_logit(10, A * Sa + Im * Sbeta);
  O ~ beta(Op[1], Op[2]);
  T ~ von_mises(Tmu, Ts);
  W ~ lognormal(Wp[1] + O * Wp[2] + Scomp * Wp[3] + Ttri * Wp[4], Ws);
}'


## ----incident-model-matrix----------------------------------------------------
Im <- model.matrix(~ I, data = aewait)
I <- as.integer(aewait$I)


## ----rstan-data-preprocessing-------------------------------------------------
A <- as.numeric(aewait$A / 100)
A[A == 0] <- .Machine$double.eps
A[A == 1] <- 1 - .Machine$double.eps
S <- as.integer(aewait$S - 2)
Scomp <- 10 - S;
T <- aewait$T / 24 * (2 * pi) -pi
Ttri <- pmax(6 - abs(aewait$T - 12), 0)


## ----load-presampled-parameters, echo = FALSE---------------------------------
aeparams <- readRDS("aeparams.rds")


## ----rstan-posterior-parameters-sampling, eval = FALSE------------------------
## parameters.model <- stan_model(model_code = stancode)
## fit <- sampling(model, iter = 3500, seed = 42, thin = 25,
##          data = list(n = nrow(aewait), A = A, I = I, S = S,
##                      Scomp = Scomp, O = aewait$O, T = T,
##                      Ttri = Ttri, W = aewait$W))
## aeparams <- as.data.frame(extract(fit))


## ----eval = FALSE-------------------------------------------------------------
## Op[1] ~ exponential(0.1);
## Op[2] ~ exponential(0.5);

