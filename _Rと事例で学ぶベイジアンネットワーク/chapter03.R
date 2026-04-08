## ----chapter3-init-stanza, include = FALSE, cache = FALSE, echo = FALSE-------
library(lme4)
library(bnlearn)


## ----healthcare-network-structure---------------------------------------------
dag <- model2network("[A][C|A][H|A][D|A:H][I|C:D][O|A][T|O:I]")


## ----healthcare-A-------------------------------------------------------------
A.lv <- c("young", "adult", "old")
A.prob <- array(c(0.35, 0.45, 0.20), dim = 3, dimnames = list(A = A.lv))

## ----healthcare-A-print-------------------------------------------------------
A.prob


## ----healthcare-C-------------------------------------------------------------
C.lv <- c("none", "mild", "severe")
C.prob <- array(c(0.88, 0.10, 0.02, 0.75, 0.20, 0.05, 0.42, 0.53, 0.05),
            dim = c(3, 3), dimnames = list(C = C.lv, A = A.lv))
C.prob


## ----healthcare-D-------------------------------------------------------------
H.lv <- c("none", "any")
H.prob <- array(c(0.90, 0.10, 0.75, 0.25, 0.60, 0.40), dim = c(2, 3),
            dimnames = list(H = H.lv, A = A.lv))
H.prob

D.coef <- list(coef = array(c(0, 0, 0, 1, 4, 7), dim = c(1, 6),
                        dimnames = list("(Intercept)", NULL)),
               sd = c(0, 0, 0, 0.5, 1, 1.5))
D.coef


## ----healthcare-I-------------------------------------------------------------
I.coef <- list(coef = array(c(1, 3, 1, 5.5, 1, 8) * 100, dim = c(2, 3),
                        dimnames = list(c("(Intercept)", "D"), NULL)),
               sd = c(30, 50, 100))
I.coef


## ----healthcare-O-------------------------------------------------------------
O.coef <- list(coef = array(c(60, 180, 360) , dim = c(1, 3),
                        dimnames = list("(Intercept)", NULL)),
               sd = c(10, 20, 40))


## ----healthcare-T-------------------------------------------------------------
T.coef <- list(coef = c("(Intercept)" = 120, I = 1.02, O = 1.05),
               sd = 10)


## ----healthcare-build-bn------------------------------------------------------
dists <- list(A = A.prob, C = C.prob, H = H.prob, D = D.coef,
              I = I.coef, O = O.coef, T = T.coef)
healthcare <- custom.fit(dag, dists)


## ----healthcare-show-I--------------------------------------------------------
healthcare$I


## ----healthcare-save-data, echo = FALSE---------------------------------------
set.seed(42)
write.table(rbn(healthcare, 2000), file = "healthcare.txt",
  row.names = FALSE)


## ----healthcare-read-data-----------------------------------------------------
costs <- read.table("healthcare.txt", header = TRUE,
           colClasses = c("factor", "factor", "numeric", "factor",
                          "numeric", "numeric", "numeric"))


## ----healthcare-hidden-reset-levels, echo = FALSE-----------------------------
costs$A <- factor(costs$A, levels = A.lv)
costs$C <- factor(costs$C, levels = C.lv)


## ----healthcare-learn-parameters----------------------------------------------
fitted <- bn.fit(dag, data = costs)


## ----healthcare-discrete-node-params------------------------------------------
cpt.H <- prop.table(table(costs[, c("H", "A")]), margin = 2)
all.equal(cpt.H, coef(fitted$H))


## ----healthcare-continuous-node-params----------------------------------------
params.T <- lm(T ~ I + O, data = costs)
all.equal(coef(fitted$T), coef(params.T))
all.equal(sigma(fitted$T), sigma(params.T))


## ----healthcare-mixture-node-coefs--------------------------------------------
models.I <- list(lm(I ~ D, data = costs[costs$C == "none", ]),
                 lm(I ~ D, data = costs[costs$C == "mild", ]),
                 lm(I ~ D, data = costs[costs$C == "severe", ]))
matrix(c(coef(models.I[[1]]), coef(models.I[[2]]), coef(models.I[[3]])),
       nrow = 2, ncol = 3, dimnames = list(c("(Intercept)", "D"),
                                           c("none", "mild", "severe")))

## ----healthcare-mixture-node-sigmas-------------------------------------------
c(none = sigma(models.I[[1]]), mild = sigma(models.I[[2]]),
  severe = sigma(models.I[[3]]))


## ----healthcare-overall-lm----------------------------------------------------
single.model <- lm(I ~ D * C, data = costs)
coef(single.model)
sigma(single.model)


## ----healthcare-coefficients-comparison---------------------------------------
coef(single.model)["(Intercept)"] + coef(single.model)["Cmild"]
coef(single.model)["D"] + coef(single.model)["D:Cmild"]


## ----healthcare-linear-mixed-model--------------------------------------------
library(lme4)
mixmod <- lmer(I ~ D + (1|C) + D:C, data = costs)


## ----healthcare-linear-mixed-model-2------------------------------------------
as.data.frame(ranef(mixmod))


## ----healthcare-linear-mixed-model-3------------------------------------------
fixef(mixmod)


## ----healthcare-scoring-------------------------------------------------------
learned <- hc(costs, score = "bic-cg")
modelstring(learned)
modelstring(dag)


## ----healthcare-scoring-singular----------------------------------------------
score(dag, costs, type = "bic-cg", by.node = TRUE)


## ----healthcare-check-inpatient-per-day---------------------------------------
part <- cpdist(healthcare, nodes = c("I", "D"),
               evidence = (D >= 1) & (I >= 100), n = 10^5)
per.day <- (part$I - 100) / part$D
c(mean = mean(per.day), quantile(per.day, c(0.01, 0.99, 0.999)))


## ----healthcare-check-outpatient-per-day--------------------------------------
part <- cpdist(healthcare, nodes = "O", evidence = (O >= 0), n = 10^5)
summary(part$O)


## ----healthcare-check-days-conditions-----------------------------------------
part <- cpdist(healthcare, nodes = "D", evidence = (H == "any"),
          n = 10^5)
c(mean = mean(part$D), quantile(part$D, c(0.01, 0.99)))
cpquery(healthcare, event = (C %in% c("mild", "severe")),
        evidence = (A == "young"), n = 10^5)
cpquery(healthcare, event = (C %in% c("mild", "severe")),
        evidence = (A == "old"), n = 10^5)


## ----healthcare-check-premiums------------------------------------------------
part <- cpdist(healthcare, nodes = c("I", "O", "T"),
          evidence = (I >= 0) & (O >= 0), n = 10^5)
summary(part$T)


## ----healthcare-excess-tax----------------------------------------------------
finances <- c(mean.tax = mean(part$T),
              mean.expenditure = mean(part$I + part$O),
              surplus = mean(part$T) - mean(part$I + part$O))
finances


## ----healthcare-young-and-old-people------------------------------------------
cpquery(healthcare, event = (T <= finances["mean.tax"]),
        evidence = (A == "young"))
cpquery(healthcare, event = (T > finances["mean.tax"]),
        evidence = (A == "old"))


## ----healthcare-older-population----------------------------------------------
new.A.prob <- array(c(0.30, 0.40, 0.30), dim = 3,
                    dimnames = list(A = A.lv))
new.A.prob
healthcare$A <- new.A.prob


## ----healthcare-young-and-old-people-2----------------------------------------
part <- cpdist(healthcare, nodes = c("I", "O"),
          evidence = (I >= 0) & (O >= 0), n = 10^5)
finances["mean.tax"] - mean(part$I + part$O)


## ----healthcare-more-sick-people----------------------------------------------
new.C.prob <- array(c(0.88, 0.10, 0.02, 0.70, 0.22, 0.08, 0.41,
                      0.51, 0.08), dim = c(3, 3),
                dimnames = list(C = C.lv, A = A.lv))
new.C.prob - C.prob
healthcare$C <- new.C.prob
part <- cpdist(healthcare, nodes = c("I", "O"),
          evidence = (I >= 0) & (O >= 0), n = 10^5)
finances["mean.tax"] - mean(part$I + part$O)


## ----healthcare-fewer-days----------------------------------------------------
new.D.coef <- list(coef = array(c(0, 0, 0, 1, 4, 6), dim = c(1, 6),
                            dimnames = list("(Intercept)", NULL)),
                   sd = c(0, 0, 0, 0.5, 1, 1.5))
healthcare$D <- new.D.coef
part <- cpdist(healthcare, nodes = c("I", "O"),
          evidence = (I >= 0) & (O >= 0), n = 10^5)
finances["mean.tax"] - mean(part$I + part$O)

