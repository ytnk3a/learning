## ----chapter6-init-stanza, include = FALSE, cache = FALSE, echo = FALSE-------
library(deal)
library(catnet)
library(pcalg)
library(Rgraphviz)
library(bnlearn)
library(abn)
library(rstan)


## ----recreate-latent-variables-example----------------------------------------
library(bnlearn)
data(marks)
latent <- factor(c(rep("A", 44), "B", rep("A", 7), rep("B", 36)))
latent.marks <- data.frame(marks, LAT = latent)


## ----load-deal----------------------------------------------------------------
library(deal)
net <- network(latent.marks)


## ----load-deal-print----------------------------------------------------------
net


## ----deal-set-prior-----------------------------------------------------------
prior <- jointprior(net, N = 5)


## ----deal-learn-from-data, results = "hide", fig.keep = "none"----------------
net <- learn(net, latent.marks, prior)$nw
best <- autosearch(net, latent.marks, prior)


## ----deal-modelstring---------------------------------------------------------
mstring <- deal::modelstring(best$nw)
bnlearn:::fcat(mstring)


## ----recreate-from-previous-chapter, echo = FALSE-----------------------------
dag.bnlearn <-
  model2network("[ANL][MECH][LAT|ANL:MECH][VECT|LAT][ALG|LAT][STAT|LAT]")

## ----compare-deal-bnlearn-----------------------------------------------------
dag.deal <- model2network(mstring)
unlist(bnlearn::compare(cpdag(dag.deal), cpdag(dag.bnlearn)))


## ----compare-deal-bnlearn-structures, echo = FALSE, results = "hide"----------
local({
pdf("figures/deal.pdf", width = 7, height = 4)
par(mfrow = c(1, 2))
graphviz.plot(dag.deal, main = "dag.deal")
gR <- graphviz.plot(dag.bnlearn, main = "dag.bnlearn", render = FALSE)
nodeRenderInfo(gR)$fontsize = 12
renderGraph(gR)
dev.off()
})


## ----catnet-structure-learning------------------------------------------------
library(catnet)
dmarks <- discretize(latent.marks, breaks = 2, method = "interval")
ord <- cnSearchSA(dmarks, maxParentSet = 2)
ord


## ----catnet-pick-best-model---------------------------------------------------
nets <- ord@nets
nets[[1]]


## ----catnet-best-bic----------------------------------------------------------
best <- cnFindBIC(ord, nrow(dmarks))
best


## ----catnet-sampling----------------------------------------------------------
cnSamples(best, numsamples = 4)


## ----catnet-import-in-bnlearn-------------------------------------------------
em <- empty.graph(names(dmarks))
arcs(em) <- cnMatEdges(best)


## ----load-pcalg-sufficient-statistics-----------------------------------------
library(pcalg)
suffStat <- list(C = cor(marks), n = nrow(marks))


## ----pcalg-run-pc-------------------------------------------------------------
pc.fit <- pc(suffStat, indepTest = gaussCItest,
            labels = colnames(marks), alpha = 0.05)


## ----pcalg-show-network-------------------------------------------------------
pc.fit@graph


## ----pcalg-fci----------------------------------------------------------------
fci.fit <- fci(suffStat, indepTest = gaussCItest,
             labels = colnames(marks), alpha = 0.05)


## ----pc-vs-fci-structure-comparison, echo = FALSE, results = "hide"-----------
local({
pdf("figures/pcalg.pdf", width = 5, height = 3)
par(mfrow = c(1, 2))
pc.dag <-
  model2network("[VECT|MECH][MECH][ALG|MECH:VECT][STAT|ALG:ANL][ANL|ALG]")
graphviz.plot(pc.dag)
plot(fci.fit)
dev.off()
})


## ----reconstruct-abn-dag, echo = FALSE, results = "hide"----------------------
local({
pdf("figures/abn.pdf", width = 4, height = 2.5)
dists <- list(MECH = "poisson", VECT = "poisson",
           ALG = "poisson", ANL = "poisson", STAT = "poisson",
           LAT = "binomial")
amat <-
  structure(c(0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
              0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
              0L, 1L, 1L, 1L, 0L, 0L), .Dim = c(6L, 6L),
              .Dimnames = list(c("MECH", "VECT", "ALG", "ANL", "STAT", "LAT"),
                               c("MECH", "VECT", "ALG", "ANL", "STAT", "LAT")))
plotabn(amat, data.dists = dists, node.fillcolor = "transparent")
})


## ----load-abn-and-prepare-data------------------------------------------------
library(abn)
dists <- list(MECH = "poisson", VECT = "poisson", ALG = "poisson",
              ANL = "poisson", STAT = "poisson", LAT = "binomial")


## ----abn-build-cache----------------------------------------------------------
cache <- buildscorecache(latent.marks, data.dists = dists,
           dag.banned = ~ LAT | .)


## ----abn-structure-learning---------------------------------------------------
exact.net <- mostprobable(cache, verbose = FALSE)
hc.net <- searchHillclimber(cache, num.searches = 20)


## ----abn-parameter-learning---------------------------------------------------
fitted <- fitabn(exact.net)


## ----rstan-showcase-compilation-----------------------------------------------
library(rstan)
model <- 'data {
  int <lower=0> N;
  int <lower=0,upper=1> y[N];
}
parameters {
  real <lower=0,upper=1> theta;
}
model {
  theta ~ beta(1,1);
  y ~ bernoulli(theta);
}'
cc <- stanc(model_code = model, model_name = "beta-binomial")
cppcode <- unlist(strsplit(cc$cppcode, "\\n"))
cat(head(cppcode, n = 15), "...", sep = "\n")

