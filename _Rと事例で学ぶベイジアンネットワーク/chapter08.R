## ----chapter7-init-stanza, include = FALSE, cache = FALSE, echo = FALSE-------
library(lattice)
lattice.options(default.theme = canonical.theme(color = FALSE))
library(gridExtra)
library(gRain)
library(rbmn)
library(graph)
library(igraph)
library(Rgraphviz)
library(bnlearn)


## ----sachs-dag-from-the-paper, echo = FALSE, results = "hide"-----------------
sachs.dag.from.paper <-
  paste0("[PKC|Plcg:PIP2][PKA|PKC][Raf|PKC:PKA][Mek|PKC:PKA:Raf][Erk|Mek:PKA]",
         "[Akt|Erk:PKA:PIP3][P38|PKC:PKA][Jnk|PKC:PKA][Plcg][PIP3|Plcg]",
         "[PIP2|Plcg:PIP3]")
sachs.dag.from.paper <- model2network(sachs.dag.from.paper)

missed.arcs.from.paper <-
  matrix(c("Plcg", "PKC",
           "PIP2", "PKC",
           "PIP3", "Akt"
         ), ncol = 2, byrow = TRUE)

local({
pdf(file = "figures/sachs-spec.pdf", width = 5, height = 3.5, paper = "special")
gR <- graphviz.plot(sachs.dag.from.paper, shape = "ellipse", render = FALSE,
        highlight = list(arcs = missed.arcs.from.paper, lty = 2, col = "black"))
nodeRenderInfo(gR)$fontsize <- 12
renderGraph(gR)
dev.off()
})


## ----load-bnlearn-and-sachs---------------------------------------------------
library(bnlearn)
sachs <- read.table("sachs.data.txt", header = TRUE)
head(sachs)


## ----learn-with-iamb----------------------------------------------------------
dag.iamb <- inter.iamb(sachs, test = "cor")
narcs(dag.iamb)
directed.arcs(dag.iamb)


## ----sachs-true-model---------------------------------------------------------
sachs.modelstring <-
  paste0("[PKC][PKA|PKC][Raf|PKC:PKA][Mek|PKC:PKA:Raf][Erk|Mek:PKA]",
         "[Akt|Erk:PKA][P38|PKC:PKA][Jnk|PKC:PKA][Plcg][PIP3|Plcg]",
         "[PIP2|Plcg:PIP3]")
dag.sachs <- model2network(sachs.modelstring)
unlist(compare(dag.sachs, dag.iamb))


## ----sachs-compare-iamb-golden------------------------------------------------
unlist(compare(skeleton(dag.sachs), skeleton(dag.iamb)))


## ----sachs-continuous-diagnostic-plots, echo = FALSE, results = "hide"--------
local({

  pdf(file = "figures/sachs-dist1.pdf", width = 6, height = 5, paper = "special")
print(histogram(~ PIP2 + PIP3 + Mek + P38, data = sachs, type = "density",
  breaks = NULL, xlab = "expression levels", ylab = "density",
  scales = list(x = list(relation = "free"),
                y = list(relation = "free", at = NULL)),
  panel = function(x, ...) {

    panel.abline(h = 0)
    panel.histogram(x, ...)
    panel.mathdensity(dmath = dnorm, col = "black",
      args = list(mean = mean(x), sd = sd(x)))

  }
))
dev.off()

pdf(file = "figures/sachs-dist2.pdf", width = 6, height = 4, paper = "special")
print(xyplot(PKA ~ PKC, data = sachs,
  scales = list(alternating = c(1), x = list(relation = "free"),
                y = list(relation = "free")),
  panel = function(x, y, ...) {

    panel.xyplot(y = y, x = x, col = "darkgray", ...)
    panel.lmline(y = y, x = x, lty = 2)

}))
dev.off()
})


## ----sachs-discretize, eval = FALSE-------------------------------------------
## dsachs <- discretize(sachs, method = "hartemink", breaks = 3,
##             ibreaks = 60, idisc = "quantile")

## ----hidden-sachs-load, echo = FALSE------------------------------------------
dsachs <- read.table("sachs.discretised.txt", header = TRUE,
            colClasses = "factor")
set.seed(123)


## ----sachs-boot-strength------------------------------------------------------
boot <- boot.strength(dsachs, R = 500, algorithm = "hc",
          algorithm.args = list(score = "bde", iss = 10))


## ----sachs-threshold-strength-boot--------------------------------------------
boot[(boot$strength > 0.85) & (boot$direction >= 0.5), ]


## ----sachs-averaged-boot------------------------------------------------------
avg.boot <- averaged.network(boot, threshold = 0.85)


## ----sachs-averaged-skeleton--------------------------------------------------
avg.boot <- skeleton(avg.boot)


## ----plot-sachs-averaged-skeleton, echo = FALSE, results = "hide"-------------
local({
pdf(file = "figures/sachs-avg.pdf", width = 3.5, height = 2.8, paper = "special")
true.dag <- set.arc(sachs.dag.from.paper, "Jnk", "P38")
true.arcs <- arcs(true.dag)
ref <- graphviz.plot(true.dag, highlight = list(arcs = true.arcs, col = "grey"),
                    shape = "ellipse", render = FALSE)
nodeRenderInfo(ref)$fontsize <- 12
edgeRenderInfo(ref) <-
  list(lty = c("Plcg~PKC" = 2, "PIP2~PKC" = 2, "PIP3~Akt" = 2))
edgeRenderInfo(ref) <-
  list(col = c("Raf~Mek" = "black", "Plcg~PIP2" = "black",
               "Plcg~PIP3" = "black", "PIP3~PIP2" = "black",
               "Erk~Akt" = "black", "PKA~Erk" = "black", "PKA~Akt" = "black",
               "PKC~P38" = "black", "PKC~Jnk" = "black", "Jnk~P38" = "black"),
       arrowhead = c("PIP3~PIP2" = "none", "PKA~Erk" = "none"),
       arrowtail = c("PIP3~PIP2" = "open", "PKA~Erk" = "open"))
edgeRenderInfo(ref) <-
  list(arrowhead = c("Raf~Mek" = "none", "Plcg~PIP2" = "none",
                     "Plcg~PIP3" = "none", "PIP3~PIP2" = "none",
                     "Erk~Akt" = "none", "PKA~Erk" = "none", "PKA~Akt" = "none",
                     "PKC~P38" = "none", "PKC~Jnk" = "none", "Jnk~P38" = "none"),
       arrowtail = c("Raf~Mek" = "none", "Plcg~PIP2" = "none",
                     "Plcg~PIP3" = "none", "PIP3~PIP2" = "none",
                     "Erk~Akt" = "none", "PKA~Erk" = "none", "PKA~Akt" = "none",
                     "PKC~P38" = "none", "PKC~Jnk" = "none", "Jnk~P38" = "none"))
renderGraph(ref)
dev.off()
})


## ----sachs-random-start-------------------------------------------------------
nodes <- names(dsachs)
start <- random.graph(nodes, method = "ic-dag", num = 500, every = 50)
netlist <- lapply(start, function(net) {
  hc(dsachs, score = "bde", iss = 10, start = net)
})


## ----sachs-averaged-random-start-1--------------------------------------------
rnd <- custom.strength(netlist, nodes = nodes)


## ----sachs-averaged-random-start-2--------------------------------------------
rnd[(rnd$strength > 0.85) & (rnd$direction >= 0.5), ]
avg.start <- averaged.network(rnd, threshold = 0.85)


## ----sachs-compare-random-start-----------------------------------------------
all.equal(cpdag(avg.boot), cpdag(avg.start))


## ----sachs-averaged-extend----------------------------------------------------
score(cextend(cpdag(avg.start)), dsachs, type = "bde", iss = 10)


## ----sachs-averaged-catnet----------------------------------------------------
library(catnet)
netlist <- vector(500, mode = "list")
ndata <- nrow(dsachs)
nodes <- names(dsachs)
netlist <- lapply(netlist, function(net) {
  boot <- dsachs[sample(ndata, replace = TRUE), ]
  top.ord <- cnSearchOrder(boot)
  best <- cnFindBIC(top.ord, ndata)
  cnMatEdges(best)
})
sann <- custom.strength(netlist, nodes = nodes)


## ----sachs-catnet-threshold---------------------------------------------------
sann[(sann$strength > 0.85) & (sann$direction >= 0.5), ]
avg.catnet <- averaged.network(sann, threshold = 0.85)


## ----sachs-compare-narcs------------------------------------------------------
narcs(avg.catnet)
narcs(avg.start)


## ----sachs-compare-scores-----------------------------------------------------
score(cextend(cpdag(avg.catnet)), dsachs, type = "bde", iss = 10)
score(cextend(cpdag(avg.start)), dsachs, type = "bde", iss = 10)


## ----sachs-compare-thresholds-------------------------------------------------
all.equal(averaged.network(boot, threshold = 0.50),
          averaged.network(boot, threshold = 0.70))


## ----sachs-default-threshold--------------------------------------------------


## ----plot-threshold-ecdf, echo = FALSE, results = "hide"----------------------
local({
pdf("figures/threshold.pdf", width = 6, height = 5, paper = "special")
plot(ecdf(boot$strength), xlab = "arc strengths",
  ylab = "cumulative distribution function", main = "",
  xlim = c(0, 1.2), xaxt = 'n')
abline(v = attr(boot, "threshold"), lty = 2)
abline(v = 0.85, col = "grey", lty = 2)
text(x = attr(boot, "threshold") , y = 0.07,
     labels = "significant\narcs", pos = 4)
arrows(attr(boot, "threshold"), 0.02, 1.1, 0.02, length = 0.1)
text(x = attr(boot, "threshold"), y = 0.6, srt = 90,
     labels = "estimated threshold", pos = 2)
text(x = 0.85, y = 0.6, srt = 90, labels = "Sachs' threshold", pos = 2)
axis(1, at = 0.2 * 0:5)
dev.off()
})


## ----compare-different-averaged-dags------------------------------------------
unlist(compare(cpdag(dag.sachs), cpdag(averaged.network(boot))))
all.equal(cpdag(avg.boot), cpdag(averaged.network(boot)))


## ----interventional-sachs-----------------------------------------------------
isachs <- read.table("sachs.interventional.txt", header = TRUE,
            colClasses = "factor")


## ----interventional-whitelist-------------------------------------------------
wh <- matrix(c(rep("INT", 11), names(isachs)[1:11]), ncol = 2)
dag.wh <- tabu(isachs, whitelist = wh, score = "bde",
            iss = 10, tabu = 50)


## ----interventional-blacklist-------------------------------------------------
tiers <- list("INT", names(isachs)[1:11])
bl <- tiers2blacklist(tiers)
dag.tiers <- tabu(isachs, blacklist = bl, score = "bde",
               iss = 1, tabu = 50)


## ----interventional-label-----------------------------------------------------
INT <- sapply(1:11, function(x) which(isachs$INT == x))
nodes <- names(isachs)[1:11]
names(INT) <- nodes


## ----model-averaging-replicated-from-sachs------------------------------------
start <- random.graph(nodes = nodes, method = "melancon", num = 500,
           burn.in = 10^5, every = 100)
netlist <- lapply(start, function(net) {
  tabu(isachs[, 1:11], score = "mbde", exp = INT, iss = 1,
    start = net, tabu = 50)
})
intscore <- custom.strength(netlist, nodes = nodes, cpdag = FALSE)


## ----finally-we-get-close-----------------------------------------------------
dag.mbde <- averaged.network(intscore)
unlist(compare(dag.sachs, dag.mbde))


## ----sachs-various-network-comparison, echo = FALSE, results = "hide"---------
local({
gR <- graphviz.plot(sachs.dag.from.paper, main = "Sachs et al. (2005)",
        sub = "dag.sachs", highlight = list(arcs = missed.arcs.from.paper,
        lty = 2, col = "black"), shape = "ellipse", render = FALSE)
edgeRenderInfo(gR) <-
  list(lwd = c("Plcg~PIP3" = 3, "Plcg~PIP2" = 3, "PIP3~PIP2" = 3,
               "PKC~Raf" = 3, "PKC~Mek" = 3, "PKC~PKA" = 3, "PKC~Jnk" = 3,
               "PKC~P38" = 3, "PKA~Raf" = 3, "PKA~Mek" = 3, "PKA~Erk" = 3,
               "PKA~Akt" = 3, "PKA~Jnk" = 3, "PKA~P38" = 3, "Raf~Mek" = 3,
               "Mek~Erk" = 3, "Erk~Akt" = 3))

pdf("figures/interventional-arcs.pdf", width = 8, height = 10, paper = "special")
par(mfrow = c(2, 2))
highlight <- list(arcs = arcs(skeleton(sachs.dag.from.paper)), lwd = 3,
                  col = "black")
graphviz.plot(dag.wh, highlight = highlight,
  main = "Whitelisted Interventions", sub = "dag.wh", shape = "ellipse")
graphviz.plot(dag.tiers, highlight = highlight, main = "Learned Interventions",
  sub = "dag.tiers", shape = "ellipse")
graphviz.plot(dag.mbde, highlight = highlight, main = "Modified BDe",
  sub = "dag.mbde", shape = "ellipse")
renderGraph(gR, graph.pars = list(graph = list(main = "Sachs et al. (2005)",
                                               sub = "dag.sachs")))
dev.off()
})


## ----sachs-interventional-fit-------------------------------------------------
isachs <- isachs[, 1:11]
for (i in names(isachs))
  levels(isachs[, i]) = c("LOW", "AVG", "HIGH")
fitted <- bn.fit(dag.sachs, isachs, method = "bayes")


## ----sachs-load-gRain---------------------------------------------------------
library(gRain)
jtree <- compile(as.grain(fitted))


## ----sachs-set-evidence-------------------------------------------------------
jlow <- setEvidence(jtree, nodes = "Erk", states  = "LOW")


## ----sachs-exact-inference----------------------------------------------------
querygrain(jtree, nodes = "Akt")$Akt
querygrain(jlow, nodes = "Akt")$Akt


## ----sachs-change-network-----------------------------------------------------
causal.sachs <- drop.arc(dag.sachs, "PKA", "Erk")
causal.sachs <- drop.arc(causal.sachs, "Mek", "Erk")
cfitted <- bn.fit(causal.sachs, isachs, method = "bayes")
cjtree <- compile(as.grain(cfitted))
cjlow <- setEvidence(cjtree, nodes = "Erk", states  = "LOW")


## ----sachs-new-query----------------------------------------------------------
querygrain(cjtree, nodes = "PKA")$PKA
querygrain(cjlow, nodes = "PKA")$PKA


## ----sachs-new-query-v2-------------------------------------------------------
querygrain(jtree, nodes = "PKA")$PKA
querygrain(jlow, nodes = "PKA")$PKA


## ----sachs-map----------------------------------------------------------------
names(which.max(querygrain(jlow, nodes = c("PKA"))$PKA))


## ----compare-gRain-queries-by-barplots, echo = FALSE, results = "hide"--------
local({
a <- querygrain(jtree, nodes = "Akt")$Akt
b <- querygrain(jlow, nodes = "Akt")$Akt
d <- data.frame(A = a, B = b, C = factor(c("LOW", "AVG", "HIGH"),
       levels = c("LOW", "AVG", "HIGH")))

lattice.options(default.theme = canonical.theme(color = FALSE))
p1 <- barchart(C ~ A + B, data = d, horizontal = TRUE,
  ylab = "Akt", xlab = "probability", main = expression(P(Akt)),
  auto.key = list(corner = c(0.9, 0.9),
                  text = c("without intervention", "with intervention")))

a <- querygrain(jtree, nodes = "PKA")$PKA
b <- querygrain(jlow, nodes = "PKA")$PKA

d <- data.frame(A = a, B = b, C = factor(c("LOW", "AVG", "HIGH"),
       levels = c("LOW", "AVG", "HIGH")))

p2 <- barchart(C ~ A + B, data = d, horizontal = TRUE,
  ylab = "PKA", xlab = "probability", main = expression(P(PKA)),
  auto.key = list(corner = c(0.9, 0.9),
                  text = c("without intervention", "with intervention")))

pdf("figures/cpquery.pdf", width = 8, height = 4, paper = "special")
grid.arrange(p1, p2, ncol = 2)
dev.off()
})


## ----bc01---------------------------------------------------------------------
library(rbmn)
data(boco)
round(head(boco), 1)
boco$B <- boco$W / boco$H^2 * 10^4
dim(boco)
n <- nrow(boco)
vr <- c("TF", "LF", "AF", "TL", "LL", "AL", "TB", "LB", "AB")
co <- c("A", "H", "W", "C", "B")


## ----bc02---------------------------------------------------------------------
set.seed(42)
sub <- split(sample(n), c("train", "validation"))
dtrain <- boco[sub$train, ]
dval <- boco[sub$validation, ]


## ----bc02b--------------------------------------------------------------------
saturated <- lm(cbind(TF, LF, AF, TL, LL, AL, TB, LB, AB) ~
                  A + H + W + C + B, data = dtrain)
resid.df <- anova(saturated)["Residuals", "Df"]
preds <- predict(saturated, newdata = dval)
bias <- abs(dval[, vr] - preds)
stdev <- outer(rep(1, nrow(dtrain)),
          sqrt(colSums(residuals(saturated)^2)/resid.df), "*")
sep <- sqrt(bias^2 + stdev^2)
summary <- cbind("|Bias|" = colMeans(bias),
                 "Sd.Dev" = colMeans(stdev),
                 "SEP" = colMeans(sep))


## -----------------------------------------------------------------------------
round(summary, 2)
round(colSums(summary), 2)


## ----bc03---------------------------------------------------------------------
library(bnlearn)
dag <- hc(dtrain)
bnlearn:::fcat(modelstring(dag))


## ----bc04---------------------------------------------------------------------
wl1 <- cbind(from = rep(co, each = 9), to = rep(vr, 5))
dag.wl <- hc(dtrain, whitelist = wl1)
bnlearn:::fcat(modelstring(dag.wl))


## ----bc05---------------------------------------------------------------------
bl1 <- wl1[, 2:1]
dag.bl <- hc(dtrain, blacklist = bl1)
bnlearn:::fcat(modelstring(dag.bl))
all.equal(dag.wl, dag.bl)


## ----bc06---------------------------------------------------------------------
iwl <- 1:15
wl2 <- wl1[iwl, ]
bl2 <- bl1[-iwl, ]
dag.wlbl <- hc(dtrain, whitelist = wl2, blacklist = bl2)
bnlearn:::fcat(modelstring(dag.wlbl))


## ----bc07---------------------------------------------------------------------
bn2 <- bn.fit(dag.wl, data = dtrain)


## ----bc08---------------------------------------------------------------------
library(rbmn)
mvnorm.dist <- gema2mn(nbn2gema(bnfit2nbn(bn2)))
bias <- stdev <- dval[, vr]
for (i in seq(nrow(dval))) {
  mvnorm.cond <- condi4joint(mvnorm.dist, par = vr, pour = co,
                       unlist(dval[i, co]))
  bias[i, vr] <- dval[i, vr] - mvnorm.cond$mu[vr]
  stdev[i, vr] <- sqrt(diag(mvnorm.cond$gamma)[vr])
}#FOR
sep <- sqrt(bias^2 + stdev^2)


## ----bc09---------------------------------------------------------------------
gscores <- cbind("|Bias|" = colMeans(abs(bias)),
                 "Sd.Dev" = colMeans(stdev),
                 "SEP" = colMeans(sep))
round(gscores, 2)
round(colSums(gscores), 2)


## ----bc10, fig.keep = "none"--------------------------------------------------
library(igraph)
load("bc.poco.rda")
cbind(position, colour)[c(10:11, 1:3), ]
idag2 <- as.igraph(dag.wl)
nad <- V(idag2)$label <- V(idag2)$name
edge.col <- rep("lightgrey", narcs(dag.wl))
aa <- which((arcs(dag.wl)[, "from"] %in% vr) &
            (arcs(dag.wl)[, "to"] %in% vr))
va <- as.numeric(E(idag2, P = t(arcs(dag.wl)[aa, ])))
edge.col[va] <- "black"
plot(idag2, layout = position[nad, ], main = "DAG 2",
       edge.color = edge.col, vertex.color = colour[nad])


## ----generate-figure-in-bc10, echo = FALSE, results = "hide"------------------
local({
pdf("figures/idag2.pdf", width = 7, height = 4.5)
par(mar = c(0, 0, 0.9, 0))
plot(idag2, layout = position[nad, ], main = "DAG 2", edge.color = edge.col,
     vertex.color = colour[nad], vertex.label.color = "black", vertex.size = 15)
dev.off()
})


## ----comparing-results-mmhc, echo = FALSE, results = "hide"-------------------
local({
pdf("figures/mmhc-rsmax2.pdf", width = 7, height = 7)
par(mfrow = c(2, 2), mar = c(1, 0, 1, 0))
for (panel in 1:4) {
  if (panel == 1) {
    title <- "mmhc with wl1";
    dagx <- mmhc(dtrain, whitelist = wl1)
  }#THEN
  else if (panel == 2) {
    title <- "mmhc with bl1";
    dagx <- mmhc(dtrain, blacklist = bl1)
  }#THEN
  else if (panel == 3) {
    title <- "rsmax2 with wl1";
    dagx <- rsmax2(dtrain, whitelist = wl1)
  }#THEN
  else if (panel == 4) {
    title <- "rsmax2 with bl1";
    dagx <- rsmax2(dtrain, blacklist = bl1)
  }#THEN
  idagx <- as.igraph(dagx)
  nad <- V(idagx)$label <- V(idagx)$name
  aa <- which((arcs(dagx)[, "from"] %in% vr) & (arcs(dagx)[, "to"] %in% vr))
  edge.col <- rep("lightgrey", narcs(dagx))
  va <- as.numeric(E(idagx, P = t(arcs(dagx)[aa, , drop = FALSE])))
  edge.col[va] <- "black"
  plot(idagx, layout = position[nad, ], main = title, edge.color = edge.col,
    vertex.color = colour[nad], vertex.label.color = "black", vertex.size = 25)
}
dev.off()

})


## ----comparing-whitelist-and-blacklist, echo = FALSE, results = "hide"--------
local({
pdf("figures/var-struct.pdf", width = 8, height = 4)
par(mfrow = c(1,2), mai = c(0.2,0.2,0.2,0.2), cex.lab = 0.5)

iwl <- matrix(NA, 0, 2)
for (i1 in c("B", "L", "F")) {
  it <- paste("T", i1, sep = "")
  il <- paste("L", i1, sep = "")
  ia <- paste("A", i1, sep = "")
  iwl <- rbind(iwl, c(it, il), c(it, ia))
}
for (i1 in c("A", "T", "L")) {
  it <- paste(i1, "L", sep = "")
  il <- paste(i1, "B", sep = "")
  ia <- paste(i1, "F", sep = "")
  iwl <- rbind(iwl, c(it, il), c(it, ia))
}

ibl <- cbind(rep(vr, 9), rep(vr, each = 9))

for (ii in 1:nrow(iwl)) {
  i1 <- iwl[ii, 1]
  i2 <- iwl[ii, 2]
  k1 <- which((i1 == ibl[ ,1]) & (i2 == ibl[, 2]))
  k2 <- which((i1 == ibl[ ,2]) & (i2 == ibl[, 1]))
  ibl <- ibl[-c(k1, k2), ]
}

kk <- which(ibl[, 1] == ibl[, 2])
ibl <- ibl[-kk, ]

dbl <- dwl <- empty.graph(vr)
arcs(dbl, check.cycles = FALSE) <- ibl
arcs(dwl, check.cycles = FALSE) <- iwl
idbl <- igraph.from.graphNEL(as.graphNEL(dbl))
idwl <- igraph.from.graphNEL(as.graphNEL(dwl))

naa <- V(idbl)$label <- V(idwl)$label <- V(idbl)$name
plot(idwl, layout = position[naa, ], main = "Whitelist (iwl)",
         edge.color = "black", vertex.color = colour[naa],
         vertex.label.color = "black", vertex.size = 25)
plot(idbl, layout = position[naa, ], main = "Blacklist (ibl)",
         edge.color = "lightgrey", vertex.color = colour[naa],
         vertex.label.color = "black", vertex.size = 25)

dev.off()
})


## ----plot-results-table, echo = FALSE, results = "hide"-----------------------
local({
SEP <- c(10.1214, 11.1953, 10.1560, 10.0141, 15.0801, 10.0235, 12.1193, 15.1440,
         12.3737, 12.6644, 12.3737, 16.6739, 12.2340, 14.8841, 12.2340, 16.6739,
         12.2340, 15.1179, 12.2340, 19.0128, 19.5920, 14.7080, 19.5920, 19.0128,
         19.5920, 15.2085, 19.5920, 18.7803, 13.8794, 16.4846, 13.8794, 10.0109)
c2v <- c(45, 12, 23, 45, 3, 45, 3, 3, 2, 4, 2, 3, 3, 5, 3, 3, 3, 4, 3, 2, 1, 5,
         1, 2, 1, 3, 1, 2, 2, 4, 2, 45)
v2v <- c(20, 17, 15, 3, 6, 4, 6, 3, 15, 4, 15, 3, 15, 3, 15, 3, 15, 3, 15, 4,
         15, 4, 15, 4, 15, 4, 15, 2, 15, 3, 15, 36)

pdf("figures/model-choice.pdf", height = 5, width = 6)
xxx <- c2v + v2v
yyy <- SEP
plot(xxx, yyy, ylab = "SEP", xlab = "number of arcs")
xlim <- 30
ylim <- 12.6
selected <- which((xxx < xlim) & (yyy < ylim))
points(xxx[selected], yyy[selected], pch = 2, cex = 2)
abline(v = xlim, lty = 3, lwd = 1.5)
abline(h = ylim, lty = 3, lwd = 1.5)
dev.off()
})


## ----plot-retained-dag, echo = FALSE, results = "hide"------------------------
local({
mf <- "[W][H][C][TL|W:H][TF|TL:C][LL|TL][TB|TL][AL|TL:LL][AF|TF:AL][AB|AL:TB][LF|TF:AF:LL][LB|LL:TB:AB]"
dag <- as.igraph(model2network(mf))
edges <- attr(E(dag), "vnames")
edge.col <- rep("lightgrey", length(edges))
edge.col[grep("(AB|TB|LB|AL|TL|LL|AF|TF|LF)\\|(AB|TB|LB|AL|TL|LL|AF|TF|LF)", edges)] <- "black"
pos <- matrix(c(95, 90, 85, 65, 45, 45, 10, 0, 0, 10, 30, 90, 25,
                10, 50, 30, 20, 40, 30, 10, 50, 70, 65, 70), nrow = 12, ncol = 2,
             dimnames = list(c("TF", "LF", "AF", "TL", "LL", "AL", "TB",
                               "LB", "AB", "W", "H", "C"), c("x", "y")))
pos <- pos[names(V(dag)), ]
vertex.col <- c(TF = "white", LF = "white", AF = "white", TL = "white",
                LL = "white", AL = "white", TB = "white", LB = "white",
                AB = "white", W = "lightgrey", H = "lightgrey", C = "lightgrey")
vertex.col <- vertex.col[names(V(dag))]
pdf("figures/retained-dag.pdf", width = 6, height = 3)
par(mar = c(0, 0, 2, 0))
plot(dag, layout = pos, main = "W-H-C / W-List / mmhc", edge.color = edge.col,
     vertex.label.color = "black", vertex.color = vertex.col, vertex.size = 25)
dev.off()
})


## ----bc11---------------------------------------------------------------------
av.tl <- anova(lm(TL ~ H + W, data = dval))
1 - av.tl["Residuals", "Sum Sq"] / sum(av.tl[, "Sum Sq"])

