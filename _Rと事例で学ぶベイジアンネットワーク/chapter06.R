## ----chapter5-init-stanza, include = FALSE, cache = FALSE, echo = FALSE-------
library(Rgraphviz)
library(bnlearn)


## ----string-to-dag------------------------------------------------------------
X <- paste0("[X1][X3][X5][X6|X8][X2|X1][X7|X5][X4|X1:X2][X8|X3:X7]",
            "[X9|X2:X7][X10|X1:X9]")
dag <- model2network(X)


## ----skeleton-and-vstructs-1--------------------------------------------------
skel <- skeleton(dag)

## ----skeleton-and-vstructs-2--------------------------------------------------
vstructs(dag)


## ----show-cpdag---------------------------------------------------------------
cp1 <- cpdag(dag)


## ----draw-dag-cpdag-skeleton-quartet, echo = FALSE, results = "hide"----------
local({
pdf(file = "figures/cpdag.pdf", width = 9, height = 9, paper = "special")
par(mfrow = c(2, 2))
h <- list(arcs = vstructs(dag, arcs = TRUE), lwd = 5, col = "black")
a <- graphviz.plot(dag, highlight = h, render = FALSE)
b <- a
edgeRenderInfo(b) = list(arrowhead = "none")
c <- graphviz.plot(cpdag(dag), highlight = h, render = FALSE)
renderGraph(a, graph.pars = list(graph = list(main = "DAG", cex.main = 2)))
renderGraph(b, graph.pars = list(graph = list(main = "Skeleton", cex.main = 2)))
renderGraph(c, graph.pars = list(graph = list(main = "CPDAG", cex.main = 2)))
edgeRenderInfo(a) <-
  list(arrowhead = c("X5~X7" = "none"), arrowtail = c("X5~X7" = "open"))
edgeRenderInfo(a) <-
  list(arrowhead = c("X2~X4" = "none"), arrowtail = c("X2~X4" = "open"))
renderGraph(a,
  graph.pars = list(graph = list(main = "An Equivalent DAG", cex.main = 2)))
dev.off()
})


## ----create-dag-from-arcs-----------------------------------------------------
dag2 <- dag
dag2 <- set.arc(dag2, "X7", "X5")
dag2 <- set.arc(dag2, "X4", "X2")
dag2 <- set.arc(dag2, "X1", "X2")
dag2 <- set.arc(dag2, "X1", "X4")
cp2 <- cpdag(dag2)
all.equal(cp1, cp2)


## ----check-dseparation--------------------------------------------------------
dsep(dag, x = "X9", y = "X5", z = c("X2", "X7", "X10"))


## ----draw-figure-showing-markov-blankets, echo = FALSE, results = "hide"------
local({
h <- list(nodes = nodes(dag), arcs = arcs(dag), col = "grey", textCol = "grey")
pdf(file = "figures/mblanket.pdf", width = 9, height = 5, paper = "special")
par(mfrow = c(1, 2))
a <- graphviz.plot(dag, highlight = h, render = FALSE)
b <- a
edgeRenderInfo(a) <-
  list(col = c("X1~X10" = "black", "X9~X10" = "black", "X2~X9" = "black",
               "X7~X9" = "black"),
       lwd = c("X1~X10" = 3, "X9~X10" = 3, "X2~X9" = 3, "X7~X9" = 3))
nodeRenderInfo(a) <-
  list(col = c("X2" = "black", "X7" = "black", "X9" = "black", "X10" = "black",
               "X1" = "black"),
       textCol = c("X2" = "black", "X7" = "black", "X9" = "black",
                   "X10" = "black", "X1" = "black"),
       fill = c("X9" = "grey"))
renderGraph(a)

edgeRenderInfo(b) <-
  list(col = c("X5~X7" = "black", "X2~X9" = "black", "X7~X9" = "black",
               "X7~X8" = "black", "X3~X8" = "black"),
       lwd = c("X5~X7" = 3, "X2~X9" = 3, "X7~X9" = 3, "X7~X8" = 3, "X3~X8" = 3))
nodeRenderInfo(b) <-
  list(col = c("X7" = "black", "X8" = "black", "X9" = "black", "X5" = "black",
               "X2" = "black", "X3" = "black"),
       textCol = c("X7" = "black", "X8" = "black", "X9" = "black",
                   "X5" = "black", "X2" = "black", "X3" = "black"),
       fill = c("X7" = "grey"))
renderGraph(b)
dev.off()
})


## ----show-markov-blankets-----------------------------------------------------
mb(dag, node = "X9")
mb(dag, node = "X7")


## ----parents-and-children-----------------------------------------------------
par.X9 <- parents(dag, node = "X9")
ch.X9 <- children(dag, node = "X9")


## ----spouses------------------------------------------------------------------
sp.X9 <- sapply(ch.X9, parents, x = dag)


## ----all-combined-give-the-mb-------------------------------------------------
sp.X9 <- setdiff(sp.X9, "X9")
union(union(par.X9, ch.X9), sp.X9)


## ----markov-blanket-and-dseparation-X9----------------------------------------
V <- setdiff(nodes(dag), "X9")
S <- mb(dag, "X9")
sapply(setdiff(V, S), dsep, bn = dag, y = "X9", z = S)


## ----markov-blanket-and-dseparation-X7----------------------------------------
V <- setdiff(nodes(dag), "X7")
S <- mb(dag, "X7")
sapply(setdiff(V, S), dsep, bn = dag, y = "X7", z = S)


## ----markov-blanket-composition-----------------------------------------------
belongs <- logical(0)
for (node in S)
  belongs[node] <- "X7" %in% mb(dag, node)
belongs


## ----compare-dag-and-moral-graph, echo = FALSE, results = "hide"--------------
local({
pdf("figures/moral.pdf", width = 10, height = 6, paper = "special")
par(mfrow = c(1, 2), oma = c(1, 1, 1, 1), mar = c(1, 5, 1, 5))
graphviz.compare(dag, moral(dag), diff = "none")
dev.off()
})


## ----moral-graph--------------------------------------------------------------
mg1 <- moral(dag)


## ----moral-graph-and-equivalence-classes--------------------------------------
all.equal(moral(dag),
          moral(set.arc(dag, from = "X7", to = "X3")))


## ----manual-moralisation------------------------------------------------------
mg2 <- dag
vs <- vstructs(dag)
for (i in seq(nrow(vs)))
  mg2 <- set.edge(mg2, from = vs[i, "X"], to = vs[i, "Y"],
           check.cycles = FALSE)


## ----remove-arc-directions----------------------------------------------------
mg2 <- skeleton(mg2)
all.equal(mg1, mg2)


## ----show-dseparation-steps, echo = FALSE, results = "hide"-------------------
local({
pdf(file = "figures/dsep.pdf", width = 12, height = 12, paper = "special")
par(mfrow = c(2, 2))
dd <- set.edge(dag, "X3", "X7")
dd <- set.edge(dd, "X2", "X7")
gR <- graphviz.plot(dd, render = FALSE, highlight = list(nodes = c("X1", "X6"),
        col = "black", fill = "lightgrey"))
edgeRenderInfo(gR)$col[c("X2~X7", "X3~X27")] = "transparent"
nodeRenderInfo(gR)$fontsize = 12
renderGraph(gR, graph.pars = list(graph = list(main = "DAG", cex.main = 3)))
nodeRenderInfo(gR)$col["X10"] = "transparent"
nodeRenderInfo(gR)$textCol["X10"] = "transparent"
edgeRenderInfo(gR)$col[c("X1~X10", "X9~X10")] = "transparent"
renderGraph(gR, graph.pars = list(graph = list(main = "Step 1", cex.main = 3)))

edgeRenderInfo(gR)$col[c("X2~X7", "X3~X27")] = "black"
edgeRenderInfo(gR)$arrowhead = "none"
renderGraph(gR, graph.pars = list(graph = list(main = "Step 2", cex.main = 3)))

nodeRenderInfo(gR)$col[c("X4", "X9")] = "transparent"
nodeRenderInfo(gR)$textCol[c("X4", "X9")] = "transparent"
edgeRenderInfo(gR)$col[c("X1~X4", "X2~X4", "X2~X9", "X7~X9")] = "transparent"
edgeRenderInfo(gR)$lwd[c("X1~X2", "X2~X7", "X7~X8", "X8~X6")] = 6
renderGraph(gR, graph.pars = list(graph = list(main = "Step 3", cex.main = 3)))
dev.off()
})


## ----manual-dseparation-------------------------------------------------------
dsep(dag, x = "X1", y = "X6", z = c("X4", "X9"))


## ----manual-dseparation-step1-1-----------------------------------------------
an.nodes = union(ancestors(dag, "X1"), ancestors(dag, "X6"))
an.nodes
an.sepset = union(ancestors(dag, "X4"), ancestors(dag, "X9"))
an.sepset


## ----manual-dseparation-step1-2-----------------------------------------------
sub.nodes = union(c("X1", "X6"), an.nodes)
sub.sep = union(c("X4", "X9"), an.sepset)
sub.dag = subgraph(dag, union(sub.nodes, sub.sep))


## ----manual-dseparation-step1-3-----------------------------------------------
sub.moral = moral(sub.dag)


## ----manual-dseparation-step1-4-----------------------------------------------
final.nodes = setdiff(nodes(sub.moral), c("X4", "X9"))
sub.final = subgraph(sub.moral, final.nodes)
path.exists(sub.final, "X1", "X6")


## ----data-for-the-chapter, include = FALSE, echo = FALSE----------------------
set.seed(4567);
dag.bnlearn <- model2network("[G][E][V|G:E][N|V][W|V][C|N:W]")
disE <- list(coef = c("(Intercept)" = 50), sd = 10)
disG <- list(coef = c("(Intercept)" = 50), sd = 10)
disV <- list(coef = c("(Intercept)" = -10.35534,
             E = 0.70711, G = 0.5), sd = 5)
disN <- list(coef = c("(Intercept)" = 45, V = 0.1), sd = 9.949874)
disW <- list(coef = c("(Intercept)" = 15, V = 0.7), sd = 7.141428)
disC <- list(coef = c("(Intercept)" = 0, N = 0.3, W = 0.7),
             sd = 6.25);
dis.liste = list(E = disE, G = disG, V = disV, N = disN,
                 W = disW, C = disC)
gbn.bnlearn <- custom.fit(dag.bnlearn, dist = dis.liste)
cropdata200 <- cpdist(gbn.bnlearn, nodes = nodes(gbn.bnlearn),
                 evidence = TRUE, n = 200)


## ----example-of-constraint-based-learning-------------------------------------
bn.cor <- gs(cropdata200, test = "cor", alpha = 0.05)
modelstring(bn.cor)


## ----constraint-based-with-different-tests------------------------------------
bn.zf <- gs(cropdata200, test = "zf", alpha = 0.05)
bn.mc <- gs(cropdata200, test = "mc-cor", B = 1000)
all.equal(bn.cor, bn.zf)
all.equal(bn.cor, bn.mc)


## ----constraint-based-with-different-algorithms-------------------------------
bn.iamb <- iamb(cropdata200, test = "cor", alpha = 0.05)
all.equal(bn.cor, bn.iamb)


## ----conditional-independence-tests-------------------------------------------
ci.test("N", "V", test = "cor", data = cropdata200)
ci.test("N", "V", "C", test = "cor", data = cropdata200)


## ----constraint-based-with-whitelist------------------------------------------
bn.cor <- gs(cropdata200, test = "cor", alpha = 0.05,
            whitelist = c("V", "N"))
all.equal(bn.cor, dag.bnlearn)


## ----read-survey-data, echo = FALSE-------------------------------------------
survey <- read.table("../discrete/survey.txt", header = TRUE,
            colClasses = "factor")


## ----score-based-learning-----------------------------------------------------
learned <- hc(survey, score = "bic")
modelstring(learned)
score(learned, data = survey, type = "bic")


## ----learned-vs-true-networks-------------------------------------------------
survey.dag <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
learned.start <- hc(survey, score = "bic", start = survey.dag)
modelstring(learned.start)
all.equal(cpdag(learned), cpdag(learned.start))


## ----hidden-seed, echo = FALSE------------------------------------------------
set.seed(1234)

## ----hill-climbing-random-restart---------------------------------------------
hc(survey, score = "bic", start = random.graph(names(survey)))


## ----step-through-hill-climbing, echo = FALSE, results = "hide"---------------
local({
pdf("figures/hcstep.pdf", width = 8, height = 13, paper = "special")
par(mfrow = c(4, 2))
p <- p2 <- graphviz.plot(survey.dag, render = FALSE)

edgeRenderInfo(p2) <- list(col = "transparent")
graph.par(graph = list(main = "Initial BIC score: -2008.943", cex.main = 2))
renderGraph(p2)
edgeRenderInfo(p2) <- list(col = c("E~R" = "black"), lwd = c("E~R" = 6),
  arrowhead = c("E~R" = "none"), arrowtail = c("E~R" = "open"))
graph.par(graph = list(main = "Current BIC score: -2005.553"))
renderGraph(p2)
edgeRenderInfo(p2) <- list(col = c("S~E" = "black"), lwd = c("E~R" = 1, "S~E" = 6),
  arrowhead = c("S~E" = "none"), arrowtail = c("S~E" = "open"))
graph.par(graph = list(main = "Current BIC score: -2002.827"))
renderGraph(p2)
edgeRenderInfo(p2) <- list(col = c("R~T" = "black"), lwd = c("S~E" = 1, "R~T" = 6))
graph.par(graph = list(main = "Current BIC score: -2000.979"))
renderGraph(p2)
edgeRenderInfo(p2) <- list(col = c("A~E" = "black"), lwd = c("R~T" = 1, "A~E" = 6),
  arrowhead = c("A~E" = "none"), arrowtail = c("A~E" = "open"))
graph.par(graph = list(main = "Current BIC score: -1999.259"))
renderGraph(p2)
edgeRenderInfo(p2) <- list(col = c("E~O" = 1), lwd = c("A~E" = 1, "E~O" = 6))
graph.par(graph = list(main = "Current BIC score: -1998.432"))
renderGraph(p2)
edgeRenderInfo(p2) <- list(lwd = c("E~O" = 1))
graph.par(graph = list(main = "Final BIC score: -1998.432"))
renderGraph(p2)
graph.par(graph = list(main = "True DAG"))
renderGraph(p)
dev.off()
})


## ----hybrid-learning----------------------------------------------------------
mmhc(survey)


## ----hybrid-learning-general--------------------------------------------------
rsmax2(survey, restrict = "mmpc", maximize = "hc")


## ----hybrid-learning-with-arguments-------------------------------------------
rsmax2(survey, restrict = "si.hiton.pc", maximize = "tabu",
  restrict.args = list(test = "x2"),
  maximize.args = list(score = "bde", iss = 1))


## ----junction-trees-step1-----------------------------------------------------
survey.moral = moral(survey.dag)


## ----recreate-survey-bn, echo = FALSE-----------------------------------------
survey.dag <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")

A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")

A.prob <- matrix(c(0.30, 0.50, 0.20), ncol = 3,
            dimnames = list(c(""), A = A.lv))

S.prob <- matrix(c(0.60, 0.40), ncol = 2,
            dimnames = list(c(""), S = S.lv))

E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64, 0.36,
            0.70, 0.30, 0.90, 0.10), dim = c(2, 3, 2),
            dimnames = list(E = E.lv, A = A.lv, S = S.lv))

O.prob <- matrix(c(0.96, 0.04, 0.92, 0.08), nrow = 2,
            dimnames = list(O = O.lv, E = E.lv))

R.prob <- matrix(c(0.25, 0.75, 0.20, 0.80), nrow = 2,
            dimnames = list(R = R.lv, E = E.lv))

T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58, 0.24,
            0.18, 0.70, 0.21, 0.09), dim = c(3, 2, 2),
            dimnames = list(T = T.lv, O = O.lv, R = R.lv))

res3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")

tdp <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob, T = T.prob)
survey.bn <- custom.fit(survey.dag, tdp)


## ----computing-parameters-c1-s12----------------------------------------------
C1 <- coef(survey.bn$E)
for (a in A.lv)
  for (s in S.lv)
    C1[, a, s] <- C1[, A = a, S = s] *
                    coef(survey.bn$A)[a] * coef(survey.bn$S)[s]
S12 <- margin.table(C1, 1)


## ----computing-parameters-c2--------------------------------------------------
C2 <- array(0, dim = c(2, 2, 2),
        dimnames = list(O = O.lv, R = R.lv, E = E.lv))
for (o in O.lv)
  for (r in R.lv)
    for (e in E.lv)
      C2[o, r, e] <- coef(survey.bn$O)[o, e] *
                       coef(survey.bn$R)[r, e] * S12[e]
S23 <- margin.table(C2, 1:2)


## ----computing-parameters-s23-c3----------------------------------------------
C3 <- coef(survey.bn$T)
for (t in T.lv)
  for (o in O.lv)
    for (r in R.lv)
      C3[t, o, r] <- C3[t, o, r] * S23[o, r]


## ----belief-propagation-1-----------------------------------------------------
new.C1 <- C1
for (e in E.lv)
  for (a in A.lv)
    for (s in S.lv)
      new.C1[e, a, s] <-
        C1[e, a, s] / S12[e] * ifelse(e == "high", 1, 0)


## ----belief-propagation-2-----------------------------------------------------
new.S12 <- margin.table(new.C1, 1)


## ----belief-propagation-3-----------------------------------------------------
new.C2 <- C2
for (o in O.lv)
  for (r in R.lv)
    for (e in E.lv)
      new.C2[o, r, e] <- C2[o, r, e] / S12[e] * new.S12[e]
new.S23 <- margin.table(new.C2, 1:2)


## ----belief-propagation-4-----------------------------------------------------
new.C3 <- C3
for (t in T.lv)
  for (o in O.lv)
    for (r in R.lv)
      new.C3[t, o, r] <- C3[t, o, r] / S23[o, r] * new.S23[o, r]


## ----belief-propagation-5-----------------------------------------------------
T <- margin.table(new.C3, 1)
S <- margin.table(new.C1, 3)
as.numeric(S["M"] * T["car"])


## ----hidden-particles, echo = FALSE------------------------------------
set.seed(123)


## ----logic-sampling-----------------------------------------------------------
cpquery(survey.bn, event = (S == "M") & (T == "car"),
          evidence = (E == "high"), n = 10^6, method = "ls")


## ----logic-sampling-step1-----------------------------------------------------
particles <- rbn(survey.bn, 10^6)
head(particles, n = 5)


## ----logic-sampling-step2-----------------------------------------------------
partE <- particles[(particles[, "E"] == "high"), ]
nE <- nrow(partE)


## ----logic-sampling-step3-----------------------------------------------------
partEq <- partE[(partE[, "S"] == "M") & (partE[, "T"] == "car"), ]
nEq <- nrow(partEq)


## ----logic-sampling-step4-----------------------------------------------------
nEq/nE


## ----likelihood-sampling-step1------------------------------------------------
mutbn <- mutilated(survey.bn, list(E = "high"))
mutbn$E


## ----hidden-seed-again, echo = FALSE------------------------------------------
set.seed(123)

## ----likelihood-sampling-step2------------------------------------------------
particles <- rbn(survey.bn, 10^6)
partQ <- particles[(particles[, "S"] == "M") &
                   (particles[, "T"] == "car"), ]
nQ <- nrow(partQ)
nQ/10^6


## ----likelihood-sampling-step3------------------------------------------------
w <- logLik(survey.bn, particles, nodes = "E", by.sample = TRUE)


## ----likelihood-sampling-step4------------------------------------------------
wEq <- sum(exp(w[(particles[, "S"] == "M") &
                 (particles[, "T"] == "car")]))
wE <- sum(exp(w))
wEq/wE


## ----hidden-seed-once-more, echo = FALSE--------------------------------------
set.seed(678)

## ----likelihood-sampling------------------------------------------------------
cpquery(survey.bn, event = (S == "M") & (T == "car"),
          evidence = list(E = "high"), method = "lw")


## ----load-and-show-marks-data-------------------------------------------------
data(marks)
head(marks, n = 5)


## ----example-with-latent-variable---------------------------------------------
latent <- factor(c(rep("A", 44), "B", rep("A", 7), rep("B", 36)))
modelstring(hc(marks[latent == "A", ]))
modelstring(hc(marks[latent == "B", ]))
modelstring(hc(marks))


## ----example-with-discretisation----------------------------------------------
dmarks <- discretize(marks, breaks = 2, method = "interval")
modelstring(hc(cbind(dmarks, LAT = latent)))


## ----dags-with-and-without-latent-grouping, echo = FALSE, results = "hide"----
local({
bn <- hc(marks)
bn.latent <- hc(cbind(dmarks, LAT = latent))
pdf("figures/latent.pdf", width = 10, height = 8.5, paper = "special")
par(mfrow = c(2, 2))
graph.par(graph = list(cex.main = 2))
p1 <- p2 <- p3 <- graphviz.plot(skeleton(bn), layout = "fdp", render = FALSE)
graph.par(graph = list(cex.main = 2))
edgeRenderInfo(p1) <- list(col = "transparent")
edgeRenderInfo(p1) <- list(col = c("MECH~ALG" = "black"),
                           arrowhead = c("MECH~ALG" = "open"))
edgeRenderInfo(p1) <- list(col = c("VECT~ALG" = "black"),
                           arrowtail = c("VECT~ALG" = "open"))
edgeRenderInfo(p1) <- list(col = c("ALG~ANL" = "black"),
                           arrowhead = c("ALG~ANL" = "open"))
edgeRenderInfo(p1) <- list(col = c("ALG~STAT" = "black"),
                           arrowhead = c("ALG~STAT" = "open"))
edgeRenderInfo(p1) <- list(col = c("ANL~STAT" = "black"),
                           arrowhead = c("ANL~STAT" = "open"))
renderGraph(p1, graph.pars = list(graph = list(main = "Group A")))
edgeRenderInfo(p2) <- list(col = "transparent")
edgeRenderInfo(p2) <- list(col = c("MECH~VECT" = "black", "VECT~MECH" = "black"),
                          arrowhead = c("MECH~VECT" = "open"))
renderGraph(p2, graph.pars = list(graph = list(main = "Group B")))
edgeRenderInfo(p3) <- list(col = "transparent")
edgeRenderInfo(p3) <- list(col = c("MECH~VECT" = "black"),
                           arrowhead = c("MECH~VECT" = "open"))
edgeRenderInfo(p3) <- list(col = c("VECT~ALG" = "black"),
                           arrowhead = c("VECT~ALG" = "open"))
edgeRenderInfo(p3) <- list(col = c("MECH~ALG" = "black"),
                           arrowhead = c("MECH~ALG" = "open"))
edgeRenderInfo(p3) <- list(col = c("ALG~STAT" = "black"),
                           arrowhead = c("ALG~STAT" = "open"))
edgeRenderInfo(p3) <- list(col = c("ANL~STAT" = "black"),
                           arrowhead = c("ANL~STAT" = "open"))
edgeRenderInfo(p3) <- list(col = c("ALG~ANL" = "black"),
                           arrowhead = c("ALG~ANL" = "open"))
renderGraph(p3, graph.pars =
            list(graph = list(main = "BN without Latent Grouping")))
graphviz.plot(bn.latent, layout = "fdp", main = "BN with Latent Grouping")
dev.off()
})


## ----10-fold-cross-validation-1-----------------------------------------------
bn.cv(marks, bn = "hc", method = "k-fold", k = 10, runs = 10)


## ----10-fold-cross-validation-2-----------------------------------------------
bn.cv(marks, bn = "hc", method = "k-fold", k = 10, runs = 10,
  loss = "cor-lw", loss.args = list(target = "ALG"))


## ----10-fold-cross-validation-fixed-dag---------------------------------------
dag.check <- model2network("[MECH][ALG|MECH][VECT][ANL|ALG][STAT|ANL]")
bn.cv(marks, bn = dag.check, method = "k-fold", k = 10, runs = 10,
  loss = "cor-lw", loss.args = list(target = "ALG"))


## ----measuring-shd------------------------------------------------------------
shd(survey.dag, hc(survey))


## ----comparing-dags-----------------------------------------------------------
diff <- compare(cpdag(survey.dag), cpdag(hc(survey)))
unlist(diff)


## ----showcase-graphviz-compare, echo = FALSE, results = "hide"----------------
pdf(file = "figures/graphviz-compare.pdf", width = 12, height = 5)
par(mfrow = c(1, 2))
graph.par(list(nodes = list(fontsize = 11)))
graphviz.compare(cpdag(survey.dag), cpdag(hc(survey)),
  main = c("Original CPDAG", "Learned CPDAG"),
  diff.args = list(fn.col = "grey", fn.lwd = 4, fn.lty = "longdash",
                   fp.col = "grey", fp.lwd = 4))
dev.off()


## ----call-graphviz-compare, fig.keep = "none"---------------------------------
graphviz.compare(cpdag(survey.dag), cpdag(hc(survey)),
  main = c("Original CPDAG", "Learned CPDAG"),
  diff.args = list(fn.col = "grey", fn.lwd = 4, fn.lty = "longdash",
                   fp.col = "grey", fp.lwd = 4))


## ----perturb-data-for-arc-strength--------------------------------------------
head(boot.strength(survey, algorithm = "hc"), n = 4)


## ----perturn-data-in-general--------------------------------------------------
boot.stats <- bn.boot(survey, statistic = narcs, algorithm = "hc",
                algorithm.args = list(score = "bde", iss = 1))
summary(unlist(boot.stats))
boot.stats <- bn.boot(survey, statistic = narcs, algorithm = "hc",
                algorithm.args = list(score = "bde", iss = 5))
summary(unlist(boot.stats))

