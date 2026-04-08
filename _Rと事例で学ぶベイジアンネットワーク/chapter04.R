## ----chapter4-init-stanza, include = FALSE, cache = FALSE, echo = FALSE-------
library(Rgraphviz)
library(bnlearn)


## ----domotics-dags, echo = FALSE, results = "hide"----------------------------
local({
pdf("figures/dags.pdf", width = 8, height = 11)
par(mfrow = c(2, 2))
static.dag <- model2network("[W][St|W][Tout][Tin|W:Tout]")
gR <- graphviz.plot(static.dag, render = FALSE)
nodeRenderInfo(gR)$fontsize <- 13
nodeRenderInfo(gR)$lwd <- 2
edgeRenderInfo(gR)$lwd <- 2
gR <- layoutGraph(gR, attrs = list(graph = list(rankdir = "LR")))
renderGraph(gR)

dyn1.dag = model2network(paste0("[W0][St0|W0][Tout0][Tin0|W0:Tout0]",
                        "[W1|W0][St1|St0:W1][Tout1|Tout0][Tin1|Tin0:W1:Tout1]"))
t0 <- c("W0", "St0", "Tout0", "Tin0")
t1 <- c("W1", "St1", "Tout1", "Tin1")
gR <- graphviz.plot(dyn1.dag, render = FALSE)
sg0 <- list(graph = subGraph(t0, gR), cluster = TRUE)
sg1 <- list(graph = subGraph(t1, gR), cluster = TRUE)
gR <- layoutGraph(gR, attrs = list(graph = list(rankdir = "LR")), subGList =
                                   list(sg0, sg1))
cross <- c("St0~St1", "Tin0~Tin1", "W0~W1", "Tout0~Tout1")
nodeRenderInfo(gR)$fontsize <- 13
nodeRenderInfo(gR)$lwd <- 2
edgeRenderInfo(gR)$lwd <- 2
edgeRenderInfo(gR)$col[cross] <- "darkgrey"
renderGraph(gR)

dyn2.dag <- model2network(paste0("[W0][St0][Tout0][Tin0]",
                        "[St1|St0:W0][Tout1|Tout0][Tin1|Tin0:W0:Tout1]"))
t1 <- c("St1", "Tout1", "Tin1")
gR <- graphviz.plot(dyn2.dag, render = FALSE)
sg0 <- list(graph = subGraph(t0, gR), cluster = TRUE)
sg1 <- list(graph = subGraph(t1, gR), cluster = TRUE)
gR <- layoutGraph(gR, attrs = list(graph = list(rankdir = "LR")), subGList =
                                   list(sg0, sg1))
cross <- c("St0~St1", "Tin0~Tin1", "Tout0~Tout1", "W0~St1", "W0~Tin1")
nodeRenderInfo(gR)$fontsize <- 13
nodeRenderInfo(gR)$lwd <- 2
edgeRenderInfo(gR)$lwd <- 2
edgeRenderInfo(gR)$col[cross] <- "darkgrey"
renderGraph(gR)

roll.dag <- model2network("[W][St|W][Tout][Tin|W:Tout]")
roll.dag$arcs = rbind(roll.dag$arcs, matrix(rep("Tout", 2), ncol = 2))
roll.dag$arcs = rbind(roll.dag$arcs, matrix(rep("Tin", 2), ncol = 2))
roll.dag$arcs = rbind(roll.dag$arcs, matrix(rep("St", 2), ncol = 2))
gR <- graphviz.plot(roll.dag, render = FALSE)
gR <- layoutGraph(gR, attrs = list(graph = list(rankdir = "LR")))
loop <- c("Tin~Tin", "Tout~Tout", "St~St", "W~St", "W~Tin")
nodeRenderInfo(gR)$fontsize <- 13
nodeRenderInfo(gR)$lwd <- 2
edgeRenderInfo(gR)$lwd <- 2
edgeRenderInfo(gR)$col[loop] <- "darkgrey"
renderGraph(gR)

dev.off()
})


## ----windows-probabilities-1>-------------------------------------------------
T.lv <- c("<18", "18-24", ">24")
Tout0.prob <- array(c(0.20, 0.70, 0.10), dim = 3,
                dimnames = list(Tout0 = T.lv))
Tout1.prob <- array(c(0.80, 0.19, 0.01, 0.10, 0.80, 0.10,
                      0.01, 0.19, 0.80), dim = c(3, 3),
                dimnames = list(Tout1 = T.lv, Tout0 = T.lv))
Tout1.prob


## ----windows-probabilities-2--------------------------------------------------
W.lv <- c("open", "closed")
W0.prob <- array(c(0.5, 0.5), dim = 2, dimnames = list(W0 = W.lv))


## ----windows-probabilities-3--------------------------------------------------
St.lv <- c("low", "high")
St0.prob <- array(c(0.50, 0.50), dim = 2, dimnames = list(St0 = St.lv))
St1.prob <- array(c(0.90, 0.10, 0.70, 0.30, 0.70,
                    0.30, 0.10, 0.90), dim = c(2, 2, 2),
              dimnames = list(St1 = St.lv, St0 = St.lv, W0 = W.lv))
St1.prob


## ----windows-probabilities-4--------------------------------------------------
Tin0.prob <- array(c(0.10, 0.85, 0.05), dim = 3,
               dimnames = list(Tin0 = T.lv))
Tin1.prob <- array(c(
          # W0 = "open", Tin0 = "<18"
          0.875, 0.125, 0, 0.075, 0.9, 0.025, 0.075, 0.7, 0.225,
          # W0 = "closed", Tin0 = "<18"
          0.875, 0.125, 0, 0.475, 0.5, 0.025, 0.025, 0.65, 0.325,
          # W0 = "open", Tin0 = "18-24"
          0.475, 0.525, 0, 0.075, 0.8, 0.125, 0, 0.875, 0.125,
          # W0 = "closed", Tin0 = "18-24"
          0.075, 0.9, 0.025, 0, 0.875, 0.125, 0, 0.475, 0.525,
          # W0 = "open", Tin0 = ">24"
          0.15, 0.725, 0.125, 0, 0.475, 0.525, 0, 0.475, 0.525,
          # W0 = "closed", Tin0 = ">24"
          0, 0.125, 0.875, 0, 0.075, 0.925, 0, 0.175, 0.825),
    dim = c(3, 3, 2, 3),
    dimnames = list(Tin1 = T.lv, Tout1 = T.lv, W0 = W.lv, Tin0 = T.lv))


## ----windows-probabilities-5--------------------------------------------------
dag <- model2network(paste0("[W0][St0][Tout0][Tin0][St1|St0:W0]",
             "[Tout1|Tout0][Tin1|Tin0:W0:Tout1]"))
cpt <- list(Tout0 = Tout0.prob, Tout1 = Tout1.prob, W0 = W0.prob,
         Tin0 = Tin0.prob, Tin1 = Tin1.prob, St0 = St0.prob,
         St1 = St1.prob)
dbn <- custom.fit(dag, cpt)


## ----windows-number-of-parameters---------------------------------------------
nparams(dbn)


## ----windows-generate-data-for-learning, echo = FALSE-------------------------
domotics <- rbn(dbn, 2000)
write.table(domotics, file = "domotics.txt")


## ----grouping-nodes-----------------------------------------------------------
t0.nodes <- c("W0", "St0", "Tout0", "Tin0")
t1.nodes <- c("St1", "Tout1", "Tin1")


## ----t0-blacklist-------------------------------------------------------------
bl <- set2blacklist(t0.nodes)


## ----t1-to-t0-blacklist-------------------------------------------------------
bl <- rbind(bl, tiers2blacklist(list(t0.nodes, t1.nodes)))
head(bl)


## ----structure-learning-with-blacklist----------------------------------------
dbn.hc <- hc(domotics, blacklist = bl)
all.equal(dag, dbn.hc)


## ----parameter-learning-------------------------------------------------------
dbn.fit <- bn.fit(dbn.hc, domotics)


## ----prediction-example-1-----------------------------------------------------
cpquery(dbn.fit, event = (St1 == "low") & (Tin1 == "18-24"),
  evidence = (St0 == "high") & (Tin0 == "18-24") &
             (Tout0 == "<18") & (W0 == "closed"))
cpquery(dbn.fit, event = (St1 == "low") & (Tin1 == "18-24"),
  evidence = (St0 == "high") & (Tin0 == "18-24") &
             (Tout0 == "<18") & (W0 == "open"))


## ----prediction-example-2-----------------------------------------------------
cpquery(dbn.fit, event = (St1 == "low") & (Tin1 == "18-24"),
  evidence = (St0 == "high") & (Tin0 == "18-24") & (W0 == "closed"))
cpquery(dbn.fit, event = (St1 == "low") & (Tin1 == "18-24"),
  evidence = (St0 == "high") & (Tin0 == "18-24") & (W0 == "open"))


## ----prediction-example-3-----------------------------------------------------
evidence <- data.frame(St0 = factor("high", levels = St.lv),
              Tin0 = factor("18-24", levels = T.lv),
              Tout0 = factor("<18", levels = T.lv),
              W0 = factor("open", levels = W.lv))


## ----prediction-example-4-----------------------------------------------------
predict(dbn.fit, data = evidence , node = "Tin1", method = "bayes-lw")
evidence$Tin1 <- factor("18-24", levels = T.lv)
predict(dbn.fit, data = evidence , node = "St1", method = "bayes-lw")


## ----horizontal-dbn-dag-------------------------------------------------------
gR <- graphviz.plot(dag, render = FALSE)


## ----horizontal-dbn-subgraphs-------------------------------------------------
t0.nodes <- c("W0", "St0", "Tout0", "Tin0")
t1.nodes <- c("St1", "Tout1", "Tin1")
sg0 <- list(graph = subGraph(t0.nodes, gR), cluster = TRUE)
sg1 <- list(graph = subGraph(t1.nodes, gR), cluster = TRUE)


## ----horizontal-dbn-layout----------------------------------------------------
gR <- layoutGraph(gR, attrs = list(graph = list(rankdir = "LR")),
        subGList = list(sg0, sg1))


## ----horizontal-dbn-edges, fig.keep = "none"----------------------------------
cross <- c("St0~St1", "Tin0~Tin1", "Tout0~Tout1", "W0~St1", "W0~Tin1")
edgeRenderInfo(gR)$col[cross] = "darkgrey"
renderGraph(gR)

