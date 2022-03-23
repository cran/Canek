## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 5,
  fig.height = 5,
  fig.align = "center",
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Canek)

# Functions
## Function to plot the pca coordinates
plotPCA <- function(pcaData = NULL, label = NULL, legPosition = "topleft"){
  col <- as.integer(label) 
  plot(x = pcaData[,"PC1"], y = pcaData[,"PC2"],
       col = as.integer(label), cex = 0.75, pch = 19,
       xlab = "PC1", ylab = "PC2")
  legend(legPosition,  pch = 19,
         legend = levels(label), 
         col =  unique(as.integer(label)))
}

## -----------------------------------------------------------------------------
lsData <- list(B1 = SimBatches$batches[[1]], B2 = SimBatches$batches[[2]])
batch <- factor(c(rep("Batch-1", ncol(lsData[[1]])),
                  rep("Batch-2", ncol(lsData[[2]]))))
celltype <- SimBatches$cell_types
table(batch)
table(celltype)

## -----------------------------------------------------------------------------
data <- Reduce(cbind, lsData)
pcaData <- prcomp(t(data), center = TRUE, scale. = TRUE)$x

## -----------------------------------------------------------------------------
plotPCA(pcaData = pcaData, label = batch, legPosition = "bottomleft")
plotPCA(pcaData = pcaData, label = celltype, legPosition = "bottomleft")

## -----------------------------------------------------------------------------
data <- RunCanek(lsData)

## -----------------------------------------------------------------------------
pcaData <- prcomp(t(data), center = TRUE, scale. = TRUE)$x

## -----------------------------------------------------------------------------
plotPCA(pcaData = pcaData, label = batch, legPosition = "topleft")
plotPCA(pcaData = pcaData, label = celltype, legPosition = "topleft")

## -----------------------------------------------------------------------------
sessionInfo()

