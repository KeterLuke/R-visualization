####add annotation to dimplot by confidence ellipse####
DoDimplot <- function(seu_obj,ellipse = F,prob = 0.1,groupby,colors){
library(ggforce)
library(paletteer)
Idents(seu_obj) <- groupby
points <- 
  data.frame(seu_obj@reductions$umap@cell.embeddings, cluster=Idents(seu_obj))
theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
circle <- cbind(cos(theta), sin(theta))
library(plyr)
aux <- function(x, one, two, prob=0.8) {
  if(nrow(x) <= 2) {
    return(NULL)
  }
  sigma <- var(cbind(x[,one], x[,two]))
  mu <- c(mean(x[,one]), mean(x[,two]))
  ed <- sqrt(qchisq(prob, df = 2))
  data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'))
}
ell <- plyr::ddply(points, "cluster", aux, one="UMAP_1", two="UMAP_2",prob = 0.1) 

if(ellipse == T){
plot <- DimPlot(seu_obj,cols = colors) + 
 geom_mark_ellipse(data=ell, aes(x=X1, y=X2, label=cluster, col=cluster),
 inherit.aes = F)
}

plot <- DimPlot(seu_obj,pt.size = 0.25,cols = colors) + geom_mark_ellipse(data=ell, aes(x=X1, y=X2, label=cluster, group=cluster), color = NA,inherit.aes = F)
return(plot)
}
