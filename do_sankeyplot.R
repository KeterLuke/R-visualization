####draw sankey plot for single cell data####
Dosankeyplot <- function(sce,factor1,factor2){
  library(ggplot2)
  library(ggalluvial)
  library(paletteer)
  df <- sce@meta.data[,c(factor1,factor2)]
  options(stringsAsFactors = F)
  colors <- unique(c(paletteer_d(`"awtools::a_palette"`),
            paletteer_d(`"awtools::bpalette"`),
            paletteer_d(`"awtools::mpalette"`),
            paletteer_d(`"awtools::gpalette"`),
            paletteer_d(`"awtools::spalette"`),
            paletteer_d(`"awtools::ppalette"`),
            paletteer_d(`"calecopal::sierra1"`),
            paletteer_d(`"calecopal::sierra2"`)))
  gg <- ggplot(df,aes(axis1 = df[,1],axis2 = df[,2]))
  gg <- gg + 
    geom_alluvium(aes(fill = as.factor(df[,1])),
                  width = 2/5,discern = F,alpha = .5) + scale_fill_manual(values = colors)
  gg <- gg + geom_stratum(width = 2/5,discern = F)
  gg <- gg + geom_text(stat = 'stratum',discern = F,
                       aes(label = after_stat(stratum)))
  gg <- gg + 
    theme(legend.position = "none",
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size =15,colour = "black"),
          axis.title = element_blank()) +
    scale_x_discrete(position = "top")
  return(gg)
}
