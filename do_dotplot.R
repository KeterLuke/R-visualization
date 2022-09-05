####draw dotplot for single cell data ####
do_dotplot <- function(sce,feature_list,group,pdf_dir,scale,pdf_filename,width,height,col_max,col_min,scale_dot,scale_max){
  library(Seurat)
  library(ggplot2)
  if(scale==T){
    gg = DotPlot(sce, features=feature_list,
                 group.by = group,
                 col.max = col_max,col.min = col_min,scale.by = scale_dot,scale.max = scale_max)
  }else{
    gg = DotPlot(sce, features=feature_list,
                 group.by = group,
                 col.max = col_max,col.min = col_min)
  }
 
  p1=gg +
    RotatedAxis()+
    theme(panel.border = element_rect(color="black"),
          panel.spacing = unit(1, "mm"),
         #strip.background = element_rect(color="red"),
         strip.text = element_text(margin=margin(b=3, unit="mm")),
         strip.placement = 'outlet', 
         axis.line = element_blank())+labs(x="", y="") + 
   scale_color_gradient(low = 'grey',high='red')
  
  library(grid)
  q <- ggplotGrob(p1)
  lg <- linesGrob(x=unit(c(0,1),"npc"), y=unit(c(0,0)+0.2,"npc"),
                  gp=gpar(col="black", lwd=4))
  
  grid.newpage(); #grid.draw(lg)
  for (k in grep("strip-t",q$layout$name)) {
    q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
  }
   pdf(paste0(pdf_dir,pdf_filename),width = width,height = height)
   grid.draw(q)
   dev.off()
}
