#' GSEA Enrichment Plots
#'
#' Create a pdf of GSEA function plot outputs 4 graphs per page and saves to your working directory
#' @param list.of.plots A list of ggplot items
#' @param plotname filename to save the plot as
#' @export

plot.ES=function(list.of.plots="",plotname=""){
  filename <- paste0(plotname,".pdf")
  pdf(filename)
  #added print
  print(marrangeGrob(pp$plots, nrow = 2,ncol=2))
  dev.off()
}

