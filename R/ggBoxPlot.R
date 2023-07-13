#' Box plot.
#'
#' @param df A data.frame of plot data.
#' @param x A string of the column name for x.
#' @param y A string of the column name for y.
#' @param fill A string of the column name for fill.
#' @param facet (Optional) A string or a pair of strings of the column name(s) for facet.
#' @param colors (Optional) A vector of colors.
#' @param ylab A string for y-axis label.
#' @param yscale The y-axis scale. Can be either "continuous" or "log10".
#' @param yscale.expand Logical. Whether the y-scale should be expanded down to zero.
#' @param facet_type The facetting type. Can be either "wrap" or "grid".
#' @param facet_scales The facetting scales.
#' @param facet_space The spacing scheme in the facetting grid.
#' @param facet_ncol Number of columns.
#' @param facet_nrow Number of rows.
#' @export
#' @rdname ggBoxPlot
#' @name ggBoxPlot
ggBoxPlot <- function(
    df,
    x,
    y,
    fill,
    facet=NULL,
    colors=NULL,
    ylab="Y",
    yscale="continuous",
    yscale.expand=F,
    facet_type="wrap",
    facet_scales="free_y",
    facet_space="fixed",
    facet_ncol=NULL,
    facet_nrow=NULL
){
  # Plot
  set.seed(12345)
  plt <- ggplot(NULL) +
    geom_boxplot(
      data=df, aes_string(x="X", y="Y", fill="Fill"),
      outlier.shape=NA,
      position=position_jitterdodge(jitter.width=0.4, dodge.width=0.75)
    ) +
    guides(fill=guide_legend(nrow=1))

  # Color
  if(!is.null(colors)){
    plt <- plt +
      scale_fill_manual(values=alpha(colors, 0.5))
  }else{
    plt <- plt +
      viridis::scale_fill_viridis(option="cividis")
  }

  # Facet
  if(!is.null(facet)){
    if(facet_type=="wrap"){
      if(length(facet)==1) f <- c("Facet")
      if(length(facet)==2) f <- c("Facet1","Facet2")
      plt <- plt + lemon::facet_rep_wrap(
        facets=f, scales=facet_scales, ncol=facet_ncol, nrow=facet_nrow, repeat.tick.labels=F
      )
    }
    if(facet_type=="grid"){
      if(length(facet)==1) f <- as.formula(".~Facet")
      if(length(facet)==2) f <- as.formula("Facet1~Facet2")
      plt <- plt + lemon::facet_rep_grid(
        facets=f, scales=facet_scales, space=facet_space, repeat.tick.labels=F
      )
    }
  }

  # Scale
  if(yscale=="log10"){
    plt <- plt + scale_y_log10(
      name=ylab,
      breaks=scales::log_breaks(),
      labels=prettyNum
    )
  }else{
    plt <- plt + scale_y_continuous(
      name=ylab
    )
  }
  if(yscale.expand){
    plt <- plt +
      expand_limits(y=0)
  }

  # Aethetics
  plt <- plt +
    xlab(NULL) +
    ggpubr::theme_pubr(14) +
    theme(legend.spacing.x=unit(0.15, 'cm'),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=14)) +
    ggpubr::rremove("legend.title")
  return(plt)
}
