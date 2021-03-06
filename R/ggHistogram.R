#' Ridged histogram for flow cytometry data.
#'
#' The dashed line represents the median.
#'
#' @param df A data.frame of plot data.
#' @param x A string of the column name for x.
#' @param y A string of the column name for y.
#' @param fill A string of the column name for fill.
#' @param facet (Optional) A string of the column name for facet. Currently facetting with only onw variable is implemented.
#' @param colors (Optional) A vector of colors.
#' @param facet_type The facetting type. Can be either "wrap" or "grid".
#' @param facet_scales The facetting scales.
#' @param facet_space The spacing scheme in the facetting grid.
#' @param facet_ncol Number of columns.
#' @param facet_nrow Number of rows.
#' @export
#' @rdname ggHistogram
#' @name ggHistogram
ggHistogram <- function(
  df, x, y, fill, facet=NULL, colors=NULL,
  facet_type="wrap", facet_scales="free", facet_space="free", facet_ncol=NULL, facet_nrow=NULL
){
  # Format data
  df_sub <- data.frame("X"=df[[x]], "Y"=df[[y]], "Fill"=df[[fill]])
  if(is.null(facet)){
    df_sub$"Facet" <- ""
  }else{
    df_sub$"Facet" <- df[[facet]]
  }
  df <- df_sub

  # Plot
  plt <- ggplot(df, aes(x=X, y=Y, fill=Fill)) +
    ggridges::stat_density_ridges(
      geom="density_ridges", alpha=0.5,
      quantile_lines=T, quantiles=2, vline_linetype="dashed"
    ) +
    xlab(NULL) +
    ylab(NULL) +
    ggridges::theme_ridges(font_size=14, center_axis_labels=T) +
    theme(legend.position="none",
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=14))
  if(!is.null(colors)){
    plt <- plt +
      scale_fill_manual(values=colors)
  }else{
    plt <- plt +
      viridis::scale_fill_viridis(option="cividis")
  }
  if(!is.null(facet)){
    if(facet_type=="wrap"){
      plt <- plt + lemon::facet_rep_wrap(
        ~Facet, scales=facet_scales, ncol=facet_ncol, nrow=facet_nrow, repeat.tick.labels=F
      )
    }
    if(facet_type=="grid"){
      plt <- plt + lemon::facet_rep_grid(
        ~Facet, scales=facet_scales, space=facet_space, repeat.tick.labels=F
      )
    }
  }
  return(plt)
}
