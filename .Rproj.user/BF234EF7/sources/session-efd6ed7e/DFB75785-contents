#' Bar chart for paired observations.
#'
#' @param df A data.frame of plot data.
#' @param x A string of the column name for x.
#' @param y A string of the column name for y.
#' @param fill A string of the column name for fill.
#' @param group A string of the column name for grouping IDs.
#' @param facet (Optional) A string or a pair of strings of the column name(s) for facet.
#' @param colors (Optional) A vector of colors.
#' @param ylab A string for y-axis label.
#' @param yscale The y-axis scale. Can be either "continuous" or "log10".
#' @param facet_type The facetting type. Can be either "wrap" or "grid".
#' @param facet_scales The facetting scales.
#' @param facet_space The spacing scheme in the facetting grid.
#' @param facet_ncol Number of columns.
#' @param facet_nrow Number of rows.
#' @export
#' @rdname ggPairedBarPlot
#' @name ggPairedBarPlot
ggPairedBarPlot <- function(
  df,
  x,
  y,
  fill,
  group,
  facet=NULL,
  colors=NULL,
  ylab="Y",
  yscale="continuous",
  facet_type="wrap",
  facet_scales="free_y",
  facet_space="fixed",
  facet_ncol=NULL,
  facet_nrow=NULL
){
  # Format data
  df_sub <- data.frame("X"=df[[x]], "Y"=df[[y]], "Fill"=df[[fill]], "GroupID"=df[[group]])
  if(is.null(facet)){
    df_sub$"Facet" <- ""
    df <- df_sub
    df_summary <- df %>%
      dplyr::group_by(X, Fill, Facet) %>%
      dplyr::summarise(N=n(), Y.mean=mean(Y), Y.up=Y.mean+plotrix::std.error(Y), Y.lo=Y.mean-plotrix::std.error(Y))
  }else{
    if(length(facet)==1){
      df_sub$"Facet" <- df[[facet]]
      df <- df_sub
      df_summary <- df %>%
        dplyr::group_by(X, Fill, Facet) %>%
        dplyr::summarise(N=n(), Y.mean=mean(Y), Y.up=Y.mean+plotrix::std.error(Y), Y.lo=Y.mean-plotrix::std.error(Y))
    }
    if(length(facet)==2){
      df_sub$"Facet1" <- df[[facet[1]]]
      df_sub$"Facet2" <- df[[facet[2]]]
      df <- df_sub
      df_summary <- df %>%
        dplyr::group_by(X, Fill, Facet1, Facet2) %>%
        dplyr::summarise(N=n(), Y.mean=mean(Y), Y.up=Y.mean+plotrix::std.error(Y), Y.lo=Y.mean-plotrix::std.error(Y))
    }
  }

  # Plot
  set.seed(12345)
  plt <- ggplot(NULL)
  plt <- plt +
    geom_bar(data=df_summary, aes_string(x="X", y="Y.mean", fill="Fill"),
             stat="identity", width=0.5, position=position_dodge(width=0.75), color="black", show.legend=F) +
    geom_line(data=df, aes_string(x="X", y="Y", group="GroupID"), color="grey80") +
    geom_point(data=df, aes_string(x="X", y="Y", fill="Fill"),
               size=2.5, shape=21, color="black", position=position_jitterdodge(jitter.width=0.4, dodge.width=0.75)) +
    guides(fill=guide_legend(nrow=1))
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
      plt <- plt + ggh4x::facet_wrap2(
        facets=f, scales=facet_scales,
        ncol=facet_ncol, nrow=facet_nrow,
        axes="all"
      )
    }
    if(facet_type=="grid"){
      if(length(facet)==1){
        plt <- plt + ggh4x::facet_grid2(
          rows="Facet",
          scales=facet_scales,
          space=facet_space, axes="all"
        )
      }
      if(length(facet)==2){
        plt <- plt + ggh4x::facet_grid2(
          rows="Facet1", cols="Facet2",
          scales=facet_scales,
          space=facet_space, axes="all"
        )
      }
    }
  }

  # Scale
  if(yscale=="log10"){
    plt <- plt + scale_y_log10(
      name=ylab,
      breaks=scales::log_breaks(),
      labels=prettyNum,
      expand=expansion(mult=c(0, 0.1))
    )
  }else{
    plt <- plt + scale_y_continuous(
      name=ylab,
      expand=expansion(mult=c(0, .1))
    )
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
