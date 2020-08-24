#' Bar chart with error bars.
#'
#' The error bar represents mean and SEM.
#'
#' @param df A data.frame of plot data.
#' @param x A string of the column name for x.
#' @param y A string of the column name for y.
#' @param fill A string of the column name for fill.
#' @param shape (Optional) A string of the column name for shape.
#' @param facet (Optional) A string or a pair of strings of the column name(s) for facet.
#' @param colors (Optional) A vector of colors.
#' @param shapes (Optional) A vector of point shapes. E.g., 21:25
#' @param ylab A string for y-axis label.
#' @param yscale The y-axis scale. Can be either "continuous" or "log10".
#' @param facet_type The facetting type. Can be either "wrap" or "grid".
#' @param facet_scales The facetting scales.
#' @param facet_space The spacing scheme in the facetting grid.
#' @param facet_ncol Number of columns.
#' @param facet_nrow Number of rows.
#' @export
#' @rdname ggBarPlot
#' @name ggBarPlot
ggBarPlot <- function(
  df,
  x,
  y,
  fill,
  shape=NULL,
  facet=NULL,
  colors=NULL,
  shapes=NULL,
  ylab="Y",
  yscale="continuous",
  facet_type="wrap",
  facet_scales="free_y",
  facet_space="fixed",
  facet_ncol=NULL,
  facet_nrow=NULL
){
  # Format data
  df_sub <- data.frame("X"=df[[x]], "Y"=df[[y]], "Fill"=df[[fill]])
  if(is.null(shape)){
    df_sub$"Shape" <- ""
  }else{
    df_sub$"Shape" <- df[[shape]]
  }
  if(is.null(facet)){
    df_sub$"Facet" <- ""
    df <- df_sub
    df_summary <- df %>%
      dplyr::group_by(X, Fill, Shape, Facet) %>%
      dplyr::summarise(N=n(), Y.mean=mean(Y), Y.up=Y.mean+plotrix::std.error(Y), Y.lo=Y.mean-plotrix::std.error(Y))
  }else{
    if(length(facet)==1){
      df_sub$"Facet" <- df[[facet]]
      df <- df_sub
      df_summary <- df %>%
        dplyr::group_by(X, Fill, Shape, Facet) %>%
        dplyr::summarise(N=n(), Y.mean=mean(Y), Y.up=Y.mean+plotrix::std.error(Y), Y.lo=Y.mean-plotrix::std.error(Y))
    }
    if(length(facet)==2){
      df_sub$"Facet1" <- df[[facet[1]]]
      df_sub$"Facet2" <- df[[facet[2]]]
      df <- df_sub
      df_summary <- df %>%
        dplyr::group_by(X, Fill, Shape, Facet1, Facet2) %>%
        dplyr::summarise(N=n(), Y.mean=mean(Y), Y.up=Y.mean+plotrix::std.error(Y), Y.lo=Y.mean-plotrix::std.error(Y))
    }
  }

  # Plot
  set.seed(12345)
  plt <- ggplot(NULL)
  if(is.null(shape)){
    plt <- plt +
      geom_bar(data=df_summary, aes_string(x="X", y="Y.mean", fill="Fill"),
               stat="identity", width=0.5, position=position_dodge(width=0.75), color="black", show.legend=F) +
      geom_point(data=df, aes_string(x="X", y="Y", fill="Fill"),
                 size=2.5, shape=21, color="black", position=position_jitterdodge(jitter.width=0.4, dodge.width=0.75)) +
      geom_errorbar(data=df_summary, aes_string(x="X", ymin="Y.lo", ymax="Y.up", fill="Fill"),
                    width=0.5, size=0.5, position=position_dodge(width=0.75), show.legend=F)
    if(!is.null(colors)){
      plt <- plt +
        scale_fill_manual(values=alpha(colors, 0.5))
    }else{
      plt <- plt +
        viridis::scale_fill_viridis(option="cividis")
    }
  }else{
    if(identical(fill, shape)){
      plt <- plt +
        geom_bar(data=df_summary, aes_string(x="X", y="Y.mean", fill="Fill"),
                 stat="identity", width=0.5, position=position_dodge(width=0.75), color="black", show.legend=F) +
        geom_point(data=df, aes_string(x="X", y="Y", group="Fill", fill="Fill", shape="Fill"),
                   size=2.5, color="black", position=position_jitterdodge(jitter.width=0.4, dodge.width=0.75)) +
        geom_errorbar(data=df_summary, aes_string(x="X", ymin="Y.lo", ymax="Y.up", fill="Fill"),
                      width=0.5, size=0.5, position=position_dodge(width=0.75), show.legend=F)
      if(!is.null(colors)){
        plt <- plt +
          scale_fill_manual(values=alpha(colors, 0.5))
      }else{
        plt <- plt +
          viridis::scale_fill_viridis(option="cividis")
      }
      if(!is.null(shapes)){
        plt <- plt +
          scale_shape_manual(values=shapes)
      }
    }else{
      plt <- plt +
        geom_bar(data=df_summary, aes_string(x="X", y="Y.mean", fill="Fill"),
                 stat="identity", width=0.5, position=position_dodge(width=0.75), color="black", show.legend=F) +
        geom_point(data=df, aes_string(x="X", y="Y", fill="Fill", shape="Shape"),
                   size=2.5, color="black", position=position_jitterdodge(jitter.width=0.4, dodge.width=0.75)) +
        geom_errorbar(data=df_summary, aes_string(x="X", ymin="Y.lo", ymax="Y.up", fill="Fill"),
                      width=0.5, size=0.5, position=position_dodge(width=0.75), show.legend=F)
      if(!is.null(colors)){
        plt <- plt +
          scale_fill_manual(values=alpha(colors, 0.5))
      }else{
        plt <- plt +
          viridis::scale_fill_viridis(option="cividis")
      }
      if(!is.null(shapes)){
        plt <- plt +
          scale_shape_manual(values=shapes)
      }
    }
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
