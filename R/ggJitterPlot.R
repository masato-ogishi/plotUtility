#' Jitter plot with error bars.
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
#' @param add.violin Logical. Whether a violin layer should be added. Default is "auto", in which case violin is generated when the maximal number of samples in a specific group >= minN.violin.
#' @param minN.violin An integer. The minimum number of data points required to add a violin layer.
#' @param ylab A string for y-axis label.
#' @param yscale The y-axis scale. Can be either "continuous" or "log10".
#' @param yscale.expand Logical. Whether the y-scale should be expanded down to zero.
#' @param facet_type The facetting type. Can be either "wrap" or "grid".
#' @param facet_scales The facetting scales.
#' @param facet_space The spacing scheme in the facetting grid.
#' @param facet_ncol Number of columns.
#' @param facet_nrow Number of rows.
#' @export
#' @rdname ggJitterPlot
#' @name ggJitterPlot
ggJitterPlot <- function(
  df,
  x,
  y,
  fill,
  shape=NULL,
  facet=NULL,
  colors=NULL,
  shapes=NULL,
  add.violin="auto",
  minN.violin=10,
  ylab="Y",
  yscale="continuous",
  yscale.expand=T,
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

  # Prepare data for the violin layer
  if(identical(add.violin, "auto")){
    add.violin <- max(df_summary$N, na.rm=T)>=minN.violin
  }
  df_violin <- merge(
    df,
    df_summary %>%
      dplyr::mutate(ViolinQ=N>=minN.violin), ## add violin for those with >=minN.violin data points
    all.x=F, all.y=T
  ) %>%
    dplyr::filter(ViolinQ==T)

  # Plot
  set.seed(12345)
  plt <- ggplot(NULL)
  if(is.null(shape)){
    plt <- plt +
      geom_point(data=df, aes_string(x="X", y="Y", fill="Fill"),
                 size=2.5, shape=21, color="black", position=position_jitterdodge(jitter.width=0.25, dodge.width=0.75)) +
      geom_crossbar(data=df_summary, aes_string(x="X", y="Y.mean", ymin="Y.mean", ymax="Y.mean", fill="Fill"),
                    width=0.9, size=0.2, position=position_dodge(width=0.75), show.legend=F) +
      geom_errorbar(data=df_summary, aes_string(x="X", ymin="Y.lo", ymax="Y.up", fill="Fill"),
                    width=0.5, size=0.5, position=position_dodge(width=0.75), show.legend=F) +
      guides(fill=guide_legend(nrow=1))
    if(!is.null(colors)){
      plt <- plt +
        scale_fill_manual(values=colors)
    }else{
      plt <- plt +
        viridis::scale_fill_viridis(option="cividis")
    }
    if(add.violin==T){
      plt$layers <- c(
        plt$layers[[1]], ## geom_point
        geom_violin(data=df_violin, aes_string(x="X", y="Y", fill="Fill"),
                    trim=T, scale="width", alpha=0.25, position=position_dodge(width=0.75), show.legend=F),
        plt$layers[2:length(plt$layers)] ## other geoms
      ) ## This order seems to be critical to retain the order of factorized variables!
    }
  }else{
    if(identical(fill, shape)){
      plt <- plt +
        geom_point(data=df, aes_string(x="X", y="Y", group="Fill", fill="Fill", shape="Fill"),
                   size=2.5, color="black", position=position_jitterdodge(jitter.width=0.25, dodge.width=0.75)) +
        geom_crossbar(data=df_summary, aes_string(x="X", y="Y.mean", ymin="Y.mean", ymax="Y.mean", group="Fill", fill="Fill"),
                      width=0.9, size=0.2, position=position_dodge(width=0.75), show.legend=F) +
        geom_errorbar(data=df_summary, aes_string(x="X", ymin="Y.lo", ymax="Y.up", group="Fill", fill="Fill"),
                      width=0.5, size=0.5, position=position_dodge(width=0.75), show.legend=F) +
        guides(fill=guide_legend(nrow=1), shape=guide_legend(nrow=1))
      if(!is.null(colors)){
        plt <- plt +
          scale_fill_manual(values=colors)
      }else{
        plt <- plt +
          viridis::scale_fill_viridis(option="cividis")
      }
      if(!is.null(shapes)){
        plt <- plt +
          scale_shape_manual(values=shapes)
      }
      if(add.violin==T){
        plt$layers <- c(
          plt$layers[[1]], ## geom_point
          geom_violin(data=df_violin, aes_string(x="X", y="Y", fill="Fill"),
                      trim=T, scale="width", alpha=0.25, position=position_dodge(width=0.75), show.legend=F),
          plt$layers[2:length(plt$layers)] ## other geoms
        ) ## This order seems to be critical to retain the order of factorized variables!
      }
    }else{
      plt <- plt +
        geom_point(data=df, aes_string(x="X", y="Y", group="Fill", fill="Fill", shape="Shape"),
                   size=2.5, color="black", position=position_jitterdodge(jitter.width=0.25, dodge.width=0.75)) +
        geom_crossbar(data=df_summary, aes_string(x="X", y="Y.mean", ymin="Y.mean", ymax="Y.mean", fill="Fill"),
                      width=0.9, size=0.2, position=position_dodge(width=0.75), show.legend=F) +
        geom_errorbar(data=df_summary, aes_string(x="X", ymin="Y.lo", ymax="Y.up", fill="Fill"),
                      width=0.5, size=0.5, position=position_dodge(width=0.75), show.legend=F) +
        guides(fill=guide_legend(nrow=1, override.aes=list(shape=21)), shape=guide_legend(nrow=1))
      ## The group aes determines dodging.
      if(!is.null(colors)){
        plt <- plt +
          scale_fill_manual(values=colors)
      }else{
        plt <- plt +
          viridis::scale_fill_viridis(option="cividis")
      }
      if(!is.null(shapes)){
        plt <- plt +
          scale_shape_manual(values=shapes)
      }
      if(add.violin==T){
        plt$layers <- c(
          plt$layers[[1]], ## geom_point
          geom_violin(data=df_violin, aes_string(x="X", y="Y", fill="Fill"),
                      trim=T, scale="width", alpha=0.25, position=position_dodge(width=0.75), show.legend=F),
          plt$layers[2:length(plt$layers)] ## other geoms
        ) ## This order seems to be critical to retain the order of factorized variables!
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
    plt <- plt + scale_y_continuous(
      trans=scales::log10_trans(),
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
