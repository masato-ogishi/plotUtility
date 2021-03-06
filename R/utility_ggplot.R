#' Publication-ready ggplot scales
#' @param palette Logical. If \code{FALSE}, a discrete scale for ggplot is returned. If \code{TRUE}, a color palette is returned.
#' @export
#' @rdname utility_ggplot_scales
#' @name utility_ggplot_scales
scale_color_Publication <- function(palette=F){
  col <- c("#386cb0","#d95f02","#7fc97f","deepskyblue","forestgreen","#662506","#e7298a","#984ea3","#e6ab02","grey50")
  if(palette==T){
    return(col)
  }else{
    discrete_scale("color", "Publication", scales::manual_pal(values=col))
  }
}

#' @export
#' @rdname utility_ggplot_scales
#' @name utility_ggplot_scales
scale_colour_Publication <- function(palette=F) {
  col <- c("#386cb0","#d95f02","#7fc97f","deepskyblue","forestgreen","#662506","#e7298a","#984ea3","#e6ab02","grey50")
  if(palette==T){
    return(col)
  }else{
    discrete_scale("colour", "Publication", scales::manual_pal(values=col))
  }
}

#' @export
#' @rdname utility_ggplot_scales
#' @name utility_ggplot_scales
scale_fill_Publication <- function(palette=F) {
  col <- c("#386cb0","#d95f02","#7fc97f","deepskyblue","forestgreen","#662506","#e7298a","#984ea3","#e6ab02","grey50")
  if(palette==T){
    return(col)
  }else{
    discrete_scale("fill", "Publication", scales::manual_pal(values=col))
  }
}

#' A publication-ready ggplot theme
#' @param base_size A default font size.
#' @export
#' @rdname utility_ggplot_theme
#' @name utility_ggplot_theme
theme_Publication <- function(base_size=14){
  OS <- Sys.info()[["sysname"]]
  if(OS=="Windows"){
    ## Translation of a device-independent R graphics font family name to a windows font description
    grDevices::windowsFonts(Helvetica=grDevices::windowsFont("Helvetica"))
  }

  # Preset ggplot parameters
  .pt <- 2.8
  .len0_null <- function(x){
    if(length(x)==0){
      return(NULL)
    }else{
      return(x)
    }
  }

  # A ggplot theme for an "L-shape" (left + bottom) border
  theme_L_border <- function(colour="black", size=1, linetype=1) {
    # use with e.g.: ggplot(...) + theme( panel.border=theme_L_border() ) + ...
    structure(
      list(colour=colour, size=size, linetype=linetype),
      class=c("theme_L_border", "element_blank", "element")
    )
  }
  element_grob.theme_L_border <- function(
    element, x=0, y=0, width=1, height=1,
    colour=NULL, size=NULL, linetype=NULL,
    ...
  ){
    gp <- grid::gpar(lwd=.len0_null(size*.pt), col=colour, lty=linetype)
    element_gp <- grid::gpar(lwd=.len0_null(element$size*.pt), col=element$colour, lty=element$linetype)
    grid::polylineGrob(
      x=c(x+width, x, x), y=c(y,y,y+height), ..., default.units="npc",
      gp=utils::modifyList(element_gp, gp)
    )
  }

  # A combined set of ggplot theme options
  ggthemes::theme_foundation(base_size=base_size, base_family="Helvetica") +
    theme(
      plot.title=element_text(face="plain", size=rel(1.2), hjust=0.5),
      text=element_text(),
      plot.background=element_rect(colour=NA),
      plot.margin=unit(c(10,5,5,5), "mm"),
      panel.background=element_rect(colour=NA),
      panel.border=theme_L_border(),
      panel.grid.major=element_line(size=0.5, colour="#f0f0f0"),
      panel.grid.minor=element_blank(),
      axis.title=element_text(size=rel(1.0)),
      axis.title.x=element_text(vjust=-0.25),
      axis.title.y=element_text(vjust=1.5),
      axis.text=element_text(size=rel(0.9)),
      axis.line=element_line(size=0.7, colour="black"),
      axis.ticks=element_line(),
      legend.position="right",
      legend.direction="vertical",
      legend.background=element_blank(),
      legend.key=element_rect(colour=NA),
      legend.key.size=unit(0.7, "cm"),
      legend.spacing=unit(0.5, "cm"),
      legend.title=element_text(face="bold", size=rel(1.0)),
      legend.text=element_text(size=rel(1.0)),
      strip.background=element_blank(),
      strip.text=element_text(size=rel(1.0))
    )
}

