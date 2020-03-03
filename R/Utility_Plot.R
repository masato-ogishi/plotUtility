#' Plot utilities.
#'
#' @param ggObject The ggplot object to be saved. If missing, the most recently depicted plot will be retrieved using ggplot2::last_plot.
#' @param outputFileName The name of the exported PDF file.
#' @param width A width.
#' @param height A height.
#' @param embedFont Whether the fonts need to be embedded using Ghostscript. Default is False.
#' @param gsPath A path to the Ghostscript exe file.
#' @param gsOption A character vector containing options to Ghostscript.
#' @export
#' @rdname Utility_Plot
#' @name Utility_Plot
savePDF <- function(
  ggObject=NULL,
  outputFileName="plot.pdf",
  width=8,
  height=5,
  embedFont=F,
  gsPath="C:/gs/gs9.16/bin/gswin32c.exe",
  gsOption="-sFONTPATH=C:/Windows/Fonts -dCompressFonts=true -dSubsetFonts=false -dEmbedAllFonts=true"
){
  # Prepare Helvetica fonts
  OS <- Sys.info()[["sysname"]]
  if(OS=="Windows"){
    grDevices::windowsFonts(Helvetica=grDevices::windowsFont("Helvetica"))
  }

  # Prepare the output path
  out <- ifelse(stringr::str_detect(outputFileName, ".pdf$"), outputFileName, paste0(outputFileName, ".pdf"))

  # Retrieve the previous graphics
  if(is.null(ggObject)) ggObject <- ggplot2::last_plot()
  if(is.null(ggObject)) return("No plot can be retrieved!")

  # Save the graphics
  print(ggObject)
  grDevices::dev.copy2pdf(file=out, width=width, height=height, pointsize=12, family="Helvetica", bg="transparent", out.type="pdf", useDingbats=F)
  grDevices::dev.off()

  # Embed fonts (optional)
  if(embedFont==T){
    if(file.exists(gsPath)){
      Sys.setenv(R_GSCMD=gsPath)
      grDevices::embedFonts(out, outfile=out, options=gsOption)
    }
  }

  # Print the output path
  print(normalizePath(out))

  # Done
  return(invisible(NULL))
}

#' @export
#' @rdname Utility_Plot
#' @name Utility_Plot
savePPTX <- function(
  ggObject=NULL,
  outputFileName="plot.pptx",
  width=8,
  height=5
){
  # Prepare the output path
  out <- ifelse(stringr::str_detect(outputFileName, ".pptx$"), outputFileName, paste0(outputFileName, ".pptx"))

  # Retrieve the previous graphics
  if(is.null(ggObject)) ggObject <- ggplot2::last_plot()
  if(is.null(ggObject)) return("No plot can be retrieved!")
  print(ggObject)

  # Save the graphics
  ggObject <- rvg::dml(ggobj=ggObject) ## convert the plot to the DrawingML object
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout="Blank", master="Office Theme")
  doc <- officer::ph_with(doc, value=ggObject, location=officer::ph_location(left=1, top=1, width=width, height=height))
  print(doc, target=out)
  grDevices::dev.off()

  # Print the output path
  print(normalizePath(out))

  # Done
  return(invisible(NULL))
}

#' Statistical significance
#' @param p A p-value, or a set of p-values.
#' @export
#' @rdname Utility_Plot_ggplot-significance
#' @name Utility_Plot_ggplot-significance
format_pvalue <- function(p){
  stringr::str_replace(paste0("P=",format.pval(p, 1)), "=<", "<")
}

#' Publication-ready ggplot scales
#' @param palette Logical. If \code{FALSE}, a discrete scale for ggplot is returned. If \code{TRUE}, a color palette is returned.
#' @export
#' @rdname Utility_Plot_ggplot-scales
#' @name Utility_Plot_ggplot-scales
scale_color_Publication <- function(palette=F){
  col <- c("#386cb0","#d95f02","#7fc97f","deepskyblue","forestgreen","#662506","#e7298a","#984ea3","#e6ab02","grey50")
  if(palette==T){
    return(col)
  }else{
    discrete_scale("color", "Publication", scales::manual_pal(values=col))
  }
}

#' @export
#' @rdname Utility_Plot_ggplot-scales
#' @name Utility_Plot_ggplot-scales
scale_colour_Publication <- function(palette=F) {
  col <- c("#386cb0","#d95f02","#7fc97f","deepskyblue","forestgreen","#662506","#e7298a","#984ea3","#e6ab02","grey50")
  if(palette==T){
    return(col)
  }else{
    discrete_scale("colour", "Publication", scales::manual_pal(values=col))
  }
}

#' @export
#' @rdname Utility_Plot_ggplot-scales
#' @name Utility_Plot_ggplot-scales
scale_fill_Publication <- function(palette=F) {
  col <- c("#386cb0","#d95f02","#7fc97f","deepskyblue","forestgreen","#662506","#e7298a","#984ea3","#e6ab02","grey50")
  if(palette==T){
    return(col)
  }else{
    discrete_scale("fill", "Publication", scales::manual_pal(values=col))
  }
}

#' A publication-ready ggplot theme
#' @param base_size The base font size.
#' @export
#' @rdname Utility_Plot_ggplot-theme
#' @name Utility_Plot_ggplot-theme
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

