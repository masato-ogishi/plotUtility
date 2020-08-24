#' Exporting plots as PDF or PowerPoint graphics
#'
#' @param graphicsObject The plot object to be saved. If missing, the most recently depicted plot will be retrieved using ggplot2::last_plot or recordPlot. Note: recordPlot is not compatible with exporting an editable vector graphic in PowerPoint.
#' @param outputFileName The name of the exported PDF file.
#' @param width A width.
#' @param height A height.
#' @param embedFont Whether the fonts need to be embedded using Ghostscript. Default is False.
#' @param gsPath A path to the Ghostscript exe file.
#' @param gsOption A character vector containing options to Ghostscript.
#' @export
#' @rdname utility_export
#' @name utility_export
savePDF <- function(
  graphicsObject=ggplot2::last_plot(),
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
  if(is.null(graphicsObject)) return("No plot can be retrieved!")

  # Save the graphics
  print(graphicsObject)
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
#' @rdname utility_export
#' @name utility_export
savePPTX <- function(
  graphicsObject=ggplot2::last_plot(),
  outputFileName="plot.pptx",
  width=8,
  height=5
){
  # Prepare the output path
  out <- ifelse(stringr::str_detect(outputFileName, ".pptx$"), outputFileName, paste0(outputFileName, ".pptx"))

  # Retrieve the previous graphics
  if(is.null(graphicsObject)) return("No plot can be retrieved!")

  # Save the graphics
  print(graphicsObject)
  if("ggplot" %in% class(graphicsObject)){
    graphicsObject <- rvg::dml(ggobj=graphicsObject) ## convert the plot to the DrawingML object
  }
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout="Blank", master="Office Theme")
  doc <- officer::ph_with(doc, value=graphicsObject, location=officer::ph_location(left=1, top=1, width=width, height=height))
  print(doc, target=out)
  grDevices::dev.off()

  # Print the output path
  print(normalizePath(out))

  # Done
  return(invisible(NULL))
}


