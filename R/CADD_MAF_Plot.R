#' CADD-MAF plot.
#'
#' @param df_popviz A data.frame which has the following columns: HOM, AF, CADD, MutType, and optionally, MutLabel
#' @param df_private (Optional) A data.frame which has the following columns: HOM, CADD, MutType, and MutLabel
#' @param selectedMutTypes A named characters that will be internally used to define the factor levels.
#' @param homOnly Logical. Whether only homozygous variants (HOM>=1) should be plotted.
#' @param color_by A string indicating the variable name to be used for coloring.
#' @param shape_by A string indicating the variable name to be used for formatting the shape of the points.
#' @param facet_by A string or a pair of strings indicating the column names of variables to be used for facetting.
#' @param msc Minimal significance cutoff. Can be identified using PopViz[http://shiva.rockefeller.edu/PopViz/].
#' @param minAF The minimal allele frequency. Private variants will be plotted based on this number.
#' @examples
#' \dontrun{
#' df_popviz <- data.table::fread("PopViz_Table_PDCD1_MSC99_AR.txt", check.names=T) %>%
#'   dplyr::mutate(MutType=CONSEQUENCE, MutLabel=NA_character_)
#' df_private <- readr::read_csv("JL5657.V4.2.csv") %>%
#'   dplyr::filter(gene=="PDCD1"&ID=="rs373081859") %>%
#'   dplyr::transmute(HOM=1, AF=NA, CADD=CADD_Phred_V1.3, MutType="frameshift", MutLabel="T36Hfs*70")
#' plt <- CADD_MAF_Plot(
#'   df_popviz,
#'   df_private,
#'   selectedMutTypes=c(
#'    "Frameshift"="frameshift",
#'    "Inframe"="missense",
#'    "Inframe"="inframe_deletion"
#'   ),
#'   homOnly=F,
#'   color_by="MutType",
#'   facet_by="HomType",
#'   msc=3.3313,
#'   minAF=10^-6
#' ) +
#'   scale_color_manual(values=c("firebrick3")) +
#'   scale_fill_manual(values=c("firebrick3","Medium Turquoise","dimgrey"))
#' }
#' @export
#' @rdname CADD_MAF_Plot
#' @name CADD_MAF_Plot
CADD_MAF_Plot <- function(
  df_popviz,
  df_private=NULL,
  selectedMutTypes=c(
    "Frameshift"="frameshift",
    "Inframe"="missense",
    "Inframe"="inframe_deletion"
  ),
  homOnly=F,
  color_by="MutType",
  shape_by=NULL,
  facet_by=NULL,
  msc=3.33,
  minAF=10^-6
){
  # Format population genetics data
  if(!"MutLabel" %in% colnames(df_popviz)){
    df_popviz <- dplyr::mutate(df_popviz, MutLabel=NA)
  }
  df <- df_popviz

  # Merge private mutation data
  if(!is.null(df_private)){
    df <- dplyr::bind_rows(df, df_private) %>%
      dplyr::select(HOM, AF, CADD, MutType, MutLabel)
  }

  # Filter non-homozygous variants
  if(homOnly==T){
    df <- dplyr::filter(df, HOM>=1)
  }

  # Format data for the plot
  df <- df %>%
    dplyr::mutate(AF=dplyr::if_else(is.na(AF), minAF, AF)) %>%
    dplyr::mutate(AF=dplyr::if_else(AF<minAF, minAF, AF)) %>%
    dplyr::filter(MutType %in% as.character(selectedMutTypes)) %>%
    dplyr::mutate(MutType=factor(MutType, levels=as.character(selectedMutTypes), labels=names(selectedMutTypes))) %>%
    dplyr::mutate(HomType=factor(dplyr::if_else(HOM==0, "HOM=0", "HOM>=1"), levels=c("HOM=0", "HOM>=1"), labels=c("Heterozygous","Homozygous"))) %>%
    dplyr::arrange(dplyr::desc(MutType))

  # Prepare labels for the plot
  df_label <- df %>%
    dplyr::filter(!is.na(MutLabel))

  # Plot
  plt <- ggplot(data=NULL)
  if(!is.null(shape_by)){
    plt <- plt + geom_point(
      data=df,
      aes_string(x="AF", y="CADD", fill=color_by, shape=shape_by),
      alpha=0.75, size=2.5
    ) +
      guides(fill=guide_legend(override.aes=list(shape=21)))
  }else{
    plt <- plt + geom_point(
      data=df,
      aes_string(x="AF", y="CADD", fill=color_by),
      shape=21, alpha=0.75, size=2.5
    )
  }
  plt <- plt +
    ggrepel::geom_label_repel(
      data=df_label,
      aes_string(x="AF", y="CADD", color="MutLabel", label="MutLabel"),
      size=4, nudge_x=2, nudge_y=5, segment.size=0.2, show.legend=F
    ) +
    annotate("text", x=0.1, y=msc+1, label=paste0("MSC=", formatC(msc, digits=2))) +
    geom_hline(yintercept=msc, linetype="dashed", color="black") +
    geom_vline(xintercept=minAF, linetype="dashed", color="black") +
    scale_x_log10(name="MAF",
                  limits=c(minAF, 1),
                  breaks=scales::trans_breaks("log10", function(x) 10^x),
                  labels=scales::trans_format(trans="log10", format=scales::math_format(10^.x))) +
    ylab("CADD score") +
    ggpubr::theme_pubr(14) +
    theme(legend.spacing.x=unit(0.15, 'cm'),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=14)) +
    ggpubr::rremove("legend.title")
  if(!is.null(facet_by)){
    if(length(facet_by)==1){
      f <- as.formula(paste0("~", facet_by))
      plt <- plt + lemon::facet_rep_wrap(f, repeat.tick.labels=F)
    }else if(length(facet_by)==2){
      f <- as.formula(paste0(facet_by[1], "~", facet_by[2]))
      plt <- plt + lemon::facet_rep_grid(f, repeat.tick.labels=F)
    }else{
      stop("facet_by must be either a string or a pair of strings that matches the column names of the input dataframe.")
    }
  }
  return(plt)
}


