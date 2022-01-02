#' Plot signatures and their 95\% credible intervals
#'
#' @param retval an object return from \code{\link{extract_ccc_from_hdp}}.
#' @param col Either a single colour for all data categories, or a vector of
#'  colours for each group (in the same order as the levels of the grouping factor)
#' @param cred_int Logical - should 95\% credibility intervals be plotted? (default TRUE)
#' @param weights (Optional) Weights over the data categories to adjust their
#'  relative contribution (multiplicative)

#' @param group_label_height Multiplicative factor from top of plot for group label placement
#' @param cat_names names displayed on x-axis, e.g. SBS96 mutation classes
#' @param cex.cat Expansion factor for the (optional) cat_names
#'
#' @export
#'
plot_component_with_credint <-
  function(retval,cat_names=NULL,
           col="grey70",
           cred_int=TRUE,
           weights=NULL,
           group_label_height=1.05, cex.cat=0.7){

    # input checks
    ccc_mean_df <- do.call(cbind,lapply(retval,function(x)x[["ccc_mean"]]))
    ccc_credint <- lapply(retval,function(x)x[["ccc_credint"]])

    ncat <- nrow(ccc_mean_df)

    comp_distn <- ccc_mean_df

    if(class(cred_int) != "logical") stop("cred_int must be TRUE or FALSE")
    if(!class(weights) %in% c("numeric", "NULL") |
       !length(weights) %in% c(ncat, 0)) {
      stop("weights must be a numeric vector with one value for every
         data category, or NULL")
    }

    # which components to plot
    comp_to_plot <- colnames(ccc_mean_df)
    cat_cols <- rep(col, ncat)

    # main titles
    plot_title <- paste("Signature", comp_to_plot)

    names(plot_title) <- comp_to_plot

    for (ii in seq_along(comp_to_plot)){

      cname <- comp_to_plot[ii]

      # mean categorical distribution (sig), and credibility interval
      sig <- ccc_mean_df[,ii]
      ci <- ccc_credint[[ii]]

      # adjust categories by weights if specified (lose cred intervals though)
      if(!is.null(weights)){
        sig <- sig %*% diag(weights)
        denom <- sum(sig)
        sig <- sig/denom
        ci <- NULL # not sure how to get cred int if adjusting with weights
      }

      sig <- as.vector(sig)

      # set categories whose credibility intervals hit zero to a different colour
      cat_cols_copy <- cat_cols

      # max plotting height
      ci[is.na(ci)] <- 0
      sig[is.na(sig)] <- 0
      if(max(ci)==0){
        plottop <- max(sig)+0.05
      }else{
        plottop <- ceiling(max(ci)/0.1)*0.1+0.01
      }

      # main barplot
      b <- barplot(sig, col=cat_cols_copy, xaxt="n", ylim=c(0,plottop*1.1),
                   border=NA, names.arg=rep("", ncat), xpd=F, las=1,
                   main=plot_title[ii])

      # add credibility intervals
      if (cred_int & !is.null(ci)){
        segments(x0=b, y0=ci[1,], y1=ci[2,], col="grey30")
      }

      # add category names
      if (!is.null(cat_names)){
        mtext(cat_names, side=1, las=2, at=b, cex=cex.cat,
              family="mono", col=cat_cols)
      }
    }
  }



#' Plot the distribution of raw clusters highly similar as the component in posterior chains
#' @param components  A matrix that containing components with each row corresponding a category and each column
#'                    corresponding a component
#'
#' @param retval An object return from \code{\link{extract_ccc_from_hdp}}
#'
#' @export
plot_component_posterior_samples <- function(components,
                                             retval){
  for(i in 1:ncol(components)){
    chain <- exposures <- sequence <- NULL
    summary.cluster <- retval[[i]][["summary.chain.info"]]
    colnames(summary.cluster) <- c("seq","chain","sample","exposures")
    cluster.name <- colnames(components)[i]
    summary.cluster <- summary.cluster[summary.cluster$chain>0,]
    summary.cluster$chain <- as.factor(summary.cluster$chain)
    plot.1 <- ggplot2::ggplot(data=summary.cluster, ggplot2::aes(x=sample, y=chain, group=chain)) +
      ggplot2::geom_point(size=0.8)+ggplot2::ggtitle(paste0("Posterior samples with raw clusters of ",cluster.name)) + ggplot2::xlab("Posterior Sample") +  ggplot2::ylab("Chain")
    plot(plot.1)

    ##I removed this before submit the paper, because this is not informative.
   # plot.2 <- ggplot2::ggplot(data=summary.cluster, ggplot2::aes(x=sequence, y=exposures, group=chain,color=chain)) +
  #    ggplot2::geom_point()+ggplot2::ggtitle(paste0("exposures of ",cluster.name," in Gibbs sample"))+ ggplot2::xlab("Posterior.Sample") +  ggplot2::ylab("Exposure")
    #plot(plot.2)
  }
}

