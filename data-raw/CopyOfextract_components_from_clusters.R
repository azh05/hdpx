########################################################################
# Merge nearly identical clusters in one posterior chain.
# This will speed up subsequent hierarchical clustering.

cosmergechain <- function(ccc, cdc, threshold = 0.99){

  ccc_and_cdc <- first_merge(ccc, cdc, threshold)
  ccc_unlist <- ccc_and_cdc$ccc
  cdc_unlist <- ccc_and_cdc$cdc
  clust_label <- ccc_and_cdc$clust_label

  stats <- data.frame(table(clust_label))

  new.spectrum   <- ccc_unlist[,which(stats$Freq>1)]
  new.cdc        <- cdc_unlist[,which(stats$Freq>1)]
  new.stats      <- stats[which(stats$Freq>1),]
  noise.spectrum <- noise.cdc <- noise.stats <- NULL

  if(sum(stats$Freq==1)>0) {
    # These are ccc's (clusters of category counts, e.g. raw clusters or
    # partially aggregated raw clusters) that are dissimilar from
    # all other ccc's in this chain. For realistic length Gibbs sampling
    # chains this means there is no confidence in them in the posterior
    # distribution. We combine these into
    noise.spectrum <- rowSums(ccc_unlist[,which(stats$Freq==1),drop=F])
    noise.cdc <- rowSums(cdc_unlist[ ,stats$Freq==1, drop=F])
    noise.stats <- sum(stats$Freq==1)
  }

  return(list(spectrum       = new.spectrum, # Partially aggregated raw clusters, column names are integers in the ... rows are channels
              spectrum_cdc   = new.cdc,      # Corresponding exposures column names are the same as in spectrum, rows are
              spectrum_stats = new.stats,    # Mapping from clust_label to the number of
                                             # Gibbs samples that contributed to that clust_label
              noise.spectrum = noise.spectrum, # Spectrum from "singleton" ccc's
              noise.cdc      = noise.cdc,      # Corresponding exposures
              noise.stats    = noise.stats     # The total number of raw clusers (singletoc ccc's)
  ))

}


########################################################################
# Merge essentially identical clusters (cosine similarity > 0.99)
# within 1 posterior Gibbs sample.

first_merge <- function(ccc, cdc, threshold = 0.99) {
  ccc <- as.matrix(ccc)
  cdc <- as.matrix(cdc)

  clust_label <- 1:ncol(ccc)
  clust_cos <- lsa::cosine(ccc)
  clust_same <- (clust_cos > threshold & lower.tri(clust_cos))
  same <- which(clust_same, arr.ind=TRUE) # merge these columns
  if (length(same)>0){
    for (index in 1:nrow(same)){
      clust_label[same[index, 1]] <- clust_label[same[index, 2]]
    }
  }

  ccc <- merge_cols(ccc,clust_label)
  cdc <- merge_cols(cdc,clust_label)
  return(list(ccc=ccc, cdc=cdc, clust_label = clust_label))
}


#' Combine "raw clusters" of mutations in "components" (clusters of mutations generated by one mutational process).
#'
#' @param x An \code{\link{hdpSampleChain-class}} or \
#' code{\link{hdpSampleMulti-class}} object
#'
#' @param hc.cutoff the height to cut hierarchical clustering tree
#'
#' @return A list with the elements \describe{
#' \item{components}{Aggregated clusters as a data frame.
#'   Rows represent the categories (i.e. for
#'   mutational signature analysis, the mutation type,
#'   e.g. ACT -> AGT). Columns are
#'   aggregated clusters, i.e. clusters after all
#'   "raw clusters" across all Gibbs samples have been
#'   combined according to the divisive clustering.
#'   Each cell contains number of items (for mutational
#'   signature analysis, the number of mutations) of
#'   a particular category in a particular aggregated
#'   cluster.}
#'
#' \item{components.post.samples}{A data frame with
#'   two columns: one is the index of column in
#'   \code{components} and
#'   the other is the number of posterior
#'   samples that contributed to that aggregated
#'   cluster (column in \code{components}).}
#'
#' \item{components.cdc}{A numerical matrix.
#'    Each row is a Dirichlet process (DP).
#'    This can either be a leaf DP, which
#'    for mutational signatures corresponds
#'    to a biological sample (for exampl, a tumor),
#'    or an parent or ancestor DP.
#'    Each column corresponds to the cluster
#'    in the corresponding column n \code{components}}
#'
#' \item{each.chain.noise.clusters}{Mo to document, called each.chain.noise.spectrum internally}
#'
#' \item{each.chain.noise.cdc}{A matrix.
#'    Each row is a dirichlet process and
#'    each column corresponds to
#'    the cluster in \code{each.chain.noise.clusters}}
#'
#' \item{multi.chains}{An \code{\link{hdpSampleChain-class}}
#'    or \code{\link{hdpSampleMulti-class}}
#'    object updated with component information.}
#'
#' \item{nsamp}{The total number of posterior samples across all
#'   Gibbs sampling chains.}
#' }
#'
#'
#' @seealso \code{\link{hdp_posterior}}, \code{\link{hdp_multi_chain}},
#'  \code{\link{plot_comp_size}}, \code{\link{plot_comp_distn}},
#'  \code{\link{plot_dp_comp_exposure}}
#'
#' @importFrom stats aggregate
#'
#' @export

extract_components_from_clusters <-  function(x, hc.cutoff = 0.1) {
  if (class(x)=="hdpSampleChain") {
    message('Extracting components on single chain. A hdpSampleMulti object is recommended, see ?hdp_multi_chain')
    is_multi <- FALSE
  } else if (class(x)=="hdpSampleMulti") {
    chlist <- x@chains
    is_multi <- TRUE
  } else if (is.list(x) &&
             all(unlist(lapply(x, class)) == "hdpSampleChain")) {
    is_multi <- TRUE
    chlist <- x
    x <- hdp_multi_chain(x)
  } else {
    stop("x must have class hdpSampleChain or hdpSampleMulti or be a list of hdpSampleChain")
  }

  if (is_multi) {
    # list of hdpSampleChain objects
    # chlist <- x@chains
    nch <- length(chlist)

    # set seed, get final state and number of posterior samples
    set.seed(sampling_seed(chlist[[1]]), kind="Mersenne-Twister", normal.kind="Inversion")
    finalstate <- final_hdpState(chlist[[1]])
    nsamp <- sum(sapply(chlist, function(x) hdp_settings(x)$n))

  }

  # number of categories, DPs,data items at each DP, and frozen priors
  ncat <- numcateg(finalstate) ##number of channel
  ndp <- numdp(finalstate) ##number of dp
  numdata <- sapply(dp(finalstate), numdata) #number of mutations in each sample
  pseudo <- pseudoDP(finalstate)
  rm(finalstate)

  is_prior <- length(pseudo) > 0
  if (is_prior) {
    priorcc <- 1:length(pseudo)
  }

  if(is_multi){
    ccc_0 <- lapply(chlist, function(ch){
      lapply(clust_categ_counts(ch), function(x){
        ans <- cbind(x)
        return(ans[, -ncol(ans)])
      })
    })

    cdc_0 <- lapply(chlist, function(ch){
      lapply(clust_dp_counts(ch), function(x){
        ans <- cbind(x)
        return(ans[, -ncol(ans)])
      })
    })

    # if priors, remove pseudo-counts from ccc_0
    if (is_prior){
      pseudodata <- sapply(dp(final_hdpState(chlist[[1]]))[pseudo],
                           function(x) table(factor(x@datass, levels=1:ncat)))

      ccc_0 <- lapply(ccc_0, function(y) lapply(y, function(x) {
        x[,priorcc] <- x[,priorcc] - pseudodata
        return(x)
      }))
    }
  }

  for(i in 1:nch) {
    test <- mapply(first_merge, ccc_0[[i]],cdc_0[[i]],
                   MoreArgs = list(threshold = 0.99), SIMPLIFY = FALSE)

    for(j in 1:length(ccc_0[[i]])){
      ccc_0[[i]][[j]] <- test[[j]]$ccc
      cdc_0[[i]][[j]] <- test[[j]]$cdc
    }
    # ccc_0[[i]] <- unlist(lapply(test, `$`, "ccc")) ?

    rm(test)
    ccc_0[[i]] <-  do.call(cbind,ccc_0[[i]])
    cdc_0[[i]] <- do.call(cbind,cdc_0[[i]])
  }

  summary <- mapply(cosmergechain,ccc_0,cdc_0,SIMPLIFY = F)

  dataframe <- each.chain.noise.spectrum <- data.frame(matrix(nrow=ncat,ncol=0))
  stats.dataframe <- data.frame(matrix(nrow=0,ncol=2))
  dp.dataframe <- each.chain.noise.cdc <- data.frame(matrix(nrow=ndp,ncol=0))

  # Combine CDC across all Gibbs samples
  for(i in 1:nch){
    dataframe <- cbind(dataframe,summary[[i]]$spectrum)
    dp.dataframe <- cbind(dp.dataframe,summary[[i]]$spectrum_cdc)
    stats.dataframe <- rbind(stats.dataframe,summary[[i]]$spectrum_stats)
    if(!is.null(summary[[i]]$noise.spectrum)){
      each.chain.noise.spectrum <- cbind(each.chain.noise.spectrum,summary[[i]]$noise.spectrum)
      each.chain.noise.cdc <- cbind(each.chain.noise.cdc,summary[[i]]$noise.cdc)
    }

  }

  while(TRUE) {

    clust_cos <- cosCpp(as.matrix(dataframe))
    clust_label <- c(1:ncol(dataframe))
    # colnames(dataframe) <- c(1:ncol(dataframe))
    # colnames(dp.dataframe) <- c(1:ncol(dataframe))
    clust_same <- (clust_cos > 0.97 & lower.tri(clust_cos))

    # Again merge more-or-less identical clusters, with the aim of
    # speeding up the hierarchical clustering later
    same <- which(clust_same, arr.ind=TRUE) # merge these columns

    if (nrow(same) == 0) break

    for (i in 1:nrow(same)){
      clust_label[same[i, 1]] <- clust_label[same[i, 2]]
    }
    dataframe <- merge_cols(as.matrix(dataframe),clust_label)
    stats.dataframe <- aggregate(stats.dataframe[,2],by=list(clust_label),sum)
    dp.dataframe <- merge_cols(as.matrix(dp.dataframe),clust_label)

    if (FALSE) {
    clust_cos <- cosCpp(as.matrix(dataframe))
    clust_label <- c(1:ncol(dataframe))
    colnames(dataframe) <- c(1:ncol(dataframe))
    colnames(dp.dataframe) <- c(1:ncol(dataframe))
    clust_same <- (clust_cos > 0.97 & lower.tri(clust_cos))
    same <- which(clust_same, arr.ind=TRUE) # merge these columns
    }
  }


  #dataframe <- dataframe[,stats.dataframe$Freq>1]##exclude spectrum that only extracted in 1 post sample in a chain, too many noisy clusters affect the final extraction
  #stats.dataframe <- stats.dataframe[stats.dataframe$Freq>1,]

  dataframe.normed <- apply(dataframe,2,function(x)x/sum(x))
  cosine.dist.df <- parallelDist::parallelDist(t(dataframe.normed),method = "cosine")

  cosine.dist.hctree.diana <- cluster::diana(x = cosine.dist.df,diss = T)
  cosine.dist.hctree <- stats::as.hclust(cosine.dist.hctree.diana)

  # Find clusters composed of highly similar clusters
  clusters <- dendextend::cutree(cosine.dist.hctree,  h=hc.cutoff)
  spectrum.df <- merge_cols(as.matrix(dataframe),clusters)
  spectrum.stats <- aggregate(stats.dataframe[,2],by=list(clusters),sum)
  spectrum.cdc <- merge_cols(as.matrix(dp.dataframe),clusters)

  return(
    invisible(
      list(
        components = spectrum.df,
        components.post.samples = spectrum.stats,
        components.cdc = spectrum.cdc,
        each.chain.noise.cdc = each.chain.noise.cdc,
        each.chain.noise.clusters = each.chain.noise.spectrum,
        multi.chains = x,
        nsamp = nsamp)))
}
