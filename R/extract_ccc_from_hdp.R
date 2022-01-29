#'Find the ccc and cdc that matched to a spectrum in ccc_0 and cdc_0
#'this function is to summarize the credint and mean of cccs and cdcs
#'and for further diagnostic plotting.
#'
#'@param signature A numerical vector representing the signature for which
#'  want to find information in \code{ccc_0}.
#'
#'@param ccc_0 "CCCs" from sample chains. A list of lists of numerical
#'   matrices. At the top level, one list for each Gibbs sample chain.
#'   Each element of a top-level list is a numerical matrix, with columns
#'   for raw clusters and rows being mutation types. The number of
#'   rows in these matrices should be the length of \code{signature}.
#'
#'@param cos.merge The minimum cosine similarity for declaring a match
#' between \code{signature} and a column of one of the matrices in
#' in \code{ccc_0}. This should probably be the same as the cuttoff
#' for separating clusters of mutations after divisive clustering
#' \code{\link{extract_components}}.
#'
#' @return Invisibly, a list with at least the elements
#'
#' - \code{ccc_mean}
#'
#' - \code{ccc_credint}
#'
#'@export
extract_ccc_from_hdp <- function(signature,
                                 ccc_0,
                                 cos.merge = 0.90) {

  spectrum.ccc <- data.frame(matrix(nrow=nrow(data.frame(ccc_0[[1]][[1]])),ncol=0))

  summary.chain.info <- data.frame(matrix(ncol=3,nrow=0))

  for(chain in 1:length(ccc_0)){

    temp.chain <- data.frame(matrix(nrow=length(ccc_0[[chain]]),ncol=3))
    temp.chain[,1] <- chain
    temp.chain[,2] <-  temp.chain[,4] <- 0
    temp.chain[,3] <- c(1:nrow(temp.chain))

    for(sample in 1:length(ccc_0[[chain]])){

      ccc_0_temp <- ccc_0[[chain]][[sample]]

      if(!is.null(ncol(ccc_0_temp))){
        cos.sims <- apply(ccc_0_temp,2,function(x){lsa::cosine(x,signature)})
        if(sum(cos.sims>cos.merge)>0){
          spectrum.ccc <- cbind(spectrum.ccc,ccc_0_temp[,cos.sims>cos.merge])
          temp.chain[sample,2] <- chain
          temp.chain[sample,4] <- sum(colSums(ccc_0_temp[,cos.sims>cos.merge,drop=F]))

        }
      }else{
        spectrum.ccc <- cbind(spectrum.ccc,data.frame(ccc_0_temp))
        temp.chain[sample,2] <- chain
        temp.chain[sample,4] <- sum(colSums(data.frame(ccc_0_temp)))
      }

    }
    summary.chain.info <- rbind(summary.chain.info,temp.chain)

  }

  ccc_norm <- apply(spectrum.ccc, 2,function(x) x/sum(x, na.rm=TRUE))

  ccc_mean <- rowMeans(ccc_norm)

  ccc_credint <- apply(ccc_norm, 1, function(y) {
    samp <- coda::as.mcmc(y)
    if (min(sum(!is.na(samp)), sum(!is.nan(samp))) %in% c(0,1)) {
      c(NaN, NaN)
    } else {
      # Get the highest posterior density with a target probability
      # contenxt of 95%
      round(coda::HPDinterval(samp, 0.95), 4)
    }
  })

  return(invisible(list(spectrum.ccc       = spectrum.ccc,
                        ccc_mean           = ccc_mean,
                        ccc_credint        = ccc_credint,
                        summary.chain.info = summary.chain.info)))
}
