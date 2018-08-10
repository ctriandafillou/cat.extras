#' Fit a mixture of two Gaussians to groups of data
#'
#' @param df Dataframe or dataframe subset with a column 'FSC.W' containing data to be fit
#' @param ouput Options are "classification", "posterior", "full"; default is "classification"
#' @export
#' @return a classified dataset ("classification", default), a dataset with the posteriors added as columns ("posterior"), or a dataset with posteriors and information about the mixtures ("full")

fit_mm_size <- function(df, output = "classification", conf = 0.5) {
  mdl <- normalmixEM(df$FSC.W, 2)
  df <- tibble::rowid_to_column(df, "ID") # Add an ID column to prevent creation of duplicates in downstream dataframe joining.

  if (output == "full"){
    df.w.posterior <- data.frame(FSC.W = mdl$x, mdl$posterior, ID = df$ID) %>%
      mutate(lambda.1 = mdl$lambda[1],
             lambda.2 = mdl$lambda[2],
             mu.1 = mdl$mu[1],
             mu.2 = mdl$mu[2],
             sigma.1 = mdl$sigma[1],
             sigma.2 = mdl$sigma[2], loglik = mdl$loglik) %>%
      full_join(., df, by = c("FSC.W", "ID"))

  } else if (output == "posterior") {
    df.w.posterior <- data.frame(FSC.W = mdl$x, mdl$posterior, ID = df$ID) %>%
      full_join(., df, by = c("FSC.W", "ID"))

  } else if (output == "classification") {
    if (mdl$mu[1] > mdl$mu[2]) {
      #print(paste(mdl$mu[1], mdl$mu[2], "one greater than 2", sep = " "))
      df.w.posterior <- data.frame(FSC.W = mdl$x, mdl$posterior, ID = df$ID) %>%
        full_join(., df, by = c("FSC.W", "ID")) %>%
        mutate(classification = factor(ifelse(comp.1 > conf, "high",
                                              ifelse(comp.2 > conf, "low", "amb")),
                                       levels = c("amb", "low", "high")),
               comp.high = comp.1,
               comp.low = comp.2)

      } else {
        #print(paste(mdl$mu[1], mdl$mu[2], "two greater than 1", sep = " "))
        df.w.posterior <- data.frame(FSC.W = mdl$x, mdl$posterior, ID = df$ID) %>%
        full_join(., df, by = c("FSC.W", "ID")) %>%
        mutate(classification = factor(ifelse(comp.2 > conf, "high",
                                              ifelse(comp.1 > conf, "low", "amb")),
                                      levels = c("amb", "low", "high")),
               comp.high = comp.2,
               comp.low = comp.1)
     }
  } else if (output == "model") {
    #reduced.df <- select(df)
    df.w.posterior <- data.frame(lambda.1 = mdl$lambda[1],
                                 lambda.2 = mdl$lambda[2],
                                 mu.1 = mdl$mu[1],
                                 mu.2 = mdl$mu[2],
                                 sigma.1 = mdl$sigma[1],
                                 sigma.2 = mdl$sigma[2], loglik = mdl$loglik)
    #bind_cols(reduced.df, df.w.posterior)
  } else {
    stop("output must be 'posterior', 'classification', 'model', or 'full'")
  }
}

