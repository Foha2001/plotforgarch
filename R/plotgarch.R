#' package to plot Garch model
#'
#' This package is used to plot series using Garch(1,1) model
#'
#' @param x
#'
#' @return plot garch(1,1)
#' @export
#'
#' @examples
#' plotgarch(x)
#'
plotgarch <- function(x){

  alpha <- 0.1
  beta <- 0.8
  omega <- var(R_data$GSPTSE.Close)*(1-alpha-beta)
  e <- R_data$GSPTSE.Close-mean(R_data$GSPTSE.Close)
  e2 <- e^2
  nobs <- length(R_data$GSPTSE.Close)
  predvar <- rep(NA,nobs)
  predvar[1] <- var(R_data$GSPTSE.Close)
  for (t in 2:nobs) {
    predvar[t] <- omega + alpha * e2[t-1] +beta * predvar[t-1]
  }
  predvol <- sqrt(predvar)
  predvol <- xts(predvol, order.by = index(R_data))
  #plot(predvol, type='l', main=" Litecoin Volatility")
  colnames(predvol)<- c("value")
  library(scales)
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(tibble)
  predvol %>%
    as.data.frame() %>%
    rownames_to_column("Date") %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    ggplot(aes(Date, value)) +
    geom_line() +
    scale_x_date(
      date_breaks = "1 month",
      labels = date_format("%b\n%Y")) +
    theme_minimal() + labs(title = "Canada index Volatility",
                           subtitle = "Garch(1,1)")






}

