#' package to plot Garch model
#'
#' This package is used to plot series using Garch(1,1) model
#'
#' @param x time series xts object
#'
#' @return plot garch(1,1)
#' @export
#' @importFrom zoo index
#' @importFrom xts xts
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom scales date_format

#' @examples
#' plotgarch(x)
#'
plotgarch <- function(x){

  alpha <- 0.1
  beta <- 0.8
  omega <- var(x)*(1-alpha-beta)
  e <- x-mean(x)
  e2 <- e^2
  nobs <- length(x)
  predvar <- rep(NA,nobs)
  predvar[1] <- var(x)
  for (t in 2:nobs) {
    predvar[t] <- omega + alpha * e2[t-1] +beta * predvar[t-1]
  }
  predvol <- sqrt(predvar)
  predvol <- xts::xts(predvol, order.by = index(x))
  colnames(predvol)<- c("value")
    predvol %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Date") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    ggplot2::ggplot(aes(Date, value)) +
    geom_line() +
    scale_x_date(
      date_breaks = "1 month",
      labels = scales::date_format("%b\n%Y")) +
    theme_minimal() + labs(title = "Canada index Volatility",
                           subtitle = "Garch(1,1)")


}

