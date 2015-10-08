#' Tidying methods for mle objects from the stats4 package
#' 
#' These methods tidy the parameter estimates resulting
#' from an estimation of a distribution.
#' 
#' @param x An object of class "mle"
#' @param ... extra arguments (not used)
#'
#' @template boilerplate
#'
#' @name mle_tidiers
#'
#' @examples
#' 
#' set.seed(2015)
#' x <- rnorm(100, 5, 2)
#' 
#' library(MASS)
#' 
#' f <- function(lambda) -sum(stats::dnorm(x, mu = 0, sd = 1, log = TRUE))
#' fit0 <- mle(f, start = list(lambda = 5), nobs = NROW(y))
NULL


#' @rdname mle_tidiers
#' 
#' @return \code{tidy.fitdistr} returns one row for each parameter that
#' was estimated, with columns:
#'   \item{term}{The term that was estimated}
#'   \item{estimate}{Estimated value}
#'   \item{std.error}{Standard error of estimate}
#' 
#' @export
tidy.mle <- function(x, conf.int = FALSE, ...) {
    coefs <- stats4::coef(x)
    ret <- data.frame(term = names(coefs),
                      estimate = unname(coefs),
                      std.error = sqrt(diag(x@vcov)))
    
    if (conf.int) {
        
    }
    ret
}


#' @rdname mle_tidiers
#' 
#' @return \code{glance.mle} returns a one-row data.frame with the columns
#'   \item{n}{Number of observations used in estimation}
#'   \item{logLik}{log-likelihood of estimated data}
#'   \item{AIC}{Akaike Information Criterion}
#'   \item{BIC}{Bayesian Information Criterion}
#' 
#' @export
glance.mle <- function(x, ...) {
    finish_glance(data.frame(n = x@nobs), x)
}
