#' tidy method for logistf objects
#'
#' @param x model object
#' @param conf.int binary indicating whether to include confidence intervals
#' @param exponentiate binary indicating whether to exponentiate coefficients
#' @param ... Additional arguments. Not used.
#'
#' @note
#' Most tidy methods provide a conf.level argument to set the confidence level.
#' Supplying one to this function only produces a warning, as the confidence
#' level should be set in the logistf call, via the alpha argument.
#'
#' @return
#' @export
#'
#' @examples
#' data(sex2)
#' fit<-logistf(case ~ age+oc+vic+vicl+vis+dia, data=sex2)
#' tidy(fit)
tidy.logistf <- function(x,
                         conf.int = FALSE,
                         exponentiate = TRUE,
                         ...){

  dots <- list(...)
  if(length(dots) > 0){
    if("conf.level" %in% names(dots)){
      warning(paste("set conf.level in the logistf call (alpha argument)",
                    x$conflev, "used instead"))
    }
  }

  result <- data.frame(term = names(coef(x)),
                       estimate = coef(x),
                       std.error = vcov(x) |> diag() |> sqrt(),
                       statistic = qchisq(1 - x$prob, 1),
                       p.value = x$prob,
                       row.names = NULL)

  if(conf.int){
    result$conf.low <- x$ci.lower
    result$conf.high <- x$ci.upper
  }
  if(exponentiate){
    result$estimate <- exp(result$estimate)
    if(conf.int){
      result$conf.low <- exp(result$conf.low)
      result$conf.high <- exp(result$conf.high)
    }
  }

  result
}

#' @importFrom generics tidy
#' @export
generics::tidy
