# sudo apt-get install libmpfr-dev
# library(Rmpfr)

#' Single Parameter Fit
#'
#' Encodes a numeric vector into a single parameter
#'
#' @param data numeric vector
#' @param binary_precision binary precision
#'
#'
#' @examples
#' x <- rnorm(100)
#' fit <- sp_fit(x, binary_precision = 12)
#'
#' # Print the single parameter
#' print(fit)
#'
#' # Plot "observed" and "predicted"
#' plot(x)
#' points(predict(fit), col = "red")
#'
#' # "Extrapolate" to unseen data
#' plot(predict(fit, length(x) + 10), col = "red")
#' points(x)
#'
#' @export
sp_fit <- function(data, binary_precision = 12) {
  m <- min(data)
  M <- max(data)
# browser()
  data <- (data - m)/(M - m)

  denormalise <- function(data) {
    data*(M - m) + m
  }

  conjugate_initial_binary <- paste0(vapply(data, function(x) {
    decimal_to_binary(phi_inv(x), binary_precision = binary_precision)
  },
  "a"),
  collapse = "")


  necessary_precision <- nchar(conjugate_initial_binary)

  stopifnot(binary_precision * length(data) == necessary_precision)

  conjugateInitial <-  binary_to_decimal(conjugate_initial_binary, precision = necessary_precision)
  decimal_initial <- phi(conjugateInitial, precision = necessary_precision)

  fit <- list(decimal          = decimal_initial,
              binary = conjugate_initial_binary,
              denormalise      = denormalise,
              n                = length(data),
              precission       = necessary_precision,
              binary_precission = binary_precision)
  class(fit) <- c("superfit", class(decimal_initial))

  return(fit)
}


#' @export
predict.superfit <- function(object, n = object$n, ...) {
  decoded_values <- vapply(seq_len(n)-1, function(k) {
    as.numeric(logistic_decoder(object[["decimal"]], k, object[["binary_precission"]]))
  },
  0)

  object$denormalise(decoded_values)
}

#' @export
print.superfit <- function(x, ...) {
  max.digits <- getOption("Rmpfr.print.max.digits", Rmpfr::getPrec(x$decimal))
  print(Rmpfr::format(x$decimal, max.digits = max.digits))
}


#' Changes a single digit of a single parameter fit
#'
#' @param fit,original_fit,new_fit fit objects returned by [sp_fit()]
#' @param which which decimal digit to change.
#'
#' @examples
#' x <- rnorm(100)
#' fit <- sp_fit(x, binary_precision = 12)
#'
#' # Randomly change the 30th decimal
#' new_fit <- change_digit(fit, 30)
#'
#' # visualise the change (numbers in red)
#' sp_difference(fit, new_fit)
#'
#' # visualise the impact on "predictions".
#' plot(x)
#' points(predict(fit), col = "red")
#' points(predict(new_fit), col = "blue")
#'
#'
#' @export
change_digit <- function(fit, which = "random") {
  number <- Rmpfr::formatMpfr(fit$decimal)
# browser()
  if (which == "random") {
    which <- sample(seq_len(nchar(number))[-c(1, 2)], 1)
  } else {
    which <- which + 2
  }

  substr(number, which, which) <- as.character(floor(stats::runif(1, 0, 10)))
  fit$decimal <- Rmpfr::mpfr(number, Rmpfr::getPrec(fit$decimal))
  fit
}

#' @export
#' @rdname change_digit
sp_difference <- function(original_fit, new_fit) {
  x_chars <- strsplit(Rmpfr::formatMpfr(original_fit$decimal), "")[[1]]
  y_chars <- strsplit(Rmpfr::formatMpfr(new_fit$decimal), "")[[1]]

  dif <- original_fit$decimal - new_fit$decimal
  exponent <- as.numeric(floor(log10(abs(dif))))

  different <- which(!(x_chars == y_chars))
  x_chars[different] <- paste0("\033[31m", x_chars[different], "\033[39m")

  cat("Difference ~ 10^", exponent, "\n", sep = "")
  cat(x_chars, sep = "")
  return(invisible(dif))
}


# Engine --------------------------------------------------

dyadic_map <- function(x) ( 2*x ) %% 1


decimal_to_binary <- function(decimal_initial, binary_precision = 12L) {
  Reduce(function(acc, ignored) {
    list(dyadic_map(acc[[1]]),
         paste0(acc[[2]],
                ifelse(acc[[1]] < 0.5, "0", "1")))
  },
  0:(binary_precision-1),
  init = list(decimal_initial, ""))[[2]]
}



# power <- function(x, y, precision) exp(Rmpfr::mpfr(y, precision)*log(Rmpfr::mpfr(x, precision)))
# power <- function(x, y, precision) {
#   Rmpfr::mpfr(x, precision) ^ Rmpfr::mpfr(y, precision)
# }

# power <- function(x, y, precision) {
#   x ^ Rmpfr::mpfr(y, precision)
# }


power <- function(x, y, precision) {
  x ^y
}

# binary_to_decimal <- function(binary_initial, precision = 53) {
#   binary_initial <- floor(as.numeric(strsplit(binary_initial, split = "")[[1]]))
#   binary_initial <- Rmpfr::mpfr(binary_initial, precision)
#
#   two <- Rmpfr::mpfr(2, precision)
#   Reduce(function(acc, val) {
#     #Rmpfr::mpfr(acc, precision) + floor(Rmpfr::mpfr(binary_initial[val], precision)) / power(2, val, precision)
#     # Rmpfr::mpfr(acc, precision) + floor(as.numeric(binary_initial[val])) / power(2, val, precision)
#     acc + binary_initial[val] / power(two, val, precision)
#   },
#   seq_along(binary_initial),
#   init = 0.0)
# }



binary_to_decimal <- function(binary_initial, precision = 53) {
  binary_initial <- floor(as.numeric(strsplit(binary_initial, split = "")[[1]]))
  binary_initial <- Rmpfr::mpfr(binary_initial, precision)
  two <- Rmpfr::mpfr(2, precision)

  n <- binary_initial / power(two, seq_along(binary_initial), precision)
  sum(n)
}


phi_inv <- function(z) {
  base::asin(base::sqrt(z)) / (2.0 * pi)
}


phi <- function(theta, precision = 53) {
  Pi <- Rmpfr::Const("pi", precision)
  sin(theta * Pi * 2.0)^ 2
}


logistic_decoder <- function(decimal_initial, k, binary_precision = 12) {
  decimal_initial <- Rmpfr::mpfr(decimal_initial, Rmpfr::getPrec(decimal_initial))
  k <-  Rmpfr::mpfr(k, Rmpfr::getPrec(decimal_initial))
  two <- Rmpfr::mpfr(2, Rmpfr::getPrec(decimal_initial))

  a <- two^(k * binary_precision)
  b <- asin(sqrt(decimal_initial))
  c <- sin(a * b)
  c^ two
}



