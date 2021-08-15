xichi <- function(x) {

  param <- ghyp::coef(x, type = "alpha.delta")
  rho <- param[["beta"]] / param[["alpha"]]
  zeta <- param[["delta"]] * sqrt(param[["alpha"]] ^ 2 - param[["beta"]] ^ 2)
  xi <- 1 / sqrt(1 + zeta)
  chi <- xi * rho
  result <- c(chi, xi)
  names(result) <- c("chi", "xi")

  result

}
