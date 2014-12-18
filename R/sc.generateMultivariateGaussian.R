#' sc.generateMultivariateGaussian
#'
#' Cette fonction génère des vecteurs dans R² selon une loi normale multivariée
#' @param n nombre de vecteurs
#' @keywords dataset generation multivariate
#' @export
#' @examples
#' sc.generateMultivariateGaussian(100)

sc.generateMultivariateGaussian <- function(n) {
    A <- matrix(c(2,1,1,0),nrow=2,ncol=2)
    Z <- matrix(c(rnorm(2 * n)),ncol=2)
    mu <- c(1,1)
    Z %*% A + mu
}
