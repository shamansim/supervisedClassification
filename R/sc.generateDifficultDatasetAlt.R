#' sc.generateDifficultDatasetAlt
#'
#' Cette fonction génère un jeu de donnée 'Difficult' en plusieurs dimensions
#' @param 
#' d nombre de dimensions
#' n taille du jeu de données
#' @keywords dataset generation dimensions
#' @export
#' @examples
#' sc.generateDifficultDatasetAlt(100,30)

sc.generateDifficultDatasetAlt <- function(d,n) {
    set.seed(42)
    w <- 2 * runif(d) - 1
    w <- w / sqrt(sum(w^2))
    points <- sapply(1:n,function(i) 4 * runif(d) - 2)
    p <- 1 / (1 + exp(0.5 * (t(points) %*% w)))
    Y <- Vectorize(function(p) rbinom(1,1,p))(p)
    cbind(as.data.frame(t(points)), data.frame(Y = 2*Y - 1))
}
