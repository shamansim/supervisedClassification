#' sc.generateDifficultDataset
#'
#' Cette fonction génère un jeu de donnée 'Difficult'
#' @param n taille du jeu de données
#' @keywords dataset generation dimensions
#' @export
#' @examples
#' sc.generateDifficultDataset(100)

sc.generateDifficultDataset <- function(n) {
	library(MASS)
    np <- ceiling(n/2)
    nn <- floor(n/2)
    pos <- mvrnorm(np,mu=c(0,0),Sigma=matrix(c(4,2,2,4),2,2))
    neg <- mvrnorm(nn,mu=c(4,2),Sigma=matrix(c(0.2,-0.4,-0.4,4),2,2))
    x <- as.data.frame(rbind(pos,neg))
    cbind(x, Y = c(rep(1,np),rep(-1,nn)))
}
