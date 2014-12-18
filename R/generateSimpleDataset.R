#' generateSimpleDataset
#'
#' Cette fonction génère un jeu de donnée simple
#' @param n taille du jeu de données
#' @keywords dataset generation
#' @export
#' @examples
#' generateSimpleDataset(100)

generateSimpleDataset <- function(n) {
    np <- ceiling(n/2)
    nn <- floor(n/2)
    pos <- mvrnorm(np,mu=c(0,0),Sigma=matrix(c(1,0.5,0.5,1),2,2))
    neg <- mvrnorm(nn,mu=c(6,2),Sigma=matrix(c(0.2,0.4,0.4,4),2,2))
    x <- as.data.frame(rbind(pos,neg))
    cbind(x, Y = c(rep(1,np),rep(-1,nn)))
}
