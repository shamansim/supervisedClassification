#' sc.mediator.demo
#'
#' Cette fonction génère un graphique à partir d'un jeu de données et représente les centre d'inertie ainsi que l'hyperplan médiateur
#' @param d jeu de données
#' @keywords plot display dataset demo mediator hyperplane
#' @export
#' @examples
#' sc.mediator.demo(sc.generateSimpleDataset(100))

sc.mediator.demo <- function(d) {
    plot.dataset(d)

    c.pos <- t(colMeans(d[d$Y==1,c("V1","V2")]))
    c.neg <- t(colMeans(d[d$Y==-1,c("V1","V2")]))
    
    points(c.pos, pch=7, lwd=2, col="red")
    points(c.neg, pch=7,lwd=2,col="blue")

    w <- c.pos - c.neg
    b <- (sum(c.neg ^ 2) - sum(c.pos ^ 2)) / 2
    abline(b = - w[1] / w[2], a = - b / w[2])
}
