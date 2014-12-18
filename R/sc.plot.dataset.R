#' sc.plot.dataset
#'
#' Cette fonction génère un graphique à partir d'un jeu de données
#' @param d jeu de données
#' @keywords plot display dataset
#' @export
#' @examples
#' sc.plot.dataset(sc.generateSimpleDataset(100))

sc.plot.dataset <- function(d) {
    lim <- c(min(d$V1,d$V2), max(d$V1,d$V2))
    plot(c(), c(),
         xlim=lim, # même échelle en abscisses et ordonnées
         ylim=lim,
         xlab=expression(x[1]),
         ylab=expression(x[2]))
    
    points(d[d$Y==1,c("V1","V2")],col='red',lw=2,pch="*")
    points(d[d$Y==-1,c("V1","V2")],col='blue',lw=2,pch="o")
}
