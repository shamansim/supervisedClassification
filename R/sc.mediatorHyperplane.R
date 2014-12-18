#' sc.mediatorHyperplane
#'
#' Cette fonction produit une liste contenant les vecteurs w et b ainsi que deux fonctions de prédiction nommées pred et f prenant en entrée un ensemble de vecteurs. 
#' @param d jeu de données
#' @keywords mediator hyperplane pred f w b 
#' @export
#' @examples
#' sc.mediatorHyperplane(sc.generateDifficultDatasetAlt(100,30))

sc.mediatorHyperplane <- function(d) {
    c.pos <- t(colMeans(d[d$Y==1, colnames(d) != "Y"]))
    c.neg <- t(colMeans(d[d$Y==-1,colnames(d) != "Y"]))

    w <- c.pos - c.neg
    b <- (sum(c.neg ^ 2) - sum(c.pos ^ 2)) / 2
    f <- function(x) {
        as.matrix(x[,colnames(d) != "Y"]) %*% t(w) + b # alternativement : utiliser apply
        # On prévient la possibilité que x contienne
        # une colonne Y, ce qui est le cas dans les tests que nous
        # faisons plus bas
    }
    pred <- function(x) {
        y <- f(x) >= 0
        y - !y
    }
    list(w = w, b = b, f = f, pred = pred)
}
