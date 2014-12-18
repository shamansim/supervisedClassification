#' sc.holdOut
#'
#' Cette fonction opère un hold-out
#' @param 
#' f fonction de prediction
#' d_train jeu d'apprentissage
#' d_test jeu de test
#' pred(optionnal, default=TRUE) precise si l'on souhaite des prédictions ou des scores
#' @keywords error
#' @export
#' @examples
#' sc.holdOut(sc.mediatorHyperplane,d_train,d_test)

sc.holdOut <- function(f, d_train, d_test, pred = TRUE) {
    if(pred) f(d_train)$pred(d_test)
    else f(d_train)$f(d_test)
}
