#' sc.accuracy
#'
#' Cette fonction calcule l'accuracy à partir d'une matrice de confusion
#' @param 
#' M matrice de confusion
#' @keywords accuracy
#' @export
#' @examples
#' sc.accuracy(matriceDeConfusion)

sc.accuracy <- function(M) {
    (M[2,2] + M[1,1]) / sum(M)
}

#' sc.sensitivity
#'
#' Cette fonction calcule la sensibilité à partir d'une matrice de confusion
#' @param 
#' M matrice de confusion
#' @keywords sensitivity
#' @export
#' @examples
#' sc.sensitivity(matriceDeConfusion)

sc.sensitivity <- function(M) {
    M[2,2] / sum(M[,2])
}

#' sc.specificity
#'
#' Cette fonction calcule la spécificité à partir d'une matrice de confusion
#' @param 
#' M matrice de confusion
#' @keywords specificity
#' @export
#' @examples
#' sc.specificity(matriceDeConfusion)

sc.specificity <- function(M) {
    M[1,1] / sum(M[,1])
}

#' sc.precision
#'
#' Cette fonction calcule la précision à partir d'une matrice de confusion
#' @param 
#' M matrice de confusion
#' @keywords precision
#' @export
#' @examples
#' sc.precision(matriceDeConfusion)

sc.precision <- function(M) {
    M[2,2] / sum(M[2,])
}

#' sc.erreur
#'
#' Cette fonction calcule le taux d'erreur à partir d'une matrice de confusion
#' @param 
#' M matrice de confusion
#' @keywords error
#' @export
#' @examples
#' sc.erreur(matriceDeConfusion)

sc.erreur <- function(M) {
	(M[1,2] + M[2,1]) / sum(rowSums(M))
}

#' sc.evaluation
#'
#' Cette fonction renvoie une liste de l'accuracy, la sensibilité, la spécificité, la précision et le taux d'erreur calculés à partir de prédictions
#' @param 
#' preds prédictions
#' labels réalité
#' @keywords confusion matrix error precision specificity sensitivity accuracy
#' @export
#' @examples
#' sc.evaluation(predictions,labels)

sc.evaluation <- function(preds,labels) {
    confusion_matrix <- table(preds, labels)
    list(
        accuracy = sc.accuracy(confusion_matrix),
        sensitivity = sc.sensitivity(confusion_matrix),
        specificity = sc.specificity(confusion_matrix),
        precision = sc.precision(confusion_matrix),
        error = sc.erreur(confusion_matrix)
    )
}
