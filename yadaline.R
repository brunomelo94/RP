yadaline <- function(X, w, eta) {
  return(eta * (cbind(1, X) %*% w))
}

yPerceptron <- function(X, w, eta) {
  return( sign(eta * (cbind(1, X) %*% w)) )
}