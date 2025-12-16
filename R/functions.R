getFormulaMeasures <- function(f){
  wanted <- sapply(formula, function(x) x$name == f)
  bone.measures <- formula[[which(wanted)]]$measures
  return(bone.measures)
}
