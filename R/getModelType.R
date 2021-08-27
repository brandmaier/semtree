getModelType <- function(model)
{
  if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
    return("OpenMx")
    control$sem.prog = "OpenMx"
  } else if (inherits(model,"lavaan")){
    return("lavaan")
  } else if ((inherits(model,"ctsemFit")) || (inherits(model,"ctsemInit"))) {
    return("ctsem")
  } else {
    ui_stop("Unknown model type selected. Use OpenMx or lavaanified lavaan models!");
  }
}