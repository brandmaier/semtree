containsHeywoodCases <-
function(sem)
{

  
#message("checking for Heywood cases.")

if (!(inherits(sem,"MxRAMModel")|| inherits(sem,"lavaan"))) {
  warning("Checking Heywood cases was selected but impossible for Non-RAM/lavaan models.");
  return(FALSE);
}

if (is.null(sem) || is.null(sem@matrices) || is.null(sem@matrices$S))
{
  warning("An error occured when accessing S matrix in RAM model. Heywood checking impossible.")
  return(FALSE);
}

# get the values of the symmetric covariance matrix
if(inherits(sem,"MxRAMModel")){
  covarianceMatrix <- sem@matrices$S@values	
}
else {
  return(any(sem@Fit@est[lavaan::parTable(sem)$lhs==lavaan::parTable(sem)$rhs] < 0))
}
	
# check whether all entries on diagonal are positive
for (i in 1:dim(covarianceMatrix)[1]) {
	if (covarianceMatrix[i,i] < 0) {
		return(TRUE);
	}
}

return(FALSE);

}
