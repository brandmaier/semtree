lav_lgcm_helper <- function( homoscedastic_errors = TRUE)
{
  obs <- paste0("o",1:5)
  
  if (homoscedastic_errors) {
  error_str <- paste0(obs, " ~~ e*",obs,";",collapse="\n")
  } else {
    error_str <- paste0(obs, " ~~ e",1:5,"*",obs,";",collapse="\n")   
  }
  
  lgcModelstr <- paste0('
g0 =~ 1*o1 + 1*o2 + 1*o3 + 1*o4 + 1*o5;
g1 =~ 0*o1 + 1*o2 + 2*o3 + 3*o4 + 4*o5;
g0 ~~ vari*g0; g0 ~ mui*1;
g1 ~~ vars*g1; g1 ~ mus*1;
g0 ~~ covis*g1;
o1 ~ 0*1;
o2 ~ 0*1;
o3 ~ 0*1;
o4 ~ 0*1;
o5 ~ 0*1;
', error_str)

 
return(lgcModelstr)
  
}


omx_lgcm_helper <- function(data = NULL, homoscedastic_errors = TRUE)
{
  dataSpec <- NULL
  if (!is.null(data)) {
    dataSpec <- mxData(data,type="raw")
    manifests <- names(data)[1:5]
  } else {
    manifests <- paste0("x",1:5)
  }
  
  if (!homoscedastic_errors) {
     error_labels <- c("residual1","residual2","residual3","residual4","residual5")
  } else {
    error_labels <- rep("residual", 5)
  }

  lgcModel <- mxModel("Linear Growth Curve Model Path Specification",
                      type="RAM",
                      manifestVars=manifests,
                      latentVars=c("intercept","slope"),
                      # residual variances
                      mxPath(
                        from=manifests,
                        arrows=2,
                        free=TRUE,
                        values = c(1, 1, 1, 1, 1),
                        labels=error_labels
                      ),
                      # latent variances and covariance
                      mxPath(
                        from=c("intercept","slope"),
                        connect="unique.pairs",
                        arrows=2,
                        free=TRUE,
                        values=c(1, 1, 1),
                        labels=c("vari", "cov", "vars")
                      ),
                      # intercept loadings
                      mxPath(
                        from="intercept",
                        to=manifests,
                        arrows=1,
                        free=FALSE,
                        values=c(1, 1, 1, 1, 1)
                      ),
                      # slope loadings
                      mxPath(
                        from="slope",
                        to=manifests,
                        arrows=1,
                        free=FALSE,
                        values=c(0, 1, 2, 3, 4)
                      ),
                      # manifest means
                      mxPath(
                        from="one",
                        to=manifests,
                        arrows=1,
                        free=FALSE,
                        values=c(0, 0, 0, 0, 0)
                      ),
                      # latent means
                      mxPath(
                        from="one",
                        to=c("intercept", "slope"),
                        arrows=1,
                        free=TRUE,
                        values=c(1, 1),
                        labels=c("meani", "means")
                      ),
                      dataSpec
  )
  
  return(lgcModel)
}