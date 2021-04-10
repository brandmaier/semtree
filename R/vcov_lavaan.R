setMethod(f = "vcov", signature = signature(object = "lavaan"),
          definition = function(object, ...) {
            if (object@Model@eq.constraints) {
              K <- eval(parse(text = "lavaan:::lav_constraints_R2K(object@Model)"))
              res <- solve(t(K) %*% lavaan::lavInspect(object, what = "information.expected") %*% K * N) 
            } else {
              res <- object@vcov$vcov
            }
            res
          })
