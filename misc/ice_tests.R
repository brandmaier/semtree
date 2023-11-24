ice.semforest_stripped <- function(x, data, reference.var, 
                                   support = 20, points = NULL, mc = NULL, FUN = "median", ...){
  #browser()
  cl <- match.call()
  cl <- cl[c(1L, which(names(cl) %in% c("data", "reference.var", "support", "points", "mc")
  ))]
  cl[[1L]] <- str2lang("semtree:::partialDependence_data")
  mp <- eval.parent(cl)
  preds <- data.table::data.table(predict(x, data = mp, type = "pars"))
  mp[,names(mp)[-which(names(mp) %in% c(reference.var, colnames(preds)))]:=NULL]
  mp <- cbind(mp, preds)
  mp$ID <- rep(1:nrow(data), each=support)
  return(mp)
}

set.seed(325)
N <- 200
pred1 <- factor(sample(c("red","green","blue"),N,replace=TRUE))
pred2 <- ordered(sample(c(0,1,2),N,replace=TRUE))
pred3 <- as.numeric(sample(1:20,size = N,replace=TRUE))
noisy <- rnorm(N)
noisy2 <- rnorm(N)
noisy3 <- rnorm(N)

x <- rnorm(N)
#x <- rnorm(N)+ifelse(pred2=="1",10,0)
x <- x + pred3/10
df <- data.frame(x, pred3,noisy,noisy2,noisy3)

model = "x ~~ var*x; x~ mu*0"
fitted_model <- lavaan(model, df)
forst = semforest(fitted_model, df,
                  control=semforest.control(control=semtree.control(method="score",
                                                                    verbose=FALSE,report.level=99,alpha = 1),num.trees = 100))



# should be: about x=1 for pred3=.1 and x=20 for pred3=2

pd = partialDependence(forst, reference.var="pred3")
plot(pd, parameter="mu")

mp=ice.semforest_stripped(forest, forest$data, reference.var="pred3")
mp

ggplot(mp, aes(x=pred3,y=mu, group=factor(ID)))+geom_line()


plot(pd,parameter="mu")
