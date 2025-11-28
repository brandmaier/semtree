N <- 100
Data = data.frame(x= rnorm(N),y=rnorm(N),z=rnorm(N),pred=sample(c(0,1),N,TRUE))
lav_obj <- 'y ~ x +z + x:z'
run_lav_obj <- sem(model = lav_obj , data = Data)
summary(run_lav_obj)

tree <- semtree(model=run_lav_obj , data=Data,
                verbose = T, predictors=c('pred'))
