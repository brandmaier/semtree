# forest <- readRDS("results/full_forest_all_2.RData")
# tree = forest$forest[[1]]
# parameters = c("meani", "means", "meanq")
clear_underbrush <- function(x, parameters){
  UseMethod("clear_underbrush", x)
}

clear_underbrush.semtree <- function(x, parameters){
  if(x$caption == "TERMINAL"){
    sums <- summary(x$model)
    return(list(parameters = sums$parameters$Estimate[match(parameters, sums$parameters$name)]))
  } else {
    return(list(rule = x$rule,
                left_child = clear_underbrush(x = x$left_child, parameters = parameters),
                right_child = clear_underbrush(x = x$right_child, parameters = parameters)))
  }  
}

clear_underbrush.semforest <- function(x, parameters = NULL){
  if(is.null(parameters)){
    sums <- summary(x$model)
    parameters <- sums$parameters$name
  }
  out <- lapply(forest$forest, clear_underbrush, parameters = parameters)
  attr(out, "parameters") <- parameters
  class(out) <- c("semforest_light", class(out))
  out
}
#clear_underbrush(tree, parameters)

traverse_light <- function(row, tree){
  if(!is.null(tree[["parameters"]])){
    return(tree[["parameters"]])
  } else {
    traverse_light(row = row,
                   tree = tree[c("left_child", "right_child")][[(do.call(tree$rule$relation, list(row[tree$rule$name], tree$rule$value))+1)]])
  }
}


predict_pars <- function(forest, data, parameters){
  UseMethod("predict_pars", forest)
}
predict_pars.semforest_light <- function(forest, data, parameters){
  t(apply(data, 1, function(r){
    apply(sapply(forest, function(t){
      traverse_light(r, t)
    }), 1, median)
  }))
}
shape_fun <- function(data){
  apply(data, 1, function(i){
    #i = df_plot[1,]
    sns <- sign(diff(i))
    if(all(sns == -1)){
      return("Decreasing")
    }
    if(all(sns == 1)){
      return("Increasing")
    }
    if(sns[1] == -1){
      return("Temporary decline")
    }
    "Temporary increase"
  })
}


# Analyses ----------------------------------------------------------------
# forest <- readRDS("results/forest_light.RData")
# param_per_person <- predict_pars(forest, df_anal, parameters)

# Get max per person
# # First, calculate apex of parabola
# i = param_per_person[1,]
# max = i[1] - (i[2]^2 / 4*i[3])

#meds <- df_plot[1,]
# times = matrix(c(rep(1, 5), 0:4, (0:4)^2), nrow = 5)
# df_plot <- t(apply(param_per_person, 1, function(meds){
#   rowSums(matrix(meds, nrow(times), ncol(times), byrow = TRUE) * times)
# }))
# 
# shape <- shape_fun(df_plot)
# shape_rawdata <- shape_fun(df_anal[1:5])
# df_plot <- do.call(c, as.data.frame(df_plot))
# df_plot <- data.frame(Leeftijd = rep(14:18, each = nrow(df_anal)),
#                       Emo = df_plot,
#                       id = rep(1:nrow(df_anal), 5),
#                       Direction = shape
# )
# 
# library(ggplot2)
# 
# ggplot(df_plot, aes(x = Leeftijd, y = Emo, group = id, colour = Direction)) + geom_line(alpha = .5) + ylab("Difficulties in emotion regulation") +theme_bw() +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_colour_manual(values = c("Temporary decline" = "yellow", "Temporary increase" = "red", "Decreasing" = "green", "Increasing" = "orange"))
# 
# c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")

#df_plot <- data.frame(t(df_plot))
#tmp <- forest$forest[1:10]

# res_rf_light <- forest_light(res_rf, parameters)
# row = res_rf$data[1,]
# tree <- clear_underbrush(res, parameters = parameters)
# 
# 
# traverse_light(row, tree)

# chunks <- cut(1:length(forest$forest), 100)
# for(i in 1:length(levels(chunks))){
#   tmp <- forest$forest[which(chunks == levels(chunks)[i])]
#   tmp <- lapply(tmp, clear_underbrush, parameters = parameters)
#   saveRDS(tmp, paste0("results/light_tree", i, ".RData"))
#   print(i)
# }
# forest <- vector("list", length = length(chunks))
# for(i in 1:length(levels(chunks))){
#   forest[which(chunks == levels(chunks)[i])] <- readRDS(paste0("results/light_tree", i, ".RData"))
# }
# saveRDS(forest, "results/forest_light.RData")
# library(worcs)
# df_anal <- load_data(to_envir = FALSE)$df_anal
# traverse_light(df_anal[1,], forest[[1]])
