evaluateTree <-
function(tree, test_set, data_type="raw", leaf_ids=NULL)
{
	

	
#model_ids <- tree$model.ids;

mapping <- getIdParameterMapping(tree)

# get a mapping of dataset rows to leaf ids
if (is.null(leaf_ids)) {
	leaf_ids <- traverse(tree, test_set)
}

# for each leaf, calculate deviance of each data row

#leafwise <- rep(NA, length(unique(leaf_ids)))
dev <- 0
for (leaf_id in unique(leaf_ids))
{
	#print(leaf_id)
	temp_set <- test_set[leaf_ids==leaf_id, ];
	
	#params <- mapping[ mapping[,1]==leaf_id, 2:ncol(mapping)]
	#model <- set_model_parameters(tree$model, params)
	
	leaf <- getNodeById( tree, leaf_id)
	
	add_dev <- evaluateDataLikelihood(leaf$model, temp_set[,,drop=F], data_type )
	#cat(paste("Add deviance ",add_dev,"leaf",leaf$node_id,"\n")); ##, toString(which(leaf_ids==leaf_id)) ,"\n"))
	dev <- dev + add_dev;
}

result <- list()
result$deviance <- dev
result$num_models <- length(unique(leaf_ids))

return(result);

}
