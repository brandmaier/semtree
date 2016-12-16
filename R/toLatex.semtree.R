toLatex.semtree <-
function(object, alternative.edge.labels=TRUE,root=NULL,
	 prev_node=NULL, latex.mapping=NULL,parameter.names=NULL, stars=FALSE, linewidth=1,
	 dash.threshold=1,ci=FALSE,sd=FALSE, parameter.order=NULL, ...)
{
	return(render_tree(object, TRUE,alternative.edge.labels,NULL,NULL,latex.mapping, 
		parameter.names, stars, linewidth,dash.threshold,ci,sd,parameter.order));	
}
