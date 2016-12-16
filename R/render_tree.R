render_tree <-
function(node, with_tex_escape=TRUE, alternative_edge_labels=TRUE,root=NULL, prev_node=NULL, latex_mapping=NULL,parameter.names=NULL, stars=TRUE, linewidth=1,dash.threshold=1,ci=FALSE,sd=FALSE, parameter.order=NULL)
{

#pre <- "\\begin{sidewaysfigure} \n 
pre <- "% make sure to add these packages:\n% \\usepackage{pst-all}\n% \\usepackage{graphicx}\n\n\\resizebox{\\textwidth}{!}{ \n "
post <- "}"# \n \\end{sidewaysfigure}\n "

content <- render_tree_recursively(node, with_tex_escape, alternative_edge_labels, root, prev_node, latex_mapping,parameter.names,stars, linewidth, dash.threshold, ci, sd, parameter.order );

output <- paste(pre,content,post)

return(output);
	
	
}
