render_tree_recursively <-
function(node, with_tex_escape=TRUE, alternative_edge_labels=TRUE,root=NULL, prev_node=NULL, latex_mapping=NULL,parameter.names=NULL, stars=TRUE, linewidth=1,
dash.threshold=1, ci=FALSE, sd=FALSE, parameter.order=NULL)
{
	#print(paste("DRAW",node$caption, "Thresh:",dash.threshold))
	if(is.null(root)) {
		root <- node;	
	}
		
	repr <- ""
	annot <- ""
	
	# prepare node caption
	node_caption <- node$caption
	if (alternative_edge_labels) {
		relCov <- which(node$min_cov_idx==root$covariate.ids)
		offset <- ( relCov - 1) * 3 +1;
		#nameid <- root$recoding$expressions[offset+2]$id;
		#node_caption <- names(root$recoding$dataset)[ nameid ]
	  node_caption <- node$rule$name
	}
	
  #node$edge_label <- node$rule$name
	# draw edge label on edge leading to the current node
	if (!is.null(node$edge_label)) {
		
	#	edge_label <- node$edge_label
		if (alternative_edge_labels) {
			#relCov <- which(prev_node$min_cov_idx==root$covariate.ids)
			#offset <- ( relCov - 1) * 3 +1;
			#offset <- (node$min_cov_idx - 1 ) *3 +1;
			#print(node$min_cov_idx)
			#print(root$covariate_ids)
			#print(paste("OFFSET",offset,"relcov",relCov));
			#value <- root$recoding$expressions[offset]$value;		
      #type <- root$recoding$expressions[offset+1]$type;		
      value <- prev_node$rule$value
      type <- prev_node$rule$relation
      print(paste("VT",prev_node$node_id,":",value,type,relCov,offset))
		
			if (type == "<") {
				if (node$edge_label == 0) {
					edge_label <- paste("\\ge ",value,"")	
				} else {
					edge_label <- paste("<",value,"")
				}	
			}
      
			if (type == ">=") {
			  if (node$edge_label == 0) {
			    edge_label <- paste("< ",value,"")	
			  } else {
			    edge_label <- paste(">",value,"")
			  }	
			}
			
			if (type == "in") {
				if (node$edge_label == 1) {
					edge_label <- paste(value,collapse=",");					
				} else {
					parent.id <- root$recoding$parents[prev_node$min_cov_idx]
					all.levels <-  levels(root$recoding$dataset[,parent.id])
					
					other <- setdiff(all.levels, value)
					
					print(paste(value,"other:",prev_node$min_cov_idx,"offset",offset,"pid",parent.id));
					#edge_label <- "other"
					edge_label <- paste(other,collapse=",")
				}	
				
			}
			
		}
		
		
		annot <- paste( "\\ncput*{",latex_escape(edge_label),"}\n"   );
	}
	
	if (node$caption == "TERMINAL") {
		#cat(prev_node)
#		if ((is.null(prev_node)) || ((prev_node$p < dash.threshold))) { 
		if ( (is.null(prev_node)) || (is.null(prev_node$p) || (prev_node$p < dash.threshold) )   ) {
				repr <- "\\TR{\\psframebox{"				
		} else {	
				repr <- "\\TR{\\psframebox[linestyle=dashed]{"
		}
		#lines <- c();
		
		repr <- paste(repr, "\\begin{tabular}{c}\n");
		if (!is.null(node$parent.model)) {		
			repr <- paste(repr, node$parent.model@name,"\\\\" );
		}
		repr <- paste(repr, "N =",toString(node$N), "\\\\");
		


		
		for (ii in 1:length(node$params))
		{

			if (is.null(parameter.order)) {
				i <- ii
			} else {
				i <- parameter.order[ii]
			}

					starstr <- ""
					
			if (stars) {
			 z <- abs(node$params[i] / node$params_sd[i] )
			 if (is.na(z)) {
			 	z <- NA
			 } else {
			if (z >= 3) { starstr <- "**"; }
			else if (z >= 2) { starstr <- "*";}
			} 
			
			}
			#if (with_tex_escape) {
			#	lines <- paste("$",lines)	
			#}
			sdstr <- ""
			if (sd) {
				sdstr <- paste("$\\pm",round(node$params_sd[i],3),"$");
			}
			
			cistr <- ""
			if (ci) {
				
				z <- qnorm(p=0.975)
				N <- node$N 
				delta <- z*node$params_sd[i]/sqrt(N)
				cistr <- paste("$[",round(node$params[i]-delta,3),";",round(node$params[i]+delta,3) ,"]$")
			}
			
			
			param_name <- node$param_names[i]

			
			if (!is.null(parameter.names)) {
			 if (!param_name %in% parameter.names ) {
			   next;		
			 }
			}			

			if (!is.null(latex_mapping)) {
				param_name <- latex_mapping[node$param_names[i]];
			}
			
			lines <-latex_escape( paste(param_name,"=",round(node$params[i],3)) );
			
			#if (with_tex_escape) {
			#	lines <- paste("$",lines)	
			#}
			
			lines <- paste(lines, sdstr,cistr,starstr) ;
			
			if (ii < length(node$params)) {
				lines <- paste(lines, "\\\\ \n");
			}	
			
			repr <- paste(repr, lines);	
		}		
		repr <- paste(repr, "\n \\end{tabular}\n")
		
#	latex_escape(lines),"}}");

	repr <- paste(repr, "}}");
	
	repr <- paste(repr, annot);
	#print(repr);
	return(repr);
	}
	
 #id_code <- paste("~*[tnpos=a,tnsep=3pt]{\\psframebox{",toString(node$id),"}}");
 id_code <- ""
 
  pstr <- "";
  
  if (root$p.values.valid) {
  	pstr <- paste(",p=",round(node$p,3));
  } else {
  	pstr <- paste(",lr=",round(node$lr,3));
  }

	if ((is.null(prev_node)) || (is.null(prev_node$p) || (prev_node$p < dash.threshold) )   )
	{
		linestyle <- "solid"
	} else {
		linestyle <- "dashed"
	}

	# create root node
            repr <- paste(repr, "\\pstree[linewidth=",linewidth,"pt,linestyle=",linestyle,",treefit=tight,levelsep=3.8cm,treesep=1.5cm]{\\Toval[linewidth=",linewidth,"pt]{$",node_caption," ",pstr,"$}",id_code," ",annot,"}",
            "{\n", sep="");
            
    # add children
              repr <- paste(repr, 
              render_tree_recursively(node$left_child, with_tex_escape, alternative_edge_labels=alternative_edge_labels, root=root, prev_node=node, latex_mapping=latex_mapping,parameter.names=parameter.names, stars, linewidth=linewidth,dash.threshold=dash.threshold,ci=ci,sd=sd,parameter.order=parameter.order),
              render_tree_recursively(node$right_child,with_tex_escape,alternative_edge_labels=alternative_edge_labels, root=root, prev_node=node, latex_mapping=latex_mapping,parameter.names=parameter.names, stars, linewidth=linewidth,dash.threshold=dash.threshold,ci=ci,sd=sd,parameter.order=parameter.order)
            , "}");	
	
	return(repr);	
	
}
