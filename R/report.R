report <- function(msg, lvl)
{
	premsg <- ""
	if (lvl > 1) {
		premsg <- paste(rep("  |  ",(lvl-1)), sep="")
	}

	if (lvl > 0) {
		premsg <- paste(premsg, "  |--",sep="")
	}

  msg <- paste(premsg,msg,sep="")
  
	message(msg);
}