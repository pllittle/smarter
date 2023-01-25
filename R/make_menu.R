# ----------
# Menu making
# ----------
print_noInput = function(){
	cat("No input, try again ...\n")
}
print_notOpt = function(){
	cat("Not an option, try again ...\n")
}

#' @title make_menu
#' @description Constructs an interactive menu
#'	 for the user
#' @param PROMPT A character string prompt to the user
#' @param OPTS A character vector where elements contain
#'	a number, then a closing parentheses, then the option 
#'	value
#' @param INDENT A character string for the amount 
#'	of indentation from the left margin
#' @export
make_menu = function(PROMPT,OPTS,INDENT = "   "){
	
	if( missing(PROMPT) ){
		PROMPT = readline("Enter a prompt: ")
		cat(sprintf("PROMPT = %s\n",PROMPT))
	}
	
	if( missing(OPTS) ){
		OPTS = c()
		while(TRUE){
			
			while(TRUE){
				RESP = make_menu(PROMPT = "Add an option?",
					OPTS = c("1) Yes","2) No"),
					INDENT = INDENT)
				
				if( RESP == "" ){
					print_noInput()
					next
				}
				
				if( ! RESP %in% c(1,2) ){
					print_notOpt()
					next
				}
				
				break
			}
			
			if( RESP == 2 ) break
			OPTS = c(OPTS,readline("Enter an option: "))
			
			if( length(OPTS) > 0 ){
				cmd = sprintf("OPTS = (%s)\n",
					paste(sprintf("'%s'",OPTS),collapse = ", "))
				cat(cmd)
			}
			
		}
		
	}
	
	cmd = sprintf("%s\n",PROMPT)
	if( length(OPTS) > 0 ){
		for(ii in seq(length(OPTS))){
			cmd = sprintf("%s%s%s\n",cmd,INDENT,OPTS[ii])
		}
	}
	cmd = sprintf("%s%s> ",cmd,INDENT)
	
	RESP = readline(cmd)
	return(RESP)
	
}


##

