# ----------
# Latex Output Functions
# ----------
format_latex = function(INPUT){
	# INPUT = "optE_AIC%"
	
	if( length(grep("^\\$",INPUT)) == 1 && length(grep("\\$$",INPUT)) == 1 ){
		return(INPUT)
	}
	
	INPUT2 = gsub("%","\\\\%",INPUT)
	INPUT2 = gsub("_","\\\\_",INPUT2)
	INPUT2
}
clean_repeats = function(VEC){
	if(FALSE){
		VEC = c(rep("a",2),rep("b",2),"a","c")
		VEC
	}
	
	curr_string = NA
	for(ii in seq(length(VEC))){
		# ii = 1
		if( ii == 1 ){
			curr_string = VEC[ii]
		} else {
			if( VEC[ii] == curr_string ){
				VEC[ii] = ""
			} else {
				curr_string = VEC[ii]
			}
		}
	}
	
	VEC
}

#' @title print_latex_table
#' @param DATA A matrix or data.frame to
#'	present as a latex table
#' @param repeat_VARS A string vector of colnames
#'	to avoid repeating values within a column
#' @param my_align A string containing letters "l",
#'	"r", or "c" for left, right, and center alignment
#' @param add_table Boolean set to TRUE to
#'	enclose tabular environment with table environment
#' @param fontsize Defaults to NULL to not specify
#'	a fontsize. Otherwise, possible values are
#'	"tiny", "footnotesize", "small", "normalsize",
#'	"large", "Large", "LARGE", "huge","Huge"
#' @param caption A string to include a table caption
#' @param label A string to represent a latex table label
#' @param midrule1 Default is set to NULL
#' @param latex_comment Add a latex comment above 
#'	the table for notes
#' @param ... arguments passed to cat
#' @return No return value
#' @export
print_latex_table = function(DATA,repeat_VARS = NULL,
	my_align = NULL,add_table = FALSE,fontsize = NULL,
	caption = NULL,label = NULL,midrule1 = NULL,
	latex_comment = NULL,...){
	
	orig_names = colnames(DATA)
	
	if( nrow(DATA) > 1 ){
		DATA = smart_df(apply(DATA,2,as.character))
	} else {
		DATA = smart_df(t(apply(DATA,2,as.character)))
	}
	
	if( !is.null(repeat_VARS) && length(repeat_VARS) > 0 ){
		# loop thru vector(column) to find repeats and replace with ""
		tmp_index = which(orig_names %in% repeat_VARS)
		DATA[,tmp_index] = apply(DATA[,tmp_index,drop=FALSE],2,clean_repeats)
	}
	
	prep_DATA = DATA
	
	message("\n",...)
	
	if( !is.null(latex_comment) ){
		message(sprintf("%% %s\n",latex_comment),...)
	}
	
	if( add_table ){
		message(paste0("\\begin{table}[!htbp] \n\\centering\n"),...)
		if( !is.null(fontsize) )
			message(paste0("\\",fontsize,"\n"),...)
		else
			message(paste0("\\normalsize\n"),...)
		if( !is.null(caption) ){
			caption = gsub("\n","",caption)
			message(paste0("\\caption{",caption,"}\n"),...)
		}
		if( !is.null(label) ) message(paste0("\\label{tab:",label,"}\n"),...)
	}
	
	if( is.null(my_align) ){
		message(paste0("\\begin{tabular}{l",
			paste(rep("c",ncol(prep_DATA)-1),collapse=""),"}\n"),...)
	} else {
		message(paste0("\\begin{tabular}{",my_align,"}\n"),...)
	}
	
	message("\\toprule\n",...)
	message(paste0(paste(sapply(orig_names,format_latex),collapse=" & ")," \\\\\n"),...)
	
	if( is.null(midrule1) ){
		message("\\midrule\n",...)
	} else {
		message(paste0(midrule1,"\n"),...)
	}
	apply(prep_DATA,1,function(x)
		message(paste0(paste(sapply(x,format_latex),
			collapse=" & ")," \\\\\n"),...))
	message("\\bottomrule\n\\end{tabular}\n",...)

	if( add_table ){
		message(paste0("\\end{table}\n"),...)
	}

	message("\n",...)
	
}

###

