# ----------
# General Functions
# ----------

#' @title smart_merge
#' @description Merges two data.frames assuming
#'	they have at least one shared column name
#' @param x A data.frame
#' @param y A data.frame
#' @param mess Default to \code{FALSE}.
#'	Otherwise a message is printed.
#' @param ... arguments passed to merge
#' @examples 
#'
#' aa = smart_df(a = c(1,2,3),b = c("a","b","c"),c = c(4,5,6))
#' bb = smart_df(a = c(2,4,5),b = c("b","d","e"),d = c("alpha","beta","gamma"))
#' smart_merge(aa,bb,all.x = TRUE)
#' smart_merge(aa,bb,all.y = TRUE)
#' smart_merge(aa,bb,all = TRUE)
#'
#' @return A merged data.frame
#'
#' @export
smart_merge = function(x,y,mess = FALSE,...){
	if( mess ){
		intersect_vars = paste(intersect(names(x),names(y)),collapse=", ")
		message(paste0("Merging dataframes on variables = { ",intersect_vars," }\n"))
	}
	
	merge(x,y,by = intersect(names(x),names(y)),...)
}

#' @title smart_table
#' @description Should elements passed into \code{table()}
#'	contain \code{NA} or \code{NaN}, we want to see them 
#'	by default.
#' @param ... arguments passed to table
#' @examples
#'
#' aa = c(1,1,2,2,2,3,NA)
#' table(aa)
#' smart_table(aa)
#' @return Return a table
#'
#' @export
smart_table = function(...){
	table(...,useNA = 'ifany')
}

#' @title smart_RT
#' @description Calls \code{read.table()} but sets
#'	argument \code{stringsAsFactors = FALSE} to prevent
#'	treating character columns as factors.
#' @param ... arguments passed to read.table
#' @return Return is identical to \code{read.table()}
#' @export
smart_RT = function(...){
	read.table(...,stringsAsFactors = FALSE)
}

#' @title smart_WF
#' @description Calls \code{write.table()} setting
#'	parameters \code{row.names} and \code{quote} 
#'	to \code{FALSE}.
#' @param ... arguments passed to write.table
#' @return Return is identical to \code{write.table()}
#' @export
smart_WT = function(...){
	write.table(...,row.names = FALSE,quote = FALSE)
}

#' @title smart_df
#' @description Construct data.frame with \code{data.frame()}
#'	but sets \code{stringsAsFactors} to \code{FALSE}.
#' @param ... arguments passed to data.frame
#' @return A data.frame
#' @export
smart_df = function(...){
	data.frame(...,stringsAsFactors = FALSE)
}

#' @title smart_mkdir
#' @description Create directory if it does not exist
#' @param input_dir A full path name for 
#'	a directory to create
#' @return No return value
#' @export
smart_mkdir = function(input_dir){
	
	if( !file.exists(input_dir) || !dir.exists(input_dir) )
		dir.create(path = input_dir,recursive = TRUE)
	
}

#' @title make_dummy
#' @description Construct a dummy-coded matrix 
#'	for a single variable
#' @param x A numeric or character vector
#'	to convert to a dummy matrix
#' @return A binary indicator matrix of ones and zeros
#' @export
make_dummy = function(x){
	len_x = length(x)
	all_factors = sort(unique(x))
	num_dummy = length(all_factors) - 1
	fact_matrix = matrix(0,nrow = len_x,
		ncol = max(1,num_dummy))
	fact_matrix = smart_df(fact_matrix)
	
	if( num_dummy == 0 )
		stop("Variable vector is constant")
	
	names(fact_matrix) = paste0(all_factors[-1],
		"_vs_",all_factors[1])
	
	for(ii in 1:len_x){
		pos = which(x[ii]==all_factors) - 1
		if( is.na(x[ii]) )
			fact_matrix[ii,] = NA
		else if( pos > 0 )
			fact_matrix[ii,pos] = 1
	}
	
	return(fact_matrix)
	
}

#' @title collapse_var
#' @description Collapse a subset of values within
#'	a vector into a new value
#' @param ORIG_VAR The input vector
#' @param ORIG_VALUES A subset of values from
#'	the input vector to be collapsed
#' @param NEW_VALUE The new value to replace
#'	\code{ORIG_VALUES} in \code{ORIG_VAR}
#' @return A character or numeric vector
#' @export
collapse_var = function(ORIG_VAR,ORIG_VALUES,NEW_VALUE){
	
	ORIG_VAR[which(ORIG_VAR %in% ORIG_VALUES)] = NEW_VALUE
	
	return(ORIG_VAR)
	
}

#' @title name_change
#' @description Substitute a column name of a matrix 
#'	or data.frame with a new name
#' @param DATA A matrix or data.frame
#' @param ORIG_NAME A single character column name to alter
#' @param NEW_NAME A single character to replace \code{ORIG_NAME}
#' @return An updated data.frame with renamed fields
#' @export
name_change = function(DATA,ORIG_NAME,NEW_NAME){
	
	old_idx = which(colnames(DATA) == ORIG_NAME)
	new_idx = which(colnames(DATA) == NEW_NAME)
	if( length(new_idx) > 0 ){
		return(DATA)
	} else if( length(old_idx) > 0 ){
		colnames(DATA)[old_idx] = NEW_NAME
		return(DATA)
	} else {
		stop(sprintf("ORIG_NAME = %s missing",ORIG_NAME))
	}
	
}

#' @title smart_SN
#' @description Convert numeric values into 
#'	scientific notation
#' @param x A numeric vector to convert 
#'	to scientific notation
#' @param digits A positive integer for 
#'	number of digits to include in notation
#' @return A character vector
#' @export
smart_SN = function(x,digits = 2){
	# For scientific notation
	formatC(x,format = "e",digits = digits)
}

#' @title smart_digits
#' @description Round numeric values to specific
#' @param x A numeric vector formatted
#'	to have consistently rounded values
#' @param digits A positive integer
#'	to regulate the number of digits to
#'	round to
#' @return Character version of rounded numeric value
#' @export
smart_digits = function(x,digits=2){
	sprintf(paste0("%.",digits,"f"),round(x,digits))
}

#' @title smart_rmcols
#' @description Drops columns from a matrix or data.frame.
#' @param OBJ A matrix or data.frame
#' @param rm_names A string vector of colnames 
#'	to remove
#' @return A matrix or data.frame
#' @export
smart_rmcols = function(OBJ,rm_names){
	rm_names = intersect(rm_names,colnames(OBJ))
	if( length(rm_names) > 0 ){
		OBJ[,!(colnames(OBJ) %in% rm_names),drop = FALSE]
	} else {
		OBJ
	}
}

#' @title smart_reqNames
#' @description Checks if required column names are
#'	contained in the matrix or data.frame.
#' @param DATA A matrix or data.frame
#' @param REQ A string vector of colnames
#'	required to be contained in DATA
#' @return No return value.
#' @export
smart_reqNames = function(DATA,REQ){
	
	if( all(REQ %in% colnames(DATA)) )
		return(NULL)
	
	miss_names = REQ[!(REQ %in% colnames(DATA))]
	stop(sprintf("Missing columns: %s",
		paste(miss_names,collapse = ",")))
	
}

#' @title smart_progress
#' @description Print progress of a for loop
#' @param ii A positive integer to track a 
#'	loop's progress
#' @param nn A positive integer for the 
#'	total number of loop iterations
#' @param string A string to print
#' @param iter A positive integer for how many
#'	multiple iterations to print "."
#' @param iter2 A positive integer to end
#'	a line of printed "." and track the loop's 
#'	progress
#' @param ... arguments passed to cat
#' @return No return value.
#' @export
smart_progress = function(ii,nn,string = ".",iter = 5,iter2 = 2e2,...){
	
	if(ii %% iter == 0)
		message(string,...)
	
	if(ii %% iter2 == 0 || ii == nn)
		message(sprintf("%s out of %s\n",ii,nn),...)
	
}

#' @title smart_dots
#' @description Prints a series of dots every
#'	few seconds
#' @param wait A number of seconds to wait before printing "."
#' @param num_dots The number of dots to print before
#'	printing a message
#' @return No return value.
#' @export
smart_dots = function(wait = 300,num_dots = 30){
	cnt = 1
	while(TRUE){
		message(".")
		num_min = wait * num_dots / 60
		
		if( cnt %% num_dots == 0 )
			message(sprintf("%s minutes passed\n",num_min))
		
		Sys.sleep(time = wait)
		cnt = cnt + 1
	}
}

#' @title bin_cont_var
#' @description Transform numeric vector into
#'	discrete bins
#' @param VAR A numeric vector of values to bin
#' @param NUM_GROUPS A positive integer for the
#'	number of bins
#' @param ROUND A nonnegative integer for displaying
#'	bin labels through binned intervals
#' @param binNUM Boolean set to TRUE to map bins to
#'	numbers. Otherwise, bins are characterized by intervals
#' @return A character or integer vector of 
#'	collapsed/binned values
#' @export
bin_cont_var = function(VAR,NUM_GROUPS,
	ROUND = 3,binNUM = FALSE){
	
	my_quantiles = as.numeric(quantile(x = VAR,
		probs = seq(NUM_GROUPS - 1) / NUM_GROUPS,
		na.rm = TRUE))
	
	out_VAR = rep(NA,length(VAR))
	for(ii in seq(NUM_GROUPS)){
		if( ii == 1 ){
			if( binNUM ){
				out_VAR[which(VAR <= my_quantiles[ii])] = ii
			} else {
				out_VAR[which(VAR <= my_quantiles[ii])] = paste0(ii,
					") ",round(min(VAR,na.rm = TRUE),ROUND),
					"-",round(my_quantiles[ii],ROUND))
			}
		} else if( ii == NUM_GROUPS ){
			if( binNUM ){
				out_VAR[which(VAR > my_quantiles[ii-1])] = ii
			} else {
				out_VAR[which(VAR > my_quantiles[ii-1])] = paste0(ii,
					") ",round(my_quantiles[ii-1],ROUND),
					"-",round(max(VAR,na.rm = TRUE),ROUND))
			}
		} else {
			if( binNUM ){
				out_VAR[which(VAR > my_quantiles[ii-1] 
					& VAR <= my_quantiles[ii])] = ii
			} else {
				out_VAR[which(VAR > my_quantiles[ii-1] 
					& VAR <= my_quantiles[ii])] = paste0(ii,
					") ",round(my_quantiles[ii-1],ROUND),
					"-",round(my_quantiles[ii],ROUND))
			}
		}
	}
	
	if( binNUM ) out_VAR = as.character(out_VAR)
	
	out_VAR
}

#' @title smart_names
#' @description Sets row/column names to matrix or data.frame
#' @param MAT A matrix
#' @param ROW A vector of length equal to \code{nrow(MAT)}
#' @param COL A vector of length equal to \code{ncol(MAT)}
#' @return Outputs a matrix or data.frame depending on 
#'	input object class
#' @export
smart_names = function(MAT,ROW = NULL,COL = NULL){
	colnames(MAT) = seq(ncol(MAT))
	rownames(MAT) = seq(nrow(MAT))
	
	if( !is.null(ROW) ){
		if( nrow(MAT) != length(ROW) ){
			stop("Issue with row name lengths and MAT dimension!")
		}
		rownames(MAT) = ROW
	}
	if( !is.null(COL) ){
		if( ncol(MAT) != length(COL) ){
			stop("Issue with column name lengths and MAT dimension!")
		}
		colnames(MAT) = COL
	}
	
	MAT
}


###
