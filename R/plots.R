# ----------
# Plotting Functions
# ----------

#' @title smart_hist
#' @param x A numeric vector
#' @param freq Boolean set to \code{FALSE}
#'	to plot density on y-axis. Otherwise
#'	\code{TRUE} to plot frequencies
#' @param dens Boolean set to \code{TRUE}
#'	to overlay kernel density
#' @param main String for plot title
#' @param ... arguments passed to hist
#' @export
smart_hist = function(x,freq = FALSE,
	dens = TRUE,main = "",...){
	
	hist(x,col = "deepskyblue",
		main = main,freq = freq,...)
	
	if( dens && !freq )
		lines(density(x,na.rm = TRUE),
			lwd = 1.5,lty = 2,col = "red")
	
}
show_png = function(){
	cat("# ----------\n")
	cat("# PNG Template\n")
	cat("# ----------\n")
	cat("png(file.path(...),units = \"px\",height = ,width = ,res = 250,type = \"cairo\",pointsize = 20)\n")
	cat("...\n")
	cat("dev.off()\n")
}

#' @title smart_compMATs
#' @param MAT1 A numeric matrix
#' @param MAT2 A second numeric matrix of columns
#'	comparable to MAT1. Default is set to \code{NULL}
#'	resulting in histograms plotted for columns of MAT1
#' @param which_range Default is set to \code{NULL}
#'	to calculate data ranges. Otherwise if set to "01",
#'	will enforce minimum 0 and maximum 1. If set to a
#'	numeric vector of two elements, will enforce the range.
#' @param xlab A string for x-axis label
#' @param ylab A string for y-axis label
#' @param show_corr Boolean set to TRUE to print
#'	Pearson and Spearman correlations
#' @param show_plot Boolean set to TRUE to plot
#'	comparison of two matrices
#' @param main A string for the plot title
#' @param vec_col A vector of colors to color scatter 
#'	plot points
#' @param ... arguments passed to plot
#' @export
smart_compMATs = function(MAT1,MAT2 = NULL,which_range = NULL,
	xlab,ylab,show_corr = TRUE,show_plot = FALSE,main = NULL,
	vec_col = NULL,...){
	
	num_vars = ncol(MAT1)
	num_col = ceiling(sqrt(num_vars))
	num_row = ceiling(num_vars / num_col)
	
	if( !is.null(MAT2) ){
		# Compare Matrices
		if( show_corr ) cat("Pearson Correlation Matrix\n")
		tmp_cor1 = cor(MAT1,MAT2,method = "pearson")
		if( show_corr ) print(tmp_cor1)
		if( show_corr ) cat("Spearman Correlation Matrix\n")
		tmp_cor2 = cor(MAT1,MAT2,method = "spearman")
		if( show_corr ) print(tmp_cor2)
		# Calculate Root Mean Square Error between true and estimated columns
		rmse = apply(MAT1 - MAT2,2,function(xx) sqrt(mean(xx^2)))
	}
	
	if( is.null(vec_col) )
		vec_col = rgb(0,0,0,0.5)
	
	if( show_plot ){
		if( is.null(main) ){
			oma = rep(0,4)
		} else {
			oma = c(0,0,2,0)
		}
		par(mfrow=c(num_row,num_col),mar=c(4.3,4,2,1),oma=oma,bty="n")
		for(ii in seq(num_vars)){
			if( !is.null(MAT2) ){
				if( is.null(which_range) ){
					tmp_range_x = range(MAT1[,ii])
					tmp_range_y = range(MAT2[,ii])
				} else if( length(which_range) == 1 && which_range == "01" ){
					tmp_range = c(0,1)
					tmp_range_x = tmp_range
					tmp_range_y = tmp_range
				} else if( length(which_range) == 2 && is.numeric(which_range) ){
					tmp_range = sort(which_range)
					tmp_range_x = tmp_range
					tmp_range_y = tmp_range
				}
				plot(x = MAT1[,ii],y = MAT2[,ii],
					main = sprintf("%s\nP=%s;S=%s;RMSE=%s",
						colnames(MAT1)[ii],
						round(diag(tmp_cor1)[ii],3),
						round(diag(tmp_cor2)[ii],3),
						round(rmse[ii],3)),
					xlab = xlab,ylab = ylab,
					xlim = tmp_range_x,
					ylim = tmp_range_y,
					col = vec_col,...)
				if( !is.null(which_range) ) abline(a=0,b=1,lty=2,col="red",lwd=2)
			} else {
				smart_hist(MAT1[,ii],breaks = 50,
					xlab = xlab,main = colnames(MAT1)[ii])
			}
		}
		if( !is.null(main) ) mtext(main,outer=TRUE,cex=1.3)
		par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1,oma=rep(0,4),bty="o")
	}
	
}
smart_scatter = function(MAT1,MAT2,mainCor=TRUE,pt_col=NULL,diagOnly=FALSE,...){
	nmat1 = ncol(MAT1)
	nmat2 = ncol(MAT2)
	if( diagOnly ) nmat1 = 1
	
	par(mfrow=c(nmat1,nmat2),mar=c(4.3,4,1.2,0.2),bty="n")
	for(ii in seq(ncol(MAT1))){
	for(jj in seq(ncol(MAT2))){
		
		xlab = ""; ylab = ""
		if(jj==1) ylab = colnames(MAT1)[ii]
		if(ii==nmat1) xlab = colnames(MAT2)[jj]
		
		if( diagOnly ){
			if( ii != jj ){
				next
			} else {
				xlab = colnames(MAT2)[jj]
				ylab = colnames(MAT1)[ii]
			}
		}
		
		if( is.null(pt_col) ){
			pt_col = rgb(0,0,0,0.5)
		}
		
		submain=""
		if( mainCor ){
			pear = cor(MAT2[,jj],MAT1[,ii],method="pearson")
			spear = cor(MAT2[,jj],MAT1[,ii],method="spear")
			pear = round(pear,3); spear = round(spear,3)
			submain = sprintf("P=%s;S=%s",pear,spear)
		}
		
		plot(MAT2[,jj],MAT1[,ii],col=pt_col,
			xlab=xlab,ylab=ylab,main=submain,...)
		abline(a=0,b=1,lwd=2,lty=2,col=rgb(0,0,0,0.3))
	}
	}
	par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1,bty="o")
}
smart_mplot = function(DATA,LABEL=NULL,ABLINE = FALSE,...){
	if( "matrix" %in% class(DATA) ){
		DATA = smart_df(DATA)
	}
	
	num_vars 		= ncol(DATA)
	num_combos 	= choose(num_vars,2)
	num_rows 		= ceiling(sqrt(num_combos))
	num_cols 		= ceiling(num_combos / num_rows)
	if( !is.null(LABEL) ){
		oma = c(0,0,1.5,0)
	} else {
		oma = rep(0,4)
	}
	par(mfrow=c(num_rows,num_cols),mar=c(4,4,1,0.5),oma = oma,bty="n")
	for(ii in seq(num_vars)){
	for(jj in seq(num_vars)){
		if(ii < jj){
			tmp_main = sprintf("%s vs. %s",
				names(DATA)[ii],names(DATA)[jj])
			plot(DATA[,c(ii,jj)],xlab = names(DATA)[ii],
				ylab = names(DATA)[jj],main = tmp_main,...)
			if( ABLINE ) abline(a = 0,b = 1,lty = 2,col = rgb(1,0,0,1))
		}
	}
	}
	if( !is.null(LABEL) ) mtext(LABEL,outer=TRUE,cex=1.3)
	par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1,oma=rep(0,4),bty="o")
}
smart_histMAT = function(MAT,MEAN = FALSE,MAIN = NULL,
	plot_MAIN = NULL,DENS = FALSE,ABLINE = NULL,gRANGE = FALSE,
	...){
	
	# Check variances
	tmp_var = diag(var(MAT,na.rm = TRUE))
	MAT = MAT[,tmp_var > 0,drop = FALSE]
	
	# Calculate plot dimensions
	num_vars = ncol(MAT)
	num_cols = ceiling(sqrt(num_vars))
	num_rows = ceiling(num_vars / num_cols)
	
	oma = c(0,0,2,0)
	if( is.null(MAIN) ){
		oma = rep(0,4)
	}
	
	par(mfrow=c(num_rows,num_cols),mar=c(4,4.2,1,0.5),oma=oma)
	for(cc in seq(num_vars)){
		main = colnames(MAT)[cc]
		if( !is.null(plot_MAIN) && length(plot_MAIN) == ncol(MAT) ){
			main = paste0(main,";",plot_MAIN[cc])
		}
		
		if( gRANGE == TRUE ){
			my_range = range(c(MAT),na.rm = TRUE)
		} else if( gRANGE == FALSE ){
			my_range = range(MAT[,cc],na.rm = TRUE)
		}
		
		if( !is.null(ABLINE) ){
			my_range = range(c(ABLINE[cc],my_range),na.rm = TRUE)
		}
		
		hist(MAT[,cc],freq=FALSE,col="deepskyblue",main=main,xlab="",xlim = my_range,...)
		
		if( MEAN == TRUE ){
			abline(v=mean(MAT[,cc],na.rm=TRUE),lty=2,lwd=2,col="red")
		}
		
		if( DENS ){
			lines(density(MAT[,cc],na.rm = TRUE),lwd = 2,lty = 2,col = "blue")
		}
		
		if( !is.null(ABLINE) ){
			abline(v = ABLINE[cc],lty = 3,lwd = 2,col = "green")
		}
		
	}
	if( !is.null(MAIN) ){
		mtext(MAIN,outer=TRUE,cex=1.4)
	}
	par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1,oma=rep(0,4))
}
smart_histDiffHist = function(MAT,MAIN = NULL){
	num_vars = ncol(MAT)
	
	if( is.null(MAIN) ){
		oma = rep(0,4)
	} else {
		oma = c(0,0,2,0)
	}
	
	par(mfrow=c(num_vars,num_vars),mar=c(4,4,1,0.5),oma=oma)
	for(ii in seq(num_vars)){
	for(jj in seq(num_vars)){
		if(ii <= jj){
			if(ii == jj){
				hist(MAT[,ii],freq=FALSE,col="gray",breaks=50,xlab="",main=colnames(MAT)[ii])
			} else {
				hist(MAT[,jj]-MAT[,ii],freq=FALSE,col="gray",breaks=50,xlab="",main=sprintf("%s - %s",
					colnames(MAT)[jj],colnames(MAT)[ii]))
			}
		} else {
			plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
		}
	}
	}
	if( !is.null(MAIN) ) mtext(MAIN,outer=TRUE,cex=1.2)
	par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1,oma=rep(0,4))
	
}

#' @title smart_boxplot
#' @param MAT A numeric matrix of columns to 
#'	plot as boxplots
#' @param mar_down A positive numeric value to 
#'	allow space below the x-axis for labels
#' @param srt A numeric value to control the 
#'	angle of x-axis labels
#' @param ... arguments passed to boxplot
#' @export
smart_boxplot = function(MAT,mar_down = 8,srt = 45,...){
	
	par(mar = c(mar_down,4,2,2) + 0.1)
	boxplot(MAT,xaxt = "n",col = "deepskyblue",...)
	axis(1,labels = FALSE,tick = FALSE)
	labels = colnames(MAT)
	text(x = seq_along(labels),
		y = par("usr")[3] - (par("usr")[4] - par("usr")[3])/30,
		srt = srt,adj = 1,labels = labels,cex = 0.8,xpd = TRUE)
	par(mar = c(5,4,4,2) + 0.1)
	
}
smart_barplot = function(VEC,cex = 0.7,srt = 35,...){
	
	plt = barplot(height = VEC,col = "deepskyblue",xaxt = "n",...)
	LABELS = names(VEC)
	text(x = plt,y = par("usr")[3],
		srt = srt,adj = 1,labels = LABELS,cex = cex,xpd = TRUE)
}
smart_histCAT = function(DATA,VARS,OUT,main_lab = "",
	FUNC = NULL,...){
	
	if(FALSE){
		DATA = res; VARS = c("CELLTYPE"); OUT = "PVAL"
		FUNC = function(xx){ mean(xx <= 0.05) }
	}
	
	uDATA = unique(DATA[,VARS,drop=FALSE]); uDATA
	keepVARS = c()
	for(VAR in VARS){
		# VAR = VARS[1]; VAR
		if( length(unique(uDATA[,VAR])) > 1 ){
			keepVARS = c(keepVARS,VAR)
		}
	}
	VARS = keepVARS
	uDATA = uDATA[,VARS,drop=FALSE]
	uDATA
	
	num_vars = nrow(uDATA)
	num_cols = ceiling(sqrt(num_vars))
	num_rows = ceiling(num_vars / num_cols)
	num_dat = nrow(DATA)
	
	par(mfrow = c(num_rows,num_cols),mar = c(4,4,1,0.5))
	for(ii in seq(nrow(uDATA))){
		# ii = 1
		idx = rep(TRUE,num_dat); label = ""
		for(VAR in VARS){
			idx = idx & DATA[,VAR] == uDATA[ii,VAR]
			label = paste0(label,sprintf("%s = %s;",VAR,uDATA[ii,VAR]))
		}
		if( main_lab != "" ){
			label_func = FUNC(DATA[idx,OUT])
			label = sprintf("%s %s = %s",label,main_lab,label_func)
		}
		smart_hist(DATA[idx,OUT],main = label,...)
	}
	par(mfrow = c(1,1),mar = c(5,4,4,2)+0.1)
	
}
make_qqplot_pvalue = function(pp,trans = FALSE,...){
	
	pp = sort(pp)
	nn = length(pp)
	obs_quant = pp
	expect_quant = seq(nn) / (nn + 1)
	
	if( trans ){
		plot(x = -log10(expect_quant),
			y = -log10(obs_quant),pch = 16,
			xlab = "-log10(Exp P-value)",
			ylab = "-log10(Obs Pvalue)",...)
	} else {
		plot(x = obs_quant,y = expect_quant,pch = 16,
			xlab = "Exp Pvalue",ylab = "Obs Pvalue",...)
	}
	abline(a = 0,b = 1,lty = 2)
	
}

#' @title smart_colors
#' @param nn A positive integer greater than 
#'	or equal to 2
#' @param alpha A positive numeric value less 
#'	than or equal to one
#' @param overwrite Boolean If nn = 2, setting to 
#'	FALSE will force colors to be white or black
#' @export
smart_colors = function(nn,alpha = 1,overwrite = FALSE){
	if( nn == 2 && overwrite == FALSE ){
		c("white","black")
	} else {
		hues = seq(15, 375, length = nn + 1)
		hcl(h = hues,l = 65,c = 100,alpha = alpha)[1:nn]
	}
}
smart_convCOLOR = function(MAT){
	cMAT = c()
	num_row = nrow(MAT)
	
	for(cc in seq(ncol(MAT))){
		uniq_elem = sort(unique(MAT[,cc]))
		uniq_num = length(uniq_elem)
		
		if( uniq_num == 1 ){
			tmp_col = rep("white",num_row)
		
		} else if( uniq_num %in% seq(2,5) ){
			vec_col = smart_colors(nn = uniq_num)
			tmp_col = rep(NA,num_row)
			for(ii in seq(uniq_num)){
				tmp_col[ MAT[,cc] == uniq_elem[ii] ] = vec_col[ii]
			}
		
		} else {
			vec_col = colorpanel(n = uniq_num,"blue","white","red")
			tmp_df = smart_df(COL = vec_col,VAR = uniq_elem)
			df1 = smart_df(ORDER = seq(num_row),VAR = MAT[,cc])
			df1 = smart_merge(df1,tmp_df,all = TRUE)
			df1 = df1[order(df1$ORDER),]
			tmp_col = df1$COL
		}
		
		cMAT = cbind(cMAT,tmp_col)
	}
	
	return(cMAT)
}


###

