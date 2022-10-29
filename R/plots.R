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
	
	oldpar = par(no.readonly = TRUE)
	
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
		par(mfrow = c(num_row,num_col),
			mar = c(4.3,4,2,1),oma = oma,
			bty = "n")
		on.exit(par(oldpar))
		
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
		
		if( !is.null(main) ) 
			mtext(main,outer = TRUE,cex = 1.3)
		
	}
	
}
smart_scatter = function(MAT1,MAT2,mainCor=TRUE,pt_col=NULL,diagOnly=FALSE,...){
	
	oldpar = par(no.readonly = TRUE)
	
	nmat1 = ncol(MAT1)
	nmat2 = ncol(MAT2)
	if( diagOnly ) nmat1 = 1
	
	par(mfrow = c(nmat1,nmat2),
		mar = c(4.3,4,1.2,0.2),
		bty = "n")
	on.exit(par(oldpar))
	
	for(ii in seq(ncol(MAT1))){
	for(jj in seq(ncol(MAT2))){
		
		xlab = ""; ylab = ""
		if( jj == 1 ) ylab = colnames(MAT1)[ii]
		if( ii == nmat1 ) xlab = colnames(MAT2)[jj]
		
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
		
		plot(MAT2[,jj],MAT1[,ii],col = pt_col,
			xlab = xlab,ylab = ylab,main = submain,
			...)
		abline(a = 0,b = 1,lwd = 2,lty = 2,
			col = rgb(0,0,0,0.3))
	}}
	
}
smart_mplot = function(DATA,LABEL=NULL,ABLINE = FALSE,...){
	
	oldpar = par(no.readonly = TRUE)
	
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
	on.exit(par(oldpar))
	
	for(ii in seq(num_vars)){
	for(jj in seq(num_vars)){
		if(ii < jj){
			tmp_main = sprintf("%s vs. %s",
				names(DATA)[ii],names(DATA)[jj])
			plot(DATA[,c(ii,jj)],xlab = names(DATA)[ii],
				ylab = names(DATA)[jj],main = tmp_main,...)
			if( ABLINE ) abline(a = 0,b = 1,lty = 2,col = rgb(1,0,0,1))
		}
	}}
	
	if( !is.null(LABEL) ) mtext(LABEL,outer=TRUE,cex=1.3)
	
}
smart_histMAT = function(MAT,MEAN = FALSE,MAIN = NULL,
	plot_MAIN = NULL,DENS = FALSE,ABLINE = NULL,gRANGE = FALSE,
	...){
	
	oldpar = par(no.readonly = TRUE)
	
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
	
	par(mfrow = c(num_rows,num_cols),
		mar = c(4,4.2,1,0.5),oma = oma)
	on.exit(par(oldpar))
	
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
	
}
smart_histDiffHist = function(MAT,MAIN = NULL){
	num_vars = ncol(MAT)
	
	oldpar = par(no.readonly = TRUE)
	
	if( is.null(MAIN) ){
		oma = rep(0,4)
	} else {
		oma = c(0,0,2,0)
	}
	
	par(mfrow = c(num_vars,num_vars),
		mar = c(4,4,1,0.5),oma = oma)
	on.exit(par(oldpar))
	
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
	}}
	
	if( !is.null(MAIN) ) mtext(MAIN,outer=TRUE,cex=1.2)
	
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
	
	oldpar = par(no.readonly = TRUE)
	
	par(mar = c(mar_down,4,2,2) + 0.1)
	on.exit(par(oldpar))
	
	boxplot(MAT,xaxt = "n",col = "deepskyblue",...)
	axis(1,labels = FALSE,tick = FALSE)
	labels = colnames(MAT)
	text(x = seq_along(labels),
		y = par("usr")[3] - (par("usr")[4] - par("usr")[3])/30,
		srt = srt,adj = 1,labels = labels,cex = 0.8,xpd = TRUE)
	
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
	
	oldpar = par(no.readonly = TRUE)
	
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
	
	par(mfrow = c(num_rows,num_cols),
		mar = c(4,4,1,0.5))
	on.exit(par(oldpar))
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
smart_convCOLOR = function(MAT,max_cats = 5,overwrite = FALSE){
	cMAT = c()
	num_row = nrow(MAT)
	
	for(cc in seq(ncol(MAT))){
		uniq_elem = sort(unique(MAT[,cc]))
		uniq_num = length(uniq_elem)
		
		if( uniq_num == 1 ){
			tmp_col = rep("white",num_row)
		
		} else if( uniq_num %in% seq(2,max_cats) ){
			vec_col = smart_colors(nn = uniq_num,
				overwrite = overwrite)
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
smart_image = function(orig_mat,border = NULL,axes = FALSE,
	GRID = NULL,xaxs = "i",yaxs = "i",resMAR = FALSE,...){
	
	oldpar = par(no.readonly = TRUE)
	
	# Set GRID parameters
	if( is.null(GRID) ){
		GRID = list(GRID = FALSE)
	} else {
		if( is.null(GRID$GRID) ){
			GRID$GRID = FALSE
		}
		if( is.null(GRID$lwd) ){
			GRID$lwd = 0.1
		}
	}
	
	# Code for creating the heatmap
	if( !is.character(orig_mat) && is.numeric(orig_mat) ){
		# No border
		if( is.null(border) ){
			par(mar = rep(0,4))
		} else {
			par(mar = border)
		}
		on.exit(par(oldpar))
		
		# Orienting the matrix to how its organized
		if( length(dim(orig_mat)) == 2 ){
			image_mat = t(orig_mat[rev(seq(nrow(orig_mat))),,drop=FALSE])
		} else if( length(dim(orig_mat)) == 0 && is.null(nrow(orig_mat)) ){
			image_mat = as.matrix(orig_mat)
		}
		# Plot
		if( length(dim(image_mat)) == 2 ){
			num_rows = nrow(image_mat); num_cols = ncol(image_mat)
			x_coor = seq(0,1,1/(num_rows+1))[-c(1,num_rows+2)] - 0.5 /(num_rows+1)
			xlims = as.numeric(summary(x_coor)[c(1,6)] + c(-1,1) * (0.5 /(num_rows+1)))
			y_coor = seq(0,1,1/(num_cols+1))[-c(1,num_cols+2)] - 0.5 /(num_cols+1)
			ylims = as.numeric(summary(y_coor)[c(1,6)] + c(-1,1) * (0.5 /(num_cols+1)))
			
			image(x = x_coor, y = y_coor, z = image_mat, 
				axes = axes,xaxs = xaxs,yaxs = yaxs,
				xlim = xlims, ylim = ylims,
				...)
			
			if( GRID$GRID ){
				abline(v = c(0,x_coor + 0.5 /(num_rows+1)),
					h = y_coor + 0.5 /(num_cols+1),
					lty = 3,lwd = GRID$lwd,col = rgb(0,0,0,1))
			}
			
		} else if( length(dim(image_mat)) == 0 ){
			
			stop("No code for vector input yet")
		}
	}
	
	# If orig_mat is a character color matrix, convert then plot
	if( is.character(orig_mat) && !is.numeric(orig_mat) ){
		# orig_mat = bb_col
		numeric_mat = orig_mat
		numeric_mat = matrix(NA,nrow(orig_mat),ncol(orig_mat))
		mat_names = names(table(orig_mat))
		mat_colors = rep(NA,length(mat_names))
		count = 1
		for(mat_name in mat_names){
			mat_colors[count] = mat_name
			numeric_mat[orig_mat == mat_name] = count
			count = count + 1
		}
		# numeric_mat = matrix(as.numeric(numeric_mat), nrow = dim(numeric_mat)[1])
		smart_image(numeric_mat,col=mat_colors)
	}
	
}

#' @title smart_heatmap
#' @param MAT A numeric matrix of values
#' @param DIST Boolean set to TRUE to treat MAT as distance
#'	matrix. Otherwise, function can perform row/column clustering
#' @param main A string for the overall heatmap title
#' @param width NULL
#' @param height NULL
#' @param GRID NULL
#' @param clustRC NULL
#' @param nodePar_col NULL
#' @param nodePar_row NULL
#' @param mar NULL
#' @param cex.main NULL
#' @param rowData NULL
#' @param colData NULL
#' @param make_key NULL
#' @param vec_cols NULL
#' @export
smart_heatmap = function(MAT = NULL,DIST = FALSE,main = "",
	width = NULL,height = NULL,GRID = NULL,clustRC = c(TRUE,TRUE),
	nodePar_col = NULL,nodePar_row = NULL,mar = 2,cex.main = 2,
	rowData = NULL,colData = NULL,make_key = TRUE,vec_cols = NULL){
	
	if(FALSE){
		
		MAT = rr; DIST = !TRUE; main = "test"
		width = NULL; height = NULL; GRID = !TRUE; clustRC = c(FALSE,TRUE)
		nodePar_col = NULL; nodePar_row = NULL; mar = 2; cex.main = 2
		rowData = NULL
		
		# rowData = list(dat = sdat,vars = c("TMB_bin","P1"))
		colData = list(dat = dat2,vars = "SECTOR")
		
		
	}
	
	oldpar = par(no.readonly = TRUE)
	
	if( is.null(MAT) ) 		stop("Specify a matrix for MAT")
	if( is.null(width) ){
		if( is.null(rowData) ){
			width = c(1,3)
			# width = c(1,1,3)
		} else {
			width = c(1,3,1)
			if( !all(rownames(MAT) %in% rownames(rowData$dat)) )
				stop("rownames mismatch")
		}
	}
	if( is.null(height) ){
		if( is.null(colData) ){
			height = c(0.1,1,3)
		} else {
			height = c(0.1,1,3,1)
			if( !all(colnames(MAT) %in% rownames(colData$dat)) )
				stop("colnames mismatch")
		}
	}
	
	if( is.null(rowData) && is.null(colData) ){
		mm = matrix(c(5,5,4,1,2,3),3,2,byrow = TRUE)
	} else if( is.null(colData) ){
		mm = matrix(c(5,5,5,4,1,7,2,3,6),3,3,byrow = TRUE)
	} else if( is.null(rowData) ){
		mm = matrix(c(5,5,4,1,2,3,7,6),ncol = 2,byrow = TRUE)
	} else {
		mm = matrix(c(5,5,5,4,1,7,2,3,6,9,8,0),4,3,byrow = TRUE)
	}
	mm
	layout(mm,widths = width,heights = height)
	# layout.show(max(mm))
	
	if( DIST ) clustRC = rep(TRUE,2)
	
	# Check row/col names
	if( is.null(colnames(MAT)) ) colnames(MAT) = seq(ncol(MAT))
	if( is.null(rownames(MAT)) ) rownames(MAT) = seq(nrow(MAT))
	
	# hclust cols
	lab_col = colnames(MAT)
	if( clustRC[2] == TRUE ){
		
		if( DIST == FALSE ){
			h_col = hclust(dist(t(MAT)))
		} else {
			h_col = hclust(as.dist(MAT))
		}
		lab_col = h_col$labels[h_col$order]
		
		par(mar = c(mar,0,0,0))
		on.exit(par(oldpar))
		
		if( is.null(nodePar_col) )
			nodePar_col = list(lab.cex = 1,pch = rep(NA,2))
		if( is.null(nodePar_col$pch) )
			nodePar_col$pch = rep(NA,2)
		if( is.null(nodePar_col$lab.cex) )
			nodePar_col$lab.cex = 1
		
		plot(as.dendrogram(h_col),xaxs = "i",
			axes = FALSE,nodePar = nodePar_col)
		
	} else {
		
		par(mar = c(0,0,0,0))
		on.exit(par(oldpar))
		
		plot(0,0,xlim = c(0,1),ylim = c(0,1),
			xlab = "",ylab = "",
			type = "n",axes = FALSE,xaxs = "i")
		nc = ncol(MAT)
		cex = 1
		
		if( is.null(nodePar_col) )
			nodePar_col = list(lab.cex = 1,srt = 90)
		if( is.null(nodePar_col$lab.cex) )
			nodePar_col$lab.cex = 1
		if( is.null(nodePar_col$srt) )
			nodePar_col$srt = 90
		if( is.null(nodePar_col$adj) )
			nodePar_col$adj = ifelse(nodePar_col$srt == 90,0,0.5)
		if( is.null(nodePar_col$xx_shift) )
			nodePar_col$xx_shift = 0
		if( is.null(nodePar_col$yy_shift) )
			nodePar_col$yy_shift = 0
		
		text(x = seq(nc)/nc - 1/(nc*2) + nodePar_col$xx_shift,
			y = rep(0,nc) + nodePar_col$yy_shift,labels = lab_col,
			srt = nodePar_col$srt,cex = nodePar_col$lab.cex,
			adj = nodePar_col$adj)
		
	}
	
	# hclust rows
	lab_row = rownames(MAT)
	if( clustRC[1] == TRUE ){
		
		if( DIST == FALSE ){
			h_row = hclust(dist(MAT))
		} else {
			h_row = hclust(as.dist(MAT))
		}
		lab_row = h_row$labels[h_row$order]
		
		par(mar = c(0,0,0,mar))
		on.exit(par(oldpar))
		
		if( is.null(nodePar_row) )
			nodePar_row = list(lab.cex = 1,pch = c(NA,NA))
		if( is.null(nodePar_row$pch) )
			nodePar_row$pch = rep(NA,2)
		if( is.null(nodePar_row$lab.cex) )
			nodePar_row$lab.cex = 1
		
		plot(rev(as.dendrogram(h_row)),yaxs = "i",
			axes = FALSE,horiz = TRUE,nodePar = nodePar_row)
		
	} else {
		
		par(mar = rep(0,4))
		on.exit(par(oldpar))
		
		plot(0,0,xlim = c(0,1),ylim = c(0,1),
			type = "n",axes = FALSE,yaxs = "i")
		nr = nrow(MAT)
		cex = 1
		
		if( is.null(nodePar_row) )
			nodePar_row = list(lab.cex = 1)
		if( is.null(nodePar_row$lab.cex) )
			nodePar_row$lab.cex = 1
		if( is.null(nodePar_row$adj) )
			nodePar_row$adj = 1
		if( is.null(nodePar_row$xx_shift) )
			nodePar_row$xx_shift = 0
		if( is.null(nodePar_row$xx) )
			nodePar_row$xx = 1
		text(rep(nodePar_row$xx,nr) + nodePar_row$xx_shift,
			seq(nr,1)/nr - 1/(nr*2),
			labels = lab_row,cex = nodePar_row$lab.cex,
			adj = nodePar_row$adj)
		
	}
	
	# heatmap
	par(mar = rep(0,4))
	on.exit(par(oldpar))
	
	ncols = min(length(unique(c(MAT))),5e1)
	if( is.null(vec_cols) ){
		vec_cols = colorpanel(n = ncols,
			"deepskyblue","white","red")
	}
	num_cols = length(vec_cols)
	MAT2 = MAT[lab_row,lab_col]
	# if( DIST ) MAT2 = 1 - MAT[lab_row,lab_col]
	if( DIST ) MAT2 = MAT[lab_row,lab_col]
	smart_image(orig_mat = MAT2,col = vec_cols,
		GRID = GRID,resMAR = TRUE)
	
	# Key/legend
	par(mar = c(5,1,1,1))
	on.exit(par(oldpar))
	
	rr_mat = range(MAT2); rr_mat
	if( make_key && rr_mat[1] != rr_mat[2] ){
		rr_bar = seq(rr_mat[1],rr_mat[2],length.out = num_cols); # rr_bar
		n_bar = length(rr_bar); n_bar
		if( DIST == FALSE ){
			hi = hist(c(MAT2),plot = FALSE,breaks = c(rr_bar[1]*0.5,rr_bar)); # hi
		} else {
			hi = hist(MAT2[upper.tri(MAT2)],plot = FALSE,breaks = c(rr_bar[1]*0.5,rr_bar))
		}
		vec_cnts = hi$counts # don't care about white
		# vec_cnts[1] = 0
		vec_cnts = vec_cnts / sum(vec_cnts)
		if( vec_cnts[1] > 0.9 ) vec_cnts[1] = 0
		vec_cnts = vec_cnts / sum(vec_cnts)
		ncnt = length(vec_cnts); ncnt
		barplot(rep(max(vec_cnts),n_bar),col = vec_cols,
			border = NA,space = 0,axes = FALSE)
		
		par(lwd = 2)
		on.exit(par(oldpar))
		
		barplot(vec_cnts,border = "cyan",col = NA,space = 0,
			axes = FALSE,add = TRUE)
		
		par(lwd = 1)
		on.exit(par(oldpar))
		
		rr_lab = smart_SN(seq(rr_mat[1],rr_mat[2],
			length.out = 10),digits = 1); # rr_lab
		axis(1,at = seq(1,n_bar,length.out = length(rr_lab)) - 1/2,
			labels = rr_lab,las = 2,cex.axis = 0.75)
	} else {
		par(mar = rep(0,4))
		on.exit(par(oldpar))
		
		plot(0,0,type = "n",axes = FALSE)
	}
	
	# Main/Title
	par(mar = rep(0,4))
	on.exit(par(oldpar))
	
	plot(0,0,xlim = c(0,1),ylim = c(0,1),type = "n",axes = FALSE)
	text(0.5,0.5,labels = main,cex = cex.main)
	
	# Row Data
	if( !is.null(rowData) ){
		par(mar = rep(0,4))
		on.exit(par(oldpar))
		
		# Convert columns to character
		rdat = smart_convCOLOR(MAT = rowData$dat[lab_row,rowData$vars,drop = FALSE])
		# class(rdat); head(rdat)
		smart_image(orig_mat = rdat,GRID = GRID,resMAR = TRUE)
		
		# Row labels
		par(mar = rep(0,4))
		on.exit(par(oldpar))
		
		plot(0,1,type = "n",xlim = c(0,1),ylim = c(0,1),
			xaxs = "i",yaxs = "i",axes = FALSE)
		nvars = length(rowData$vars)
		text(x = seq(1,2*nvars,2)/(2*nvars),y = rep(0.2,nvars),
			labels = rowData$vars,adj = 0.5,srt = 45)
	}
	
	# Col Data
	if( !is.null(colData) ){
		par(mar = rep(0,4))
		on.exit(par(oldpar))
		
		# Convert columns to character
		cdat = smart_convCOLOR(MAT = colData$dat[lab_col,colData$vars,drop = FALSE])
		# class(cdat); head(cdat)
		smart_image(orig_mat = t(cdat),GRID = GRID,resMAR = TRUE)
		
		# Row labels
		par(mar = rep(0,4))
		on.exit(par(oldpar))
		
		plot(0,1,type = "n",xlim = c(0,1),ylim = c(0,1),
			xaxs = "i",yaxs = "i",axes = FALSE)
		nvars = length(colData$vars)
		text(x = rep(0.75,nvars),y = rev(seq(1,2*nvars,2))/(2*nvars),
			labels = colData$vars,adj = 0.5,srt = 0)
	}
	
	return(MAT2)
	
}


###

