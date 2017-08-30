#' Adding bars to an existing plot.
#' 
#' @export
#' @import stats
#' @import graphics
#' @param x Numeric vector with x-positions of bars.
#' @param y Numeric vector with height of the bars.
#' @param y0 Optional numeric value or vector with the onset(s) of the bars. 
#' When \code{y0} is not specified, the lowest value of the y-axis is used.
#' @param width Numeric value, determining the width of the bars in units of 
#' the x-axis.
#' @param horiz Logical value: whether or not to plot horizontal bars. 
#' Defaults to FALSE.
#' @param ... Other arguments for plotting, see \code{\link[graphics]{par}}.
#' @author Jacolien van Rij
#' @examples
#' # hypothetical experiment:
#' adults   =  stats::rpois(100, lambda = 5)
#' children =  stats::rpois(100, lambda = 4)
#' newd <- data.frame(Adults = table( factor(adults, levels=0:15) ),
#'     Children = table( factor(children, levels=0:15) ) )
#' newd <- newd[,c(1,2,4)]
#' names(newd)[1] <- "value"
#' 
#' # barplot of Adults:
#' b <- barplot(newd$Adults.Freq, beside=TRUE, names.arg=newd$value, border=NA, ylim=c(0,30))
#' # overlay Children measures:
#' add_bars(b, newd$Children.Freq, col='red', density=25, xpd=TRUE)
#' 
#' # variants:
#' b <- barplot(newd$Adults.Freq, beside=TRUE, names.arg=newd$value, border=NA, ylim=c(0,30))
#' add_bars(b+.1, newd$Children.Freq, width=.85, col=alpha('red'), border=NA, xpd=TRUE)
#' 
#' emptyPlot(c(-30,30), c(0,15), v0=0, ylab="Condition")
#' add_bars(-1*newd$Children.Freq, 0:15, y0=0, col=alpha("blue"), 
#'     border="blue", horiz=TRUE)
#' add_bars(newd$Adults.Freq, 0:15, y0=0, col=alpha("red"), 
#'     border="red", horiz=TRUE)
#' mtext(c("Children", "Adults"), side=3, at=c(-15,15), line=1, cex=1.25, font=2)
#' 
#' # adding shadow:
#' b <- barplot(newd$Adults.Freq, beside=TRUE, names.arg=newd$value, with=.9, col='black', border=NA)
#' add_bars(b+.2, newd$Adults.Freq+.2, y0=.2, width=.9, col=alpha('black', f=.2), border=NA, xpd=TRUE)
#' 
#' @family Functions for plotting
add_bars <- function(x, y, y0=NULL, width=1, horiz=FALSE, ...){
	# make sure x and y are vectors:
	x = as.vector(x)
	y = as.vector(y)
	if(length(x) != length(y)){
		stop("Different lengths of x and y.")
	}
	gpc <- getFigCoords('p')
	par = list(...)
	col = NULL
	if(!'col' %in% names(par)){
		par[['col']] <- 1
	}
	if(is.null(y0)){
		min.y <- findAbsMin(c(0,gpc[3]))
		y0 = rep(min.y, length(y))
		if(horiz==TRUE){
			min.y <- findAbsMin(c(0,gpc[1]))
			y0 = rep(min.y, length(y))
		}
	}else{
		if(length(y0)==1){
			y0 = rep(y0,length(y))
		}else if(length(y0) != length(y)){
			warning("Length y0 does not equal length y. The first element of y0 will be used.")
			y0 = rep(y0[1], length(y))
		}
	}
	if(length(width)==1){
		width = rep(width, length(x))
	}else if(length(width) != length(x)){
		warning("Length of width does not equal length x. The first element of width will be used.")
		width = rep(width[1], length(x))
	}
	addargs <- list2str(names(par), par)
	if(horiz==TRUE){
		eval(parse(text=sprintf("rect(xleft=x, xright=y0,
			ybottom=y-.5*width, ytop=y+.5*width,%s)", addargs) ))
	}else{
		eval(parse(text=sprintf("rect(xleft=x-0.5*width, xright=x+0.5*width,
			ybottom=y0, ytop=y,%s)", addargs) ))
	}
	
}





#' Add groups of points to a plot
#'
#' @export
#' @import grDevices
#' @import graphics
#' @description Add groups of points to a plot
#' 
#' @param x average X position of points to plot,
#' @param y average Y position of points to plot,
#' @param n number of points to plot.
#' @param horiz Logical: whether or not to plot the sequence of point in 
#' horizontal direction (x-axis). Defaults to TRUE, the points are plotted in 
#' horizontal direction.
#' @param width Numeric value: width that sequence of points can take.
#' @param sep Numeric value: separation between sequences of points. 
#' Separation reduces the width. If the value is smaller, the sequences take 
#' more space.
#' @param ... Optional graphical parameters (see \code{\link[graphics]{par}}).
#' @author Jacolien van Rij
#' @examples
#' 
#' s <- table(cars$speed)
#' d <- tapply(cars$dist, list(cars$speed), mean)
#' 
#' emptyPlot(range(as.numeric(names(s))), range(d), 
#'     xlab="dist", ylab="mean speed")
#' add_n_points(as.numeric(names(s)), d, s)
#' 
#' # decrease space between groups of points:
#' emptyPlot(range(as.numeric(names(s))), range(d), 
#'     xlab="dist", ylab="mean speed")
#' add_n_points(as.numeric(names(s)), d, s, sep=0)
#' 
#' # decrease width of groups of points:
#' emptyPlot(range(as.numeric(names(s))), range(d), 
#'     xlab="dist", ylab="mean speed")
#' add_n_points(as.numeric(names(s)), d, s, width=0.8)
#' 
#' # horizontal vs vertical:
#' emptyPlot(range(d),range(as.numeric(names(s))),  
#'     ylab="dist", xlab="mean speed")
#' add_n_points(d, as.numeric(names(s)), s, horiz=FALSE) 
#' 
#' @family Functions for plotting
add_n_points <- function(x, y, n, horiz=TRUE, width=1, sep=.2, ...){
  max.n <- max(n, na.rm=TRUE)
  d <- diff(seq(-1*(width-(sep))/2,  (width-(sep))/2, length=max.n)[1:2])
  if(horiz==TRUE){
  	x.new <- unlist(mapply(function(a, b){
    	xpos = scale(1:b, center=TRUE, scale=FALSE)*d + a
  	}, as.list(x), as.list(n)))
  	y.new <- rep(y, n)
  	points(x.new, y.new, ...)
  }else{
  	y.new <- unlist(mapply(function(a, b){
    	ypos = scale(1:b, center=TRUE, scale=FALSE)*d + a
  	}, as.list(y), as.list(n)))
  	x.new <- rep(x, n)
  	points(x.new, y.new, ...)  	
  }
}





#' Draw intervals or arrows on plots.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @description Add horizontal or vertical interval indications. 
#' This function can also be used to plot asymmetric (non-parametric)
#' error bars or confidence intervals. Basically a wrapper around arrows.
#' 
#' @param pos Vector with x- or y-values (depending on \code{horizontal}).
#' @param lowVals Vector with low values, .
#' @param highVals Vector with errors or confidence bands.
#' @param horiz Logical: whether or not to plot the intervals horizontally.
#' Defaults to TRUE (horizontal intervals).
#' @param minmax Optional argument, vector with two values indicating the 
#' minimum and maximum value for the error bars. If NULL (default) the error 
#' bars are not corrected.
#' @param length Number, size of the edges in inches.
#' @param ... Optional graphical parameters (see \code{\link[graphics]{par}})
#' to be forwarded to the function \code{\link[graphics]{arrows}}.
#' @author Jacolien van Rij
#' @examples
#' emptyPlot(1000,5, xlab='Time', ylab='Y')
#' # add interval indication for Time=200 to Time=750:
#' addInterval(1, 200, 750, lwd=2, col='red')
#'
#' # zero-length intervals also should work:
#' addInterval(pos=521, lowVals=c(1.35, 1.5, 4.33), highVals=c(1.15,1.5, 4.05),
#'     horiz=FALSE, length=.1, lwd=4)
#'
#' # combine with getCoords for consistent positions with different axes:
#' par(mfrow=c(2,2))
#' # 1st plot:
#' emptyPlot(1000,c(-1,5), h0=0)
#' addInterval(getCoords(.1,side=2), 200,800, 
#'     col='red', lwd=2)
#' addInterval(getCoords(.5,side=1), 1,4, horiz=FALSE,
#'     col='blue', length=.15, angle=100, lwd=4)
#' abline(h=getCoords(.1, side=2), lty=3, col='red', xpd=TRUE)
#' abline(v=getCoords(.5, side=1), lty=3, col='blue', xpd=TRUE)
#' # 2nd plot:
#' emptyPlot(1000,c(-250, 120), h0=0)
#' addInterval(getCoords(.1,side=2), 750,1200, 
#'     col='red', lwd=2, minmax=c(0,1000))
#' abline(h=getCoords(.1, side=2), lty=3, col='red', xpd=TRUE)
#' # 3rd plot:
#' emptyPlot(c(-50,50),c(20,120), h0=0)
#' addInterval(getCoords(.5,side=1), 80,120, horiz=FALSE,
#'     col='blue', code=2, length=.15, lwd=4, lend=1)
#' abline(v=getCoords(.5, side=1), lty=3, col='blue', xpd=TRUE)
#'
#' # Alternative boxplots: 
#' b <- boxplot(count ~ spray, data = InsectSprays, plot=FALSE)$stats
#' emptyPlot(c(1,6), range(b[c(1,5),]), h0=0)
#' addInterval(1:6, b[1,], b[5,], horiz=FALSE)
#' # no end lines:
#' addInterval(1:6, b[2,], b[4,], horiz=FALSE, lwd=8, length=0, lend=2)
#' # no error with zero-length intervals:
#' addInterval(1:6, b[3,], b[3,], horiz=FALSE, lwd=2, length=.1, lend=2)
#' 
#' # reset
#' par(mfrow=c(1,1))
#' @family Functions for plotting
addInterval <- function(pos, lowVals, highVals, horiz=TRUE, minmax=NULL, length=.05,...){
    convert2num <- function(x){  
        if(!any(c("numeric", "integer") %in% class(x))){
            if("factor" %in% class(x)){
                return( as.numeric( as.character(x)) )
            }else{
                return( as.vector(unlist(x)) )
            }
        }else{
            return(x)
        }
    }
    pos <- convert2num(pos)
    lowVals <- convert2num(lowVals)
    highVals <- convert2num(highVals)
    if(!is.null(minmax)){
        lowVals[!is.na(lowVals) & lowVals < minmax[1]] <- minmax[1]
        highVals[!is.na(highVals) & highVals > minmax[2]] <- minmax[2]
    }
    if(length(lowVals) != length(highVals)){
        if(length(lowVals)==1){
            lowVals <- rep(lowVals, length(highVals))
        }else if(length(highVals)==1){
            highVals <- rep(highVals, length(lowVals))          
        }else{
            stop('Vectors lowVals and highVals do not have same length.')
        }
    }
    if(length(pos)==1){
        pos <- rep(pos, length(lowVals))
    }else if(length(pos) != length(lowVals)){
        stop('Vector pos should be of the same length as lowVals and highVals.')
    }
    dnm <- names(list(...))
    pars <- list()
    if(!"angle" %in% dnm){
        pars[["angle"]] <- 90
    }
    if(!"code" %in% dnm){
        pars[["code"]] <- 3
    }
    if(length(pars) > 0){
        pars <- paste(paste(names(pars),pars, sep='='), collapse=',')
    }else{
        pars <- NULL
    }
    len.check <- highVals - lowVals
    len.check <- which(len.check == 0)
    if(length(len.check)>0){
        usr <- par()$usr
        pin <- par()$pin
        
        if(horiz){
            len <- ((usr[4]-usr[3])*length) / pin[2]
            segments(x0=lowVals[len.check], x1=lowVals[len.check], y0=pos-len, y1=pos+len, ...)
        }else{
            len <- ((usr[2]-usr[1])*length) / pin[1]
            segments(y0=lowVals[len.check], y1=lowVals[len.check], x0=pos-len, x1=pos+len, ...)
        }
        # pos <- pos[-len.check]
        # lowVals <- lowVals[-len.check]
        # highVals <- highVals[-len.check]
        # set of warnings
        options(warn=-1)
    }
    if(horiz){
        if(is.null(pars)){
            arrows(x0=lowVals, x1=highVals, y0=pos, y1=pos, length=length, ...)
        }else{
            eval(parse(text=paste('arrows(x0=lowVals, x1=highVals, y0=pos, y1=pos,length=length,', pars ,',...)', sep='')))
        }
    }else{
        if(is.null(pars)){
            arrows(y0=lowVals, y1=highVals, x0=pos, x1=pos, length=length, ...)
        }else{
            eval(parse(text=paste('arrows(y0=lowVals, y1=highVals, x0=pos, x1=pos, length=length,', pars ,',...)', sep='')))
        }        
    }
    if(length(len.check)>0){
        options(warn=0)
    }
}





#' Adjusting the transparency of colors.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @description Wrapper around \code{\link[grDevices]{adjustcolor}}.
#' 
#' @param x A color or a vector with color values.
#' @param f A number for adjusting the transparency ranging from 0 (completely 
#' transparent) to 1 (not transparent).
#'
#' @family Utility functions for plotting
#' @section Note: 
#' Does not always work for x11 panels.
#' @examples
#' emptyPlot(100,100, h=50, v=50)
#' rect(25,25,75,75, col=alpha('red',f=1))
#' rect(35,41,63,81, col=alpha(rgb(0,1,.5),f=.25), 
#'    border=alpha(rgb(0,1,.5), f=.65), lwd=4)
#' 
#' emptyPlot(1,1, axes=FALSE, main="Tunnel of 11 squares")
#' center <- c(.75, .25)
#' mycol <- "steelblue"
#' for(i in seq(0,1,by=.1)){
#'     rect(center[1]-center[1]*(1.1-i), center[2]-center[2]*(1.1-i), 
#'         center[1]+(1-center[1])*(1.1-i), center[2]+(1-center[2])*(1.1-i), 
#'         col=alpha(mycol, f=i), border=mycol, lty=1, lwd=.5, xpd=TRUE)
#' }
#' axis(1, at=center[1]-center[1]*(1.1-seq(0,1,by=.1)), labels=seq(0,1,by=.1))
#' 
#' # see alphaPalette for an elaboration of this example
#' 
#' @family Functions for plotting
alpha <- function(x, f = 0.5) {
    if(f > 1 | f < 0){
        stop("Transparency value should be in range 0 to 1.")
    }else{
        return( adjustcolor(x, alpha.f = f) )
    }
}





#' Manipulate the transparency in a palette.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @description Generate an color palette with changing transparency.
#' 
#' @param x A vector with color values. Could be a single value specifying a 
#' single color palette, ranging in transparency values, or a vector with 
#' different colors. 
#' @param f.seq A vector with transparency values, ranging from 0 to 1.
#' @param n Optional argument. A number specifying the number of colors in the 
#' palette. If \code{n} > 1, then N transparency values are generated ranging  
#' from the minimum of \code{f.seq} to the maximum of \code{f.seq}. \code{n} 
#' will only be used when the vector \code{f.seq} has two elements or more.
#' @return A vector with color values.
#' @author Jacolien van Rij
#' @section Warning:
#' On Linux \code{\link{x11}} devices may not support transparency. 
#' In that case, a solution might be to write the plots immediately to a file 
#' using functions such as \code{\link{pdf}}, or \code{\link{png}}.
#' @seealso 
#' \code{\link[grDevices]{palette}}, \code{\link[grDevices]{colorRampPalette}},
#' \code{\link[grDevices]{adjustcolor}}, \code{\link[grDevices]{convertColor}}
#' @examples 
#' # a palette of 5 white transparent colors:
#' alphaPalette('white', f.seq=1:5/5)
#' # the same palette:
#' alphaPalette('white', f.seq=c(.2,1), n=5)
#' # a palette with 10 colors blue, yellow and red, that differ in transparency
#' alphaPalette(c('blue', "yellow", "red"), f.seq=c(0.1,.8), n=10)
#' 
#' emptyPlot(1,1, axes=FALSE, main="Tunnel of 11 squares")
#' mycol <- "steelblue"
#' center <- c(.75, .25)
#' i = seq(0,1,by=.1)
#' fillcol <- alphaPalette(c(mycol, "black"), f.seq=i)
#' linecol <- alphaPalette(mycol, f.seq=1-i)
#' rect(center[1]-center[1]*(1.1-i), center[2]-center[2]*(1.1-i), 
#'     center[1]+(1-center[1])*(1.1-i), center[2]+(1-center[2])*(1.1-i), 
#'     col=fillcol, border=linecol, lty=1, lwd=1, xpd=TRUE)
#'
#' @family Functions for plotting
alphaPalette <- function(x, f.seq, n=NULL) {
    out <- c()
    if(!is.null(n)){
        if(n[1]>1 & length(f.seq) == 2){
            f.seq <- seq(min(max(c(f.seq[1],0)),1), min(max(c(f.seq[2],0)),1), length=n)
        } else if(n[1]>1 & length(f.seq) > 2){
            f.seq <- seq(min(f.seq), max(f.seq), length=n)
            warning("N values between the min and max of f.seq are selected.")
        }else{
            n <- length(f.seq)
            warning("Argument n will be ignored.")
        }
    }else{
    	n <- length(f.seq)
    }
    if (length(x) == length(f.seq)) {
        out <- x
    } else if(length(x) == 1){
        out <- rep(x[1], length(f.seq))
    }else{
        x <- colorRampPalette(x)(n)
        out <- x
    }
    return(mapply(function(a, b) {
        alpha(a, b)
    }, out, f.seq, USE.NAMES = FALSE))
}





#' Compare distribution of data with normal distribution.
#' 
#' @export
#' @import stats
#' @import grDevices
#' @import graphics
#' @param res Vector with residuals or other data for which the distribution .
#' @param col Color for filling the area. Default is black.
#' @param col.normal Color for shading and line of normal distribution.
#' @param legend.pos Position of legend, can be string (e.g., 'topleft') or an 
#' \code{\link[grDevices]{xy.coords}} object.
#' @param legend.label Text string, label for plotted data distribution.
#' @param ... Optional arguments for the lines. See \code{\link{par}}.
#' @section Note:
#' Assumes centered data as input.
#' @examples
#' set.seed(123)
#' # normal distribution:
#' test <- rnorm(1000)
#' check_normaldist(test)
#' # t-distribution:
#' test <- rt(1000, df=5)
#' check_normaldist(test)
#' # skewed data, e.g., reaction times:
#' test <- exp(rnorm(1000, mean=.500, sd=.25))
#' check_normaldist(test)
#' # center first:
#' check_normaldist(scale(test))
#' # binomial distribution:
#' test <- rbinom(1000, 1, .3)
#' check_normaldist(test)
#' # count data:
#' test <- rbinom(1000, 100, .3)
#' check_normaldist(test)
#' @family Functions for plotting
#' @author Jacolien van Rij
check_normaldist <- function(res, col='red', col.normal='black', 
	legend.pos='topright', legend.label='data', ...){
    x <- sort(res[!is.na(res)])
    sd.x <- sd(x)
    d <- density(x)
    d.norm <- dnorm(d$x, mean=mean(x), sd=sd.x)
    parlist <- list(...)
    emptyPlot(range(d$x), range(c(d$y, d.norm)),
        main='Density', xlab=deparse(substitute(res)))
    fill_area(d$x, d.norm, col=col.normal)
    lines(d$x, d.norm, col=col.normal)
    if('lwd' %in% names(parlist)){
        lwd <- NULL
        lines(d$x, d$y, col=col, ...)
    }else{
        lines(d$x, d$y, col=col, lwd=2, ...)
    }
    
    if(!is.null(legend.pos)){
        legend(legend.pos, 
            legend=legend.label,
            col=c(col, col.normal), seg.len=1,
            lwd=c(ifelse('lwd' %in% names(parlist), parlist[['lwd']], 2), 1),
            bty='n')
    }
}





#' Creates a contour plot with colored background.
#'
#' @description This function is a wrapper around \code{\link[graphics]{image}}
#' and \code{\link[graphics]{contour}}. See \code{vignette("plotfunctions")} 
#' for an example of how you could use \code{\link[graphics]{image}} and 
#' \code{\link[graphics]{contour}}.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @param x Locations of grid lines at which the values in z are measured. 
#' These must be in ascending order. By default, equally spaced values from 0 
#' to 1 are used. If x is a list, its components x$x and x$y are used for x 
#' and y, respectively. If the list has component z this is used for z.
#' @param y Locations of grid lines at which the values in z are measured. 
#' @param z a matrix containing the values to be plotted (NAs are allowed). 
#' Note that x can be used instead of z for convenience.
#' @param main Text string, an overall title for the plot.
#' @param xlab Label for x axis. Default is name of first \code{view} variable.
#' @param ylab Label for y axis. Default is name of second \code{view} 
#' variable.
#' @param xlim x-limits for the plot.
#' @param ylim y-limits for the plot.
#' @param zlim z-limits for the plot.
#' @param col Color for the  contour lines and labels.
#' @param color a list of colors such as that generated by 
#' \code{\link[grDevices]{rainbow}}, \code{\link[grDevices]{heat.colors}}
#' \code{\link[grDevices]{colors}}, \code{\link[grDevices]{topo.colors}}, 
#' \code{\link[grDevices]{terrain.colors}} or similar functions.
#' @param nCol The number of colors to use in color schemes.
#' @param add.color.legend Logical: whether or not to add a color legend. 
#' Default is TRUE. If FALSE (omitted), one could use the function
#' \code{\link{gradientLegend}} to add a legend manually at any position.
#' @param ... Optional parameters for \code{\link[graphics]{image}}
#' and \code{\link[graphics]{contour}}.
#' @author Jacolien van Rij
#' @seealso \code{\link[graphics]{image}}, \code{\link[graphics]{contour}},
#' \code{\link[graphics]{filled.contour}}. See \code{\link{plotsurface}}
#' for plotting model predictions using \code{color_contour}.
#' @examples
#'
#' # Volcano example of R (package datasets)
#' color_contour(z=volcano)
#' # change color and lines:
#' color_contour(z=volcano, color='terrain', col=alpha(1), lwd=2, lty=5)
#' # change x-axis values and zlim:
#' color_contour(x=seq(500,700, length=nrow(volcano)),
#'     z=volcano, color='terrain', col=alpha(1), lwd=2, zlim=c(0,200))
#'
#' # compare with similar functions:
#' filled.contour(volcano, color.palette=terrain.colors)
#' 
#' # without contour lines:
#' color_contour(z=volcano, color='terrain', lwd=0, drawlabels=FALSE)
#' # without background:
#' color_contour(z=volcano, color=NULL, add.color.legend=FALSE)
#' @family Functions for plotting
#' @seealso \code{\link{plotsurface}}
color_contour <- function(x = seq(0, 1, length.out = nrow(z)),
    y = seq(0, 1, length.out = ncol(z)),
    z,
    main=NULL, xlab=NULL, ylab=NULL, 
    xlim=NULL, ylim=NULL, zlim=NULL,
    col=NULL, color=topo.colors(50), nCol=50,
    add.color.legend=TRUE, ...){
    # check input:
    if(is.null(dim(z))){
        stop('z should be a matrix.')
    }
    if(length(x) != nrow(z)){
        stop(sprintf('x should have %d values, because z has %d rows.', nrow(z), nrow(z)))
    }
    if(length(y) != ncol(z)){
        stop(sprintf('y should have %d values, because z has %d columns.', ncol(z), ncol(z)))
    }
        
    ## Check plot settings
    if(is.null(main)){
        main=""
    }
    if(is.null(xlab)){
        xlab=""
    }
    if(is.null(ylab)){
        ylab=""
    }
    if(is.null(xlim)){
        xlim=range(x)
    }
    if(is.null(ylim)){
        ylim=range(y)
    }   
    if(is.null(zlim)){
        zlim=range(z)
    }   
    # colors:
    if(is.null(color)){
        color <- alphaPalette('white', f.seq=c(0,0), n=nCol)
    }
    if (color[1] == "heat") {
        color <- heat.colors(nCol)
        if(is.null(col)){
            col <- 3
        }
    } else if (color[1] == "topo") {
        color <- topo.colors(nCol)
        if(is.null(col)){
            col <- 2
        }
    } else if (color[1] == "cm") {
        color <- cm.colors(nCol)
        if(is.null(col)){
            col <- 1
        }
    } else if (color[1] == "terrain") {
        color <- terrain.colors(nCol)
        if(is.null(col)){
            col <- 2
        }
    } else if (color[1] == "bpy") {
        if (requireNamespace("sp", quietly = TRUE)) {
            color <- sp::bpy.colors(nCol)
            if(is.null(col)){
                col <- 3
            }
        } else {
            warning("Package 'sp' needed for bpy color palette. Using topo.colors instead (default).")
            color <- topo.colors(nCol)
            col <- 2
        }
    } else if (color[1] == "gray" || color[1] == "bw") {
        color <- gray(seq(0.1, 0.9, length = nCol))
        col <- 1
    }else {
            if( all(isColor(color)) ){
                color <- colorRampPalette(color)(nCol)
            }else{
                stop("color scheme not recognised")
            }  
    }
    if (is.null(col)){
        col <- 'black'
    } 
    dnm <- list(...)
    parlist <- names(dnm)
    type2string <- function(x){
        out <- ""
        if(length(x)>1){
            if(is.character(x)){
                out <- sprintf("c(%s)", paste(sprintf("'%s'", x), collapse=','))
            }else{
                out <- sprintf("c(%s)", paste(x, collapse=','))
            }
        }else{
            if(is.character(x)){
                out <- sprintf("'%s'", x)
            }else{
                out <- sprintf("%s", x)
            }
        }
        return(out)
    }
    # check contour input:
    cpar <- c()
    contourarg <- c('nlevels', 'levels', 'labels', 'labcex', 'drawlabels', 'method', 'lty', 'lwd')
    for(i in parlist[parlist %in% contourarg] ){
        cpar <- c(cpar, sprintf("%s=%s", i, type2string(dnm[[i]])))
    }
    cpar <- paste(",", paste(cpar, collapse=','))
    cpar2 <- c()
    for(i in parlist[parlist %in% c('nlevels', 'levels', 'method')] ){
        cpar2 <- c(cpar2, sprintf("%s=%s", i, type2string(dnm[[i]])))
    }
    cpar2 <- paste(",", paste(cpar2, collapse=','))
    # check image input:
    ipar <- c()
    contourarg <- c('nlevels', 'levels', 'labels', 'labcex', 'drawlabels', 'method', 'lty', 'lwd')
    for(i in parlist[!parlist %in% contourarg] ){
        ipar <- c(ipar, sprintf("%s=%s", i, type2string(dnm[[i]])))
    }
    ipar <- paste(",", paste(ipar, collapse=','))
    eval(parse(text=sprintf("image(x, y, z, col=color, xlim=xlim, ylim=ylim, zlim=zlim, main=main, xlab=xlab, ylab=ylab, add=FALSE%s)", ipar)))
    eval(parse(text=sprintf("contour(x, y, z, col=col, add=TRUE%s)",
        cpar)))
    if(add.color.legend){
        gradientLegend(round(zlim, 3), n.seg=3, pos=.875, 
            color=color)
    }
}





#' Utility function
#' 
#' @export
#' @export
#' @import grDevices
#' @import graphics
#' @import stats
#' @import datasets
#' @description Adjusted version of the a Cleveland dot plot implemented in 
#' \code{\link[graphics]{dotchart}} with the option to add confidence 
#' intervals.
#' 
#' @param x  either a vector or matrix of numeric values (NAs are allowed). 
#' If x is a matrix the overall plot consists of juxtaposed dotplots for each 
#' row. Inputs which satisfy is.numeric(x) but not is.vector(x) || is.matrix(
#' x) are coerced by as.numeric, with a warning.
#' @param se.val a vector or matrix of numeric values representing the 
#' standard error or confidence bands.
#' @param labels a vector of labels for each point. For vectors the default is 
#' to use names(x) and for matrices the row labels dimnames(x)[[1]].
#' @param groups  an optional factor indicating how the elements of x are 
#' grouped. If x is a matrix, groups will default to the columns of x.
#' @param gdata data values for the groups. This is typically a summary such 
#' as the median or mean of each group.
#' @param cex the character size to be used. Setting cex to a value smaller
#' than one can be a useful way of avoiding label overlap. Unlike many other 
#' graphics functions, this sets the actual size, not a multiple of par("cex").
#' @param pch the plotting character or symbol to be used.
#' @param gpch the plotting character or symbol to be used for group values.
#' @param bg  the background color of plotting characters or symbols to be 
#' used; use par(bg= *) to set the background color of the whole plot.
#' @param color the color(s) to be used for points and labels.
#' @param gcolor the single color to be used for group labels and values.
#' @param lcolor the color(s) to be used for the horizontal lines.
#' @param xlim horizontal range for the plot, see plot.window, e.g.
#' @param main overall title for the plot, see title.
#' @param xlab x-axis annotation as in title.
#' @param ylab y-axis annotation as in title.
#' @param lwd with of error bars.
#' @param ... graphical parameters can also be specified as arguments
#' see \code{\link[graphics]{par}}
#' @author This function is a slightly adjusted version of the function 
#' \code{\link[graphics]{dotchart}} of the package \code{\link{graphics}} 
#' version 3.1.1
#' @examples
#' 
#' # example InsectSprays from R datasets
#' avg <- aggregate(count ~ spray, data=InsectSprays, mean)
#' avg <- merge(avg, 
#'     aggregate(count ~ spray, data=InsectSprays, sd),
#'     by="spray", all=TRUE)
#' 
#' dotplot_error(avg$count.x, se.val=avg$count.y, labels=avg$spray)
#' 
#' # we could add the type of spray to the averages:
#' avg$type <- c(1,1,2,2,2,1)
#' dotplot_error(avg$count.x, se.val=avg$count.y, groups=avg$type, labels=avg$spray) 
#' 
#' @seealso \code{\link[graphics]{dotchart}}
#' @family Functions for plotting
dotplot_error <- function (x, se.val=NULL, labels = NULL, groups = NULL, 
    gdata = NULL, cex = par("cex"), 
    pch = 21, gpch = 21, bg = "black", color = par("fg"), gcolor = par("fg"), 
    lcolor = "gray", xlim = NULL, main = NULL, 
    xlab = NULL, ylab = NULL, lwd=1, ...) 
{
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector or matrix")
    n <- length(x)
    if(!is.null(se.val)){
        if(length(x) != length(se.val)){
            warning("se.val not equal in length as x. se.val will be ignored.")
            se.val <- NULL
        }
    }
    if (is.matrix(x)) {
        if (is.null(labels)) 
            labels <- rownames(x)
        if (is.null(labels)) 
            labels <- as.character(1L:nrow(x))
        labels <- rep_len(labels, n)
        if (is.null(groups)) 
            groups <- col(x, as.factor = TRUE)
        glabels <- levels(groups)
    }
    else {
        if (is.null(labels)) 
            labels <- names(x)
        glabels <- if (!is.null(groups)) 
            levels(groups)
        if (!is.vector(x)) {
            warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
            x <- as.numeric(x)
        }
        if(! is.null(se.val)){
            if (!is.vector(se.val)) {
                warning("'se.val' is neither a vector nor a matrix: using as.numeric(se.val)")
                se.val <- as.numeric(se.val)
            }
        }
    }
    if(is.null(xlim)){
        xlim <- range(x[is.finite(x)])
        if(!is.null(se.val)){
            xlim <- range(c(x[is.finite(x)]-se.val[is.finite(se.val)], x[is.finite(x)]+se.val[is.finite(se.val)]))
        }
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- sort.list(as.numeric(x), decreasing = TRUE)
        x <- x[o]
        y <- 1L:n
        ylim <- c(0, n + 1)
    }
    else {
        o <- group_sort(x, group=groups, decreasing = TRUE)
        x <- x[o]
        if(!is.null(se.val)){
            se.val <- se.val[o]
        }
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        bg <- rep_len(bg, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
            col = color, las = 2, cex = cex, ...)
    }
    abline(h = y, lty = "dotted", col = lcolor)
    if(!is.null(se.val)){
        segments(x0=x-se.val, x1=x+se.val, y0=y, y1=y, col=color, lwd=lwd)
    }
    points(x, y, pch = pch, col = color, bg = bg)
    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                ...)
        }
    }
    axis(1)
    box()
    title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
}





#' Draw arrows between different plots.
#'
#' @export
#' @import graphics
#' @import grDevices
#' @description Function for drawing arrows between different plot regions. 
#'
#' @param start The x and y coordinates of a set of points that define 
#' the start points of the arrow(s), specified in a 
#' list with x and y slots. Similar to \code{\link[grDevices]{xy.coords}}.
#' Alternatively, start could be a matrix with start and end points in 
#' two columns. In that case, \code{end} is set to NULL.
#' @param end The x and y coordinates of a set of points that define 
#' the end points of the arrow(s), specified in a 
#' list with x and y slots.
#' @param arrows On which end of the line to draw arrows: 
#' "end" (default), "start", "both", "none".
#' @param units Units in which x- and y-coordinates are provided:
#' "inch" (default), "prop" (proportion), "coords". "inch" and "prop" are 
#' with respect to device region.
#' @param ... graphical parameters and parameters provided for 
#' \code{\link[graphics]{arrows}}.
#' 
#' @author Jacolien van Rij
#' @family Functions for plotting
#'
#' @examples
#'
#' ### EXAMPLE 1 ################################
#' 
#' # setup 4 panels:
#' par(mfrow=c(2,2))
#' 
#' #------------------
#' # PLOT 1: two points
#' #------------------
#' 
#' plot(0.5, 0.5, main="1", 
#' 	pch=21, lwd=3, col='red', bg='white', cex=1.2)
#' points(.5, .375, pch=22, lwd=3, col="blue", cex=1.2)
#' 
#' # Draw an error between the two points:
#' drawDevArrows(start=c(.5,.5), end=c(.5,.375), 
#' 	units="coords", arrows="start", length=.1, lty=1)
#' # ... which is the same as using arrows:
#' arrows(x0=.5, x1=.5, y0=.5, y1=.375, code=1, length=.1, lty=1)
#' 
#' # ... but these arrows can also be clipped to the device 
#' # instead of the plot region (see leftbottom corner):
#' drawDevArrows(start=c(.5,.5), end=c(.5,.375), 
#' 	units="dev", arrows="start", length=.1, lty=1)
#' 
#' # The function getArrowPos converts coordinates to device coordinates:
#' x1 <- getArrowPos(x=0.5, y=0.5, units="coords")
#' x2 <- getArrowPos(x=0.5, y=0.375, units="coords")
#' drawDevArrows(x1, x2, col="purple",
#' 	arrows="start", length=.1, lty=2, lwd=2)
#' 
#' 
#' # Setup 4 arrows with the same starting points, 
#' # but defined differently:
#' a1 <- getArrowPos(x=0.5, y=0.375, units="coords")
#' a2 <- getArrowPos(x=0.5, y=0.21, units="prop")
#' a3 <- getArrowPos(x=0.55, y=0.36, units="prop", dev="fig")
#' a4 <- getArrowPos(x=0.5*0.55, y=.5*0.36+.5, units="prop", dev="dev")
#' 
#' # Setup 3 arrows with the same x and y values, 
#' # which define different starting points in practice:
#' b1 <- getArrowPos(x=.5, y=.5, units="prop", dev="plot")
#' b2 <- getArrowPos(x=.5, y=.5, units="prop", dev="fig")
#' b3 <- getArrowPos(x=.5, y=.5, units="prop", dev="dev")
#' 
#' 
#' #------------------
#' # PLOT 2: different coordinates
#' #------------------
#' 
#' plot(c(-2.33, 20), c(.3, .8), type='n', main='2')
#' points(15,.8, pch=21, lwd=3, col='red', bg='white', cex=1.2)
#' 
#' # define end point for b:
#' b <- getArrowPos(x=15, y=.8)
#' 
#' # Draw arrow b1:
#' drawDevArrows(start=b1, end=b, arrows="start", length=.1, lty=1)
#' 
#' 
#' #------------------
#' # PLOT 3: upside down axis
#' #------------------
#' 
#' emptyPlot(c(25, 1050), c(15,-15), eegAxis=TRUE, h0=0)
#' # plot line:
#' x <- 0:1000
#' y <- 10*cos(x/100)
#' lines(x, y, col=4)
#' # draw point points on gthe line:
#' x <- c(200,400,600,800)
#' y <- 10*cos(x/100)
#' points(x,y, pch=18)
#' 
#' # To avoid calling the function drawDevArrows 4 times, we rewrite
#' # the x- and y-positions of the 4 coordinates a1, a2, a3, a4 in one list:
#' a.start <- list(x=c(a1$x, a2$x, a3$x, a4$x), y=c(a1$y, a2$y, a3$y, a4$y))
#' # Define end points on the line:
#' a.end <- getArrowPos(x=x, y=y)
#' drawDevArrows(start=a.start, end=a.end, arrows="none", lty=3)
#' 
#' # Note that these four coordinates are actually referring 
#' # to the same starting point!
#' # So instead we could have written:
#' drawDevArrows(start=a1, end=a.end, arrows="none", col=alpha("red"), lwd=2)
#' 
#' 
#' #------------------
#' # PLOT 4: wrapping up
#' #------------------
#' 
#' # Arrows could be constructed when the plot is not yet called, 
#' # as they are clipped to the device:
#' drawDevArrows(start=c(0,7), end=c(7,0), col='gray', lwd=4, lty=3, arrows="none")
#' 
#' # Add the plot:
#' plot(1,1, bg="green")
#' 
#' # Finish b2 and b3: same x and y, but different coordinates
#' drawDevArrows(start=b2, end=b, arrows="start", length=.1, lty=2)
#' drawDevArrows(start=b3, end=b, arrows="start", length=.1, lty=3)
#' 
#' 
#' 
#' ### EXAMPLE 2 ################################
#' 
#' 
#' 
#' # setup 4 plots:
#' par(mfrow=c(2,2))
#' 
#' n <- 50
#' 
#' #------------------
#' # PLOT 1: empty
#' #------------------
#' 
#' 
#' emptyPlot(c(25, 1050), c(15,-15), axes=FALSE)
#' lines(0:1000, 10*cos(0:1000/200), col=4)
#' x <- seq(0,1000, length=n)
#' y <- 10*cos(x/200)
#' 
#' a <- getArrowPos(x=x, y=y)
#' 
#' 
#' #------------------
#' # PLOT 2
#' #------------------
#' 
#' emptyPlot(c(25, 1050), c(15,-15), axes=FALSE)
#' lines(0:1000, 10*sin(0:1000/200), col=1)
#' x <- seq(0,1000, length=n)
#' y <- 10*sin(x/200)
#' 
#' 
#' b <- getArrowPos(x=x, y=y)
#' 
#' 
#' 
#' #------------------
#' # PLOT 3
#' #------------------
#' 
#' emptyPlot(c(25, 1050), c(15,-15), axes=FALSE)
#' lines(0:1000, 10*cos(0:1000/200), col=4)
#' x <- seq(0,1000, length=n)
#' y <- 10*cos(x/200)
#' 
#' 
#' c <- getArrowPos(x=rev(x), y=rev(y))
#' 
#' 
#' #------------------
#' # PLOT 4
#' #------------------
#' 
#' emptyPlot(c(25, 1050), c(15,-15), axes=FALSE)
#' lines(0:1000, 10*sin(0:1000/200), col=1)
#' x <- seq(0,1000, length=n)
#' y <- 10*sin(x/200)
#' 
#' d1 <- getArrowPos(x=rev(x), y=rev(y))
#' d2 <- getArrowPos(x=x, y=y)
#' 
#' 
#' #------------------
#' # DRAW ARROWS
#' #------------------
#' 
#' drawDevArrows(start=a, end=b, arrows="none", col='gray')
#' drawDevArrows(start=c, end=d1, arrows="none", col='gray')
#' 
#' drawDevArrows(start=a, end=c, arrows="none", 
#'     col=alphaPalette(c('green', 'blue'), f.seq=c(0,1), n=n))
#' drawDevArrows(start=b, end=d2, arrows="none", 
#'     col=alphaPalette('pink', f.seq=c(1,.1), n=n))
drawDevArrows <- function(start, end=NULL, 
	arrows = c("end", "start", "both", "none"),
	units=c("inch", "prop", "coords"),
	...
	){
	# process input 
	arrows = tolower(arrows[1])
	if(!arrows %in% c("end", "start", "both", "none")){
		warning(sprintf("Incorrect arrow type '%s'. Must be 'end', 'start', 'both', or 'none'. By default 'end' is selected.", arrows))
		arrows="end"
	}
	units = tolower(units[1])
	if(!units %in% c("coords", "c", "prop", "inch", "proportions", "inches", 'p', 'i')){
		warning(sprintf("Incorrect units '%s'. Must be 'coords'/'c' (coordinates of current plot region), 'prop'/'p' (proportions), or 'inch'/'i'. By default 'inch' is selected.", units))
		units="inch"
	}else{
		if(tolower(substr(units,1,1))=="p"){
			units="prop"
		}else if(tolower(substr(units,1,1))=="i"){
			units="inch"
		}else if(tolower(substr(units,1,1))=="c"){
			units="coords"
		}
	}
	# x and y:
	x0 <- x1 <- NULL
	y0 <- y1 <- NULL
	if(!is.null(dim(start))){
		if(dim(start)[2] < 2){
			stop("Start should have two columns, for x and y coordinates respectively.")
		}
		x0 <- start[,1]
		y0 <- start[,2]
	}else if(is.list(start)){
		x0 <- start$x
		y0 <- start$y
	}else{
		x0 <- start[1]
		y0 <- start[2]
	}
	if(!is.null(dim(end))){
		if(dim(end)[2] < 2){
			stop("End should have two columns, for x and y coordinates respectively.")
		}
		x1 <- end[,1]
		y1 <- end[,2]
	}else if(is.list(end)){
		x1 <- end$x
		y1 <- end$y
	}else{
		x1 <- end[1]
		y1 <- end[2]
	}
	pos0 <- list(x=x0, y=y0)
	pos1 <- list(x=x1, y=y1)
	# convert to coords:
	if(units=="inch"){
		pos0 <- inch2coords(x0, ypos=y0, simplify=FALSE)
		pos1 <- inch2coords(x1, ypos=y1, simplify=FALSE)
	}
	
	# draw lines:
	if(arrows=="none"){
		segments(x0=pos0$x, x1=pos1$x, y0=pos0$y, y1=pos1$y, xpd=NA, ...)		
	}else if(arrows=="start"){
		arrows(x0=pos0$x, x1=pos1$x, y0=pos0$y, y1=pos1$y, xpd=NA, code=1, ...)
	}else if(arrows=="end"){
		arrows(x0=pos0$x, x1=pos1$x, y0=pos0$y, y1=pos1$y, xpd=NA, code=2, ...)
	}else if(arrows=="both"){
		arrows(x0=pos0$x, x1=pos1$x, y0=pos0$y, y1=pos1$y, xpd=NA, code=3, ...)
	}
}





#' Utility function
#' 
#' @description Generate an empty plot window.
#' 
#' @export
#' @import grDevices
#' @import graphics
#' @param xlim A one- or two-value vector indicating the range of the x-axis. 
#' If \code{xlim} is a number, then it is assumed that the other value is 0. 
#' Thus, \code{xlim=3000} wil result in a x-axis ranging from 0 to 3000, 
#' and \code{xlim=-3} will result in a x-axis ranging from -3 to 0.
#' Optional, \code{xlim} can be a vector with categorical x-axis labels. 
#' @param ylim A one- or two-value vector indicating the range of the y-axis. 
#' (See \code{xlim}) for more information.
#' Optional, \code{ylim} can be a vector with categorical y-axis labels. 
#' @param main Title for the plot. Empty by default. 
#' Note: the title can be added later using \code{\link[graphics]{title}}.
#' @param xlab Label for x-axis. Empty by default. If no label is provided, 
#' use \code{\link[graphics]{mtext}} for adding axis labels.
#' @param ylab Label for y-axis. Empty by default. (See \code{xlab}.)
#' @param h0 A vector indicating where to add solid horizontal lines for 
#' reference. By default no values provided.
#' @param v0 A vector indicating where to add dotted vertical lines for 
#' reference. By default no values provided.
#' @param bty A character string which determined the type of box which is 
#' drawn about plots. If bty is one of "o", "l", "7", "c", "u", or "]" the 
#' resulting box resembles the corresponding upper case letter. A value of 
#' "n"  (the default) suppresses the box.
#' @param eegAxis Logical: whether or not to reverse the y-axis, plotting the 
#' negative amplitudes upwards as traditionally is done in EEG research.
#' If eeg.axes is TRUE, labels for x- and y-axis are provided, when not 
#' provided by the user. Default value is FALSE.
#' @param ... Other arguments for plotting, see \code{\link[graphics]{par}}.
#' @return An empty plot window.
#' @author Jacolien van Rij
#' @seealso Use \code{\link[graphics]{title}} and 
#' \code{\link[graphics]{mtext}}  for drawing labels and titles; 
#' use  \code{\link[graphics]{lines}} and \code{\link[graphics]{points}} 
#' for plotting the data; 
#' use \code{\link[graphics]{legend}} or 
#' \code{\link{legend_margin}} for adding a legend.
#' @examples
#' # generate some measurements:
#' x <- runif(100,0,100)
#' y <- rpois(100,lambda=3)
#' 
#' # Setup empty plot window fitting for data:
#' emptyPlot(range(x), range(y))
#' # To add data, use lines() and points()
#' points(x,y, pch=16, col=alpha('steelblue'))
#' 
#' # Category labels:
#' emptyPlot(toupper(letters[1:5]), 1)
#' # order matters:
#' emptyPlot(sample(toupper(letters[1:5])), 1)
#' # actually, they are plotted on x-positions 1:5
#' points(1:5, rnorm(5, mean=.5, sd=.1))
#' # also possible for y-axis or both:
#' emptyPlot(c(200,700), toupper(letters[1:5]))
#' emptyPlot(as.character(8:3), toupper(letters[1:5]))
#' # change orientation of labels:
#' par(las=1)
#' emptyPlot(c(200,700), toupper(letters[1:5]))
#' par(las=0) # set back to default
#' 
#' # More options:
#' emptyPlot(range(x), range(y),
#'     main='Data', ylab='Y', xlab='Time')
#' # add averages:
#' m <- tapply(y, list(round(x/10)*10), mean)
#' lines(as.numeric(names(m)), m, type='o', pch=4)
#' 
#' # with vertical and horizontal lines:
#' emptyPlot(1, 1, h0=.5, v0=.75)
#' # eeg axis (note the axes labels):
#' emptyPlot(c(-200,1000), c(-5,5),
#'     main="EEG", v0=0, h0=0,
#'     eegAxis=TRUE)
#' 
#' # empty window:
#' emptyPlot(1,1,axes=FALSE)
#' # add box:
#' emptyPlot(1,1, bty='o')
#' 
#' @family Functions for plotting
emptyPlot <- function(xlim, ylim, 
    main=NULL, xlab=NULL, ylab=NULL, h0=NULL, v0=NULL, 
    bty='n', eegAxis=FALSE, ...){
    xlabels <- NULL
    ylabels <- NULL
    if(length(xlim)==1){
        xlim <- sort(c(0,xlim))
    }else if (length(xlim) == 2){
        if(!is.numeric(xlim)){
            xlabels = xlim
            xlim=c(1,2)+c(-.1,.1)
        }
    }else if (length(xlim) > 2){
        if(is.numeric(xlim)){
            stop("xlim should be a 1 or 2 value numeric vector, indicating the range of values, or a vector with (ordered) category labels. If the categories are indicated by numbers, please convert to character first: as.character(xlim).")
        }
        xlabels = xlim
        xlim = c(1,length(xlabels))+c(-.1,.1)*(length(xlabels)-1)
    }
    if(length(ylim)==1){
        ylim <- sort(c(0,ylim))
    }else if (length(ylim) == 2){
        if(!is.numeric(ylim)){
            ylabels = ylim
            ylim=c(1,2)+c(-.1,.1)
        }
    }else if (length(ylim) > 2){
        if(is.numeric(ylim)){
            stop("ylim should be a 1 or 2 value numeric vector, indicating the range of values, or a vector with (ordered) category labels. If the categories are indicated by numbers, please convert to character first: as.character(ylim).")
        }
        ylabels = ylim
        ylim = c(1,length(ylabels))+c(-.1,.1)*(length(ylabels)-1)
    }
    if(is.null(main)){
        main=''
    }
    if(eegAxis){
        ylim <- sort(ylim, decreasing=T)
        if(is.null(ylab)){
            ylab=expression(paste('Amplitude (', mu, V,')', sep=''))
        }
        if(is.null(xlab)){
            xlab="Time (ms)"
        }
    }else{
        if(is.null(ylab)){
            ylab=''
        }
        if(is.null(xlab)){
            xlab=""
        }
    }
    
    if(!is.null(xlabels) | !is.null(ylabels)){
        plot(range(xlim), range(ylim), type='n',
            xlim=xlim, ylim=ylim, 
            main=main, xlab=xlab, ylab=ylab,
            bty=bty, axes=FALSE, ...)
        if(!is.null(xlabels)){
            axis(1, at=1:length(xlabels), labels=xlabels)
        }else{
            axis(1)
        }
        if(!is.null(ylabels)){
            axis(2, at=1:length(ylabels), labels=ylabels)
        }else{
            axis(2)
        }
    }else{
        plot(range(xlim), range(ylim), type='n',
            xlim=xlim, ylim=ylim, 
            main=main, xlab=xlab, ylab=ylab,
            bty=bty, ...)
    }
    if(!is.null(h0)){
        abline(h=h0)
    }
    if(!is.null(v0)){
        abline(v=v0, lty=3)
    }
}





#' Add error bars to a plot.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @import stats
#' @description Add vertical error bars.
#' 
#' @param x Vector with x-values (or y-values in case \code{horiz=TRUE}).
#' @param mean Vector with means.
#' @param ci Vector with errors or confidence bands, e.g. SE values. If 
#' \code{ci.l} is not defined, the errors are assumed to be symmetric. If 
#' \code{ci.l} is defined, than \code{ci} is assumed to be the upper 
#' confidence band. Note that the \code{ci} will be added (or substracted) 
#' from the mean.
#' @param ci.l Optional: vector with error to calculate lower confidence band.
#' @param minmax Optional argument, vector with two values indicating the 
#' minimum and maximum value for the error bars. If NULL (default) the error 
#' bars are not corrected.
#' @param horiz Logical: whether or not to plot horizontal error bars. 
#' Defaults to FALSE (plotting vertical error bars).
#' @param ... Optional graphical parameters (see \code{\link[graphics]{par}}).
#' @author Jacolien van Rij
#' @examples
#' 
#' # example InsectSprays from R datasets
#' 
#' InsectSprays$type <- ifelse( InsectSprays$spray %in% c("A", "B", "F"), 1,2)
#' avg <- with(InsectSprays, tapply(count, list(spray), mean))
#' sds <- with(InsectSprays, tapply(count, list(spray), sd))
#' 
#' 
#' # barplot:
#' b <- barplot(avg, besides=TRUE, main="Insect Sprays", ylim=c(0,20))
#' errorBars(b, avg, sds, xpd=TRUE, length=.05)
#' 
#' # constrain error bars to max and min of plot:
#' b <- barplot(avg, besides=TRUE, main="Insect Sprays", ylim=c(0,20))
#' errorBars(b, avg, sds, minmax=c(0,20), xpd=TRUE, length=.05)                
#' 
#' # line plot:
#' emptyPlot(toupper(letters[1:6]), 20, main="Averages", xlab="Spray")
#' 
#' # fake errors:
#' ci.low <- abs(rnorm(6, mean=2))
#' ci.high <-  abs(rnorm(6, mean=4))
#' 
#' errorBars(1:6, avg, ci.high, ci.l= ci.low, length=.05, lwd=2)
#' points(1:6, avg, pch=21, type='o', lty=3, lwd=2,
#'     bg="white", xpd=TRUE)
#' # also horizontal bars possible:
#' errorBars(10, 1, 1.2, horiz=TRUE, col='red')
#' 
#' @family Functions for plotting
errorBars <- function(x, mean, ci, ci.l=NULL, minmax=NULL, horiz=FALSE, ...){
    cu <- mean+ci
    cl <- mean-ci
    if(!is.null(ci.l)){
        cl <- mean-ci.l
    }
    if(!is.null(minmax)){
        cu[!is.na(cu) & cu>minmax[2]] <- minmax[2]
        cl[!is.na(cl) & cl<minmax[1]] <- minmax[1]
    }
    dnm <- list(...)
    if(!"length" %in% names(dnm)){
        dnm[['length']] <- .1
    }
    if(!"angle" %in% names(dnm)){
        dnm[["angle"]] <- 90
    }
    if(!"code" %in% names(dnm)){
        dnm[["code"]] <- 3
    }
    if(length(dnm) > 0){
        pars <- list2str(names(dnm), dnm)
        if(horiz==TRUE){
            eval(parse(text=paste('arrows(x0=cl, x1=cu, y0=x, y1=x,', pars, ')', sep='')))
        }else{
            eval(parse(text=paste('arrows(x0=x, x1=x, y0=cl, y1=cu,', pars, ')', sep='')))
        }
    }else{
        if(horiz==TRUE){
            arrows(x0=cl, x1=cu, y0=x, y1=x,...)
        }else{
            arrows(x0=x, x1=x, y0=cl, y1=cu,...)
        }
    }        
}





#' Utility function
#' 
#' @description Fill area under line or plot.
#' 
#' @export
#' @import grDevices
#' @import graphics
#' @param x Vector with values on x-axis.
#' @param y Vector with values on y-axis.
#' @param from A number indicating until which value on the y-axis the graph 
#' is colored. Defaults to 0.
#' @param col Color for filling the area. Default is black.
#' @param alpha Transparency of shaded area. Number between 0 
#' (completely transparent) and 1 (not transparent). Default is .25.
#' @param border A color, indicating the color of the border around 
#' shaded area. No border with value NA (default). 
#' @param na.rm Logical: whether or not to remove the missing values in 
#' \code{x} and \code{y}. Defaults to TRUE. If set to FALSE, missing values 
#' may cause that the filled area is split in various smaller areas.
#' @param horiz Logical: whether or not to plot with respect to the 
#' x-axis (TRUE) or y-xis (FALSE). Defaults to TRUE.
#' @param outline Logical: whether or not to draw the outline instead of only 
#' the upper border of the shape. Default is FALSE (no complete outline).
#' @param ... Optional arguments for the lines. See \code{\link{par}}.
#' @author Jacolien van Rij
#' @examples
#' # density of a random sample from normal distribution:
#' test <- density(rnorm(1000))
#' emptyPlot(range(test$x), range(test$y))
#' fill_area(test$x, test$y)
#' fill_area(test$x, test$y, from=.1, col='red')
#' fill_area(test$x, test$y, from=.2, col='blue', density=10, lwd=3)
#' lines(test$x, test$y, lwd=2)
#' 
#' @seealso \code{\link{check_normaldist}}
#' @family Functions for plotting
fill_area <- function(x, y, from=0, col='black', alpha=.25,  border=NA, na.rm=TRUE, 
    horiz=TRUE, outline=FALSE, ...){
    el.narm <- c()
    if(na.rm){
        el.narm <- which(is.na(x) | is.na(y))
        if(length(from)==length(x)){
            from = from[!is.na(x) | !is.na(y)]
        }
        x <- x[!is.na(x) | !is.na(y)]
        y <- y[!is.na(x) | !is.na(y)]
    }
    xval <- c(x, rev(x))
    yval <- c()
    if(length(from)==1){
        yval <- c(y, rep(from, length(y)))
    }else if(length(from)==length(x)){
        yval <- c(y, rev(from))
    }else{
        warning("Argument from has more than 1 element. Only first element being used.")
        yval <- c(y, rep(from, length(y)))
    }
    if(names(dev.cur())[1] %in% c("X11", "postscript", "xfig", "pictex") ){
        alpha = 1
    }
    line.args <- list2str(x=c("type", "pch", "lty", "bg", "cex", "lwd", "lend", "ljoin", "lmitre"), inputlist=list(...))
    fill.args <- list2str(x= c("density", "angle", "lty", "fillOddEven", "lwd", "lend", "ljoin", "lmitre"), inputlist=list(...))
    
    if(horiz){  
        if(!is.na(border) ){
            if( outline==TRUE){
                eval(parse(text=sprintf("polygon(x=xval, y=yval, border=border, col=alpha(col, f=alpha), %s, xpd=TRUE)", fill.args )  ))
            }else{
                eval(parse(text=sprintf("polygon(x=xval, y=yval, border=NA, col=alpha(col, f=alpha), %s, xpd=TRUE)", fill.args )  ))
                eval(parse(text=sprintf("lines(x=x, y=y, col=border, %s, xpd=TRUE)", line.args )  ))
            }
        }else{
            eval(parse(text=sprintf("polygon(x=xval, y=yval, border=NA, col=alpha(col, f=alpha), %s, xpd=TRUE)", fill.args )  ))
        }
    }else{  
        if(!is.na(border) ){
            if( outline==TRUE){
                eval(parse(text=sprintf("polygon(x=yval, y=xval, border=border, col=alpha(col, f=alpha), %s, xpd=TRUE)", fill.args )  ))
            }else{
                eval(parse(text=sprintf("polygon(x=yval, y=xval, border=NA, col=alpha(col, f=alpha), %s, xpd=TRUE)", fill.args )  ))
                eval(parse(text=sprintf("lines(x=y, y=x, col=border, %s, xpd=TRUE)", line.args )  ))
            }
        }else{
            eval(parse(text=sprintf("polygon(x=yval, y=xval, border=NA, col=alpha(col, f=alpha), %s, xpd=TRUE)", fill.args )  ))
        }     
    }
}





#' Convert proportions into coordinates of the plot or figure region.
#'
#' @export
#' @import graphics
#' @description Function for positioning a legend or label in or outside the 
#' plot region based on proportion of the plot region rather than Cartesian 
#' coordinates.
#' 
#' @param pos A number indicating the proportion on the x-axis. Default is 1.1.
#' @param side Which axis to choose: 1=bottom, 2=left, 3=top, 4=right. Default is 1.
#' @param input Which proportion to take: with respect to the plot region 
#' (input 'p', default), or with respect to figure region (input 'f').
#' @author Jacolien van Rij
#' @examples
#' # set larger plot window, depending on your system:
#' # dev.new(,with=8, height=4) # windows, mac
#' # quartz(,8,4)               # Mac
#' # x11(width=8, height=4)     # linux
#' par(mfrow=c(1,2))
#' 
#' # PLOT 1: y-range is -1 to 1
#' emptyPlot(c(0,1),c(-1,1), h0=0, v0=0.5)
#' # calculate the x-coordinates for points at proportion
#' # -0.2, 0, .25, .5, 1.0, and 1.1 of the plot window:
#' p1 <- getCoords(pos=c(-0.2,0,.25,.5,1,1.1), side=2)
#' # use xpd=TRUE to plot outside plot region:
#' points(rep(0.5,length(p1)), p1, pch=16, xpd=TRUE)
#' # add legend outside plot region, in upper-right corner of figure:
#' legend(x=getCoords(1,side=1, input='f'), y=getCoords(1, side=2, input='f'),
#'     xjust=1, yjust=1,
#'     legend=c("points"), pch=16, xpd=TRUE)
#' # Note: this can easier be achieved with function getFigCoords
#' 
#' # PLOT 2: y-range is 25 to 37
#' # we would like to plot the points and legend at same positions
#' emptyPlot(c(0,1),c(25,37), h0=0, v0=0.5)
#' p1 <- getCoords(pos=c(-0.2,0,.25,.5,1,1.1), side=2)
#' points(rep(0.5,length(p1)), p1, pch=16, xpd=TRUE)
#' # add legend outside plot region, in upper-left corner of figure:
#' legend(x=getCoords(0,side=1, input='f'), y=getCoords(1, side=2, input='f'),
#'     xjust=0, yjust=1,
#'     legend=c("points"), pch=16, xpd=TRUE)
#'
#' @seealso
#' \code{\link{getFigCoords}}, \code{\link{getProps}}
#' @family Functions for plotting
getCoords <- function(pos = 1.1, side = 1, input='p') {
    p <- par()
    if(input=='p'){
        x.width = p$usr[2] - p$usr[1]
        y.width = p$usr[4] - p$usr[3]
        out <- rep(NA, length(pos))
        if(length(side)==1){
            side <- rep(side, length(pos))
        }
        out[which(side %in% c(1,3))] <- pos[which(side %in% c(1,3))] * x.width + p$usr[1]
        out[which(side %in% c(2,4))] <- pos[which(side %in% c(2,4))] * y.width + p$usr[3]
        return(out)        
    }else if(input=='f'){
        gfc <- getFigCoords('f')
        x.width = gfc[2] - gfc[1]
        y.width = gfc[4] - gfc[3]
        out <- rep(NA, length(pos))
        if(length(side)==1){
            side <- rep(side, length(pos))
        }
        out[which(side %in% c(1,3))] <- pos[which(side %in% c(1,3))] * x.width + gfc[1]
        out[which(side %in% c(2,4))] <- pos[which(side %in% c(2,4))] * y.width + gfc[3]
        return(out)                
    }
} 





#' Get the figure region as coordinates of the current plot region, 
#' or as corrdinates of the figure region.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @param input Text string: 'f' (figure, default), 'p' (plot region), 
#' 'hf' (half way figure region), or 'hp' (half way plot region)
#' @return A vector of the form c(x1, x2, y1, y2) giving the 
#' boundaries of the figure region as coordinates of the current 
#' plot region.
#' @author Jacolien van Rij
#' @examples
#' # setup plot region:
#' emptyPlot(1,1, bty='o')
#' fc <- getFigCoords()
#' pc <- getFigCoords('p')
#' arrows(x0=pc[c(1,2,1,2)], x1=fc[c(1,2,1,2)],
#'     y0=pc[c(3,3,4,4)], y1=fc[c(3,3,4,4)], xpd=TRUE)
#' 
#' # Same plot with different axis:
#' emptyPlot(c(250,500),c(331, 336), bty='o')
#' fc <- getFigCoords()
#' pc <- getFigCoords('p')
#' arrows(x0=pc[c(1,2,1,2)], x1=fc[c(1,2,1,2)],
#'     y0=pc[c(3,3,4,4)], y1=fc[c(3,3,4,4)], xpd=TRUE)
#' hc <-  getFigCoords('h')
#' 
#' # other options:
#' # 1. center of figure region:
#' abline(v=getFigCoords('hf')[1], col='blue', xpd=TRUE)
#' abline(h=getFigCoords('hf')[2], col='blue', xpd=TRUE)
#' # 2. center of plot region:
#' abline(v=getFigCoords('hp')[1], col='red', lty=3)
#' abline(h=getFigCoords('hp')[2], col='red', lty=3)
#' 
#' @seealso
#' \code{\link{getCoords}}, \code{\link{getProps}}
#' @family Functions for plotting
getFigCoords <- function(input='f'){
    p <- par()
    x.width = p$usr[2] - p$usr[1]
    y.width = p$usr[4] - p$usr[3]
    x.w = p$plt[2] - p$plt[1]
    y.w = p$plt[4] - p$plt[3]  
    if(input=='f'){
        return( c(p$usr[1]-p$plt[1]*x.width/x.w, # xmin
            p$usr[2]+(1-p$plt[2])*x.width/x.w,   # xmax
            p$usr[3]-p$plt[3]*y.width/y.w,       # ymin
            p$usr[4]+(1-p$plt[4])*y.width/y.w    # ymax
            ) )
    }else if(input=='p'){
        return(p$usr)
    }else if(input=='hp'){
        return( c( 0.5*x.width + p$usr[1], # x
            0.5*y.width + p$usr[3] ) )    # y
    }else if(input=='hf'){
        return( c( p$usr[1]+(0.5-p$plt[1])*(x.width / x.w), # x
                   p$usr[3]+(0.5-p$plt[3])*(y.width / y.w)  # y
                ))
    }else{
        return(NULL)
    }
}





#' Transform coordinates into proportions of the figure or plot region.
#'
#' @export
#' @import graphics
#' @description Function for positioning a legend or label in or outside the 
#' plot region based on proportion of the plot region rather than Cartesian 
#' coordinates.
#' 
#' @param pos A number indicating the coordinates on the x- or y-axis. 
#' @param side Which axis to choose: 1=bottom, 2=left, 3=top, 4=right. Default is 1.
#' @param output Which proportion to take: with respect to the plot region 
#' (input 'p', default), or with respect to figure region (output 'f').
#' @author Jacolien van Rij
#' @examples
#' # not very easy-to-calculate-with x- and y-axis values
#' emptyPlot(c(-2.35, 37.4), c(9,11), v0=0)
#' # draw a mirror symmetric image of boxes:
#' p1 <- c(9.5, 9.5)
#' p2 <- c(4,9.7)
#' p3 <- c(20,9)
#' p1m <- getCoords(1-getProps(p1, side=c(1,2)), side=c(1,2))
#' p2m <- getCoords(1-getProps(p2, side=c(1,2)), side=c(1,2))
#' p3m <- getCoords(1-getProps(p3, side=c(1,2)), side=c(1,2))
#' xdist <- diff(getCoords(c(0,.1), side=1))
#' ydist <- diff(getCoords(c(0,.1), side=2))
#' rect(xleft=c(p1[1],p2[1], p3[1], p1m[1], p2m[1], p3m[1])-xdist, 
#'     xright=c(p1[1],p2[1], p3[1], p1m[1], p2m[1], p3m[1])+xdist,
#'     ybottom=c(p1[2],p2[2], p3[2], p1m[2], p2m[2], p3m[2])-ydist, 
#'     ytop=c(p1[2],p2[2], p3[2], p1m[2], p2m[2], p3m[2])+ydist, 
#'     col=rep(c("red", NA, "lightblue"),2), xpd=TRUE )
#' 
#' @seealso
#' \code{\link{getCoords}}, \code{\link{getFigCoords}}
#' @family Functions for plotting
getProps <- function(pos, side=1, output='p'){
    p <- par()
    if(output=='p'){
        x.width = p$usr[2] - p$usr[1]
        y.width = p$usr[4] - p$usr[3]
        out <- rep(NA, length(pos))
        if(length(side)==1){
            side <- rep(side, length(pos))
        }
        out[which(side %in% c(1,3))] <- (pos[which(side %in% c(1,3))] - p$usr[1]) / x.width
        out[which(side %in% c(2,4))] <- (pos[which(side %in% c(2,4))] - p$usr[3]) / y.width 
        return(out)        
    }else if(output=='f'){
        gfc <- getFigCoords('f')
        x.width = gfc[2] - gfc[1]
        y.width = gfc[4] - gfc[3]
        out <- rep(NA, length(pos))
        if(length(side)==1){
            side <- rep(side, length(pos))
        }
        out[which(side %in% c(1,3))] <- (pos[which(side %in% c(1,3))] - gfc[1]) / x.width
        out[which(side %in% c(2,4))] <- (pos[which(side %in% c(2,4))] - gfc[3]) / y.width 
        return(out)                
    }
}





#' Add a gradient legend to a plot.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @description Add a gradient legend to a contour plot (or other plot) to 
#' indicate the range of values represented by the color palette.
#' 
#' @param valRange Range of the values that is represented by the color 
#' palette. Normally two value-vector. If a larger vector is provided, only 
#' the min and max values are being used.
#' @param color Name of color palette to use ('topo', 'terrain', 'heat', 
#' 'rainbow'). Custom color palettes can also be provided, but then the 
#' argument \code{nCol} is ignored.
#' @param pos A number indicating the position on the axis in proportion. 
#' Using the arguments \code{length} and \code{depth} and \code{side} the 
#' position of the legend is calculated automatically. Alternatively, one 
#' could provide  a vector with 4 numbers, providing the xleft, ybottom, 
#' xright, ytop of a rectangle. These 4 points are indicated in proportions of 
#' the x- and y-axis. However, if the argument \code{coords} is set to TRUE, 
#' these positions are taken as values in the Cartesian coordinate system of 
#' the plot. Note: \code{coords} is only considered for 4-number vectors of 
#' \code{pos}.
#' @param side Which axis to choose: 1=bottom, 2=left, 3=top, 4=right.
#' Default is 4.
#' @param length Number, indicating the width of the legend as proportion with 
#' respect to the axis indicated by \code{side}. 
#' Note: when \code{pos} is defined by 4 numbers, \code{length} is ignored.
#' @param depth Number, indicating the height of the legend as proportion 
#' with respect to the axis perpendicular to \code{side}.
#' Note: when \code{pos} is defined by 4 numbers, \code{depth} is ignored.
#' @param pos.num Numeric value, indicating the position of the numbers with 
#' respect to the tick marks. 1=bottom, 2=left, 3=top, 4=right.
#' @param inside Logical: whether or not to plot the legend inside or outside 
#' the plot area.
#' Note: when \code{pos} is defined by 4 numbers, \code{inside} is ignored.
#' @param coords Logical: whether or not \code{pos} is defined as coordinates. 
#' When FALSE, the default, \code{pos} is defined in proportions. 
#' Note: when \code{pos} is defined by 1 number, \code{inside} is ignored.
#' #' @param color Name of color palette to use ('topo', 'terrain', 'heat', 
#' 'rainbow'). Custom color palettes can also be provided, but then the 
#' argument \code{nCol} is ignored.
#' @param nCol Number of colors in the color palette.
#' @param n.seg Number of ticks and markers on the scale.
#' @param border.col Color of the border and the ticks.
#' @param dec Number of decimals for rounding the numbers, set to NULL on 
#' default (no rounding). 
#' @param fit.margin Logical: whether the labels of the gradient legend 
#' should be forced to fit in the margin or not. 
#' @author Jacolien van Rij
#' @examples
#' # empty plot:
#' emptyPlot(1,1, main="Test plot")
#' gradientLegend(valRange=c(-14,14),pos=.5, side=3)
#' 
#' # This produces a warning, as there is no space for labels here:
#' \dontrun{
#' gradientLegend(valRange=c(-14,14),pos=.125, side=4, inside=FALSE)
#' }
#' # Following options to fix this:
#' ## a. put labels on other side of legend -
#' ## not a good option, as the labels will overlap with the plot
#' gradientLegend(valRange=c(-14,14),pos=.125, side=4, inside=FALSE, pos.num=2)
#' ## b. put the legend in the plot region
#' emptyPlot(1,1, main="Test plot")
#' gradientLegend(valRange=c(-14,14),pos=.125, side=4, inside=TRUE)
#' ## c. Increase the margins:
#' oldmar = par()$mar
#' par(mar=oldmar+c(0,0,0,1))
#' emptyPlot(1,1, main="Test plot")
#' gradientLegend(valRange=c(-14,14),pos=.125, side=4, inside=FALSE)
#' par(mar=oldmar)
#' ## d. the last option is not a fix, but avoid warnings: 
#' ## set fit.margin to FALSE
#' emptyPlot(1,1, main="Test plot")
#' gradientLegend(valRange=c(-14,14),pos=.125, side=4, inside=FALSE, fit.margin=FALSE)
#' 
#' # change border color (and font color too!)
#' gradientLegend(valRange=c(-14,14),pos=.75, length=.5,
#' color=alphaPalette('white', f.seq=seq(0,1, by=.1)), border.col=alpha('gray'))
#' 
#' # when defining custom points, it is still important to specify side:
#' 
#' gradientLegend(valRange=c(-14,14), pos=c(.5,.25,.7,-.05), coords=TRUE, 
#' border.col='red', side=1)
#' 
#' 
#' @family Functions for plotting
gradientLegend <- function (valRange, color = "topo", nCol = 30, pos = 0.5, side = 4, 
    length = 0.25, depth = 0.05, inside = TRUE, coords = FALSE, pos.num=NULL,
    n.seg = 3, border.col = "black", dec = NULL, fit.margin=TRUE) 
{
    # xl, yb, xr, yt:
    loc <- c(0, 0, 0, 0)
    if(is.null(pos.num)){
        if(side %in% c(1,3)){
            pos.num = 3
        }else{
            pos.num = side
        }
    }
    if (length(pos) == 1) {
        pos.other <- ifelse(side > 2, 1, 0)
        if (side %in% c(1, 3)) {
            switch <- ifelse(inside, 0, 1)
            switch <- ifelse(side > 2, 1 - switch, switch)
            loc <- getCoords(c(pos - 0.5 * length, pos.other - 
                switch * depth, pos + 0.5 * length, pos.other + 
                (1 - switch) * depth), side = c(side, 2, side, 
                2))
        }
        else if (side %in% c(2, 4)) {
            switch <- ifelse(inside, 0, 1)
            switch <- ifelse(side > 2, 1 - switch, switch)
            loc <- getCoords(c(pos.other - switch * depth, pos - 
                0.5 * length, pos.other + (1 - switch) * depth, 
                pos + 0.5 * length), side = c(1, side, 1, side))
        }
    } else if (length(pos) == 4) {
        if (coords) {
            loc <- pos
        } else {
            loc <- getCoords(pos, side = c(1, 2, 1, 2))
        }
    }
    mycolors <- c()
    if (length(color) > 1) {
        mycolors <- color
    } else if (!is.null(nCol)) {
        if (color == "topo") {
            mycolors <- topo.colors(nCol)
        }
        else if (color == "heat") {
            mycolors <- heat.colors(nCol)
        }
        else if (color == "terrain") {
            mycolors <- terrain.colors(nCol)
        }
        else if (color == "rainbow") {
            mycolors <- rainbow(nCol)
        }
        else {
            warning("Color %s not recognized. A palette of topo.colors is used instead.")
            mycolors <- topo.colors(nCol)
        }
    } else {
        stop("No color palette provided.")
    }
    vals <- seq(min(valRange), max(valRange), length = length(mycolors))
    if (!is.null(dec)) {
        vals <- round(vals, dec[1])
    }
    im <- as.raster(mycolors[matrix(1:length(mycolors), ncol = 1)])
    ticks <- c()
    if (side%%2 == 1) {
        rasterImage(t(im), loc[1], loc[2], loc[3], loc[4], col = mycolors, 
            xpd = T)
        rect(loc[1], loc[2], loc[3], loc[4], border = border.col, 
            xpd = T)
        ticks <- seq(loc[1], loc[3], length = n.seg)
        segments(x0 = ticks, x1 = ticks, y0 = rep(loc[2], n.seg), 
            y1 = rep(loc[4], n.seg), col = border.col, xpd = TRUE)        
    } else {
        rasterImage(rev(im), loc[1], loc[2], loc[3], loc[4], 
            col = mycolors, xpd = T)
        rect(loc[1], loc[2], loc[3], loc[4], border = border.col, 
            xpd = T)
        ticks <- seq(loc[2], loc[4], length = n.seg)
        segments(x0 = rep(loc[1], n.seg), x1 = rep(loc[3], n.seg), 
            y0 = ticks, y1 = ticks, col = border.col, xpd = TRUE)
    }
    determineDec <- function(x){
        out = max(unlist( lapply( strsplit(x, split="\\."), function(y){
            return( ifelse(length(y)>1, nchar( gsub("^([^0]*)([0]+)$", "\\1", as.character(y[2])) ), 0) )
        }) ))
        return(out)
    }
    labels = sprintf("%f", seq(min(valRange), 
            max(valRange), length = n.seg) )
    if(is.null(dec)){
        dec <- min(c(6, determineDec(labels)))
    }
    eval(parse(text=sprintf("labels = sprintf('%s', round(seq(min(valRange), max(valRange), length = n.seg), dec) )",
                        paste("%.", dec, "f", sep=""))))
    
    if(pos.num == 1){
        # check label height:
        if(fit.margin){
            lab.height  = max(strheight(labels)) * 0.8
            max.pos    = getFigCoords()[3]
            if ((max.pos - loc[2]) < lab.height){
                warning("Increase bottom margin, because labels for legend do not fit.")
            }          
        }
        text(y = loc[2], x = ticks, labels = seq(min(valRange), 
            max(valRange), length = n.seg), col=border.col, 
            pos = 1, cex = 0.8, xpd = T)
    }else if (pos.num == 2){
        # check label width:
        if(fit.margin){
            checkagain = TRUE
            while (checkagain==TRUE){
                lab.width  = ( max(strwidth(labels)) + 0.5*par()$cxy[1] )*0.8
                min.pos    = getFigCoords()[1]
                if ((loc[1] - min.pos) < lab.width){
                    if(!is.null(dec)){
                        dec = max(c(0,dec-1))
                        if(dec == 0){
                            warning("Decimal set to 0 (dec=0), but the labels still don't fit in the margin. You may want to add the color legend to another side, or increase the margin of the plot.")
                            checkagain = FALSE
                        }
                    }else{
                        tmp = max(unlist( lapply(strsplit(labels, split="\\."), function(x){ return(ifelse(length(x)>1, nchar(x[2]), 0)) }) ))
                        dec = max(c(0,tmp-1))
                        if(dec == 0){
                            warning("Decimal set to 0 (dec=0), but the labels still don't fit in the margin. You may want to add the color legend to another side, or increase the margin of the plot.")
                            checkagain = FALSE
                        }
                    }
                    eval(parse(text=sprintf("labels = sprintf('%s', round(seq(min(valRange), max(valRange), length = n.seg), dec) )",
                        paste("%.", dec, "f", sep=""))))
                }else{
                    checkagain = FALSE
                }
            }
        }
        text(y = ticks, x = loc[1], labels = labels, pos = 2, cex = 0.8, col=border.col, xpd = T)
    }else if (pos.num == 3){
        if(fit.margin){        
            lab.height  = max(strheight(labels)) * 0.8
            max.pos    = getFigCoords()[4]
            if ((max.pos - loc[4]) < lab.height){
                warning("Increase top margin, because labels for legend do not fit.")
            }
        }
        text(y = loc[4], x = ticks, labels = seq(min(valRange), 
            max(valRange), length = n.seg), col=border.col, 
            pos = 3, cex = 0.8, xpd = T)
    }else if (pos.num == 4){
        # check label width:
        if(fit.margin){
            checkagain = TRUE
            while (checkagain==TRUE){
                lab.width  = ( max(strwidth(labels)) + 0.5*par()$cxy[1] )*0.8
                max.pos    = getFigCoords()[2]
                if ((max.pos - loc[3]) < lab.width){
                    if(!is.null(dec)){
                        dec = max(c(0,dec-1))
                        if(dec == 0){
                            warning("Decimal set to 0 (dec=0), but the labels still don't fit in the margin. You may want to add the color legend to another side, or increase the margin of the plot.")
                            checkagain = FALSE
                        }
                    }else{
                        tmp = max(unlist( lapply(strsplit(labels, split="\\."), function(x){ return(ifelse(length(x)>1, nchar(x[2]), 0)) }) ))
                        dec = max(c(0,tmp-1))
                        if(dec == 0){
                            warning("Decimal set to 0 (dec=0), but the labels still don't fit in the margin. You may want to add the color legend to another side, or increase the margin of the plot.")
                            checkagain = FALSE
                        }
                    }
                    eval(parse(text=sprintf("labels = sprintf('%s', round(seq(min(valRange), max(valRange), length = n.seg), dec) )",
                        paste("%.", dec, "f", sep=""))))
                }else{
                    checkagain = FALSE
                }
            }
        }
        text(y = ticks, x = loc[3], labels = labels, pos = 4, col=border.col, cex = 0.8, xpd = T)
    }
}





#' Add legend with respect to figure instead of plot region. 
#' Allows to move legend to margin of plot.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @description Add legend with respect to figure instead of plot region. 
#' Wrapper around the function \code{\link[graphics]{legend}}.
#' 
#' @param x Text string, the location of the legend relative to the figure 
#' region. Single keyword from the list "bottomright", "bottom", "bottomleft", 
#' "left", "topleft", "top", "topright", "right" and "center". 
#' @param legend Vector with text strings to appear in the legend.
#' @param adj Numeric vector of length 1 or 2; the string adjustment for 
#' legend text. 
#' @param ... Other parameters for specifying the legend 
#' (see \code{\link[graphics]{legend}}). 
#' @author Jacolien van Rij
#' @examples
#' plot(cars$speed, cars$dist, pch=16)
#' legend_margin("topleft", legend=c("points"), pch=16)
#' # compare with default legend:
#' legend("topleft", legend=c("points"), pch=16)
#' @family Functions for plotting
legend_margin <- function(x, legend, adj=NULL, ... ){
  xloc <- getCoords(.5, side=1, input='f')
  yloc <- getCoords(.5, side=2, input='f')
  xadj <- 0.5
  yadj <- 0.5
  if(is.character(x)){
    if(grepl("top", x)){
      yloc <- getCoords(1, side=2, input='f')
      yadj <- 1
    }else if(grepl("bottom", x)){
      yloc <- getCoords(0, side=2, input='f')
      yadj <- 0
    }
    if(grepl("left", x)){
      xloc <- getCoords(0, input='f')
      xadj <- 0
    }else if (grepl("right", x)){
      xloc <- getCoords(1, input='f')
      xadj <- 1
    }
  }else if(is.numeric(x)){
    if(length(x) < 2){
      stop("Provide two numerical values for x. Proportion of width and height of figure region respectively.")
    }else if(length(x)>2){
      warning("Only first two values of x will be used.")
    }
    xloc <- getCoords(x[1], input='f')
    yloc <- getCoords(x[2], input='f')
    if(!is.null(adj)){
      xadj <- adj[1]
      if(length(x)==1){
        yadj <- adj[1]
      }else{
        yadj <- adj[2]
      }
    }
  }
  legend(x=xloc, y=yloc, legend=legend, xjust=xadj, yjust=yadj, xpd=TRUE, ...)
}





#' Plot density of distribution in margins of the plot.
#' 
#' @export
#' @import grDevices
#' @import graphics
#' @param x Density object, or vector with x-values.
#' @param y If \code{x} is not a density object, the vector \code{y} 
#' provides the y-values to plot.
#' @param side Number: 1 = bottom, 2 = left, 3 = top, 4 = left
#' @param from A number indicating the starting position (bottom) of the 
#' density plot. Measured in plot coordinates. 
#' Defaults to NULL, which indicate that the border of the plot 
#' is taken as the base of the density plot. 
#' @param scale Scale of the density plot. By default set to 1, 
#' which is the size of the margin region.
#' @param maxDensityValue Number for scaling the density axis. 
#' Default is NULL (automatic scaling fitting the d)
#' @param allDensities List with other density objects to determine 
#' the plotting scale such that they all fit. Defaults to NULL.
#' @param plot Logical: whether to plot the density (default) or not.
#' @param ... Optional arguments for the lines and fill_area. See \code{\link{par}}.
#' @author Jacolien van Rij
#' @examples
#' # density of a random sample from normal distribution:
#' val1 <- qnorm(ppoints(500))
#' val2 <- qt(ppoints(500), df = 2)
#' dens1 <- density(val1)
#' dens2 <- density(val2)
#' 
#' # setup plot window:
#' par(mfrow=c(1,1), cex=1.1)
#' 
#' # increase margin
#' oldmar <- par()$mar 
#' par(mar=oldmar + c(0,0,0,4))
#' 
#' # plot qqnorm
#' qqnorm(val2, main='t distribution',
#'        pch="*", col='steelblue',
#'        xlim=c(-3,3),
#'        bty='n')
#' qqline(val1)
#' abline(h=0, col=alpha('gray'))
#' abline(v=0, col=alpha('gray'))
#' 
#' # filled distribution in right margin:
#' marginDensityPlot(dens2, side=4, allDensities=list(dens1, dens2),
#' 	col='steelblue',lwd=2)
#' # add lines:
#' marginDensityPlot(dens2, side=4, allDensities=list(dens1, dens2),
#' 	col='steelblue',density=25, lwd=2)
#' # compare to normal:
#' marginDensityPlot(dens1, side=4, allDensities=list(dens1, dens2), 
#' 	col=NA, border=1)
#' # Other sides are also possible:
#' marginDensityPlot(dens1, side=3, allDensities=list(dens1, dens2), 
#' 	col=NA, border=alpha(1), lwd=2)
#' marginDensityPlot(dens2, side=3, allDensities=list(dens1, dens2), 
#' 	col=NA, border=alpha('steelblue'), lwd=3)
#' # adjust the starting point with argument 'from' to bottom of plot:
#' marginDensityPlot(dens1, side=3, 
#' 	from=getCoords(0, side=2), lwd=2)
#' marginDensityPlot(dens2, side=3, 
#' 	col='steelblue', from=getCoords(0, side=2), lwd=2,
#'  maxDensityValue=2*max(dens2$y))
#' 
#' legend(getFigCoords('p')[2], getFigCoords('p')[3],
#' 	yjust=0,
#' 	legend=c("t distribution", "Gaussian"),
#' 	fill=c("steelblue", 'black'),
#' 	cex=.75,
#' 	xpd=TRUE, bty='n')
#' 
#' 
#' @seealso \code{\link{check_normaldist}}
#' @family Functions for plotting
#' 
marginDensityPlot <- function(x, y=NULL, side, 
	from=NULL, scale=1,
	maxDensityValue=NULL, 
	allDensities=NULL, plot=TRUE, ...){
    if(!inherits(x, "density")){
        if(is.null(y)){
            d <- density(x, na.rm=TRUE)
            x <- d$x
            y <- d$y
            message("x converted to density object.")
        }else{
            if(length(x) != length(y)){
                stop("x and y do not have the same length.")
            }
        }
    }else{
        y <- x$y
        x <- x$x
    }
    if(is.null(maxDensityValue) & is.null(allDensities)){
        maxDensityValue = max(y, na.rm=TRUE)
    }else if (is.null(maxDensityValue) & !is.null(allDensities)){
    	maxDensityValue <- max( unlist( lapply(allDensities, function(a){ max(a$y)}) ) )
    }
    horiz=TRUE
    # set region:
    x0 <- y0 <- 0
    x1 <- y1 <- 1
    y.range <- 1
    y.dist <- 1
    gfc.f <- getFigCoords("f")
    gfc.p <- getFigCoords("p")
    if( side==1){       # bottom, going down
        x0 <- gfc.p[1]
        x1 <- gfc.p[2]
        y.range <- scale*.95*(gfc.f[3] - gfc.p[3])
        y0 <- gfc.p[3]
        if(!is.null(from)){
        	y0 <- from
        }
        y1 <- y0+y.range
        y.dist <- y1-y0
    }else if (side==2){   # left
        x0 <- gfc.p[3]
        x1 <- gfc.p[4]
        y.range <- scale*.95*(gfc.f[1] - gfc.p[1])
        y0 <- gfc.p[1]
        if(!is.null(from)){
        	y0 <- from
        }
        y1 <- y0+y.range
        y.dist <- y1-y0
        horiz = FALSE
    }else if (side==3){   # top
        x0 <- gfc.p[1]
        x1 <- gfc.p[2]
        y.range <- scale*.95*(gfc.f[4] - gfc.p[4])
        y0 <- gfc.p[4]
        if(!is.null(from)){
        	y0 <- from
        }
        y1 <- y0+y.range
        y.dist <- y1-y0
    }else if (side==4){   # right
        x0 <- gfc.p[3]
        x1 <- gfc.p[4]
        y.range <- scale*.95*(gfc.f[2] - gfc.p[2])
        y0 <- gfc.p[2]
        if(!is.null(from)){
        	y0 <- from
        }
        y1 <- y0+y.range
        y.dist <- y1-y0
        horiz = FALSE
    }
    f <- y.dist / maxDensityValue
    if(plot){
        fill_area(x, y*f+y0, from=y0, horiz=horiz, xpd=TRUE, ...)
    }
    
    invisible( list(plot.x=x, plot.y=y*f+y0, x=x, y=y, f=f, y0=y0 ))
 
}





#' Utility function
#' 
#' @description Plot line with confidence intervals.
#' 
#' @export
#' @import grDevices
#' @import graphics
#' @import datasets
#' @param x Vector with values on x-axis.
#' @param fit Vector with values on y-axis.
#' @param se.fit Vector with standard error; or when \code{se.fit2}
#' is provided, \code{se.fit} specifies upper values confidence
#' interval.
#' @param se.fit2 Optional: lower values confidence interval.
#' @param shade Logical: whether or not to produce shaded regions as 
#' confidence bands.
#' @param f Factor for converting standard error in confidence intervals. 
#' Defaults to 1. Use 1.96 for 95\% CI, and 2.58 for 99\% CI.
#' @param col Color for lines and confindence bands.
#' @param alpha Transparency of shaded area. Number between 0 
#' (completely transparent) and 1 (not transparent). 
#' @param ci.lty Line type to be used for the error lines, see 
#' \code{\link[graphics]{par}}. 
#' @param ci.lwd Line type to be used for the error lines, see 
#' \code{\link[graphics]{par}}.
#' @param border The color to draw the border for the shaded confidence 
#' interval. The default, FALSE, omits borders.
#' @param ... Optional arguments for the lines and shaded area.
#' @author Jacolien van Rij
#'
#' @examples
#' 
#' # generate some data:
#' x <- -10:20
#' y <- 0.3*(x - 3)^2 + rnorm(length(x))
#' s <- 0.2*abs(100-y + rnorm(length(x)))
#' 
#' # Plot line and standard deviation:
#' emptyPlot(range(x), range(y), h0=0)
#' plot_error(x, y, s)
#' # Change layout:
#' emptyPlot(range(x), range(y), h0=0)
#' plot_error(x, y, s, shade=TRUE, lty=3, lwd=3)
#' 
#' # Use of se.fit2 for asymmetrical error bars:
#' cu <- y + .65*s
#' cl <- y - s
#' emptyPlot(range(x), range(y), h0=0)
#' plot_error(x, y, s, shade=TRUE)
#' plot_error(x, y, se.fit=cu, se.fit2=cl, col='red', shade=TRUE)
#' 
#' # Some layout options:
#' emptyPlot(range(x), range(y), h0=0)
#' plot_error(x, y, s, lty=3, lwd=1, ci.lty=1, ci.lwd=3)
#' emptyPlot(range(x), range(y), h0=0)
#' plot_error(x, y, s, shade=TRUE, lty=3, lwd=3)
#' emptyPlot(range(x), range(y), h0=0)
#' plot_error(x, y, s, shade=TRUE, lty=1, lwd=3, ci.lwd=3, border='red')
#' emptyPlot(range(x), range(y), h0=0)
#' plot_error(x, y, s, shade=TRUE, lty=1, lwd=3, density=10, ci.lwd=3)
#'
#' @family Functions for plotting
plot_error <- function(x, fit, se.fit, se.fit2=NULL, 
    shade=FALSE, f=1, col='black', ci.lty=NULL, ci.lwd=NULL, 
    border=FALSE, alpha=.25,  ...){
    parlist=list(...)
    if(is.na(border)){
        border = FALSE
    }
    if(is.logical(border) & border==TRUE){
        border=col
    }
    
    if(is.null(ci.lty)){
        if(shade){
            if(border==FALSE){
                ci.lty = 1
            }else{
                if("lty" %in% names(parlist)){
                     ci.lty = parlist[['lty']]
                }else{
                     ci.lty=1
                }
            }
        }else{
             ci.lty=2
        } 
    }
    if(is.null(ci.lwd)){
        if(shade){
            if(border==FALSE){
                ci.lwd = 0
            }else{
                if("lwd" %in% names(parlist)){
                     ci.lwd = parlist[['lwd']]
                }else{
                     ci.lwd=1
                }
            }
        }else{
            ci.lwd = 1
        }
    }
                
    line.par   <- c("type", "pch", "lty", "bg", "cex", "lwd", "lend", "ljoin", "lmitre", "xpd")
    line.args  <- list2str(x=line.par,inputlist=parlist)
    err.par   <- c("type", "pch", "bg", "cex", "lend", "ljoin", "lmitre", "xpd")
    err.args   <- list2str(x=err.par,inputlist=parlist)
    shade.par  <- c("density", "angle", "fillOddEven", "xpd")
    shade.args <- list2str(x=shade.par,inputlist=parlist)
    if(shade){
        xval <- c(x, rev(x))
        yval <- NA
        if(is.null(se.fit2)){
            yval <- c(fit+f*se.fit, rev(fit-f*se.fit))
        }else{
            yval <- c(se.fit, rev(se.fit2))
        }
        suppressWarnings( {
            if("density" %in% names(parlist) && ci.lwd > 0){
                eval(parse(text=sprintf("polygon(x=xval, y=yval, lty=ci.lty, col=alpha(col, f=alpha), border=border, lwd=ci.lwd, %s)", shade.args)))
            }else{
                eval(parse(text=sprintf("polygon(x=xval, y=yval, lty=ci.lty, col=alpha(col, f=alpha), border=border, %s)", shade.args)))
            }
        })
    }else{
        if(is.null(se.fit2)){
            eval(parse(text=sprintf("lines(x, fit+f*se.fit, lty=  ci.lty, col=col, lwd= ci.lwd, %s)", err.args)))
            eval(parse(text=sprintf("lines(x, fit-f*se.fit, lty= ci.lty, col=col, lwd= ci.lwd, %s)", err.args)))
        }else{
            eval(parse(text=sprintf("lines(x, se.fit, lty=ci.lty, col=col, lwd=ci.lwd, %s)", err.args)))
            eval(parse(text=sprintf("lines(x, se.fit2, lty=ci.lty, col=col, lwd=ci.lwd, %s)", err.args)))
        } 
    }
    eval(parse(text=sprintf("lines(x, fit, col=col, %s)", line.args)))
}





#' Add images to plots.
#' 
#' @description Add images to plots.
#' 
#' @export
#' @import grDevices
#' @import graphics
#' @import utils
#' @param img Matrix or image object (list with 'image', a matrix, and 'col', 
#' a vector with color values), or a string indicating the filename of an 
#' image to read.
#' @param type String, 'image' (default), 'png', 'jpeg', 'gif'
#' @param col Vector with colors.
#' @param show.axes Logical: whether or not to plot the axes.
#' @param xrange Two-value vector providing the xleft and xright coordinate 
#' values of the picture. Default set to c(0,1).
#' @param yrange Two-value vector providing the ybottom and ytop coordinate 
#' values of the picture. Default set to c(0,1).
#' @param fill.plotregion Logical: whether or not to fill the complete plot 
#' region. Defaults to FALSE.
#' @param replace.colors Named list for replacing colors. The names are the 
#' colors (in hexadecimal values), or regular expressions matching colors. The 
#' values are the replacements.
#' @param add Logical: whether or not to add the plot to the current plot.
#' @param interpolate Logical: a logical vector (or scalar) indicating whether 
#' to apply linear interpolation to the image when drawing.
#' @param ... Other arguments for plotting, see \code{\link[graphics]{par}}.
#' @return Optionally returns
#' @author Jacolien van Rij
#' @examples
#' 
#' # see Volcano example at help(image)
#' # create image object:
#' myimg <- list(image=volcano-min(volcano), col=terrain.colors(max(volcano)-min(volcano)))
#' # create emoty plot window:
#' emptyPlot(1,1, main="Volcano images")
#' # add image topleft corner:
#' plot_image(img=myimg, xrange=c(0,.25), yrange=c(.75,1), add=TRUE)
#' # add transparent image as overlay:
#' myimg$col <- alpha(myimg$col, f=.25)
#' plot_image(img=myimg, add=TRUE, fill.plotregion=TRUE, bty='n')
#' # add image:
#' myimg$col <- topo.colors(max(myimg$image))
#' plot_image(img=myimg, xrange=c(0.125,.375), yrange=c(.5,.875), add=TRUE)
#' # add some points and lines:
#' points(runif(10,0,1), runif(10,0,1), type='o')
#' 
#' @family Functions for plotting
plot_image <- function(img, type='image',
	col = NULL,
	show.axes = FALSE,
	xrange=c(0,1), yrange=c(0,1), 
	fill.plotregion=FALSE,
	replace.colors=NULL, 
	add=FALSE, interpolate=TRUE, ...){
	# Check if the appropriate packages are installed:
	checkpkg <- function(x){
	    if(x %in% rownames(installed.packages())==FALSE) {
	        stop(sprintf("Cannot load image, because package %s is not installed. See help(plot_image) for instructions.", x))
	    } else {
	        eval(parse(text=sprintf("require(%s)",x)))
	    }
	}
	get_file_type <- function(x){
		if(!grepl('\\.', x)){
			warning('No file extension found.')
			return(NULL)
		}
		return( gsub('^(.*)(\\.)([^\\.]+)$', '\\3', x) )
	}
	convert2colors <- function(x){
		out <- NULL
		if(is.null(dim(x))){
			return(x)
		}else if(length(dim(x))<=2){
			if(is.null(col)){
				if(max(x) <= 1){
					out <- gray(x)
				}else{
					x <- x / max(x)
					out <- gray(x)
				}
			}else{
				if(length(col) >= max(x)){
					out <- col[x]
				}else{
					warning('Color definition does not fit image. Converted to gray scale.')
					x <- x / max(x)
					out <- gray(x)					
				}
			}
		}else if(dim(x)[3]==1){
			# grayscale
			out <- gray(x)
		}else if(dim(x)[3]==2){
			# GA
			stop('Not implemented for GA colors.')
		}else if(dim(x)[3]==3){
			out <- rgb(x[,,1], x[,,2], x[,,3])
		}else if(dim(x)[3]==4){
			out <- rgb(x[,,1], x[,,2], x[,,3], alpha=x[,,4])
		}
		col <- sort(unique(out))
		colnum <- 1:length(col)
		names(colnum) <- col
		out <- as.vector(colnum[out])
		out <- list(image= matrix(out, nrow=dim(x)[1], byrow=FALSE), col=col)
	}
	shift.col <- 0
	type <- tolower(type)
	if(type=="image"){
		if(is.character(img)){
			type = tolower(get_file_type(img))
		}else if(is.matrix(img)){
			img <- convert2colors(img)
		}
	}
	if(!type %in% c("image", "gif","png", "jpg", "jpeg")){
		stop("Image type must be one of 'image', 'gif', 'png', or 'jpeg'. Other image formats are currently not implemented.")
	}
	if(type=="gif"){
		checkpkg("caTools")
		eval(parse(text=sprintf("img <- caTools::read.gif('%s')", img)))
		shift.col <- 1
	}else if(type=="png"){
		checkpkg("png")
		eval(parse(text=sprintf("img <- convert2colors(png::readPNG('%s'))", img)))
	}else if(type %in% c("jpeg", "jpg")){
		checkpkg("jpeg")
		eval(parse(text=sprintf("img <- convert2colors(jpeg::readJPEG('%s'))", img)))
		shift.col <- 1
	}
	if(is.list(img)){
		if(is.null(col) & ("col" %in% names(img))){
			col = img$col
		}
		if(! "image" %in% names(img)){
			stop(sprintf("Cannot find image in list %s. Please provide image matrix in field 'image'.", 
				deparse(substitute(img))) )
		}else{
			img = img$image
		}
	}
	if(!is.null(replace.colors)){
		for(i in names(replace.colors)){
			if(any(grepl(i, col)) ){
				col[grepl(i, col)] <- replace.colors[[i]]
			}
		}
	}
	parlist=list(...)
	plot.args <- list2str(x=c("main", "sub", "xlab", "ylab", "asp", "h0", "v0", "eegAxis", "xpd"), inputlist=list(...))
    box.args <- list2str(x= c("col", "lwd", "lty", "xpd"), inputlist=list(...))
	fc <- c(xrange, yrange)
	if(add==FALSE){
		par(xaxs='i', yaxs='i')
		eval(parse(text=sprintf("emptyPlot(xrange,yrange, axes=show.axes, %s)",
			plot.args)))
		par(xaxs='r', yaxs='r')
	}
	if(fill.plotregion==TRUE){
		fc <- getFigCoords('p')
	}
	
	xpd=FALSE
	if('xpd' %in% names(parlist)){
		xpd=parlist[['xpd']]
	}		
	rasterImage(as.raster(matrix(col[img+shift.col], nrow=nrow(img))), 
		xleft=fc[1], xright=fc[2], ybottom=fc[3], ytop=fc[4], xpd=xpd, interpolate=interpolate)
	if(!'bty' %in% names(parlist)){
		if(add==TRUE){
			parlist[['border']] <- parlist[['col']]
			parlist[['col']] <- NA
			box.args <- list2str(x= c("border", "lwd", "lty", "xpd"), inputlist=list(...))
			eval(parse(text=sprintf(
				"rect(xleft=xrange[1], xright=xrange[2], ybottom=yrange[1], ytop=yrange[2], %s)",
				box.args)))
		}else{
			eval(parse(text=sprintf("box(%s)", box.args)))
		}
	}else{
		if(parlist[['bty']] %in% c("o", "l", "7", "c", "u", "]")){
			if(add==TRUE){
				parlist[['border']] <- parlist[['col']]
				parlist[['col']] <- NA
				box.args <- list2str(x= c("border", "lwd", "lty", "xpd"), inputlist=list(...))
				eval(parse(text=sprintf(
					"rect(xleft=xrange[1], xright=xrange[2], ybottom=yrange[1], ytop=yrange[2], %s)",
					box.args)))
			}else{
				eval(parse(text=sprintf("box(%s)", box.args)))
			}			
		}
	}
	invisible(list(image=img, col=col))
}





#' Creates a colored surface plot from data frame input.
#'
#' @export
#' @import stats
#' @import grDevices
#' @import graphics
#' @description This function is a wrapper around \code{\link[graphics]{image}}
#' and \code{\link[graphics]{contour}}. See \code{vignette("plotfunctions")} 
#' for an example of how you could use \code{\link[graphics]{image}} and 
#' \code{\link[graphics]{contour}}.
#'
#' @param data Data frame or list with plot data. A data frame needs to have a 
#' column with x values, a column with y values and a column with z values. A 
#' list contains a vector with unique x values, a vector with unique y values, 
#' and a matrix with z-values. The output of the function fvisgam is an 
#' example of a suitable list. 
#' @param view A vector with the names or numbers of the columns to plot on 
#' the x axis and y axis respectively.
#' @param predictor Optional: the name of the column in the data frame 
#' \code{data} that provides the z-values. If data contains more than one 
#' column besides the x- and y-values, the \code{predictor} should be provided.
#' @param valCI Optional: the name of the column in the data frame 
#' \code{data} that provides the CI-values. If not NULL, CI contour lines
#' will be plotted.
#' @param main Text string, an overall title for the plot.
#' @param xlab Label for x axis. Default is name of first \code{view} variable.
#' @param ylab Label for y axis. Default is name of second \code{view} 
#' variable.
#' @param xlim x-limits for the plot.
#' @param ylim y-limits for the plot.
#' @param zlim z-limits for the plot.
#' @param col Color for the  contour lines and labels.
#' @param color The color scheme to use for plots. One of "topo", "heat", 
#' "cm", "terrain", "gray" or "bw". Or a list of colors such as that 
#' generated by \code{\link[grDevices]{rainbow}}, 
#' \code{\link[grDevices]{heat.colors}}
#' \code{\link[grDevices]{colors}}, \code{\link[grDevices]{topo.colors}}, 
#' \code{\link[grDevices]{terrain.colors}} or similar functions.
#' Alternatively a vector with some colors can be provided for a custom 
#' color palette.
#' @param ci.col Two-value vector with colors for the lower CI contour lines 
#' and for the upper CI contour lines.
#' @param nCol The number of colors to use in color schemes.
#' @param add.color.legend Logical: whether or not to add a color legend. 
#' Default is TRUE. If FALSE (omitted), one could use the function
#' \code{\link{gradientLegend}} to add a legend manually at any position.
#' @param dec Numeric: number of decimals for rounding the color legend. 
#' When NULL (default), no rounding. If -1 (default), automatically determined. 
#' Note: if value = -1 (default), rounding will be applied also when 
#' \code{zlim} is provided.
#' @param fit.margin  Logical: whether the labels of the gradient legend 
#' should be forced to fit in the margin or not. 
#' @param ... Optional parameters for \code{\link[graphics]{image}}
#' and \code{\link[graphics]{contour}}.
#' @author Jacolien van Rij
#' @seealso \code{\link[graphics]{image}}, \code{\link[graphics]{contour}}, 
#' \code{\link{color_contour}}
#' @examples
#' 
#' # From the package graphics, see help(image):
#' x <- 10*(1:nrow(volcano))
#' y <- 10*(1:ncol(volcano))
#' image(x, y, volcano, col = terrain.colors(100), axes = FALSE)
#' contour(x, y, volcano, levels = seq(90, 200, by = 5),
#'         add = TRUE, col = "peru")
#' axis(1, at = seq(100, 800, by = 100))
#' axis(2, at = seq(100, 600, by = 100))
#' box()
#' title(main = "Maunga Whau Volcano", font.main = 4)
#' 
#' # now with plot surface:
#' # first convert to data frame
#' tmp <- data.frame(value = as.vector(volcano), 
#'     x = 10*rep(1:nrow(volcano), ncol(volcano)), 
#'     y = 10*rep(1:ncol(volcano), each=nrow(volcano)))
#' plotsurface(tmp, view=c('x', "y"), predictor='value', main="Maunga Whau Volcano")
#' 
#' # or with terrain colors:
#' plotsurface(tmp, view=c('x', "y"), predictor='value', 
#'     main="Maunga Whau Volcano", color="terrain")
#' 
#' # change color range:
#' plotsurface(tmp, view=c('x', "y"), predictor='value', 
#'     main="Maunga Whau Volcano", zlim=c(0,200))
#' 
#' #' remove color and color legend:
#' plotsurface(tmp, view=c('x', "y"), predictor='value', 
#'     main="Maunga Whau Volcano", 
#'     color=NULL, col=1, add.color.legend=FALSE)
#'
#' @family Functions for plotting
plotsurface <- function(data, view, predictor=NULL, valCI=NULL,
	main=NULL, xlab=NULL, ylab=NULL, 
	xlim=NULL, ylim=NULL, zlim=NULL,
	col=NULL, color=topo.colors(50), ci.col =c('green', 'red'), nCol=50,
	add.color.legend=TRUE, dec=NULL, fit.margin=TRUE, ...){
	xval <- c()
	yval <- c()
	zval <- c()
	cival.l <- NULL
	cival.u <- NULL
	# check input:
	# 1. check data:
	if(is.null(data)){
		stop("No data provided. Please provide data and view predictors.")
	}else if(is.list(data)){
			# 2a. check view
			if(is.numeric(view)){
				if(view[1] <= length(data)){
					xval <- data[[view[1]]]
				}else{
					stop(sprintf("First view element incorrect: data has only %d elements.", length(data)))
				}
				if(view[2] <= length(data)){
					yval <- data[[view[2]]]
				}else{
					stop(sprintf("Second view element incorrect: data has only %d elements.", length(data)))
				}
			}else{
				cn <- names(data)
				if(view[1] %in% cn){
					xval <- data[[view[1]]]
				}else{
					stop(sprintf("%s not available in data.", view[1]))
				}
				if(view[2] %in% cn){
					yval <- data[[view[2]]]
				}else{
					stop(sprintf("%s not available in data.", view[2]))
				}
			}
			# 3a. check predictor
			if(is.null(predictor)){
				if(length(data)==3){
					cn <- 1:3
					if(!is.numeric(view)){
						cn <- names(data)
					}
					zval <- data[[ cn[!cn %in% view] ]]
				}else{
					stop(sprintf("Not sure which element of %s should be plotted. Provide predictor.", deparse(substitute(data))))
				}
			}else{
				if(is.numeric(predictor)){
					if(length(data) >= predictor){
						zval <- data[[predictor]]
					}else{
						stop(sprintf("Value of predictor incorrect: data has only %d elements.", length(data)))
					}
				}else{
					cn <- names(data)
					if(predictor %in% cn){
						zval <- data[[predictor]]
					}else{
						stop(sprintf("%s not available in data.", predictor))
					}
				}
			}
			
			# 4a. check CI
			if(!is.null(valCI)){
				if(is.numeric(valCI)){
					if(length(data) >= valCI[1]){
						cival.l <- cival.u <- names(data)[valCI[1]]
					}else{
						stop(sprintf("Value of valCI incorrect: data has only %d elements.", length(data)))
					}
					if(length(valCI)>1){
						valCI <- valCI[1:2]
						if(length(data) >= valCI[2]){
							cival.u <- names(data)[valCI[2]]
						}else{
							warning(sprintf("Value of second valCI incorrect: data has only %d elements. First valCI is also used for upper limit.", length(data)))
						}
					}
				}else{
					cn <- names(data)
					if(valCI[1] %in% cn){
						cival.l <- cival.u <- valCI[1]
					}else{
						stop(sprintf("%s not available in data.", valCI[1]))
					}
					if(length(valCI)>1){
						valCI <- valCI[1:2]
						if(valCI[2] %in% cn){
							cival.u <- valCI[2]
						}else{
							warning(sprintf("Value of second valCI incorrect: %s not available in data. First valCI is also used for upper limit.", valCI[2]))
						}
					}
				}
			} # end valCI
			if(!is.matrix(zval)){
				# sort data:
				data <- as.data.frame(data)
				data <- data[order(data[[view[1]]], data[[view[2]]]),]
				xval <- sort(unique(xval))
				yval <- sort(unique(yval))
				zval <- matrix(data[, predictor], byrow=TRUE, 
					nrow=length(xval),ncol=length(yval))
				if(!is.null(cival.l)){
					cival.l <- matrix(data[, cival.l], byrow=TRUE, 
						nrow=length(xval),ncol=length(yval))
					cival.u <- matrix(data[, cival.u], byrow=TRUE, 
						nrow=length(xval),ncol=length(yval))
				}
				# warning('z-values should be provided as matrix. List is converted to data frame with x values, y values, and z values (and optionally CI values). See examples.')
			}else{
				if(!is.null(cival.l)){
					cival.l <- data[[cival.l]]
					cival.u <- data[[cival.u]]
				}
			}
	}else{
	 	stop('Data is not a list or data frame.')
	}
	
	## Check plot settings
	if(is.null(main)){
		if(is.null(predictor)){
			main=""
		}else{
			main=predictor
		}
	}
	if(is.null(xlab)){
		xlab=view[1]
	}
	if(is.null(ylab)){
		ylab=view[2]
	}
	if(is.null(xlim)){
		xlim=range(xval)
	}
	if(is.null(ylim)){
		ylim=range(yval)
	}	
	if(is.null(zlim)){
		zlim=range(zval)
	}	
	if(add.color.legend==TRUE & !is.null(dec)){
        if(dec == -1){
            dec <- getDec(min(zlim))
        }
        zlim <- getRange(zlim, step=(.1^dec), n.seg=2)
	}
	# colors:
    if(is.null(color)){
    	color <- alphaPalette('white', f.seq=c(0,0), n=nCol)
    } else if (color[1] == "heat") {
        color <- heat.colors(nCol)
        if(is.null(col)){
        	col <- 3
        }
    } else if (color[1] == "topo") {
        color <- topo.colors(nCol)
        if(is.null(col)){
        	col <- 2
        }
    } else if (color[1] == "cm") {
        color <- cm.colors(nCol)
        if(is.null(col)){
        	col <- 1
        }
    } else if (color[1] == "terrain") {
        color <- terrain.colors(nCol)
        if(is.null(col)){
        	col <- 2
        }
    } else if (color[1] == "bpy") {
        if (requireNamespace("sp", quietly = TRUE)) {
            color <- sp::bpy.colors(nCol)
            if(is.null(col)){
        		col <- 3
        	}
        } else {
            warning("Package 'sp' needed for bpy color palette. Using topo.colors instead (default).")
            color <- topo.colors(nCol)
            col <- 2
        }
    } else if (color[1] == "gray" || color[1] == "bw") {
        color <- gray(seq(0.1, 0.9, length = nCol))
        col <- 1
    } else {
        if( all(isColor(color)) ){
        	if(length(color) < nCol){
        		color <- colorRampPalette(color)(nCol)
        	}
        }else{
            stop("color scheme not recognised")
        }  
    } 
    if (is.null(col)){
    	col <- 'red'
    } 
	dnm <- list(...)
	parlist <- names(dnm)
	type2string <- function(x){
		out <- ""
		if(length(x)>1){
			if(is.character(x)){
				out <- sprintf("c(%s)", paste(sprintf("'%s'", x), collapse=','))
			}else{
				out <- sprintf("c(%s)", paste(x, collapse=','))
			}
		}else{
			if(is.character(x)){
				out <- sprintf("'%s'", x)
			}else{
				out <- sprintf("%s", x)
			}
		}
		return(out)
	}
	# check contour input:
	cpar <- c()
	contourarg <- c('nlevels', 'levels', 'labels', 'labcex', 'drawlabels', 'method', 'lty', 'lwd')
	for(i in parlist[parlist %in% contourarg] ){
		cpar <- c(cpar, sprintf("%s=%s", i, type2string(dnm[[i]])))
	}
	cpar <- paste(",", paste(cpar, collapse=','))
	cpar2 <- c()
	for(i in parlist[parlist %in% c('nlevels', 'levels', 'method')] ){
		cpar2 <- c(cpar2, sprintf("%s=%s", i, type2string(dnm[[i]])))
	}
	cpar2 <- paste(",", paste(cpar2, collapse=','))
	# check image input:
	ipar <- c()
	contourarg <- c('nlevels', 'levels', 'labels', 'labcex', 'drawlabels', 'method', 'lty', 'lwd')
	for(i in parlist[!parlist %in% contourarg] ){
		ipar <- c(ipar, sprintf("%s=%s", i, type2string(dnm[[i]])))
	}
	ipar <- paste(",", paste(ipar, collapse=','))
	eval(parse(text=sprintf("image(xval, yval, zval, col=color, xlim=xlim, ylim=ylim, zlim=zlim, main=main, xlab=xlab, ylab=ylab, add=FALSE%s)", ipar)))
	eval(parse(text=sprintf("contour(xval, yval, zval, col=col, add=TRUE%s)",
		cpar)))
	if(!is.null(valCI)){
		eval(parse(text=sprintf("contour(xval, yval, zval-cival.l, col=ci.col[1], add=TRUE, lty=3, drawlabels=FALSE%s)",
			cpar2)))
		eval(parse(text=sprintf("contour(xval, yval, zval+cival.u, col=ci.col[2], add=TRUE, lty=3, drawlabels=FALSE%s)",
			cpar2)))	
	}
    if(add.color.legend){
        gradientLegend(zlim, n.seg=3, pos=.875, dec=dec,
            color=color, fit.margin=fit.margin)
    }
	invisible(list(x=xval, y=yval, z=zval, ci.l = cival.l, ci.u = cival.u))
}





#' Creates a colored surface plot from data frame input.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @description This function uses \code{\link[graphics]{rasterImage}} to 
#' indicate which points in the surface are not significantly different from 
#' zero. Note that the shape of these non-significant regions depends on the 
#' number of data points (often specified with \code{n.grid}).
#'
#' @param data Data frame with plot data. A data frame needs to have a 
#' column with x values, a column with y values (specified in \code{view}), 
#' a column with z values (\code{predictor}), and one or two columns with 
#' CI values (\code{valCI}). 
#' @param view A vector of length 2 with the names or numbers of the columns 
#' to plot on the x axis and y axis respectively.
#' @param predictor The name of the column in the data frame 
#' \code{data} that provides the z-values. If data contains more than one 
#' column besides the x- and y-values, the \code{predictor} should be provided.
#' @param valCI The name of the column in the data frame 
#' \code{data} that provides the CI-values. Alternatively, 
#' two column names can be provided for the lower and upper CI respectively. 
#' @param col Color for the nonsignificant areas.
#' @param alpha Level of transparency, number between 0 (transparent) and 1 
#' (no transparency)
#' @param ... Optional parameters for \code{\link[graphics]{rasterImage}}
#' @author Jacolien van Rij
#' @seealso \code{\link[graphics]{rasterImage}}
#' @examples
#' # From the package graphics, see help(image):
#' x <- 10*(1:nrow(volcano))
#' y <- 10*(1:ncol(volcano))
#' tmp <- data.frame(value = (as.vector(volcano) - 120), 
#'     x = 10*rep(1:nrow(volcano), ncol(volcano)), 
#'     y = 10*rep(1:ncol(volcano), each=nrow(volcano)),
#'     CI = rep(20, nrow(volcano)*ncol(volcano)))
#' plotsurface(tmp, view=c('x', "y"), predictor='value', main="Maunga Whau Volcano")
#' plot_signifArea(tmp, view=c("x", "y"), predictor="value", valCI="CI")
#' 
#' # change color:
#' plotsurface(tmp, view=c('x', "y"), predictor='value', main="Maunga Whau Volcano")
#' plot_signifArea(tmp, view=c("x", "y"), predictor="value", valCI="CI", 
#'     col="red")
#' # or completely remove "nonsignificant" area:
#' plot_signifArea(tmp, view=c("x", "y"), predictor="value", valCI="CI", 
#'     col="white", alpha=1)
#' 
# valCI: lower and upper CI
plot_signifArea <- function(data, view, predictor=NULL, 
	valCI, col=1, alpha=.5, ...){
	# 1. check input: only list or data frames allowed
	if(is.data.frame(data)){
		# 2a. check view
		if(is.numeric(view)){
			if(view[1] > length(data)){
				stop(sprintf("First view element incorrect: data has only %d elements.", length(data)))
			}
			if(view[2] > length(data)){
				stop(sprintf("Second view element incorrect: data has only %d elements.", length(data)))
			}
			view = names(data)[view]
		}else{
			cn <- names(data)
			if(!view[1] %in% cn){
				stop(sprintf("%s not available in data.", view[1]))
			}
			if(!view[2] %in% cn){
				stop(sprintf("%s not available in data.", view[2]))
			}
		}
		# 3a. check predictor
		if(is.null(predictor)){
			if(length(data)==3){
				cn <- 1:3
				if(!is.numeric(view)){
					cn <- names(data)
				}
				predictor = cn[!cn %in% view]
			}else{
				stop(sprintf("Not sure which element of %s should be plotted. Provide predictor.", deparse(substitute(data))))
			}
		}else{
			if(is.numeric(predictor)){
				if(length(data) < predictor){
					stop(sprintf("Value of predictor incorrect: data has only %d elements.", length(data)))
				}
				predictor = names(data)[predictor]
			}else{
				cn <- names(data)
				if(!predictor %in% cn){
					stop(sprintf("%s not available in data.", predictor))
				}
			}
		}
		# 4a. check CI
		if(length(valCI)==1){
			valCI <- rep(valCI, 2)
		}else if(length(valCI) > 2){
			valCI <- valCI[1:2]
		}
		if(is.numeric(valCI)){
			if((length(data) < valCI[1]) | (length(data) < valCI[2])){
				stop(sprintf("Value of valCI incorrect: data has only %d elements.", length(data)))
			}
			valCI = names(data)[valCI]
		}else{
			cn <- names(data)
			if(!valCI[1] %in% cn){
				stop(sprintf("%s not available in data.", valCI[1]))
			}
			if(!valCI[2] %in% cn){
				stop(sprintf("%s not available in data.", valCI[2]))
			}
		}
	}else{
		stop("Data should be a list or data frame.")
	}
	# order data:
	data$sign <- ((data[,predictor]-data[,valCI[1]]) <= 0) & ((data[,predictor]+data[,valCI[2]]) >= 0)
    data <- data[order(data[,view[1]], data[,view[2]]),]
    sign.raster <- rep(alpha('white', f=0), nrow(data))
    sign.raster[data$sign] <- alpha(col, f=alpha)
    # raster images are row-first, in contrast to images...
    n.grid1 <- length(unique(data[,view[1]]))
    n.grid2 <- length(unique(data[,view[2]]))
    sign.raster <- matrix(sign.raster, byrow=FALSE, nrow=n.grid2, ncol=n.grid1)
    sign.raster <- as.raster(sign.raster[nrow(sign.raster):1,])
    gfc <- getFigCoords('p')
    rasterImage(sign.raster, xleft=gfc[1], xright=gfc[2], ybottom=gfc[3], ytop=gfc[4], ...)
}





#' Sort groups based on a function such as mean value or deviation.
#' 
#' @export
#' @import stats
#' @param formula Formula for splitting the data
#' @param FUN Function to apply to each group
#' @param decreasing Logical: sort groups on decreasing values or not 
#' (default is FALSE, sorting on increasing values).
#' @param ... Additional arguments for the function 
#' \code{\link[stats]{aggregate}}.
#' @return The order of levels.
#' @examples
#' head(ToothGrowth)
#' # sort on basis of mean length:
#' sortGroups(len ~ dose:supp, data = ToothGrowth)
#' labels = levels(interaction(ToothGrowth$dose, ToothGrowth$supp))
#' labels[sortGroups(len ~ dose:supp, data = ToothGrowth)]
#' @author Jacolien van Rij
#' @family Utility functions
#' 
sortGroups <- function(formula, FUN='mean', decreasing=FALSE, ...){
	tmp <- aggregate(formula, FUN=FUN, ...)
	idx <- sort.int(tmp[,ncol(tmp)], index.return=TRUE, decreasing=decreasing)
	return(idx$ix)
}
#' Order boxplot stats following a given ordering.
#' 
#' @export
#' @import stats
#' @param stats List with information produced by a box-and-whisker plot.
#' @param idx Order of group levels.
#' @return The ordered stats.
#' @examples
#' head(ToothGrowth)
#' # sort on basis of mean length:
#' bp <- boxplot(len ~ dose:supp, data = ToothGrowth, plot=FALSE)
#' idx <- sortGroups(len ~ dose:supp, data = ToothGrowth)
#' bp2 <- orderBoxplot(bp, idx)
#' # compare:
#' bp$names
#' bp2$names
#' @author Jacolien van Rij
#' @family Utility functions
#' 
orderBoxplot <- function(stats, idx){
	for( i in c("stats", "n", "conf", "names")){
		if(is.null( dim(stats[[i]]) ) ){
			stats[[i]] <- stats[[i]][idx]
		}else if ( length( dim(stats[[i]]) ) == 2  ){
			stats[[i]] <- stats[[i]][,idx]
		}
	}
	if(!is.null(stats$group) & length(stats$group) > 0){
		tmp <- 1:length(stats[['names']])
		names(tmp) <- idx
		stats[['group']] <- as.vector( tmp[as.character(stats[['group']])] )
	}
	return(stats)
}
#' Produce box-and-whisker plot(s) ordered by function such as 
#' mean or median.
#' 
#' @description Produce box-and-whisker plot(s) ordered by function such as 
#' mean or median. Wrapper around \code{\link[graphics]{boxplot}}.
#' @export
#' @import stats
#' @param formula a formula, such as 'y ~ Condition', where 'y' is a numeric vector
#' of data values to be split into groups according to the
#' grouping variable 'Condition' (usually a factor).
#' @param data a data.frame from which the variables in 'formula'
#' should be taken.
#' @param decreasing Logical:  Indicating whether the sort should be 
#' increasing or decreasing.
#' @param FUN a function to compute the summary statistics which can be
#' applied to all data subsets.
#' @param idx Numeric vector providing the ordering of groups instead of 
#' specifying a function in \code{FUN}. Only is used when \code{FUN} is set 
#' to NULL.
#' @param col Fill color of the boxplots. Alias for \code{boxfill}.
#' @param ... Other parameters to adjust the layout of the boxplots. 
#' See \code{\link[graphics]{bxp}}.
#' @return The ordered stats.
#' @examples
#' head(ToothGrowth)
#' # sort on basis of mean length:
#' sortBoxplot(len ~ dose:supp, data = ToothGrowth)
#' # sort on basis of median length:
#' sortBoxplot(len ~ dose:supp, data = ToothGrowth, decreasing=FALSE)
#' # on the basis of variation (sd):
#' sortBoxplot(len ~ dose:supp, data = ToothGrowth, FUN='sd', col=alpha(2))
#' @author Jacolien van Rij
#' @family Functions for plotting
#' 
sortBoxplot <- function(formula, data=NULL, 
	decreasing=TRUE, FUN = "median", idx=NULL, 
	col = "gray", ...){
	bps <- boxplot(formula, data=data, FUN=FUN, plot=FALSE, ...)
	additional <- par(...)
	# determine order:
	if(is.null(idx)){
		if(FUN %in% c("median", "min", "max", "lower.hinge", "upper.hinge", "n", "group.names")){
			if(FUN=="median"){
				sorti <- sort.int(bps$stats[3,], decreasing = decreasing, index.return=TRUE)
				idx <- sorti$ix
			}else if(FUN=="min"){
				sorti <- sort.int(bps$stats[1,], decreasing = decreasing, index.return=TRUE)
				idx <- sorti$ix
			}else if(FUN=="max"){
				sorti <- sort.int(bps$stats[5,], decreasing = decreasing, index.return=TRUE)
				idx <- sorti$ix
			}else if(FUN=="lower.hinge"){
				sorti <- sort.int(bps$stats[2,], decreasing = decreasing, index.return=TRUE)
				idx <- sorti$ix
			}else if(FUN=="upper.hinge"){
				sorti <- sort.int(bps$stats[4,], decreasing = decreasing, index.return=TRUE)
				idx <- sorti$ix
			}else if(FUN=="n"){
				sorti <- sort.int(bps$n, decreasing = decreasing, index.return=TRUE)
				idx <- sorti$ix
			}else if(FUN=="group.names"){
				sorti <- sort.int(bps$names, decreasing = decreasing, index.return=TRUE)
				idx <- sorti$ix
			}
		}else{
			idx <- sortGroups(formula, data=data, FUN=FUN, decreasing=decreasing)
			
		}
	}
	# order:
	bps <- orderBoxplot(bps, idx)
	# plot boxplots:
	par <- list(...)
	if(!"boxfill" %in% names(par)){
		par[['boxfill']] <- col
	}
	bp.args <- list2str(names(par), par)
	eval(parse(text=sprintf("bxp(bps, %s)",bp.args)))
	
	invisible(bps)
}





