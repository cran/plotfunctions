#' Return n neighbors around given indices.
#' 
#' @export
#' @import grDevices
#' @import graphics
#' @param el A numeric vector.
#' @param n Number indicating how many points around the elements of \code{el} 
#' need to be selected.
#' @param max The maximum value of the returned elements.
#' @return A vector with the elements of x surrounded by n points.
#' @examples
#' vectorIndices <- 1:1000
#' indOutliers <- c(2,10, 473, 359, 717, 519)
#' fn3 <- find_n_neighbors(indOutliers, n=3, max=max(vectorIndices))
#' fn20 <- find_n_neighbors(indOutliers, n=20, max=max(vectorIndices))
#'
#' # check fn3:
#' print(fn3)
#'
#' # Plot:
#' emptyPlot(c(-10,1000), c(-1,1), h0=0, v0=indOutliers)
#' points(fn3, rep(.5, length(fn3)), pch='*')
#' points(fn20, rep(-.5, length(fn20)), pch='*')
#' @author Jacolien van Rij
#' @family Utility functions
find_n_neighbors <- function(el, n, max) {
    if (length(el) > 0) {
        new_el <- sort(unique(unlist(lapply(el, FUN = function(x) {
            return(sort(unique(c(x, (x - n):x, x:(x + n)))))
        }))))
        new_el <- new_el[new_el >= 1 & new_el <= max]
        return(new_el)
    } else {
        return(NULL)
    }
}





#' Return the value (or the element with the value) closest to zero. 
#' 
#' @export
#' @import stats
#' @param x A numeric vector.
#' @param element Logical: whether or not to return the value (FALSE, default) 
#' or the index (TRUE).
#' @return The value or index of the element closest to zero (absolute 
#' minimum).
#' @examples
#' (test <- seq(-25,25, by=3))
#' min(test[test>0])
#' max(test[test<0])
#' min(abs(test))
#' findAbsMin(test)
#' @author Jacolien van Rij
#' @family Utility functions
findAbsMin <- function(x, element = FALSE) {
    abs_x <- abs(x)
    el <- max(which(abs_x == min(abs_x)))
    if (element) {
        return(el)
    } else {
        return(x[el])
    }
}





#' Converts coordinates in current plot region to device positions (in inch).
#'
#' @export
#' @param x Numeric: x coordinate(s)
#' @param y Numeric: y coordinate(s)
#' @param units Coordinates (default) or proportions with respect to 
#' plot region.
#' @param dev x and y position are measured with respect to the 
#' plot region (default), figure panel, or device.
#' @return list 
#' @family Utility functions
getArrowPos <- function(x, y, 
	units=c("coords", "prop"), 
	dev=c("plot", "figure", "panel")){
	# process input 
	dev = tolower(dev[1])
	if(!dev %in% c("plot", "fig", "dev")){
		warning(sprintf("Incorrect device (dev) '%s'. Must be 'plot', 'fig' (figure), or 'dev' (device). By default 'plot' is selected.", dev))
		dev = "plot"
	}
	units = tolower(units[1])
	if(!units %in% c("prop", "coords", "proportions", "coordinates", 'p', 'c')){
		warning(sprintf("Incorrect units '%s'. Must be 'prop'/'p' (proportions), or 'coords'/'c' (coordinates). By default 'coords' is selected.", units))
		units="coords"
	}else{
		if(tolower(substr(units,1,1))=="p"){
			units="prop"
		}else if(tolower(substr(units,1,1))=="c"){
			units="coords"
		}
	}
	if((dev != "plot") & units=="coords"){
		warning(sprintf("Units will be set to 'prop' (proportions), because dev is set to '%s'. So x and y are proportions of the %s region.", dev))
		units="prop"
	}
	
	# convert input
	# din = device dimensions, fin = figure dimensions, pin = plot dimensions
	# plt = A vector of the form c(x1, x2, y1, y2) giving the coordinates 
	# of the plot region as fractions of the current figure region.
	# fig = fig A numerical vector of the form c(x1, x2, y1, y2) 
	# which gives the (NDC) coordinates of the figure region in 
	# the display region of the device.
	# usr = A vector of the form c(x1, x2, y1, y2) giving the extremes of 
	# the user coordinates of the plotting region.
	x2 <- NULL
	y2 <- NULL
	d <- par()$din
	f <- par()$fin
	p <- par()$pin
	if(dev=="plot"){
		# convert coords to proportions of plot region
		if(units=="coords"){
			pr <- par()$usr
			x <- (x-pr[1])/(pr[2]-pr[1])
			y <- (y-pr[3])/(pr[4]-pr[3])
		}
		# convert to coords of fig region (inches):
		pr <- par()$plt
		x2 <- x*p[1]+ pr[1]*f[1] 
		y2 <- y*p[2]+ pr[3]*f[2]
		# convert to coords of dev region (inches):
		pr <- par()$fig
		x2 <- x2 + d[1]*pr[1]
		y2 <- y2 + d[2]*pr[3]
	}else if(dev=="fig"){
		# convert to coords of dev region (inches):
		pr <- par()$fig
		x2 <- x*f[1] + d[1]*pr[1]
		y2 <- y*f[2] + d[2]*pr[3]		
	}else if(dev=="dev"){
		# convert to coords of dev region (inches):
		pr <- par()$fig
		x2 <- x*d[1]
		y2 <- y*d[2]
	}
	return(list(x=x2, y=y2))
}





#' Return the number of decimal places.
#' 
#' @export
#' @import stats
#' @param x A numeric vector.
#' @return Number of decimals
#' @examples
#' getDec(c(10,10.432, 11.01, .000001))
#' @author Based on http://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r, but improved
#' @family Utility functions
#' 
getDec <- function(x){
	dec <- 0
	dec <- sapply(x, function(a){
		if ((a %% 1) != 0) {
			chnum <- as.character(a)
			if(grepl("e\\+", chnum)){
				return(0)
			}else if (grepl("e\\-", chnum)){
				return( as.numeric(gsub("^([0-9]+)(e\\-)([0-9]+)$", "\\3", chnum)) )
			}else{
				return( nchar(unlist(strsplit(sub('0+$', '', chnum), ".", fixed=TRUE))[[2]]) )
			}
	    } else {
	        return(0)
	    }
	})
	return(dec)
}





#' Function for rounding and/or segmenting a range.
#' 
#' @export
#' @import stats
#' @param x A numeric vector.
#' @param dec Number of decimal points for rounding using function 
#' \code{\link[base]{round}}. Applied after argument 
#' \code{step}. If NULL (default), no rounding is applied.
#' @param step Round the 
#' @param n.seg Numeric value, number of values in the equally spaced sequence. 
#' Default is 2 (min, max).
#' @return vector, range of equally spaced sequence.
#' @examples
#' zlim <- c(-2.5, 3.01)
#' # does not change anything:
#' getRange(zlim)
#' # create a range of 5 numbers: 
#' # (basically just using seq )
#' getRange(zlim, n.seg=5)
#' # rounds the numbers:
#' getRange(zlim, dec=0)
#' getRange(zlim, n.seg=5, dec=0)
#' # extreme values are multiplications of 5
#' # that contains zlim values:
#' getRange(zlim, step=5)
#' getRange(zlim, step=5, n.seg=5)
#' # similar, but not the same:
#' getRange(zlim, n.seg=5, dec=0)
#' getRange(zlim, n.seg=5, step=1)
#' # combining:
#' getRange(zlim, n.seg=5, step=1, dec=0)
#' 
#' @author Jacolien van Rij
#' @family Utility functions
#' 
getRange <- function(x, dec=NULL, step=NULL, n.seg=2){
	vals <- seq(min(x), max(x), length=n.seg)
    if (!is.null(step)){
        vals <- seq(floor(min(x)/step)*step, ceiling(max(x)/step)*step, length=n.seg)
    }
    if (!is.null(dec)){
        vals <- round(vals, dec)
    }
    return(vals)
}





#' Move a vector n elements forward or backward.
#' 
#' @export
#' @param ratio Numeric, height : width ratio. If \code{ratio} > 1, the width is 
#' larger than the height, if \code{ration} < 1, the height is 
#' larger than the width.
#' @param width The desired width in plot coordinates or proportions. 
#' If not specified (NULL), 
#' the maximal width fitting in the plot region is returned.
#' @param height The desired height in plot coordinates or proportions. 
#' If not specified (NULL), 
#' the maximal height fitting in the plot region is returned.
#' @param input Unit of input width and height, "coords" (plot coordinates, default),
#' or "prop" (proportions of plot region).
#' @param ... Optional arguments: \code{xcenter}, \code{xleft}, or 
#' \code{xright}, and \code{ycenter}, \code{ybottom}, or 
#' \code{ytop} could be specified. If not specified, the width and height are 
#' centered around the center of the plot. 
#' @return A list with 5 elements:
#' \itemize{
#' \item \code{width}: width of the element in x-axis coordinates;
#' \item \code{height}: height of the element in y-axis coordinates;
#' \item \code{ratio}: provided ratio (for confirmation);
#' \item \code{x}: two-number vector with x-coordinates of 
#' left and right sides;
#' \item \code{y}: two-number vector with y-coordinates of 
#' bottom and top sides.
#' }
#' @examples
#' data(img)
#' emptyPlot(100, c(50, 100), h0=0, v0=0)
#' # calculate height : width ratio of image:
#' im.r <- dim(img$image)[1]/dim(img$image)[2]
#' p <- getRatioCoords(ratio=im.r, width=20)
#' # inspect p:
#' p
#' # No position specified, so centered:
#' plot_image(img, type="image", add=TRUE,
#'     xrange=p$x, yrange=p$y)
#' # ... or we could provide a position:
#' p <- getRatioCoords(ratio=im.r, width=20,
#'     xleft=20, ybottom=60)
#' plot_image(img, type="image", add=TRUE,
#'     xrange=p$x, yrange=p$y)
#' 
#' # Using proportions of plot region:
#' p <- getRatioCoords(ratio=im.r, height=.5,
#'     xleft=0, ytop=1, input="prop")
#' plot_image(img, type="image", add=TRUE,
#'     xrange=p$x, yrange=p$y)
#' 
#' # Changing the ratio to square:
#' p <- getRatioCoords(ratio=1, height=.5,
#'     xright=1, ybottom=0, input="prop")
#' plot_image(img, type="image", add=TRUE,
#'     xrange=p$x, yrange=p$y)
#' # ... and to a long rectangle:
#' p <- getRatioCoords(ratio=.5, height=1,
#'     xright=1, ybottom=0, input="prop")
#' plot_image(img, type="image", add=TRUE,
#'     xrange=p$x, yrange=p$y, 
#'     replace.colors=list("#B.+"="#FF000033"),
#'     border='red')
#' 
#' @author Jacolien van Rij
#' @family Utility functions
getRatioCoords <- function(ratio, 
    width=NULL, height=NULL, 
    input=c("coords", "prop"),
    ...) {
    # global variables:
    out.width  = NULL
    out.height = NULL
    gfc = getFigCoords("p")
    pin.c = c(gfc[2]-gfc[1], gfc[4]-gfc[3]) # in coordinates
    pin.i = par()$pin                       # in inches
    pin.f = pin.c / pin.i                   # recalculating c/in
    xleft = NULL
    ybottom = NULL
    # check input 
    if(length(input) > 1){
        input =  input[1]
    }
    if(!input %in% c("coords", "prop")){
        stop(sprintf("Input '%s' not valid - should be one of 'coords' or 'prop'.", input))
    }
    if(is.null(width) & is.null(height)){
        if(ratio > 1){
            height = gfc[4]-gfc[3]
        }else{
            width = gfc[2] - gfc[1]
        }
    }
    # first convert height to inches
    if(is.null(width)){
        if(input == "prop"){
            height = height*pin.c[2]
        }
        out.height = height*(1/pin.f[2])
        out.width  = ratio * out.height
    }else{
        if(!is.null(height)){
            warning("Both width and height are provided. Height is recalculated based on ratio.")
        }
        if(input == "prop"){
            width = width*pin.c[1]
        }
        out.width = width*(1/pin.f[1])
        out.height  = ratio * out.width
    }
    # then calculate back to coordinates
    out.height = out.height * pin.f[2]
    out.width  = out.width  * pin.f[1]
    par=list(...)
    if(input=="coords"){
        if("xcenter" %in% names(par)){
            xleft = par[['xcenter']] - (.5*out.width)
        }else if ("xleft" %in% names(par)){
            xleft = par[['xleft']]
        }else if ("xright" %in% names(par)){
            xleft = par[['xright']] - out.width
        }else{
            xleft = gfc[1] + pin.c[1]/2 - (.5*out.width)
        }
        if("ycenter" %in% names(par)){
            ybottom = par[['ycenter']] - (.5*out.height)
        }else if ("ybottom" %in% names(par)){
            ybottom = par[['ybottom']]
        }else if ("ytop" %in% names(par)){
            ybottom = par[['ytop']] - out.height
        }else{
            ybottom = gfc[3] + pin.c[2]/2 - (.5*out.height)
        }
    }else{
        if("xcenter" %in% names(par)){
            xleft = gfc[1] + par[['xcenter']]*pin.c[1] - (.5*out.width)
        }else if ("xleft" %in% names(par)){
            xleft = gfc[1] + par[['xleft']]*pin.c[1]
        }else if ("xright" %in% names(par)){
            xleft = gfc[1] + par[['xright']]*pin.c[1] - out.width
        }else{
            xleft = gfc[1] + pin.c[1]/2 - (.5*out.width)
        }
        if("ycenter" %in% names(par)){
            ybottom = gfc[3] + par[['ycenter']]*pin.c[2] - (.5*out.height)
        }else if ("ybottom" %in% names(par)){
            ybottom = gfc[3] + par[['ybottom']]*pin.c[2]
        }else if ("ytop" %in% names(par)){
            ybottom = gfc[3] + par[['ytop']]*pin.c[2] - out.height
        }else{
            ybottom = gfc[3] + pin.c[2]/2 - (.5*out.height)
        }
    }
    return(list(width=out.width, height=out.height, ratio=ratio,
        x=c(xleft, xleft+out.width), 
        y=c(ybottom, ybottom+out.height)))
} 





#' Sort split by grouping predictor.
#' 
#' @export
#' @description Function uses \code{\link[base]{sort.list}} to return indices
#' of of a vector, sorted per group.
#' @param x A vector to be sorted.
#' @param group A names list that specify the different groups to split the 
#' data.
#' @param decreasing Logical: whether or not the sort order should be 
#' decreasing.
#' @return Indices indicating the order of vector x per group.
#' @author Jacolien van Rij
#' @examples
#' # example InsectSprays from R datasets
#' InsectSprays$Type <- ifelse(InsectSprays$spray %in% c("A","B", "F"), 1, 2)
#' 
#' ind <- group_sort(InsectSprays$count, 
#'     group=list(Spray=InsectSprays$spray, Type=InsectSprays$Type))
#' InsectSprays[ind,]
#' InsectSprays
#' @seealso \code{\link[base]{sort.list}}
#' @family Utility functions
group_sort <- function(x, group=NULL, decreasing=FALSE){
    if(is.null(group)){
        return(sort.list(as.numeric(x), decreasing=decreasing))
    }else{
        el <- which(is.na(x))
        tmp <- data.frame(x=x, i=1:length(x))
        x.split <- split(tmp, f=group, drop=TRUE)
        x.split <- as.vector(unlist(lapply(x.split, 
            function(x){
                x[sort.list(as.numeric(x$x), decreasing=decreasing),'i']
            })))
        if(length(el) > 0){
            x.split <- c(x.split, el[!el %in% x.split])
        }
        return(x.split)
    }
}





#' Convert device position (inch) to coordinates in current plot region.
#'
#' @export
#' @param xpos x position in device, inches between position and left side 
#' of device.
#'  When defined as two-number vector, x- and y-position as measured from 
#' bottomleft corner of device.
#' @param ypos y position (in inches) from bottom of device.
#' @param simplify Logical: whether or not to output a vector instead of a list.
#' @return list or 2-number vector
#' @family Utility functions
#' 
inch2coords <- function(xpos, ypos=NULL, simplify=FALSE){
	# convert position in device to currect plot region
	x <- NULL
	y <- NULL
	if(!is.null(ypos)){
		x <- xpos
		y <- ypos
		if(length(x) != length(y)){
			stop("pos and ypos need to have the same length.")
		}
	}else{
		if(length(xpos) < 2){
			stop("If ypos is not specified, xpos needs to be a two-element vector c(x,y).")
		}
		if(is.list(xpos)){
			x <- xpos[[1]]
			y <- xpos[[2]]
		}else{
			x <- xpos[1]
			y <- xpos[2]
		}
		
	}
	x2 <- NULL
	y2 <- NULL
	d <- par()$din
	f <- par()$fin
	p <- par()$pin
	# 1. calc proportion of dev
	x <- x / d[1]
	y <- y / d[2]
	# 2. calc prop region as proportion of dev
	pr <- par()$fig
	curfig <- c(pr[1], pr[1]+f[1]/d[1], pr[3], pr[3]+f[2]/d[2])
	pr <- par()$plt
	curplot <- c(pr[1]*(f[1]/d[1])+curfig[1], pr[2]*(f[1]/d[1])+curfig[1], 
		pr[3]*(f[2]/d[2])+curfig[3],pr[4]*(f[2]/d[2])+curfig[3])
	# 3. convert new position to coords:
	pr <- par()$usr
	x2 <- (x-curplot[1]) / (curplot[2]-curplot[1])
	x2 <- x2*(pr[2]-pr[1])+pr[1]
	y2 <- (y-curplot[3]) / (curplot[4]-curplot[3])
	y2 <- y2*(pr[4]-pr[3])+pr[3]
	if(simplify){
		if(length(x2)==1){
			return(c(x2,y2))
		}
	}
	return(list(x=x2, y=y2))	
}





#' Check whether color specifications exists.
#'
#' @export
#' @import grDevices
#' @description Function to check whether all specified colors are 
#' actual colors.
#' @param x Vector of any of the three kinds of R color specifications, 
#' i.e., either a color name (as listed by 
#' \code{\link[grDevices]{palette}colors()}), a hexadecimal string of the form 
#' "#rrggbb" or "#rrggbbaa" (see rgb), or a positive integer i meaning 
#' \code{\link[grDevices]{palette}()[i]}.
#' @param return.colors Logical: logical values (FALSE, default) or 
#' returning colors (TRUE)
#' @author Jacolien van Rij
#' @return Logical value (or colors)
#' @family Utility functions
#' @examples
#' # correct color definitions:
#' isColor(c("#FF0000FF", "#00FF00FF", "#0000FFFF"))
#' isColor(c('red', 'steelblue', "green3"))
#' isColor(c(1,7,28))
#' # mixtures are possible too:
#' isColor(c("#FF0000FF", "red", 1, "#FF0000", rgb(.1,0,0)))
#' 
#' # return colors:
#' # note that 28 is converted to 4...
#' isColor(c(1,7,28), return.colors=TRUE) 
#' isColor(c("#FF0000CC", "red", 1, "#FF0000"), return.colors=TRUE)
#' 
#' # 4 incorrect colors, 1 correct:
#' test <- c("#FH0000", 3, "#FF00991", "lavendel", "#AABBCCFFF")
#' isColor(test)
#' isColor(test, return.colors=TRUE)
#' 
isColor <- function(x, return.colors=FALSE){
	# numeric colors, max 8
	if(is.numeric(x)){
		if(! return.colors){
			return(x > 0 & (x %% 1 == 0))
		}else{
			return(palette()[ (x - 1) %% 8 + 1])
		}
	}
	# convert any numeric values:
	if( any(grepl("^[0-9]+$", x))){
		x[grepl("^[0-9]+$", x)] <- palette()[ (as.numeric(x[grepl("^[0-9]+$", x)]) - 1) %% 8 + 1]
	}
	
	# color names and hexadecimal colors
	y <- grepl("^\\#[a-fA-F0-9]{6}$", x) | grepl("^\\#[a-fA-F0-9]{8}$", x) | (x %in% colors())
	if( ! return.colors){
		return( y )
	}else{
		x[!y] <- NA 
		return(x)
	}
}





#' Combine list values as string.
#'
#' @export
#' @param x A vector with the names or numbers of list elements to be combined.
#' @param inputlist A (named) list with information, e.g., graphical parameter settings.
#' @return String
#' @examples
#' test <- list(a=c(1,2,3), b="a", c=c(TRUE, FALSE), d="test")
#' list2str(c("a","c", "d"), test) 
#' @family Utility functions
list2str <- function(x, inputlist) {
    out <- c()
    for(i in x){
        name.i <- NULL
        val.i  <- NULL
        if(is.numeric(i)){
            if(i > 0 & i <= length(inputlist)){
                name.i <- sprintf("el%.0f", i)
                val.i  <- inputlist[[i]]
            }
        }else if(i %in% names(inputlist)){
            name.i <- i
            val.i  <- inputlist[[i]]
        }
        if(! is.null(name.i)){
            if(inherits(val.i, c("numeric", "logical"))){
                out <- c(out, sprintf("%s=c(%s)", name.i, paste(val.i, collapse=",")))
            }else if(inherits(val.i, c("character", "factor"))){
                out <- c(out, sprintf("%s=c(%s)", name.i, paste(sprintf("'%s'",val.i), collapse=",")))
            }else{
                warning(sprintf("Class %s is not supported, element %s is ignored.",
                    class(name.i)[1], name.i))
            } 
        }
    }
    return(paste(out, collapse=", "))
}





#' Move a vector n elements forward or backward.
#' 
#' @export
#' @param x A vector.
#' @param n Number indicating how many steps the vector should shift forward 
#' (N > 0) or backward (n < 0).
#' @param na_value The value to replace the empty cells with (e.g., the first 
#' or last points). Defaults to NA.
#' @return A vector with the same length of \code{x}, all moved \code{n} steps.
#' @examples
#' (x <- -10:30)
#' prev <- move_n_point(x)
#' change <- x - prev
#' post5 <- move_n_point(x, n=-5)
#'
#' emptyPlot(length(x), range(x))
#' lines(x)
#' lines(prev, col='red')
#' lines(post5, col='blue')
#'
#' @author Jacolien van Rij
#' @family Utility functions
move_n_point <- function(x, n = 1, na_value = NA) {
    x <- as.vector(x)
    
    if (length(x) > abs(n)) {
        if (n > 0) {
            return(c(rep(na_value, n), x[1:(length(x) - n)]))
        } else {
            return(c(x[(abs(n) + 1):(length(x))], rep(na_value, abs(n))))
        }
    } else if (length(x) == abs(n)) {
        return(NA)
    } else {
        return(NULL)
    }
} 





#' Calculate standard error of the mean.
#' 
#' @import datasets
#' @import grDevices
#' @import utils
#' @export
#' @param x A vector.
#' @return Standard Error of the mean.
#' @family Utility functions
#' @examples
#' # load example data:
#' data(chickwts)
#' str(chickwts)
#' 
#' # first calculate means per feeding type:
#' avg <- with(chickwts, tapply(weight, list(feed), mean))
#' par(cex=1.25)
#' b <- barplot(avg, beside=TRUE, names.arg=FALSE, ylim=c(0,450))
#' text(b, rep(0, length(b)), labels=names(avg), srt=90, adj=-.25)
#' 
#' # calculate mean collapsing over feeding types:
#' abline(h=mean(avg), lwd=1.5, col="red1")
#' # add SE reflecting variation between feeding types:
#' abline(h=mean(avg)+c(-1,1)*se(avg), lty=2, col="red1")
#' text(getCoords(.5), mean(avg)+se(avg), 
#'     labels=expression("mean" %+-% "1SE"), pos=3, col="red1")
#' 
#' # Note that SE makes more sense for experiments with 
#' # different groups or participants.
#' 
se <- function(x){
    s <- sd(x)
    if (is.na(s)){
        warning("Problem in calculating SD.")
        return(NA)
    }
    else{ return(sd(x)/sqrt(length(x))) } 
}





