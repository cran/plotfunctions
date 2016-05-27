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
#' @export
#' @param x A vector.
#' @return Standard Error of the mean.
#' @family Utility functions
se <- function(x){
    s <- sd(x)
    if (is.na(s)){
        warning("Problem in calculating SD.")
        return(NA)
    }
    else{ return(sd(x)/sqrt(length(x))) } 
}





