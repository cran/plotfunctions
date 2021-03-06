% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{alpha}
\alias{alpha}
\title{Adjusting the transparency of colors.}
\usage{
alpha(x, f = 0.5)
}
\arguments{
\item{x}{A color or a vector with color values.}

\item{f}{A number for adjusting the transparency ranging from 0 (completely 
transparent) to 1 (not transparent).}
}
\description{
Wrapper around \code{\link[grDevices]{adjustcolor}}.
}
\section{Note}{
 
Does not always work for x11 panels.
}

\examples{
emptyPlot(100,100, h=50, v=50)
rect(25,25,75,75, col=alpha('red',f=1))
rect(35,41,63,81, col=alpha(rgb(0,1,.5),f=.25), 
   border=alpha(rgb(0,1,.5), f=.65), lwd=4)

emptyPlot(1,1, axes=FALSE, main='Tunnel of 11 squares')
center <- c(.75, .25)
mycol <- 'steelblue'
for(i in seq(0,1,by=.1)){
    rect(center[1]-center[1]*(1.1-i), center[2]-center[2]*(1.1-i), 
        center[1]+(1-center[1])*(1.1-i), center[2]+(1-center[2])*(1.1-i), 
        col=alpha(mycol, f=i), border=mycol, lty=1, lwd=.5, xpd=TRUE)
}
axis(1, at=center[1]-center[1]*(1.1-seq(0,1,by=.1)), labels=seq(0,1,by=.1))

# see alphaPalette for an elaboration of this example

}
\seealso{
Other Functions for plotting: 
\code{\link{addInterval}()},
\code{\link{add_bars}()},
\code{\link{add_n_points}()},
\code{\link{alphaPalette}()},
\code{\link{check_normaldist}()},
\code{\link{color_contour}()},
\code{\link{dotplot_error}()},
\code{\link{drawDevArrows}()},
\code{\link{emptyPlot}()},
\code{\link{errorBars}()},
\code{\link{fill_area}()},
\code{\link{getCoords}()},
\code{\link{getFigCoords}()},
\code{\link{getProps}()},
\code{\link{gradientLegend}()},
\code{\link{legend_margin}()},
\code{\link{marginDensityPlot}()},
\code{\link{plot_error}()},
\code{\link{plot_image}()},
\code{\link{plotsurface}()},
\code{\link{sortBoxplot}()}
}
\concept{Functions for plotting}
\concept{Utility functions for plotting}
