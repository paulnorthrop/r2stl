# r2stl.R - produce an STL file containing a 3D surface plot
# suitable for printing using a 3D printer or rapid prototyper
# Version 0.1, 2012 by Ian Walker, University of Bath, i.walker@bath.ac.uk
# Released under a Creative Commons BY-NC-SA licence

# The data take the same form as in R's persp() plots: x and y
# represent a grid and z gives heights above this grid

#' Save R data to an STL file
#' 
#' `r2stl` takes numeric input exactly as with R's normal `persp()` function. 
#' The output is a STL (stereolithography) file.
#'
#' @param x A numeric vector with the x-coordinates to plot.
#' @param y A numeric vector with the y-coordinates to plot.
#' @param z A numeric vector with the z-coordinates to plot.
#' @param filename The STL filename.
#' @param object.name The object that is being described must have a name 
#'   specified inside the file. There's probably no point changing it from the 
#'   default.
#' @param z.expand To force the 3D plot to touch all six faces of the imaginary 
#'   cube that surrounds it, set this parameter to TRUE.
#' @param min.height The minimum height for the printed material.
#' @param show.persp If set to `TRUE`` then a `persp()` plot of this object is 
#'   shown on the screen.
#' @param strict.stl If set to `TRUE` it makes files smaller but isn't 
#'   strictly proper STL format.
#'
#' @details To view and test the STL files before printing them can be done 
#'   with many programs, for example an open-source option is Meshlab 
#'   \url{http://meshlab.sourceforge.net/}.   
#'
#' @author Ian Walker.
#' 
#' @keywords Programming
#' 
#' @return None.   
#' 
#' @examples
#' # Let's do the classic persp() demo plot
#' x <- seq(-10, 10, length= 100)
#' y <- x
#' f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
#' z <- outer(x, y, f)
#' z[is.na(z)] <- 1
#' r2stl(x, y, z, filename = "lovelyfunction.stl", show.persp = TRUE)
#'
#' # Now let's look at R's Volcano data
#' z <- volcano
#' x <- 1:dim(volcano)[1]
#' y <- 1:dim(volcano)[2]
#' r2stl(x, y, z, filename = "volcano.stl", show.persp = TRUE)
#' @export
r2stl <- function(x, y, z, filename='3d-R-object.stl', object.name='r2stl-object', z.expand=FALSE, min.height=0.008, show.persp=FALSE, strict.stl=FALSE) {
    # NB assuming a 60mm height for printed object, default min.height of 
    # 0.008 gives a minimum printed height of 0.5mm
    
    # *Auto setting* If min.height >= 1, we interpret this not as the minimum 
    # proportion of the object to be printed, but as the height
    # of the printed object in mm, and provide a 0.5 mm minimum
    # (0.5 mm seems a common minimum recommended height for many 3D printers)

if (!is.numeric(x)) stop('Argument <<x>> should be a number')
if (!is.numeric(y)) stop('Argument <<y>> should be a number')
if (!is.numeric(z)) stop('Argument <<z>> should be a number')
if (!is.character(filename)) stop('Argument <<filename>> should be a string')
if (!is.character(object.name)) stop('Argument <<object.name>> should be a string')
if (!is.logical(z.expand)) stop('Argument <<z.expand>> should be a boolean')
if (!is.numeric(min.height)) stop('Argument <<min.height>> should be a number')
if (!is.logical(show.persp)) stop('Argument <<show.persp>> should be a boolean')
if (!is.logical(strict.stl)) stop('Argument <<strict.stl>> should be a boolean')

    if (min.height >= 1) {
        min.height <- 0.5 / min.height
    }

	# sanity checks
    if (length(x) < 3 | length(y) < 3 | length(z) < 3) {
        stop("You do not appear to have enough data for a plot to be generated")
    }
    
    ##
    # Define some functions to be used later
    ##

	# function to normalize scores on a scale from 0 to 1
	normit <- function(x) { 
		xprime <- (x - min(x, na.rm=TRUE)) / ( max(x, na.rm=TRUE) - min(x, na.rm=TRUE) )
		return(xprime)
	}

    # function to provide a minimum z height (to avoid too-thin printing)
    correct.min <- function(x) {
        xprime <- x
        xprime[xprime < min.height] <- min.height
        return(xprime)
    }
    
    ##
    # Enough functions, let's get processing
    ##
    
	# open file for writing
	fp <- file(filename, open="w")
    if (!fp) { stop("There was a problem opening the file for writing") }

	# normalize all data onto a scale of 0 to 1
	zz <- normit(z)
	xx <- normit(x)
	yy <- normit(y)
	
	# z range has been normalized to fill the same 0 to 1 range as the x and y data. 
    # This is necessary to get everything onto a size-neutral 0 to 1 range, but
    # often messes up prints. So if required, rescale the z scores back to the original data range
	if(!z.expand) { 
		zz <- zz * ( (max(z) - min(z)) / max(z) ) 
		if (max(zz) > 1 | min(zz) < 0) zz <- normit(zz) # if -ve numbers have messed things
	}

    # to avoid trying to print infintesimally thin surfaces, provide a minimum height in 
    # the z data
    if (min.height) { # gives the option to set it to FALSE. Don't know why you would though
        zz <- correct.min(zz)
    }

	# Option to see a surfaceplot of your data as the 3D version is generated
    if (show.persp) {
    	persp(xx,yy,zz, xlim=c(0,1), ylim=c(0,1), zlim=c(0,1), theta=120, phi=15, col="lightgreen")
    }
    
	# Output file header 	
	write(sprintf('solid %s created using r2stl.r by Ian Walker, University of Bath', object.name), file=fp)
	
	###
	# Begin the six faces
	###

	# The approach is to picture the object as sitting inside a cube and go around
	# the six faces producing triangles from the x, y, z grid.
	#
	# The run along each face divides the rectangles in the data into triangles.
	# The two triangles in each rectangle are arbitrarily called A and B. On the side
	# faces, A is the lower triangle on the z axis and B the upper; on the top and 
	# bottom faces, A is the triangle lower on the y axis. 
		
	# First side face, y is fixed at 0 and x increments
	for (i in 1:(length(xx)-1)) { 
		# to length-1 as we triangulate from a point to its next neighbour and so
		# the penultimate step will take us to the far edge

		j = 0 # fix the non-moving axis

		# triangle A
		write('  facet normal 0.0 -1.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, zz[i+1, j+1]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		#triangle B
		write('  facet normal 0.0 -1.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, zz[i+1, j+1]), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, zz[i+1,j+1]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	}
	
	# Second side face, x is fixed at 0 and y increments
	for (i in 1:(length(yy)-1) ) { 
		j = 0			

		# triangle A
		write('  facet normal -1.0 0.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], zz[j+1, i+1]), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], 0), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		#triangle B
		write('  facet normal -1.0 0.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], zz[j+1,i]), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], zz[j+1, i+1]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	}

	# Third side face, y is fixed at its max value and x increments
	for (i in 1:(length(xx)-1) ) { 
		j = 1 #normalized highest value
		k = length(yy) # actual highest value (for addressing the array)

		# triangle A
		write('  facet normal 0.0 1.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, zz[i+1, k]), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, 0), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		#triangle B
		write('  facet normal 0.0 1.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, zz[i, k]), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, zz[i+1, k]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	}
	
	# Fourth side face, x is fixed at its max value and y increments
	for (i in 1:(length(yy)-1) ) { 
		j = 1		
		k = length(xx)

		# triangle A
		write('  facet normal 1.0 0.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], zz[k, i+1]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		#triangle B
		write('  facet normal 1.0 0.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], zz[k, i+1]), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], zz[k,i]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	}

	# top face - run through the x by y grid as seen from above
	for (i in 1:(length(xx)-1) ) {
		for (j in 1:(length(yy)-1) ) {
	
			# triangle A
			write('  facet normal 0.0 0.0 1.0', file=fp)
			write('    outer loop', file=fp)
			write(sprintf('      vertex %f %f %f', xx[i], yy[j], zz[i,j]), file=fp)
			write(sprintf('      vertex %f %f %f', xx[i+1], yy[j], zz[i+1,j]), file=fp)
			write(sprintf('      vertex %f %f %f', xx[i+1], yy[j+1], zz[i+1,j+1]), file=fp)
			write('    endloop', file=fp)
			write('  endfacet', file=fp)
			
			#triangle B
			write('  facet normal 0.0 0.0 1.0', file=fp)
			write('    outer loop', file=fp)
			write(sprintf('      vertex %f %f %f', xx[i], yy[j], zz[i,j]), file=fp)
			write(sprintf('      vertex %f %f %f', xx[i+1], yy[j+1], zz[i+1,j+1]), file=fp)
			write(sprintf('      vertex %f %f %f', xx[i], yy[j+1], zz[i,j+1]), file=fp)
			write('    endloop', file=fp)
			write('  endfacet', file=fp)
		}
	}
	
	# Bottom face. This is always a flat rectangle so we can cheat by making it two 
	# massive triangles. But as this isn't strict STL format we offer a fully-
	# triangulated version of the grid, albeit at the cost of larger files
	if (!strict.stl) {
		write('  facet normal 0.0 0.0 -1.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', 0, 0, 0), file=fp)
		write(sprintf('      vertex %f %f %f', 1, 1, 0), file=fp)	
		write(sprintf('      vertex %f %f %f', 1, 0, 0), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	
		write('  facet normal 0.0 0.0 -1.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', 0, 0, 0), file=fp)
		write(sprintf('      vertex %f %f %f', 0, 1, 0), file=fp)
		write(sprintf('      vertex %f %f %f', 1, 1, 0), file=fp)	
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	} else { # copy of top-face code, with all z values set to zero
		for (i in 1:(length(xx)-1) ) {
			for (j in 1:(length(yy)-1) ) {

				# triangle A
				write('  facet normal 0.0 0.0 -1.0', file=fp)
				write('    outer loop', file=fp)
				write(sprintf('      vertex %f %f %f', xx[i], yy[j], 0), file=fp)
				write(sprintf('      vertex %f %f %f', xx[i+1], yy[j], 0), file=fp)
				write(sprintf('      vertex %f %f %f', xx[i+1], yy[j+1], 0), file=fp)
				write('    endloop', file=fp)
				write('  endfacet', file=fp)
			
				#triangle B
				write('  facet normal 0.0 0.0 -1.0', file=fp)
				write('    outer loop', file=fp)
				write(sprintf('      vertex %f %f %f', xx[i], yy[j], 0), file=fp)
				write(sprintf('      vertex %f %f %f', xx[i+1], yy[j+1], 0), file=fp)
				write(sprintf('      vertex %f %f %f', xx[i], yy[j+1], 0), file=fp)
				write('    endloop', file=fp)
				write('  endfacet', file=fp)
			}
		}
	} 
	
	###
	# End of the six faces
	###

	# Write the footer and end the file
	write(sprintf('endsolid %s', object.name), file=fp)
	close(fp)
}

