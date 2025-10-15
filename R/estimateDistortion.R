estimateDistortion <- function(undistort.params, img.size, run.parallel=FALSE){

	nx <- 10
	ny <- 10

	# Generate evenly spaced corners throughout image
	d_corners <- cbind(c(matrix(t(matrix(seq(0, img.size[1], length=ny), nrow=ny, ncol=nx)), nrow=1, byrow=F)), 
		rep(seq(0,img.size[2],length=nx), ny))
	
	# Undistort using undistortion parameters
	u_corners <- undistort(d_corners, image.size=img.size, center=undistort.params[c('cx', 'cy')], 
		k=undistort.params[c('k1', 'k2', 'k3')], p=undistort.params[c('p1', 'p2')])

	# Try different starting parameters
	p_start <- list(
		rep(0, 5),
		c(0.1, NA, NA, NA, NA),
		c(0.1, 0.01, NA, NA, NA),
		c(0.14, 0.006, 0.002, NA, NA),
		c(0.1, 0.01, 1e-5, NA, NA),
		#c(0.1, 0.01, 1e-5, -1e-5, -0.1),
		c(0.01, -0.001, 1e-5, NA, NA),
		c(0.01, 0.1, -1e-5, NA, NA)
	)
	
	# Save with each try
	objectives <- rep(NA, length(p_start))
	par <- as.list(rep(NA, length(p_start)))
	
	# Save objective with no distortion
	objectives[1] <- distortionError(p=c(img.size[1]/2, img.size[2]/2, p_start[[1]]), 
		ucoor=u_corners, dcoor=d_corners, img.size=img.size)
	par[[1]] <- c(img.size[1]/2, img.size[2]/2, p_start[[1]])

	# Skip first (no distortion) case
	nlm.input <- lapply(2:length(p_start), function(i) {
		list(
			i=i,
			start=c(img.size[1]/2, img.size[2]/2, p_start[[i]]),
			objective=distortionError,
			ucoor=u_corners,
			dcoor=d_corners,
			img.size=img.size)
	})

	if(run.parallel == FALSE) {
		applyFn <- lapply
	} else {
		if(run.parallel == TRUE) {
			numCores <- detectCores()
		} else {
			numCores <- run.parallel
		}
		cl <- makeCluster(numCores)
			applyFn <- function(input, fn) {
			parLapply(cl, input, fn)
		}
	}

	# Find optimal distortion coefficients, skip if returns error
	nlm.results <- applyFn(nlm.input, function(params) {
		nlm.fit <- tryCatch(
			expr={
				nlminb(
					start=params$start,
					objective=params$objective,
					ucoor=params$ucoor,
					dcoor=params$dcoor,
					img.size=params$img.size)
			},
			error=function(cond) return(NULL),
			warning=function(cond) return(NULL)
		)
		list(i=params$i, fit=nlm.fit)
	})
	if(run.parallel) stopCluster(cl)

	invisible(lapply(nlm.results, function(result) {
		if(!is.null(result$fit)) {
			objectives[result$i] <- result$fit$objective
			par[[result$i]] <- result$fit$par
		}
	}))
	
	# Get parameter from run with the lowest error (including no distortion case)
	p <- par[[which.min(objectives)]]
	
	# Add names to parameters
	names(p) <- c('cx', 'cy', 'k1', 'k2', 'k3', 'p1', 'p2')

	return(p)
	
	# Check distortion parameters against original distorted corners
	#rd_corners <- undistort(u_corners, image.size=img.size, center=c(p[1], p[2]), 
	#	k=c(p[3], p[4], p[5]), p=c(p[6], p[7]))

	#errors <- sqrt(rowSums((d_corners - rd_corners)^2))
	#print(errors)

	#pdf('estimateDistortion.pdf')
	#plot(d_corners, asp=1)
	#points(u_corners, cex=0.5, col='red')
	#points(rd_corners, cex=0.7, col='green')
	#dev.off()
}
