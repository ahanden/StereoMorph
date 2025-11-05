estimateUndistortion <- function(coor.2d, cal.nx, image.size, run.parallel = FALSE){

	# GET NUMBER OF CORNERS IN OTHER DIMENSION
	cal.ny <- dim(coor.2d)[1] / cal.nx

	# SET PLANAR 3D COORDINATES (OBJECT COORDINATES)
	#coor_obj <- cbind(rep(0:(cal.nx-1), cal.ny), c(matrix(t(matrix((cal.ny-1):0, nrow=cal.ny, ncol=cal.nx)), nrow=1, byrow=F)), rep(0, cal.nx*cal.ny))
	coor_obj <- cbind(
		c(matrix(t(matrix(0:(cal.ny-1), nrow=cal.ny, ncol=cal.nx)), nrow=1, byrow=F)), 
		rep(0:(cal.nx-1), cal.ny), 
		rep(0, cal.nx*cal.ny)
	)

	# FILL ARRAY OF SAME DIMENSIONS AS 2D COORDINATES
	coor_obj_array <- array(coor_obj, dim = c(dim(coor_obj), dim(coor.2d)[3]))
	
	# REMOVE NA ASPECTS
	coor_obj_array <- coor_obj_array[, , !is.na(coor.2d[1,1,])]
	coor.2d <- coor.2d[, , !is.na(coor.2d[1,1,])]

	# TRY DIFFERENT STARTING PARAMETERS
	p_start <- list(
		rep(0, 5),
		c(0.1),
		c(0.1, 0.01),
		c(0.14, 0.006, 0.002),
		c(0.1, 0.01, 1e-5),
		#c(0.1, 0.01, 1e-5, -1e-5, -0.1),
		c(0.01, -0.001, 1e-5),
		c(0.01, 0.1, -1e-5)
	)
	
	# SAVE WITH EACH TRY
	objectives <- rep(NA, length(p_start))
	par <- as.list(rep(NA, length(p_start)))
	
	# SAVE OBJECTIVE WITH NO DISTORTION
	objectives[1] <- undistortionError(p=c(image.size[1]/2, image.size[2]/2, p_start[[1]]), 
		coor.img=coor.2d, coor.obj=coor_obj_array, image.size=image.size)
	par[[1]] <- c(image.size[1]/2, image.size[2]/2, p_start[[1]])

	if (run.parallel) {
		if (run.parallel == TRUE) {
			run.parallel <- parallel::detectCores()
		}
		cl <- parallel::makeCluster(min(run.parallel, length(p_start) - 1))
                parallel::clusterEvalQ(cl, library(StereoMorph))
		applyFn <- function(input, fn) { parallel::parLapply(cl, input, fn) }
	} else {
		applyFn <- lapply
	}

	inputs <- lapply(2:length(p_start), function(i) {
		list(i = i,
                     start = c(image.size[1] / 2, image.size[2] / 2, p_start[[i]]),
                     objective = undistortionError,
                     coor.img = coor.2d,
                     coor.obj = coor_obj_array,
                     image.size = image.size)
	})
        results <- applyFn(inputs, function(params) {
          nlm_fit <- nlminb(start = params$start,
                            objective = params$objective,
                            coor.img = params$coor.img,
                            coor.obj = params$coor.obj,
                            image.size = params$image.size)
          list(i=params$i, fit=nlm_fit)
        })
        for (fit in results) {
		if(!is.null(fit$fit)) {
                  objectives[fit$i] <- fit$fit$objective
                  par[[fit$i]] <- fit$fit$par
                }
	}

        if (run.parallel) parallel::stopCluster(cl)
	
	# GET PARAMETERS FROM RUN WITH LOWEST ERROR (INCLUDING NO DISTORTION CASE)
	dist_params <- par[[which.min(objectives)]]
	dist_params <- c(dist_params, rep(NA, 7 - length(dist_params)))
	
	# ADD NAMES TO PARAMETERS
	names(dist_params) <- c('cx', 'cy', 'k1', 'k2', 'k3', 'p1', 'p2')
	
	dist_params
}
