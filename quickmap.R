quickmap <- function(tif.file, band = 1, palette = 'jet', reverse = F, classes = NULL, margins = F) {
	# ------------------------------------------------------------------
	# A quick GeoTIFF viewer using terra + rasterVis.
	#
	# Continuous mode:
	#   Uses gradient palette.
	#
	# Categorical mode:
	#   Supply `classes = list(list(val=..., col=..., lab=...), ...)`
	#   No raster value scanning performed.
	#
	# Args:
	#   tif.file  : path to raster
	#   band      : band number (default 1)
	#   palette   : palette name (continuous only)
	#   reverse   : reverse palette
	#   classes   : optional categorical class definitions
	#   margins   : include marginal histograms
	#
	# Returns:
	#   None. Plots raster.
	# 
	# Examples:
	#   # default is continuous mode
	#   quickmap('input.tif')
	#
	#   # specify class definitions to plot categorical map
	#   quickmap('input.tif', classes = list(
	#       list(val = 0, col = 'red', lab = 'outside'), 
	#       list(val = 1, col = 'wheat', lab = 'inside')
	#   ))
	#
	# ------------------------------------------------------------------

	# check that raster exists
	if (!file.exists(tif.file)) stop(sprintf('%s does not exist.', tif.file))

	# select band if multi-layer
	n.bands <- terra::nlyr(terra::rast(tif.file))
	if (n.bands < band) {
		stop(sprintf('Requested band %s, but raster has %s layer(s).', band, n.bands), call. = F)
	}

	# read raster
	ras <- terra::rast(tif.file, lyrs = band)

	# map title
	title <- if (n.bands > 1) sprintf('%s - Band %s', basename(tif.file), band) else basename(tif.file)

	# get plot params
	if (is.null(classes)) {
		# params <- continuous.helper(palette, reverse)

		# palettes
		cp.vir <- c('#440154FF','#472D7BFF','#3B528BFF','#2C728EFF','#21908CFF','#27AD81FF','#5DC863FF','#AADC32FF','#FDE725FF')
		cp.mag <- c('#000004FF','#1D1147FF','#51127CFF','#822681FF','#B63679FF','#E65164FF','#FB8861FF','#FEC287FF','#FCFDBFFF')
		cp.inf <- c('#000004FF','#210C4AFF','#56106EFF','#89226AFF','#BB3754FF','#E35932FF','#F98C0AFF','#F9C932FF','#FCFFA4FF')
		cp.pla <- c('#0D0887FF','#4C02A1FF','#7E03A8FF','#A92395FF','#CC4678FF','#E56B5DFF','#F89441FF','#FDC328FF','#F0F921FF')
		cp.rai <- c('#FF0000FF','#FFAA00FF','#AAFF00FF','#00FF00FF','#00FFAAFF','#00AAFFFF','#0000FFFF','#AA00FFFF','#FF00AAFF')
		cp.ter <- c('#00A600FF','#3EBB00FF','#8BD000FF','#E6E600FF','#E8C32EFF','#EBB25EFF','#EDB48EFF','#F0C9C0FF','#F2F2F2FF')
		cp.gry <- c('#4D4D4D','#6F6F6F','#888888','#9D9D9D','#AEAEAE','#BEBEBE','#CCCCCC','#D9D9D9','#E6E6E6')
		cp.jet <- c('#0000FF','#0055FF','#00AAFF','#00FFFF','#55FFAA','#AAFF55','#FFFF00','#FFAA00','#FF5500')
		cp.mat <- c('#0000AA','#0055FF','#00AAFF','#55FFFF','#AAFFAA','#FFFF55','#FFAA00','#FF5500','#AA0000')
		cp.b2r <- c('#0000FF','#0040FF','#0080FF','#00BFFF','#00FFFF','#FFBF00','#FF8000','#FF4000','#FF0000')
		cp.spe <- c('#D53E4F','#F46D43','#FDAE61','#FEE08B','#FFFFBF','#E6F598','#ABDDA4','#66C2A5','#3288BD')

		# select color palette
		cols <- switch(
			palette,
			viridis   = cp.vir,
			magma     = cp.mag,
			inferno   = cp.inf,
			plasma    = cp.pla,
			rainbow   = cp.rai,
			terrain   = cp.ter,
			grayscale = cp.gry,
			jet       = cp.jet,
			matlab    = cp.mat,
			blue2red  = cp.b2r,
			spectral  = cp.spe,
			NULL
		)
		if (is.null(cols)) stop('incorrect color palette choice.', call. = F)

		# reverse color palette
		if (reverse) cols <- rev(cols)

		# create palette function
		cols <- colorRampPalette(cols)

		# define basic colorkey
		colorkey <- list(space = 'right')

	} else {

		# params <- categorical.helper(classes)
		vals <- vapply(classes, `[[`, numeric(1), 'val')
		cols <- vapply(classes, `[[`, character(1), 'col')
		labs <- vapply(classes, `[[`, character(1), 'lab')

		# order by value (recommended)
		ord <- order(vals)
		vals <- vals[ord]; cols <- cols[ord]; labs <- labs[ord]

		# discrete breaks centered on integer class codes
		at <- c(vals - 0.5, max(vals) + 0.5)

		colorkey <- list(
			at = at,
			labels = list(
				at = vals, 
				labels = labs
			)
		)
	}

	# print plot
	print(
		rasterVis::levelplot(
			ras,
			main = title,
			margin = margins,
			col.regions = cols,
			colorkey = colorkey
		)
	)
}
