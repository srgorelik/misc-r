quickmap <- function(
	tif.file,
	band = 1,
	palette = 'jet',
	reverse = F,
	margins = F
) {
	suppressPackageStartupMessages({
		library(terra)
		library(rasterVis)
	})

	# check that raster exists
	if (!file.exists(tif.file)) stop(sprintf('%s does not exist.', tif.file))

	# select band if multi-layer
	n.bands <- nlyr(rast(tif.file))
	if (n.bands < band) {
		stop(sprintf('Requested band %s, but raster has %s layer(s).', band, n.bands), call. = F)
	}

	# read raster
	ras <- rast(tif.file, lyrs = band)

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

	# title
	title <- if (n.bands > 1) sprintf('%s - Band %s', basename(f), band) else basename(tif.file)

	# plot raster
	print(
		levelplot(
			ras,
			margin = margins,
			col.regions = colorRampPalette(cols),
			main = title
		)
	)
}
