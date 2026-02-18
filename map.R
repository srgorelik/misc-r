# ------------------------------------------------------------------
# map()
# 
# Purpose:
#   Plot a matrix or raster object for quick visualization.
#   Works best for small spatial extents and/or coarse resolutions.
#
# Continuous mode:
#   Uses a gradient color palette.
#
# Discrete mode:
#   Set `cut = TRUE` and supply either:
#     - A number of breaks, or
#     - Explicit break values via `brks`.
#
# Args:
#   x           : matrix or raster object to plot
#   pal         : color palette code (letter or numeric selection)
#   cut         : use discrete color ramp (TRUE/FALSE)
#   brks        : number of breaks or explicit break vector
#   units       : string or expression for cell value units
#   rev.pal     : reverse color palette (TRUE/FALSE)
#   vals        : print cell values at grid cell centers (TRUE/FALSE)
#   axes        : draw axes with tick marks and labels (TRUE/FALSE)
#   na.col      : color for NoData cells
#   fg          : foreground color (text/lines)
#   bg          : background color
#   draw.cells  : draw borders around grid cells (TRUE/FALSE)
#   legend      : display legend (TRUE/FALSE)
#   ...         : additional arguments passed to rasterVis::levelplot()
#
# Returns:
#   None. Plots raster or matrix.
# 
# Examples:
#   # continuous mode
#   map(r)
#
#   # discrete mode with 5 breaks
#   map(r, cut = T, brks = 5)
#
#   # discrete mode with custom breaks
#   map(r, cut = T, brks = c(0, 10, 25, 50, 100))
#
# Notes:
#   See https://gist.github.com/srgorelik/feb2d257cfa5d62d17351e44d2213af1
#
# ------------------------------------------------------------------


map <- function(
	x,
	pal = 'j',
	cut = F,
	brks = 10,
	units = '',
	rev.pal = F,
	vals = F,
	axes = T,
	na.col = 'black',
	fg = 'black',
	bg = 'white',
	draw.cells = F,
	legend = T,
	...
) {
	for (pkg in c('rasterVis', 'grid', 'colorRamps', 'viridis', 'matlab', 'RColorBrewer')) {
		require(pkg, character.only = T, quietly = T, warn.conflicts = F)
	}
	col.pal <- switch(
		pal,
		j = jet.colors(256),
		v = viridis(256),
		m = magma(256),
		i = inferno(256),
		p = plasma(256),
		t = terrain.colors(256),
		r = rainbow(256),
		g = gray.colors(256),
		s = colorRampPalette(brewer.pal(11, 'Spectral'))(256),
		b = blue2red(256),
		l = matlab.like(256),
		1
	)
	if ((length(col.pal) == 1) | (is.null(col.pal))) {
		stop('incorrect color palette choice.', call. = F)
	}
	if (rev.pal) {
		col.pal <- rev(col.pal)
	}
	min <- floor(min(as.matrix(x), na.rm = T))
	max <- ceiling(max(as.matrix(x), na.rm = T))
	if (!is.logical(cut)) {
		stop('cut must be TRUE or FALSE.', call. = F)
	} else if (!cut) {
		# continuous
		brks <- seq(min, max, by = ((max - min) / 256))
		ck <- list()
	} else {
		# discrete
		is.scalar <- function(x) is.atomic(x) && length(x) == 1L
		if (is.scalar(brks)) {
			brks <- seq(min, max, by = ((max - min) / brks))
		}
		ck <- list(at = brks, labels = list(at = brks))
	}
	if (!legend) {
		ck <- F
	}
	if (is.matrix(x)) {
		x <- t(apply(x, 2, rev))
	}
	p <- levelplot(x, margin = F, ylab.right = units, col.regions = col.pal, at = brks, colorkey = ck, ...)
	p$par.settings$panel.background$col <- na.col
	p$par.settings$axis.line$col <- fg
	p$par.settings$axis.text$col <- fg
	p$par.settings$add.text$col <- fg
	p$par.settings$background$col <- bg
	p$par.settings$layout.heights$main <- 1.5
	p$par.settings$layout.heights$top.padding <- 2
	p$par.settings$layout.widths$axis.key.padding <- 1
	p$par.settings$layout.widths$ylab.right <- 2
	p$par.settings$par.ylab.text$col <- fg
	if (is.matrix(x)) {
		p$xlab <- 'column'
		p$ylab <- 'row'
	}

	if (!axes) {
		p$xlab <- p$ylab <- ''
	}
	p$main <- list(p$main, col = fg, font = 1)
	p$xlab <- list(p$xlab, col = fg)
	p$ylab <- list(p$ylab, col = fg)
	p$x.scales$draw <- axes
	p$y.scales$draw <- axes
	if (vals) {
		p$panel <- function(x, y, z, ..., subscripts = subscripts) {
			panel.levelplot(
				x, y, z, ..., subscripts = subscripts
			)
			panel.text(x = x[subscripts], y = y[subscripts], labels = round(z[subscripts], 1))
		}
	}
	if (draw.cells) {
		p <- p + layer(panel.grid(h = 1, v = 1, col.line = fg), data = list(fg = fg))
	}
	update(p, aspect = 1)
}
