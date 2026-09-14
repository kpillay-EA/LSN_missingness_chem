### Plot functions to use with missing data eval
### Base R plots :)

### Create colour map 
# For use with sf plotting (e.g. the site level maps)

# x = the data (must not be factor)
# colmap = hcl.colors colour maps code
# rev = reverse the colours?
col_map <- function(x, colmap="YlOrRd", rev=TRUE) {
    r <- range(x)
    u <- length(unique(x))
    map <- data.table(value=0:r[2], cols=hcl.colors(u, colmap, rev=rev))
    # Join
    d <- data.table(value=x)
    dt_col <- map[d, on="value"]
    cols <- unlist(dt_col[, cols])
    return(cols)
}

### National level Plots

# Yearly change by month (percentage)
# x = yearly data (should be a vector)
# col = barplot colors
# title = title on barplot
# ... = passed to barplot
plot_miss_nat_yearly_month <- function(x, ylim=c(0, 100), col=colorRampPalette(c("blue2", "blue4"))(12), title="", ylab="Missing data (%)", ...) {
    barplot(x, col=col, ylim=ylim, ylab=ylab, main=title,, las=2, ...)
    axis(4, las=2)
    abline(h=mean(x), col="grey50", lwd=2)
}


# Heatmap (custom function)
# Regional data by month
# x = regional monthly data (should be matrix)
# lta = adjustment value for legend text
plot_miss_heatmap <- function(x, col=hcl.colors(100, "YlOrRd", rev=TRUE), zlim=c(0, 100), mar=c(4,6,4,6), xlabs, ylabs, title="", legend=TRUE, lta=0, las=0) {
    # Get rows/columns
    r <- nrow(x)
    c <- ncol(x)
    # set margins
    opar <- par("mar") # get original
    par(mar=mar) # set new
    # Create image
    image(1:c, 1:r, t(x), col=col, ylim=c(r + 0.5, 0.5), zlim=zlim, ann=FALSE, axes=FALSE)
    # Axes
    if(missing(xlabs)) {xlabs <- 1:c}
    if(missing(ylabs)) {ylabs <- 1:r}
    axis(1, 1:c, labels=xlabs, las=las)
    axis(2, 1:r, labels=ylabs, las=1)
    abline(v=1.5:(c + 0.5))
    abline(h=1.5:(r + 0.5))
    box()
    # title
    title(main=title)
    # Legend
    if(legend==TRUE) {
        rasterImage(as.raster(matrix(rev(col), ncol=1)), xleft=c+1, ybottom=r + 0.5, xright=c+1.5, ytop=0.5, xpd=TRUE)
        text(x=c+1.5, y=seq(r + 0.5, 0.5, l=6), labels="–", adj=0, xpd=TRUE)
        text(x=c+2+lta, y=seq(r + 0.5, 0.5, l=6), labels=seq(0, 100, by=20), xpd=TRUE)
        rect(xleft=c+1, ybottom=r + 0.5, xright=c+1.5, ytop=0.5, xpd=TRUE)
    }
    # revert par back to original
    par(mar=opar)
}