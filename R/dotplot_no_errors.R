#'
#' dotplot_no_errors
#'
#' build a vertical dotplot without error bars. Used for
#' weighted averages
#'
#' @param x dataframe
#' @param myTheme default values for pch and col (todo)
#' @param qlabel text for axis label
#' @param add.text.to.qlabel additional text for axis label
#' @param reorder.groups default is TRUE
#' @param reordering order of sorting when reorder.groups is TRUE
#' @param label.define (todo)
#' @param reference.line value for the reference line. default is null
#' @param bar.color default is 1
#' @param horizontal layout of the dotplot. default is TRUE
#' @param ... further arguments passed to or from other methods.
#' @author Rollie Parrish
#' @export

dotplot_no_errors <- function(x, myTheme = simpleTheme(pch = 19, col = 1),
                           qlabel = "Weighted average", add.text.to.qlabel = "",
                           reorder.groups = TRUE,
                           reordering = "decreasing", label.define = list(), reference.line = NULL,
                           bar.color = 1, horizontal = TRUE, ...)
{
  o <- c(which(colnames(x) == "group"), which(colnames(x) == "est"))
  if (length(o) != 2) stop("Error: Incorrect data frame")
  x <- x[, o]
  x$group <- factor(x$group)
  if (horizontal == T) hor <- 1 else hor <- -1
  if (reordering == "decreasing")
    FUN.to.reorder <- function(x) mean(x) * hor
  else FUN.to.reorder <- function(x) -mean(x) * hor
  if (reorder.groups) x$group <-
    with(x, reorder(group, est, FUN = FUN.to.reorder))

  xlab <- qlabel

  if (horizontal == T)
    p <- stripplot(group ~ est, data = x,
                   xlim = range(min(x$est), max(x$est))+c(-.10,.10)*range(min(x$est), max(x$est)),
                   xlab = c(list(xlab), label.define),
                   par.settings = myTheme,
                   panel = function(x, y, ..., subscripts) {
                     if (is.null(reference.line) == F)
                       panel.abline(v = reference.line, col = "grey")
                     panel.abline(h = y, col = "grey", lty = "dashed")
                     panel.stripplot(x, y, ...) }, ...)
    else
    p <- stripplot(est ~ group, data = x,
                   ylim = range(min(x$est), max(x$est))+c(-.04,.04)*range(min(x$est), max(x$est)),
                   ylab = c(list(xlab), label.define),
                   par.settings = myTheme,
                   panel = function(x, y, lower, upper, ..., subscripts) {
                    if (is.null(reference.line) == F)
                      panel.abline(h = reference.line, col = "grey")
                  panel.abline(v = x, col = "grey", lty = "dashed")
                  panel.stripplot(x, y, ...) }, ...)
  #print(p)
  invisible(p)
}
