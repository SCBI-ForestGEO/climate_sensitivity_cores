my.mdcplot <- function (x, sig, rescale = TRUE, main, ...) 
{
  if (!is.data.frame(x)) {
    x <- x$coef
  }
  
  blues <- colorRamp(c("#FFFFFF", "#395cd4"))
  reds <- colorRamp(c("#FFFFFF", "#dd291c"))
  
  m <- dim(x)[1]
  n <- dim(x)[2]

  if (rescale) {
    pos.max <- max(x)
    neg.max <- abs(min(x))
  }
  
  op <- par(no.readonly = TRUE)
  par(oma = c(1, 5, 5, 3), mai = c(1, 0.5, 0.2, 1)) #par(oma = c(0, 3, 5, 0), mai = c(0.5, 0.8, 0.2, 2))
  plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n", 
       yaxt = "n", ylab = "", xlab = "")
  axis(side = 3, at = 1:n, labels = colnames(x), las = 2) # change here
  axis(side = 2, at = 1:m, labels = rownames(x), las = 1)
  
  title(main, line = 3, outer = T)
  
  X.left <- X.right <- Y.bottom <- Y.top <- x
  
  X.left[] <- rep((1:n - 0.5), each = m)
  X.right[] <- rep((1:n + 0.5), each = m)
  Y.bottom[] <- rep(1:m - 0.5, n)
  Y.top[] <- rep(1:m + 0.5, n)
  
  
  x.left <- unlist(c(X.left))
  x.right <- unlist(c(X.right))
  y.bottom <- unlist(c(Y.bottom))
  y.top <- unlist(c(Y.top))
  
  xs <- unlist(c(x))
  xs.sig <- unlist(c(sig))
  
  color <- xs
  color[xs <= 0] <- rgb(reds(abs(xs[xs <= 0])/ neg.max), maxColorValue = 255)
  color[xs > 0] <- rgb(blues(xs[xs > 0]/ pos.max), maxColorValue = 255)
  
  rect(x.left, y.bottom , x.right, y.top, col = color, border = "white")
  
  points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig,  "white", "transparent"), col = ifelse(xs.sig, "black", "transparent"), pch = 21) 
  
  
  

  par(xpd = NA)
  leg.unit <- (m/15)
  start.unit <- leg.unit/4 + leg.unit
  right.pos <- n + n/5
  leg.width <- n/20
  values <- seq(-1, 1, length = 11)
  
  neg.rescaled.values <- round(seq(min(x), 0, length = 6), 
                               2)
  pos.rescaled.values <- rev(round(seq(max(x), 0, length = 6), 
                                   2)[-6])
  rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)
  for (i in 1:11) {
    if (values[i] <= 0) {
      polygon(c(right.pos, right.pos + leg.width, right.pos + 
                  leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                           leg.unit), start.unit + ((i - 1) * leg.unit), 
                                           start.unit + (i * leg.unit), start.unit + (i * 
                                                                                        leg.unit)), col = rgb(reds(abs(values[i])), 
                                                                                                              maxColorValue = 255), lty = 0)
      text(right.pos + leg.width , start.unit + (i * 
                                                      leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
                                                                                     values[i]), pos = 4)
    }
    else {
      polygon(c(right.pos, right.pos + leg.width, right.pos + 
                  leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                           leg.unit), start.unit + ((i - 1) * leg.unit), 
                                           start.unit + (i * leg.unit), start.unit + (i * 
                                                                                        leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255), 
              lty = 0)
      text(right.pos + leg.width, start.unit + (i * 
                                                      leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
                                                                                     values[i]), pos = 4)
    }
  }
  
  # par(op)
}