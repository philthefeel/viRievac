


# Correlogram function ----------------------------------------------------

#' @data - dataset
#' @method - 'spearman'
#' @lower.col - dotpplot color
#' @signif.col - singificance stars color
#' @sig.level - significance level. default:0.05
#' @main - Chart title
#' @save - Save the output
#' @path.output - path
#' @filename - pdf file name
#' @date - include the current date in the filename?
#' @regression.table - logical. Save regression.table in an excel (only when save is TRUE)


correlogram = function(data,
                       method='spearman',
                       sig.level=0.05,
                       signif.col = '#ffbb00',
                       upper.col=grDevices::colorRamp(RColorBrewer::brewer.pal(11, "Spectral")),
                       lower.col=adjustcolor(1,.4),
                       regression.line=TRUE,
                       regression.line.col='#ba5536',
                       main='',
                       save = FALSE,
                       path.output='./',
                       filename='correlogram',
                       date=TRUE,
                       height=12,
                       width=12,
                       regression.table=TRUE,...){

  myColorRampFunc = upper.col

  panel.cor <- function(w, z,digits=2,prefix='',...) {
    correlation <- cor(w, z, method = method)

    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    ## because the func needs [0,1] and cor gives [-1,1], we need to
    ## shift and scale it
    col <- rgb(myColorRampFunc((1+correlation)/2)/255)

    ## square it to avoid visual bias due to "area vs diameter"
    radius <- sqrt(abs(correlation))
    radians <- seq(0, 2*pi, len=50)     # 50 is arbitrary
    x <- radius * cos(radians)
    y <- radius * sin(radians)
    ## make them full loops
    x <- c(x, tail(x,n=1))
    y <- c(y, tail(y,n=1))


    txt <- format(c(correlation, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    cx <- 0.6/strwidth(txt)

    test <- cor.test(w,z,method=method)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                     cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                     symbols = c("***", "**", "*", " "))


    ## I trick the "don't create a new plot" thing by following the
    ## advice here: http://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
    ## This allows
    par(new=TRUE)
    plot(0, type='n', xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, asp=1)
    polygon(x, y, border=col, col=col)
    text(0, 0, txt, cex = cx * abs(correlation),family='sans')
    text(-.65, -.9, Signif, cex=cx, col=signif.col)
    text(.65, -.9, round(test$p.value,4), cex=1, col=signif.col)

  }

  reg <- function(x, y, line.col = regression.line.col,...) {
    points(x,y,...)
    abline(lm(y~x),col=line.col)
  }

  pairwise_simpleLM <- function (dat) {
    ## matrix and its dimension (n: numbeta.ser of data; p: numbeta.ser of variables)
    dat <- as.matrix(dat)
    n <- nrow(dat)
    p <- ncol(dat)
    ## variable summary: mean, (unscaled) covariance and (unscaled) variance
    m <- colMeans(dat)
    V <- crossprod(dat) - tcrossprod(m * sqrt(n))
    d <- diag(V)
    ## R-squared (explained variance) and its complement
    R2 <- (V ^ 2) * tcrossprod(1 / d)
    R2_complement <- 1 - R2
    R2_complement[seq.int(from = 1, by = p + 1, length = p)] <- 0
    ## slope and intercept
    beta <- V * rep(1 / d, each = p)
    alpha <- m - beta * rep(m, each = p)
    ## residual sum of squares and standard error
    RSS <- R2_complement * d
    sig <- sqrt(RSS * (1 / (n - 2)))
    ## statistics for slope
    beta.se <- sig * rep(1 / sqrt(d), each = p)
    beta.tv <- beta / beta.se
    beta.pv <- 2 * pt(abs(beta.tv), n - 2, lower.tail = FALSE)
    ## F-statistic and p-value
    F.fv <- (n - 2) * R2 / R2_complement
    F.pv <- pf(F.fv, 1, n - 2, lower.tail = FALSE)
    ## export
    data.frame(LHS = rep(colnames(dat), times = p),
               RHS = rep(colnames(dat), each = p),
               alpha = c(alpha),
               beta = c(beta),
               beta.se = c(beta.se),
               beta.tv = c(beta.tv),
               beta.pv = c(beta.pv),
               sig = c(sig),
               R2 = c(R2),
               F.fv = c(F.fv),
               F.pv = c(F.pv),
               stringsAsFactors = FALSE)
  }

  pToSign = function(x){

    ELSE=TRUE
    case_when(
      (x>0.05) ~ 'ns',
      (x<0.0001) ~ '****',
      (x<0.001) ~ '***',
      (x<0.01)~ '**',
      ELSE ~ '*'
    )

  }


  if(save){
    if(date) filename = paste(filename,Sys.Date(),sep = '_')
    fn = paste0(path.output,filename,'.pdf')
    pdf(fn,height=height,width = width)
    pairs(data,
          upper.panel = panel.cor,
          lower.panel = if(regression.line) reg else points,
          pch=19,
          cex=1.5,
          col=lower.col,
          main=main, mar=c(1,1,1,1),...)
    dev.off()

    if(regression.table){
      pw = pairwise_simpleLM(data)
      pw = pw[!duplicated(pw$F.pv),] %>% filter(LHS!=RHS) %>%
        mutate(Pval.sign = pToSign(F.pv),
               rho = sqrt(R2)) %>%
        select(yvar=LHS,xvar=RHS,alpha,beta,rho,R2,Pval=F.pv,Pval.sign)

      uniexport(pw,'excel',path.output,filename,row.names=F)
    }

  } else{

    pairs(data,
          upper.panel = panel.cor,
          lower.panel = if(regression.line) reg else points,
          pch=19,
          cex=1.5,
          col=lower.col,
          main=main, mar=c(1,1,1,1),...)

  }

}





