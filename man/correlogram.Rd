\name{correlogram}
\alias{correlogram}
\title{Correlogram }
\usage{
correlogram(data,method,upper.col,lower.col,signif.col,sig.level,
            main,save,height,width,path.output,filename,date,regression.table)
}
\arguments{

  \item{data}{Data set only with numerical variables and without missings.}
  \item{method}{a character string indicating which correlation coefficient is to be used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated.}
  \item{upper.col}{Upper circles color palette}
  \item{lower.col}{Lower dotplot color}
  \item{signif.col}{significance stars color}
  \item{sig.level}{significance level. default:0.05}
  \item{main}{Chart title}
  \item{save}{logical. Save the correlogram in pdf}
  \item{height}{pdf parameter}
  \item{width}{pdf parameter}
  \item{path.output}{path}
  \item{filename}{pdf file name}
  \item{date}{include the current date in the filename?}
  \item{regression.table}{logical. Save regression.table in an excel (only when save is TRUE)}

}
\value{
A correlogram plot.

When specified, a regression table with all possible pairwise linear regressions is created and saved in excel. The output has 7 columns: yvar, xvar, alpha, beta, R2, Pval and Pval.sign.
}
\description{
R function that creates a correlation matrix or correlogram with all the numerical variables included in the dataset provided. As an addition to the plot, the function also performs all possible pairwise linear regressions between variables giving the results in an excel table.
}
\examples{
data(iris)

correlogram(iris[,-5],method='pearson')



}
