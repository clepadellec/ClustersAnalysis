% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Univariate_Class.R
\name{u_ttest_all}
\alias{u_ttest_all}
\title{Mean's comparison using student test}
\usage{
u_ttest_all(object)
}
\arguments{
\item{object}{your Univariate object}
}
\value{
a table with the combination of each group X explanatory variable. For each combination, you can see if the fact to belong to a group as an influence on the explonatories variables (based on student test, means comparisons)
}
\description{
Mean's comparison using student test
}
\examples{
u_ttest_all(Univariate_object(esoph,1))
}
