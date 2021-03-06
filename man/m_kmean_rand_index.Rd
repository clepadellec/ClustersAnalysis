% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Multivariate_Class.R
\name{m_kmean_rand_index}
\alias{m_kmean_rand_index}
\title{Compare two partitions, real classes vs kmeans classes}
\usage{
m_kmean_rand_index(object, rescale = FALSE)
}
\arguments{
\item{object}{your Multivariate object}
}
\value{
rand index between the result of kmean and y
}
\description{
Compare two partitions, real classes vs kmeans classes
}
\examples{
m_kmean_rand_index(multivariate_object(infert,1))

}
