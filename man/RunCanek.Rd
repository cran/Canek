% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RunCanek.R
\name{RunCanek}
\alias{RunCanek}
\alias{RunCanek.Seurat}
\alias{RunCanek.SingleCellExperiment}
\alias{RunCanek.list}
\title{RunCanek}
\usage{
RunCanek(x, ...)

\method{RunCanek}{Seurat}(
  x,
  batches = NULL,
  slot = "data",
  assay = "RNA",
  features = NULL,
  selection.method = "vst",
  fvf.nfeatures = 2000,
  debug = FALSE,
  ...
)

\method{RunCanek}{SingleCellExperiment}(x, batches = NULL, assay = "counts", debug = FALSE, ...)

\method{RunCanek}{list}(x, ...)
}
\arguments{
\item{x}{object with expression counts or list of matrices.}

\item{...}{additional arguments passed down to methods.}

\item{batches}{for S4 objects the column containing batch information.}

\item{slot}{slot used for Seurat objects (default: data).}

\item{assay}{assay used for Seurat objects (default: RNA).}

\item{features}{optional vector of features to use for correction.}

\item{selection.method}{method used for FindVariableFeatures on Seurat objects when features is NULL.}

\item{fvf.nfeatures}{function used to collapse variable features from different batches. Default is intersect.}

\item{debug}{whether to store information about correction vector.}
}
\value{
An object of the appropriate type.
}
\description{
Runs Canek integration.
}