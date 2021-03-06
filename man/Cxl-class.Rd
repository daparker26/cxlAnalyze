% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_cxl.R
\docType{class}
\name{Cxl-class}
\alias{Cxl-class}
\alias{Cxl}
\title{Cxl class}
\value{
Cxl object
}
\description{
\code{Cxl} Creates a Cxl class from a CXL document
}
\section{Fields}{

\describe{
\item{\code{doc}}{xml_document. Class from xml2 package.}

\item{\code{width}}{numeric.}

\item{\code{height}}{numeric.}
}}

\section{Methods}{

\describe{
\item{\code{get_concepts()}}{Returns CXL concepts in a dataframe}

\item{\code{get_connections()}}{Returns CXL connections and phrases in a dataframe}

\item{\code{initialize(cxl_data)}}{Validate and initialize fields}
}}

