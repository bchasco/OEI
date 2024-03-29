% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTable.r
\name{getTable}
\alias{getTable}
\title{Read in information from database using}
\usage{
getTable(
  uid = uid,
  pwd = pwd,
  db = "NWFSC_OCEAN",
  schemaName = c("crepo", "globec", "ncc", "predator", "prerecruit", "dbo"),
  tableName = NA,
  conditions = NULL,
  remove_dup = FALSE
)
}
\arguments{
\item{uid}{User ID}

\item{pwd}{Default password}

\item{db}{Database name}

\item{tableName}{The name of the schema and table of interest - "schema.table"}

\item{conditions}{use for the where clause in sql}

\item{remove_dup}{logical indicating whether the query should remove duplicate results}
}
\value{
table A table from the list of tables that have already been built.
}
\description{
getTable
}
