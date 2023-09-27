\name{nolock}
\alias{nolock}
\title{'Append WITH (NOLOCK) to SQL Queries'}
\usage{
nolock(query = NULL)
}
\description{
Automatically appends 'WITH (nolock)' to all tables in 'SQL' queries using
a utility function. This facilitates reading from databases in scenarios where
non-blocking reads are preferable, such as in high-transaction environments.
}
\value{
Returns the processed 'SQL' query as a character vector with 'WITH (nolock)'
added for each table in the query.
}
\arguments{
\item{query}{Character vector. SQL query to be processed. If NULL, a temporary SQL text document is opened for user input.}
}
\examples{

example_SQL <- "

WITH CTE AS (SELECT C.TABLE_NAME,
                    C.COLUMN_NAME,
                    T.TABLE_TYPE
             FROM INFORMATION_SCHEMA.COLUMNS AS C
                      JOIN INFORMATION_SCHEMA.TABLES T ON C.TABLE_NAME = T.TABLE_NAME)

SELECT *
FROM CTE;"

nolock(query = example_SQL)

\dontrun{
nolock()
}
}
