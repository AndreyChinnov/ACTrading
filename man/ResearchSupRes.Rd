\name{ResearchSupRes}
\alias{ResearchSupRes}
\title{ResearchSupRes}
\usage{
ResearchSupRes(OHLC_data, Delta)
OHLC_data - xts data in OHLC format
Delta > 0
}
\description{
The function goes with the step of $Delta$ analyses
}
\examples{
result<-ResearchSupRes(getSymbols("AAPL"), 0.001)
}
