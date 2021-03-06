% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/u.spells.R
\name{u.spells}
\alias{u.spells}
\title{DREAM unemployment spell encoding}
\usage{
u.spells(data, row, period = NULL, other.exit = c(651, 652),
  other.censor = c(997, 998, 999), emp.weeks = 4)
}
\arguments{
\item{data}{A \code{data.frame} (or \code{data.table}) containing a DREAM dataset.}

\item{row}{An integer valued row number (1 to \code{nrow}(data)).}

\item{period}{A character vector of DREAM weeks, e.g. \code{c("y_1701","y_1702","y_1703",...)}. If \code{NULL} then all DREAM benefit weeks in \code{data} is used. The default is \code{NULL}.}

\item{other.exit}{An integer vector of DREAM benefit codes that identifies exit other than to employment. If \code{NULL} then no other exit. The default is DREAM benefit codes \code{c(651,652)}.}

\item{other.censor}{An integer vector of DREAM benefit codes that identifies right censoring other than censoring of observations. If \code{NULL} then no other censoring. The default is DREAM benefit codes \code{c(997,998,999)}.}

\item{emp.weeks}{An integer that defines the number of consecutive weeks without benefit and with an income the corresponding months needed for employment. The default is \code{4}.}
}
\value{
A list with components:\cr\cr
\code{lengths}: An integer vector containing the length of each run.\cr\cr
\code{start}: A character vector of same length as \code{lengths} with start weeks. \code{NA} imply that the spell start prior to first week in \code{period}.\cr\cr
\code{end}: A character vector of same length as \code{lengths} with end weeks. \code{NA} imply that the spell end after last week in \code{period}.\cr\cr
\code{right.censored}: A logical vector (\code{TRUE/FALSE}) of same length as \code{lengths} which indicates whether the spell is right censored.
}
\description{
Computes the length, start, and end, of unemployment spells in a DREAM dataset.
Unemployment is everything that is not employment. Employment is (by default) defined as 4 consecutive weeks without benefit and with an income the corresponding months.
}
