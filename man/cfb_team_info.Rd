% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_team_info.R
\name{cfb_team_info}
\alias{cfb_team_info}
\title{Team Info Lookup
Lists all teams in conference or all D-I teams if conference is left NULL
Current support only for D-I}
\source{
\url{https://api.collegefootballdata.com/teams}
}
\usage{
cfb_team_info(conference = NULL, only_fbs = TRUE, year = NULL)
}
\arguments{
\item{conference}{(\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
Conference abbreviations P5: ACC, B12, B1G, SEC, PAC,\cr
Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr}

\item{only_fbs}{(\emph{Logical} default TRUE): Filter for only returning FBS teams for a given year.\cr
If year is left blank while only_fbs is TRUE, then will return values for most current year}

\item{year}{(\emph{Integer} optional): Year, 4 digit format (\emph{YYYY}). Filter for getting a list of major division team for a given year}
}
\value{
A data frame with 12 variables:
\describe{
\item{\code{team_id}}{integer.}
\item{\code{school}}{character.}
\item{\code{mascot}}{character.}
\item{\code{abbreviation}}{character.}
\item{\code{alt_name1}}{character.}
\item{\code{alt_name2}}{character.}
\item{\code{alt_name3}}{character.}
\item{\code{conference}}{character.}
\item{\code{division}}{character.}
\item{\code{color}}{character.}
\item{\code{alt_color}}{character.}
\item{\code{logos}}{list.}
}
}
\description{
Team Info Lookup
Lists all teams in conference or all D-I teams if conference is left NULL
Current support only for D-I
}
\examples{

cfb_team_info(conference = "SEC")

cfb_team_info(conference = "Ind")

cfb_team_info(year = 2019)

}
\keyword{Teams}
