% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttest.R
\name{ttApp}
\alias{ttApp}
\title{t-test and Paired t-test}
\usage{
ttApp()
}
\value{
Nothing
}
\description{
ttApp() function opens up an interactive shiny app which will allow
user to easily perform one sample t-test, unpaired two sample t-test,
unpaired two sample Welch t-test, paired t-test, test for homogeneity of variance (F-test),
and obtain plots like boxplot and paired plot by uploading CSV file.
}
\details{
This app uses \code{t.test} function to calculate t statistic.Descriptive statistics
were calculated using \code{stat.desc} function of \code{pastecs} package.
\code{var.test} function is used for F-test.\code{ggboxplot} function
of \code{ggpubr} package is used to draw boxplot. Paired plot is obtained
using \code{paired} function of package \code{PairedData}.
}
\examples{
if (interactive()) {
  ttApp()
}
}
\references{
\insertRef{R_2021}{grapesAgri1}

\insertRef{shiny_2021}{grapesAgri1}

\insertRef{sw_2021}{grapesAgri1}

\insertRef{dplyr_2021}{grapesAgri1}

\insertRef{ggpubr_2020}{grapesAgri1}

\insertRef{past_2018}{grapesAgri1}

\insertRef{gupta1985statistical}{grapesAgri1}

\insertRef{paired_data2018}{grapesAgri1}
}
\keyword{Testing}
\keyword{Welch}
\keyword{and}
\keyword{boxplot}
\keyword{homogeneity}
\keyword{of}
\keyword{one}
\keyword{paired}
\keyword{plot}
\keyword{sample}
\keyword{t-test}
\keyword{two}
\keyword{unpaired}
\keyword{variance}
