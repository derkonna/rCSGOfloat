\name{neededFloat}
\alias{neededFloat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average needed float calculation for trade-up contracts.
}
\description{
Calculate the average float of input skins needed to get the output skin in the desired condition.
A helpful resource for finding the float boundaries is https://csgostash.com/
}
\usage{
neededFloat(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{desiredCondition}{
    Character vector containing the desired condition of the output skin from the trade-up contract.

    Format:

    "FN" | "Factory New"

    "MW" | "Minimal Wear"

    "FT" | "Field-Tested"

    "WW" |  "Well-Worn"

    "BS" | "Battle-Scarred"
}
  \item{min}{
  The upper boundary of the float value of the skin you want to trade up to.
}
  \item{max}{
  The lower boundary of the float value of the skin you want to trade up to.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
neededFloat(desiredCondition = "MW",min = 0,max = 0.75)
 > Average Condition Needed:  0.192307692307692
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
