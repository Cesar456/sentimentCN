\name{emotions}
\alias{emotions}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a dataset containing words categorized into six emotion categories.
}
\description{
A dataset containing approximately 1500 words classified into six emotion categories: anger, disgust, fear, joy, sadness, and surprise.
}
\usage{
data(emotions)
}
\format{
A \code{data.frame} containing two columns.

1. \code{word} - A word from the WordNet database.

2. \code{emotion} - The emotion it is classified under (e.g. anger, disgust, fear, joy, sadness, surprise).
}
\source{
Carlo Strapparava and Alessandro Valitutti, "WordNet-Affect: an affective
extension of WordNet". In Proceedings of the 4th International Conference
on Language Resources and Evaluation (LREC 2004), Lisbon, May 2004, pp.
1083-1086. \url{http://www.cse.unt.edu/~rada/affectivetext/}
}
\examples{
# READ THE CSV
data <- read.csv(system.file("data/emotions.csv.gz",package="sentiment"))
# ALTERNATIVELY, USE THE data() FUNCTION
data(emotions)
}
\keyword{datasets}

