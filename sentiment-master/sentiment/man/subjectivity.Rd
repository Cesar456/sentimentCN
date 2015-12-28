\name{subjectivity}
\alias{subjectivity}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a dataset containing a list of positive and negative subjective words.
}
\description{
A dataset containing  a list of positive and negative subjective words parsed from Janyce Wiebe's subjectivity lexicon.
}
\usage{
data(subjectivity)
}
\format{
A \code{data.frame} containing three columns.

1. \code{word} - A word from Janyce Wiebe's subjectivity lexicon.

2. \code{subjectivity} - A string indicating whether the word is strongly or weakly subjective.

3. \code{polarity} - A string indicating whether the word expresses a positive or negative sentiment.
}
\source{
Riloff and Wiebe (2003). Learning extraction patterns for subjective
expressions. EMNLP-2003. \url{http://www.cs.pitt.edu/mpqa/#subj_lexicon}
}
\examples{
# READ THE CSV
data <- read.csv(system.file("data/subjectivity.csv.gz",package="sentiment"))
# ALTERNATIVELY, USE THE data() FUNCTION
data(subjectivity)
}
\keyword{datasets}

