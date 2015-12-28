\name{classify_emotion}
\alias{classify_emotion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
classifies the emotion (e.g. anger, disgust, fear, joy, sadness, surprise) of a set of texts.
}
\description{
Classifies the emotion (e.g. anger, disgust, fear, joy, sadness, surprise) of a set of texts using a naive Bayes classifier trained on Carlo Strapparava and Alessandro Valitutti's \code{\link{emotions}} lexicon.
}
\usage{
classify_emotion(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{textColumns}{
A \code{data.frame} of text documents listed one per row.
}
  \item{algorithm}{
A \code{string} indicating whether to use the naive \code{bayes} algorithm or a simple \code{voter} algorithm.
}
  \item{prior}{
a \code{numeric} specifying the prior probability to use for the naive Bayes classifier.
}
  \item{verbose}{
A \code{logical} specifying whether to print detailed output regarding the classification process.
}
  \item{\dots}{
Additional parameters to be passed into the \code{\link{create_matrix}} function.
}
}
\value{
Returns an object of class \code{data.frame} with seven columns and one row for each document.

\item{anger}{The absolute log likelihood of the document expressing an angry sentiment.}
\item{disgust}{The absolute log likelihood of the document expressing a disgusted sentiment.}
\item{fear}{The absolute log likelihood of the document expressing a fearful sentiment.}
\item{joy}{The absolute log likelihood of the document expressing a joyous sentiment.}
\item{sadness}{The absolute log likelihood of the document expressing a sad sentiment.}
\item{surprise}{The absolute log likelihood of the document expressing a surprised sentiment.}
\item{best_fit}{The most likely sentiment category (e.g. anger, disgust, fear, joy, sadness, surprise) for the given text.}
}
\author{
Timothy P. Jurka <tpjurka@ucdavis.edu>
}
\examples{
# LOAD LIBRARY
library(sentiment)

# DEFINE DOCUMENTS
documents <- c("I am very happy, excited, and optimistic.",
				"I am very scared, annoyed, and irritated.",
				"Iraq's political crisis entered its second week one step closer to the potential 
				dissolution of the government, with a call for elections by a vital coalition partner 
				and a suicide attack that extended the spate of violence that has followed the withdrawal 
				of U.S. troops.")

# CLASSIFY EMOTIONS
classify_emotion(documents,algorithm="bayes",verbose=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}