\name{classify_polarity}
\alias{classify_polarity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
classifies the polarity (e.g. positive or negative) of a set of texts.
}
\description{
Classifies the polarity (e.g. positive or negative) of a set of texts using a naive Bayes classifier trained on Janyce Wiebe's \code{\link{subjectivity}} lexicon.
}
\usage{
classify_polarity(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,
prior=1.0,verbose=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{textColumns}{
A \code{data.frame} of text documents listed one per row.
}
  \item{algorithm}{
A \code{string} indicating whether to use the naive \code{bayes} algorithm or a simple \code{voter} algorithm.
}
  \item{pstrong}{
A \code{numeric} specifying the probability that a strongly subjective term appears in the given text.
}
  \item{pweak}{
A \code{numeric} specifying the probability that a weakly subjective term appears in the given text.
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
Returns an object of class \code{data.frame} with four columns and one row for each document.

\item{pos}{The absolute log likelihood of the document expressing a positive sentiment.}
\item{neg}{The absolute log likelihood of the document expressing a negative sentiment.}
\item{pos/neg}{The ratio of absolute log likelihoods between positive and negative sentiment scores. A score of 1 indicates a neutral sentiment, less than 1 indicates a negative sentiment, and greater than 1 indicates a positive sentiment.}
\item{best_fit}{The most likely sentiment category (e.g. positive, negative, neutral) for the given text.}
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
				of U.S. troops.",
				"With nightfall approaching, Los Angeles authorities are urging residents to keep their
				outdoor lights on as police and fire officials try to catch the person or people responsible 
				for nearly 40 arson fires in the last three days.")
				
# CLASSIFY POLARITY
classify_polarity(documents,algorithm="bayes",verbose=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}