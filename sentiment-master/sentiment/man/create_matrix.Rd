\name{create_matrix}
\alias{create_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
creates a document-term matrix.
}
\description{
Creates an object of class \code{DocumentTermMatrix} from \pkg{tm}.
}
\usage{
create_matrix(textColumns, language="english", minDocFreq=1, minWordLength=3, 
removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, 
stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{textColumns}{
Either character vector (e.g. data$Title) or a \code{cbind()} of columns to use for training the algorithms (e.g. \code{cbind(data$Title,data$Subject)}).
}
  \item{language}{
The language to be used for stemming the text data.
}
  \item{minDocFreq}{
The minimum number of times a word should appear in a document for it to be included in the matrix. See package \pkg{tm} for more details.
}
  \item{minWordLength}{
The minimum number of letters a word should contain to be included in the matrix. See package \pkg{tm} for more details.
}
  \item{removeNumbers}{
A \code{logical} parameter to specify whether to remove numbers.
}
  \item{removePunctuation}{
A \code{logical} parameter to specify whether to remove punctuation.
}
  \item{removeSparseTerms}{
See package \pkg{tm} for more details.
}
  \item{removeStopwords}{
A \code{logical} parameter to specify whether to remove stopwords using the language specified in language.
}
  \item{stemWords}{
A \code{logical} parameter to specify whether to stem words using the language specified in language.
}
  \item{stripWhitespace}{
A \code{logical} parameter to specify whether to strip whitespace.
}
  \item{toLower}{
A \code{logical} parameter to specify whether to make all text lowercase.
}
  \item{weighting}{
Either \code{weightTf} or \code{weightTfIdf}. See package \pkg{tm} for more details.
}
}
\author{
Timothy P. Jurka <tpjurka@ucdavis.edu>
}
\examples{
library(sentiment)

# DEFINE THE DOCUMENTS
documents <- c("I am very happy, excited, and optimistic.",
				"I am very scared, annoyed, and irritated.",
				"Iraq's political crisis entered its second week one step closer to the potential 
				dissolution of the government, with a call for elections by a vital coalition partner 
				and a suicide attack that extended the spate of violence that has followed the withdrawal 
				of U.S. troops.",
				"With nightfall approaching, Los Angeles authorities are urging residents to keep their
				outdoor lights on as police and fire officials try to catch the person or people responsible 
				for nearly 40 arson fires in the last three days.")

matrix <- create_matrix(documents, language="english", removeNumbers=TRUE, 
stemWords=FALSE, weighting=weightTfIdf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
