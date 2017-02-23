\name{validnames}
\alias{validnames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find out if your data input has the right names for scoring the model.(NB: without this function, if you submit data with wrong names you will not obtain results)
}
\description{
This function let you know if your data input has the sames variable names than the one stored in your SPSS Stream in Watson Machine learning}
\usage{
validnames(contextid, urlmeta, accesskey, datajust)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{contextid}{
Context ID you defined on BLUEMIX (go to your bluemix account to find it)
}
  \item{urlmeta}{
the url defined in your Watson Machine Learning service credential on BLUEMIX (it could be as the following : https://ibm-watson-ml.mybluemix.net/pm/v1/metadata/)
}
  \item{accesskey}{
the unique access key for your Watson Machine Learning service credential on BLUEMIX
}
  \item{datajust}{
the data input to score.
}
}
\details{
Before using this package you will have to create an account on BLUEMIX and to store your SPSS Stream on Watson Machine Learning service. The package uses API CALL to provide scores from models
}

\author{
DATALYTICS SOLUTIONS D'AFFAIRES
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
