\name{modelinfo}
\alias{modelinfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get information related to your stored SPSS stream
}
\description{
 This function will give you information about your stored model data input Labels and names.

 }
\usage{
modelinfo(contextid, urlmeta, accesskey)
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
}
\details{
Before using this package you will have to create an account on BLUEMIX and to store your SPSS Stream on Watson Machine Learning service. The package uses API CALL to provide scores from models.
}

\author{
DATALYTICS SOLUTIONS D'AFFAIRES
}


\keyword{ info }% use one of  RShowDoc("KEYWORDS")
\keyword{ model }% __ONLY ONE__ keyword per line
