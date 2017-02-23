\name{realtime_score}
\alias{realtime_score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Score the data input in real time from your SPSS STREAM
}
\description{
The function let you score your data in real time based on the SPSS Modeler stream stored on Bluemix. The function uses Watson Machine Learning Bluemix service to score your data input.
}
\usage{
realtime_score(tablename, header, datajust, contextid, urlscore, accesskey)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tablename}{
The name of the node in SPSS that manage data input (refere to your SPSS node in your stream)
}
  \item{header}{
Variable names as it is named in your SPSS stream.
}
  \item{datajust}{
the data input to score.
}
  \item{contextid}{
Context ID you defined on BLUEMIX (go to your bluemix account to find it)
}
  \item{urlscore}{
the url defined in your Watson Machine Learning service credential on BLUEMIX (it could be as the following : https://ibm-watson-ml.mybluemix.net/pm/v1/score/)
}
  \item{accesskey}{
the unique access key for your Watson Machine Learning service credential on BLUEMIX
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
\keyword{ spss }% use one of  RShowDoc("KEYWORDS")
\keyword{ realtime }% __ONLY ONE__ keyword per line
