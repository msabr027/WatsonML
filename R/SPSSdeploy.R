# DATALYTICS SOLUTIONS D'AFFAIRES


realtime_score <- function(tablename,header,datajust,contextid,urlscore,accesskey) {

  posturl=paste0(urlscore,contextid,"?accesskey=",accesskey)

  data<- NULL
  for(i in 1:dim(datajust)[1]){
    data[i]<- list(as.vector(as.character(datajust[i,]),mode = "list"))
  }


  body=list("tablename"=tablename,"header"=header,"data"=data)
  body= toJSON(body)
  request = POST(posturl, body = body, accept_json(),add_headers("Content-Type" = "application/json"),verbose())

  r= content(request)


  if(status_code(request)==200 & is.null(r$flag) ){
    a  <- matrix(data=NA , nrow = dim(matrix(r[[1]]$data))[1] , ncol = dim(matrix(r[[1]]$header))[1])
    for(i in 1:dim(matrix(r[[1]]$data))[1]){
      a[i,]<-rbind(sapply(r[[1]]$data[[i]], "[[", 1,simplify = TRUE))
    }
    a = data.frame(a,row.names = NULL,stringsAsFactors = FALSE )

    names(a) = NULL
    for(i in 1:dim(matrix(r[[1]]$header))[1]){
      names(a)[i]<- r[[1]]$header[[i]]
    }
    a
  }else if (status_code(request)==200 & (r$flag== FALSE)) {
    substr(r$message, 1, 100)
  }else if (status_code(request)==500) {
    paste0("Please verify you data format" )
  }else {
  paste0("make sure that you entered the right parameters:",contextid,tablename )
  }
}

########################################################################
modelinfo <- function(contextid,urlmeta,accesskey) {


geturl=paste0(urlmeta,contextid,"?accesskey=",accesskey)


request = GET(geturl, accept_json(),add_headers("Content-Type" = "application/json"),verbose())

r= content(request)

if(status_code(request)==200 & (r$flag!= FALSE)){
b =xmlToList(r$message)


field<- NULL
for(i in 1:(nrow(matrix(b$table))-1)){
  field[i]<- b$table[i]$field[[1]]
}

field = data.frame(field, stringsAsFactors=FALSE)

type<- NULL
for(i in 1:(nrow(matrix(b$table))-1)){
  type[i]<- b$table[i]$field[[3]]
}

type = data.frame(type, stringsAsFactors=FALSE)

datacategory = cbind(field,type)

return(datacategory)

}else if  (status_code(request)==200 & (r$flag== FALSE)) {

substr(r$message, 1, 100)

}else {
  paste0("make sure that you entered the right parameters:",contextid,accesskey)
}
}
####################################################################

validnames <- function(contextid,urlmeta,accesskey,datajust) {

  geturl=paste0(urlmeta,contextid,"?accesskey=",accesskey)


  request = GET(geturl, accept_json(),add_headers("Content-Type" = "application/json"),verbose())

  r= content(request)

  if(status_code(request)==200 & (r$flag!= FALSE)){
    b =xmlToList(r$message)


    field<- NULL
    for(i in 1:(nrow(matrix(b$table))-1)){
      field[i]<- b$table[i]$field[[1]]
    }

    field = data.frame(field, stringsAsFactors=FALSE)

    type<- NULL
    for(i in 1:(nrow(matrix(b$table))-1)){
      type[i]<- b$table[i]$field[[3]]
    }

    type = data.frame(type, stringsAsFactors=FALSE)

    datacategory = cbind(field,type)


b= sapply(datajust, class)
c=data.frame(b,stringsAsFactors=FALSE)
names <- rownames(c)
rownames(c) <- NULL
datac <- cbind(names,c,stringsAsFactors=FALSE)
comparison <- compare(datacategory$field,datac$names, ignoreOrder = TRUE)
Model_Assigned_Names = comparison$tMpartial
Your_data_names = comparison$tCpartial

if (nrow(data.frame(Model_Assigned_Names,stringsAsFactors=FALSE))== nrow(data.frame(Your_data_names,stringsAsFactors=FALSE))){
compareresult=cbind(data.frame(Model_Assigned_Names,stringsAsFactors=FALSE), data.frame(Your_data_names,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
resultscompare= if(comparison$result==FALSE){
  paste0("Column names are not maching please make the necessary modifications")
}else if(comparison$result==TRUE){
  paste0("Column names are matching you can push the data for analysis ! ")
}
comparenames=NULL
comparenames$table=compareresult
comparenames$info=resultscompare
return(comparenames)
}else { paste0("your data doesn't have ",nrow(data.frame(Model_Assigned_Names,stringsAsFactors=FALSE)),"variables !" )}
  }else if  (status_code(request)==200 & (r$flag== FALSE)) {

    substr(r$message, 1, 100)

  }else {
    paste0("make sure that you entered the right parameters:",contextid,accesskey)
  }


}
