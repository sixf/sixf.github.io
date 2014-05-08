require(RCurl)
require(XML)

npaper=365
res = matrix(0,ncol=7,nrow=npaper,dimnames=list(1:npaper,c('Received','Accepted','Published','Public','Citation','Editor','Subject')))
res = data.frame(res)

#retrive numbers
no.f=function(test){
  test=as.character(test)
  ntest=nchar(test)
  testsplit=strsplit(test,NULL)[[1]]
  en.res=paste(testsplit[testsplit%in%c(0:9)],collapse="")
  en.res
}

##### function 'days.fn' calculating number of days
days.fn <- function(date) ##set the day format as "yyyy-mm-dd"
{
  bg.date=date[1]
  ed.date=date[2]
  bg.year<-substr(as.character(bg.date),1,4)
  bg.month<-substr(as.character(bg.date),6,7)
  bg.day<-substr(as.character(bg.date),9,10)
  ed.year<-substr(as.character(ed.date),1,4)
  ed.month<-substr(as.character(ed.date),6,7)
  ed.day<-substr(as.character(ed.date),9,10)
  bg.x <- paste(bg.year,bg.month,bg.day,sep="-")
  ed.x <- paste(ed.year,ed.month,ed.day,sep="-")
  bg.date <- as.POSIXlt(strptime(bg.x, "%Y-%m-%d"))
  ed.date <- as.POSIXlt(strptime(ed.x, "%Y-%m-%d"))
  diff.date <- ed.date-bg.date
  format.day <- format(diff.date) #result format as "365 days"
  diff.day <- substr(format.day,1,nchar(format.day)-5)
  diff.day <- as.numeric(diff.day)+1 #add the current day
  date.series <- seq(bg.date,ed.date,by="days")
  format.date.series <- format(date.series,"%Y%m%d")
  return(diff.day)
}

for (i in 1:npaper) {
    urlPJ=paste('https://peerj.com/articles/',i,'/',sep="")
    docPJ = getURL(urlPJ)#,timeout = 130)
    treePJ = htmlTreeParse(docPJ)
    
    body = treePJ$chi[['html']][['body']]
    content=xmlValue(body[[5]][[3]][[1]])
    
    date.bg = nchar(strsplit(content,"Published20")[[1]][1])+1 #gregexpr("Published20",content)[[1]]
    date.ed = nchar(strsplit(content,"Academic Editor")[[1]][1])
    date = substr(content,date.bg,date.ed)
    Rdate=substr(date,46,55)
    Adate=substr(date,28,37)
    Pdate=substr(date,10,19)
    
    editor.bg = nchar(strsplit(content,"Academic Editor")[[1]][1])+1+nchar("Academic Editor")
    editor.ed = nchar(strsplit(content,"Subject Area")[[1]][1])
    editor = substr(content,editor.bg,editor.ed)
    
    subject.bg = nchar(strsplit(content,"Subject Areas")[[1]][1])+1+nchar("Subject Areas")
    subject.ed = nchar(strsplit(content,"Keywords")[[1]][1])
    subject = substr(content,subject.bg,subject.ed)
    
    citation.bg=nchar(strsplit(content,"Articles citing this paper")[[1]][1])+nchar('Articles citing this paper')
    citation = no.f(substr(content,citation.bg+1,citation.bg+4))
    if (citation=="") citation=0
    
    public.yn = strsplit(content,"makethe review history of this articlepublic.")
    public = summary(public.yn[[1]])[1]==2 ## if review history was set as public, return TRUE
    
    res[i,"Received"]=Rdate
    res[i,"Accepted"]=Adate
    res[i,"Published"]=Pdate
    res[i,"Editor"]=editor
    res[i,"Public"]=public
    res[i,"Citation"]=citation
    res[i,"Subject"]=subject
    write.table(res,"res.txt")
    print(paste(i,format(Sys.time(), "%H:%M:%S")))
}

## this calculation depends on the hardware of your computer and the condition of your Internet.
## Normally, it will need 20-30 minutes to run this loop.
## Please check this algorithm periodically to ensure it is running,
## because it will be interrupted due to timeout when the Internet is poor.
## By Xingfeng Si. May 08, 2014