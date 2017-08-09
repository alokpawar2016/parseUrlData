#' @title
#' Get URL Data
#' @description
#' getData function accepts URL parameter, based on content Type it returns list or text formated
#' for content.type=="application/json"  = list data returned
#' getData function also cache the responce data to optimize the performance
#' only first time URL actualy requested, but after subsequent call URL reponce pulled from cache
#' @param url: web URL
#'
#' @return list (for json based object) or text
#' @examples
#'  q<-getData("http://echo.jsontest.com/fieldkey/fieldvalue/purpose/test")
#'  q<-getData("http://www.google.com")
#' @export
getData<-function(url){
  # check url in cache
  cache.data<-getCacheData(url)
  if(!is.null(cache.data))
  {
    print("Responce data from cache")
    return(cache.data)
  }
  # no cache data request the URL and store date in cache
  responce.data <-parseUrl(url)

  # check the URL success responce
  if (status_code(responce.data )!= 200){
    stop("URL not exist or not accessable")
  }
  content.header <- headers(responce.data)
  content_type <- getContentType(content.header)
  content.data <- getUrlData(content_type,responce.data)

  setCacheData(content.data,url)
  return(content.data)
}

parseUrl<-function(url){
  return(GET(url, add_headers(customheader ="CS")))
}

getUrlData<-function(content.type,responce.data){
  #create output data based on content Type
  if(content.type=="application/json")
  {
    content.data<-content(responce.data)
  }else{
    content.data<-content(responce.data , as="text")
  }
  return(content.data)
}

getContentType<-function(header)
{
  content.type<-strsplit(header$`content-type`,";")
  content.type<-content.type[[1]][1]
  return(content.type)
}

## Cacheing Data
getCacheData<-function(url){
  cache.object<-loadCache(list(url))
  return(cache.object)
}

setCacheData<-function(url,object){
  saveCache(object,list(url))
}
