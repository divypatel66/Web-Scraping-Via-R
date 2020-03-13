library(stringr)
library(rvest)
library(xml2)

webUrl = "https://www.jstage.jst.go.jp/browse/cbij/%s/%s/_contents/-char/en"
htmlTags = list(
  "article-list" = "search-resultslisting",
  "article-title" = "searchlist-title",
  "article-author" = "searchlist-authortags.customTooltip",
  "article-additional-info" = "searchlist-additional-info",
  "article-doi" = "searchlist-doi",
  "article-pdf" = "lft",
  "article-abstract" = "inner-content abstract",
  "article-keywords" = "global-tags"
)


getPdf <- function(url){
  # <TODO @Divya: parse the pdf from url, save it as .txt and return as well>
}


countSubLinks <- function(year){
  # <TODO @Jasneek: parse the extra urls for the years 2001-2008 and return the count>
  n = 1
  n
}


scrapePage <- function(url){
  # <TODO @Kajal: parse the url page for information>
  # Title, Authors, Author Affiliations, 
  # Correspondence Author, Correspondence Author's Email
  # Publish Date, Abstract, Keywords, Full Paper (Text format)
  page = read_html(url)
  
}

getJournal <- function(year){
  if(year > 2020 | year < 2001){
    stop("The articles for the given year does not exist for our Journal. Please provide year between 2001-2020")
  }
  
  yearUrls = c()
  
  if(year > 2008){
    yearUrls[length(yearUrls) + 1] = sprintf(webUrl, year%%100, 0)
  }
  else{
    extraUrlLength = countSubLinks(year)
    for( i in seq(extraUrlLength)){
      yearUrls[length(yearUrls) + 1] = sprintf(webUrl, year%%100, i)
    }
  }
  
  for( i in seq_along(yearUrls)){
    scrapePage(yearUrls[i])
  }
}
  
