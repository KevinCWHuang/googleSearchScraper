library(dplyr)
library(rvest)
library(data.table)

str_pair <- function(v_str, v_split, out = 'domain'){
  a = strsplit(x = v_str, split = v_split)
  l = length(a[[1]])
  if(out=='domain'){
    re = a[[1]][1]
  }else{
    re = a[[1]][l]
  }
  return (re)
}

scrape_term <- function(search_term,npages){
  base_url = "http://www.google.com/search?"
  search_string = paste0("q=",paste0(strsplit(search_term," ")[[1]],collapse="+"))
  dat = data.table( domain = as.character(), desc = as.character(),mark = as.character() )
  # names(dat)=c("domain","desc")
  for(i in 1:npages){
    if(i==1){
      url1 = paste0(base_url,search_string)
    }else{
      start_string = paste0("&start=",(i-1)*10)
      url1 = paste0(base_url,search_string,start_string)
    }
    
    # title <- read_html(url1) %>% html_nodes(css = "a h3") %>% html_text()
    link <- read_html(url1) %>% html_nodes(xpath = "//div/div/div/a/div[not(div)]") %>% html_text()
    link <- link[link != ">"]
    link <- link[link != ""]
    link <- link[link != "全部顯示"]
    link <- link[nzchar(link)]
    link <- link[grepl('http',link)]
    
    DT = data.table(link)
    DT[, link:= gsub("https://", "",link)]
    DT[, link := gsub(" › ", "/",link)]
    DT[, domain:= sapply(X = link,FUN = function(X) str_pair(X,'/','domain')) ]
    DT[, desc:= sapply(X = link,FUN = function(X) str_pair(X,'/','desc')) ]
    DT[, mark:= 'search']
    DT = unique(DT[,c('domain','desc','mark')])
    Sys.sleep(2)
    
    reg <- ".*\\.(.*\\.(com|es)).*"
    ad <- read_html(url1) %>% html_nodes(css = "a span") %>% html_text()
    ad <- ad[ad != ">"]
    ad <- ad[ad != ""]
    
    AD = data.table(ad)
    AD[, domain:= sapply(X = ad,FUN = function(X) str_pair(X,'/','domain')) ]
    AD[, desc:= sapply(X = ad,FUN = function(X) str_pair(X,'/','desc')) ]
    AD[, mark:= 'ad']
    AD = unique(AD[grep(reg, domain ),c('domain','desc','mark')])
    
    
    tmp = unique(rbind(DT,AD))
    dat = unique(rbind(dat, tmp))
 
    Sys.sleep(4)
  }
  return(dat)
}

terms = c('掃地機器人','氣炸鍋')
npages = 2

for(j in 1:length(terms)){
  result = scrape_term(terms[j],npages)
  write.csv(result,paste0("search_",terms[j],".csv"), row.names = F)
  Sys.sleep(3)
}

rm(list = ls())
gc()
