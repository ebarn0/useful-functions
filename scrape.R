#Scrape data from NHS Digital
scrape_data = function(pageurl, filetype = "csv", filter_phrase = "Provider", date_format_type = 0, skip = 0, sheet = 1) {
    library(rvest)
    require(readxl)
    require(tidyverse)
    require(httr)
  library(lubridate)
    
    #Extracts URLs from a page
    page = read_html(pageurl)
    allurls =page %>%
      html_nodes('a') %>%
      html_attr('href')
    
    #Filter URLs based on file ending and a key phrase
    dataurls = allurls[endsWith(as.character(allurls), filetype)]
    final_urls = na.omit(dataurls[str_detect(dataurls, filter_phrase)])
    
    #Adds on https
    if(!startsWith(final_urls[1],'http')) {
      final_urls = paste("https://digital.nhs.uk/", final_urls, sep = "")
      
    }
    #Extracts data
    df = data.frame()
    for(i in 1:length(final_urls)){
      
      
      if(filetype == "csv"){
        temp = read.csv(final_urls[i], skip = skip)
      }
      if(filetype != "csv"){
        GET(final_urls[i], write_disk(tf <- tempfile(fileext = paste(".", filetype, sep ="")))) 
        temp <- read_excel(tf,  skip = skip, sheet = sheet)
      }
      
      
      #Add in dates dependent upon the format type
      #Date format type 1 is "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Incomplete-Provider-Apr19-revised-XLS-8362K.xls"
      #Date format type 2 is  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/06/May-2020-CSV-bm1js-1.csv"
      
      if(date_format_type==1){
        temp = temp %>%
          mutate(ADate = dmy(paste("01-", gsub('^(.{3})(.*)$', '\\1-\\2',str_split(final_urls[i], "-")[[1]][4] ) , sep = "")))
      }
      if(date_format_type == 2){
        temp = temp %>%
          mutate(ADate = dmy(paste('01', str_split(str_split(final_urls[i], "/")[[1]][11], "-")[[1]][1], str_split(str_split(final_urls[i], "/")[[1]][11], "-")[[1]][2])) )
        
      }
      else{
        temp = temp%>%
          mutate(url = final_urls[i])
      }
        
      df =bind_rows(df, temp)
    }
    return(df)

}

