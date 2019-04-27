install.packages("scholar")

library(scholar)

library (magrittr)


moher <- scholar::get_citation_history("QhVtC1wAAAAJ")


scholar::get_profile("QhVtC1wAAAAJ")


# Search a list of uottawa authors 

ottawa <- "https://scholar.google.ca/citations?view_op=view_org&org=5757600927927532557&hl=en&oi=io"

toronto <- "https://scholar.google.ca/citations?view_op=view_org&org=8515235176732148308&hl=en&oi=io"


page <- httr::GET(ottawa)



str(page)


page_content <- httr::content(page, "text")


write.table(x = page_content,
            col.names = F,
            row.names = F,
            quote =  F,
            file = "test_1.html")
)

library (rvest)

webpage <- xml2::read_html(ottawa)


#Using CSS selectors to scrap the rankings section
data_html <- html_nodes(webpage,'#gs_hdr_tsi')

#Converting the ranking data to text
data <- html_text(data_html)


#Loading both the required libraries
library(rvest)
library(V8)
#URL with js-rendered content to be scraped
link <- 'https://food.list.co.uk/place/22191-brewhemia-edinburgh/'
#Read the html page content and extract all javascript codes that are inside a list

link  %>% 
  html_nodes('li') %>% 
  html_nodes('script') %>% 
  html_text()
# Create a new v8 context
ct <- v8()
#parse the html content from the js output and print it as text
read_html(ct$eval(gsub('document.write','',link))) %>% 
  html_text()



