## Scraping bibliometric data using R

This blog article is created to describe how to scrape data from html web pages using R and RStudio. I'm assuming that you have a basic knowledge of html websites. If not, I strongly recommend reading before these two articles : [Introduction to web scraping](https://data-lessons.github.io/library-webscraping-DEPRECATED/02-csssel/) and [Scraping a JS-Rendered Page](`https://stanford.edu/~mgorkove/cgi-bin/rpython_tutorials/Scraping_a_Webpage_Rendered_by_Javascript_Using_Python.php).  

### Terms of use

It is important to understand that while the information you find in most websites is available for the general public, it doesn't mean you can crawl data from any web page to use it for research, educational or business purposes. Too many requests per second made by the program created to extract information can generate a heavy load on the servers hosting websites and make them vulnerable to cyber attacks.  In addition, there could also be an interest in some business to extract personal data for-profit purposes. Organizations have taken steps to minimize the use of these techniques by adding features like double authentication. Before scrapping data from any website, you should always be aware of the terms of use and respect basic scraping "rules" like the number requests per second. In the blog post ["Web Scraping and Crawling Are Perfectly Legal, Right?"](https://benbernardblog.com/web-scraping-and-crawling-are-perfectly-legal-right), Benoit Bernard talks about more about the possible consequences of web scraping and crawling.  

For this blog post, I will explore how to extract data from Wikipedia and Google scholar. The information to extract is related to bibliometric data of [the group of 15 Canadian Research Universities (U15).](https://en.wikipedia.org/wiki/U15_Group_of_Canadian_Research_Universities) and their top 10 researches by number of citations.

The main packages to extract this information are `rvest` and `tidyverse` developed by [Hadley Wickham](https://cran.r-project.org/web/checks/check_results_hadley_at_rstudio.com.html]). 

### Data scraping: HTML websites

We start by loading the packages.
```r 
library(rvest)
library(xml2)
library(httr)
library(tidyverse)
library(magrittr)
```

Firstly, we'll obtain a list of the U15 universities from Wikipedia. By looking at the source code you can see that most of the tables in a Wikipedia page, have the classification `<table class="wikitable sortable">` in their configuration. The first table is most of the time the info card that you can see at the top right side of the page. The function `wikiScrap` will let you select any table from a Wikipedia page and convert it to a data frame. 

``` r 
# Identify the website and use the package xml2 to extract the data
u15 <- xml2::read_html("https://en.wikipedia.org/wiki/U15_Group_of_Canadian_Research_Universities")

# Create a function to extract the data from Wikipedia websites
wikiScrap <- function (x, y) {
  i <- rvest::html_nodes(x, "table")
  i <- rvest::html_table(i, fill = TRUE) %>%
    .[y]
  i <- as.data.frame(i)
  return(i)
}
# Apply the function to get the content from the second table in the Wikipedia page
u15_df <- wikiScrap(u15, 2)
# Create a vector
u15 <-  u15_df [ , 1]
```

Now you can use the same function to extract data from another web page. Let's test the function with some of the results of the 2019 global rankings of Canadian universities.

``` r 
#test function with other Wikipedia page
rankingCanUniv <- xml2::read_html("https://en.wikipedia.org/wiki/Rankings_of_universities_in_Canada")

#Select the second table in the rank.
rank_univ_can  <- wikiScrap(rankingCanUniv, 2)
```

You still need to do some data wrangling to obtain a clean dataset that you can actually use for any purpose, but this is the general idea of how to use the html elements on a web page with the `html_nodes` function of the rvest package.

### Scraping lists of authors and citations by university (Top 10)

Using the information obtained from the Wikipedia page, I will create a data frame with a unique identifier by institutions, removing words like University and University.

``` r 
# Creating a normalized list of u15 universities
u15 <- u15 %>% 
  stringr::str_replace("University of", " ") %>% 
  stringr::str_replace("University", " ") %>% 
  stringr::str_replace("Université de", " ") %>% 
  stringr::str_replace("Université", " ") %>% 
  stringr::str_replace("Ontario", " ") %>% 
  stringr::str_trim()
```

Google scholar include the `id` of the institution at the end of the url. Next step is to manually search the list of institutions' ids and merge the data with the university name.  

``` r 
#Get the main link by institution
url_ins <- "https://scholar.google.com/citations?view_op=view_org&hl=en&org="

#Manually get the list of institutions ids in google scholar
ins_id <- c("16627554827500071773", "13655899619131983200", "2186568608501296974",
            "4396926741242628134", "4136666809584843007", "17983032966567625180", 
            "13784427342582529234", "16902803553507995100", "4964519586676348649",
            "5757600927927532557", "15288470216663349706", "7846091072518378427",
            "8515235176732148308", "12436753108180256887", "4065822778065209794")

#merge data to identify the id with the institution
u15tbl<- cbind(u15,ins_id)

````


The next step is to create a function to retrieve data from the nodes `.gs_ai_name`that contains the author and `.gs_ai_cby'` that has the numer of citations. 

``` r 
#Creating functions to extrac author and citations data
get_author <- function(x){
        x %>% 
        html_nodes('.gs_ai_name') %>%      
        html_text() %>% 
        str_trim() %>%                       
        unlist()    
}

get_citations <- function(x){
        x %>% 
        rvest::html_nodes('.gs_ai_cby') %>%      
        rvest::html_text() %>% 
        stringr::str_trim() %>%
        stringr::str_replace ("Cited by ", "")%>%
        as.numeric() %>% 
        unlist()    
}

#Create a master function 
get_data_table <- function (x) {
  
  i <- get_author(x)
  j <- get_citations(x)
 
  combined_data <- tibble (authors = i,
                          citations = j)
  
    }

#Testing the funtion with one URL 
html_uottawa <- xml2::read_html(paste0(url_ins,u15tbl[10,2]))
data <- get_data_table(html_uottawa)

```


The following lines of code will use the previous list of ids and the main link to read the html from each university and extract a list of 10 authors per university.

``` r
#create a function to read the html using the links of the institutions

u15ID <- u15tbl[,2] 

u15_data <- lapply(seq_along(u15ID), function(x) {
  i <- paste0(url_ins,u15ID[[x]])
  j  <- xml2::read_html(i)
  k <- get_data_table(j)
  print("html was read and data was saved in a list, waiting 5 seconds to read next html...")
  Sys.sleep(time = 5)
  print("Starting to read next html...")
  return(k)
  })


```

The final result is a list containing the 15 tables with the top 10 researches by number of citations in Google Scholar.

