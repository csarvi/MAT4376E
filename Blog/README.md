## Scraping bibliometric data using R

This blog article is created to illustrate how to scrape data from html and Javascript rendered web pages using R and RStudio. I'm assuming that you have a basic knowledge of html and Javascript rendered websites. If not, I strongly recommend reading before these two articles : [Introduction to web scraping](https://data-lessons.github.io/library-webscraping-DEPRECATED/02-csssel/) and [Scraping a JS-Rendered Page](`https://stanford.edu/~mgorkove/cgi-bin/rpython_tutorials/Scraping_a_Webpage_Rendered_by_Javascript_Using_Python.php).

### Terms of use

It is important to understand that while the information you find in most websites is available for the general public, it doesn't mean you can crawl data from any web page to use it for research, educational or business purposes. Too many requests per second made by the program created to extract information can generate a heavy load on the servers hosting the websites and make them vulnerable to cyber attacks.  In addition, there could also be an interest in some business to extract personal data for-profit purposes. Organizations have taken steps to minimize the use of these techniques by adding features like double authentication. Before scrapping data from any website, you should always be aware of the terms of use and respect basic scraping "rules" like the number requests per second. In the blog post [¨Web Scraping and Crawling Are Perfectly Legal, Right?¨](https://benbernardblog.com/web-scraping-and-crawling-are-perfectly-legal-right), Benoit Bernard talks about more about the possible consequences of web scraping and crawling.  

For this blog post, I will explore how to extract data from Wikipedia and Google scholar. The information to extract is related to bibliometric data of [the group of 15 Canadian Research Universities (U15).](https://en.wikipedia.org/wiki/U15_Group_of_Canadian_Research_Universities) and their top 100 researches by number of publications.

The main packages to extract this information are `rvest` and `tidyverse` developed by [Hadley Wickham](https://cran.r-project.org/web/checks/check_results_hadley_at_rstudio.com.html]).  The package `splahsr`. The package `scholar` will be used to show a co-citation analysis and main areas of research of the authors by university.

### Data scraping: HTML websites

We start by loading the packages.
``` r 
library(rvest)
library(xml2)
library(httr)
library(tidyverse)
library(magrittr)
library(splashr)
library(scholar )
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

There is some data wrangling to complete having a clean dataset that you can actually use for calculations, but this is the general idea of how to use the html elements on a web page with the `html_nodes`.


