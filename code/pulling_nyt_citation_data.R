library(rvest)
library(tidyverse)


# build a list of the columnists' homepages
columnists <- c("https://www.nytimes.com/column/thomas-l-friedman",
                "https://www.nytimes.com/column/ross-douthat",
                "https://www.nytimes.com/column/gail-collins",
                "https://www.nytimes.com/column/bret-stephens", 
                "https://www.nytimes.com/column/paul-krugman", 
                "https://www.nytimes.com/column/david-brooks",
                "https://www.nytimes.com/column/maureen-dowd",
                "https://www.nytimes.com/column/charles-m-blow",
                "https://www.nytimes.com/column/roger-cohen",
                "https://www.nytimes.com/column/nicholas-kristof",
                "https://www.nytimes.com/column/frank-bruni")

# function to pull urls out of html page and return a tibble
linksPull <- function(base_url, node, attr) {

  read_html(base_url) %>%
    html_nodes(node) %>% 
    html_attr(attr) %>%
    # build tibble from the results
    tibble(nested_url = ., 
           base_url = base_url) %>%
    # drop any duplicates of the nested url
    distinct(nested_url, .keep_all = T) %>%
  return(.)

}

# map the function over the list of columnist pages and pull out the links in the header
# this will return a tibble of links to the 10 most recent articles for all columnists
articles <- map_df(columnists, node = "a.story-link", attr = "href", linksPull) %>%
  # rename the columns with descriptive titles
  rename(article_url = nested_url, author_url = base_url)


# map the function over the list of article urls to pull out links in the body of the text
links <- map_df(articles$article_url, node = "p.story-body-text.story-content a", attr = "href", linksPull) %>%
  # rename the columns with descriptive titles
  rename(outlink_url = nested_url, article_url = base_url)

# final cleaned data
data_fin <- links %>% 
  # join in the article
  left_join(., articles) %>% 
  # use regex to simplify down the the base url
  mutate(outlink = gsub(".*//|.*//www.|/.*", "", outlink_url), 
         # drop the first part of the url to get the author's name
         author = gsub("https://www.nytimes.com/column/", "", author_url))


