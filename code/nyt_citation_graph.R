library(tidyverse)
library(visNetwork)

### build network data ------------
raw_links <- read_csv("https://raw.githubusercontent.com/etachov/nyt_opinion_citations/master/data/links_data.csv")

# build the raw edges data.frame
edges <- raw_links %>%
  select(from = author, to = outlink) %>%
  # calculate the # of times each columnist cited the source
  group_by(from, to) %>%
  tally() %>%
  # rename n to value so that it works with visNetwork
  rename(value = n) %>% 
  # calculate the total # of times a source was cited
  group_by(to) %>%
  mutate(total = sum(value), 
         from = stringi::stri_trans_totitle(gsub("-", " ", from))) 

# calculate the number of outlinks for the columnists and inlinks for sources
# bind together into one data frame
num_links <- bind_rows(edges %>%
                         group_by(from) %>%
                         summarise(n = sum(value)) %>%
                         rename(id = from), 
                       edges %>%
                         group_by(to) %>%
                         summarise(n = sum(value)) %>%
                         rename(id = to))


# set colors 
citation_color <- "rgba(92, 75, 117, .9)" # flat purple
columnist_color <- "rgba(78, 148, 122, 1)" # flat green
citiation_color_half_alpha <- "rgba(92, 75, 117, .5)" # same purple, half alpha
edge_color <- "rgba(153, 153, 153, .4)"
white_half_alpha <- "rgba(255, 255, 255, .5)" 


# build the raw nodes data.frame
nodes <- data.frame(id = unique(c(edges$from, edges$to))) %>%
  # create styling columns for visNetwork using the colors above
  mutate(title = id,
         group = ifelse(id %in% unique(edges$from), "Columnist", "Citation")) %>%
  left_join(., num_links) %>%
  mutate(value = log2(n)^2,
         color.background = ifelse(group == "Citation", citation_color, columnist_color),
         color.highlight.background = ifelse(group == "Citation", citation_color, columnist_color),
         color.border = white_half_alpha,
         color.highlight.border = ifelse(group == "Citation", citation_color, columnist_color),
         shape = ifelse(group == "Columnist",  "square", "dot"), 
         font.size = ifelse(group == "Columnist", 60, ifelse(value < 30, 30, value)))



### build the graph ------
graph <- visNetwork(edges = edges, 
                    nodes = nodes, 
                    height = "800", 
                    width = "100%", 
                    main = list(text = "What Sites do NYT Columnists Cite?", 
                                style = "font-size:36px; font-family: monospace;text-align:center;"),
                    submain = list(text = "&#x25CF; Columnist   &#x25A0; Source   &#x2014; Citation<br>Click, drag, and zoom to adjust the graph. Data available <a href='https://github.com/etachov/nyt_opinion_citations' style='color:#000000' target='_blank'>here</a>.", 
                                   style = "font-size:20px;font-family: monospace;text-align:center;")) %>%
  visOptions(highlightNearest = list(enabled = TRUE, 
                                     degree = 1, 
                                     labelOnly = FALSE), 
             nodesIdSelection = list(enabled = TRUE,
                                     values = unique(c(sort(edges$from), sort(edges$to))),
                                     style = "background: #000000;color: #ffffff;width:200px;text-align:center;"))  %>%
  visEdges(color = list(color = edge_color, highlight = citiation_color_half_alpha)) %>%
  visPhysics(solver = "forceAtlas2Based", 
             stabilization = list(iterations = 80, fit = TRUE)) %>%
  visInteraction(tooltipStyle = NA) %>%
  visLayout(randomSeed = 86) 


visSave(graph, 
        file = "/Users/evan/Dropbox/Projects/2017_Q4_NYT/nyt_citation_graph.html",
        selfcontained = TRUE)


## analyzing which columnists links out the most
out_links <- edges %>%
  group_by(from) %>%
  summarise(num.source = n(), 
            num.links = sum(value))

num_links_sources <- ggplot(out_links, aes(x = num.source, y = num.links, label = from)) +
  geom_point(size = 2, color = "#4E947A") +
  geom_text_repel(family = "mono", box.padding = .4, size = 4.5) +
  theme_minimal(base_family = "mono", base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  xlim(c(0, 60)) +
  ylim(c(10, 100)) +
  labs(title = "How Often do NYT Columnists Link Out?",
       x = "# of sites linked to", 
       y = "Total # of links")

ggsave("2018-01-28-citation_frequency.png", num_links_sources, width = 7, height = 6, dpi = 500)

## determine who links to the most unique outlets
in_links <- edges %>%
  group_by(to) %>%
  summarise(n = n())

# build vector of sites with only one columnist linking to them
unique_vec <- in_links %>% filter(n == 1) %>% .[["to"]]

# number of times each columnist linked to a unique outlet
unique_table <- edges %>%
  filter(to %in% unique_vec) %>%
  group_by(from) %>%
  summarise(num.unique = n())

# final dataframe with the % of unique outlets
out_links_unique <- out_links %>%
  left_join(., unique_table) %>%
  mutate(pct.unique = round(100*num.unique/num.source))


