## Telling stories with visualisations

library(tidyverse)
library(gridExtra)
set.seed(1969)

comparison <- tibble(item = LETTERS[1:5],
                     value = sample(10:20, 5))
c <- ggplot(comparison, aes(item, value)) + geom_col(fill = "#002859", alpha = 0.5) +
    theme_minimal(base_size = 40) +
    theme(plot.title = element_text(face="bold")) +
    scale_x_discrete(breaks = NULL) +     
    ggtitle("Comparison")

relationship <- tibble(x = 1:20,
                       y = x + sample(-2:2, 20, replace = TRUE),
                       z = sample(1:5, 20 , replace = TRUE))
r1 <- ggplot(relationship, aes(x, y, z)) +
    geom_point(size = 10, col = "#002859") +
    ggtitle("Relationship: Two variables") + 
    theme_minimal(base_size = 40) +
    theme(plot.title = element_text(face="bold"))
r2 <- ggplot(relationship, aes(x, y, z)) +
    geom_point(aes(size = z), col = "#002859", alpha = 0.5) +
    scale_size_area(max_size = 25) +
    theme_minimal(base_size = 40) +
    theme(plot.title = element_text(face="bold")) +
    ggtitle("Relationship: Three variables")

distribution <- enframe(rnorm(1000))
d <- ggplot(distribution, aes(value)) +
    geom_histogram(bins = 40, fill = "#002859", alpha = 0.5) +
    theme_minimal(base_size = 40) +
    theme(plot.title = element_text(face="bold")) +
    ggtitle("Distribution")

png("../manuscript/images/figure07_Stories.png", width = 2048, height = 1536)
grid.arrange(c, d, r1, r2, ncol=2)
dev.off()

## Data to ink ratio
## library(devtools)
## devtools::install_github("hilaryparker/cats")

library(cats)

bad <- ggplot(comparison, aes(item, value, fill = item)) +
    add_cat(bw = FALSE) +
    geom_col(alpha = 0.5, col = "black") +
    theme_gray(base_size = 40) +
    theme(legend.position="left", legend.key.size = unit(5,"line"),
          plot.title = element_text(face = "bold")) +
    ggtitle("Low Data-Pixel Ratio")

comparison$item <- factor(comparison$item, level = comparison$item[order(comparison$value)])

good <- ggplot(comparison, aes(item, value)) +
    geom_col(fill = "#002859", alpha = 0.7) +
    scale_x_discrete(breaks = NULL) + 
    theme_minimal(base_size = 40) +
    theme(plot.title = element_text(face = "bold")) + 
    ggtitle("High Data-Pixel Ratio")

png("../manuscript/images/figure09_Visualisation.png",
    width = 1800, height = 1000)
grid.arrange(bad, good, ncol = 2, widths = c(1.1, 0.9))
dev.off()

## Wordcloud
library(wordcloud)
library(tidytext)
library(tidyverse)

ch2 <- readLines("../manuscript/chapter_2.md") %>%
    enframe() %>%
    filter(value != "")
ch2

tokens <- ch2 %>% 
  unnest_tokens(word, value) %>% 
  count(word, sort = TRUE) %>% 
    ungroup()

data("stop_words")
tokens_clean <- tokens %>%
    anti_join(stop_words)
tokens_clean

set.seed(202)
pdf("wordcloud.pdf")

with(tokens_clean, wordcloud(word, n, random.order = FALSE, max.words = 25,
                             colors = c("#002358", "#24405d", "#00a59d")))

dev.off()
