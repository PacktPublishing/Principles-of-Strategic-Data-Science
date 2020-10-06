## Chapter 3
library(tidyverse)
library(datasauRus)

ggplot(filter(datasaurus_dozen, dataset %in% c("dino", "away", "star", "bullseye", "slant_up", "dots")), aes(x = x, y = y)) +
  geom_point(colour = "#002859") +
  theme_void(base_size = 20) +
    theme(legend.position = "none") +
    facet_wrap(~dataset, ncol=3)

ggsave("../manuscript/images/figure13_Datasuarus.png",
       width = 6, height = 4, units = "in")


datasaurus_dozen %>%
    group_by(dataset) %>%
    summarise(meanX = mean(x),
              meanY = mean(x),
              cor = cor(x, y))

cor(datasaurus_dozen[,-1])
