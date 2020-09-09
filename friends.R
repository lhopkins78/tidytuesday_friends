library(tidyverse)
library(friends)
library(tidytext)
library(cowplot)

#stuff to be used later
`%notin%` <- Negate(`%in%`)
main_cast <- c("Ross Geller", "Rachel Green", "Joey Tribbiani", "Chandler Bing", "Phoebe Buffay")
f_cols <- c("#e91e23", "#02b2e7", "#fabc16", "#43C59E","#2B3A67")

#words without stop words
friends_words_stop <- friends %>% unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% filter(speaker != "Scene Directions")

#words with stop words
friends_words <- friends %>% unnest_tokens(word, text) %>%
  filter(speaker != "Scene Directions")

friends_word_count_season <- friends_words %>% 
  filter(speaker %in% main_cast) %>%
  group_by(speaker, season) %>%
  tally() %>% ungroup() %>%
  group_by(season) %>% mutate(perc = n/sum(n)) %>%
  arrange(desc(n)) 

img_bing <- "https://img.bricklink.com/ItemImage/MN/0/idea058.png"
img_joey <- "https://img.bricklink.com/ItemImage/MN/0/idea060.png"
img_rachel <- "https://img.bricklink.com/ItemImage/MN/0/idea059.png"
img_ross <- "https://img.bricklink.com/ItemImage/MN/0/idea056.png"
img_phoebe <- "https://img.bricklink.com/ItemImage/MN/0/idea061.png"

p1 <- ggplot(friends_word_count_season, aes(y=perc, x=factor(season), fill=speaker, group=speaker)) + 
  geom_area() + scale_fill_manual(values=f_cols) +
  theme_minimal() + 
  facet_wrap(~speaker, nrow=1) +
  labs(title="Democratic speech?", subtitle="Word share for the big five in Friends by season",
       x="Season", y="") +
  theme(legend.position="none",
    text=element_text(family="Avenir", size=14),
    plot.title=element_text(family="Gabriel Weiss' Friends Font", size=40)) +
  scale_y_continuous(labels=scales::percent)

ggdraw() +
  draw_plot(p1) +
  draw_image(img_joey, scale=0.25, y=-0.2, x=-0.165) +
  draw_image(img_bing, scale=0.25, y=-0.2, x=-0.355) +
  draw_image(img_phoebe, scale=0.25, y=-0.2, x=0.024) +
  draw_image(img_rachel, scale=0.25, y=-0.2, x=+0.21) +
  draw_image(img_ross, scale=0.25, y=-0.2, x=+0.4) 
ggsave("friends.png")
