library(tidyverse)
library(gutenbergr)
library(tidytext)
library(wordcloud2)
library(RColorBrewer)

# gut <- gutenberg_works()
# gut %>% filter(str_detect(title, "Walden"))
# Walden ID = 205
walden_txt <- gutenberg_download(205)

# Removing Civil Disobedience
civil_index <- str_which(walden_txt$text, "ON THE DUTY OF CIVIL DISOBEDIENCE")
civil_index <- civil_index[3]:length(walden_txt$text)
civil_txt <- data.frame(text = walden_txt$text[civil_index], stringsAsFactors = F)
walden_txt <- data.frame(text = walden_txt$text[-civil_index], stringsAsFactors = F)

walden_words <- walden_txt %>% 
  unnest_tokens("word",text)

walden_words <- walden_words %>% 
  filter(!word %in% stop_words$word,! str_detect(word,"\\d")) 

walden_cloud_words <- walden_words %>% count(word) %>% arrange(desc(n)) %>% 
  filter(n >= 30)

cloud <- wordcloud2(data=walden_cloud_words, size=0.4, color= "random-dark", shape =  'pentagon')

afinn <- get_sentiments("afinn")

walden_afinn_summary <- walden_words %>% 
  inner_join(afinn) %>% 
  summarise(avg = mean(value), sd = sd(value)) 
walden_afinn_summary
walden_afinn_bar <- walden_words %>% 
  inner_join(afinn) %>% 
  ggplot(aes(value, fill = value)) +
  geom_bar()

nrc <- get_sentiments("nrc")

walden_nrc <- walden_words %>% 
  inner_join(nrc) %>%
  within( 
        sentiment <- factor(sentiment, 
                            levels=names(sort(table(sentiment), 
                                                          decreasing=F))))
  
nrc_plot<- walden_nrc %>% 
  ggplot(aes(sentiment, fill = sentiment)) +
  geom_bar(aes(y = ..prop.., group = 1, fill = sentiment))

