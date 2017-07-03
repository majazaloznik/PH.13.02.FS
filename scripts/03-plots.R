library(RColorBrewer)



x.sum1  %>% 
  unite(category, old, SEX) %>% 
  select(-count) %>% 
  spread(category, prop) -> x.heights

x.sum1  %>% 
  unite(category, old, SEX) %>% 
  select(-prop) %>% 
  group_by(category) %>% 
  summarise(sum = sum(count))-> x.widths

barplot(as.matrix(x.heights[,2:5]) , 
        width = x.widths[[2]], 
        legend.text = x.heights[[1]],
        col = brewer.pal(7, "Dark2"))
