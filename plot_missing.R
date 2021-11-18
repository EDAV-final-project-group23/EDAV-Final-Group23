library(tidyverse)
library(patchwork)

plot_missing<-function(df, percent = FALSE){
  missing_patterns <- data.frame(is.na(df)) %>%
    group_by_all() %>%
    count(name = "counts", sort = TRUE) %>%
    ungroup()
  if (percent){
    missing_patterns$counts<- missing_patterns$counts/nrow(df)*100
  }
  
  missing_patterns$pattern<-1:nrow(missing_patterns)
  missing_patterns$pattern<-as.character(missing_patterns$pattern)
  missing_patterns$pattern <- factor(missing_patterns$pattern, 
                                     levels = rev(factor(missing_patterns$pattern)))
  # print(missing_patterns)
  
  first_var <- colnames(missing_patterns)[1]
  last_var <- colnames(missing_patterns)[ncol(df)]
  helper<- missing_patterns%>%gather(var_name, is_missing, first_var:last_var)
  helper$is_missing<-as.numeric(helper$is_missing)
  var_miss_counts<-helper%>%group_by(var_name)%>% 
    summarise(mis_counts = sum(counts*is_missing)) %>% 
    arrange(-mis_counts)
  helper$var_name<-factor(helper$var_name, levels= var_miss_counts$var_name)
  
  # print(helper)
  
  complete <- NULL
  rs <- rowSums(missing_patterns[1:ncol(df)])
  for (i in 1:nrow(missing_patterns)){
    if (rs[i]==0){
      complete = missing_patterns$pattern[i]
      break
    }
  }
  if (is.null(complete)){
    helper$alphas <- 0.5
  }
  else{
    helper$alphas <- if_else( helper$pattern==complete, 1.0, 0.5)
  }
  
  
  p1<-ggplot(helper, aes(x = var_name, y = pattern, fill = is_missing)) +
    geom_tile(color = "white", alpha = helper$alphas) +
    scale_fill_gradient(low = "dark grey", high = "#4B0082")+
    theme(legend.position = "None")
  if (!is.null(complete)){
    p1 <- p1+ annotate('text', x = var_miss_counts$var_name[(ncol(df)+1)%/%2],
                       y =complete,
                       label = "complete cases",
                       color = 'black')
  }
  
  if (percent){
    p2<-ggplot(var_miss_counts, aes(x = reorder(var_name, -mis_counts), y = mis_counts)) +
      geom_bar(stat="identity", fill = '#6495ED', alpha = 0.7)+  xlab("") + 
      ylab("% rows missing:")+
      ylim(0, 100)
    
    p3<-ggplot(missing_patterns, aes(x = reorder(pattern, counts), y = counts)) +
      geom_bar(stat="identity", fill = '#6495ED', alpha = 0.7)+  
      xlab("") + 
      ylab("% rows")+
      coord_flip()+
      ylim(0, 100)
  }
  else{
    p2<-ggplot(var_miss_counts, aes(x = reorder(var_name, -mis_counts), y = mis_counts)) +
      geom_bar(stat="identity", fill = '#6495ED', alpha = 0.7)+  xlab("") + 
      ylab("num rows missing:")
    
    p3<-ggplot(missing_patterns, aes(x = reorder(pattern, counts), y = counts)) +
      geom_bar(stat="identity", fill = '#6495ED', alpha = 0.7)+  
      xlab("") + 
      ylab("row count")+
      coord_flip()
  }
  
  
  p4<-p1+p3+
    plot_layout(heights = 5, widths = c(5,1))
  p5<-p2+ plot_spacer()+
    plot_layout(heights = 5, widths = c(7,2))
  p5/p4 +
    plot_layout( heights = c(2,7), widths = c(1))
}

