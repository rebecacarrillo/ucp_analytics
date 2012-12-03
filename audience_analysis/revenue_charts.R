source('~/R/Revenue_Frequency_Tables.R')
(library "ggplot2")
(library "scales")

Subscriber<-Bake.Subs(Revenue)

Subscribers<-ggplot(Subscriber, aes(x=factor(Subscriber$Subscriber.Groups), 
                                 y=Subscriber$Percentage, 
                                 fill=factor(Subscriber$Subscriber.Groups),))
Subscribers<-Subscribers + geom_bar() + 
  coord_flip() + 
  scale_y_continuous (labels = percent)

Subscribers<-Subscribers + 
  xlab('')+
  ylab('')+
  labs(fill='Category')
Subscribers<- Subscribers + xlab('') + ylab('') + labs(fill='Category')

Subscribers <- Subscribers + 
  ggtitle("Subscribers")