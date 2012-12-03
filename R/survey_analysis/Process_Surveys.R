library(ggplot2)
library(scales)


#Live Music
#factor and table LiveMusic
 LiveMusic<-as.factor(ConcertPI$LiveMusic)
 LiveMusic<-ordered(LiveMusic, levels= c(1,2,3,4),
              labels=c("2 or less",
                       "3 to 5", 
                       "6 to 8", 
                       "9 or more"))
 LM.Response<-c("2 or less",
                "3 to 5", 
                "6 to 8", 
                "9 or more")
 
 LiveMusic <- table(LiveMusic)/length(LiveMusic)
 LM.Summary<-c(as.vector(LiveMusic))
 LiveMusic<-data.frame(LM.Summary, LM.Response)

#UCPLoyalty
 UCPLoyalty<-as.factor(ConcertPI$UCPLoyalty)
 UCPLoyalty<-ordered(UCPLoyalty, levels= c(1,2,3,4,5),
                     labels=c("First Time",
                              "0 to 4 years",
                              "5 to 9 years",
                              "10 to 19 years",
                              "20+ years"))
 Loy.Response<-c("First Time",
                 "0 to 4 years",
                 "5 to 9 years",
                 "10 to 19 years",
                 "20+ years")
 UCPLoyalty<-table(UCPLoyalty)/length(UCPLoyalty)
 Loy.Summary<-c(as.vector(UCPLoyalty))
 UCPLoyalty<-data.frame(Loy.Summary, Loy.Response)
 
 

 
#Shuttle
 Shuttle<-as.factor(ConcertPI$Shuttle)
 Shuttle<-ordered(ConcertPI$Shuttle, levels = c(1,2,3,4,5),
                 labels=c("Definitely",
                          "Probably",
                          "Not Sure",
                          "Probably Not","Definitely Not"))
 Shut.Response<-c("Definitely",
                  "Probably",
                  "Not Sure",
                  "Probably Not","Definitely Not")
 Shuttle<-table(Shuttle)/length(Shuttle)
 Shut.Summary<-c(as.vector(Shuttle))
 Shuttle<-data.frame(Shut.Summary, Shut.Response)

#Subscriber
 Subscriber<-as.factor(ConcertPI$Subscriber)
 Subscriber<-ordered(ConcertPI$Subscriber, levels = c(1,2), 
                    labels=c("No","Yes"))
 Sub.Response<-c("No", "Yes")
 Subscriber<-table(Subscriber)/length(Subscriber)
 Sub.Summary<-c(as.vector(Subscriber))
 Subscriber<-data.frame(Sub.Summary, Sub.Response)
 
 rm(Loy.Response, Loy.Summary, 
    Shut.Response, Shut.Summary,
    LM.Response, LM.Summary,
    Sub.Response, Sub.Summary)
 
 # === Make Bar Charts ==== #
 
 
 #UCP Loyalty

 png("UCPLoyalty.png", width=500, height=500)
 UCPLoyalty<-ggplot(UCPLoyalty, aes(x=factor(Loy.Response), 
                                    y=Loy.Summary, 
                                    fill=factor(Loy.Response),))
 
 UCPLoyalty<-UCPLoyalty +
   geom_bar()+
   coord_flip()+ 
   scale_y_continuous(labels = percent)+ 
   xlab('')+ 
   ylab('')+ 
   labs(fill='Response')+ 
   ggtitle("How long have you been attending concerts with UCP? \n")
 print(UCPLoyalty)
 dev.off()
 
 
 
 #Live Music
 png("LiveMusic.png", width=500, height=500)
 LiveMusic<-ggplot(LiveMusic, aes(x=factor(LM.Response), 
                                  y=LM.Summary, 
                                  fill=factor(LM.Response),))
 LiveMusic<-LiveMusic + geom_bar() + 
   coord_flip() + 
   scale_y_continuous (labels = percent)
 
 LiveMusic<-LiveMusic + 
   xlab('')+
   ylab('')+
   labs(fill='Response')
 LiveMusic<- LiveMusic + xlab('') + ylab('') + labs(fill='Response')
 
 LiveMusic <- LiveMusic + 
   ggtitle("How many live music performances\n do you attend per year? \n")
print(LiveMusic)
 dev.off()
 
 
 
 #Shuttle
 png("Shuttle.png", width=500, height=500)
 Shuttle<-ggplot(Shuttle, aes(x=factor(Shut.Response), 
                              y=Shut.Summary, 
                              fill=factor(Shut.Response),))
 Shuttle<-Shuttle + 
   geom_bar() + 
   coord_flip() + 
   scale_y_continuous(labels = percent) 
 
 Shuttle<-Shuttle + 
   xlab('') + 
   ylab('') + 
   labs(fill='Response') + 
   ggtitle("If offered, would you use shuttle services? \n")
 print(Shuttle)
 dev.off()
 
 
 #Subscriber Plot
 png("Subscriber.png", width=500, height=500)
 Subscriber<-ggplot(Subscriber, aes(x=factor(Sub.Response), 
                                    y=Sub.Summary, 
                                    fill=factor(Sub.Response),))
 
 
 
 Subscriber<-Subscriber + 
   geom_bar() + 
   coord_flip()+ 
   scale_y_continuous(labels = percent)+ 
   xlab('') + 
   ylab('') + 
   labs(fill='Response')+ 
   ggtitle("Subscriber? \n")
 print(Subscriber)
 dev.off()
 
 
 #==Performance Elements==
 
 
 #Return five-number summaries for Performance Elements
 tumble<-function(ConcertPE){
   
   Overall.Exp<-summary(ConcertPE$Overall.Exp)
   ArtP<-summary(ConcertPE$ArtP)
   IntM<-summary(ConcertPE$IntM)
   PreCT<-summary(ConcertPE$PreCT)
   Venue<-summary(ConcertPE$Venue)
   PriceValue<-summary(ConcertPE$PriceValue)
   
   sumlist<-list(Overall.Exp, ArtP, IntM, PreCT, Venue, PriceValue)
   return(sumlist)}

#sink sumlist to wd
dev.off()
Five.Number<-tumble(ConcertPE)
"FuckThisJob.pdf"



