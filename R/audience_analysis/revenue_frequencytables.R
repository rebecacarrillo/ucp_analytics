library("ggplot2")
library("scales")
library("odfWeave")

Revenue<-read.csv("revenue.csv")

Bake.Subs<-function(Revenue){
  
#Subscriber Frequency tables
#Breakdown of Subscribers among Subscribers
GenSubFrequency<-(Revenue$GenSub/Revenue$SubTotal)
FacSubFrequency<-(Revenue$FacSub/Revenue$SubTotal)
StuSubFrequency<-(Revenue$StuSub/Revenue$SubTotal)

Subscriber.Groups<-c("General Subscribers", 
              "Facutly Subscribers", 
              "Student Subscribers")

SubFreq<-data.frame(GenSubFrequency, FacSubFrequency, StuSubFrequency)
General.Subscribers<-SubFreq$GenSubFrequency
Faculty.Subscribers<-SubFreq$FacSubFrequency
Student.Subscribers<-SubFreq$StuSubFrequency

Percentage<-c((100 * General.Subscribers), 
              (100 * Faculty.Subscribers), 
              (100 * Student.Subscribers))

SubscriberTickets<-data.frame(Subscriber.Groups, Percentage)
return(SubscriberTickets)}


#General Frequency tables
#Breakdown of General Ticket buyers among General Ticket Buyers

Bake.Gen<-function(Revenue){
GenFrequency<-(Revenue$Gen/(Revenue$AllTotal-Revenue$SubTotal))
FacFrequency<-(Revenue$Fac/(Revenue$AllTotal-Revenue$SubTotal))
StuFrequency<-(Revenue$Stu/(Revenue$AllTotal-Revenue$SubTotal))
CompFrequency<-(Revenue$Comp/(Revenue$AllTotal-Revenue$SubTotal))

General.Groups<-c("General Single-Ticket Buyers", 
                     "Facutly Single-Ticket Buyers",
                     "Student Single-Ticket Buyers",
                     "Comps")

GenFreq<-data.frame(GenFrequency, FacFrequency, StuFrequency, CompFrequency)
General<-GenFreq$GenFrequency
Faculty<-GenFreq$FacFrequency
Student<-GenFreq$StuFrequency
Comps<-GenFreq$CompFrequency

Percentage<-c((100 * General), 
              (100 * Faculty), 
              (100 * Student),
           (100 * Comps))

GenTickets<-data.frame(General.Groups, Percentage)
return(GenTickets)}


#All Frequency Tables
#Breakdown among all ticket buyers
Bake<-function(Revenue){
  GenFrequency<-(Revenue$Gen/Revenue$AllTotal)
  FacFrequency<-(Revenue$Fac/Revenue$AllTotal)
  StuFrequency<-(Revenue$Stu/Revenue$AllTotal)
  CompFrequency<-(Revenue$Comp/Revenue$AllTotal)
  GenSubFrequency<-(Revenue$GenSub/Revenue$AllTotal)
  FacSubFrequency<-(Revenue$FacSub/Revenue$AllTotal)
  StuSubFrequency<-(Revenue$StuSub/Revenue$AllTotal)
  
  All.Groups<-c("General Single-Ticket Buyers", 
                "Facutly Single-Ticket Buyers",
                "Student Single-Ticket Buyers",
                "Comps",
                "General Subscribers", 
                "Facutly Subscribers", 
                "Student Subscribers")
  
  AllFreq<-data.frame(GenFrequency, 
                      FacFrequency, 
                      StuFrequency, 
                      CompFrequency,
                      GenSubFrequency, 
                      FacSubFrequency,
                      StuSubFrequency)
  
  GeneralSingleTickets<-AllFreq$GenFrequency
  FacultySingleTickets<-AllFreq$FacFrequency
  StudentSingleTickets<-AllFreq$StuFrequency
  GeneralSubscribers<-AllFreq$GenSubFrequency
  FacultySubscribers<-AllFreq$FacSubFrequency
  StudentSubscribers<-AllFreq$StuSubFrequency
  Comps<-AllFreq$CompFrequency
  
  Percentage<-c((100 * GeneralSingleTickets), 
             (100 * FacultySingleTickets), 
             (100 * StudentSingleTickets),
             (100 * GeneralSubscribers),
         (100 * FacultySubscribers),
         (100 * StudentSubscribers),
         (100 * Comps))
         
  
  AllTickets<-data.frame(All.Groups, Percentage)
  return(AllTickets)}

odfWeave("~/R/AudienceAnalysis/ConcertRevenueTemplate.odt", 
         "~/R/AudienceAnalysis/Test.odt")