# Packages ----------------------------------------------------------------
library(tidyverse)

# Read data ---------------------------------------------------------------

  library(readr)
stss <- read_csv("Data/results-for-sutton-trust-2018-07-09-1024.csv")

View(stss)
#recodes

stss$Q7[stss$Q7=="5"] <- "Very Poor"
stss$Q7[stss$Q7=="4"] <- "Poor"
stss$Q7[stss$Q7=="3"] <- "Okay"
stss$Q7[stss$Q7=="2"] <- "Good"
stss$Q7[stss$Q7=="1"] <- "Excellent"

stss$Q8[stss$Q8=="5"] <- "Very Poor"
stss$Q8[stss$Q8=="4"] <- "Poor"
stss$Q8[stss$Q8=="3"] <- "Okay"
stss$Q8[stss$Q8=="2"] <- "Good"
stss$Q8[stss$Q8=="1"] <- "Excellent"

stss$Q15[stss$Q15=="5"] <- "Very Poor"
stss$Q15[stss$Q15=="4"] <- "Poor"
stss$Q15[stss$Q15=="3"] <- "Okay"
stss$Q15[stss$Q15=="2"] <- "Good"
stss$Q15[stss$Q15=="1"] <- "Excellent"


stss$Q31[stss$Q31=="5"] <- "Very Poor"
stss$Q31[stss$Q31=="4"] <- "Poor"
stss$Q31[stss$Q31=="3"] <- "Okay"
stss$Q31[stss$Q31=="2"] <- "Good"
stss$Q31[stss$Q31=="1"] <- "Excellent"




# How would you rate academic sessions (Main and Second subjects combined)

a <- as.data.frame(table(stss$Q7))
View(a)
b <- as.data.frame(table(stss$Q8))
b <- rbind(b,data.frame("Var1"="Very Poor","Freq"=0))
View(b)

c <- a$Freq+b$Freq
View(c)
as.data.frame(c)
c <- cbind(c,data.frame(c("Excellent","Good","Okay","Poor","Very Poor")))
colnames(c) <- c("Freq","Var1")

c<-mutate(c,
          pct=(Freq/sum(Freq))*100)

c$pct <- round(c$pct,digits=1)




text(barplot(c$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
        las=1,
        border=NA,
        names.arg=c$Var1),
     c$pct,labels=paste0(c$pct,"%"),pos=3)

View(x)
View(Q15)
#group project sessions All Subjects Together

Q15 <- round(prop.table(table(stss$Q15))*100,digits=1)

Q15 <- as.data.frame(Q15)

Q15 <- rbind(Q15,data.frame("Var1"="Very Poor","Freq"=0.0))


text(barplot(Q15$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=c$Var1),
     Q15$Freq,labels=paste0(Q15$Freq,"%"),pos=3)




# all subject section

x <- (prop.table(table(stss$Q31))*100)


View(x)
x <- as.data.frame(x)

View(xdf)

xdf <- rbind(x,data.frame("Var1"="Poor","Freq"=0))

xdf <- rbind (xdf,data.frame("Var1"="Very Poor","Freq"=0))

View(xdf)            
            


barplot(xdf$Freq, col = c("red","green","blue","purple"),ylim=c(0,100),las=1)

barplot(xdf$Freq, col = c("red","green","blue","purple"),ylim=c(0,100),
        las=1,
        names.arg=xdf$Var1,
        text(xdf$Freq, labels=paste0(xdf$Freq,"%"), pos=3))



??text

BP <- barplot(xdf$Freq, col = c("red","green","blue","purple"),
        ylim=c(0,100),
        las=1,
        names.arg=xdf$Var1)


BO <- text(BP, xdf$Freq, labels=paste0(xdf$Freq,"%"), pos=3)


View(BO)

ggplot(xdf,aes(x = `Var1`, y = `Freq`))+
  geom_bar(stat = "indentity")





  geom_bar(stat = 'identity', fill = 'blue') +
  scale_y_continuous(limits = c(0, 80))+
  geom_text(vjust = -1) +
  labs(title = 'Chart for Matt')


barplot((prop.table(table(stss$`31. Overall, how would you rate the Sutton Trust Summer School at the University of Edinburgh?`))*100), col = c("red","green","blue","purple"))

ggplot(data=stss)+
  geom_bar(mapping=aes(x=`31. Overall, how would you rate the Sutton Trust Summer School at the University of Edinburgh?`)) +
  ylab("")+
  ylim(0,100)
  

ggplot(data=xdf)+
  geom_col(mapping=aes(x=Var1,y=Freq,fill="yellow"))+
  ylab("")+
  xlab("")+
  ylim(0,100)+
  theme(panel.background=element_blank())
  



prop.table(table(stss$`32.2.a. Overall, are you more or less likely to consider applying to any university now?`))
barplot(prop.table(table(stss$`32.2.a. Overall, are you more or less likely to consider applying to any university now?`)))

prop.table(table(stss$`32.1.a. Overall, are you more or less likely to consider applying to the University of Edinburgh now?`))
barplot(prop.table(table(stss$`32.1.a. Overall, are you more or less likely to consider applying to the University of Edinburgh now?`)))


# Results by subject

table(stss$`5. What was your MAIN subject at the summer school? (i.e. the one you worked on for your group project)`)
prop.table(table(stss$`5. What was your MAIN subject at the summer school? (i.e. the one you worked on for your group project)`))

table(stss$`6. What was your SECOND subject at the summer school?`)
prop.table(table(stss$`6. What was your SECOND subject at the summer school?`))




#main or second subject combo

prop.table(table(stss$`7. Please tell us how you would rate your [A1] morning sessions?`))
prop.table(table(stss$`8. Please tell us how you would rate your [A2] morning sessions?`))

View(combo.long)

#make biological sci main or second subset

biocombo <- subset(stss, `5. What was your MAIN subject at the summer school? (i.e. the one you worked on for your group project)`== 'Biological Sciences' | `6. What was your SECOND subject at the summer school?`== 'Biological Sciences')

#make biological sci main subject And second subject ONLY

biomain <- subset(stss, `5. What was your MAIN subject at the summer school? (i.e. the one you worked on for your group project)`== 'Biological Sciences')
biosec <- subset(stss, `6. What was your SECOND subject at the summer school?` == 'Biological Sciences' )

biomaintable <- table(biomain$`7. Please tell us how you would rate your [A1] morning sessions?`)
biosectable <- table(biosec$`8. Please tell us how you would rate your [A2] morning sessions?`)

tablecombo <- cbind(biomaintable,biosectable)



View(tablecombo)

tablecombosum <-prop.table(rowSums(tablecombo))
View(tablecombosum)

rbind(tablecombo, c("Poor", 0,0))

?rbind

barplot(prop.table(rowSums(tablecombo)))

barplot(tablecombosum,col = c("red","green","blue","purple"),ylim=c(0,1))

?barplot


barplot(prop.table(rowSums((tablecombo)*100)))

barplot((tablecombosum)*100) + ylab("balls")

tablecombosum <- as.data.frame(tablecombosum)

View(tablecombosum)

ggplot(data=as.data.frame(tablecombosum)) +
  geom_bar(mapping=aes(x=tablecombosum))

biomain$`7. Please tell us how you would rate your [A1] morning sessions?`

ggplot()

#main subject


prop.table(table(stss$`15. Please tell us how you would rate the afternoon group project sessions in [A1] (Monday,Tuesday and Wednesday)`))
prop.table(table(stss$`31. Overall, how would you rate the Sutton Trust Summer School at the University of Edinburgh?`)) 
prop.table(table(stss$`32.1.a. Overall, are you more or less likely to consider applying to the University of Edinburgh now?`))
prop.table(table(stss$`32.2.a. Overall, are you more or less likely to consider applying to any university now?`))


--------------------
# subset by first subject
  
bio <- subset(stss, `5. What was your MAIN subject at the summer school? (i.e. the one you worked on for your group project)`== 'Biological Sciences'
)

biocombo <- subset(stss, `5. What was your MAIN subject at the summer school? (i.e. the one you worked on for your group project)`== 'Biological Sciences' | `6. What was your SECOND subject at the summer school?`== 'Biological Sciences')


#questions by main subject only

#bio/if bio was your first subject

prop.table(table(stss$`15. Please tell us how you would rate the afternoon group project sessions in [A1] (Monday,Tuesday and Wednesday)`))
prop.table(table(stss$`31. Overall, how would you rate the Sutton Trust Summer School at the University of Edinburgh?`)) 
prop.table(table(stss$`32.1.a. Overall, are you more or less likely to consider applying to the University of Edinburgh now?`))
prop.table(table(stss$`32.2.a. Overall, are you more or less likely to consider applying to any university now?`))


biocombo


# first and  second subject combo

prop.table(table(stss$`7. Please tell us how you would rate your [A1] morning sessions?`))
prop.table(table(stss$`9. Overall, how would you rate the academic staff from your [A1] sessions?))
prop.table(table(stss$`8. Please tell us how you would rate your [A2] morning sessions?`))
prop.table(table(stss$`10. Overall, how would you rate the academic staff from your [A2] sessions?`

stss$





stucon <- read_csv("Raw Data/wpcon.csv")

# Create table ------------------------------------------------------------
# First I select the columns I'm interested in, and give them names that are easier to work with (although these names later appear in the chart)
# Then I 'gather' the data to create new variables called 'question' and 'response'
# I then create a simple summary table using group_by and summarise
# Finally I create a new variable which converts the numbers into percentages. Pay attention to how group_by works here.

View(stucon)


table <- stucon %>% 
  select(campus_tours = `4.1.a. Campus tours`,
         accomodation_welcome_tours = `4.2.a. Accommodation welcome talks`,
         academic_fair = `4.3.a. Academic Fair`,
         school_events = `4.4.a. UoE School/Department events such as welcome lectures`,
         coffee_crawls = `4.5.a. Coffee crawls`,
         library_tours = `4.6.a. Library tours`,
         student_association = `4.7.a. Student Association Social Events`) %>% 
  gather(question,
         response,
         campus_tours,
         accomodation_welcome_tours,
         academic_fair,
         school_events,
         coffee_crawls,
         library_tours,
         student_association) %>% 
  group_by(question, response) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(question) %>% 
  mutate(percent = (n/sum(n))*100)

# Create chart ------------------------------------------------------------

chart <- ggplot(table, aes(question, percent, fill = response, label = round(percent, 1))) +
  geom_bar(stat = 'identity') +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(title = 'Response by question',
       subtitle = 'A chart for Matt!',
         x = 'Question',
         y = 'Percent') +
  coord_flip()

chart

# Save chart --------------------------------------------------------------

ggsave(chart, filename = 'Output/chart.jpg')


View(table3)
-------------------------------
table3 <- stucon %>% 
  select('4.b.1.a. New Student Website',
         '4.b.2.a. UoE Events App',
         '4.b.3.a. Essential Getting Started Guide and Checklist',
         '4.b.4.a. Here to Help Guide',
         '4.b.5.a. IT and Library Guide')  %>% 
gather(response,question,
         '4.b.1.a. New Student Website',
         '4.b.2.a. UoE Events App',
         '4.b.3.a. Essential Getting Started Guide and Checklist',
         '4.b.4.a. Here to Help Guide',
         '4.b.5.a. IT and Library Guide') %>% 
  group_by(response,question) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(question) %>% 
  mutate(percent = (n/sum(n))*100)

