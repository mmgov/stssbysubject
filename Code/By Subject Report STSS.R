# Packages ----------------------------------------------------------------
library(tidyverse)
library(officer)


# Read data ---------------------------------------------------------------

stss <- read_csv("Data/results-for-sutton-trust-2018-07-10-1344.csv")
View(stss)
View(stss)
#recodes

stss$Q5[stss$Q5=="5"] <- "Very Poor"
stss$Q5[stss$Q5=="4"] <- "Poor"
stss$Q5[stss$Q5=="3"] <- "Okay"
stss$Q5[stss$Q5=="2"] <- "Good"
stss$Q5[stss$Q5=="1"] <- "Excellent"

stss$Q6[stss$Q6=="5"] <- "Very Poor"
stss$Q6[stss$Q6=="4"] <- "Poor"
stss$Q6[stss$Q6=="3"] <- "Okay"
stss$Q6[stss$Q6=="2"] <- "Good"
stss$Q6[stss$Q6=="1"] <- "Excellent"



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


stss$Q14[stss$Q14=="5"] <- "Very Poor"
stss$Q14[stss$Q14=="4"] <- "Poor"
stss$Q14[stss$Q14=="3"] <- "Okay"
stss$Q14[stss$Q14=="2"] <- "Good"
stss$Q14[stss$Q14=="1"] <- "Excellent"


stss$Q31[stss$Q31=="5"] <- "Very Poor"
stss$Q31[stss$Q31=="4"] <- "Poor"
stss$Q31[stss$Q31=="3"] <- "Okay"
stss$Q31[stss$Q31=="2"] <- "Good"
stss$Q31[stss$Q31=="1"] <- "Excellent"



stss$Q32_2_a[stss$Q32_2_a=="1"] <- "More likely"
stss$Q32_2_a[stss$Q32_2_a=="2"] <- "About the same"
stss$Q32_2_a[stss$Q32_2_a=="3"] <- "Less likely"
stss$Q32_2_a[stss$Q32_2_a=="4"] <- "NA/Don't know"


stss$Q32_1_a[stss$Q32_1_a=="1"] <- "More likely"
stss$Q32_1_a[stss$Q32_1_a=="2"] <- "About the same"
stss$Q32_1_a[stss$Q32_1_a=="3"] <- "Less likely"
stss$Q32_1_a[stss$Q32_1_a=="4"] <- "NA/Don't know"

View(stss)

#Question Response orders

MLorder <- c("More likely","About the same","Less likely","NA/Don't know")
EVorder <- c("Excellent","Good","Okay","Poor","Very Poor")


#create standard dfs

FreqB <- c(0,0,0,0,0)
FreqC <- c(0,0,0,0)

EVtable <- data.frame(EVorder,FreqB)
MLtable <- data.frame(MLorder,FreqC)
colnames(EVtable) <- c("Var1","FreqB")
colnames(MLtable) <- c("Var1","FreqB")
View(EVtable)
View(MLtable)
# How would you rate academic sessions (Main and Second subjects combined)

a <- as.data.frame(table(stss$Q5))
View(a)

a <- merge(EVtable,
              a[,c("Var1",
                   "Freq")],
              by="Var1",all.x=TRUE)

a[is.na(a)] <- 0


b <- as.data.frame(table(stss$Q6))

b <- merge(EVtable,
           b[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

b[is.na(b)] <- 0


c <- a$Freq+b$Freq
View(c)
as.data.frame(c)
c <- cbind(c,data.frame(c(EVorder)))
colnames(c) <- c("Freq","Var1")

c<-mutate(c,
          pct=(Freq/sum(Freq))*100)

c$pct <- round(c$pct,digits=1)

text(barplot(c$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
        las=1,
        border=NA,
        names.arg=c$Var1),
     c$pct,labels=paste0(c$pct,"%"),pos=3)

# generate and inset graph
my_doc <- read_docx(path = "ISTSS.docx") 
styles_info(my_doc)

src <- tempfile(fileext = ".png")
png(filename = src, width = 5.86, height = 3, units = 'in', res = 300)
text(barplot(c$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=c$Var1),
     c$pct,labels=paste0(c$pct,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 1 Academic sessions", style = "Normal") %>% 
  body_add_img(src = src, width = 5.86, height = 3)




View(x)
View(Q14)
#group project sessions All Subjects Together

Q14 <- round(prop.table(table(stss$Q14))*100,digits=1)
View(Q14)
Q14 <- as.data.frame(Q14)

Q14 <- merge(EVtable,
           Q14[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

Q14[is.na(Q14)] <- 0


text(barplot(Q14$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q14$Var1),
     Q14$Freq,labels=paste0(Q14$Freq,"%"),pos=3)



src <- tempfile(fileext = ".png")
png(filename = src, width = 5.86, height = 3, units = 'in', res = 300)
text(barplot(Q14$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q14$Var1),
     Q14$Freq,labels=paste0(Q14$Freq,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 2 Group project sessions", style = "Normal") %>%
  body_add_img(src = src, width = 5.86, height = 3)






# How would you rate the Academic staff (Main and Second subjects combined)

a <- as.data.frame(table(stss$Q7))
View(a)
a <- rbind(a,data.frame("Var1"="Very Poor","Freq"=0))

a <- merge(EVtable,
           a[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

a[is.na(a)] <- 0




b <- as.data.frame(table(stss$Q8))

b <- merge(EVtable,
           b[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

b[is.na(b)] <- 0



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


src <- tempfile(fileext = ".png")
png(filename = src, width = 5.86, height = 3, units = 'in', res = 300)
text(barplot(c$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=c$Var1),
     c$pct,labels=paste0(c$pct,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 3 Academic staff", style = "Normal") %>%
  body_add_img(src = src, width = 5.86, height = 3, style = "Normal")







# Overall rating of the STSS

Q31 <- round((prop.table(table(stss$Q31))*100),digits=1)

Q31 <- as.data.frame(Q31)

View(Q31)

Q31 <- merge(EVtable,
             Q31[,c("Var1",
                    "Freq")],
             by="Var1",all.x=TRUE)

Q31[is.na(Q31)] <- 0







text(barplot(Q31$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q31$Var1),
     Q31$Freq,labels=paste0(Q31$Freq,"%"),pos=3)

src <- tempfile(fileext = ".png")
png(filename = src, width = 5.86, height = 3, units = 'in', res = 300)
text(barplot(Q31$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q31$Var1),
     Q31$Freq,labels=paste0(Q31$Freq,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 4 Overall, how would you rate the Sutton Trust Summer school at UoE?", style = "Normal") %>%
  body_add_img(src = src, width = 5.86, height = 3, style = "Normal")


# Are you more likely to consider applying to any uni now?

Q32_2_a <- round((prop.table(table(stss$Q32_2_a))*100),digits = 1)

Q32_2_a <- as.data.frame(Q32_2_a)

View(Q32_2_a)




Q32_2_a <- merge(MLtable,
             Q32_2_a[,c("Var1",
                    "Freq")],
             by="Var1",all.x=TRUE)

Q32_2_a[is.na(Q32_2_a)] <- 0



Q32_2_a <- Q32_2_a%>%
  slice(match(MLorder,Var1))


View(Q32_2_a)

text(barplot(Q32_2_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q32_2_a$Var1),
     Q32_2_a$Freq,labels=paste0(Q32_2_a$Freq,"%"),pos=3)


src <- tempfile(fileext = ".png")
png(filename = src, width = 5.86, height = 3, units = 'in', res = 300)
text(barplot(Q32_2_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q32_2_a$Var1),
     Q32_2_a$Freq,labels=paste0(Q32_2_a$Freq,"%"),pos=3)

dev.off()


my_doc <- my_doc %>% 
  body_add_par("Chart 5 Overall, are you more or less likely to consider applying to ANY university now?", style = "Normal") %>%
  body_add_img(src = src, width = 5.86, height = 3, style = "Normal")





# Are you more likely to consider applying to UoE now?

Q32_1_a <- round((prop.table(table(stss$Q32_1_a))*100),digits = 1)

Q32_1_a <- as.data.frame(Q32_1_a)

View(Q32_1_a)

Q32_1_a <- merge(MLtable,
                 Q32_1_a[,c("Var1",
                            "Freq")],
                 by="Var1",all.x=TRUE)

Q32_1_a[is.na(Q32_1_a)] <- 0


Q32_1_a <- Q32_1_a%>%
  slice(match(MLorder,Var1))

text(barplot(Q32_1_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q32_1_a$Var1),
     Q32_1_a$Freq,labels=paste0(Q32_1_a$Freq,"%"),pos=3)

src <- tempfile(fileext = ".png")
png(filename = src, width = 5.86, height = 3, units = 'in', res = 300)
text(barplot(Q32_1_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q32_1_a$Var1),
     Q32_1_a$Freq,labels=paste0(Q32_1_a$Freq,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 6 Overall, are you more or less likely to consider applying to UoE now?", style = "Normal") %>%
  body_add_img(src = src, width = 5.86, height = 3, style = "Normal")


print(my_doc, target = "STSS18.docx")


# Section 2 Results by subject --------------------------------------------

# Results by subject

table(stss$Q3)
round(prop.table(table(stss$Q3))*100,digits=1)

table(stss$Q4)
round(prop.table(table(stss$Q4))*100,digits=1)







#Begin by subject section Biology



















tablecombo <- cbind(biomaintable,biosectable)




View(biomaintable)
View(biosectable)



View(tablecombo)

tablecombosum <-prop.table(rowSums(tablecombo))
View(tablecombosum)




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














tablecombo <- cbind(biomaintable,biosectable)

rbind(tablecombo, c("Poor", 0,0))

tablecombo <- rbind(tablecombo,data.frame("Var1"="Less likely","Freq"=0))


View(tablecombo)














            

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

