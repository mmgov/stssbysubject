# Packages ----------------------------------------------------------------
library(tidyverse)
library(officer)


# Read data ---------------------------------------------------------------

stss <- read_csv("Data/results-for-sutton-trust-2018-07-10-1344.csv")

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

# How would you rate academic sessions (Main and Second subjects combined)

a <- as.data.frame(table(stss$Q5))


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
png(filename = src, width = 6, height = 3, units = 'in', res = 300)
text(barplot(c$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=c$Var1),
     c$pct,labels=paste0(c$pct,"%"),pos=3)

dev.off()

my_doc <- my_doc %>%
  body_add_par("Chart 1 Academic sessions", style = "Normal") %>% 
  body_add_img(src = src, width = 6, height = 3)





View(Q14)
#group project sessions All Subjects Together

Q14 <- round(prop.table(table(stss$Q14))*100,digits=1)

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
png(filename = src, width = 6, height = 3, units = 'in', res = 300)
text(barplot(Q14$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q14$Var1),
     Q14$Freq,labels=paste0(Q14$Freq,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 2 Group project sessions", style = "Normal") %>%
  body_add_img(src = src, width = 6, height = 3)






# How would you rate the Academic staff (Main and Second subjects combined)

a <- as.data.frame(table(stss$Q7))

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
png(filename = src, width = 6, height = 3, units = 'in', res = 300)
text(barplot(c$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=c$Var1),
     c$pct,labels=paste0(c$pct,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 3 Academic staff", style = "Normal") %>%
  body_add_img(src = src, width = 6, height = 3, style = "Normal")







# Overall rating of the STSS

Q31 <- round((prop.table(table(stss$Q31))*100),digits=1)

Q31 <- as.data.frame(Q31)

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
png(filename = src, width = 6, height = 3, units = 'in', res = 300)
text(barplot(Q31$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q31$Var1),
     Q31$Freq,labels=paste0(Q31$Freq,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 4 Overall, how would you rate the Sutton Trust Summer school at UoE?", style = "Normal") %>%
  body_add_img(src = src, width = 6, height = 3, style = "Normal")


# Are you more likely to consider applying to any uni now?

Q32_2_a <- round((prop.table(table(stss$Q32_2_a))*100),digits = 1)

Q32_2_a <- as.data.frame(Q32_2_a)

Q32_2_a <- merge(MLtable,
             Q32_2_a[,c("Var1",
                    "Freq")],
             by="Var1",all.x=TRUE)

Q32_2_a[is.na(Q32_2_a)] <- 0

Q32_2_a <- Q32_2_a%>%
  slice(match(MLorder,Var1))



text(barplot(Q32_2_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q32_2_a$Var1),
     Q32_2_a$Freq,labels=paste0(Q32_2_a$Freq,"%"),pos=3)


src <- tempfile(fileext = ".png")
png(filename = src, width = 6, height = 3, units = 'in', res = 300)
text(barplot(Q32_2_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q32_2_a$Var1),
     Q32_2_a$Freq,labels=paste0(Q32_2_a$Freq,"%"),pos=3)

dev.off()


my_doc <- my_doc %>% 
  body_add_par("Chart 5 Overall, are you more or less likely to consider applying to ANY university now?", style = "Normal") %>%
  body_add_img(src = src, width = 6, height = 3, style = "Normal")





# Are you more likely to consider applying to UoE now?

Q32_1_a <- round((prop.table(table(stss$Q32_1_a))*100),digits = 1)

Q32_1_a <- as.data.frame(Q32_1_a)

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
png(filename = src, width = 6, height = 3, units = 'in', res = 300)
text(barplot(Q32_1_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=Q32_1_a$Var1),
     Q32_1_a$Freq,labels=paste0(Q32_1_a$Freq,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart 6 Overall, are you more or less likely to consider applying to UoE now?", style = "Normal") %>%
  body_add_img(src = src, width = 6, height = 3, style = "Normal")





# Section 2 Results by subject --------------------------------------------

# Results by subject

table(stss$Q3)
round(prop.table(table(stss$Q3))*100,digits=1)

table(stss$Q4)
round(prop.table(table(stss$Q4))*100,digits=1)







#Begin by subject section Biology

