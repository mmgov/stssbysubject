# Print subject heading

my_doc <- my_doc %>% 
  body_add_par("E. Informatics", style = "Section Header")

#The Academic Sessions

my_doc <- my_doc %>% 
  body_add_par("The academic sessions", style = "Normal")

#Q7 for those who selected Biology for Q5, Q8 for those who selected Biology for Q6

#make biological sci main subject And second subject ONLY

main <- subset(stss, `Q3`== 'Computer Science (Informatics)')

sec <- subset(stss, `Q4` == 'Computer Science (Informatics)' )

a <- as.data.frame(table(main$Q5))

a <- merge(EVtable,
           a[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

a[is.na(a)] <- 0


b <- as.data.frame(table(sec$Q6))

b <- merge(EVtable,
           b[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

b[is.na(b)] <- 0

c <- a$Freq+b$Freq
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

src <- tempfile(fileext = ".png")
png(filename = src, width = 6,  height = 3, units = 'in', res = 300)
text(barplot(c$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=c$Var1),
     c$pct,labels=paste0(c$pct,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart E1 Academic sessions (XX respondents)", style = "Normal") %>% 
  body_add_img(src = src, width = 6,  height = 3)



my_doc <- my_doc %>% 
  body_add_par("How could the sessions be improved?", style = "Normal")%>% 
  body_add_par("Experience of the academic sessions", style = "Normal")%>%
  body_add_par("Experience of the morning academic sessions", style = "Normal")%>%
  body_add_par("Experience of the whole academic programme", style = "Normal")%>%
  body_add_par("Experience of the academic staff", style = "Normal")




#Experience of the academic staff

a <- as.data.frame(table(main$Q7))

a <- merge(EVtable,
           a[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

a[is.na(a)] <- 0

View(a)


b <- as.data.frame(table(sec$Q8))

b <- merge(EVtable,
           b[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

b[is.na(b)] <- 0



c <- a$Freq+b$Freq
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

src <- tempfile(fileext = ".png")
png(filename = src, width = 6,  height = 3, units = 'in', res = 300)
text(barplot(c$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=c$Var1),
     c$pct,labels=paste0(c$pct,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart E2 The academic staff (XX respondents)", style = "Normal") %>% 
  body_add_img(src = src, width = 6,  height = 3)


my_doc <- my_doc %>% 
  body_add_par("Experience of the academic staff open box feedback", style = "Normal")%>% 
  body_add_par("The group project sessions", style = "Normal")






# The group project sessions


Q14 <- round((prop.table(table(main$Q14))*100),digits = 1)

Q14 <- as.data.frame(Q14)

Q14 <- merge(EVtable,
             Q14[,c("Var1",
                    "Freq")],
             by="Var1",all.x=TRUE)

Q14[is.na(Q14)] <- 0



text(barplot(Q14$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,130),
             las=1,
             border=NA,
             names.arg=Q14$Var1),
     Q14$Freq,labels=paste0(Q14$Freq,"%"),pos=3)


# generate and inset graph

src <- tempfile(fileext = ".png")
png(filename = src, width = 6,  height = 3, units = 'in', res = 300)
text(barplot(Q14$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,130),
             las=1,
             border=NA,
             names.arg=Q14$Var1),
     Q14$Freq,labels=paste0(Q14$Freq,"%"),pos=3)

dev.off()




my_doc <- my_doc %>% 
  body_add_par("Chart E3 The group project sessions (XX respondents)", style = "Normal") %>% 
  body_add_img(src = src, width = 6,  height = 3)



my_doc <- my_doc %>% 
  body_add_par("How could the group project sessions be improved?", style = "Normal")%>% 
  body_add_par("Anything else about your experience of the group project sessions", style = "Normal")


#Overall rating 

Q31 <- round((prop.table(table(main$Q31))*100),digits=1)

Q31 <- as.data.frame(Q31)

View(Q31)

Q31 <- merge(EVtable,
             Q31[,c("Var1",
                    "Freq")],
             by="Var1",all.x=TRUE)

Q31[is.na(Q31)] <- 0

text(barplot(Q31$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,130),
             las=1,
             border=NA,
             names.arg=Q31$Var1),
     Q31$Freq,labels=paste0(Q31$Freq,"%"),pos=3)

src <- tempfile(fileext = ".png")
png(filename = src, width = 6,  height = 3, units = 'in', res = 300)
text(barplot(Q31$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,130),
             las=1,
             border=NA,
             names.arg=Q31$Var1),
     Q31$Freq,labels=paste0(Q31$Freq,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart E4 Overall, how would you rate the Sutton Trust Summer school at UoE? (XX respondents)", style = "Normal") %>%
  body_add_img(src = src, width = 6,  height = 3, style = "Normal")




# Are you more likely to apply to ANY uni

Q32_2_a <- round((prop.table(table(main$Q32_2_a))*100),digits = 1)

Q32_2_a <- as.data.frame(Q32_2_a)

View(Q32_2_a)


Q32_2_a <- merge(MLtable,
                 Q32_2_a[,c("Var1",
                            "Freq")],
                 by="Var1",all.x=TRUE)

Q32_2_a[is.na(Q32_2_a)] <- 0



Q32_2_a <- Q32_2_a%>%
  slice(match(MLorder,Var1))


text(barplot(Q32_2_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,130),
             las=1,
             border=NA,
             names.arg=Q32_2_a$Var1),
     Q32_2_a$Freq,labels=paste0(Q32_2_a$Freq,"%"),pos=3)


src <- tempfile(fileext = ".png")
png(filename = src, width = 6,  height = 3, units = 'in', res = 300)
text(barplot(Q32_2_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,130),
             las=1,
             border=NA,
             names.arg=Q32_2_a$Var1),
     Q32_2_a$Freq,labels=paste0(Q32_2_a$Freq,"%"),pos=3)

dev.off()


my_doc <- my_doc %>% 
  body_add_par("Chart E5 Overall, are you more or less likely to consider applying to ANY university now? (XX respondents)", style = "Normal") %>%
  body_add_img(src = src, width = 6,  height = 3, style = "Normal")









# Are you more likely to apply to UoE

Q32_1_a <- round((prop.table(table(main$Q32_1_a))*100),digits = 1)

Q32_1_a <- as.data.frame(Q32_1_a)

View(Q32_1_a)

Q32_1_a <- merge(MLtable,
                 Q32_1_a[,c("Var1",
                            "Freq")],
                 by="Var1",all.x=TRUE)

Q32_1_a[is.na(Q32_1_a)] <- 0


Q32_1_a <- Q32_1_a%>%
  slice(match(MLorder,Var1))

text(barplot(Q32_1_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,130),
             las=1,
             border=NA,
             names.arg=Q32_1_a$Var1),
     Q32_1_a$Freq,labels=paste0(Q32_1_a$Freq,"%"),pos=3)

src <- tempfile(fileext = ".png")
par(mar=c(5,6,15,1)+.1)
png(filename = src, width = 6, height = 3, units = 'in', res = 300)
text(barplot(Q32_1_a$Freq, col = c("red","green","blue","purple","yellow"),ylim=c(0,130),
             las=1,
             border=NA,
             names.arg=Q32_1_a$Var1),
     Q32_1_a$Freq,labels=paste0(Q32_1_a$Freq,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart E6 Overall, are you more or less likely to consider applying to UoE now? (XX respondents)", style = "Normal") %>%
  body_add_img(src = src, width = 6, height = 3, style = "Normal")


print(my_doc, target = "In18.docx")


?abline
??las
  


