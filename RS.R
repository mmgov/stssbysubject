# Print subject heading

my_doc <- my_doc %>% 
  body_add_par("L. Religious Studies", style = "Section Header")

#The Academic Sessions

my_doc <- my_doc %>% 
  body_add_par("The academic sessions", style = "Normal")

#Q7 for those who selected RS for Q5, Q8 for those who selected RS for Q6

#make biological sci main subject And second subject ONLY

main <- subset(stss, `Q3`== 'Religious Studies')

sec <- subset(stss, `Q4` == 'Religious Studies' )



b <- as.data.frame(table(sec$Q6))

b <- merge(EVtable,
           b[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

b[is.na(b)] <- 0


b<-mutate(b,
          pct=(Freq/sum(Freq))*100)

b$pct <- round(b$pct,digits=1)

text(barplot(b$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=b$Var1),
     b$pct,labels=paste0(b$pct,"%"),pos=3)


# generate and inset graph

src <- tempfile(fileext = ".png")
png(filename = src, width = 6,  height = 3, units = 'in', res = 300)
text(barplot(b$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=b$Var1),
     b$pct,labels=paste0(b$pct,"%"),pos=3)

dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart L1 Academic sessions (XX respondents)", style = "Normal") %>% 
  body_add_img(src = src, width = 6,  height = 3)



my_doc <- my_doc %>% 
  body_add_par("How could the sessions be improved?", style = "Normal")%>% 
  body_add_par("Experience of the academic sessions", style = "Normal")%>%
  body_add_par("Experience of the morning academic sessions", style = "Normal")%>%
  body_add_par("Experience of the whole academic programme", style = "Normal")%>%
  body_add_par("Experience of the academic staff", style = "Normal")




#Experience of the academic staff




b <- as.data.frame(table(sec$Q8))

b <- merge(EVtable,
           b[,c("Var1",
                "Freq")],
           by="Var1",all.x=TRUE)

b[is.na(b)] <- 0


b<-mutate(b,
          pct=(Freq/sum(Freq))*100)

b$pct <- round(b$pct,digits=1)

text(barplot(b$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=b$Var1),
     b$pct,labels=paste0(b$pct,"%"),pos=3)


# generate and inset graph

src <- tempfile(fileext = ".png")
png(filename = src, width = 6,  height = 3, units = 'in', res = 300)
text(barplot(b$pct, col = c("red","green","blue","purple","yellow"),ylim=c(0,100),
             las=1,
             border=NA,
             names.arg=b$Var1),
     b$pct,labels=paste0(b$pct,"%"),pos=3)


dev.off()

my_doc <- my_doc %>% 
  body_add_par("Chart L2 The academic staff (XX respondents)", style = "Normal") %>% 
  body_add_img(src = src, width = 6,  height = 3)


my_doc <- my_doc %>% 
  body_add_par("Experience of the academic staff open box feedback", style = "Normal")%>% 
 




# The group project sessions
#Overall rating 
# Are you more likely to apply to ANY uni
# Are you more likely to apply to UoE

# No responses to the above as no one does RS as a main/project subject.


print(my_doc, target = "RS18.docx")



