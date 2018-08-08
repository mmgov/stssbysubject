m <- as.data.frame.character(stss$Q9)


my_doc <- read_docx(path = "ISTSS.docx") 
styles_info(my_doc)


my_doc <- my_doc %>%
  body_add_par("Chart 1 Academic sessions", style = "Normal") %>% 
  body_add_par("Free Text", style = "Section Header")%>%
  body_add_table(value = a )

print(my_doc, target = "STSS18.docx")



body_add_tabl
View(iris)

print(stss$Q9)
View(a)
a <- as.data.frame(stss$Q9)
View(EVtable)
m<- as.data.frame(cat(stss$Q9))

View(stss$Q9)

View(m)

View(cat(stss$Q9))


stss$Q9