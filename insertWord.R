require(tidyverse)
install.packages("R.utils")
install.packages("officer")
require(officer)


my_doc <- read_docx() 
styles_info(my_doc)

src <- tempfile(fileext = ".png")
png(filename = src, width = 5, height = 6, units = 'in', res = 300)
barplot(xdf$Freq, col = c("red","green","blue","purple"),ylim=c(0,100),
        las=1,
        names.arg=xdf$Var1)
dev.off()

my_doc <- my_doc %>% 
  body_add_img(src = src, width = 5, height = 6, style = "centered")

print(my_doc, target = "first_example.docx")

======================

  img.file <- file.path( R.home("doc"), "html", "logo.jpg" )
read_docx() %>%
  body_add_par("R logo: ", style = "Normal") %>%
  slip_in_img(src = img.file, style = "strong", 
              width = .3, height = .3, pos = "after") %>% 
  slip_in_text(" - This is ", style = "strong", pos = "before") %>% 
  slip_in_seqfield(str = "SEQ Figure \u005C* ARABIC",
                   style = 'strong', pos = "before") %>% 
  print(target = "insert.docx")




----------------------------------------------
  
  
my_doc <- read_docx("Insert.docx") 
  
  
  
  
  



  
  
  my_doc <- read_docx() 
styles_info(my_doc)

balls <- tempfile(fileext = ".png")
png(filename = balls, width = 5, height = 6, units = 'in', res = 300)
barplot(xdf$Freq, col = c("red","green","blue","purple"),ylim=c(0,100),
        las=1,
        names.arg=xdf$Var1)
dev.off()

my_doc <- my_doc %>% 
  body_add_img(balls = balls, width = 5, height = 4, style = "centered")

print(my_doc, target = "first_example.docx")


==========================================




my_stssdoc <- read_docx() 
styles_info(my_stssdoc)

srcstss <- tempfile(fileext = ".png")
png(filename = srcstss, width = 5, height = 6, units = 'in', res = 300)
barplot(xdf$Freq, col = c("red","green","blue","purple"),ylim=c(0,100),
        las=1,
        names.arg=xdf$Var1)
dev.off()

my_stssdoc <- my_stssdoc %>% 
  body_add_img(srcstss = srcstss, width = 5, height = 6, style = "centered")

print(my_stssdoc, target = "first_stssexample.docx")


read_docx() %>% styles_info() %>% 
  subset( style_type %in% "paragraph" )




