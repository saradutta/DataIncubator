###############################
#### Load pdfs of pharmaceutical companies
#### Look at occurrence of specific words
#### Created 05/01/17 by Sara Dutta
################################

require(pdftools)
require(ggplot2)
require(tidyr)
require(corrplot)

#--- clear workspace
rm(list=ls())

#--- download files
download.file("https://annualreport.gsk.com/assets/downloads/1_GSK.AR.FULL.V4.pdf",
              "GSK-2016-R.pdf",mode="wb")
download.file("https://jnj.brightspotcdn.com/88/3f/b666368546bcab9fd520594a6016/2017-0310-ar-bookmarked.pdf",
              "Johnson-2016-R.pdf",mode="wb")
download.file("https://www.takeda.com/siteassets/system/investors/report/annual-reports/ar2016_en.pdf",
              "Takeda-2016-R.pdf",mode="wb")
download.file("http://files.shareholder.com/downloads/LLY/4396906919x0x933961/450B26F2-F56C-44B0-8AEB-3E15E98D24D8/",
              "Lilly-2016-R.pdf",mode="wb")
download.file("http://s21.q4cdn.com/957025354/files/doc_downloads/BMS-2016-AR.pdf",
              "BMS-2016-AR-R.pdf", mode = "wb")

#---- load data
file_names<-c("GSK-2016-R.pdf","Johnson-2016-R.pdf","BMS-2016-AR-R.pdf",
              "Takeda-2016-R.pdf","Lilly-2016-R.pdf")
company_names<-unlist(lapply(strsplit(file_names,'-'),function(x) x[1]))
all_txt<-lapply(file_names,pdf_text)
#--- calculate total number of words
num_words <- unlist(lapply(all_txt,function(x) length(unlist(strsplit(x, "\\W")))))
#---- see how many times specific words are mentioned
words<-c("oncology","patients","cancer","revenue","pediatric")
results<-as.data.frame(matrix(0,length(all_txt),length(words)))
for(i in 1:length(words)){
  results[,i]<-unlist(lapply(all_txt,function(x) length(grep(words[i],x))))/num_words
}
colnames(results)<-words
results<-cbind(company=company_names,results)

#---- convert to long format and plot
results_long<-gather(results,word,occurrence,-company)
ggplot(data=results_long,aes(x=company,y=occurrence,colour=word))+geom_point()
ggsave('Frequency_per_company.png')
ggplot(data=results_long,aes(x=word,y=occurrence,colour=company))+geom_point()
ggsave('Frequency_per_word.png')

#---- calculate and plot correlation matrix
corr_res<-cor(results[2:6])
png(height=1200, width=1200, pointsize=25, file="CorrelationMatrix.png")
corrplot(corr_res, method="number")
dev.off()



