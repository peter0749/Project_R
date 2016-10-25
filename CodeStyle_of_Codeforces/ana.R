rm(list=ls(all=TRUE))
library(NLP)
library(tm)
f2n <- function(X){ as.numeric(levels(X)[X]) } 
member = readRDS('member.RDS')
id_list = readRDS("id_list.RDS")
con_list = readRDS('con_list.RDS')
dir_prefix = './D_CODE'
dir_range = 1:nrow(id_list)
style_data = data.frame()
for(i in dir_range)
{
  totalSrc = Corpus(DirSource(paste(dir_prefix,member[i,1],sep='/')), list(language=NA))
  totalSrc = matrix(unlist(totalSrc), nrow=length(totalSrc))
  sig_newline = sum(grepl(pattern = '^\\{',x = totalSrc))
  sig_right = sum(grepl(pattern='.\\{',x=totalSrc))
  sig_newline_e = sum(grepl(pattern = '^\\{$',x = totalSrc))
  sig_right_e = sum(grepl(pattern='.\\{$',x=totalSrc))
  style_data = rbind(style_data, cbind(sig_newline,sig_newline_e,sig_right,sig_right_e))
}
style_vs_score = cbind(member[1:nrow(style_data),],style_data)

