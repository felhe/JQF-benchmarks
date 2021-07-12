# library
library(ggplot2)
library(grid)
library(dplyr)
library(data.table)
library(rlist)
library(stringr)
library(xts)
library(zoo)
library(Hmisc)
library(ggpubr)
library(lubridate)



Path_to_Table2_CSV  ="table2/bench_8m_25_comparepest.csv";

generateTable2Boxplot <- function(Path_to_Table2_CSV){
  
  performance_score = read.csv(file = Path_to_Table2_CSV)
  
  performance_score$concat_name = paste(performance_score$benchmark, performance_score$exception)
  
  performance_score$tool_f = factor(performance_score$tool)
  performance_score$concat_name_f = factor(performance_score$concat_name)
  
  max_x = max(performance_score$mtf)
  
  
  
  number_rows = nlevels(performance_score$concat_name_f)
  N_Guidance_V = nlevels(performance_score$tool_f)
  
  performance_score$x = 1;
  performance_score$y = max_x+2;
  
  xoffset = 60
  
  annotate_rel = character(nrow(performance_score));
  annotate_rel[1] = "Reliability:"
  
  performance_score$annotate_rel = annotate_rel
  
  performance_score$xreliab = performance_score$x
  
  performance_score$xreliab =  ifelse(performance_score$tool == 'zest',performance_score$xreliab + (N_Guidance_V-1) , performance_score$xreliab)
  performance_score$xreliab =  ifelse(performance_score$tool == 'pest2',performance_score$xreliab + (N_Guidance_V-2) , performance_score$xreliab)
  
  
  
  
  
  crash_names <- c(
    'ant java.lang.IllegalStateException' = "IllegalStateException",
    'closure java.lang.NullPointerException' = "NullPointerException",
    'rhino java.lang.NullPointerException' = "NullPointerException",
    'rhino java.lang.IllegalStateException' = "IllegalStateException",
    'bcel org.apache.bcel.classfile.ClassFormatException' = "ClassFormatException",
    'bcel org.apache.bcel.verifier.exc.AssertionViolatedException' = "AssertionViolatedException",
    'rhino java.lang.VerifyError' = "VerifyError",
    'rhino java.lang.ClassCastException'= "ClassCastException"
  )
  
  
  p  <- ggplot(performance_score,aes(fill =tool, y=mtf,x = tool ))+
    #geom_bar(position="dodge", stat="identity") +
    geom_boxplot()+
    #geom_dotplot(binaxis = 'y', dotsize = 0.3,stackdir = 'center')+
    facet_wrap(~concat_name ,strip.position = "top",nrow = number_rows,labeller = as_labeller(crash_names) )+
    theme(legend.position="top") +
    ylab("Mean time to Find in s")+
    xlab("")+
    scale_fill_brewer(palette = "Set1") +
    coord_flip(clip = "off",ylim = c(0,max_x),xlim = c(1,N_Guidance_V))+
    labs(fill ="Guidance Version")+
    theme(strip.text =  element_text(size = rel(1.3),face = "bold", colour = "gray39"),
          axis.text = element_text( size = 14 ),
          axis.text.x = element_text( size = 20 ),
          axis.text.y = element_text( size = 20 ),
          axis.title = element_text( size = 20, face = "bold" ),
          legend.title = element_text(size = 19, face = "bold" ),
          legend.text = element_text(size=19),
          plot.margin = unit(c(0.5,5,1,1), "cm")
    )+
    geom_text( # Class under Test
      data    = performance_score,
      mapping = aes(x = x, y = y, label = benchmark),
      alpha = 1,
      size = 20,
      colour = "gray50",
      hjust = 1
    )+
    geom_text( # reliability scores
      data    = performance_score,
      mapping = aes(x = xreliab, y = y+xoffset, label = round(repeatibility,2) ),
      alpha = 1,
      size = 10
    )+
    geom_text( #reliability label
      data = performance_score,
      colour = "black",
      mapping = aes(x = xreliab+1.9, y = y+xoffset, label = annotate_rel,fontface = "bold" ),
      size = 8
    ) 
  
  
  
  ggsave("table2Box.pdf", plot= p,device = "pdf",path = "imgs/",width = 30,height = 40, units = "cm",dpi = "print")
  
  ggsave("table2Box.jpg", plot= p,device = "jpg",path = "imgs/",width = 30,height = 40, units = "cm",dpi = "print")
}

generateTable2Barplot <- function(Path_to_Table2_CSV){
  
  performance_score = read.csv(file = Path_to_Table2_CSV)
  
  performance_score = aggregate(mtf~tool+exception+benchmark+repeatibility,performance_score,mean)
  
  
  performance_score$concat_name = paste(performance_score$benchmark, performance_score$exception)
  
  
  performance_score$tool_f = factor(performance_score$tool)
  performance_score$concat_name_f = factor(performance_score$concat_name)
  
  performance_score$concat_name_full = paste(performance_score$concat_name, performance_score$tool)
  max_x = max(performance_score$mtf)
  
  
  
  number_rows = nlevels(performance_score$concat_name_f)
  N_Guidance_V = nlevels(performance_score$tool_f)
  
  performance_score$x = 1;
  performance_score$y = max_x+2;
  
  xoffset = 30
  annotate_rel = character(nrow(performance_score));
  annotate_rel[1] = "Reliability:"
  
  performance_score$annotate_rel = annotate_rel
  
  performance_score$xreliab = performance_score$x
  
  performance_score$xreliab =  ifelse(performance_score$tool == 'zest',performance_score$xreliab + (N_Guidance_V-1) , performance_score$xreliab)
  performance_score$xreliab =  ifelse(performance_score$tool == 'pest2',performance_score$xreliab + (N_Guidance_V-2) , performance_score$xreliab)
  
  
  
  
  
  crash_names <- c(
    'ant java.lang.IllegalStateException' = "IllegalStateException",
    'closure java.lang.NullPointerException' = "NullPointerException",
    'rhino java.lang.NullPointerException' = "NullPointerException",
    'rhino java.lang.IllegalStateException' = "IllegalStateException",
    'bcel org.apache.bcel.classfile.ClassFormatException' = "ClassFormatException",
    'bcel org.apache.bcel.verifier.exc.AssertionViolatedException' = "AssertionViolatedException",
    'rhino java.lang.VerifyError' = "VerifyError",
    'rhino java.lang.ClassCastException'= "ClassCastException"
  )
  
  
  
  p  <- ggplot(performance_score,aes(fill =tool, y=mtf,x = tool ))+
    geom_bar(position="dodge", stat="identity") +
    #geom_boxplot()+
    #geom_dotplot(binaxis = 'y', dotsize = 0.3,stackdir = 'center')+
    facet_wrap(~concat_name ,strip.position = "top",nrow = number_rows,labeller = as_labeller(crash_names) )+
    theme(legend.position="top") +
    ylab("Mean time to Find in s")+
    xlab("")+
    scale_fill_brewer(palette = "Set1") +
    coord_flip(clip = "off",ylim = c(0,max_x),xlim = c(1,N_Guidance_V))+
    labs(fill ="Guidance Version")+
    theme(strip.text =  element_text(size = rel(1.3),face = "bold", colour = "gray39"),
          axis.text = element_text( size = 14 ),
          axis.text.x = element_text( size = 20 ),
          axis.text.y = element_text( size = 20 ),
          axis.title = element_text( size = 20, face = "bold" ),
          legend.title = element_text(size = 19, face = "bold" ),
          legend.text = element_text(size=19),
          plot.margin = unit(c(0.5,5,1,1), "cm")
    )+
    geom_text( # Class under Test
      data    = performance_score,
      mapping = aes(x = x, y = y, label = benchmark),
      alpha = 1,
      size = 20,
      colour = "gray50",
      hjust = 1
    )+
    geom_text( # reliability scores
      data    = performance_score,
      mapping = aes(x = xreliab, y = y+xoffset, label = round(repeatibility,2) ),
      alpha = 1,
      size = 10
    )+
    geom_text( #reliability label
      data = performance_score,
      colour = "black",
      mapping = aes(x = xreliab+17.9, y = y+xoffset, label = annotate_rel,fontface = "bold" ),
      size = 8
    ) 
  
  
  
  ggsave("table2Bar.pdf", plot= p,device = "pdf",path = "imgs/",width = 30,height = 40, units = "cm",dpi = "print")
  ggsave("table2Bar.jpg", plot= p,device = "jpg",path = "imgs/",width = 30,height = 40, units = "cm",dpi = "print")
}



generateTable2Boxplot( Path_to_Table2_CSV  )
generateTable2Barplot( Path_to_Table2_CSV  )


#read_Meas_Data <- function(mydir){

mydir = "results_8m_20_comp"

myfiles_pest2 <- list.files(path=mydir, pattern="viz.csv", full.names=TRUE,recursive = TRUE)


myfiles_pest_zest <- list.files(path=mydir, pattern="plot_data", full.names=TRUE,recursive = TRUE)



#myfiles_pest_zest <- myfiles_pest_zest[!str_detect(myfiles_pest_zest,pattern="pest2")]

colames_p_z <- c("unix_time", "cycles_done", "cur_path", "paths_total", "pending_total", "pending_favs", "map_size", "unique_crashes", "unique_hangs", "max_depth","execs_per_sec","valid_inputs", "invalid_inputs", "valid_cov")
colames_p2  <- c("execsPerSec", "saved_Inputs", "nonZeroCount", "nonZeroValidCount", "elapsedMilliseconds","null")


dat_z_p <- list()
dat_z_p = lapply(myfiles_pest_zest,function(x){
  if(!file.size(x)==0){
    y = read.table(x,header = FALSE,sep = ",",quote = "",fill=TRUE,col.names =colames_p_z)
  }
  y$path = x
  
  if(grepl("pest",x, fixed=TRUE)){
    y$guidance = "pest"
  }
  
  if(grepl( "pest2",x, fixed=TRUE)){
    y$guidance = "pest2"
  }
  
  if(grepl("zest",x, fixed=TRUE)){
    y$guidance = "zest"
  }
  
  if(grepl("ant",x, fixed=TRUE)){
    y$tool = "ant"
  }
  
  if(grepl("closure",x, fixed=TRUE)){
    y$tool = "closure"
  }
  
  if(grepl("rhino",x, fixed=TRUE)){
    y$tool = "rhino"
  }
  
  if(grepl("bcel",x, fixed=TRUE)){
    y$tool = "bcel"
  }
  
  if(grepl( "maven",x, fixed=TRUE)){
    y$tool = "maven"
  }
  
  rep_id <- gsub("-","",str_sub(x, start= -12,end = -11))
  
  y$repetition_id <- strtoi(rep_id, base = 0L)
  
  t_start <- y$unix_time[1]
  
  y$unix_time <- y$unix_time - t_start
  
 # y <- xts(y[,-1], order.by=as.POSIXct(y$unix_time, origin = '2021-07-15 13:00:00'))
  
#  y <- align.time( y[endpoints(y, "seconds", 1)], n=1 )
  
  y %>% 
    filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  
  


  
  return(y)
}
)





dat_p2 <- list()
dat_p2 = lapply(myfiles_pest2,function(x){
  if(!file.size(x)==0){
    y = read.table(x,header = TRUE,sep = ",",quote = "",fill=TRUE,col.names =colames_p2)
  }
  y$path = x
  
  
  y$elapsedMilliseconds <- gsub("m","M",y$elapsedMilliseconds)
  y$elapsedMilliseconds <- duration(y$elapsedMilliseconds)
  
  y$elapsedMilliseconds <- as.numeric(y$elapsedMilliseconds)
  
  

  y$cycles_done <- seq.int(nrow(y))

  
  if(grepl("pest",x, fixed=TRUE)){
    y$guidance = "pest"
  }
  
  if(grepl( "pest2",x, fixed=TRUE)){
    y$guidance = "pest2"
  }
  
  if(grepl("zest",x, fixed=TRUE)){
    y$guidance = "zest"
  }
  
  if(grepl("ant",x, fixed=TRUE)){
    y$tool = "ant"
  }
  
  if(grepl("closure",x, fixed=TRUE)){
    y$tool = "closure"
  }
  
  if(grepl("rhino",x, fixed=TRUE)){
    y$tool = "rhino"
  }
  
  if(grepl("bcel",x, fixed=TRUE)){
    y$tool = "bcel"
  }
  
  if(grepl( "maven",x, fixed=TRUE)){
    y$tool = "maven"
  }
  
  rep_id <- gsub("-","",str_sub(x, start= -11,end = -9))
  
  y$repetition_id <- strtoi(rep_id, base = 0L)
  
  #t_start <- y$unix_time[1]
  
  #y$unix_time <- y$unix_time - t_start
  
  y$valid_cov <- NULL
  y$map_size <- NULL
  
  return(y)
}
)





table_p2 <- bind_rows(dat_p2)



table_z_p <- bind_rows(dat_z_p)




h = 30
w = 50


label_leg = c("pest/performances-core","pest/input","zest")

valid_inputs <-  ggplot(table_z_p,aes( y=valid_inputs,x = unix_time,color = guidance ))+
  geom_smooth(na.rm = TRUE)+
  theme(legend.position="top") +
  facet_wrap(~tool,nrow = 1)+
  ylab("valid Inputs")+
  xlab("time in s")+
  scale_colour_discrete(name = "Guidance Version", labels = label_leg)



unique_crashes <- ggplot(table_z_p,aes( y=unique_crashes,x = unix_time,color = guidance ))+
  geom_smooth(na.rm = TRUE)+
  theme(legend.position="top") +
  facet_wrap(~tool,nrow = 1)+
  ylab("unique crashes")+
  xlab("time in s")+
  scale_colour_discrete(name = "Guidance Version", labels = label_leg)
    #scale_fill_brewer(palette = "Set1") 
  

execs_per_sec <- ggplot(table_z_p,aes( y=execs_per_sec,x = unix_time,color = guidance ))+
  geom_smooth(na.rm = TRUE)+
  facet_wrap(~tool,nrow = 1)+
  theme(legend.position="top") +
  ylab("executions per second")+
  xlab("time in s")+
  scale_colour_discrete(name = "Guidance Version", labels = label_leg)
  #scale_fill_brewer(palette = "Set1") 




paths_total <- ggplot(table_z_p,aes( y=paths_total,x = unix_time,color = guidance ))+
  geom_smooth(na.rm = TRUE)+
  theme(legend.position="top") +
  facet_wrap(~tool,nrow = 1)+
  ylab("paths total")+
  xlab("time in s")+
  scale_colour_discrete(name = "Guidance Version", labels = label_leg)
  #scale_fill_brewer(palette = "Set1") 

cycles_done <- ggplot(table_z_p,aes( y=cycles_done,x = unix_time,color = guidance ))+
  geom_smooth(na.rm = TRUE,data =subset(table_z_p, guidance != "pest2"))+
  geom_smooth(data = table_p2,aes(x = elapsedMilliseconds, y= cycles_done,color=guidance ))+
  facet_wrap(~tool,nrow = 1)+
  theme(legend.position="top") +
  ylab("cycles done")+
  xlab("time in s")+
  scale_colour_discrete(name = "Guidance Version", labels = label_leg)
  #scale_fill_brewer(palette = "Set1") 



majorPlot <- ggarrange(valid_inputs, unique_crashes,execs_per_sec,paths_total,cycles_done,
          labels = c("A", "B", "C","D","E"),
          ncol = 1, nrow = 5)

ggsave("all.pdf", plot= majorPlot,device = "pdf",path = "imgs/",width = 50,height = 40, units = "cm",dpi = "print")
ggsave("all.jpg", plot= majorPlot,device = "jpg",path = "imgs/",width = 50,height = 40, units = "cm",dpi = "print")



saved_inputs <- ggplot(table_p2,aes( y=saved_Inputs,x = elapsedMilliseconds,color = guidance ))+
  geom_smooth(na.rm = TRUE)+
  facet_wrap(~tool,nrow = 1)+
  ylab("queue length")+
  xlab("time in s")+
  theme(legend.position="top") +
  scale_colour_discrete(name = "Guidance Version", labels = label_leg)
#scale_fill_brewer(palette = "Set1") 



# Plots for Report

# input queue
ggsave("saved_inputs.pdf", plot= saved_inputs,device = "pdf",path = "imgs/",width = 30,height = 10, units = "cm",dpi = "print")
ggsave("saved_inputs.jpg", plot= saved_inputs,device = "jpg",path = "imgs/",width = 30,height = 10, units = "cm",dpi = "print")

#execs per second
ggsave("execs_per_sec.pdf", plot= execs_per_sec,device = "pdf",path = "imgs/",width = 30,height = 10, units = "cm",dpi = "print")
ggsave("execs_per_sec.jpg", plot= execs_per_sec,device = "jpg",path = "imgs/",width = 30,height = 10, units = "cm",dpi = "print")


#execs per second
ggsave("unique_crashes.pdf", plot= unique_crashes,device = "pdf",path = "imgs/",width = 30,height = 10, units = "cm",dpi = "print")
ggsave("unique_crashes.jpg", plot= unique_crashes,device = "jpg",path = "imgs/",width = 30,height = 10, units = "cm",dpi = "print")


#valid inputs
ggsave("valid_inputs.pdf", plot= valid_inputs,device = "pdf",path = "imgs/",width = 30,height = 10, units = "cm",dpi = "print")
ggsave("valid_inputs.jpg", plot= valid_inputs,device = "jpg",path = "imgs/",width = 30,height = 10, units = "cm",dpi = "print")


#return(dat_txt)
#}





