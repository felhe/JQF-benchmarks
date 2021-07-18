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
library(cowplot)






## Generate Table simmilar to Table 2 in ZestPaper
generateTable2Boxplot <- function(Path_to_Table2_CSV){
  
  performance_score = read.csv(file = Path_to_Table2_CSV)
  
  performance_score$concat_name = paste(performance_score$benchmark, performance_score$exception)
  
  performance_score$tool_f = factor(performance_score$tool)
  performance_score$concat_name_f = factor(performance_score$concat_name)
  
  max_x = 400 #max(performance_score$mtf)
  
  
  
  number_rows = nlevels(performance_score$concat_name_f)
  N_Guidance_V = nlevels(performance_score$tool_f)
  
  performance_score$x = 1;
  performance_score$y =  max_x+10;
  
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
    'closure java.lang.RuntimeException' = "RuntimeException",
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
    scale_fill_brewer(palette = "Set2",labels = c("pest/perf.-score","pest/input","zest")) +
    scale_x_discrete( labels = c("pest" = "perf.-score","zest" = "zest", "pest2" = "input"))+
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
      mapping = aes(x = x+0.2, y = y, label = benchmark),
      alpha = 1,
      size = 17,
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


## generates Barplot from the same Data
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
    scale_x_discrete( labels = c("pest" = "perf.-score","zest" = "zest", "pest2" = "input"))+
    theme(legend.position="top") +
    ylab("Mean time to Find in s")+
    xlab("")+
    scale_fill_brewer(palette = "Set2",labels = c("pest/perf.-score","pest/input","zest")) +
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
      mapping = aes(x = x+0.2, y = y, label = benchmark),
      alpha = 1,
      size = 17,
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
      mapping = aes(x = xreliab+19.9, y = y+xoffset, label = annotate_rel,fontface = "bold" ),
      size = 8
    ) 
  
  
  
  ggsave("table2Bar.pdf", plot= p,device = "pdf",path = "imgs/",width = 30,height = 40, units = "cm",dpi = "print")
  ggsave("table2Bar.jpg", plot= p,device = "jpg",path = "imgs/",width = 30,height = 40, units = "cm",dpi = "print")
}



# looks for "plot_data" files in subdirectorys and reads all of them 
# adds $tool $guidance and $repetition_id collumn 
# unix_time collumn is also ajusted to start at 0 for each run
read_Meas_p_z_p2Data <- function(mydir){


myfiles_pest_zest <- list.files(path=mydir, pattern="plot_data", full.names=TRUE,recursive = TRUE)



#myfiles_pest_zest <- myfiles_pest_zest[!str_detect(myfiles_pest_zest,pattern="pest2")]

colames_p_z <- c("unix_time", "cycles_done", "cur_path", "paths_total", "pending_total", "pending_favs", "map_size", "unique_crashes", "unique_hangs", "max_depth","execs_per_sec","valid_inputs", "invalid_inputs", "valid_cov")


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














table_z_p <- bind_rows(dat_z_p)


return(table_z_p)
}


# looks for "viz.csv" files in subdirectorys and reads all of them 
read_Meas_p2_Data <- function(mydir){

  myfiles_pest2 <- list.files(path=mydir, pattern="viz.csv", full.names=TRUE,recursive = TRUE)
  
  colames_p2  <- c("execsPerSec", "saved_Inputs", "nonZeroCount", "nonZeroValidCount", "elapsedMilliseconds","null")
  
  
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
  
  return(table_p2)
}




#prints plot to /imgs diras .jpg and .pdf  with height and width in cm 
print_plot <- function(gg_plot,file_str,h,w){
  ggsave(paste(file_str,".pdf"), plot= gg_plot,device = "pdf",path = "imgs/",width = w,height = h, units = "cm",dpi = "print")
  ggsave(paste(file_str,".jpg"), plot= gg_plot,device = "jpg",path = "imgs/",width = w,height = h, units = "cm",dpi = "print")
  
}


# plots a var of the table over time  for each fuzzed tool
plot_data_Variable <- function(data_table,time_interval,colName,ylabel ,scale,leglab, rows_number){
  
  
  aggregated_tab <- data_table                        %>% 
    select(guidance,unix_time,tool,.data[[colName]])  %>%
    group_by(guidance,tool,unix_time,)                %>%
    mutate(G = floor(unix_time/time_interval))        %>%
    mutate(unix_time = G * time_interval)             %>%
    select(guidance,unix_time,tool,.data[[colName]])  %>%
    summarise(mean = mean(.data[[colName]]),
              sd = sd(.data[[colName]]),
              max = mean + sd,
              min = mean -sd,
              N = n())
  var_plot <-  aggregated_tab %>%  ggplot(aes(y = mean ,x = unix_time, ymin = min,ymax = max,color = guidance))+
    facet_wrap(~tool,nrow = rows_number,scales = scale)+
    scale_color_brewer(palette="Set2",labels= leglab,name = "Guidance")+
    geom_ribbon( alpha = 0.1)+
    geom_line(size = 1.1)+ 
    theme_bw()+
    theme(legend.position="right") +
    ylab(ylabel)+
    xlab("time in s")
  
  return(var_plot)
}


# plots a var of the table over time  for each fuzzed tool
plot_data_Variable_omit_pest2 <- function(data_table,data_table2,time_interval,colName,colName2,ylabel ,scale,leglab,rows_number){
  
  
  

  data_table <- subset(table_z_p, guidance != "pest2")    
  
  aggregated_tab <- data_table                        %>% 
    select(guidance,unix_time,tool,.data[[colName]])  %>%
    group_by(guidance,tool,unix_time,)                %>%
    mutate(G = floor(unix_time/time_interval))        %>%
    mutate(unix_time = G * time_interval)             %>%
    select(guidance,unix_time,tool,.data[[colName]])  %>%
    summarise(mean = mean(.data[[colName]]),
              sd = sd(.data[[colName]]),
              max = mean + sd,
              min = mean -sd,
              N = n())
  
  
  aggregated_tab2 <- data_table2                       %>% 
    select(guidance,elapsedMilliseconds,tool,.data[[colName2]])  %>%
    group_by(guidance,tool,elapsedMilliseconds,)                %>%
    mutate(G = floor(elapsedMilliseconds/time_interval))        %>%
    mutate(elapsedMilliseconds = G * time_interval)             %>%
    select(guidance,elapsedMilliseconds,tool,.data[[colName2]])  %>%
    summarise(mean = mean(.data[[colName2]]),
              sd = sd(.data[[colName2]]),
              max = mean + sd,
              min = mean -sd,
              N = n())
  
  var_plot <-  aggregated_tab %>%  ggplot(aes(y = mean ,x = unix_time, ymin = min,ymax = max,color = guidance))+
    facet_wrap(~tool,nrow = rows_number,scales = scale)+
    scale_color_brewer(palette="Set2",labels= leglab,name = "Guidance")+
    geom_ribbon( alpha = 0.1)+
    geom_smooth(data = aggregated_tab2,aes(y = mean ,x = elapsedMilliseconds, ymin = min,ymax = max,color = guidance ))+
    geom_line(size = 1.1)+ 
    theme_bw()+
    theme(legend.position="right") +
    ylab(ylabel)+
    xlab("time in s")
  
  return(var_plot)
}





##################################################################################################################################
##################################################################################################################################
##################################################################################################################################



## specify path to .csv file for Table 2 generation
Path_to_Table2_CSV_bar  ="results_main_bench/figures/Table_2_bar.csv";
Path_to_Table2_CSV_box  ="results_main_bench/figures/Table_2_box.csv";

## Directory with benchmark rawdata
path_bench_data = "results_main_bench"



generateTable2Boxplot( Path_to_Table2_CSV_box  )
generateTable2Barplot( Path_to_Table2_CSV_bar  )







### Generate Table 2
table_z_p <- read_Meas_p_z_p2Data(path_bench_data)
table_p2 <- read_Meas_p2_Data(path_bench_data)








## Labels for Legend 
label_leg = c("pest/performance-score","pest/input","zest")

## x axis tick labels for some plots
x_tic_labs =  c("pest" = "perf.-score","zest" = "zest", "pest2" = "input")



## NUmber of rows in undividual Plot (set to 1 for correct all.pdf/all.jpg printing)
number_of_rows <- 1


h = 15
w = 60



paths_total_vec <- c("paths_total","paths totoal","free")

cycles_done_vec <- c("cycles_done","cycles done","free")

execs_per_sec_vec <-c("execs_per_sec","executions per second","free")

unique_crashes_vec <-c("unique_crashes","unique crashes","free")

valid_inputs <-c("valid_inputs","valid inputs","free")

invalid_inputs <-c("invalid_inputs","invalid inputs","free")


var_list <- list(cycles_done_vec,
                 unique_crashes_vec,
                 valid_inputs,
                 invalid_inputs,
                 execs_per_sec_vec)


plot_list <- list()
i <- 1

for (var in var_list){
  plotted_var <- plot_data_Variable(table_z_p,100,var[1],var[2],var[3],label_leg,number_of_rows)  
  print_plot(plotted_var,var[1],h,w)
  
  plot_list[[i]] <- plotted_var
  i <- i+1
  
}





############# Execs per Second VIOLIN PLOT ###########################
aggregated_exec_per_sec   <- table_z_p                %>%

  select(guidance,unix_time,tool,execs_per_sec)     %>%
  group_by(guidance,tool)      

plot_list[[length(plot_list)+1]] <- 
  ggplot(data = aggregated_exec_per_sec,aes(x = guidance, y = execs_per_sec, group = factor(guidance), color = factor(guidance),fill = factor(guidance) ))+
  facet_wrap(~tool,scales = "free",nrow = number_of_rows)+
  geom_violin(trim = "true")+
  ylab("executions per second")+
  scale_x_discrete( labels =x_tic_labs)+
  theme_bw()+
  geom_boxplot(width=0.1,fill = "white")+
  scale_color_brewer(palette="Set2",labels= label_leg,name = "Guidance")+
  scale_fill_brewer(palette="Set2",labels= label_leg,name = "Guidance")

print_plot(plot_list[[length(plot_list)]],"execs_per_sec_violin",h,w)

 aggregated_exec_per_sec <- aggregated_exec_per_sec %>% summarise(mean_exec_per_sec = mean(execs_per_sec),
                                     median = median(execs_per_sec),
                                     standard_dev = sd(execs_per_sec))



write.csv(aggregated_exec_per_sec,"imgs/exexcs_per_sec.csv", row.names = FALSE)


## saved inputs #####################################
plot_list[[length(plot_list)+1]] <- plot_data_Variable_omit_pest2(table_z_p,table_p2,20,"paths_total","saved_Inputs","Inputs saved","free",label_leg,number_of_rows)

print_plot(plot_list[[length(plot_list)]],"saved_Inputs",h,w)






major <- plot_grid( plotlist = plot_list,rows = length(plot_list),align = "hv" )




##### Arrange all plots in list in one PDF/JPG ##########################

ggsave("all.pdf", plot= major,device = "pdf",path = "imgs/",width = 50,height = 50, units = "cm",dpi = "print")
ggsave("all.jpg", plot= major,device = "jpg",path = "imgs/",width = 50,height = 50, units = "cm",dpi = "print")

      
 









cycles_done <- ggplot(table_z_p,aes( y=cycles_done,x = unix_time,color = guidance ))+
  geom_smooth(na.rm = TRUE,data =subset(table_z_p, guidance != "pest2"))+
  geom_smooth(data = table_p2,aes(x = elapsedMilliseconds, y= cycles_done,color=guidance ))+
  facet_wrap(~tool,nrow = 1)+
  theme(legend.position="top") +
  ylab("cycles done")+
  xlab("time in s")+
  scale_colour_discrete(name = "Guidance Version", labels = label_leg)
#scale_fill_brewer(palette = "Set1") 





saved_inputs <- ggplot(table_p2,aes( y=saved_Inputs,x = elapsedMilliseconds,color = guidance ))+
  geom_smooth(na.rm = TRUE)+
  facet_wrap(~tool,nrow = 1)+
  ylab("queue length")+
  xlab("time in s")+
  theme(legend.position="top") +
  scale_colour_discrete(name = "Guidance Version", labels = label_leg)
#scale_fill_brewer(palette = "Set1") 




