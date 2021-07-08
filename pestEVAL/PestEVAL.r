# library
library(ggplot2)
library(grid)

Path_to_Table2_CSV  ="table2/bench_performance_score_40m_10_short.csv";

#generateTable2 <- function(Path_to_Table2_CSV){

performance_score = read.csv(file = Path_to_Table2_CSV)

number_rows = ceiling(nrow(performance_score)/2)

performance_score$x = 1;
performance_score$y = 265;

annotate_rel = character(nrow(performance_score));
annotate_rel[1] = "Reliability:"

performance_score$annotate_rel = annotate_rel

performance_score$xreliab = performance_score$x

performance_score$xreliab =  ifelse(performance_score$tool == 'zest',performance_score$xreliab + 1 , performance_score$xreliab)


performance_score$concat_name = paste(performance_score$benchmark, performance_score$exception)



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
  facet_wrap(~concat_name ,strip.position = "top",nrow = number_rows,labeller = as_labeller(crash_names) )+
  theme(legend.position="top") +
  ylab("Mean time to Find in s")+
  xlab("")+
  scale_fill_brewer(palette = "Set1") +
  coord_flip(clip = "off",ylim = c(0,300),xlim = c(1,2))+
  labs(fill ="Guidance Version")+
  theme(strip.text =  element_text(size = rel(1.3),face = "bold", colour = "gray39"),
        axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 20 ),
        axis.text.y = element_text( size = 20 ),
        axis.title = element_text( size = 16, face = "bold" ),
        legend.title = element_text(size = 19, face = "bold" ),
        legend.text = element_text(size=19),
        plot.margin = unit(c(0.5,5,1,1), "cm")
  )+
  geom_text(
  data    = performance_score,
  mapping = aes(x = x, y = y, label = benchmark),
  alpha = 1,
  size = 20,
  colour = "gray50")+
  geom_text(
    data    = performance_score,
    mapping = aes(x = xreliab, y = y+85, label = repeatibility ),
    alpha = 1,
    size = 10)+
  geom_text(
            data = performance_score,
            colour = "black",
            mapping = aes(x = xreliab+1.6, y = y+85, label = annotate_rel,fontface = "bold" ),
            size = 8
            ) 



ggsave("table2.pdf", plot= p,device = "pdf",path = "imgs/",width = 30,height = 40, units = "cm",dpi = "print")
#}



#generateTable2( Path_to_Table2_CSV  )