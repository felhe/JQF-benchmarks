# library
library(ggplot2)
library(grid)



generateTable2 <- function(Path_to_Table2_CSV){

performance_score = read.csv(file = Path_to_Table2_CSV)

performance_score$x = 1;
performance_score$y = 150;

performance_score$concat_name = paste(performance_score$benchmark, performance_score$exception)

crash_names <- c(
  'ant java.lang.IllegalStateException' = "IllegalStateException",
  'closure java.lang.NullPointerException' = "NullPointerException",
  'rhino java.lang.NullPointerException' = "NullPointerException",
  'rhino java.lang.IllegalStateException' = "IllegalStateException",
  'bcel org.apache.bcel.classfile.ClassFormatException' = "ClassFormatException",
  'bcel org.apache.bcel.verifier.exc.AssertionViolatedException' = "AssertionViolatedException",
  'rhino java.lang.VerifyError' = "VerifyError"
)


p  <- ggplot(performance_score,aes(fill =tool, y=mtf,x = tool ))+
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~concat_name ,strip.position = "left",nrow = 7,labeller = as_labeller(crash_names)) +
  theme(legend.position="none") +
  ylab("Mean time to Find in s")+
  xlab("Guidance Version")+
  scale_fill_brewer(palette = "Set1") +
  coord_flip()+
  theme(strip.text =  element_text(size = rel(1),face = "bold", colour = "gray39"),
        axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 20 ),
        axis.title = element_text( size = 16, face = "bold" ))+
  geom_text(
   data    = performance_score,
  mapping = aes(x = x, y = y, label = benchmark),
  alpha = 0.1,
  size = 20)

ggsave("table2.pdf", plot= p,device = "pdf",path = "imgs/",width = 30,height = 40, units = "cm",dpi = "print")
}



generateTable2( "table2/bench_performance_score.csv" )