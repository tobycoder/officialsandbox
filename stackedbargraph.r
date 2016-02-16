#4763
statement_28 = read.csv2("statement_28_new.csv")
head(statement_28)

# write a simple function to add footnote
makeFootnote <- function(footnoteText =
                           format(Sys.time(), "%d %b %Y"),
                         size = .7, color = grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = size, col = color))
  popViewport()
}

#statement_28$us <- factor(statement_28$us , levels=statement_28$us) #unique(as.character(
#statement_28$Var1 <- factor(statement_28$Var1 , levels= statement_28$Var1)
#statement_28 = data.frame(statement_28)
color = c("#36d861", "#b0ffa4", "#ffffff", "#ff9393", "#ff3d3d" )

## zet de factor levels van Var1 op de gewenste volgorder, en sorteer de data hierop
statement_28$Var1 = factor(as.character(statement_28$Var1), levels = c("Strongly Agree", "Agree", "No Opninion", "Disagree", "Strongly Disagree"))
statement_28 = statement_28[order(statement_28$Var1),]

library(ggplot2)

size = 8 # hiermee kun je in 1 keer de width, height en res in dezelfde mate vergroten/verkleinen
png(file="statement28.png", width=700*size, height = 500*size, res = 75*size)

ggplot(statement_28, aes(x = us, y = per, fill = Var1, order = us)) + 
  geom_bar(stat = "identity", width = 0.3, color='grey') +
  coord_flip() + 
  scale_fill_manual(values = color) +
  #scale_y_continuous(labels = scales::comma) +
 ggtitle("All semi-automatic weapons should be banned \n") +
  xlab("Candidate Preference") +
  ylab("Agreement %") +
  theme(plot.title = element_text(size = 20, margin = margin(20,0,0,0)), 
        axis.title.y = element_text(size = 10, margin = margin(0,15,0,0)),
        axis.title.x = element_text(size = 10, margin = margin(15,0,0,0)),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key=element_rect(size=2),
        legend.key.size = unit(1, "lines")) +
  scale_x_discrete(limits = c("Cruz", "Trump", "Rubio", "Carson", "Bush", "Kasich", "Sanders", "Clinton"))

c("Clinton", "Sanders", "Kasich", "Bush", "Trump", "Carson", "Rubio","Cruz")
makeFootnote("Number of respondents: 4838", color = "black")
dev.off()











