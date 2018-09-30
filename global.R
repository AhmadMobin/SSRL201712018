library(shiny)
library(shinydashboard)
library(RCurl) #package for the get URL function 
library(plotly)
library(RColorBrewer) #colors for plot_ly
library(ggplot2)
library(dplyr)
library(grid) #adding text to ggplots 
library(DT) #datatables
library(ggvis)
library(ndtv)
library(network)


##### TOTAL RESEARCH STUDIES
Fig1<-read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/Fig1.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")


#Changing the levels so most recent year is displayed first
Fig1$Year<-as.factor(Fig1$Year)

test<- ggplot(Fig1, aes(x= Year, y=Count, fill= Status)) +
  geom_bar(stat= "identity", position="stack", width= 0.5)+
  scale_fill_manual(values=c('#420c00', '#354377'))+
  #labs(caption= "Figure 1- Total Research Studies")+
  #ggtitle("Total Research Studies") + #MAIN TITLE
  #theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=20, hjust=1.0, vjust=-0.5))+
  xlab(NULL)+ #X-axis label 
  theme (axis.title.x=element_text (angle=0, size=18, face="bold", color="black"))+  #Formating title of x-axis
  theme (axis.text.x=element_text(angle=0, size=12, vjust=90))+ 
  theme (axis.text.y=element_text(angle=0, size=10))+
  ylab (NULL) + #no title for y-axis
  #geom_text(aes(Year, total, label = total, fill = NULL, vjust=-0.5,  size=20), data = totals)+
  #annotate("text", label= "39", fontface="bold", x=1, y=39)+
  #annotate("text", label= "92", x=2, y=92)+
  #annotate("text", label= "156", x=3, y=156)+
  #annotate("text", label= "208", x=4, y=208)+
  #annotate("text", label= "299", x=5, y=299)+
  theme (panel.grid.minor=element_blank(), #Hiding the minor gridlines
         panel.grid.major=element_blank()) + #Hiding the major gridlines
  theme (plot.background=element_rect(fill='white'))+ #changes the plot background (not the panel) colour
  theme (panel.background=element_rect (fill='white'))+
  theme (axis.line.x=element_line(color="black", size=1))+ #black line on x-axis
  theme (axis.line.y=element_line(color="black", size=1))+
  scale_y_continuous (breaks=seq(0,350, by=50), expand=c(0,0))+ #setting custom y-axis breaks & also expand=c removes space btwn bars and x-axis
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0,350))+ #zooms into the y-axis at 0 to 600 
  theme(legend.title=element_blank(), legend.position="bottom") #removes title from legend
#####

##### STUDENT ENGAGEMENT

Fig2<- read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/StudentEngagement.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
#removing Post Docs
Fig2<- Fig2 %>%  filter(Student!="Post Doctoral Fellows")
#ordering Factor levels
Fig2$Student<- as.factor(Fig2$Student)
Fig2$Student<- factor(Fig2$Student,
                      levels=c(#"Post Doctoral Fellows",
                               "Doctoral Students",
                               "Master's Students",
                               "Undergraduate Students"))

#Changing the levels so most recent year is displayed first
Fig2$Year<-as.factor(Fig2$Year)



test1<- ggplot(Fig2, aes(x= Year, y=Count, fill= Student)) +
  geom_bar(stat= "identity", position="stack", width= 0.5)+
  scale_fill_manual(values=c(#'#050000',#black: Post Docs
                             '#c8d60e',#yellow PhD
                             '#420c00',#red Master's
                             '#354377'))+#blue Undergrad
                   
  #labs(caption= "Figure 1- Total Research Studies")+
  #ggtitle("Total Research Studies") + #MAIN TITLE
  #theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=20, hjust=1.0, vjust=-0.5))+
  xlab(NULL)+ #X-axis label 
  theme (axis.title.x=element_text (angle=0, size=18, face="bold", color="black"))+  #Formating title of x-axis
  theme (axis.text.x=element_text(angle=0, size=12, vjust=90))+ 
  theme (axis.text.y=element_text(angle=0, size=10))+
  ylab (NULL) + #no title for y-axis
  #geom_text(aes(Year, total, label = total, fill = NULL, vjust=-0.5,  size=20), data = totals)+
  #annotate("text", label= "328", x=1, y=328)+
  #annotate("text", label= "372", x=2, y=372)+
  #annotate("text", label= "274", x=3, y=274)+
  #annotate("text", label= "638", x=4, y=638)+
  #annotate("text", label= "753", x=5, y=753)+
  theme (panel.grid.minor=element_blank(), #Hiding the minor gridlines
         panel.grid.major=element_blank()) + #Hiding the major gridlines
  theme (plot.background=element_rect(fill='white'))+ #changes the plot background (not the panel) colour
  theme (panel.background=element_rect (fill='white'))+
  theme (axis.line.x=element_line(color="black", size=1))+ #black line on x-axis
  theme (axis.line.y=element_line(color="black", size=1))+
  scale_y_continuous (breaks=seq(0,800, by=50), expand=c(0,0))+ #setting custom y-axis breaks & also expand=c removes space btwn bars and x-axis
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0,800))+ #zooms into the y-axis at 0 to 800 
  theme(legend.title=element_blank(), legend.position="bottom")

##### 
#COLABORATIONS
#Table 1- Number of Research Studies By Lab
Table1<-read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/Table1_ResearchStudiesByLab.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

#Renaming Columns
colnames(Table1)[2]<- "2012-2013"
colnames(Table1)[3]<- "2013-2014"
colnames(Table1)[4]<- "2014-2015"
colnames(Table1)[5]<- "2015-2016"
colnames(Table1)[6]<- "2016-2017"
colnames(Table1)[7]<- "2017-2018"

#Creating Hyperlinks
Links<- c("ssrl.usask.ca/laboratories/col.php", # COL
          "ssrl.usask.ca/laboratories/edl.php", #EDL
          "ssrl.usask.ca/laboratories/ehl.php", #EHL
          "ssrl.usask.ca/laboratories/qrl.php", #QRL
          "ssrl.usask.ca/laboratories/safihr.php", #Spatial
          "ssrl.usask.ca/laboratories/sgal.php", #SGAL
          "ssrl.usask.ca/laboratories/snl.php", #SNL
          "ssrl.usask.ca/laboratories/vital.php", #Vital
          "-" #Multi Lab Collab 
          ) 
Table1$Links<-Links
#making hyperlinks in data table
Table1$Links <- sapply(Table1$Links, function(x) 
  toString(tags$a(href=paste0("http://", x), x)))

#Arraging Labs by alphabetically
Table1<- Table1 %>% arrange(Lab)

#TABLE 1 A- Total number of research studies
Year_1<-39
Year_2<-92
Year_3<-156
Year_4<-208
Year_5<-299
Year_6<- 339
Lab<-"TOTAL RESEARCH STUDIES"

Table1a<-cbind(Lab, 
               Year_1, 
               Year_2, 
               Year_3, 
               Year_4, 
               Year_5, 
               Year_6)
Table1a<- as.data.frame(Table1a)
#Renaming columns
colnames(Table1a)[1]<- "Summary"
colnames(Table1a)[2]<- "2012-2013"
colnames(Table1a)[3]<- "2013-2014"
colnames(Table1a)[4]<- "2014-2015"
colnames(Table1a)[5]<- "2015-2016"
colnames(Table1a)[6]<- "2016-2017"
colnames(Table1a)[7]<- "2017-2018"
#####
#Figure 2017-2018: Collaborative Studies
CollabStudies20172018<-read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/CollabStudies20172018.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")


#Renaming Columns
colnames(CollabStudies20172018)[2]<- "Count"
#FOR PLOT_LY GRAPH
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#070707"
)
x <- list(
  title = "Number of Studies",
  titlefont = f
)
y <- list(
  title = "",
  titlefont = f
)
#####
#Table 3- Number of Research By Origin
Table3<-read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/Table3_ResearcherByOrigin.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
Table3<- Table3 %>%  arrange(Lab)
#Renaming Columns
colnames(Table3)[1]<-"Affiliated University of Saskatchewan Colleges, Schools and Supporting Units"
colnames(Table3)[2]<- "2012-2013"
colnames(Table3)[3]<- "2013-2014"
colnames(Table3)[4]<- "2014-2015"
colnames(Table3)[5]<- "2015-2016"
colnames(Table3)[6]<- "2016-2017"
colnames(Table3)[7]<- "2017-2018"

#Replacing "--" to blanks
Table3[Table3=="--"]<-NA

#SUMMARY TABLE OF TABLE 3
Table3a<-read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/Table3a_Summary.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

colnames(Table3a)[2]<- "2012-2013"
colnames(Table3a)[3]<- "2013-2014"
colnames(Table3a)[4]<- "2014-2015"
colnames(Table3a)[5]<- "2015-2016"
colnames(Table3a)[6]<- "2016-2017"
colnames(Table3a)[7]<- "2017-2018"


#####

#Research Outputs

Fig3<-read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/ResearchOutputs.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

colnames(Fig3)[1]<- "Year"
colnames(Fig3)[2]<- "Review Articles"
colnames(Fig3)[3]<- "Non-Refereed Journal Articles"
colnames(Fig3)[4]<- "Books"
colnames(Fig3)[5]<- "Book Chapters"
colnames(Fig3)[6]<- "Theses"
colnames(Fig3)[7]<- "Websites"
colnames(Fig3)[8]<- "Technical Reports"
colnames(Fig3)[9]<- "Media Articles"
colnames(Fig3)[10]<- "Peer-Reviewed Journal Articles"
colnames(Fig3)[11]<- "Conference Papers and Presentations"

####
#SOCIAL NETOWORK ANALYSIS

nodes <- read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/ssrlNODES17%2618%5B1%5D.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

links <-  read.csv (text=getURL("https://raw.githubusercontent.com/AhmadMobin/SSRL201712018/master/ssrlEDGES17%2618%5B1%5D.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")  

net3 <- network(links, vertex.attr=nodes, matrix.type="edgelist",
                loops=F, multiple=F, ignore.eval = F)

net3[,]
net3 %n% "net.name" <- "SSRL Network" # network attribute
net3 %v% "group" # Node attribute
net3 %e% "value" # Edge attribute

net3 %v% "col" <- c("blueviolet", "blue", "red", "midnightblue")[net3 %v% "group"]
plot(net3, vertex.cex=(net3 %v% "size")/7, vertex.col="col")

vs <- data.frame(onset=0, terminus=204, vertex.id=1:88)
es <- data.frame(onset=1:203, terminus=204,
                 head=as.matrix(net3, matrix.type="edgelist")[,1],
                 tail=as.matrix(net3, matrix.type="edgelist")[,2])
net3.dyn <- networkDynamic(base.net=net3, edge.spells=es, vertex.spells=vs)



