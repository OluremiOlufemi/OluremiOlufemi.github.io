require(ggplot2)
require(gridExtra)
require(dplyr)
require(ggthemes)
require(tidyr)
policy <- read.csv("GDCPolicy.csv")
counties <- read.csv("CountiesNoClinics.csv")
UStrend <- read.csv("2009-2012.csv")
teenbirths <- read.csv("teenbirthrate.csv")
providers <- read.csv("USProvidersTrend.csv")
intended <- read.csv("Intended.csv")
View(policy)

policy %>% gather(variable, rate, 4:9)
policy2 <- policy %>% gather (variable, rate, 4:9) 


chart1 <- ggplot(policy2) +
  aes(x = variable, fill = rate) +
  geom_bar() +
  ggtitle("Women's Abortion Rights by State Policy") +
  theme_solarized()+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#BB0529", face="bold", size=20, hjust=0),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill="#FFECEF"),
        plot.background = element_rect(fill="#FFECEF"))
print(chart1)


 

chart2 <- ggplot(counties,  aes(x= reorder(state_id, Percent), y=Percent)) +
  geom_bar(stat="identity",
           fill = "#FE8893")+
  ggtitle("Percentage of Women Living in Counties Without Access to an Abortion Clinic") +
  theme_solarized()+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#BB0529", face="bold", size=20, hjust=0),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill="#FFECEF"),
        plot.background = element_rect(fill="#FFECEF"))+
facet_wrap(~Party, scale="free")
print(chart2)


trend <- UStrend %>% gather (year, count, 2:5)
chart3 <- ggplot(trend, aes(x=year, y=count, fill=factor(year))) +
geom_boxplot() +
ggtitle("Average Rate of Abortion by State") +
  theme_solarized()+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#BB0529", face="bold", size=20, hjust=0),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill="#FFECEF"),
        plot.background = element_rect(fill="#FFECEF"))
print(chart3)


chart4 <- ggplot(teenbirths, aes(x=first_year, y=datum, colour=factor(state_id))) +
geom_line() +
  ggtitle("Teen Pregnancy from 1985 to 2012") +
  theme_solarized()+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#BB0529", face="bold", size=20, hjust=0),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill="#FFECEF"),
        plot.background = element_rect(fill="#FFECEF"))
print(chart4)


chart5 <- ggplot(providers, aes(x=first_year, y=datum, colour= factor(state_id))) +
  geom_step()+ 
  ggtitle("Number of U.S. Abortion Clinics Overtime") +
  theme_solarized()+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#BB0529", face="bold", size=20, hjust=0),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill="#FFECEF"),
        plot.background = element_rect(fill="#FFECEF"))
print(chart5)

chart5b <- ggplot(providers, aes(x=first_year, y=datum, colour= factor(state_id))) +
  geom_point()+ geom_line()+
  ggtitle("Number of U.S. Abortion Clinics Overtime") +
  theme_solarized()+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#BB0529", face="bold", size=20, hjust=0),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill="#FFECEF"),
        plot.background = element_rect(fill="#FFECEF"))
print(chart5b)

 
chart6 <- ggplot(intended, aes(x=first_year, y=datum, colour= factor(state_id))) +
  geom_point() + geom_line()+
  ggtitle("No Significant Trend of Intended Pregnancies from 2006-2010") +
  theme_solarized()+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#BB0529", face="bold", size=20, hjust=0),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill="#FFECEF"),
        plot.background = element_rect(fill="#FFECEF"))
print(chart6)

chart7 <- ggplot(intended, aes(x=first_year, y=datum, colour= factor(state_id))) +
  geom_jitter() + geom_line()+
  ggtitle("No Significant Trend of Intended Pregnancies from 2006-2010") +
  theme_solarized()+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#BB0529", face="bold", size=20, hjust=0),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill="#FFECEF"),
        plot.background = element_rect(fill="#FFECEF"))

print(chart7)

ggsave(filename="Chart1.pdf", plot=chart1)
ggsave(filename="Chart2.pdf", plot=chart2)
ggsave(filename="Chart3.pdf", plot=chart3)
ggsave(filename="Chart4.pdf", plot=chart4)
ggsave(filename="Chart5.pdf", plot=chart5)
ggsave(filename="Chart6.pdf", plot=chart6)
ggsave(filename="Chart7.pdf", plot=chart7)
ggsave(filename="Chart5b.pdf", plot=chart5b)

