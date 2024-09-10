# Reading PISA 2018 data in R

#Setting working directory
install.packages("foreign")
install.packages("haven")
install.packages("psych")
# Install and Load the below required packages
library(foreign)
library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(tidyr)
#Load the data and read
PISA_data18<-read.spss("CY07_MSU_STU_QQQ.sav", to.data.frame = T, use.value.labels = F)
View(PISA_data18)
unique(PISA_data18$CNT)
#output: among the countries, we are concerned with Great Britain i.e GBR

PISA2018_GBR<- PISA_data18 %>% filter(CNT=="GBR")
View(PISA2018_GBR)

#Creating a new data frame with selected columns required for analysis(country code, country name, sex, qs, pv math scores)
PISA2018_GBR_new<- PISA2018_GBR[, c(1:2, 18, 321:341, 1027:1036)]
View(PISA2018_GBR_new)

# Using na.omit() to remove rows with any NA values
#library(tidyr)
PISA2018_GBR_cleaned1 <- na.omit(PISA2018_GBR_new)
View(PISA2018_GBR_cleaned1)
dim(PISA2018_GBR_cleaned1)
# 6516   34

#renaming columns
PISA2018_GBR_cleaned2 <-PISA2018_GBR_cleaned1 %>% rename( 
                                                       ICTH_Comp = IC001Q01TA,
                                                       ICTH_Laptop = IC001Q02TA,
                                                       ICTH_Tab = IC001Q03TA,
                                                       ICTH_Internet = IC001Q04TA,
                                                       ICTH_digame = IC001Q05TA,
                                                       ICTH_Mob_no_intaccess = IC001Q06TA,
                                                       ICTH_Mob_intaccess = IC001Q07TA,
                                                       ICTH_Music_player = IC001Q08TA,
                                                       ICTH_Printer = IC001Q09TA,
                                                       ICTH_USB = IC001Q10TA,
                                                       ICTH_Kindle = IC001Q11TA,
                                                       ICTS_Comp = IC009Q01TA,
                                                       ICTS_Laptop = IC009Q02TA,
                                                       ICTS_Tab = IC009Q03TA,
                                                       ICTS_Int_Comp = IC009Q05NA,
                                                       ICTS_Int_wireless = IC009Q06NA,
                                                       ICTS_storg_schdata = IC009Q07NA,
                                                       ICTS_USB = IC009Q08TA,
                                                       ICTS_Kindle = IC009Q09TA,
                                                       ICTS_projector = IC009Q10NA,
                                                       ICTS_smartboard = IC009Q11NA)

View(PISA2018_GBR_cleaned2) 

#--------------------------------------Recoding Values--------------------------------------------#
# mutating, sex column, avg of PV values and recoding the data as
#Version 1
#3------>0 (unavailable), 2-------->0 (avl. but do not use), 1-------->1 (avl. and use)
PISA2018_GBR_cleaned3<- PISA2018_GBR_cleaned2 %>% 
  mutate(Sex = ifelse(ST004D01T == 1, "M", ifelse(ST004D01T == 2, "F", NA)))%>%
  mutate(PV_avg = rowMeans(select(.,PV1MATH:PV10MATH), na.rm = TRUE)) %>%
  mutate_at(vars(ICTH_Comp:ICTS_smartboard), 
                                 ~ recode(.,'2' = 0, '3' = 0))# this is to see the difference in the scores of those 
#who use and not use/unvailability of digital device


#Version 2
#3------>0 (unavailable), 2-------->1 (avl. but do not use), 1-------->2 (avl. and use)
#PISA2018_GBR_cleaned3<- PISA2018_GBR_cleaned2 %>% 
#                        mutate(Sex = ifelse(ST004D01T == 1, "M", ifelse(ST004D01T == 2, "F", NA)))%>%
#                        mutate(PV_avg = rowMeans(select(.,PV1MATH:PV10MATH), na.rm = TRUE)) %>%
#                        mutate_at(vars(ICTH_Comp:ICTS_smartboard), 
#                        ~ recode(., '1' = 2, '2' = 1, '3' = 0)) #this is to gain insights on the scores
#of students who use digital device, who have digital device but do not use and the students who do not have any
#digital device

View(PISA2018_GBR_cleaned3) 





#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#



###---------------------------5.1 Analysis of ICT Scores: PISA 2018 -------------------###

PISA2018_GBR_cleaned4 <- PISA2018_GBR_cleaned3 %>%
                        select(-ST004D01T, -PV1MATH, -PV2MATH, -PV3MATH, -PV4MATH, -PV5MATH,
                               -PV6MATH, -PV7MATH, -PV8MATH, -PV9MATH, -PV10MATH)%>%
                         mutate(ICTH_Sum = rowSums(select(., ICTH_Comp:ICTH_Kindle), na.rm = TRUE)) %>%
                         mutate(ICTS_Sum = rowSums(select(., ICTS_Comp:ICTS_smartboard), na.rm = TRUE))
                                
View(PISA2018_GBR_cleaned4)

#----------------------Average ICT scores based on location------------------------#
# Reshape the data into long format
PISA2018_GBR_dataframe<- PISA2018_GBR_cleaned4 %>%
  pivot_longer(cols = c(ICTH_Sum, ICTS_Sum),
               names_to = "ICTuse_location",
               values_to = "Score")

summary_stats1 <- PISA2018_GBR_dataframe %>%
  group_by(ICTuse_location) %>%
  summarise(
    Median = median(Score, na.rm = TRUE),
    Mean = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  )
summary_stats1

#output
#  ICTuse_location Median  Mean
#<chr>            <dbl> <dbl>
#1 ICTH_Sum         6    6.57
#2 ICTS_Sum         6    5.39

##......................VISUALISATION......................................##
#Visualizing through Box Plots
# Creating box plots with median and mean labels

#Box Plot : Comparison of ICT Scores by location
ggplot( PISA2018_GBR_dataframe, aes(x = ICTuse_location, y = Score, fill = ICTuse_location)) +
  geom_boxplot() +
  geom_text(data = summary_stats1, aes(x = ICTuse_location, y = Median, label = round(Median, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, color = "black") +
  geom_text(data = summary_stats1, aes(x = ICTuse_location, y = Mean, label = round(Mean, 1)),
            position = position_dodge(width = 0.75), vjust = 2.5, color = "red") +
  scale_fill_manual(values = c("#b1f016", "#28e9f5")) +  # Custom fill colors
  labs(title = "Distribution of ICT Scores by Location",
       x = "ICTuse_location",
       y = "Score",
       fill = "Comparison of ICT Scores by Location") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 20,colour = "steelblue", face = "bold", hjust = 0.5),  # Title styling
    axis.title.x = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # X-axis title styling
    axis.title.y = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # Y-axis title styling
    legend.title = element_text(size = 12),  # Legend title styling
    legend.text = element_text(size = 10),   # Legend text styling
    legend.position = "right"   #position of Legend
  )

#----------------------Average ICT scores of the two genders based on location------------------------#

# Calculate median and mean values for each group
summary_stats2 <- PISA2018_GBR_dataframe %>%
  group_by(Sex, ICTuse_location) %>%
  summarise(
    Median = median(Score, na.rm = TRUE),
    Mean = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  )
summary_stats2
#output
#  Sex   ICTuse_location Median  Mean
#<chr> <chr>            <dbl> <dbl>
#1 F     ICTH_Sum             7  6.82
#2 F     ICTS_Sum             6  5.43
#3 M     ICTH_Sum             6  6.34
#4 M     ICTS_Sum             6  5.34

##......................VISUALISATION.......................................##
#Box Plot : Comparison of ICT Scores by gender

ggplot(PISA2018_GBR_dataframe, aes(x = Sex, y = Score, fill = ICTuse_location)) +
  geom_boxplot() +
  geom_text(data = summary_stats2, aes(x = Sex, y = Median, label = round(Median, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, color = "black") +
  geom_text(data = summary_stats2, aes(x = Sex, y = Mean, label = round(Mean, 1)),
            position = position_dodge(width = 0.75), vjust = 1.9, color = "red") +
  scale_fill_manual(values = c("lightblue", "lightpink")) +  # Custom fill colors
  labs(title = "Distribution of ICT Scores by Sex",
       x = "Sex",
       y = "Score",
       fill = "Location of ICT use") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 20,colour = "#0622BB", face = "bold", hjust = 0.5),  # Title styling
    axis.title.x = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # X-axis title styling
    axis.title.y = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # Y-axis title styling
    legend.title = element_text(size = 12),  # Legend title styling
    legend.text = element_text(size = 10)  # Legend text styling
  )
#output:
# mean ICTH score of girls : 6.8
# mean ICTS score of girls : 5.4
# mean ICTH score of boys :  6.3
# mean ICTS score of boys : 5.3

#Normality Check

# Filter data for boys and girls for 2018
boys_2018df <- PISA2018_GBR_cleaned4 %>% filter(Sex == "M")
girls_2018df <- PISA2018_GBR_cleaned4 %>% filter(Sex == "F")

#............ICT School Usage ....................
#Q-Q plot of TCTS Sum for boys
qqnorm(boys_2018df$ICTS_Sum, main="Q-Q Plot of ICTS Sum for Boys") 
qqline(boys_2018df$ICTS_Sum, col = "red")
#The graph looks quite normal


#Q-Q plot of TCTS Sum for girls
qqnorm(girls_2018df$ICTS_Sum, main = "Q-Q Plot of ICTS Sum for Girls") 
qqline(girls_2018df$ICTS_Sum, col = "blue")
#The graph looks quite normal
#............ICT Home Usage ....................
#Q-Q plot of TCTH Sum for boys
qqnorm(boys_2018df$ICTH_Sum, main="Q-Q Plot of ICTH Sum for Boys") 
qqline(boys_2018df$ICTH_Sum, col = "red")
#data appears normal

#Q-Q plot of TCTH Sum for girls
qqnorm(girls_2018df$ICTH_Sum, main = "Q-Q Plot of ICTH Sum for Girls") 
qqline(girls_2018df$ICTH_Sum, col = "blue")
#The graphs look quite normal


#histogram 1
ggplot(boys_2018df, aes(x = ICTH_Sum)) +
  geom_histogram(binwidth = 1, fill = "#E9F310",colour = "black", alpha = 0.5) +
  ggtitle("Plot for ICTH Scores of Boys 2018")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, colour = "#F35F10", face = "bold"),
        axis.title = element_text(size = 13, face = "bold", colour = "#68C00B"),#
        axis.text = element_text(size = 13, face = "bold", colour = "#68C00B")
  ) +
  labs(
    x = "ICTH Scores",
    y ="Frequency" 
  )


#histogram 2
ggplot(boys_2018df, aes(x = ICTS_Sum)) +
  geom_histogram(binwidth = 1, fill = "#44e7c7",colour = "black", alpha = 0.5) +
  ggtitle("Plot for ICTS Scores of Boys 2018")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, colour = "#e23614", face = "bold"),
        axis.title = element_text(size = 13, face = "bold", colour = "darkblue"),#
        axis.text = element_text(size = 13, face = "bold", colour = "darkblue")
  ) +
  labs(
    x = "ICTS Scores",
    y ="Frequency" 
  )



#histogram 3
ggplot(girls_2018df, aes(x = ICTH_Sum)) +
  geom_histogram(binwidth = 1, fill = "lightpink",colour = "black", alpha = 0.5) +
  ggtitle("Histogram Plot for ICTH Scores of Girls 2018")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, colour = "darkblue", face = "bold"),
        axis.title = element_text(size = 13, face = "bold", colour = "steelblue"),#
        axis.text = element_text(size = 13, face = "bold", colour = "steelblue")
  ) +
  labs(
    x = "ICTH Scores",
    y ="Frequency" 
  )
# they look normal 

#histogram 4
ggplot(girls_2018df, aes(x = ICTS_Sum)) +
  geom_histogram(binwidth = 1, fill = "#38e4f5",colour = "black", alpha = 0.5) +
  ggtitle("Histogram Plot for ICTS Scores of Girls 2018")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, colour = "#e10297", face = "bold"),
        axis.title = element_text(size = 13, face = "bold", colour = "darkblue"),#
        axis.text = element_text(size = 13, face = "bold", colour = "darkblue")
  ) +
  labs(
    x = "ICTS Scores",
    y ="Frequency" 
  )


#.......................Visualizations for normality for ICTH and ICTS scores........................#
#----------------------------------------------------------------------------------------------------#

#Q-Q plot of ICTH Sum for students
qqnorm(PISA2018_GBR_cleaned4$ICTH_Sum, main="Q-Q Plot Of ICTH Scores of PISA 2018") 
qqline(PISA2018_GBR_cleaned4$ICTH_Sum, col = "red")
#output: data appears normal


#Q-Q plot of ICTS Sum for students
qqnorm(PISA2018_GBR_cleaned4$ICTS_Sum, main="Q-Q Plot Of ICTS Scores of PISA 2018") 
qqline(PISA2018_GBR_cleaned4$ICTS_Sum, col = "red")
#output: data appears normal

#histogram 1
ggplot(PISA2018_GBR_cleaned4, aes(x = ICTH_Sum)) +
  geom_histogram(binwidth = 1, fill = "#f2e480",colour = "black", alpha = 0.5) +
  ggtitle("Plot for ICTH Scores of Students 2018")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, colour = "#c90d4c", face = "bold"),
        axis.title = element_text(size = 13, face = "bold", colour = "darkgreen"),#
        axis.text = element_text(size = 13, face = "bold", colour = "darkgreen")
  ) +
  labs(
    x = "ICTH Scores",
    y ="Frequency" 
  )


#histogram 2
ggplot(PISA2018_GBR_cleaned4, aes(x = ICTS_Sum)) +
  geom_histogram(binwidth = 1, fill = "#acf58a",colour = "black", alpha = 0.5) +
  ggtitle("Plot for ICTS Scores of Students 2018")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, colour = "#c90d4c", face = "bold"),
        axis.title = element_text(size = 13, face = "bold", colour = "#f1530e"),#
        axis.text = element_text(size = 13, face = "bold", colour = "#f1530e")
  ) +
  labs(
    x = "ICTS Scores",
    y ="Frequency" 
  )


#.............................T- Test...........................................
##............................TEST 1##.........................................

# Since the above visualisations of the data seem close to normal distribution , we proceed with T-Tests

# H0(Null Hypothesis): There is no significant difference in the avg. ICT scores of students at home and school resp. 
#within a year(2018)

# H1: The average ICT use score at home is significantly different from the average ICT use score at 
#school within a year(2018)

# Paired T-Test for home usage within each year
#The paired t-test is more appropriate when comparing two related groups (e.g., ICT use at home and school 
#for the same students) because it accounts for the pairing of the data points.
t.test(PISA2018_GBR_cleaned4$ICTH_Sum, 
       PISA2018_GBR_cleaned4$ICTS_Sum,
       paired = TRUE)

#1. p-value < 2.2e-16 <0.05, reject H0=> There is a significant difference in the ICT scores of students at home and school

#2. 95 percent confidence interval: 1.123890 1.251493

#3. mean difference 1.187692


# T-Test
#H0: The avg. ICT scores at home is less than the avg. ICT scores at school
#H1: The avg. ICT score at home is significantly greater than that at school

t.test(PISA2018_GBR_cleaned4$ICTH_Sum, PISA2018_GBR_cleaned4$ICTS_Sum, 
       paired = TRUE, 
       alternative = "greater")
#1. p-value < 2.2e-16 <0.05 => reject H0
#2. 95 percent confidence interval: 1.13415     Inf
#3. mean difference 1.187692 




#.............................T- Test...........................................
##............................TEST 2##..........................................

#Since the above visualisations of the data seem close to normal distribution , we proceed with T-Test
# H0(Null Hypothesis): There is no significant difference in the ICT scores of boys and girls at home within a year(2018)
# H1: There is a significant difference in the ICT scores of boys and girls at home within a year(2018)

# T-Test for home usage within each year
t_test_ICTH_2018 <- t.test(ICTH_Sum ~ Sex, data = PISA2018_GBR_cleaned4)
t_test_ICTH_2018

#output: 1. p-value < 2.2e-16 <0.05
#Interpretation: so we fail to accept H0 => there is a significant difference in the ICT usage between boys and girls 
#at home within a year(2018)

#2. 95 percent confidence interval: 0.3788762 0.5827771

#3. mean in group F mean in group M : 6.821316        6.340489 


#To check whether the avg. ICTH score of girls is greater than boys?
#H0: The average ICTH score for girls is less than or equal to the average ICTH score for boys.
#H1: The average ICTH score for girls is greater than the average ICTH score for boys.
t.test(ICTH_Sum ~ Sex, data = PISA2018_GBR_cleaned4, alternative = "greater")

#1. p-value < 2.2e-16 < 0.05

#2. 95 percent confidence interval: 0.3952709       Inf

#3. mean in group F mean in group M  6.821316        6.340489 



# T-Test for school usage within each year
# H0(Null Hypothesis): There is no significant difference in the ICT scores of boys and girls at schools within a year(2018)
# H1: There is a significant difference in the ICT scores of boys and girls at schools within a year(2018)

t_test_ICTS_2018 <- t.test(ICTS_Sum ~ Sex, data = PISA2018_GBR_cleaned4)
t_test_ICTS_2018

#output: 1.  p-value = 0.1182 > 0.05,
# Interpretation: so we fail to reject the null hypothesis => there is no significant difference in the ICT usage between boys and 
# girls at schools within a year(2018)

#2. 95 percent confidence interval: -0.02222307  0.19679839

#3. mean in group F =5.431056 and mean in group M = 5.343769 





#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#



###---------------------5.2 Analysis of Average Mathematics Scores: PISA 2018-------------------###
#To check PV avg. of students for the year 2018
PISA2018_GBR_cleaned7<- PISA2018_GBR_cleaned4 %>%
  summarise(
    PV_average = mean(PV_avg, na.rm = TRUE))
PISA2018_GBR_cleaned7
#output: 508.5959


#-----------------------------Average Mathematics scores of Boys and Girls------------------------#
#To compare the average PV Math values by sex

PISA2018_GBR_cleaned6<- PISA2018_GBR_cleaned4 %>%
  group_by(Sex) %>%
  summarise(
    PV_average = mean(PV_avg, na.rm = TRUE))
View(PISA2018_GBR_cleaned6)
#Output
# F = 516.7051, M = 500.9509

#---------------------------------------Normality check through Q-Q plots-----------------------------#

qqnorm(PISA2018_GBR_cleaned4$PV_avg[PISA2018_GBR_cleaned4$Sex=="M"], main="Q-Q Plot for Boys PV_avg") 
qqline(PISA2018_GBR_cleaned4$PV_avg[PISA2018_GBR_cleaned4$Sex=="M"], col = "red")

qqnorm(PISA2018_GBR_cleaned4$PV_avg[PISA2018_GBR_cleaned4$Sex=="F"], main="Q-Q Plot for Girls PV_avg") 
qqline(PISA2018_GBR_cleaned4$PV_avg[PISA2018_GBR_cleaned4$Sex=="F"], col = "blue")


#T-Test: to check whether this difference is significant between boys and girls
#H0: There is no significant difference in the average mathematics scores between boys and girls
#H1: There is a significant difference in the average mathematics scores between boys and girls
t.test(PV_avg ~ Sex, data = PISA2018_GBR_cleaned4)
#p-val = 8.269e-15 (extremely small)<0.05 => reject H0 => There is a significant difference




#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#




###---------------5.3 Different Levels of ICT use and mathematics scores PISA 2018-------------###
#With Version 1 of Recoded values

PISA2018_GBR_dataframe3 <- PISA2018_GBR_cleaned4 %>%
  mutate(ICT_SUM = ICTH_Sum + ICTS_Sum)

summary_stats3 <-PISA2018_GBR_dataframe3%>%
  summarise (
    mean_ICT_SUM = mean(ICT_SUM, na.rm = TRUE),
    median_ICT_SUM = median(ICT_SUM, na.rm = TRUE)
  )
summary_stats3

#output
##mean_ICT_SUM = 11.95994; median_ICT_SUM = 12

PISA2018_GBR_dataframe3<-PISA2018_GBR_cleaned4 %>%
                          mutate(ICT_SUM = ICTH_Sum + ICTS_Sum,
                                 category= ifelse(ICT_SUM <= 12, "Low Frequency ICT Use", 
                                                         "High Frequency ICT Use"))
View(PISA2018_GBR_dataframe3)


summary_stats4 <- PISA2018_GBR_dataframe3 %>%
  group_by(category) %>%
  summarise( Avg_PV_Math = mean(PV_avg, na.rm = TRUE)
  )

summary_stats4
#output
# A tibble: 2 × 2
#category               Avg_PV_Math
#<chr>                        <dbl>
#1 High Frequency ICT Use       505.
#2 Low Frequency ICT Use        511.

##......................VISUALISATION......................................##
#Boxplot: Comparing the mean math scores of the students with low and high frequency ICT use
ggplot(PISA2018_GBR_dataframe3, aes(x = category, y = PV_avg, fill = category)) +
  geom_boxplot() +
  geom_text(data = summary_stats4, aes(x = category, y = Avg_PV_Math, label = round(Avg_PV_Math, 1)),
            position = position_dodge(width = 0.75), vjust = 1.5, color = "red") +
  scale_fill_manual(values = c("#f1cf93", "#83f1bf")) +  # Custom fill colors
  labs(title = "Distribution of Math Scores by Frequency of ICT Use",
       x = "Frequency of Use",
       y = "Average Math Scores",
       fill = "Frequency of ICT Use") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 20,colour = "steelblue", face = "bold", hjust = 0.5),  # Title styling
    axis.title.x = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # X-axis title styling
    axis.title.y = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # Y-axis title styling
    legend.title = element_text(size = 12),  # Legend title styling
    legend.text = element_text(size = 10),   # Legend text styling
    legend.position = "bottom"   #position of Legend
  )

#.......................Visualizations for normality...........................

#Normality Check through Q-Q plots

qqnorm(PISA2018_GBR_dataframe3$ICT_SUM, main="Q-Q Plot for ICT_SUM PISA 2018") 
qqline(PISA2018_GBR_dataframe3$ICT_SUM, col = "red")

#T-Test
#Let us check whether there is any significant difference in the avg. mathematics scores of students
#with varied levels of ICT use
#H0: There is no significant difference in the scores avg. mathematics scores of students
#with varied levels of ICT use

#H1: There is a significant difference in he scores avg. mathematics scores of students
#with varied levels of ICT use

t.test(PV_avg ~ category, data = PISA2018_GBR_dataframe3)
#p-val=0.001376 < 0.05 => reject H0 => There is a significant difference in the scores avg. 
#mathematics scores of students with varied levels of ICT use


#-----------------Different Levels of ICT Use and Mathematics scores: Version 2 of Recoded values----#

#creating a function with arguments to compare the mean Math scores of students with 
#varied levels of ICT use

ICTuse_mathscore_comp <-function(data, ICT_device, Mathscore) {
#grouping by specified column, calculating the 0s, 1s, and 2s, then calculating the average mathscores
  
  result <- data %>%
    group_by_at(ICT_device) %>%
    summarise(
      count = n(),
    PV_average = mean(!!sym(Mathscore), na.rm = TRUE)
    )


#visualising the above through bar graphs

plot<- ggplot(data, aes(x= factor(!!sym(ICT_device)), y=PV_average, fill= factor(!!sym(ICT_device))))+
  geom_bar(sta = "identity", position  = "dodge") +
  labs(title = paste("Average Mathematics Scores by", ICT_device, "Usage"),
       x=paste(ICT_device, "Usage"),
       y= "Average Mathematics Scores",
       fill = "Usage" )+
         theme_bw() +
         theme(
           plot.title = element_text(size = 16, face = "bold", color = "steelblue", hjust=0.5),
           axis.title.x = element_text(size = 12, face = "bold", color = "magenta"),
           axis.title.y = element_text(size = 12, face = "bold", color = "magenta")
         )
        
list(result = result, plot = plot)        

}

#------------------------------------Digital Devices for Home Use--------------------------------#
#-------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned4, "ICTH_Comp", "PV_avg")
output$result
#output
#        ICTH_Comp count PV_average
#        <dbl>    <int>      <dbl>
#1         0      2125       507.
#2         1      1046       511.
#3         2      3345       509.
#-------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned4, "ICTH_Laptop", "PV_avg")
output$result
#output
#       ICTH_Laptop count PV_average
#           <dbl>   <int>      <dbl>
#1           0      522       486.
#2           1      1027       511.
#3           2     4967       510.

#--------------------------------------------------------------------------------------------#

output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned4, "ICTH_Tab", "PV_avg")
output$result
#output
#       ICTH_Tab count PV_average
#         <dbl> <int>      <dbl>
#1        0     699        514.
#2        1     1566       523.
#3        2     4251       503.
#----------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned4, "ICTH_Internet", "PV_avg")
output$result
#output
#      ICTH_Internet count PV_average
#             <dbl>   <int>     <dbl>
#1             0       21        448.
#2             1       40        428.
#3             2      6455       509.


#Output: students who use internet have scored comparatively well than others
#Let us check whether the difference in average mathematics score significant or nor through ANOVA Test
PISA2018_GBR_cleaned5<- PISA2018_GBR_cleaned4 %>% 
                        mutate(ICTH_Int_cat = factor(ICTH_Internet,
                                                     levels = c(0,1,2),
                                                     labels = c("Not availbale",
                                                                "Available but do not use",
                                                                "Available and use")))

#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the three groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_testa<- aov(PV_avg ~ factor(ICTH_Internet), data = PISA2018_GBR_cleaned3)
summary(ANOVA_testa)
#p-value(extremely small) =1.05e-11 <0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_testa)
#output
#        diff       lwr       upr     p adj
#1-0 -19.99917 -71.57419  31.57584 0.6346033        #not significant
#2-0  61.09545  19.26335 102.92756 0.0017985        #statistically significant
#2-1  81.09463  50.73995 111.44930 0.0000000        #statistically significant

#interpretations
#1. p-val = 0.001 <0.05 =>There is a statistical significant difference in the scores of students who have 
#access to internet and use it than who do not have access

#2. p-val = 0<0.005 => here is a statistical significant difference in the scores of students who have 
#access to internet and use it than who have access but do not use


#-----------------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned3, "ICTH_digame", "PV_avg")
output$result
#output
#      ICTH_digame count PV_average
#            <dbl> <int>     <dbl>
#1           0   939       516.
#2           1  1457       507.
#3           2  4120       507.


#-----------------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned3, "ICTH_Mob_intaccess", "PV_avg")
output$result
#output
#         ICTH_Mob_intaccess count PV_average
#                  <dbl>    <int>     <dbl>
#1                  0        72       463.
#2                  1       103       498.
#3                  2       6341      509.

ANOVA_testb<- aov(PV_avg ~ factor(ICTH_Mob_intaccess), data = PISA2018_GBR_cleaned4)
summary(ANOVA_testb)
#p-value(extremely small)= 6.48e-06 <0.05 => fail to reject H1
#Tukey Test
TukeyHSD(ANOVA_testb)
#output
#        diff       lwr      upr     p adj
#1-0 34.90695  5.446645 64.36726 0.0151680      #statistically significant
#2-0 45.79341 23.063979 68.52284 0.0000071      #statistically significant
#2-1 10.88646 -8.163031 29.93595 0.3731805      #not significant

#interpretations
#p-val = 0 < 0.05 => There is a statistical significant difference in the scores of students who use
#a mobile with internet access than who do not have a mobile

#------------------------------------Digital Devices for School Use--------------------------------#

output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned3, "ICTS_Comp", "PV_avg")
output$result
#output
#      ICTS_Comp count PV_average
#          <dbl> <int>      <dbl>
#1         0     223       500.
#2         1     901       504.
#3         2     5392      510.

# let us check whether these differences are statistically significant
ANOVA_testc<- aov(PV_avg ~ factor(ICTS_Comp), data = PISA2018_GBR_cleaned3)
summary(ANOVA_testc)
#p-value = 0.02<0.05 => fail to reject H1
#Tukey Test
TukeyHSD(ANOVA_testc)
#output
#        diff         lwr      upr     p adj
#1-0  4.072232 -10.2901961 18.43466 0.7840093     #not statistically significant
#2-0 10.250375  -2.8718366 23.37259 0.1595023     #not statistically significant
#2-1  6.178143  -0.7330185 13.08931 0.0907998     #not statistically significant

#------------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned3, "ICTS_Laptop", "PV_avg")
output$result

#          ICTS_Laptop count PV_average
#                <dbl> <int>      <dbl>
#1           0         1692       519.
#2           1         1533       516.
#3           2         3291       500.


ANOVA_testd<- aov(PV_avg ~ factor(ICTS_Laptop), data = PISA2018_GBR_cleaned3)
summary(ANOVA_testd)
#p-value <2e-16 <0.05 => fail to reject H1
#Tukey Test
TukeyHSD(ANOVA_testd)
#          diff       lwr        upr     p adj
#1-0  -3.427166 -10.16208   3.307749 0.4574273      #not statistically significant
#2-0 -19.211421 -24.92515 -13.497689 0.0000000      #statistically significant
#2-1 -15.784255 -21.69044  -9.878068 0.0000000      #statistically significant

#----------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned3, "ICTS_Tab", "PV_avg")
output$result
#output
#          ICTS_Tab count PV_average
#          <dbl>   <int>      <dbl>
#1        0        3098       514.
#2        1        1538       512.
#3        2        1880       496.

ANOVA_teste<- aov(PV_avg ~ factor(ICTS_Tab), data = PISA2018_GBR_cleaned3)
summary(ANOVA_teste)
#p-value= 9.55e-14(extremely small) <0.05 => fail to reject H1
#Tukey Test
TukeyHSD(ANOVA_teste)

#        diff        lwr        upr     p adj
#1-0  -2.792218  -8.757808   3.173372 0.5158078     #not statistically significant
#2-0 -18.065353 -23.656596 -12.474109 0.0000000     #statistically significant
#2-1 -15.273135 -21.848646  -8.697624 0.0000002     #statistically significant

#-----------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned3, "ICTS_Int_Comp", "PV_avg")
output$result

#        ICTS_Int_Comp count PV_average
#          <dbl>       <int>    <dbl>
#1           0         198       464.
#2           1         680       500.
#3           2         5638      511.

ANOVA_testf<- aov(PV_avg ~ factor(ICTS_Int_Comp), data = PISA2018_GBR_cleaned3)
summary(ANOVA_testf)

#p-value=<2e-16 (extremely small)<0.05 => fail to reject H1
#Tukey Test
TukeyHSD(ANOVA_testf)

#        diff       lwr      upr     p adj
#1-0 36.83943 21.409987 52.26888 0.0000001      #statistically significant
#2-0 47.54654 33.731483 61.36159 0.0000000      #statistically significant
#2-1 10.70711  2.950658 18.46355 0.0034890      #statistically significant

#-----------------------------------------------------------------------------------------------------#

output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned3, "ICTS_Int_wireless", "PV_avg")
output$result
#output
#              ICTS_Int_wireless count PV_average
#               <dbl>    <int>      <dbl>
#1                 0       2003       504.
#2                 1       1098       506.
#3                 2       3415       512.

ANOVA_testg<- aov(PV_avg ~ factor(ICTS_Int_wireless), data = PISA2018_GBR_cleaned3)
summary(ANOVA_testg)
#p-value= 0.00231 < 0.05 => fail to reject H1
#Tukey Test
TukeyHSD(ANOVA_testg)

#        diff        lwr       upr     p adj
#1-0 1.513697 -5.6941266  8.721521 0.8749813        #not statistically significant
#2-0 7.545344  2.1430453 12.947643 0.0030580        #statistically significant
#2-1 6.031647 -0.6276913 12.690985 0.0852372        #not statistically significant

#------------------------------------------------------------------------------------------------------#

output <-ICTuse_mathscore_comp(PISA2018_GBR_cleaned3, "ICTS_smartboard", "PV_avg")
output$result
#output
#       ICTS_smartboard count PV_average
#          <dbl> <int>      <dbl>
#1           0   575       481.
#2           1  2304       514.
#3           2  3637       509.
ANOVA_testh<- aov(PV_avg ~ factor(ICTS_smartboard), data = PISA2018_GBR_cleaned3)
summary(ANOVA_testh)
#p-value < 2e-16 < 0.05 => fail to reject H1
#Tukey Test
TukeyHSD(ANOVA_testh)

#        diff       lwr          upr     p adj
#1-0 32.856795  23.95150 41.762088329 0.0000000
#2-0 27.761719  19.18855 36.334883011 0.0000000
#2-1 -5.095077 -10.18158 -0.008572255 0.0495025






#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#



###.............5.4 Analysis of ICT use during Mathematics Lessons and Mathematics Scores: PISA 2018...............##

PISA2018_GBR_new1<- PISA2018_GBR[, c(1:2, 18, 347:349, 1027:1036)]
#library(tidyr)
PISA2018_GBR_new1_cleaned1 <- na.omit(PISA2018_GBR_new1)
dim(PISA2018_GBR_new1_cleaned1)
View(PISA2018_GBR_new1_cleaned1)

#renaming columns
PISA2018_GBR_new1_cleaned2 <-PISA2018_GBR_new1_cleaned1 %>% 
                            rename(Test_Language = IC150Q01HA,
                                   Mathematics = IC150Q02HA,
                                   Science = IC150Q03HA) %>%
                            mutate(Sex = ifelse(ST004D01T == 1, "M",
                                                ifelse(ST004D01T == 2, "F", NA)))%>%
                            mutate(PV_avg = rowMeans(select(.,PV1MATH:PV10MATH), na.rm = TRUE))
                            
View(PISA2018_GBR_new1_cleaned2)

PISA2018_GBR_new1_cleaned3 <- PISA2018_GBR_new1_cleaned2 %>%
                             select(-ST004D01T, -PV1MATH, -PV2MATH, -PV3MATH, -PV4MATH, -PV5MATH,
                                    -PV6MATH, -PV7MATH, -PV8MATH, -PV9MATH, -PV10MATH) %>%
                              mutate_at(vars(Test_Language:Science), 
                                         ~ recode(., 
                                                  '1' = 0, 
                                                  '2' = 1, 
                                                  '3' = 2, 
                                                  '4' = 3 ))

View(PISA2018_GBR_new1_cleaned3)

PISA2018_GBR_new1_cleaned3a <- PISA2018_GBR_new1_cleaned3 %>%
                                filter(Mathematics != 5)
View(PISA2018_GBR_new1_cleaned3a)
  
PISA2018_GBR_new1_cleaned4 <- PISA2018_GBR_new1_cleaned3a %>%
                              mutate(Math_ICT_cat = factor(ifelse(Mathematics ==0, "No ICT Use",
                                                           ifelse(Mathematics ==1, "Low ICT Use",
                                                           ifelse(Mathematics ==2, "Medium ICT Use",
                                                                                   "High ICT Use"
                                                                 ))),
                               levels = c("No ICT Use", "Low ICT Use", "Medium ICT Use",
                                          "High ICT Use")))   
View(PISA2018_GBR_new1_cleaned4)                            

PISA2018_GBR_new1_cleaned5<- PISA2018_GBR_new1_cleaned4 %>%
                             group_by(Math_ICT_cat) %>%
                             summarise(
                            PV_Math_average = mean(PV_avg, na.rm = TRUE))
PISA2018_GBR_new1_cleaned5
#output
#     Math_ICT_cat   PV_Math_average
#        <fct>              <dbl>
#1 No ICT Use                506.
#2 Low ICT Use               492.
#3 Medium ICT Use            498.
#4 High ICT Use              522.


ggplot(PISA2018_GBR_new1_cleaned4, aes(x = Math_ICT_cat, y = PV_avg, fill = Math_ICT_cat)) +
  geom_boxplot() +
  geom_text(data = PISA2018_GBR_new1_cleaned5, aes(x =  Math_ICT_cat, y = PV_Math_average, 
                                                   label = round(PV_Math_average, 1)),
            vjust = 1.9, color = "red") +
  scale_fill_manual(values = c("#8df459" , "lightblue", "lightpink", "#c2f960")) +  # Custom fill colors
  labs(title = "Distribution of Mathematics Scores by Frequency of ICT Use",
       x = "ICT Use Category",
       y = "Average Mathematics Scores",
       fill = "ICT Use Category") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 15,colour = "#0622BB", face = "bold", hjust = 0.5),  # Title styling
    axis.title.x = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # X-axis title styling
    axis.title.y = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # Y-axis title styling
    legend.title = element_text(size = 12),  # Legend title styling
    legend.text = element_text(size = 10)  # Legend text styling
  )

#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the four groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_test<- aov(PV_avg ~ factor(Math_ICT_cat), data = PISA2018_GBR_new1_cleaned4)
summary(ANOVA_test)
#p-value(extremely small)= 2.74e-12 < 0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test)
#                                diff        lwr       upr     p adj
#Low ICT Use-No ICT Use      -14.449369 -21.106155 -7.792584 0.0000002    significant difference
#Medium ICT Use-No ICT Use    -8.481726 -17.811911  0.848458 0.0900925
#High ICT Use-No ICT Use      15.713343   5.425380 26.001305 0.0005081    significant difference
#Medium ICT Use-Low ICT Use    5.967643  -4.642142 16.577428 0.4710137
#High ICT Use-Low ICT Use     30.162712  18.701588 41.623836 0.0000000    significant difference
#High ICT Use-Medium ICT Use  24.195069  11.000490 37.389648 0.0000149    significant difference



