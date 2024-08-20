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
library(tidyr)
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


# mutating, sex column, avg of PV values and recoding the data as
#3------>0 (unavailable), 2-------->0 (avl. but do not use), 1-------->1 (avl. and use)
PISA2018_GBR_cleaned3<- PISA2018_GBR_cleaned2 %>% 
                        mutate(Sex = ifelse(ST004D01T == 1, "M", ifelse(ST004D01T == 2, "F", NA)))%>%
                        mutate(PV_avg = rowMeans(select(.,PV1MATH:PV10MATH), na.rm = TRUE)) %>%
                        mutate_at(vars(ICTH_Comp:ICTS_smartboard), 
                        ~ recode(., '2' = 0, '3' = 0)) 
                    

View(PISA2018_GBR_cleaned3) 

PISA2018_GBR_cleaned4 <- PISA2018_GBR_cleaned3 %>%
                        select(-ST004D01T, -PV1MATH, -PV2MATH, -PV3MATH, -PV4MATH, -PV5MATH,
                               -PV6MATH, -PV7MATH, -PV8MATH, -PV9MATH, -PV10MATH)%>%
                         mutate(ICTH_Sum = rowSums(select(., ICTH_Comp:ICTH_Kindle), na.rm = TRUE)) %>%
                         mutate(ICTS_Sum = rowSums(select(., ICTS_Comp:ICTS_smartboard), na.rm = TRUE))
                                
View(PISA2018_GBR_cleaned4)

#To compare the average PV Math values by sex
PISA2018_GBR_cleaned5<- PISA2018_GBR_cleaned4 %>%
                        group_by(Sex) %>%
                        summarise(
                          PV_average = mean(PV_avg, na.rm = TRUE))
View(PISA2018_GBR_cleaned5)
#Output
# F = 516.7051, M = 500.9509

PISA2018_GBR_cleaned6<- PISA2018_GBR_cleaned4 %>%
  summarise(
    PV_average = mean(PV_avg, na.rm = TRUE))
PISA2018_GBR_cleaned6
#output: 508.5959


# Reshape the data into long format
PISA2018_GBR_dataframe<- PISA2018_GBR_cleaned4 %>%
  pivot_longer(cols = c(ICTH_Sum, ICTS_Sum),
               names_to = "ICTuse_location",
               values_to = "Score")

# Calculate median and mean values for each group
summary_stats <- PISA2018_GBR_dataframe %>%
  group_by(Sex, ICTuse_location) %>%
  summarise(
    Median = median(Score, na.rm = TRUE),
    Mean = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  )
summary_stats
#output
#  Sex   ICTuse_location Median  Mean
#<chr> <chr>            <dbl> <dbl>
#1 F     ICTH_Sum             7  6.82
#2 F     ICTS_Sum             6  5.43
#3 M     ICTH_Sum             6  6.34
#4 M     ICTS_Sum             6  5.34


summary_stats2 <- PISA2018_GBR_dataframe %>%
  group_by(ICTuse_location) %>%
  summarise(
    Median = median(Score, na.rm = TRUE),
    Mean = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  )
summary_stats2

#output
#  ICTuse_location Median  Mean
#<chr>            <dbl> <dbl>
#1 ICTH_Sum         6    6.57
#2 ICTS_Sum         6    5.39


#Let's check whether there is any significant difference in the Math scores of 
#students with low and high frequency of ICT use

PISA2018_GBR_dataframe3 <- PISA2018_GBR_cleaned4 %>%
  mutate(ICT_SUM = ICTH_Sum + ICTS_Sum) %>%
  mutate(category= ifelse(ICT_SUM < 8, "Low Frequency ICT Use", 
                          "High Frequency ICT Use")
  )
summary_stats3 <- PISA2018_GBR_dataframe3 %>%
  group_by(category) %>%
  summarise( Avg_PV_Math = mean(PV_avg, na.rm = TRUE)
  )

summary_stats3

#output
# A tibble: 2 × 2
#category               Avg_PV_Math
#<chr>                        <dbl>
#1 High Frequency ICT Use       511.
#2 Low Frequency ICT Use        484.


# Create box plots with median and mean labels
#Box Plot 1
library(ggplot2)
ggplot(PISA2018_GBR_dataframe, aes(x = Sex, y = Score, fill = ICTuse_location)) +
  geom_boxplot() +
  geom_text(data = summary_stats, aes(x = Sex, y = Median, label = round(Median, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, color = "black") +
  geom_text(data = summary_stats, aes(x = Sex, y = Mean, label = round(Mean, 1)),
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

#Box Plot 2
ggplot( PISA2018_GBR_dataframe, aes(x = ICTuse_location, y = Score, fill = ICTuse_location)) +
  geom_boxplot() +
  geom_text(data = summary_stats2, aes(x = ICTuse_location, y = Median, label = round(Median, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, color = "black") +
  geom_text(data = summary_stats2, aes(x = ICTuse_location, y = Mean, label = round(Mean, 1)),
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



#...............................Phase 2........................................
#.......................Visualizations for normality...........................
#............ICT Home Usage ....................
#Q-Q plot of ICTH Sum for students
qqnorm(PISA2018_GBR_cleaned4$ICTH_Sum, main="Q-Q Plot Of ICTH Scores") 
qqline(PISA2018_GBR_cleaned4$ICTH_Sum, col = "red")
#output: data appears normal


#Q-Q plot of ICTS Sum for students
qqnorm(PISA2018_GBR_cleaned4$ICTS_Sum, main="Q-Q Plot Of ICTS Scores") 
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
##............................TEST 1##..........................................
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



#Normality
# Filter data for boys and girls for 2018
boys_2018df <- PISA2018_GBR_cleaned4 %>% filter(Sex == "M")
girls_2018df <- PISA2018_GBR_cleaned4 %>% filter(Sex == "F")

#Applying Shapiro-Wilk Test
#null hypothesis: data follows a normal distribution
shapiro.test(boys_2018df$ICTH_Sum)
#output: p value < 2.2e-16, since p value < 0.05(extremely small), we reject null hypothesis

shapiro.test(girls_2018df$ICTH_Sum)
#output: p value < 2.2e-16, since p value < 0.05(extremely small), we reject null hypothesis


#.......................Visualizations for normality...........................
#............ICT Home Usage ....................
#Q-Q plot of TCTH Sum for boys
qqnorm(boys_2018df$ICTH_Sum, main="Q-Q Plot for Boys") 
qqline(boys_2018df$ICTH_Sum, col = "red")
#data appears normal

#Q-Q plot of TCTH Sum for girls
qqnorm(girls_2018df$ICTH_Sum, main = "Q-Q Plot for Girls") 
qqline(girls_2018df$ICTH_Sum, col = "red")
#The graphs look quite normal


#............ICT School Usage ....................
#Q-Q plot of TCTS Sum for boys
qqnorm(boys_2018df$ICTS_Sum, main="Q-Q Plot for Boys") 
qqline(boys_2018df$ICTS_Sum, col = "red")
#The graph looks quite normal


#Q-Q plot of TCTS Sum for girls
qqnorm(girls_2018df$ICTS_Sum, main = "Q-Q Plot for Girls") 
qqline(girls_2018df$ICTS_Sum, col = "red")
#The graph looks quite normal

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


#.............................T-Tests...........................................

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

#1. p-value < 2.2e-16 <0.05

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










###....................................................................................................###

PISA2018_GBR_new1<- PISA2018_GBR[, c(1:2, 18, 347:349, 1027:1036)]
library(tidyr)
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
                                         ~ recode(., '1' = 0, '2' = 1, '3' = 2, '4' = 3, '5' = 0))

View(PISA2018_GBR_new1_cleaned3)

PISA2018_GBR_new1_cleaned4 <- PISA2018_GBR_new1_cleaned3 %>%
                              mutate(Math_ICT_cat = factor(ifelse(Mathematics ==0, "No ICT Use",
                                                           ifelse(Mathematics ==1, "Low ICT Use",
                                                           ifelse(Mathematics ==2, "Medium ICT Use",
                                                                                   "High ICT Use"))),
                               levels = c("No ICT Use", "Low ICT Use", "Medium ICT Use",
                                          "High ICT Use")))   
PISA2018_GBR_new1_cleaned4                             

PISA2018_GBR_new1_cleaned5<- PISA2018_GBR_new1_cleaned4 %>%
                             group_by(Math_ICT_cat) %>%
                             summarise(
                            PV_Math_average = mean(PV_avg, na.rm = TRUE))
PISA2018_GBR_new1_cleaned5

library(ggplot2)
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
