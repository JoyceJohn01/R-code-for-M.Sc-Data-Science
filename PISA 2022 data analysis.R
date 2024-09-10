# Install and Load the below required packages
library(foreign)
library(haven)
library(psych)
library(dplyr)
library(tidyr)
library(ggplot2)

#Load the data and read
PISA_data22<-read.spss("CY08MSP_STU_QQQ.SAV", to.data.frame = T, use.value.labels = F)
View(PISA_data22)

unique(PISA_data22$CNT)

PISA2022_GBR<- PISA_data22 %>% filter(CNT=="GBR")
View(PISA2022_GBR)

#Creating a new data frame with selected columns required for analysis(country code, country name, sex, qs, pv math scores)
PISA2022_GBR_new<- PISA2022_GBR[, c(1, 2, 26, 632:644, 1167:1176)]
View(PISA2022_GBR_new)
dim(PISA2022_GBR_new)

# Using na.omit() to remove rows with any NA values
PISA2022_GBR_cleaned <- na.omit(PISA2022_GBR_new)
View(PISA2022_GBR_cleaned)
dim(PISA2022_GBR_cleaned)
#output
#9253   26 (approx. 71% of total data)


#renaming columns
PISA2022_GBR_cleaned2 <-PISA2022_GBR_cleaned %>% 
                        mutate(Sex = ifelse(ST004D01T == 1, "M", ifelse(ST004D01T == 2, "F", NA)))%>%
                        mutate(PV_avg = rowMeans(select(.,PV1MATH:PV10MATH), na.rm = TRUE)) %>%
                        rename(ICTS_Comp = IC170Q01JA,
                               ICTS_Mob_int_acess =IC170Q02JA,
                               ICTS_Tab_eBook = IC170Q03JA,
                               ICTS_Internet_access = IC170Q04JA,
                               ICTS_School_portal = IC170Q05JA,
                               ICTS_edu_apps_games = IC170Q06JA,
                               ICTS_LMS = IC170Q07JA,
                               ICTH_Comp = IC171Q01JA,
                               ICTH_Mob_int_access = IC171Q02JA,
                               ICTH_Tab_eBook = IC171Q03JA,
                               ICTH_Internet_access = IC171Q04JA,
                               ICTH_edu_apps_games = IC171Q05JA,
                               ICTH_gaming_platforms = IC171Q06JA
                               )
View(PISA2022_GBR_cleaned2)
#--------------------------------------Recoding Values--------------------------------------------#
#Version 1
#1 & 6---->0 (not available/never use)
# 2---->1 (atleast once a month)
#3--->2 (atleast once a week)
#4---->3 (almost everyday)
#5---->4 (several times a day)
#vars() func. specifies range of columns to be mutated
#mutate_at() func. is used when values or elements need to be transformed/ replaced by other values/characters
#recode func. is to change values as commanded in the specific columns.
PISA2022_GBR_cleaned3 <- PISA2022_GBR_cleaned2%>%
                         select (-ST004D01T, -PV1MATH, -PV2MATH, -PV3MATH, -PV4MATH, -PV5MATH,
                                 -PV6MATH, -PV7MATH, -PV8MATH, -PV9MATH, -PV10MATH) %>%
                        mutate_at(vars(ICTS_Comp:ICTH_gaming_platforms), 
                       ~ recode(., '1' = 0, '2' = 1, '3' = 2, '4'= 3, '5' = 4, '6' = 0)) 

View(PISA2022_GBR_cleaned3)

#Version 2
# 1 ---->0 (never use)
#6-----> 1 (not available)
#2----> (atleast once a month)
#3---> (atleast once a week)
#4----> (almost everyday)
#5----> (several times a day)

PISA2022_GBR_cleaned3 <- PISA2022_GBR_cleaned2%>%
                        select (-ST004D01T, -PV1MATH, -PV2MATH, -PV3MATH, -PV4MATH, -PV5MATH,
                             -PV6MATH, -PV7MATH, -PV8MATH, -PV9MATH, -PV10MATH) %>%
                              mutate_at(vars(ICTS_Comp:ICTH_gaming_platforms), 
                       ~ recode(., '1' = 0, '6' = 1)) #Version 2

#View(PISA2022_GBR_cleaned3)




#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#





###---------------------------5.1 Analysis of ICT Scores: PISA 2022 -------------------###

#na.rm=TRUE is to ignore NA values in the calculation if any
PISA2022_GBR_cleaned4 <- PISA2022_GBR_cleaned3 %>%
                         mutate(ICTS_Sum = rowSums(select(., ICTS_Comp:ICTS_LMS), na.rm = TRUE)) %>%
                         mutate(ICTH_Sum = rowSums(select(., ICTH_Comp:ICTH_gaming_platforms),
                                                   na.rm = TRUE))
#View(PISA2022_GBR_cleaned4)



#----------------------Average ICT scores based on location------------------------#
# Reshape the data into long format
PISA2022_GBR_dataframe<- PISA2022_GBR_cleaned4 %>%
  pivot_longer(cols = c(ICTS_Sum, ICTH_Sum),
               names_to = "ICTuse_location",
               values_to = "Score")

summary_stats1 <- PISA2022_GBR_dataframe %>%
  group_by(ICTuse_location) %>%
  summarise(
    Median = median(Score, na.rm = TRUE),
    Mean = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  )
summary_stats1
#output 

#  ICTuse_location Median  Mean
#<chr>            <dbl>    <dbl>
#1 ICTH_Sum        15      14.3
#2 ICTS_Sum        12      12.3

##......................VISUALISATION......................................##
#Visualizing through Box Plots
# Creating box plots with median and mean labels

#Box Plot : Comparison of ICT Scores by location
ggplot(PISA2022_GBR_dataframe, aes(x = ICTuse_location, y = Score, fill = ICTuse_location)) +
  geom_boxplot() +
  geom_text(data = summary_stats1, aes(x = ICTuse_location, y = Median, label = round(Median, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, color = "black") +
  geom_text(data = summary_stats1, aes(x = ICTuse_location, y = Mean, label = round(Mean, 1)),
            position = position_dodge(width = 0.75), vjust = 1.5, color = "red") +
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
    legend.position = "bottom"   #position of Legend
  )



#----------------------Average ICT scores of the two genders based on location------------------------#

# Calculate median and mean values for each group
summary_stats2 <- PISA2022_GBR_dataframe %>%
  group_by(Sex, ICTuse_location) %>%
  summarise(
    Median = median(Score, na.rm = TRUE),
    Mean = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  )
summary_stats2
#output:
#  Sex   ICTuse_location Median  Mean
#<chr>    <chr>            <dbl> <dbl>
#1 F     ICTH_Sum            15  14.5
#2 F     ICTS_Sum            12  12.4
#3 M     ICTH_Sum            14  14.1
#4 M     ICTS_Sum            12  12.2

##......................VISUALISATION.......................................##
#Box Plot : Comparison of ICT Scores by gender

ggplot(PISA2022_GBR_dataframe, aes(x = Sex, y = Score, fill = ICTuse_location)) +
  geom_boxplot() +
  geom_text(data = summary_stats2, aes(x = Sex, y = Median, label = round(Median, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, color = "black") +
  geom_text(data = summary_stats2, aes(x = Sex, y = Mean, label = round(Mean, 1)),
            position = position_dodge(width = 0.75), vjust = 1.5, color = "red") +
  scale_fill_manual(values = c("#0AD3C6", "#EED010")) +  # Custom fill colors
  labs(title = "Distribution of ICT Scores by Gender in PISA 2022",
       x = "Gender",
       y = "Score",
       fill = "Location of ICT use") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 18,colour = "steelblue", face = "bold", hjust = 0.5),  # Title styling
    axis.title.x = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # X-axis title styling
    axis.title.y = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # Y-axis title styling
    legend.title = element_text(size = 12),  # Legend title styling
    legend.text = element_text(size = 10)  # Legend text styling
  )

#Normality Check
# Filter data for ICTH Sum of boys and girls for 2022
boys_2022df <- PISA2022_GBR_cleaned4 %>% filter(Sex == "M")
girls_2022df <- PISA2022_GBR_cleaned4 %>% filter(Sex == "F")

#............ICT School Usage ....................
#Q-Q plot of TCTS Sum for boys
qqnorm(boys_2022df$ICTS_Sum, main="Q-Q Plot of ICTS Sum for Boys 2022") 
qqline(boys_2022df$ICTS_Sum, col = "red")
#The graph looks quite normal


#Q-Q plot of TCTS Sum for girls
qqnorm(girls_2022df$ICTS_Sum, main = "Q-Q Plot of ICTS Sum for Girls 2022") 
qqline(girls_2022df$ICTS_Sum, col = "blue")
#The graph looks quite normal

#............ICT Home Usage ....................
#Q-Q plot of TCTH Sum for boys
qqnorm(boys_2022df$ICTH_Sum, main="Q-Q Plot of ICTH Sum for Boys 2022") 
qqline(boys_2022df$ICTH_Sum, col = "red")
#The graph looks quite normal

#Q-Q plot of TCTH Sum for girls
qqnorm(girls_2022df$ICTH_Sum, main = "Q-Q Plot of ICTH Sum for Girls 2022") 
qqline(girls_2022df$ICTH_Sum, col = "blue")
#The graph looks quite normal


#histogram 1
ggplot(boys_2022df, aes(x = ICTH_Sum)) +
  geom_histogram(binwidth = 1, fill = "#E9F310",colour = "black", alpha = 0.5) +
  ggtitle("Plot for ICTH Scores of Boys 2022")+
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
ggplot(boys_2022df, aes(x = ICTS_Sum)) +
  geom_histogram(binwidth = 1, fill = "#44e7c7",colour = "black", alpha = 0.5) +
  ggtitle("Plot for ICTS Scores of Boys 2022")+
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
ggplot(girls_2022df, aes(x = ICTH_Sum)) +
  geom_histogram(binwidth = 1, fill = "lightpink",colour = "black", alpha = 0.5) +
  ggtitle("Histogram Plot for ICTH Scores of Girls 2022")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, colour = "darkblue", face = "bold"),
        axis.title = element_text(size = 13, face = "bold", colour = "steelblue"),#
        axis.text = element_text(size = 13, face = "bold", colour = "steelblue")
  ) +
  labs(
    x = "ICTH Scores",
    y ="Frequency" 
  )


#histogram 4
ggplot(girls_2022df, aes(x = ICTS_Sum)) +
  geom_histogram(binwidth = 1, fill = "#38e4f5",colour = "black", alpha = 0.5) +
  ggtitle("Histogram Plot for ICTS Scores of Girls 2022")+
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
#Q-Q plot of ICTH Scores of PISA 2022
qqnorm(PISA2022_GBR_cleaned4$ICTH_Sum, main="Q-Q Plot for ICTH Scores of PISA 2022") 
qqline(PISA2022_GBR_cleaned4$ICTH_Sum, col = "red")

#Q-Q plot of ICTS Scores of PISA 2022
qqnorm(PISA2022_GBR_cleaned4$ICTS_Sum, main="Q-Q Plot for ICTS Scores of PISA 2022") 
qqline(PISA2022_GBR_cleaned4$ICTS_Sum, col = "red")

#histogram 1
ggplot(PISA2022_GBR_cleaned4, aes(x = ICTH_Sum)) +
  geom_histogram(binwidth = 1, fill = "#d485f2",colour = "black", alpha = 0.5) +
  ggtitle("Plot for ICTH Scores of Students 2022")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, colour = "#c90d4c", face = "bold"),
        axis.title = element_text(size = 13, face = "bold", colour = "#400dc9"),#
        axis.text = element_text(size = 13, face = "bold", colour = "#400dc9")
  ) +
  labs(
    x = "ICTH Scores",
    y ="Frequency" 
  )


#histogram 2
ggplot(PISA2022_GBR_cleaned4, aes(x = ICTS_Sum)) +
  geom_histogram(binwidth = 1, fill = "#acf58a",colour = "black", alpha = 0.5) +
  ggtitle("Plot for ICTS Scores of Students 2022")+
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

# Since the above visualisations of the data seem similar to a normal distribution, we proceed with T-Test
# H0(Null Hypothesis): There is no significant difference in the avg. ICT scores of students at home and school  within a year(2022)
# H1: The average ICT use score at home is significantly different from the average ICT use score at 
#school within a year(2022)

# Paired T-Test for ICT usage within each year
#The paired t-test is more appropriate when comparing two related groups (e.g., ICT use at home and school 
#for the same students) because it accounts for the pairing of the data points.
t.test(PISA2022_GBR_cleaned4$ICTH_Sum, 
       PISA2022_GBR_cleaned4$ICTS_Sum,
       paired = TRUE)
#1. p-value < 2.2e-16 <0.05, reject H0=> There is a significant difference in the ICT scores of students at home and school

#2. 95 percent confidence interval: 1.904456 2.157362

#3. mean difference 2.030909 


#H0: The avg. ICT score at home is less than or equal to that of school 
#H1: The avg. ICT score at home is significantly greater than that at school

t.test(PISA2022_GBR_cleaned4$ICTH_Sum, PISA2022_GBR_cleaned4$ICTS_Sum, 
       paired = TRUE, 
       alternative = "greater")
#1. p-value < 2.2e-16 <0.05 => reject H0=>The avg. ICT score at home is significantly greater than that at school
#2. 95 percent confidence interval: 1.92479     Inf
#3. mean difference 2.030909 


#the below t-test is different in the sense the two variables ICTH and ICTS 
#are considered as two independent groups and showing the result
#t.test(Score ~ ICTuse_location, data = PISA2022_GBR_dataframe)
#1. p-value < 2.2e-16
#2.95 percent confidence interval: 1.862516 2.199302
#3. mean in group ICTH_Sum mean in group ICTS_Sum 14.30595    12.27505 






#.............................T- Test...........................................
##............................TEST 2##..........................................

# Since the above visualisations of the data seem close to normal distribution , we proceed with T-Test
# H0(Null Hypothesis): There is no significant difference in the avg. ICT scores of boys and girls at home within a year(2022)
# H1: There is a significant difference in the avg. ICT scores of boys and girls at home within a year(2022)

# T-Test for home usage within each year
t_test_ICTH_2022 <- t.test(ICTH_Sum ~ Sex, data = PISA2022_GBR_cleaned4)
t_test_ICTH_2022

#output: 1. p-value < 2.577e-05 < 0.05
#Interpretation: reject H0 => there is a significant difference in the avg. ICT scores of 
#boys and girls at home within a year(2022)

#2. 95 percent confidence interval: 0.2417300 0.6629526

#To check whether the avg. ICTH score of girls is greater than boys?
#H0: The average ICTH score for girls is less than or equal to the average ICTH score for boys.
#H1: The average ICTH score for girls is greater than the average ICTH score for boys.
t.test(ICTH_Sum ~ Sex, data = PISA2022_GBR_cleaned4, alternative = "greater")

#Output
#1. p-value = 1.289e-05 <0.05 => reject H0 => The average ICTH score for girls is greater 
#than the average ICTH score for boys.
#2. 95 percent confidence interval: 0.2755962       Inf
#3. mean in group F mean in group M  : 14.53147        14.07912 



# T-Test for school usage within a year
# H0(Null Hypothesis): There is no significant difference in the avg. ICT scores of boys and girls at schools within a year(2022)
# H1: There is a significant difference in the avg. ICT scores of boys and girls at schools within a year(2022)

t_test_ICTS_2022 <- t.test(ICTS_Sum ~ Sex, data = PISA2022_GBR_cleaned4)
t_test_ICTS_2022

#output: 1.  p-value = 0.2179 > 0.05,
# Interpretation: so we fail to reject the null hypothesis => there is no significant difference in the 
#ICT scores of boys and girls at schools within a year(2022)

#2. 95 percent confidence interval: -0.09752645  0.42761626
#3. mean in group F = 12.35733, mean in group M : 12.19228



#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#




###---------------------5.2 Analysis of Average Mathematics Scores: PISA 2022-------------------###
#To check PV avg. of students for the year 2022
PISA2022_GBR_cleaned5<- PISA2022_GBR_cleaned4 %>%
  summarise(
    PV_average = mean(PV_avg, na.rm = TRUE))
PISA2022_GBR_cleaned5
#output: 494.8937

#-----------------------------Average Mathematics scores of Boys and Girls------------------------#
#To compare the average PV Math values by sex
PISA2022_GBR_cleaned6<- PISA2022_GBR_cleaned4 %>%
  group_by(Sex) %>%
  summarise(
    PV_average = mean(PV_avg, na.rm = TRUE))
View(PISA2022_GBR_cleaned6)
#Output
# F = 501.5336, M= 488.2149
#---------------------------------------Normality check through Q-Q plots-----------------------------#

qqnorm(PISA2022_GBR_cleaned4$PV_avg[PISA2022_GBR_cleaned4$Sex=="M"], main="Q-Q Plot for Boys PV_avg") 
qqline(PISA2022_GBR_cleaned4$PV_avg[PISA2022_GBR_cleaned4$Sex=="M"], col = "red")

qqnorm(PISA2022_GBR_cleaned4$PV_avg[PISA2022_GBR_cleaned4$Sex=="F"], main="Q-Q Plot for Girls PV_avg") 
qqline(PISA2022_GBR_cleaned4$PV_avg[PISA2022_GBR_cleaned4$Sex=="F"], col = "blue")


#.............................T- Test...........................................
##............................TEST 3##..........................................
#T-Test: to check whether this difference is significant between boys and girls
#H0: There is no significant difference in the average mathematics scores between boys and girls
#H1: There is a significant difference in the average mathematics scores between boys and girls
t.test(PV_avg ~ Sex, data = PISA2022_GBR_cleaned4)
#output
#p-value = 9.341e-13(extremely small)<0.05 => reject H0 => There is a significant difference




#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#





###---------------5.3 Different Levels of ICT use and mathematics scores PISA 2022-------------###
#With Version 1 of Recoded values
PISA2022_GBR_dataframe3 <- PISA2022_GBR_cleaned4 %>%
  mutate(ICT_SUM = ICTH_Sum + ICTS_Sum) 

summary_stats3 <-PISA2022_GBR_dataframe3%>%
  summarise (
    mean_ICT_SUM = mean(ICT_SUM, na.rm = TRUE),
    median_ICT_SUM = median(ICT_SUM, na.rm = TRUE)
  )
summary_stats3
#output
# mean_ICT_SUM = 26.581
#median_ICT_SUM = 26 (we will set this median value as a conditional value to differentiate b/w high and low frequency)

PISA2022_GBR_dataframe3 <- PISA2022_GBR_cleaned4 %>%
  mutate(ICT_SUM = ICTH_Sum + ICTS_Sum, 
         category= ifelse(ICT_SUM <= 26, "Low Frequency ICT Use", 
                          "High Frequency ICT Use")
  )
summary_stats4 <- PISA2022_GBR_dataframe3 %>%
  group_by(category) %>%
  summarise( Avg_PV_Math = mean(PV_avg, na.rm = TRUE)
  )
summary_stats4
#Output
# A tibble: 2 × 2
#category               Avg_PV_Math
#<chr>                        <dbl>
#1 High Frequency ICT Use      494.
#2 Low Frequency ICT Use       496.


##......................VISUALISATION......................................##
#Boxplot: Comparing the mean math scores of the students with low and high frequency ICT use
ggplot(PISA2022_GBR_dataframe3, aes(x = category, y = PV_avg, fill = category)) +
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
qqnorm(PISA2022_GBR_dataframe3$ICT_SUM, main="Q-Q Plot for ICT_SUM PISA 2022") 
qqline(PISA2022_GBR_dataframe3$ICT_SUM, col = "blue")

#T-Test
#Let us check whether there is any significant difference in the avg. mathematics scores of students
#with high and low levels of ICT use

#H0: There is no significant difference in the  avg. mathematics scores of students
#with high and low levels of ICT use

#H1: There is a significant difference in the avg. mathematics scores of students
#with high and low levels of ICT use


t.test(PV_avg ~ category, data = PISA2022_GBR_dataframe3)
#p-val = 0.3477 > 0.05 => fail to reject H0 => There is no significant difference in the scores avg. 
#mathematics scores of students with high and low levels of ICT use

#95% confidence interval: -5.420843  1.909395




#-----------------Different Levels of ICT Use and Mathematics scores: Version 2 of Recoded values-------------#
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

#------------------------------------Digital Devices for School Use--------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTS_Comp", "PV_avg")
output$result
#output
#        ICTS_Comp count PV_average
#            <dbl> <int>    <dbl>
#1         0       712       483.
#2         1       133       449.
#3         2       1469      504.
#4         3       3492      498.
#5         4       2035      492.
#6         5       1412      492.
#usage of computer/laptop seems to have a positive effect on the scores, let us check whether the difference in the scores of 
#those who never use computer/laptop and those who use is significant
PISA2022_GBR_cleaned4a<- PISA2022_GBR_cleaned3 %>% 
                         mutate(ICTS_Comp_cat = factor(ICTS_Comp,
                                               levels = c(0,1,2,3,4,5),
                                               labels = c("Never Use",
                                                "Not Available",
                                                "atleast once a month",
                                                "atleast once a week",
                                                "almost everyday",
                                                "several times a day")))

#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the six groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_test4a<- aov(PV_avg ~ factor(ICTS_Comp), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4a)
#p-value=(extremely small)6.6e-14<0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4a)
#Output
#        diff        lwr        upr     p adj
#1-0 -33.7222071 -57.828911 -9.6155032 0.0009519
#2-0  20.8287096   9.175321 32.4820984 0.0000053
#3-0  15.6199798   5.126258 26.1137013 0.0003212
#4-0   8.8964068  -2.215342 20.0081558 0.2014202
#5-0   9.3061699  -2.423754 21.0360941 0.2101969
#2-1  54.5509167  31.442519 77.6593144 0.0000000
#3-1  49.3421868  26.796350 71.8880239 0.0000000
#4-1  42.6186139  19.778571 65.4586564 0.0000016
#5-1  43.0283769  19.881289 66.1754653 0.0000018
#3-2  -5.2087299 -13.144917  2.7274572 0.4203902
#4-2 -11.9323028 -20.669341 -3.1952645 0.0013980
#5-2 -11.5225398 -21.033375 -2.0117041 0.0073504
#4-3  -6.7235730 -13.840634  0.3934883 0.0767365
#5-3  -6.3138099 -14.361960  1.7343401 0.2212575
#5-4   0.4097631  -8.429100  9.2486257 0.9999944

#significant differences
#1. 1-0, 2-0, 3-0, 2-1, 3-1, 4-1, 5-1, 4-2, 5-2

#Insignificant differences
#4-0, 5-0, 3-2, 4-3, 5-3, 5-4


#------------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTS_Mob_int_acess", "PV_avg")
output$result
#output
#            ICTS_Mob_int_acess count PV_average
#              <dbl>            <int>      <dbl>
#1                  0           1992       506.
#2                  1           428        489.
#3                  2           1059       510.
#4                  3           1501       501.
#5                  4           1965       486.
#6                  5           2308       483.

#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the six groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_test4b<- aov(PV_avg ~ factor(ICTS_Mob_int_acess), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4b)
#p-value(extremely small) <2e-16 <0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4b)
#Output
#        diff        lwr        upr     p adj
#1-0 -16.554892 -30.105393  -3.004392 0.0066299     #statistically significant
#2-0   4.217195  -5.455393  13.889782 0.8157550     #not statistically significant
#3-0  -4.564305 -13.257482   4.128871 0.6665415     #not statistically significant
#4-0 -20.186189 -28.272888 -12.099490 0.0000000     #statistically significant
#5-0 -23.142549 -30.920870 -15.364228 0.0000000     #statistically significant
#2-1  20.772087   6.204091  35.340084 0.0006912     #statistically significant
#3-1  11.990587  -1.946374  25.927548 0.1387364     #not statistically significant
#4-1  -3.631296 -17.198252   9.935659 0.9736324     #not statistically significant
#5-1  -6.587657 -19.973091   6.797777 0.7254242     #not statistically significant
#3-2  -8.781500 -18.988447   1.425447 0.1387355     #not statistically significant
#4-2 -24.403384 -34.099009 -14.707758 0.0000000     #statistically significant
#5-2 -27.359744 -36.799698 -17.919790 0.0000000     #statistically significant
#4-3 -15.621883 -24.340686  -6.903080 0.0000050     #statistically significant
#5-3 -18.578244 -27.011814 -10.144674 0.0000000     #statistically significant
#5-4  -2.956361 -10.763312   4.850591 0.8897576     #not statistically significant

#significant differences
#1. 1-0, 4-0, 5-0, 2-1, 4-2, 5-2, 4-3, 5-3

#Insignificant differences
#2-0, 3-0, 3-1, 4-1, 5-1, 3-2, 5-4 


#----------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTS_Tab_eBook", "PV_avg")
output$result
#output
#        ICTS_Tab_eBook count PV_average
#                <dbl> <int>     <dbl>
#1              0      5170      505.
#2              1      821       507.
#3              2      980       493.
#4              3      830       469.
#5              4      685       459.
#6              5      767       476.

#-----------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTS_Internet_access", "PV_avg")
output$result

#        ICTS_Internet_access count PV_average
#                      <dbl> <int>      <dbl>
#1                    0      969        483.
#2                    1      246        453.
#3                    2      1005       506.
#4                    3      2330       505.
#5                    4      2263       492.
#6                    5      2440       492.


#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the six groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_test4c<- aov(PV_avg ~ factor(ICTS_Internet_access), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4c)
#p-value(extremely small) < 2e-16 < 0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4c)
#Output
#        diff         lwr        upr     p adj
#1-0 -29.0843684 -47.2517920 -10.916945 0.0000748     #statistically significant
#2-0  23.3740959  11.9172967  34.830895 0.0000001     #statistically significant
#3-0  22.9371647  13.2100070  32.664322 0.0000000     #statistically significant
#4-0   9.8882252   0.1188642  19.657586 0.0453124     #statistically significant
#5-0   9.1011807  -0.5613602  18.763722 0.0783587     #not significant
#2-1  52.4584642  34.3570412  70.559887 0.0000000     #statistically significant
#3-1  52.0215331  34.9621994  69.080867 0.0000000     #statistically significant
#4-1  38.9725936  21.8891606  56.056027 0.0000000     #statistically significant
#5-1  38.1855490  21.1629768  55.208121 0.0000000     #statistically significant
#3-2  -0.4369312 -10.0402550   9.166393 0.9999949     #not significant
#4-2 -13.4858707 -23.1319397  -3.839802 0.0009609     #statistically significant
#5-2 -14.2729152 -23.8107834  -4.735047 0.0002908     #statistically significant
#4-3 -13.0489395 -20.5593320  -5.538547 0.0000111     #statistically significant
#5-3 -13.8359840 -21.2068916  -6.465076 0.0000013     #statistically significant
#5-4  -0.7870445  -8.2135576   6.639468 0.9996651     #not significant

#significant differences
#1. 1-0, 2-0, 3-0, 4-0, 2-1, 3-1, 4-1, 5-1, 4-2, 5-2, 4-3, 5-3

#Insignificant differences
#5-0, 3-2, 5-4 


#--------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTS_School_portal", "PV_avg")
output$result
#output
#      ICTS_School_portal count PV_average
#                  <dbl> <int>      <dbl>
#1                  0    3901       498.
#2                  1     617       500.
#3                  2    1071       500.
#4                  3    1265       492.
#5                  4    1312       490.
#6                  5    1087       486.

#---------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTS_edu_apps_games", "PV_avg")
output$result

#        ICTS_edu_apps_games count PV_average
#                      <dbl> <int>      <dbl>
#1                        0  2563       499.
#2                        1   344       477.
#3                        2  1713       507.
#4                        3  2331       500.
#5                        4  1345       483.
#6                        5   957       472.

ANOVA_test4d<-aov(PV_avg ~ factor(ICTS_edu_apps_games), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4d)
#p-value(extremely small) < 2e-16 < 0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4d)
#        diff         lwr         upr     p adj
#1-0 -22.8642461 -37.4563853  -8.2721068 0.0001175    #not significant
#2-0   8.0589229   0.1281441  15.9897017 0.0438584    #significant
#3-0   0.1838313  -7.0895527   7.4572153 0.9999997    #not significant
#4-0 -16.3228343 -24.8792487  -7.7664200 0.0000008    #significant
#5-0 -27.6721682 -37.2991695 -18.0451668 0.0000000    #significant
#2-1  30.9231690  15.9087263  45.9376117 0.0000001    #significant
#3-1  23.0480774   8.3702590  37.7258957 0.0001124    #significant
#4-1   6.5414118  -8.8126907  21.8955142 0.8300574    #not significant
#5-1  -4.8079221 -20.7833843  11.1675401 0.9563224    #not significant
#3-2  -7.8750916 -15.9624321   0.2122489 0.0615138    #not significant
#4-2 -24.3817572 -33.6400070 -15.1235075 0.0000000    #significant
#5-2 -35.7310911 -45.9869244 -25.4752577 0.0000000    #significant
#4-3 -16.5066656 -25.2083925  -7.8049388 0.0000010    #significant
#5-3 -27.8559994 -37.6123809 -18.0996180 0.0000000    #significant
#5-4 -11.3493338 -22.0962911  -0.6023765 0.0313816    #significant



#--------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTS_LMS", "PV_avg")
output$result
#output
#          ICTS_LMS count PV_average
#          <dbl>   <int>      <dbl>
#1        0        2046       494.
#2        1        308        470.
#3        2        1067       499.
#4        3        2098       500.
#5        4        2112       495.
#6        5        1622       492.

ANOVA_test4e<-aov(PV_avg ~ factor(ICTS_LMS), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4e)
#p-value(extremely small) = 1.62e-06 < 0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4e)
#          diff        lwr        upr     p adj
#1-0 -23.5466038 -39.174387 -7.9188209 0.0002555    #significant
#2-0   5.4983799  -4.157166 15.1539261 0.5831462    #not significant
#3-0   6.2376349  -1.707049 14.1823190 0.2204607    #not significant
#4-0   1.8753623  -6.056310  9.8070350 0.9848111    #not significant
#5-0  -1.9055154 -10.406295  6.5952638 0.9880751    #not significant
#2-1  29.0449836  12.505723 45.5842444 0.0000084    #significant
#3-1  29.7842387  14.181817 45.3866609 0.0000008    #significant
#4-1  25.4219661   9.826165 41.0177669 0.0000504    #significant
#5-1  21.6410883   5.748297 37.5338798 0.0014638    #significant
#3-2   0.7392551  -8.875190 10.3537000 0.9999310    #not significant
#4-2  -3.6230176 -13.226714  5.9806785 0.8913375    #not significant
#5-2  -7.4038953 -17.482724  2.6749337 0.2904767    #not significant
#4-3  -4.3622727 -12.243859  3.5193141 0.6134629    #not significant
#5-3  -8.1431504 -16.597216  0.3109153 0.0667165    #not significant
#5-4  -3.7808777 -12.222717  4.6609617 0.7979604    #not significant


#------------------------------------Digital Devices for Home Use--------------------------------#

output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTH_Comp", "PV_avg")
output$result
#output
#      ICTH_Comp count PV_average
#          <dbl> <int>     <dbl>
#1         0     915       461.
#2         1     174       459.
#3         2     958       476.
#4         3     2124      493.
#5         4     2558      507.
#6         5     2524      506.

#-------------------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTH_Mob_int_access", "PV_avg")
output$result

#         ICTH_Mob_int_access count PV_average
#                       <dbl> <int>     <dbl>
#1                        0    306       446.
#2                        1    156       444.
#3                        2    292       434.
#4                        3    595       466.
#5                        4   2110       489.
#6                        5   5794       507.


#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the six groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_test4f<- aov(PV_avg ~ factor(ICTH_Mob_int_access), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4f)
#p-value(extremely small) < 2e-16 < 0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4f)
#Output
#        diff         lwr       upr     p adj
#1-0  -1.585271 -26.1631255 22.992584 0.9999711       #not significant
#2-0 -11.611026 -32.0493114  8.827259 0.5857177       #not significant
#3-0  20.152710   2.5779578 37.727461 0.0138450       #statistically significant
#4-0  42.979595  27.6971611 58.262029 0.0000000       #statistically significant
#5-0  61.302701  46.6485404 75.956863 0.0000000       #statistically significant
#2-1 -10.025756 -34.8017609 14.750250 0.8587636       #not significant
#3-1  21.737980  -0.7341989 44.210159 0.0646468       #not significant
#4-1  44.564866  23.8361483 65.293583 0.0000000       #statistically significant
#5-1  62.887972  42.6180098 83.157934 0.0000000       #statistically significant
#3-2  31.763736  13.9129269 49.614545 0.0000060       #statistically significant
#4-2  54.590621  38.9915102 70.189732 0.0000000       #statistically significant
#5-2  72.913728  57.9296056 87.897850 0.0000000       #statistically significant
#4-3  22.826885  11.2303054 34.423465 0.0000003       #statistically significant
#5-3  41.149992  30.3948854 51.905098 0.0000000       #statistically significant
#5-4  18.323107  11.9706896 24.675523 0.0000000       #statistically significant

#significant differences
#1. 4-0, 5-0, 4-1, 5-1, 3-2, 4-2, 5-2, 4-3, 5-3, 5-4

#Insignificant differences
#1-0, 2-0, 3-0, 2-1, 3-1


#---------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTH_Tab_eBook", "PV_avg")
output$result
#output
#      ICTH_Tab_eBook count PV_average
#             <dbl> <int>      <dbl>
#1              0    3340       506.
#2              1    459        504.
#3              2    1026       502.
#4              3    1353       487.
#5              4    1488       481.
#6              5    1587       485.

#-----------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTH_Internet_access", "PV_avg")
output$result
#              ICTH_Internet_access count PV_average
#                           <dbl> <int>    <dbl>
#1                            0   486       455.
#2                            1   144       443.
#3                            2   380       467.
#4                            3   999       483.
#5                            4   2400      494.
#6                            5   4844      506.

#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the six groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_test4g<- aov(PV_avg ~ factor(ICTH_Internet_access), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4g)
#p-value(extremely small)< 2e-16 < 0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4g)
#Output
#       diff         lwr      upr     p adj
#1-0 -11.54069 -35.4925191 12.41113 0.7431196           #not significant
#2-0  12.08664  -5.2002434 29.37353 0.3463781           #not significant
#3-0  28.61059  14.6491493 42.57202 0.0000001           #statistically significant
#4-0  39.06248  26.5052957 51.61966 0.0000000           #statistically significant
#5-0  50.87279  38.8609070 62.88468 0.0000000           #statistically significant
#2-1  23.62733  -1.0762700 48.33094 0.0702592           #not significant
#3-1  40.15128  17.6489809 62.65358 0.0000055           #statistically significant
#4-1  50.60317  28.9441187 72.26222 0.0000000           #statistically significant
#5-1  62.41348  41.0659539 83.76101 0.0000000           #statistically significant
#3-2  16.52394   1.3088254 31.73906 0.0241532           #statistically significant
#4-2  26.97583  13.0380833 40.91359 0.0000005           #statistically significant
#5-2  38.78615  25.3375998 52.23470 0.0000000           #statistically significant
#4-3  10.45189   0.9468294 19.95695 0.0213874           #statistically significant
#5-3  22.26221  13.4901609 31.03425 0.0000000           #statistically significant
#5-4  11.81031   5.5087376 18.11189 0.0000014           #statistically significant

#significant differences
#1. 3-0, 4-0, 5-0, 3-1, 4-1, 5-1, 3-2, 4-2, 5-2, 4-3, 5-3, 5-4

#Insignificant differences
# 1-0, 2-0, 2-1 

#-------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTH_edu_apps_games", "PV_avg")
output$result
#          ICTH_edu_apps_games count PV_average
#                        <dbl> <int>      <dbl>
#1                   0          2929       498.
#2                   1          257        477.
#3                   2          1359       502.
#4                   3          2165       504.
#5                   4          1434       488.
#6                   5          1109       473.

#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the six groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_test4h<- aov(PV_avg ~ factor(ICTH_edu_apps_games), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4h)
#p-value(extremely small)<2e-16<0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4h)
#Output
#        diff        lwr        upr     p adj
#1-0 -21.230954 -37.791757  -4.670152 0.0035343     #statistically significant
#2-0   3.767251  -4.587677  12.122178 0.7933021     #not statistically significant
#3-0   6.121511  -1.093310  13.336332 0.1498838     #not statistically significant
#4-0 -10.202728 -18.407058  -1.998399 0.0052977     #statistically significant
#5-0 -24.518961 -33.494126 -15.543796 0.0000000     #statistically significant
#2-1  24.998205   7.682941  42.313469 0.0005576     #statistically significant
#3-1  27.352466  10.557609  44.147322 0.0000514     #statistically significant
#4-1  11.028226  -6.214876  28.271328 0.4508386     #not statistically significant
#5-1  -3.288007 -20.910922  14.334908 0.9949007     #not statistically significant
#3-2   2.354261  -6.455498  11.164019 0.9738150     #not statistically significant
#4-2 -13.969979 -23.606847  -4.333111 0.0005189     #statistically significant
#5-2 -28.286212 -38.587269 -17.985154 0.0000000     #statistically significant
#4-3 -16.324240 -24.991306  -7.657173 0.0000012     #statistically significant
#5-3 -30.640472 -40.040505 -21.240440 0.0000000     #statistically significant
#5-4 -14.316233 -24.495524  -4.136941 0.0008711     #statistically significant

#significant differences
#1. 1-0, 4-0, 5-0, 2-1, 3-1, 4-2, 5-2, 4-3, 5-3, 5-4

#Insignificant differences
# 2-0, 3-0, 4-1, 5-1, 3-2

#---------------------------------------------------------------------------------------------------------#
output <-ICTuse_mathscore_comp(PISA2022_GBR_cleaned3, "ICTH_gaming_platforms", "PV_avg")
output$result
#output
#      ICTH_gaming_platforms count PV_average
#                       <dbl> <int>     <dbl>
#1                     0      1899       489.
#2                     1       273       471.
#3                     2       904       501.
#4                     3      1591       502.
#5                     4      1946       494.
#6                     5      2640       496.
#ANOVA Test
#H0:There is no significant difference in the average mathematics scores of the six groups
#H1: There is significant difference in the average math scores of between at least two of the groups
ANOVA_test4i<- aov(PV_avg ~ factor(ICTH_gaming_platforms), data = PISA2022_GBR_cleaned3)
summary(ANOVA_test4i)
#p-value(extremely small)=1.1e-07<0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_test4i)
#Output
#       diff         lwr       upr     p adj
#1-0 -17.823137 -34.3683261 -1.277949 0.0261026     #statistically significant
#2-0  11.538153   1.2093391 21.866968 0.0182446     #statistically significant
#3-0  12.822560   4.1349406 21.510178 0.0003755     #statistically significant
#4-0   5.414478  -2.8306977 13.659653 0.4197478
#5-0   6.880551  -0.8107755 14.571878 0.1100938
#2-1  29.361291  11.7087056 47.013876 0.0000318     #statistically significant
#3-1  30.645697  13.9004265 47.390967 0.0000028     #statistically significant
#4-1  23.237615   6.7175583 39.757672 0.0008688     #statistically significant
#5-1  24.703689   8.4529698 40.954408 0.0002154     #statistically significant
#3-2   1.284406  -9.3619648 11.930777 0.9993692
#4-2  -6.123676 -16.4121843  4.164833 0.5341020
#5-2  -4.657602 -14.5078305  5.192626 0.7581637
#4-3  -7.408082 -16.0477418  1.231578 0.1413925
#5-3  -5.942008 -14.0547953  2.170779 0.2937528
#5-4   1.466074  -6.1710401  9.103188 0.9941730



#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#



###...........................5.4 Analysis of ICT use during Mathematics Lessons and Mathematics Scores: PISA 2022...............##

PISA2022_GBR_new1<- PISA2022_GBR[, c(1, 2, 26, 655, 1167:1176)]

PISA2022_GBR_new1_cleaned <- na.omit(PISA2022_GBR_new1)
dim(PISA2022_GBR_new1_cleaned)

#renaming columns
PISA2022_GBR_new1_cleaned2 <-PISA2022_GBR_new1_cleaned %>% 
                             mutate(Sex = ifelse(ST004D01T == 1, "M", 
                                          ifelse(ST004D01T == 2, "F", NA)))%>%
                             mutate(PV_avg = rowMeans(select(.,PV1MATH:PV10MATH), na.rm = TRUE)) %>%
                             rename(Mathematics = IC173Q02JA)

View(PISA2022_GBR_new1_cleaned2)

PISA2022_GBR_new1_cleaned3 <- PISA2022_GBR_new1_cleaned2%>%
                              select (-ST004D01T, -PV1MATH, -PV2MATH, -PV3MATH, -PV4MATH, -PV5MATH,
                                      -PV6MATH, -PV7MATH, -PV8MATH, -PV9MATH, -PV10MATH) %>%
                              mutate_at(vars(Mathematics), ~ recode(., '1' = 0, 
                                                                    '2' = 1, 
                                                                    '3' = 1, 
                                                                    '4'= 2, 
                                                                    '5' = 3, 
                                                                      )) 
View(PISA2022_GBR_new1_cleaned3)
PISA2022_GBR_new1_cleaned3a <- PISA2022_GBR_new1_cleaned3 %>%
                               filter(Mathematics != 6)
View(PISA2022_GBR_new1_cleaned3a)

PISA2022_GBR_new1_cleaned4 <- PISA2022_GBR_new1_cleaned3 %>%
                              mutate(Math_ICT_cat = factor(ifelse(Mathematics ==0, "No ICT Use",
                                                           ifelse(Mathematics %in% c(1,2), "Low ICT Use",
                                                           ifelse(Mathematics == 3,  "Medium ICT Use",
                                                                  "High ICT Use"))),
                               levels = c("No ICT Use", "Low ICT Use", "Medium ICT Use",
                                          "High ICT Use")))
View(PISA2022_GBR_new1_cleaned4)

PISA2022_GBR_new1_cleaned5<- PISA2022_GBR_new1_cleaned4 %>%
                             group_by(Math_ICT_cat) %>%
                             summarise(
                             PV_Math_average = mean(PV_avg, na.rm = TRUE))
PISA2022_GBR_new1_cleaned5
#output
#    Math_ICT_cat   PV_Math_average
#      <fct>               <dbl>
#1 No ICT Use                493.
#2 Low ICT Use               484.
#3 Medium ICT Use            499.
#4 High ICT Use              455.

#--------------------------------------VISUALISATION ------------------------------------------------------------#
#Boxplot
ggplot(PISA2022_GBR_new1_cleaned4, aes(x = Math_ICT_cat, y = PV_avg, fill = Math_ICT_cat)) +
  geom_boxplot() +
  geom_text(data = PISA2022_GBR_new1_cleaned5, aes(x =  Math_ICT_cat, y = PV_Math_average, 
                                                   label = round(PV_Math_average, 1)),
            vjust = 1.9, color = "red") +
  scale_fill_manual(values = c("#f7dca4", "lightblue", "lightpink", "#c2f960")) +  # Custom fill colors
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
ANOVA_Test<- aov(PV_avg ~ factor(Math_ICT_cat), data = PISA2022_GBR_new1_cleaned4)
summary(ANOVA_Test)
#p-value(extremely small)= 8.19e-15 < 0.05 => fail to reject H1

#Tukey Test
TukeyHSD(ANOVA_Test)
#output
#                                diff         lwr        upr     p adj
#Low ICT Use-No ICT Use       -9.168460 -14.1576366  -4.179284 0.0000141   #statistically significant
#Medium ICT Use-No ICT Use     6.265207  -0.8766903  13.407104 0.1090847    
#High ICT Use-No ICT Use     -38.204668 -55.2553328 -21.154004 0.0000001   #statistically significant
#Medium ICT Use-Low ICT Use   15.433667   8.3526036  22.514731 0.0000001   #statistically significant
#High ICT Use-Low ICT Use    -29.036208 -46.0614811 -12.010935 0.0000700   #statistically significant
#High ICT Use-Medium ICT Use -44.469875 -62.2455537 -26.694197 0.0000000   #statistically significant





