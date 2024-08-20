# Install and Load the below required packages
library(foreign)
library(haven)
library(psych)
library(dplyr)

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
library(tidyr)
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
                               ICTH_edu_apps_videogames = IC171Q05JA,
                               ICTH_gaming_platforms = IC171Q06JA
                               )
View(PISA2022_GBR_cleaned2)

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

#na.rm=TRUE is to ignore NA values in the calculation if any
PISA2022_GBR_cleaned4 <- PISA2022_GBR_cleaned3 %>%
                         mutate(ICTS_Sum = rowSums(select(., ICTS_Comp:ICTS_LMS), na.rm = TRUE)) %>%
                         mutate(ICTH_Sum = rowSums(select(., ICTH_Comp:ICTH_gaming_platforms), na.rm = TRUE))

View(PISA2022_GBR_cleaned4)

#To compare the average PV Math values by sex
PISA2022_GBR_cleaned5<- PISA2022_GBR_cleaned4 %>%
  group_by(Sex) %>%
  summarise(
    PV_average = mean(PV_avg, na.rm = TRUE))
View(PISA2022_GBR_cleaned5)
#Output
# F = 501.5336, M= 488.2149

#To check PV avg. of students for the year 2022
PISA2022_GBR_cleaned6<- PISA2022_GBR_cleaned4 %>%
  summarise(
    PV_average = mean(PV_avg, na.rm = TRUE))
PISA2022_GBR_cleaned6
#output: 494.8937

# Reshape the data into long format
PISA2022_GBR_dataframe<- PISA2022_GBR_cleaned4 %>%
  pivot_longer(cols = c(ICTS_Sum, ICTH_Sum),
               names_to = "ICTuse_location",
               values_to = "Score")


# Calculate median and mean values for each group
summary_stats <- PISA2022_GBR_dataframe %>%
  group_by(Sex, ICTuse_location) %>%
  summarise(
    Median = median(Score, na.rm = TRUE),
    Mean = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  )
summary_stats
#output:
#  Sex   ICTuse_location Median  Mean
#<chr>    <chr>            <dbl> <dbl>
#1 F     ICTH_Sum            15  14.5
#2 F     ICTS_Sum            12  12.4
#3 M     ICTH_Sum            14  14.1
#4 M     ICTS_Sum            12  12.2


summary_stats2 <- PISA2022_GBR_dataframe %>%
  group_by(ICTuse_location) %>%
  summarise(
    Median = median(Score, na.rm = TRUE),
    Mean = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  )
summary_stats2
#output 

#  ICTuse_location Median  Mean
#<chr>            <dbl>    <dbl>
#1 ICTH_Sum        15      14.3
#2 ICTS_Sum        12      12.3



#PISA2022_GBR_dataframe2<- PISA2022_GBR_cleaned4 %>%
#  mutate(Year = 2022)

#View(PISA2022_GBR_dataframe2)

PISA2022_GBR_dataframe3 <- PISA2022_GBR_cleaned4 %>%
                           mutate(ICT_SUM = ICTH_Sum + ICTS_Sum) %>%
                           mutate(category= ifelse(ICT_SUM < 20, "Low Frequency ICT Use", 
                                                   "High Frequency ICT Use")
                           )
summary_stats3 <- PISA2022_GBR_dataframe3 %>%
  group_by(category) %>%
  summarise( Avg_PV_Math = mean(PV_avg, na.rm = TRUE)
  )
summary_stats3
#Output
# A tibble: 2 × 2
#category               Avg_PV_Math
#<chr>                        <dbl>
 # 1 High Frequency ICT Use    497.
#2 Low Frequency ICT Use       486.



##......................VISUALISATIONAS.......................................##
#Visualizing through Box Plots
# Create box plots with median and mean labels
##Box Plot 1: Comparison of ICT Scores by location
ggplot(PISA2022_GBR_dataframe, aes(x = ICTuse_location, y = Score, fill = ICTuse_location)) +
  geom_boxplot() +
  geom_text(data = summary_stats2, aes(x = ICTuse_location, y = Median, label = round(Median, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, color = "black") +
  geom_text(data = summary_stats2, aes(x = ICTuse_location, y = Mean, label = round(Mean, 1)),
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


#Box Plot 2: Comparison of ICT Scores by gender
library(ggplot2)
ggplot(PISA2022_GBR_dataframe, aes(x = Sex, y = Score, fill = ICTuse_location)) +
  geom_boxplot() +
  geom_text(data = summary_stats, aes(x = Sex, y = Median, label = round(Median, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, color = "black") +
  geom_text(data = summary_stats, aes(x = Sex, y = Mean, label = round(Mean, 1)),
            position = position_dodge(width = 0.75), vjust = 1.5, color = "red") +
  scale_fill_manual(values = c("#0AD3C6", "#EED010")) +  # Custom fill colors
  labs(title = "Distribution of ICT Scores by Sex",
       x = "Sex",
       y = "Score",
       fill = "Location of ICT use") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 20,colour = "steelblue", face = "bold", hjust = 0.5),  # Title styling
    axis.title.x = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # X-axis title styling
    axis.title.y = element_text(size = 15, colour = "#D30A6E", face = "bold"),  # Y-axis title styling
    legend.title = element_text(size = 12),  # Legend title styling
    legend.text = element_text(size = 10)  # Legend text styling
  )

#Output:
# mean ICTH score of girls : 14.5
# mean ICTS score of girls : 12.4
# mean ICTH score of boys : 14.1
# mean ICTS score of boys : 12.2

#Boxplot 3
#Comparing the mean math scores of the students with low and high frequency ICT use
ggplot(PISA2022_GBR_dataframe3, aes(x = category, y = PV_avg, fill = category)) +
  geom_boxplot() +
  geom_text(data = summary_stats3, aes(x = category, y = Avg_PV_Math, label = round(Avg_PV_Math, 1)),
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

#...............................Phase 2........................................
#.......................Visualizations for normality...........................
#............ICT Home Usage ....................
#Q-Q plot of ICTH Sum for students
qqnorm(PISA2022_GBR_cleaned4$ICTH_Sum, main="Q-Q Plot Of ICTH Scores 2022") 
qqline(PISA2022_GBR_cleaned4$ICTH_Sum, col = "red")
#The graph looks quite normal

#Q-Q plot of ICTH Sum for students
qqnorm(PISA2022_GBR_cleaned4$ICTS_Sum, main = "Q-Q Plot Of ICTS Scores 2022") 
qqline(PISA2022_GBR_cleaned4$ICTS_Sum, col = "red")
#The graph looks normal

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

# Since the above visualisations of the data seem close to normal distribution , we proceed with T-Test
# H0(Null Hypothesis): There is no significant difference in the avg. ICT scores of students at home and school resp. within a year(2022)
# H1: The average ICT use score at home is significantly different from the average ICT use score at 
#school within a year(2022)

# Paired T-Test for home usage within each year
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
#1. p-value < 2.2e-16 <0.05 => reject H0
#2. 95 percent confidence interval: 1.92479     Inf
#3. mean difference 2.030909 


#the below t-test is different in the sense the two variables ICTH and ICTS 
#are considered as two independent groups and showing the result
t.test(Score ~ ICTuse_location, data = PISA2022_GBR_dataframe)
#1. p-value < 2.2e-16
#2.95 percent confidence interval: 1.862516 2.199302
#3. mean in group ICTH_Sum mean in group ICTS_Sum 14.30595    12.27505 


# Is there a significant difference in the Scores of Girls and Boys?
#1. Checking conditions for a t-test

#Normality
# Filter data for boys and girls for 2018
boys_2022df <- PISA2022_GBR_cleaned4 %>% filter(Sex == "M")
girls_2022df <- PISA2022_GBR_cleaned4 %>% filter(Sex == "F")

#Applying Shapiro-Wilk Test
#null hypothesis: data follows a normal distribution
shapiro.test(boys_2022df$ICTH_Sum)
#output: p value < 2.2e-16, since p value < 0.05(extremely small), we reject null hypothesis

shapiro.test(girls_2022df$ICTH_Sum)
#output: p value < 2.2e-16, since p value < 0.05(extremely small), we reject null hypothesis



#.......................Visualizations for normality...........................
#............ICT Home Usage ....................
#Q-Q plot of TCTH Sum for boys
qqnorm(boys_2022df$ICTH_Sum, main="Q-Q Plot for Boys 2022") 
qqline(boys_2022df$ICTH_Sum, col = "red")
#The graph looks quite normal

#Q-Q plot of TCTH Sum for girls
qqnorm(girls_2022df$ICTH_Sum, main = "Q-Q Plot for Girls 2022") 
qqline(girls_2022df$ICTH_Sum, col = "red")
#The graph looks quite normal

#............ICT School Usage ....................
#Q-Q plot of TCTS Sum for boys
qqnorm(boys_2022df$ICTS_Sum, main="Q-Q Plot for Boys 2022") 
qqline(boys_2022df$ICTS_Sum, col = "red")
#The graph looks quite normal


#Q-Q plot of TCTS Sum for girls
qqnorm(girls_2022df$ICTS_Sum, main = "Q-Q Plot for Girls 2022") 
qqline(girls_2022df$ICTS_Sum, col = "red")
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


#.............................T- Test...........................................
##............................TEST 2##..........................................

# Since the above visualisations of the data seem close to normal distribution , we proceed with T-Test
# H0(Null Hypothesis): There is no significant difference in the avg. ICT scores of boys and girls at home within a year(2022)
# H1: There is a significant difference in the avg. ICT scores of boys and girls at home within a year(2018)

# T-Test for home usage within each year
t_test_ICTH_2022 <- t.test(ICTH_Sum ~ Sex, data = PISA2022_GBR_cleaned4)
t_test_ICTH_2022

#output: 1. p-value < 2.577e-05 < 0.05
#Interpretation: so we fail to accept H0 => there is a significant difference in the avg. ICT scores of 
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



# T-Test for school usage within each year
# H0(Null Hypothesis): There is no significant difference in the avg. ICT scores of boys and girls at schools within a year(2022)
# H1: There is a significant difference in the avg. ICT scores of boys and girls at schools within a year(2018)

t_test_ICTS_2022 <- t.test(ICTS_Sum ~ Sex, data = PISA2022_GBR_cleaned4)
t_test_ICTS_2022

#output: 1.  p-value = 0.2179 > 0.05,
# Interpretation: so we fail to reject the null hypothesis => there is no significant difference in the 
#ICT scores of boys and girls at schools within a year(2022)

#2. 95 percent confidence interval: -0.09752645  0.42761626
#3. mean in group F = 12.35733, mean in group M : 12.19228






#Q-Q plot of ICTH Scores of PISA 2022
qqnorm(PISA2022_GBR_cleaned4$ICTH_Sum, main="Q-Q Plot for ICTH Scores of PISA 2022") 
qqline(PISA2022_GBR_cleaned4$ICTH_Sum, col = "red")



#Q-Q plot of ICTS Scores of PISA 2022
qqnorm(PISA2022_GBR_cleaned4$ICTS_Sum, main="Q-Q Plot for ICTH Scores of PISA 2022") 
qqline(PISA2022_GBR_cleaned4$ICTS_Sum, col = "red")




##.................................................................................##

PISA2022_GBR_new1<- PISA2022_GBR[, c(1, 2, 26, 655, 1167:1176)]
library(tidyr)
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
                                                                    '3' = 2, 
                                                                    '4'= 3, 
                                                                    '5' = 4, 
                                                                    '6' = 0)) 

View(PISA2022_GBR_new1_cleaned3)

PISA2022_GBR_new1_cleaned4 <- PISA2022_GBR_new1_cleaned3 %>%
                              mutate(Math_ICT_cat = factor(ifelse(Mathematics ==0, "No ICT Use",
                                                           ifelse(Mathematics == 1, "Low ICT Use",
                                                           ifelse(Mathematics == 2,  "Medium ICT Use",
                                                                  "High ICT Use"))),
                               levels = c("No ICT Use", "Low ICT Use", "Medium ICT Use",
                                          "High ICT Use")))
View(PISA2022_GBR_new1_cleaned4)

PISA2022_GBR_new1_cleaned5<- PISA2022_GBR_new1_cleaned4 %>%
                             group_by(Math_ICT_cat) %>%
                             summarise(
                             PV_Math_average = mean(PV_avg, na.rm = TRUE))

PISA2022_GBR_new1_cleaned5

library(ggplot2)
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




