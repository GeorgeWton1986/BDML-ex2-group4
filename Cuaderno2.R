## Install Packages

if(!require(pacman)) install.packages("pacman") ; require(pacman)


p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       gridExtra, # visualizing missing data
       corrplot, # Correlation Plots 
       stargazer, # tables/output to TEX. 
       MASS # various statistical functions
)   

# load data
db <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")
db <- as_tibble(db) %>% rename(gender=sex) 

# Create variables

db_of<- db %>% 
  group_by(oficio, gender)  %>% 
  summarise(ofic_ingLab= mean(y_ingLab_m, na.rm=T), .groups="drop") %>%
  mutate(ofic_ingLab= ofic_ingLab/1000000)

db_of  %>% dplyr:: select(oficio, gender, ofic_ingLab) %>% head(4)

# merge with the original data set

db_int <- db %>% inner_join(db_of, by=c("oficio", "gender"))

summary(db_int$totalHoursWorked)
summary(db_int$ofic_ingLab)

# child indicator
db_int <- db_int %>%
  mutate(flag = ifelse(age <= 18, 1, 0))

#  total number of children per household

db_int <- db_int %>%
  group_by(directorio, secuencia_p) %>%
  mutate(nmenores = sum(flag)) %>%
  dplyr::select(-flag) %>% 
  ungroup()


db_int %>% dplyr:: select(directorio, secuencia_p, age , nmenores ) %>% tail()}

table(db_int$p6050)

#the category 1 is Jefe de Hogar.

db_int <- db_int %>%
  mutate(H_Head = ifelse( p6050== 1, 1, 0))

table(db_int$H_Head)

# subset  with people with positive labor force supply participation
db_int= db_int %>% filter(totalHoursWorked>0)

# drop missing values in each variable (but you 
# are invited to check if there is an imputation
#  method that can be useful) this to include 
#  post estimation variables in the data set


features<- c("ofic_ingLab", "nmenores",  "H_Head",  "age",  "gender" )

db_int<- db_int %>% drop_na(any_of(features))



## Regression
linear_model<- lm(totalHoursWorked ~ ofic_ingLab + nmenores  + 
                    nmenores*gender + H_Head + age + gender, data=db_int  )
summary(linear_model)

## leverage 
db_int<- db_int %>% mutate(leverage = hatvalues(linear_model))


## residuals
db_int<- db_int %>% mutate(residuals= linear_model$residuals)

N <- nrow(db_int)

db_int$id<- seq(1 , N)

a<- ggplot(db_int , aes(y = leverage , x = id , color= ofic_ingLab, shape= as.factor(gender) )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Leverage",
       title = "") # labels


b<- ggplot(db_int , aes(y = leverage , x = residuals  )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Residuals",  
       y = "Leverage",
       title = "") # labels


# Arrange the ggplot2 plots side by side using grid.arrange()
grid.arrange(a, b, ncol = 2)

p <- mean(db_int$leverage)
p

cutt <- 3*p
cutt

db_int2 <-  db_int %>% 
  dplyr:: filter(leverage<= cutt)

# re run the model
linear_model2<- lm(totalHoursWorked ~ ofic_ingLab + nmenores  +  nmenores*gender + H_Head + age + gender, data=db_int2  )


stargazer(linear_model, linear_model2, type="text",
          covariate.labels=c("Mean Ocu Income","N under 18","Male",
                             "Hausehold Head","Age", "N under 18 x Male" ))

influential<- db_int %>% 
  filter(leverage> cutt ) %>%
  dplyr:: select(totalHoursWorked, ofic_ingLab,  nmenores,  H_Head, age,  gender)

des_vars<- c("totalHoursWorked", "ofic_ingLab",  "nmenores",  "H_Head", "age",  "gender")

influential<- as.data.frame(influential)

stargazer(influential[des_vars] , type="text")

bf <- as.data.frame(db_int%>% 
                      filter(leverage<= cutt ))
stargazer(bf[des_vars]  , type="text")

##boxplot for Hours Worked

ggplot(data= db_int, 
       mapping = aes(y=totalHoursWorked, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Total Hours Worked")+
  xlab("")

# definition 1

low <- quantile(db_int$totalHoursWorked, 0.01)
up <- quantile(db_int$totalHoursWorked, 0.99)

low

up

b<-ggplot(data= db_int, 
          mapping = aes(y=totalHoursWorked, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Total Hours Worked")+
  xlab("")


b <- b + geom_hline(yintercept = low,linetype="solid",color="blue",linewidth=0.7) +
  geom_hline(yintercept = up,linetype="solid",color="blue",linewidth=0.7)

# Create sample data
set.seed(123)
x <- c(rnorm(97, mean=10, sd=2), 20, 22, 0)  # 97 normal points + 3 outliers
boxplot(x)

# 2-sigma rule
mean_x <- mean(x)
sd_x <- sd(x)
lower_bound <- mean_x - 2*sd_x
upper_bound <- mean_x + 2*sd_x

# Z-value calculation
z_value <- (x - mean_x)/sd_x

# Compare results
outliers_2sigma <- x[x < lower_bound | x > upper_bound]
outliers_zscore <- x[abs(z_value) > 2]

# definition 2
c<-ggplot(data= db_int, 
          mapping = aes(y=totalHoursWorked, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Total Hours Worked")+
  xlab("")

low <- mean(db_int$totalHoursWorked) - 2* sd(db_int$totalHoursWorked)
up <- mean(db_int$totalHoursWorked) + 2* sd(db_int$totalHoursWorked)

c <- c + geom_hline(yintercept = low,linetype="solid",color="red",size=0.7) +
  geom_hline(yintercept = up,linetype="solid",color="red",size=0.7)

grid.arrange(b, c, ncol = 2)

db_int<- db_int %>% mutate(out_totalHoursWorked=  ifelse( test=(totalHoursWorked< low | totalHoursWorked> up), 
                                                          yes= 1,
                                                          no= 0))
up<- quantile(db_int$y_ingLab_m, 0.975, na.rm=T)


db_int<- db_int %>% mutate(y_ingLab_m=  ifelse( test=( y_ingLab_m>= up), 
                                                yes= up,
                                                no= y_ingLab_m ))
b<-ggplot(data= db_int, 
          mapping = aes(y=y_ingLab_m, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Wage")+
  xlab("")


b <- b + geom_hline(yintercept = low,linetype="solid",color="blue",size=0.7) +
  geom_hline(yintercept = up,linetype="solid",color="blue",size=0.7)
b 

ggplot(data= db_int, 
       mapping = aes(x=residuals)) +
  theme_bw() + 
  geom_density() 

## visualize this regression model

ggplot(db_int , aes(y = residuals , x = id )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

ggplot(db_int , aes(y = residuals , x = id , color= age, shape= as.factor(gender) )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

## use the studres funtion from MASS package

db_int<-db_int %>% mutate(m1_std_residuals= studres(linear_model) )


ggplot(db_int , aes(y = m1_std_residuals , x = id , color= age, shape= as.factor(gender) )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

db_int <- db_int %>% filter(m1_std_residuals<2 & m1_std_residuals>-2 )

# re run the model
linear_model3<- lm(
  totalHoursWorked ~ ofic_ingLab + nmenores  +  nmenores*gender
  + H_Head + age + gender,
  data=db_int)



stargazer(linear_model, linear_model2, linear_model3  , type="text",
          covariate.labels=c("Mean Ocu Income","N under 18","Male",
                             "Hausehold Head","Age", "N under 18 x Male" ))


