#libraries 
library(dplyr)
library(caret)
library(MLmetrics)
library(xgboost)
library(InformationValue)
library(knitr)
library(fastDummies)
library(ggplot2)
library(ClusterR)
library(flexclust)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(gridExtra)

#------------------------Data Exploration-----------------------
data= employee

#I
ggplot(data, aes(Attrition, fill = Attrition)) + geom_bar() +
  ggtitle("Attrition By Numbers")


#II
p1 = ggplot(data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) + geom_boxplot() 
p2 = ggplot(data, aes(x = Attrition, y = HourlyRate, fill = Attrition)) + geom_boxplot() 
p3 = ggplot(data, aes(x = Attrition, y = DailyRate, fill = Attrition)) + geom_boxplot() 
p4 = ggplot(data, aes(x = Attrition, y = MonthlyRate, fill = Attrition)) + geom_boxplot() 

grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)


#III
data_df = data %>%
  mutate(attrition = factor(Attrition)) %>%
  group_by(attrition, OverTime) %>%
  count() %>%
  group_by(attrition) %>%
  mutate(percentage = n/sum(n))
data_df$percentage = round(data_df$percentage, digits = 2)

ggplot(data_df, aes(x = OverTime, y = percentage, fill = OverTime)) +
  geom_bar(position = 'dodge', stat = 'identity') + facet_grid(~attrition) + 
  geom_text(aes(label=percentage)) + ggtitle("Attrition")


#IV 
ggplot(data, aes(x = YearsAtCompany, y = YearsSinceLastPromotion, col = OverTime)) + 
  geom_point(aes( colour = OverTime)) +
  geom_smooth(method = "lm") + facet_grid(~Attrition) + ggtitle("Attrition")


#V
ggplot(data, aes(x = as.character(WorkLifeBalance), y = DistanceFromHome, 
                 fill = as.character(WorkLifeBalance))) + 
  geom_boxplot() + facet_grid(~Attrition) + theme(legend.position = "none") + 
  ggtitle("Attrition vs Distance From Home vs Worklife Balance")


#VI
data_df2 = data %>%
  mutate(attrition = factor(Attrition)) %>%
  group_by(attrition, BusinessTravel) %>%
  count() %>%
  group_by(attrition) %>%
  mutate(percentage = n/sum(n))
data_df2$percentage = round(data_df2$percentage, digits = 2)

ggplot(data_df2, aes(x = BusinessTravel, y = percentage, fill = BusinessTravel)) +
  geom_bar(position = 'dodge', stat = 'identity') + facet_grid(~attrition) + 
  geom_text(aes(label=percentage)) + ggtitle("Attrition % vs Business Travel") +
  theme(legend.position = "none") 


#VII
data_df3 = data %>%
  mutate(attrition = factor(Attrition)) %>%
  group_by(attrition, Department) %>%
  count() %>%
  group_by(Department) %>%
  mutate(percentage = n/sum(n))
data_df3$percentage = round(data_df3$percentage, digits = 2)

ggplot(data_df3, aes(x = attrition, y = percentage, fill = attrition)) +
  geom_bar(position = 'dodge', stat = 'identity') + facet_grid(~Department) + 
  geom_text(aes(label=percentage)) + ggtitle("Department vs Attrition %") +
  theme(legend.position = "none") 


#VIII
ggplot(data, aes(MaritalStatus, fill = Attrition)) +
  geom_bar(position = 'dodge') + ggtitle("Attrition Count vs Marital Status") 


#IX
ggplot(data, aes(x = ï..Age, y = MonthlyIncome, fill = Attrition)) +
  geom_point(aes(size = ï..Age, colour = Attrition, alpha = 0.5)) + facet_grid(~MaritalStatus) +
  ggtitle("Attrition Level Comparison") 


#X
ggplot(data, aes(x = Education, fill = Attrition)) +
  geom_bar(aes(fill = Attrition, position = "stack")) + facet_grid(~EducationField) +
  ggtitle("Education Levels and Field of Education vs Attrition") 


#XI
ggplot(data, aes(y = TotalWorkingYears, x = Attrition, fill = Attrition)) +
  geom_boxplot() + ggtitle("Education Levels and Field of Education vs Attrition") +
  coord_flip()


#XII
p1.2 = ggplot(filter(data, Attrition == "Yes"), aes(TotalWorkingYears, fill = Attrition)) +
  geom_histogram() + facet_grid(~Attrition) + theme(legend.position = "none") + 
  ggtitle("Histogram of Years of Experience / Attrition = Yes") 

p2.2 = ggplot(filter(data, Attrition == "No"), aes(TotalWorkingYears, fill = Attrition)) +
  geom_histogram() + facet_grid(~Attrition) + theme(legend.position = "none") +
  ggtitle("Histogram of Years of Experience / Attrition = No") 

grid.arrange(p1.2, p2.2, nrow = 2)


#XIII
data_df4 = data %>%
  mutate(attrition = factor(Attrition)) %>%
  group_by(attrition, JobInvolvement) %>%
  count() %>%
  group_by(attrition) %>%
  mutate(percentage = n/sum(n))
data_df4$percentage = round(data_df4$percentage, digits = 2)

ggplot(data_df4, aes(x = as.character(JobInvolvement), y = percentage, fill = JobInvolvement)) +
  geom_bar(position = 'dodge', stat = 'identity') + facet_grid(~attrition) + 
  geom_text(aes(label=percentage)) + ggtitle("Attrition % vs Job Involvement") +
  theme(legend.position = "none")

#XIV

data_df5 = data %>%
  mutate(attrition = factor(Attrition)) %>%
  group_by(attrition, JobSatisfaction) %>%
  count() %>%
  group_by(attrition) %>%
  mutate(percentage = n/sum(n))
data_df5$percentage = round(data_df5$percentage, digits = 2)

ggplot(data_df5, aes(x = as.character(JobSatisfaction), y = percentage, fill = JobSatisfaction)) +
  geom_bar(position = 'dodge', stat = 'identity') + facet_grid(~attrition) + 
  geom_text(aes(label=percentage)) + ggtitle("Attrition % vs JobSatisfaction") +
  theme(legend.position = "none")


#XV
data_df6 = data %>%
  mutate(attrition = factor(Attrition)) %>%
  group_by(attrition, PerformanceRating) %>%
  count() %>%
  group_by(attrition) %>%
  mutate(percentage = n/sum(n))
data_df6$percentage = round(data_df6$percentage, digits = 2)

ggplot(data_df6, aes(x = as.character(PerformanceRating), y = percentage, fill = PerformanceRating)) +
  geom_bar(position = 'dodge', stat = 'identity') + facet_grid(~attrition) + 
  geom_text(aes(label=percentage)) + ggtitle("Attrition % vs PerformanceRating") +
  theme(legend.position = "none") 


#XVI 
ggplot(data, aes(x = YearsAtCompany, y = MonthlyIncome, fill = Attrition)) + 
  geom_point(aes(size = MonthlyIncome, colour = Attrition)) +
  geom_smooth(method = "lm") + ggtitle("Years at Company vs Monthly Income vs Attrition")

#-----------------------Data Preprocessing-----------------------
data= employee

target_variable = "Attrition"

negligible_variables = c("EmployeeCount", "Over18", "StandardHours")


#define the "target" variables name & exclude the variables that are negligible 
target = data[target_variable]
colnames(target) = "target"
data = data.frame(data, target)
data = data[,-which(names(data) %in% c(target_variable, negligible_variables))]

#make sure that the target variable has the class values of 0 and 1
levels(data$target) = c(1,0)
data$target = as.numeric(as.character(data$target))

#----------------------------------------------------Mean encoding 

#Define the number of levels for the factor variables
var_levels = list()
n_levels = list()

for (n in names(data))
  if (is.factor(data[[n]])) {
    var_levels[[n]] = n
    n_levels[[n]] = nlevels(data[[n]])
  }
n_var = cbind(var_levels, n_levels) %>% as.data.frame()

n_var_column = subset(n_var$var_levels, n_var$n_levels >= 10)


#Apply for each factor variable mean encoding
simpleFunction <- function(dataset, col_name){
  
  require("dplyr")
  col_name <- as.name(col_name)
  dataset %>%
    group_by(!! col_name) %>%
    summarise(mean_encoded = mean(target)) 
  
}

n_var_column2 = subset(n_var$var_levels, n_var$n_levels < 10 
                       & n_var$n_levels >= 5) 

lookup = list()
data_aa = list()
data_aa_final = list()


for(i in names(n_var_column2)) {
  
  lookup[[i]] = simpleFunction(data, n_var_column2[[i]])
  
  data_aa[[i]] = left_join(data[n_var_column2[[i]]], lookup[[i]])
  data_aa[[names(lookup[[i]][1])]][[names(lookup[[i]][1])]] = data_aa[[names(lookup[[i]][1])]]$mean_encoded
  data_aa_final[[i]] = cbind(data_aa[[i]][1])
}

mutate_mean_encoded =  data_aa_final %>% as.data.frame()    
data2 = data[,-which(names(data) %in% n_var_column2)]
data2 = dummy_cols(data2)
data_comp = cbind(data2, mutate_mean_encoded)


#---------------------------Logistic regression

#Data Split

splitSample <- sample(1:2, size=nrow(data_comp), prob=c(0.7,0.3), replace = TRUE)
training <- data_comp[splitSample==1,]
testing <- data_comp[splitSample==2,]

training$target = as.factor(training$target)
testing$target = as.factor(testing$target)

#Setting X variables and y variables

f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}

#fitting
fitControl <- trainControl(method = "repeatedcv", summaryFunction =  f1)


#Logistic Regression
fit_log <- train(target ~ ., data = training, method = "glm", trControl = fitControl, family = "binomial", metric = "F1_Score")

#Prediction and excluding target variable
pred_log = predict(fit_log, testing[,-which(names(testing) == "target")])

#Confusion matrix

#switching "," by "." to get the correct number F1 score
pred_log_num = as.numeric(sub(",", ".", pred_log, fixed = TRUE))
original_log_num = as.numeric(sub(",", ".", testing$target, fixed = TRUE))

ConfusionMatrix(y_pred = pred_log_num, y_true = original_log_num)

df = mutate(as.data.frame(pred_log), original = testing$target)

#F1 - Precision / Recall Score
recall_log = sum(pred_log_num & original_log_num) / sum(original_log_num)

precision_log = precision(original_log_num, pred_log_num)

fscore_log = (2 * precision_log * recall_log) / (precision_log + recall_log)

#------------------------------XG Boost

fit_xg <- train(target ~., data = training, method = "xgbTree", metric = "f1", trControl = fitControl)

pred_xg = predict(fit_xg, testing[,-which(names(testing) == "target")])

df_xg = mutate(as.data.frame(pred_xg), original = testing$target)

pred_xg_num = as.numeric(sub(",", ".", pred_xg, fixed = TRUE))
original_xg_num = as.numeric(sub(",", ".", testing$target, fixed = TRUE))

ConfusionMatrix(y_pred = pred_xg_num, y_true = original_xg_num)

#Performance measure

#F1 - Precision / Recall Score
recall_xg = sum(pred_xg_num & original_xg_num) / sum(original_xg_num)

precision_xg = precision(original_xg_num, pred_xg_num)

fscore_xg = (2 * precision_xg * recall_xg) / (precision_xg + recall_xg)


#Classification Performance Measures
Fscore = c(0.939,	0.923)

performances = rbind(Fscore)
colnames(performances) = c("Logistic Regression", "Extreme Gradient Boosting")

kable(performances)

#-------------------------Clustering and PCA

data= employee

fit_log$finalModel %>% summary


#Sorting the variables according to the strongest impact on retention up to the lowest impact
fit_output = fit_log$finalModel %>% summary
fit_output_coef = cbind(fit_output$coefficients ) %>%   
  as.data.frame() 
fit_output_coef2 = rownames(fit_output$coefficients) %>% as.data.frame()

fit_output_coef_final = cbind(fit_output_coef2, fit_output_coef) %>% 
  arrange(desc(Estimate)) %>% 
  mutate(effect = exp(Estimate)) %>% 
  mutate(effect_perc. = (effect - 1)*100)


#regression output adjusted
fit_output_coef_final2 = fit_output_coef_final[c(-1,-2),] %>% filter(`Pr(>|z|)`<0.05)
fit_output_coef_final2 = fit_output_coef_final2[-17,]
kable(fit_output_coef_final2) #relevant / significant variables


#Non significant variables
fit_output_coef_final3 = fit_output_coef_final[c(-1,-2),] %>% filter(`Pr(>|z|)`>0.05)
kable(fit_output_coef_final3) #non significant variables


#Preparing data for clustering
data_comp2 = data_comp[,which(names(data_comp) %in% c(as.character(fit_output_coef_final2$.), "MaritalStatus_Single", "BusinessTravel_Travel_Rarely", "OverTime_Yes", "BusinessTravel_Travel_Frequently"))]


#----------------------------------------K means clustering
set.seed(3)

#standardize data
cl_data <- scale(data_comp2) 

#Exclude any column without variation in its columns 
cl_data_clean = cl_data[ , colSums(is.na(cl_data)) == 0]

#cluster data 
cluster_train = kcca(cl_data_clean, k=3, kccaFamily("kmeans")) 

cluster_data = mutate(data_comp2, cluster = flexclust::clusters(cluster_train, cl_data_clean))


#pre-requirements for biplots and correlation plots
cluster_data$cluster = as.factor(cluster_data$cluster)

pca_cl = PCA(cluster_data[,-which(names(cluster_data) %in% c("cluster"))], scale.unit = TRUE)


#PCA Coefficient Table
kable(pca_cl$var$cor[,1:2])


#Correlation plot 
fviz_pca_var(pca_cl, col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )


#PCA Biplot - combination with clusters 
plot_cl = fviz_pca_biplot(pca_cl,geom.ind = "point",fill.ind = cluster_data$cluster, col.ind = "black",pointshape = 21, pointsize = 1,palette = "jco",addEllipses = TRUE,alpha.var ="contrib", col.var = "contrib", alpha.ind = 0.5,gradient.cols = "RdYlBu",legend.title = list(fill = "Cluster", color = "Contrib",alpha = "Contrib"))
plot_cl


data_vis = cbind(data, cluster = cluster_data$cluster)


#PLotting the densities to evaluate the frequencies
ggplot(data_vis, aes(MonthlyIncome, colour = cluster)) + geom_density() +ggtitle("Monthly Income Density Plot per Cluster")
ggplot(data_vis, aes(Attrition, colour = cluster)) + geom_density()+ggtitle("Attrition Density Plot per Cluster")
ggplot(data_vis, aes(ï..Age, colour = cluster)) + geom_density()+ggtitle("Age Density Plot per Cluster")


#positive effect on retention
p1 = ggplot(data_vis, aes(JobInvolvement, colour = cluster)) + geom_density()
p2 = ggplot(data_vis, aes(EnvironmentSatisfaction, colour = cluster)) + geom_density()
p3 = ggplot(data_vis, aes(WorkLifeBalance , colour = cluster)) + geom_density()
p4 = ggplot(data_vis, aes(JobSatisfaction , colour = cluster)) + geom_density()

grid.arrange(p1,p2,p3,p4,nrow = 2,top = textGrob("Positive effect on retention",gp=gpar(fontsize=15,font=3))) 


#negative effect on retention
p1.2 = ggplot(data_vis, aes(BusinessTravel, colour = cluster)) + geom_density()
p2.2 = ggplot(data_vis, aes(OverTime, colour = cluster)) + geom_density()
p3.2 = ggplot(data_vis, aes(MaritalStatus , colour = cluster)) + geom_density()
p4.2 = ggplot(data_vis, aes(YearsSinceLastPromotion , colour = cluster)) + geom_density()

grid.arrange(p1.2,p2.2,p3.2,p4.2,nrow = 2,top = textGrob("Negative effect on retention",gp=gpar(fontsize=15,font=3))) 


#variables with a positive effect on retention & Monthly Income
p1.3 = ggplot(data_vis, aes(x = JobInvolvement, y = MonthlyIncome, colour = cluster)) + geom_point()
p2.3 = ggplot(data_vis, aes(x = EnvironmentSatisfaction, y = MonthlyIncome, colour = cluster)) + geom_point()
p3.3 = ggplot(data_vis, aes(x = WorkLifeBalance, y = MonthlyIncome, colour = cluster)) + geom_point()
p4.3 = ggplot(data_vis, aes(x = JobSatisfaction, y = MonthlyIncome, colour = cluster)) + geom_point()

grid.arrange(p1.3,p2.3,p3.3,p4.3,nrow = 2,top = textGrob("Variables with a positive effect on Monthly Income",gp=gpar(fontsize=15,font=3))) 

#variables with a negative effect on retention & Monthly Income
p1.4 = ggplot(data_vis, aes(x = BusinessTravel, y = MonthlyIncome, colour = cluster)) + geom_point()
p2.4 = ggplot(data_vis, aes(x = OverTime, y = MonthlyIncome, colour = cluster)) + geom_point()
p3.4 = ggplot(data_vis, aes(x = MaritalStatus, y = MonthlyIncome, colour = cluster)) + geom_point()
p4.4 = ggplot(data_vis, aes(x = YearsSinceLastPromotion, y = MonthlyIncome, colour = cluster)) + geom_point()

grid.arrange(p1.4,p2.4,p3.4,p4.4,nrow = 2,top = textGrob("Variables with a Negative effect on Monthly Income",gp=gpar(fontsize=15,font=3))) 


#variables with a positive effect on retention & Monthly Income
p1.5 = ggplot(data_vis, aes(x = JobInvolvement, y = Attrition, colour = cluster)) + geom_point()
p2.5 = ggplot(data_vis, aes(x = EnvironmentSatisfaction, y = Attrition, colour = cluster)) + geom_point()
p3.5 = ggplot(data_vis, aes(x = WorkLifeBalance, y = Attrition, colour = cluster)) + geom_point()
p4.5 = ggplot(data_vis, aes(x = JobSatisfaction, y = Attrition, colour = cluster)) + geom_point()

grid.arrange(p1.5,p2.5,p3.5,p4.5,nrow = 2) 

#variables with a negative effect on retention & Monthly Income
p1.6 = ggplot(data_vis, aes(x = BusinessTravel, y = Attrition, colour = cluster)) + geom_point()
p2.6 = ggplot(data_vis, aes(x = OverTime, y = Attrition, colour = cluster)) + geom_point()
p3.6 = ggplot(data_vis, aes(x = MaritalStatus, y = Attrition, colour = cluster)) + geom_point()
p4.6 = ggplot(data_vis, aes(x = YearsSinceLastPromotion, y = Attrition, colour = cluster)) + geom_point()

grid.arrange(p1.6,p2.6,p3.6,p4.6,nrow = 2) 



