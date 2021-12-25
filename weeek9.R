library(tidyverse)
library(Stat2Data)
data("Hawks")
hawks_total<-Hawks%>%
  select(Weight,Wing,Hallux,Tail,Species)%>%
  filter(Species!="RT")%>%
  drop_na()%>%
  mutate(Species=as.numeric(Species=="SS"))

hawks_total
num_total<-hawks_total%>%nrow()# number of penguin data
num_train<-floor(num_total*0.6)# number of train examples 向下取整
num_test<-num_total-num_train# number of test samples
set.seed(123)# set random seed for reproducibility

test_inds<-sample(seq(num_total),num_test)# random sample of test indicies
train_inds<-setdiff(seq(num_total),test_inds)# training data indicies
hawks_train<-hawks_total%>%filter(row_number()%in%train_inds)# train data
hawks_test<-hawks_total%>%filter(row_number()%in%test_inds)# test data
hawks_train%>%nrow()
hawks_test%>%nrow()
hawks_train_x<-hawks_train%>%select(-Species)# train feature vectors
hawks_train_y<-hawks_train%>%pull(Species)# train labels
hawks_test_x<-hawks_test%>%select(-Species)# test feature vectors
hawks_test_y<-hawks_test%>%pull(Species)# test labels


train_error_phi_0<-mean(abs(hawks_train_y-0))
train_error_phi_0
train_error_phi_1<-mean(abs(hawks_train_y-1))
train_error_phi_1
if(train_error_phi_0<train_error_phi_1){
  y_hat<-0
}else{
  y_hat<-1
}
y_hat<-as.numeric(mean(hawks_train_y)>=0.5)
y_hat

train_error_simple<-mean(abs(y_hat-hawks_train_y))# train error
test_error_simple<-mean(abs(y_hat-hawks_test_y))# test error
train_error_simple
test_error_simple




lda_model<-MASS::lda(Species~.,data=hawks_train)# fit LDA model
lda_train_predicted_y<-predict(lda_model,hawks_train_x)$class%>%
  as.character()%>%as.numeric()# get vector of predicted ys
lda_train_error<-mean(abs(lda_train_predicted_y-hawks_train_y))# compute train error
lda_train_error

lda_test_predicted_y<-predict(lda_model,hawks_test_x)$class%>%
  as.character()%>%as.numeric()# get vector of predicted ys
lda_test_error<-mean(abs(lda_test_predicted_y-hawks_test_y))# compute test error
lda_test_error




data.frame(z=seq(-10,10,0.001))%>%
  mutate(sigmoid=1/(1+exp(-z)))%>%
  ggplot(aes(x=z,y=sigmoid))+
  geom_smooth()+
  theme_bw()+
  labs(x="z",y="S(z)")

library(glmnet)# load the glmnet library
logistic_model<-glmnet(x=hawks_train_x%>%as.matrix(),y=hawks_train_y,
                       family ="binomial",alpha=0,lambda=0)# train a logistic model
logistic_train_predicted_y<-predict(logistic_model,hawks_train_x%>%
                                      as.matrix(),type="class")%>%as.integer()
logistic_train_error<-mean(abs(logistic_train_predicted_y-hawks_train_y))# train error
logistic_train_error


logistic_test_predicted_y<-predict(logistic_model,hawks_test_x%>%
                                     as.matrix(),type="class")%>%as.integer()
logistic_test_error<-mean(abs(logistic_test_predicted_y-hawks_test_y))# test error
logistic_test_error

