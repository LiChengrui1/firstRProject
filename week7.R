t<-qt(1-0.05/2,df=30-1)

library(tidyverse)
library(Stat2Data)
data("Hawks")
select<-dplyr::select
rt_weights<-Hawks%>%
  filter(Species=="RT")%>%
  pull(Weight)%>%
  discard(is.na)# extract a vector of weights


alpha<-0.01
sample_size<-length(rt_weights)
sample_mean<-mean(rt_weights)
sample_sd<-sd(rt_weights)
t<-qt(1-alpha/2,df=sample_size-1)
confidence_interval_l<-sample_mean-t*sample_sd/sqrt(sample_size)
confidence_interval_u<-sample_mean+t*sample_sd/sqrt(sample_size)
confidence_interval<-c(confidence_interval_l,confidence_interval_u)
confidence_interval

ggplot(data=Hawks%>%filter(Species=="RT"),aes(x=Weight))+
  geom_density()+theme_bw()


ggplot(data=Hawks%>%filter(Species=="RT"),aes(sample=Weight))+
  theme_bw()+stat_qq()+stat_qq_line(color="blue")


Hawks%>%filter(Species=="RT")%>%nrow()


library(boot)# load the library
set.seed(123)# set random seed
#first define a function which computes the mean of a column of interest
compute_mean<-function(df,indicies,col_name){
  sub_sample<-df%>%slice(indicies)%>%pull(all_of(col_name))# extract subsample
  return(mean(sub_sample,na.rm=1))}# return median
# use the boot function to generate the bootstrap statistics
results<-boot(data =Hawks%>%filter(Species=="RT"),
              statistic =compute_mean,col_name="Weight",R =10000)
# compute the 99% confidence interval for the mean
boot.ci(boot.out =results,type ="basic",conf=1-alpha)


library(palmerpenguins)
bill_adelie<-penguins%>%
  filter(species=="Adelie")%>%
  pull(bill_length_mm)%>%
  na.omit()

tibble(bill_adelie)%>%
  ggplot(aes(x=bill_adelie))+
  geom_density()

tibble(bill_adelie)%>%
  ggplot(aes(sample=bill_adelie))+theme_bw()+stat_qq()+stat_qq_line(color="blue")


t.test(x=bill_adelie,mu=40,conf.level=0.99)

one_sample_t_test<-function(x,mu){
  n<-length(x)
  t_stat<-(mean(x)-mu)/(sd(x)/sqrt(n))
  p_value<-2*(1-pt(abs(t_stat),df=n-1))
  return(p_value)
}

one_sample_t_test(x=bill_adelie,mu=40)



library(PairedData)
data("Barley")


t.test(x=Barley$Glabron,y=Barley$Velvet,paired=TRUE,conf.level=0.99)


#the effect size using Cohen’s d statistic
diffs<-Barley%>%
  mutate(diff=Glabron-Velvet)%>%
  pull(diff)
cohens_d<-mean(diffs)/sd(diffs)
cohens_d


Barley%>%
  mutate(diff=Glabron-Velvet)%>%
  ggplot(aes(x=diff))+geom_density()+theme_bw()+
  labs(x="Differences of yields (bushels)",y="Density")

Barley%>%
  mutate(diff=Glabron-Velvet)%>%
  ggplot(aes(sample=diff))+theme_bw()+
  stat_qq()+stat_qq_line(color="blue")+
  labs(x="Theoretical",y="Sample")


student_t_confidence_interval<-function(sample,confidence_level){
  sample<-sample[!is.na(sample)]# remove any missing values
  n<-length(sample)# compute sample size
  mu_est<-mean(sample)# compute sample mean
  sig_est<-sd(sample)# compute sample sd
  alpha=1-confidence_level# alpha from gamma
  t<-qt(1-alpha/2,df=n-1)# get student t quantile
  l=mu_est-(t/sqrt(n))*sig_est# lower
  u=mu_est+(t/sqrt(n))*sig_est# upper
  return(c(l,u))
}



num_trials<-100000
sample_size<-30
mu_0<-1
sigma_0<-3
alpha<-0.05
set.seed(0)# set random seed for reproducibility
single_alpha_coverage_simulation_df<-data.frame(trial=seq(num_trials))%>%
  mutate(sample=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)))%>%
  # generate random Gaussian samples
  mutate(ci_interval=map(.x=sample,.f=~student_t_confidence_interval(.x,1-alpha)))%>%
  # generate confidence intervals
  mutate(cover=map_lgl(.x=ci_interval,
                       .f=~((min(.x)<=mu_0)&(max(.x)>=mu_0))))%>%
  # check if interval covers mu_0
  mutate(ci_length=map_dbl(.x=ci_interval,
                           .f=~(max(.x)-min(.x))))
# compute interval length
single_alpha_coverage_simulation_df%>%
  pull(cover)%>%
  mean()# estimate of coverage probability



num_trials_per_alpha<-100
sample_size<-30
mu_0<-1
sigma_0<-3
alpha_min<-0.0025

alpha_max<-0.25
alpha_inc<-0.0025
set.seed(0)# set random seed for reproducibility
coverage_simulation_df<-crossing(trial=seq(num_trials_per_alpha),
                                 alpha=seq(alpha_min,alpha_max,alpha_inc)
)%>%
  mutate(sample=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)))%>%
  mutate(gamma=1-alpha)%>%
  # generate random Gaussian samples
  mutate(ci_interval=map2(.x=sample,.y=gamma,.f=~student_t_confidence_interval(.x,.y)))%>%
  # generate confidence intervals
  mutate(cover=map_lgl(.x=ci_interval,
                       .f=~((min(.x)<=mu_0)&(max(.x)>=mu_0))))%>%
  # check if interval covers mu_0
  mutate(ci_length=map_dbl(.x=ci_interval,
                           .f=~(max(.x)-min(.x))))%>%
  # compute interval length
  group_by(gamma)%>%
  summarise(coverage=mean(cover),mean_length=mean(ci_length))
coverage_simulation_df%>%
  ggplot(aes(x=gamma,y=coverage))+
  geom_smooth()+xlab("Confidence level (%)")+ylab("Coverage")+
  theme_bw()

coverage_simulation_df%>%
  ggplot(aes(x=gamma,y=mean_length))+
  geom_smooth()+xlab("Confidence level (%)")+ylab("Mean interval length")+
  theme_bw()

