library(Stat2Data)
data("Hawks")

RedTailedDf<-Hawks%>%
  filter(Species=="RT")%>%
  select(Weight,Tail,Wing)

#maximum likelihood
tail_RT<-RedTailedDf%>%pull(Tail)
n<-length(tail_RT)
mu_mle_RT<-mean(tail_RT,na.rm=TRUE)
var_mle_RT<-var(tail_RT,na.rm=TRUE)*((n-1)/n)
sigma_mle_RT<-sqrt(var_mle_RT)

tail_inc<-seq(mu_mle_RT-3*sigma_mle_RT,
              mu_mle_RT+3*sigma_mle_RT,sigma_mle_RT*0.001)# generate indicies
colors<-c("MLE density"="red","Kernel density"="blue")# set color legend
ggplot()+
  geom_line(data=data.frame(tail=tail_inc,Density=dnorm(tail_inc,
                                                        mean=mu_mle_RT,sd=sigma_mle_RT)),
            aes(x=tail,y=Density,color="MLE density"))+# plot MLE
  geom_density(data=data.frame(tail_RT=tail_RT),
               aes(x=tail_RT,color="Kernel density"))+# plot kernel density
  labs(y="Density function",color="Estimator")+
  theme_bw()+scale_color_manual(values=colors)+ylab("Tail length (mm)")


set.seed(0)
num_trials_per_sample_size<-100
min_sample_size<-5
max_sample_size<-1000
sample_size_inc<-5
mu_0<-1
sigma_0<-3
simulation_df<-crossing(trial=seq(num_trials_per_sample_size),
                        sample_size=seq(min_sample_size,
                                        max_sample_size,sample_size_inc))%>%
  # create data frame of all pairs of sample_size and trial
  mutate(simulation=pmap(.l=list(trial,sample_size),
                         .f=~rnorm(.y,mean=mu_0,sd=sigma_0)))%>%
  
  # simulate sequences of Gaussian random variables
  mutate(sample_md=map_dbl(.x=simulation,.f=median))%>%
  # compute the sample medians
  group_by(sample_size)%>%
  summarise(msq_error_md=mean((sample_md-mu_0)*(sample_md-mu_0)))
simulation_df


set.seed(0)
num_trials_per_sample_size<-100
min_sample_size<-5
max_sample_size<-1000
sample_size_inc<-5
mu_0<-1
sigma_0<-3
simulation_df<-crossing(trial=seq(num_trials_per_sample_size),
                        sample_size=seq(min_sample_size,max_sample_size,
                                        sample_size_inc))%>%
  # create data frame of all pairs of sample_size and trial
  mutate(simulation=pmap(.l=list(trial,sample_size),
                         .f=~rnorm(.y,mean=mu_0,sd=sigma_0)))%>%
  # simulate sequences of Gaussian random variables
  mutate(sample_md=map_dbl(.x=simulation,.f=median))%>%
  # compute the sample medians
  mutate(sample_mn=map_dbl(.x=simulation,.f=mean))%>%
  # compute the sample means
  group_by(sample_size)%>%
  summarise(msq_error_md=mean((sample_md-mu_0)*(sample_md-mu_0)),
            msq_error_mn=mean((sample_mn-mu_0)*(sample_mn-mu_0)))



simulation_df%>%
  pivot_longer(cols=c(msq_error_md,msq_error_mn),
               names_to="Estimator",values_to="msq_error")%>%
  mutate(Estimator=case_when(Estimator=="msq_error_md"~"Median",
                             Estimator=="msq_error_mn"~"Mean"))%>%
  ggplot(aes(x=sample_size,y=msq_error,color=Estimator,linetype=Estimator))+
  geom_smooth()+theme_bw()+xlab("Sample size")+ylab("Mean square error")




set.seed(0)
num_trials_per_sample_size<-1000
min_sample_size<-5
max_sample_size<-100
sample_size_inc<-5
mu_0<-1
sigma_0<-3
bias_simulation_df<-crossing(trial=seq(num_trials_per_sample_size),
                             sample_size=seq(min_sample_size,max_sample_size,
                                             sample_size_inc))%>%
  # create data frame of all pairs of sample_size and trial
  mutate(simulation=pmap(.l=list(trial,sample_size),
                         .f=~rnorm(.y,mean=mu_0,sd=sigma_0)))%>%
  # simulate sequences of Gaussian random variables
  mutate(var_u=map_dbl(.x=simulation,.f=var))%>%
  # compute the sample var with n-1 normalisation
  mutate(var_mle=pmap_dbl(.l=list(.x=simulation,.y=sample_size),
                          .f=~(((.y-1)/.y)*var(.x))))%>%
  # compute the mle var estimate with n normalisation
  group_by(sample_size)%>%
  summarise(bias_var_u=mean(var_u)-sigma_0^2,
            bias_var_mle=mean(var_mle)-sigma_0^2)%>%
  pivot_longer(cols=c(bias_var_u,bias_var_mle),
               names_to="Estimator",values_to="Bias")%>%
  mutate(Estimator=case_when(Estimator=="bias_var_u"~"Unbiased",
                             Estimator=="bias_var_mle"~"MLE"))
bias_simulation_df%>%
  ggplot(aes(x=sample_size,y=Bias,color=Estimator,linetype=Estimator))+
  geom_smooth()+
  theme_bw()+
  xlab("Sample size")+ylab("Bias")



set.seed(0)
num_trials_per_sample_size<-1000
min_sample_size<-5
max_sample_size<-100
sample_size_inc<-5
mu_0<-1
sigma_0<-3
bias_sd_simulation_df<-crossing(trial=seq(num_trials_per_sample_size),
                                sample_size=seq(min_sample_size,max_sample_size,
                                                sample_size_inc))%>%
  # create data frame of all pairs of sample_size and trial
  mutate(simulation=pmap(.l=list(trial,sample_size),
                         .f=~rnorm(.y,mean=mu_0,sd=sigma_0)))%>%
  # simulate sequences of Gaussian random variables
  mutate(sqrt_var_u=map_dbl(.x=simulation,.f=~sqrt(var(.x))))%>%
  # compute the sample sd
  group_by(sample_size)%>%
  summarise(bias_sample_sd=mean(sqrt_var_u)-sigma_0)
bias_sd_simulation_df%>%
  ggplot(aes(x=sample_size,y=bias_sample_sd))+
  geom_smooth()+
  theme_bw()+
  xlab("Sample size")+ylab("Bias sample s.d.")





set.seed(0)
num_trials_per_sample_size<-1000
min_sample_size<-5
max_sample_size<-100
sample_size_inc<-5
lambda_0<-0.5
poisson_simulation_df<-crossing(trial=seq(num_trials_per_sample_size),
                                sample_size=seq(min_sample_size,max_sample_size,
                                                sample_size_inc))%>%
  # create data frame of all pairs of sample_size and trial
  mutate(simulation=pmap(.l=list(trial,sample_size),
                         .f=~rpois(.y,lambda=lambda_0)))%>%
  # simulate sequences of Gaussian random variables
  mutate(lambda_mle=map_dbl(.x=simulation,.f=mean))%>%
  # compute the sample sd
  group_by(sample_size)%>%
  summarise(msq_error=mean((lambda_mle-lambda_0)^2))
poisson_simulation_df%>%
  ggplot(aes(x=sample_size,y=msq_error))+
  geom_smooth()+
  theme_bw()+
  xlab("Sample size")+ylab("Mean square error")



folder_name<-"D:\\study\\R\\firstRProject\\firstRProject"
file_name<-"VonBortkiewicz"
bortkiewicz_horsekick_data<-read.csv(paste0(folder_name,"\\",file_name,".csv"))

horse_kick_fatalities<-bortkiewicz_horsekick_data%>%pull(fatalities)
lambda_MLE=mean(horse_kick_fatalities)
lambda_MLE

dpois(0,lambda=lambda_MLE)




file_name<-"CustomerPurchases"
customer_data<-read.csv(paste0(folder_name,"\\",file_name,".csv"))

customer_data<-customer_data%>%
  mutate(time_diffs=lead(Time)-Time)


time_diffs<-customer_data%>%pull(time_diffs)
lambda_MLE<-1/mean(time_diffs,na.rm =TRUE)
lambda_MLE
1-pexp(60,rate=lambda_MLE)

