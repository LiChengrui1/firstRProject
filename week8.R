library(palmerpenguins)
peng_AC<-penguins%>%
  drop_na(species,body_mass_g)%>%
  filter(species!="Gentoo")


t_test_function<-function(data,val_col,group_col,var_equal=TRUE){
  stats<-data%>%
    rename(group=!!(group_col),val=!!(val_col))%>%
    group_by(group)%>%
    drop_na(val)%>%
    summarise(mn=mean(val),vr=var(val),n=n())
  pooled_sd<-sqrt(((stats$n[1]-1)*stats$vr[1]+(stats$n[2]-1)*stats$vr[2])/(stats$n[1]+stats$n[2]-2))
  if(var_equal){
    t_stat<-(stats$mn[1]-stats$mn[2])/(pooled_sd*sqrt(1/stats$n[1]+1/stats$n[2]))
    dof<-stats$n[1]+stats$n[2]-2
  }else{
    t_stat=(stats$mn[1]-stats$mn[2])/sqrt(stats$vr[1]/stats$n[1]+stats$vr[2]/stats$n[2])
    dof=(stats$vr[1]/stats$n[1]+stats$vr[2]/stats$n[2])^2/(
      (stats$vr[1]/stats$n[1])^2/(stats$n[1]-1)+
        (stats$vr[2]/stats$n[2])^2/(stats$n[2]-1)
    )
  }
  p_val<-2*(1-pt(abs(t_stat),df=dof))
  return(data.frame(t_stat=t_stat,dof=dof,p_val=p_val))
}

t_test_function(data=peng_AC,val_col="body_mass_g",group_col="species")

t.test(body_mass_g~species,data=peng_AC,var.equal =FALSE)


num_trials<-10000
sample_size<-30
mu_0<-1
mu_1<-1
sigma_0<-3
sigma_1<-3
alpha<-0.05
set.seed(0)# set random seed for reproducibility
single_alpha_test_size_simulation_df<-data.frame(trial=seq(num_trials))%>%
  mutate(sample_0=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)),
         sample_1=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_1,sd=sigma_1)))%>%
  # generate random Gaussian samples
  mutate(p_value=pmap(.l=list(trial,sample_0,sample_1),
                      .f=~t.test(..2,..3,var.equal =TRUE)$p.value))%>%
  # generate p values
  mutate(type_1_error=p_value<alpha)
single_alpha_test_size_simulation_df%>%
  pull(type_1_error)%>%
  mean()# estimate probability of type I error i.e. the size of the test



num_trials_per_alpha<-100
sample_size<-30
mu_0<-1
mu_1<-1

sigma_0<-3
sigma_1<-3
alpha_min<-0.0025
alpha_max<-0.25
alpha_inc<-0.0025
set.seed(0)# set random seed for reproducibility
many_alpha_test_size_simulation_df<-crossing(trial=seq(num_trials_per_alpha),
                                             alpha=seq(alpha_min,alpha_max,alpha_inc)
)%>%
  mutate(sample_0=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)),
         sample_1=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_1,sd=sigma_1)))%>%
  # generate random Gaussian samples
  mutate(p_value=pmap(.l=list(trial,sample_0,sample_1),
                      .f=~t.test(..2,..3,var.equal =TRUE)$p.value))%>%
  # generate p values
  mutate(type_1_error=p_value<alpha)
many_alpha_test_size_simulation_df%>%
  group_by(alpha)%>%
  summarise(test_size=mean(type_1_error))%>%
  ggplot(aes(x=alpha,y=test_size))+
  geom_smooth()+xlab("Significance level (%)")+ylab("Test size")+
  theme_bw()

num_trials<-10000
n_0<-30
n_1<-30
mu_0<-3
mu_1<-4
sigma_0<-2
sigma_1<-2
alpha<-0.05
set.seed(0)# set random seed for reproducibility
data.frame(trial=seq(num_trials))%>%
  mutate(sample_0=map(.x=trial,.f=~rnorm(n=n_0,mean=mu_0,sd=sigma_0)),
         sample_1=map(.x=trial,.f=~rnorm(n=n_1,mean=mu_1,sd=sigma_1)))%>%
  # generate random Gaussian samples
  mutate(p_value=pmap(.l=list(trial,sample_0,sample_1),
                      .f=~t.test(..2,..3,var.equal =TRUE)$p.value))%>%
  # generate p values
  mutate(reject_null=p_value<alpha)%>%
  pull(reject_null)%>%
  mean()# estimate of coverage probability



num_trials_per_scenario<-100
n_0<-30
n_1<-30
mu_0<-3
mu_1<-4
sigma_0<-2
sigma_1<-2
alpha_min<-0.0025
alpha_max<-0.25
alpha_inc<-0.0025
set.seed(0)# set random seed for reproducibility
crossing(trial=seq(num_trials_per_scenario),
         alpha=seq(alpha_min,alpha_max,alpha_inc))%>%
  mutate(sample_0=map(.x=trial,.f=~rnorm(n=n_0,mean=mu_0,sd=sigma_0)),
         sample_1=map(.x=trial,.f=~rnorm(n=n_1,mean=mu_1,sd=sigma_1)))%>%
  # generate random Gaussian samples
  mutate(p_value=pmap(.l=list(trial,sample_0,sample_1),
                      .f=~t.test(..2,..3,var.equal =TRUE)$p.value))%>%
  # generate p values
  mutate(reject_null=p_value<alpha)%>%
  group_by(alpha)%>%
  summarise(statistical_power=mean(reject_null))%>%
  ggplot(aes(x=alpha,y=statistical_power))+
  geom_smooth()+xlab("Significance level (%)")+ylab("Power")+
  theme_bw()




num_trials_per_scenario<-100
n_0<-30
n_1<-30
mu_0<-3
sigma_0<-2
sigma_1<-2
alpha<-0.05
delta_min<-0
delta_max<-5
delta_inc<-0.1
set.seed(0)# set random seed for reproducibility
crossing(trial=seq(num_trials_per_scenario),
         delta=seq(delta_min,delta_max,delta_inc))%>%
  mutate(sample_0=map(.x=trial,.f=~rnorm(n=n_0,mean=mu_0,sd=sigma_0)),
         sample_1=map2(.x=trial,.y=delta,.f=~rnorm(n=n_1,mean=mu_0+.y,sd=sigma_1)))%>%
  # generate random Gaussian samples
  mutate(p_value=pmap(.l=list(trial,sample_0,sample_1),
                      .f=~t.test(..2,..3,var.equal =TRUE)$p.value))%>%
  # generate p values
  mutate(reject_null=p_value<alpha)%>%
  group_by(delta)%>%
  summarise(statistical_power=mean(reject_null))%>%
  ggplot(aes(x=delta,y=statistical_power))+
  geom_smooth()+xlab("Difference in means")+ylab("Power")+
  theme_bw()



num_trials_per_scenario<-100
mu_0<-3
mu_1<-4
alpha<-0.05
sigma_0<-2
sigma_1<-2
n_min<-5
n_max<-300
n_inc<-5
set.seed(0)# set random seed for reproducibility
crossing(trial=seq(num_trials_per_scenario),
         sample_size=seq(n_min,n_max,n_inc))%>%
  mutate(sample_0=map2(.x=trial,.y=sample_size,.f=~rnorm(n=.y,mean=mu_0,sd=sigma_0)),
         sample_1=map2(.x=trial,.y=sample_size,.f=~rnorm(n=.y,mean=mu_1,sd=sigma_1)))%>%
  # generate random Gaussian samples
  mutate(p_value=pmap_dbl(.l=list(trial,sample_0,sample_1),
                          .f=~t.test(..2,..3,var.equal =TRUE)$p.value))%>%
  # generate p values
  mutate(reject_null=p_value<alpha)%>%
  group_by(sample_size)%>%
  summarise(statistical_power=mean(reject_null))%>%
  ggplot(aes(x=sample_size,y=statistical_power))+
  geom_smooth()+xlab("Sample size")+ylab("Power")+
  theme_bw()



