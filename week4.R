choose(8,3)

num_red_balls<-3
num_blue_balls<-7
total_draws<-35
prob_red_spheres<-function(z){
  total_balls<-num_red_balls+num_blue_balls
  log_prob<-log(choose(total_draws,z))+z*log(num_red_balls/total_balls)+
    (total_draws-z)*log(num_blue_balls/total_balls)
  return(exp(log_prob))
}
prob_red_spheres(20)

library(tidyverse)

prob_by_num_reds<-data.frame(num_reds=seq(35))%>%
  mutate(prob=prob_red_spheres(num_reds))

prob_by_num_reds%>%head(5)


prob_by_num_reds%>%
  ggplot(mapping=aes(x=num_reds,y=prob))+
  geom_line()+
  theme_bw()+
  xlab("Number of reds")+
  ylab("Probability")


sample(10,35,replace=TRUE)



for(i in 1:5){
  print(sample(100,5,replace=FALSE))
  # The result may well differ every time
}


for(i in 1:5){
  set.seed(1)
  print(sample(100,5,replace=FALSE))
  # The result should not change
}

num_trials<-1000# set the number of trials
set.seed(0)# set the random seed
sampling_with_replacement_simulation<-data.frame(trial=1:num_trials)%>%
  mutate(sample_balls=map(.x=trial,~sample(10,35,replace =TRUE)))
# generate collection of num_trials simulations


sampling_with_replacement_simulation<-sampling_with_replacement_simulation%>%
  mutate(num_reds=map_dbl(.x=sample_balls,~sum(.x<=3)))



sampling_with_replacement_simulation



num_reds_in_simulation<-sampling_with_replacement_simulation%>%pull(num_reds)
# extract a vector corresponding to the number of reds in each of our simulation trials
prob_by_num_reds<-prob_by_num_reds%>%
  mutate(simulation_count=map_dbl(.x=num_reds,~sum(num_reds_in_simulation==.x)))
# add a column which gives the number of trials which gave each number of reds

prob_by_num_reds<-prob_by_num_reds%>%
  mutate(expected_count=num_trials*prob)


prob_by_num_reds


prob_by_num_reds%>%
  rename(Simulation=simulation_count,Expected=expected_count)%>%
  pivot_longer(cols=c("Simulation","Expected"),
               names_to="Type",values_to="count")%>%
  ggplot(aes(num_reds,count))+
  geom_line(aes(linetype=Type,color=Type))+
  scale_linetype_manual(values =c("solid","dashed"))+
  theme_bw()+
  xlab("Number of reds")+
  ylab("Count")





set.seed(0)# set the random seed
num_trials<-10000000# set the number of trials
n_red<-50
n_blue<-30
n_green<-20
sample_size<-10
total<-n_red+n_blue+n_green
sampling_without_replacement_simulation<-data.frame(trial=1:num_trials)%>%
  mutate(sample_balls=map(.x=trial,~sample(total,sample_size,replace =FALSE)))%>%
  # simulate without replacement the sampling of 10 balls from a bag of 100
  mutate(num_reds=map_dbl(.x=sample_balls,~sum(.x<=n_red)))%>%
  mutate(num_blues=map_dbl(.x=sample_balls,~sum((n_red<.x)&(.x<=n_red+n_blue))))%>%
  mutate(num_greens=map_dbl(.x=sample_balls,~sum((n_red+n_blue<.x)&
                                                   (.x<=n_red+n_blue+n_green))))%>%
  # count the number of each colour
  mutate(colour_missing=(pmin(num_reds,num_blues,num_greens)==0))

sampling_without_replacement_simulation%>%head(5)

# look for missing colours
prob_missing_simulation<-mean(sampling_without_replacement_simulation%>%
                                pull(colour_missing))


prob_missing_simulation






NR=choose(total-n_red,sample_size)
NB=choose(total-n_blue,sample_size)
NG=choose(total-n_green,sample_size)
MR=choose(n_red,sample_size)
MB=choose(n_blue,sample_size)
MG=choose(n_green,sample_size)
Q=NR+NB+NG-MR-MB-MG
TS=choose(total,sample_size)
num_one_of_each_subsets=TS-NR-NB-NG+MR+MB+MG
prob_missing_theory=Q/TS
prob_missing_theory
