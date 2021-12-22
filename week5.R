set.seed(0)
n<-1000
sample_X<-data.frame(U=runif(n))%>% #生成1000个随机数
  mutate(X=case_when(
    (0<=U)&(U<0.25)~1,
    (0.25<=U)&(U<0.5)~5,
    (0.5<=U)&(U<=1)~0))%>%
  pull(X)

sample_X

sample_X_015<-function(n,alpha,beta){
  sample_X<-data.frame(U=runif(n))%>%
    mutate(X=case_when(
      (0<=U)&(U<alpha)~1,
      (alpha<=U)&(U<alpha+beta)~5,
      (alpha+beta<=U)&(U<=1)~0))%>%
    pull(X)
  return(sample_X)
}

n<-10000
alpha<-1/2
beta<-1/10
sample_X<-sample_X_015(n,alpha,beta)
mean(sample_X)

var(sample_X) #方差


set.seed(0)
n<-100
alpha<-1/10
simulation_by_beta<-data.frame(beta=seq(0,9/10,0.01))%>%
  mutate(sample_X=map(.x=beta,~sample_X_015(n,alpha,.x)))%>%
  mutate(sample_avg=map_dbl(.x=sample_X,~mean(.x)))%>%
  select(-sample_X)%>%
  mutate(expectation=alpha+5*beta)

simulation_by_beta%>%head(5)


df_pivot<-simulation_by_beta%>%
  rename(Sample=sample_avg,Expectation=expectation)%>%
  pivot_longer(cols=!beta,names_to ="var",values_to ="val")


df_pivot%>%head(5)

df_pivot%>%
  ggplot(aes(x=beta,y=val,linetype=var))+
  geom_line(data=df_pivot%>%
              filter(var=="Expectation"))+
  geom_point(data=df_pivot%>%
               filter(var=="Sample"))+
  labs(x="beta",y="Mean",linetype="")+
  theme_bw()

#probability density function

x<-seq(-4,6,0.1)
normal_densities_by_x<-data.frame(x=x,density=dnorm(x,mean=1,sd=sqrt(1)),var=1)%>%
  rbind(data.frame(x=x,density=dnorm(x,mean=1,sd=sqrt(2)),var=2))%>%
  rbind(data.frame(x=x,density=dnorm(x,mean=1,sd=sqrt(3)),var=3))
ggplot(normal_densities_by_x,aes(x,y=density,color=as.character(var),
                                 linetype=as.character(var)))+geom_line()+
  theme_bw()+labs(color="Variance",linetype="Variance",x="x",y="Density")


#cumulative distribution function(累积密度函数)

normal_cdf_by_x<-data.frame(x=x,cdf=pnorm(x,mean=1,sd=sqrt(1)),var=1)%>%
  rbind(data.frame(x=x,cdf=pnorm(x,mean=1,sd=sqrt(2)),var=2))%>%
  rbind(data.frame(x=x,cdf=pnorm(x,mean=1,sd=sqrt(3)),var=3))
ggplot(normal_cdf_by_x,aes(x,y=cdf,color=as.character(var),linetype=as.character(var)))+
  geom_line()+
  theme_bw()+
  labs(color="Variance",linetype="Variance",x="x",y="Cumulative distribution function")

#quantile function

probs=seq(0,1,0.01)
normal_cdf_by_x<-data.frame(p=probs,q=qnorm(probs,mean=1,sd=sqrt(1)),var=1)%>%
  rbind(data.frame(p=probs,q=qnorm(probs,mean=1,sd=sqrt(2)),var=2))%>%
  rbind(data.frame(p=probs,q=qnorm(probs,mean=1,sd=sqrt(3)),var=3))
ggplot(normal_cdf_by_x,aes(x=p,y=q,color=as.character(var),linetype=as.character(var)))+
  geom_line()+
  theme_bw()+
  labs(color="Variance",linetype="Variance",y="Quantile",x="Probability")



set.seed(0)
standardGaussianSample<-rnorm(100)
mean1Var3GaussianSampleA<-1+sqrt(3)*standardGaussianSample

set.seed(0)
mean1Var3GaussianSampleB<-rnorm(100,1,sqrt(3))
all.equal(mean1Var3GaussianSampleA,mean1Var3GaussianSampleB)

mean1Var3GaussianSampleA


colors<-c("Population density"="red","Sample kernel density"="blue",
          "Population mean"="green","Sample mean"="violet")
linetypes<-c("Population density"="solid","Sample kernel density"="dashed",
             "Population mean"="solid","Sample mean"="dashed")
ggplot()+labs(x="x",y="Density")+theme_bw()+
  geom_line(data=(normal_densities_by_x%>%
                    filter(var==3)),
            aes(x,y=density,color="Population density"))+
  # create plot of theoretical density
  geom_density(data=data.frame(x=mean1Var3GaussianSampleA),
               aes(x=x,color="Sample kernel density",
                   linetype="Sample kernel density"))+
  # add in kernel density plot from real sample
  geom_vline(aes(xintercept=1,color="Population mean",
                 linetype="Population mean"))+
  geom_vline(aes(xintercept=mean(mean1Var3GaussianSampleA),
                 color="Sample mean",linetype="Sample mean"))+
  scale_color_manual(name ="Legend",values=colors)+
  scale_linetype_manual(name="Legend",values=linetypes)



#Binomial distribution

p<-0.7
n<-50
binom_df<-data.frame(x=seq(0,n))%>%
  mutate(pmf=map_dbl(.x=x,~dbinom(x=.x,size=n,prob=p)))
binom_df%>%
  head(3)



inc<-0.01
mu=n*p
sigma=sqrt(n*p*(1-p))
gaussian_df<-data.frame(x=seq(0,n,inc))%>%
  mutate(pdf=map_dbl(.x=x,~dnorm(x=.x,mean=mu,sd=sigma)))
gaussian_df%>%head(3)


colors<-c("Gaussian pdf"="red","Binomial pmf"="blue")
fill<-c("Gaussian pdf"="white","Binomial pmf"="white")
ggplot()+labs(x="x",y="Probability")+theme_bw()+
  geom_line(data=gaussian_df,
            aes(x,y=pdf,color="Gaussian pdf"),size=2)+
  # create plot of Gaussian density
  geom_col(data=binom_df,
           aes(x=x,y=pmf,color="Binomial pmf",fill="Binomial pmf"))+
  scale_color_manual(name ="",values=colors)+
  scale_fill_manual(name ="",values=fill)+
  xlim(c(20,50))


my_cdf_exp<-function(x,lambda){
  if(x<0){
    return(0)
  }else{
    return(1-exp(-lambda*x))
  }
}

lambda<-1/2
map_dbl(.x=seq(-1,4),.f=~my_cdf_exp(x=.x,lambda=lambda))

test_inputs<-seq(-1,10,0.1)
my_cdf_output<-map_dbl(.x=test_inputs,.f=~my_cdf_exp(x=.x,lambda=lambda))
inbuilt_cdf_output<-map_dbl(.x=test_inputs,.f=~pexp(q=.x,rate=lambda))
all.equal(my_cdf_output,inbuilt_cdf_output)


my_quantile_exp<-function(p,lambda){
  q<--(1/lambda)*log(1-p)
  return(q)
}

inc<-0.01
test_inputs<-seq(inc,1-inc,inc)
my_quantile_output<-map_dbl(.x=test_inputs,.f=~my_quantile_exp(p=.x,lambda=lambda))
inbuilt_quantile_output<-map_dbl(.x=test_inputs,.f=~qexp(p=.x,rate=lambda))
all.equal(my_quantile_output,inbuilt_quantile_output)

