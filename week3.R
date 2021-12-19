library(tidyverse)

impute_by_mean<-function(x){
  mu<-mean(x,na.rm=1)# first compute the mean of x
  impute_f<-function(z){# coordinate-wise imputation
    if(is.na(z)){
      return(mu)# if z is na replace with mean
    }else{
      return(z)# otherwise leave in place
    }
  }
  return(map_dbl(x,impute_f))# apply the map function to impute across vector
}

install.packages("readxl")

library(readxl)# load the readxl library
folder_path<-"D:\\study\\R\\firstRProject\\firstRProject\\"

file_name<-"HockeyLeague.xlsx"# set the file name
file_path<-paste(folder_path,file_name,sep="")# create the file_path
wins_data_frame<-read_excel(file_path,sheet="Wins")# read of a sheet from an xl file

wins_data_frame%>%
  select(1:5)%>%
  head(3)
library(tidyr)

read_excel(file_path,sheet="Wins")%>%
  rename(Team=...1)%>%
  pivot_longer(!Team,names_to="Year",values_to="val")%>%
  mutate(Year=as.integer(Year))%>%
  separate(col=val,into=c("Wins","Total"),sep=" of ",convert=TRUE)

