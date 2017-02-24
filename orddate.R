#install.packages(c("lubridate", "plyr"))
library("lubridate"); library("plyr")

# 0. making a sample data
len = 10 #the number of observations
set.seed(1)
id = sample(1:2, len, replace=T)
date = sample(seq(as.Date("2016/09/01"), as.Date("2017/08/31"), "day"), len, replace=T)
item = sample(c("a", "b", "c"), len, replace=T)
#head(id); head(date); head(item)

data1 = data.frame(id, date, item)
data1 = data1[order(data1$id, data1$date),]
head(data1)

#write.csv(data1, file.choose())
#data1 = read.csv(file.choose()) #read the csv file
#data1 = data1[,-1]



# 1. function1
ord.date = function(data1, pd, st.date=1) { #st.date = start date #pd = period
#pd = 3; st.date = 9; st.date = 2

# (1) specify the format for the inputs  
# (i) data
  data1 = data.frame(data1) #form the data as data frame format
  colnames(data1) = c("ID", "Date", "Item")
  data1$Date = as_date(data1$Date, "day")
  #data.class(data1$Date)
  
  id.list = split(data1, data1$ID) #split data for each ID
  #data.class(id.list)

  for (i in 1:length(id.list)) {

# (ii) period
# period = 1: weekly
#          2: calendar monthly 
#          3: quaterly of a year 
#          4: twice of a year
#          5: annual
    
# (iii) start date
# for period = 1, start date: Sun=1 < Mon=2 < ... < Sat=6
# for period = 4,             1, ..., 12
# for period = 5,             1, ..., 12
    
# (2) get the number for each period: dif.date
    if (pd == 1) {
      st.date = st.date + ifelse(st.date > wday(min(id.list[[i]]$Date)), -7, 0)
      r.st.date = min(id.list[[i]]$Date) - wday(min(id.list[[i]]$Date)) + st.date #real start date
      dif.day = as.numeric(id.list[[i]]$Date - r.st.date) #day difference
      dif.date = paste("week", dif.day %/% 7 + 1)
    }
    if (pd == 2) {
      fst.date = c(year(min(id.list[[i]]$Date)), month(min(id.list[[i]]$Date)))
      ym.mat = matrix(c(year(id.list[[i]]$Date), month(id.list[[i]]$Date)), ncol=2)
      dif.mat = ym.mat - matrix(fst.date, byrow=T, nrow=nrow(ym.mat), ncol=2) #year & month difference
      dif.date = paste("month", dif.mat[,1]*12 + dif.mat[,2] + 1)
    }
    if (pd == 3) {
      fst.date = c(year(min(id.list[[i]]$Date)), quarter(min(id.list[[i]]$Date)))
      ym.mat = matrix(c(year(id.list[[i]]$Date), quarter(id.list[[i]]$Date)), ncol=2)
      dif.mat = ym.mat - matrix(fst.date, byrow=T, nrow=nrow(ym.mat), ncol=2) #year & month difference
      dif.date = paste("quarter", dif.mat[,1]*4 + dif.mat[,2] + 1)
    }
    if (pd == 4) {
      st.year = year(min(id.list[[i]]$Date)) - (st.date > month(min(id.list[[i]]$Date)))
      r.st.date = c(st.year, st.date) #real start date
      ym.mat = matrix(c(year(id.list[[i]]$Date), month(id.list[[i]]$Date)), ncol=2)
      dif.mat = ym.mat - matrix(r.st.date, byrow=T, nrow=nrow(ym.mat), ncol=2) 
      dif.mon = dif.mat[,1]*12 + dif.mat[,2] #month difference
      dif.date = paste("half", dif.mon %/% 6 + 1)
    }
    if (pd == 5) {
      st.year = year(min(id.list[[i]]$Date)) - (st.date > month(min(id.list[[i]]$Date)))
      r.st.date = c(st.year, st.date) #real start date
      ym.mat = matrix(c(year(id.list[[i]]$Date), month(id.list[[i]]$Date)), ncol=2)
      dif.mat = ym.mat - matrix(r.st.date, byrow=T, nrow=nrow(ym.mat), ncol=2)
      dif.mon = dif.mat[,1]*12 + dif.mat[,2] #month difference
      dif.date = paste("year", dif.mon %/% 12 + 1)
    }
  
# (3) output
    id.list[[i]]$Date = dif.date
  }

  data1 = ldply(id.list, data.frame)[,-1]
  return(data1)
}

# 2. test function 1
data2 = ord.date(data1, 4, 9)
data2

# 3. function 2
cvt.seq = function(data1, pd, st.date=1) {
  data2 = ord.date(data1, pd, st.date)
#data1 = data2
  id.list = split(data2, data2$ID)
  seq.list = vector(mode="list", length=length(id.list)) #to save the result
  
  for (i in 1:length(id.list)) {
    date.list = split(id.list[[i]], id.list[[i]]$Date)
    for (j in 1:length(date.list)) {
      seq.list[[i]][[j]] = unique(date.list[[j]]$Item)
    }
  }
  return(seq.list)
}

# 4. test function 2
data3 = cvt.seq(data1, 4, 9)
data3
