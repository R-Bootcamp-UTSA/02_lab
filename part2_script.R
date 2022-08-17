##########################################
## R Bootcamp - Part 2: Data import, wrangling & Vizualization
## Author: Esteban Lopez Ochoa Ph.D., Wei Zhai, Ph.D.
## Program: Master of Science in Urban and Regional Planning
## Institution: The University of Texas at San Antonio
##########################################

#---- Objectives ----
# In this Lab you will learn to:

# 1. The `data.table` syntax for data manipulation
# 2. Learn change, create, delete, sort, variables within a dataset.
# 3. Merge or combine two datasets, map visualization
# 4. Data visualization

#data.table is a package that extends data.frames. 
#Two of data.table's most notable features are speed and cleaner syntax.
#Part 0. The `data.frame` basis and why do we need `data.table`

#data.table, read data 
d0<-read.csv("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv",header = T)
#Of course, we can use "Import Dataset" directly

# Let's read the data using `data.frame`. 3.38 sec elapsed
system.time({
  d0<-read.csv("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv",header = T)
})
# “User CPU time” gives the CPU time spent by the current process (i.e., the current R session)
# “system CPU time” gives the CPU time spent by the kernel (the operating system) on behalf of the current process.

##How could we access a vairable in `data.frame` ?
d0$czname # access a variable within dataset
d0[,c("czname")] #same


#How could we filter data records with conditions in data.frame?
d0_SA<-d0[d0$czname=="San Antonio", ] # Filters only SA data


#How to create a new variable?
d0$ln_income<-log(d0$hhinc_mean2000) # creates a variable
d0$ln_income<-NULL # delete a variable

#What if we want to know the mean income  and mean commute time of the tracts in each city
by(d0[,c("hhinc_mean2000","mean_commutetime2000")],
   INDICES = d0$czname, 
   FUN = function(x){sapply(x, mean,na.rm=T)}) # operations by a third variable
#you can define the function by yourself, try FUN = function(x){sapply(x, max,na.rm=T)


#---- Part 1: The `data.table` syntax for data manipulation ----

install.packages("data.table")

library(data.table)

d1<-fread("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv")

system.time({
  d1<-fread("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv")
})

class(d1)

#How could we access a variable in `data.table` ? It is same as data.frame
d1$czname # access a variable within dataset
d1[,.(state,county,tract)] #same, but better

#How to find city names where hh income is greater than 50000 and poor share is less than 5%?
d1[hhinc_mean2000>50000 & poor_share2000<0.05,names(table(czname))]

View(d1[,.N,by=czname])
#'.N' is a special built-in variable that holds the number of observations in the current group.

#How could we filter data records with conditions in data.table?
d1[czname=="San Antonio",] # Filters only SA data, compare it data.frame above
#d0[czname=="San Antonio",]

#How to create in data.table? colon + equal 
d1[,ln_income:=log(hhinc_mean2000)] # creates a variable
d1[,ln_income:=NULL]       # remove a column by reference

#How to get the mean value of hh income for each city 
#'na.rm', To exclude missing values when performing these calculations
mean_values<-d1[,mean(hhinc_mean2000,na.rm=TRUE), by=czname]

#How to get the mean value of for multiple columns, at the state level 
mean_values_incom_poor<-d1[,.(mean(hhinc_mean2000,na.rm=TRUE),
                               mean(poor_share2000,na.rm=TRUE)), 
                               by=.(state)]

#How to change the column names? "v1", "v2" are default names
colnames(mean_values_incom_poor)[2] <- "mean_income_share"
colnames(mean_values_incom_poor)[3] <- "mean_poor_share"

#of course, we have include more variables for the calcualtion
mean_values_incom_poor_pop<-d1[,.(mean(hhinc_mean2000,na.rm=TRUE),
                               mean(poor_share2000,na.rm=TRUE), 
                               mean(popdensity2000,na.rm=TRUE)), 
                            by=.(state)]

d1[,.(mean_income=mean(hhinc_mean2000),mean_com_time=mean(mean_commutetime2000)),by=czname] # operations by a third variable

#----- Part 2. Learn change, create, delete, sort, variables within a dataset.-----

# How to get the count of cities in the table
d1[,table(czname)] # variable exploration - counts
count_city=d1[,.N, by=.(czname)] #alternative

# What if we only need the information about our city?
SA_OI<- d1[czname=="San Antonio",] # filtering + object creation (subsetting)

# How to get rid of unknown objects
SA_OI<- SA_OI[is.na(hhinc_mean2000)==FALSE,] # creating an object without na's

# create a new variable
SA_OI[, ones:=1] # variable creation

# create a new variable, hh income less than the median income
SA_OI[hhinc_mean2000<=quantile(hhinc_mean2000,probs = 0.5), 
      bellow_medianIncome:=1] # variable creation by condition

#quantile(SA_OI$hhinc_mean2000,probs = 0.5) #probs: Numeric vector of probabilities

# define 0 for tracts above median income
SA_OI[is.na(bellow_medianIncome), bellow_medianIncome:=0] # variable value replacement by condition

# calculate the median household income growth rate
SA_OI[, med_hhinc_growth1990_2006:=((med_hhinc2016-med_hhinc1990)/med_hhinc1990)*100]# variable creation by a mathematical manipulation of other variables.


#---- Part 3. Merge or combine two datasets ----
# download census data using the API
install.packages('tidycensus')

library(tidycensus)

census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648",install = TRUE)# you must acquired your own key at http://api.census.gov/data/key_signup.html

#Capture the census tract-level median income data in Bexar county
#Margins of error (MOE) are provided for every American Community Survey (ACS) estimate.
bexar_medincome <- get_acs(geography = "tract", variables = "B19013_001",
                           state = "TX", county = "Bexar", geometry = TRUE,year = 2019)

plot(bexar_medincome)


#Next, let's merge SA_OI data with the map

#check the column names
head(SA_OI) ; head(bexar_medincome)

SA_OI[,GEOID:=paste0(state,"0",county,tract)] #creating a GEOID variable to have a common variable between the two data sets

#check variable classes
class(bexar_medincome$GEOID)
class(SA_OI$GEOID)

#find how many of them have overlaps
table(bexar_medincome$GEOID %in%  SA_OI$GEOID) # checking overlap

# merge/join by GEOIDS
bexar_medincome2 <- merge(bexar_medincome,SA_OI,by="GEOID",)

#plotting
plot(bexar_medincome2[,"med_hhinc_growth1990_2006"])# fast plotting

library(ggplot2)
library(viridis) # prettier plotting

'geom_sf() allows for visualizing sf objects. Conveniently, 
geom_sf() automatically detects the geometry type of spatial 
objects stored in sf and draw maps accordingly. '

ggplot(bexar_medincome2)+
  geom_sf(aes(fill=med_hhinc_growth1990_2006))+
  #geom_sf(aes(fill=med_hhinc1990))+  #we can change the variable
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")


library(leaflet) # dynamic plotting

pal <- colorQuantile("YlOrRd", domain = bexar_medincome2$med_hhinc_growth1990_2006,n = 5)

leaflet(bexar_medincome2)%>%
  addProviderTiles(provider = providers$CartoDB.Positron)%>%
  addPolygons(fillColor = ~pal(med_hhinc_growth1990_2006),
              label = ~med_hhinc_growth1990_2006,
              color=~pal(med_hhinc_growth1990_2006),
              fillOpacity = 0.5,
              weight = 0.1)%>%
  addLegend(pal = pal,values = ~med_hhinc_growth1990_2006,opacity = 1,title = "Income Growth 1990-2016",labels = c("a","b","c","d","e"))


#----- Part 4. Data visualization-----
bb<-fread("https://data.sanantonio.gov/dataset/05012dcb-ba1b-4ade-b5f3-7403bc7f52eb/resource/fbb7202e-c6c1-475b-849e-c5c2cfb65833/download/accelasubmitpermitsextract.csv")
names(bb)

# get the count of different permit applications
bb[,.N,by=.(`PERMIT TYPE`)]

# we can find the permit type for "Res Building Application" only, and then get the count of date issued
bb[`PERMIT TYPE`=="Res Building Application",.N,by=.(`DATE ISSUED`)]

# we can also do it for data submitted
bb[`PERMIT TYPE`=="Res Building Application",.N,by=.(`DATE SUBMITTED`)]

# ensure the date submitted is in R-recognized date
bb[,date:=as.Date(`DATE SUBMITTED`)]
class(bb$`DATE SUBMITTED`)
class(bb$date)

# visualize the temporal change
ggplot(bb[`PERMIT TYPE`=="Res Building Application",.N,by=.(date)],
       aes(x=date,y=N))+
       geom_line()
      #geom_point()


# extract the month from date, the rationale of line 
bb[,mm:=month(date)]

# extract the year from date
bb[,yy:=year(date)]

#find the year and month of those applications
new_housing<-bb[`PERMIT TYPE`=="Res Building Application",.N,by=.(yy,mm)]

#create a new date column for the new dataframe
new_housing[,date:=as.Date(paste0(yy,"-",mm,"-","01"))]

#visualize the data by month
ggplot(new_housing,aes(x=date,y=N))+geom_line()

# get the count based on the year, month, and permit type
nh2<-bb[,.N,by=.(yy,mm,`PERMIT TYPE`)]

#create a new date column for the new dataframe
nh2[,date:=as.Date(paste0(yy,"-",mm,"-","01"))]

#visulaze
'
facet_wrap() makes a long ribbon of panels (generated by any number of 
variables) and wraps it into 2d. This is useful if you have a single 
variable with many levels and want to arrange the plots in a more 
space efficient manner. The scales argument is for freeing the y for each facetted plot.
Use it when the ranges of your variables vary greatly and need to be freed.
'

ggplot(nh2[yy==2022],aes(x=date,y=N))+
  geom_col()+
  facet_wrap(~`PERMIT TYPE`,scales = "free_y")+
  labs(title = "CoSA Building Permits visualization", 
       subtitle = "Number of applications submitted by date submitted", 
       caption = "Source: https://data.sanantonio.gov/dataset/building-permits")


