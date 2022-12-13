#Jo-Ann Ciobanu#
##12/05/2022##
###Final##
data<-read.csv("sqf.csv", stringsAsFactors = T)
data

##Question: Do factors such as age,sex, and race affect who gets arrested after being searched?

#AGE
#Checking how my people in each age group who are searched and arrested
age_less_30<-subset(data,age<=30 & searched=="Y" & arstmade=="Y")
nrow(age_less_30)
age_less_50<-subset(data,age>30 & age<=50 & searched=="Y" & arstmade=="Y")
nrow(age_less_50)
age_less_70<-subset(data,age>50 & age<=70 & searched=="Y" & arstmade=="Y")
nrow(age_less_70)
age_less_100<-subset(data,age>70 & age<=80 & searched=="Y" & arstmade=="Y")
nrow(age_less_100)
values<-c(nrow(age_less_30),nrow(age_less_50),nrow(age_less_70),nrow(age_less_100))
names(values)<-c("<30",">30 & <=50",">50 & <=70",">70 & <=100")
#graphing the data:
barplot(values, main = "Amount of arrest made after being searched by age ",cex.main=.75, ylim = c(0,20000),ylab="Number of People",cex.lab=.5)

##data shows that almost 17,000 people searched and arrested where under 30, interesting
#----------------------
#Checking how my people in each age group who are searched and not arrested
age_less_301<-subset(data,age<=30 & searched=="Y" & arstmade=="N")
nrow(age_less_301)
age_less_501<-subset(data,age>30 & age<=50 & searched=="Y" & arstmade=="N")
nrow(age_less_501)
age_less_701<-subset(data,age>50 & age<=70 & searched=="Y" & arstmade=="N")
nrow(age_less_701)
age_less_1001<-subset(data,age>70 & age<=80 & searched=="Y" & arstmade=="N")
nrow(age_less_1001)
values1<-c(nrow(age_less_301),nrow(age_less_501),nrow(age_less_701),nrow(age_less_1001))
names(values1)<-c("<30",">30 & <=50",">50 & <=70",">70 & <=100")
#graphing the data:
barplot(values1, main = "Amount of NOT arrest made after being searched by age ",cex.main=.75, ylim = c(0,30000),ylab="Number of People",cex.lab=.5)
values
values1

#In both graphs it shows that those who are in the range of <30 had the most 
#people being searched and the most people being arrested aswell

#Q. Does sex have an effect on it aswell?

#SEX
male<-subset(data,sex=="MALE"& searched=="Y"&arstmade=="Y")
nrow(male)
female<-subset(data,sex=="FEMALE"& searched=="Y"&arstmade=="Y")
nrow(female)
dat<-c(nrow(male),nrow(female))
names(dat)<-c("MALE","FEMALE")
#graphing data:
barplot(dat, main = "Amount of arrest made after being searched by SEX ",cex.main=.75, ylim = c(0,30000),ylab="Number of People",cex.lab=.5)

#Intersting to see that about 25,000 men are searched and arrested vs only around 5,000 females

#What about not arrested
male1<-subset(data,sex=="MALE"& searched=="Y"&arstmade=="N")
nrow(male1)
female1<-subset(data,sex=="FEMALE"& searched=="Y"&arstmade=="Y")
nrow(female1)
dat1<-c(nrow(male1),nrow(female1))
names(dat1)<-c("MALE","FEMALE")
#graphing data:
barplot(dat1, main = "Amount of NOT arrest made after being searched by SEX ",cex.main=.75, ylim = c(0,40000),ylab="Number of People",cex.lab=.5)

#Interseting to see that there are about 50,000 men searched in general vs around 10,000 women

#Q. What about the <30 age group?

male2<-subset(data,sex=="MALE"& searched=="Y"&arstmade=="Y" & age<=30)
nrow(male2)
female2<-subset(data,sex=="FEMALE"& searched=="Y"&arstmade=="Y" & age<=30)
nrow(female2)
dat2<-c(nrow(male2),nrow(female2))
names(dat2)<-c("MALE","FEMALE")
#graphing data:
barplot(dat2, main = "Amount of arrest made after being searched by SEX under 30 ",cex.main=.75, ylim = c(0,20000),ylab="Number of People",cex.lab=.5)

# This is BIG, out of those who are <30, almost 16,000 are men vs that of females which is way less than 5,000
#Maybe men are discriminated against more in this instance?

#Q. What about Race which race is affected the most

na<-subset(data,race=="AMERICAN INDIAN/ALASKAN NATIVE" & searched=="Y"& arstmade=="Y")
a<-subset(data,race=="ASIAN /PACIFIC ISLANDER" & searched=="Y"& arstmade=="Y")
b<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y")
bh<-subset(data,race=="BLACK HISPANIC" & searched=="Y"& arstmade=="Y")
w<-subset(data,race=="WHITE" & searched=="Y"& arstmade=="Y")
wh<-subset(data,race=="WHITE HISPANIC" & searched=="Y"& arstmade=="Y")

dat3<-c(nrow(na),nrow(a),nrow(b),nrow(bh),nrow(w),nrow(wh))
names(dat3)<-c("Native","Asian","Black","B Hispanic","White","W Hispanic")

#graph:
barplot(dat3, main = "Amount of arrest made after being searched by Race ",cex.main=.75, ylim = c(0,20000),ylab="Number of People",cex.lab=.5)

# We can see that Blacks are the highest in the count at around 15,000 while whites sit around below 5,000

#Q. What bout not arrested
na1<-subset(data,race=="AMERICAN INDIAN/ALASKAN NATIVE" & searched=="Y"& arstmade=="N")
a1<-subset(data,race=="ASIAN /PACIFIC ISLANDER" & searched=="Y"& arstmade=="N")
b1<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="N")
bh1<-subset(data,race=="BLACK HISPANIC" & searched=="Y"& arstmade=="N")
w1<-subset(data,race=="WHITE" & searched=="Y"& arstmade=="N")
wh1<-subset(data,race=="WHITE HISPANIC" & searched=="Y"& arstmade=="N")

dat31<-c(nrow(na1),nrow(a1),nrow(b1),nrow(bh1),nrow(w1),nrow(wh1))
names(dat31)<-c("Native","Asian","Black","B Hispanic","White","W Hispanic")

#graph:
barplot(dat31, main = "Amount of NOT arrest made after being searched by Race ",cex.main=.75, ylim = c(0,20000),ylab="Number of People",cex.lab=.5)
dat3
dat31

#We can see that Blacks are the highest here, but that this indicates that they are the highest 
#to be searched in the first place at around 21,000 vs whites are around 6,000

#There could be bias in this case

#Q. What about race and sex then?
#Lets look at the Black people cause they are the most affected
b1M<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y"& sex=="MALE")
b1F<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y"& sex=="FEMALE")
dat32<-c(nrow(b1M),nrow(b1F))
names(dat32)<-c("MALE","FEMALE")
barplot(dat32, main = "Amount of arrest made after being searched by Black and sex ",cex.main=.75, ylim = c(0,20000),ylab="Number of People",cex.lab=.5)

#Black men are searched and arrested by 10,000 more than the women

#Q. What about race and age then?
#Lets look at the Black people cause they are the most affected
b_less_30<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y"& age<=30)
b_less_50<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y"& age<=50 & age>30)
b_less_70<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y"& age<=70 & age>50)
b_less_100<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y"& age<=100 & age>70)
dat33<-c(nrow(b_less_30),nrow(b_less_50),nrow(b_less_70),nrow(b_less_100))
names(dat33)<-c("<30",">30 & <=50",">50 & <=70",">70 & <=100")
barplot(dat33, main = "Amount of arrest made after being searched by Black and age ",cex.main=.75, ylim = c(0,20000),ylab="Number of People",cex.lab=.5)

#Just below 10,000 is the amount of black people who are searched and then arrested

#Q. Now lets look at the sex break up of that

b2M<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y"& sex=="MALE" & age<=30)
b2F<-subset(data,race=="BLACK" & searched=="Y"& arstmade=="Y"& sex=="FEMALE" & age<=30)
dat34<-c(nrow(b2M),nrow(b2F))
names(dat34)<-c("MALE","FEMALE")
barplot(dat34, main = "Amount of arrest made after being searched by Black and sex and <30 ",cex.main=.75, ylim = c(0,20000),ylab="Number of People",cex.lab=.5)
dat34

# Around 8,000 of these people who are Black and <30 are Males and around 794 are Women

# All this data toghether shows that there are factors such as sex,age, and race that are affected
# by searching, and of those people, those who are Black, Male, <30 are the most affected

