## load required packages
library(modeldata)
library(tidyverse)

## load data
ames <- ames
View(ames)


## descriptive of ames data
ames %>% summarise( mu=mean(Lot_Area),
                   pop_med=median(Lot_Area),
                   sigma=sd(Lot_Area),
                   pop_min=min(Lot_Area),
                   pop_max=max(Lot_Area),
                   pop_iqr=IQR(Lot_Area),
                   pop_q1=quantile(Lot_Area,0.25),
                   pop_q3=quantile(Lot_Area,0.75))


ames_area <- ames$Lot_Area
summary(ames_area)

percentile_50th <- IQR(ames_area)

## Plot histogram
ggplot(data=ames, aes(x=Lot_Area)) + geom_histogram(binwidth = 250)

## The unknown sampling distribution
#50 sample size chosen
samp1 <- ames %>% sample_n(size =50)
dim(samp1)

samp1 %>% summarise(x_bar=mean(Lot_Area),
                    s_med=median(Lot_Area),
                    s_sd=sd(Lot_Area),
                    s_iqr=IQR(Lot_Area),
                    s_25_qtl=quantile(Lot_Area, 0.25),
                    s_75th_qtl=quantile(Lot_Area, 0.75))

#EDA of sample 
ggplot(data = samp1, aes(Lot_Area)) + geom_histogram()




ame <- ames %>% select(House_Style,Overall_Cond,Sale_Price)
View(ame)

## what is the average sale price of each housing style?
st <- aggregate(Sale_Price~House_Style,ame,mean)
ggplot(data= st, aes(x= reorder(House_Style,+Sale_Price),y=Sale_Price))+
  geom_col(fill="brown4")+theme_minimal()+coord_flip()+
  geom_text(label=round(st$Sale_Price,0), size=3.5, vjust=0.5)+xlab("House Style")+
  ggtitle("Average sale Prices of housing styles in Iowa state")

###Here we find that houses with one and half unfinished housing units cost the least in Iowa state
###whilst on average two and half finished houses has the highest price of $220,200

## what is the average sale price of house characterised by overall condition?
sc<- aggregate(Sale_Price ~ Overall_Cond, ame, mean)
ggplot(data= sc, aes(x= reorder(Overall_Cond,+Sale_Price),y=Sale_Price))+
  geom_col(fill="magenta4")+theme_classic()+coord_flip()+
  geom_text(label=round(sc$Sale_Price,0), size=3.5, vjust=0.5)+
  xlab("Overall Housing Condition")+
  ggtitle("Average Sale price per the overall housing condition of Iowa state houses")
  
  

## which type of housing style is more expensive?
he<- aggregate(Sale_Price ~ House_Style, ame, max)
ggplot(data= he, aes(x= reorder(House_Style,+Sale_Price), y=Sale_Price))+
  geom_col(fill="cyan4")+coord_flip()+theme_get()+
  geom_text(label=he$Sale_Price, size=3.5, vjust=0.5)+
  xlab("Housing Style")+
  ggtitle("The most expensive housing style in Iowa state")

#### Here we see that two story buildings are the most expensive from the graph shown

## what is the pricing distribution of the most expensive housing style?
tstory <- ame %>% subset(House_Style=="Two_Story")
View(tstory)
dim(tstory)
tstory %>% count(Overall_Cond)
# from this counts we find that there are only 2 two_story buildings with overall poor condition however 
# their mean sale price is beyond the over all fair condition houses. we want to investigate those 
#two house prices
tstory %>% subset(Overall_Cond == "Poor")
#here we see that 1 of the over all poor condition two story houses has a colossal sale price of 159000
#this account for the huge mean price over the over all fair condition houses.

#form the count we also see that there are more two story houses with over all average condition i.e 590

ats <- aggregate(Sale_Price ~ Overall_Cond, tstory, mean)
ggplot(ats, aes(x=reorder(Overall_Cond,+Sale_Price), y=Sale_Price)) + geom_col(fill="green3")+
  geom_text(label=round(ats$Sale_Price,0), size=3.5,vjust=0.5)+theme_bw()+coord_flip()+
  xlab("Housing Condition")+
  ggtitle("Average sale prices of two story building by their Over condition")

#now we want to explore the general sale price distribution of two story buildings 
#with overall average conditions

average_ts <- tstory %>% subset(Overall_Cond == "Average")
dim(average_ts)
average_ts$Sales <- round((average_ts$Sale_Price)/1000, 0)
view(average_ts)
range(average_ts$Sales)

mean_sales <- average_ts %>% summarise(mean_prx=mean(Sales))

ggplot(data = average_ts, aes(Sales))+
  geom_histogram(binwidth=18, fill="brown4",color="white") + theme_minimal()+
  geom_vline(aes(xintercept=mean_prx),mean_sales,color="black",linewidth=2)+
  ggtitle("Distribution of the Two story building sale price in Iowa state")
  

## which type of housing is less expensive?
hle <- aggregate(Sale_Price ~ House_Style, ame, min)
ggplot(data= hle, aes(x= reorder(House_Style,+Sale_Price), y=Sale_Price))+
  geom_col(fill="plum4")+coord_flip()+theme_get()+
  geom_text(label=hle$Sale_Price, size=3.5, vjust=0.5)+
  xlab("Housing Style")+
  ggtitle("The most expensive housing style in Iowa state")

# the least expensive house here is one story buildings with average sale price of 12,789 dollars

## what is the pricing distribution of the less costly housing style?
One_Story <- ame %>% subset(House_Style == "One_Story")
View( One_Story)
dim( One_Story)

One_Story %>% count(Overall_Cond)
# from this counts we find that there are only 2 one_story buildings with overall very poor condition.
#The over all average condition houses are many in the locality.

lts <- aggregate(Sale_Price ~ Overall_Cond, One_Story,mean)
ggplot(lts, aes(x=reorder(Overall_Cond,+Sale_Price), y=Sale_Price)) + geom_col(fill="tan3")+
  geom_text(label=round(lts$Sale_Price,0), size=3.5,vjust=0.5)+theme_bw()+coord_flip()+
  xlab("Housing Condition")+
  ggtitle("Average sale prices of two story building by their Over condition")

One_Story %>% subset(Overall_Cond == "Very_Poor")
#there are only two one story structures that are in very poor condition

#now we want to explore the general sale price distribution of one story buildings
One_Story$Sales_Px <- round((One_Story$Sale_Price)/1000,0)

view(One_Story)

mean_Px <- One_Story %>% summarise(mean_Px = mean(Sales_Px))

ggplot(data = One_Story, aes(Sales_Px))+
  geom_histogram(binwidth=20, fill="midnightblue",color="white") + theme_minimal()+
  geom_vline(aes(xintercept=mean_Px),mean_Px,color="red3",linewidth=1.5)+
  annotate("text", x=280,y=220,angle=0,color="black",size=4,label="Median Sale Price=179000")+
  ggtitle("Distribution of one story building sale Price in Iowa state")

##what is the most expensive housing unit in the dataset?
max(ame$Sale_Price)
### the most expensive house has a sale price of 755000 dollars
ame %>% filter(Sale_Price == 755000)

##what is the overall condition of this housing?
###the overall condition of the most expensive house is **Above_Average**

##what is the housing style of this housing unity?
###the House_Style of the most expensive house is **Two_Story**








