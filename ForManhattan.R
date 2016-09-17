#install.packages("gdata")
library("doBy")
require("gdata")
library("ggplot2")

perl<-"C:/Perl/bin/perl5.22.1.exe"
bk <- read.xls("rollingsales_manhattan.xls",pattern="BOROUGH",perl=perl)
write.csv(bk)

head(bk)

names(bk) <- tolower(names(bk))
bk$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",bk$sale.price))

# clean/format the data with regular expressions
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",  bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date)
bk$year.built <- as.numeric(as.character(bk$year.built))

# keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]
head(bk.sale)
summary(bk.sale)

#Price for various sq ft.
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))

#Number of houses for each year
ggplot(bk.sale, aes(x=year.built, fill="year.built"))+geom_histogram(binwidth=1)+scale_x_continuous(limits = c(1800, 2017))

#Mean price for year built
bk.sum<-summaryBy(sale.price~year.built, data=bk.sale)
bk.sum
ggplot(bk.sum, aes(x=year.built, y=sale.price.mean, fill="year.built"))+geom_bar(stat="identity", colour="blue", fill="blue")+scale_x_continuous(limits = c(1800, 2017))

#Mean sq ft for year built 
bk.sqft<-summaryBy(gross.sqft~year.built, data=bk.sale)
bk.sqft
ggplot(subset(bk.sqft,gross.sqft.mean>0), aes(x=year.built, y=gross.sqft.mean, fill="year.built"))+geom_bar(stat="identity", colour="red", fill="red")+scale_x_continuous(limits = c(1800, 2017))

#mean price by zipcode 
bk.zip<-summaryBy(sale.price.n~zip.code, data=bk.sale)
bk.zip
ggplot(subset(bk.zip,zip.code>0), aes(x=zip.code, y=sale.price.n.mean, fill="zip.code"))+geom_bar(stat="identity", colour="yellow", fill="yellow")

head(bk.sale)

#summary of price for category
bk.class=summaryBy(bk.sale$sale.price.n~bk.sale$building.class.category,data=bk.sale)
bk.class
ggplot(subset(bk.zip,zip.code>0), aes(x=zip.code, y=sale.price.n.mean, fill="zip.code"))+geom_bar(stat="identity", colour="yellow", fill="yellow")

## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY", bk.sale$building.class.category))]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
bk.homes[which(bk.homes$sale.price.n<100000),][order(bk.homes[which(bk.homes$sale.price.n<100000),] $sale.price.n),]

head(bk.homes)

## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
