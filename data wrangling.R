library(data.table)
library(tidyverse)
library(lubridate)

path = '/Users/harshit/Desktop/UIUC/Fall 2020/Econ 490/Final Project/Data'
list.files(path)

###############
## Crude Oil ##
###############

crudeOil1 = fread(paste0(path, '/crude oil price1.csv'))
crudeOil2 = fread(paste0(path, '/crude oil price2.csv'))
crudeOil3 = fread(paste0(path, '/crude oil price3.csv'))

names(crudeOil1) = crudeOil1 %>% names %>% tolower
names(crudeOil2) = crudeOil2 %>% names %>% tolower
names(crudeOil3) = crudeOil3 %>% names %>% tolower

crudeOil = rbind(crudeOil1, crudeOil2)
crudeOil = rbind(crudeOil, crudeOil3)

crudeOil = rename(crudeOil, crudeoilusdperbarrel = usdperbarrel)

# Ordering data based on date
setorder(crudeOil, date)

# Converting type
crudeOil$crudeoilusdperbarrel = as.numeric(crudeOil$crudeoilusdperbarrel)

# Removing redundant data
rm(crudeOil1)
rm(crudeOil2)
rm(crudeOil3)

##################################
## Effective federal fund rates ##
##################################

effr = fread(paste0(path, '/Effective federal fund rates.csv'))

names(effr) = effr %>% names %>% tolower

effr$effr = as.numeric(effr$effr)

setorder(effr, date)


##########
## Gold ##
##########

gold = fread(paste0(path, '/gold.csv'))

names(gold) = gold %>% names %>% tolower

gold = rename(gold, goldusdpertroyounce = usdpertroyounce)

gold$goldusdpertroyounce = as.numeric(gold$goldusdpertroyounce)

setorder(gold, date)


###################################
## Indian government bond yields ##
###################################

igby = fread(paste0(path, '/India 30-Year Bond Yield Historical Data.csv'))

names(igby) = igby %>% names %>% tolower

igby = rename(igby, igbyrate = price)
igby = rename(igby, strdate = date)

igby = igby %>% mutate(date = mdy(strdate))

igby = igby %>% select(c('date', 'igbyrate'))

setorder(igby, date)


##################
## Indian Index ##
##################

indiaindex = fread(paste0(path, '/India index.csv'))

names(indiaindex) = indiaindex %>% names %>% tolower

indiaindex = indiaindex %>% select(c('date', 'open'))

indiaindex = rename(indiaindex, indiaindex = open)

indiaindex$indiaindex = as.numeric(indiaindex$indiaindex)

setorder(indiaindex, date)


################
## Moody DAAA ##
################

moody = fread(paste0(path, '/Moody.csv'))

names(moody) = moody %>% names %>% tolower

moody$daaa = as.numeric(moody$daaa)

setorder(moody, date)


###################
## Treasure Bill ##
###################

treasurebill = fread(paste0(path, '/Treasury bills.csv'))

names(treasurebill) = treasurebill %>% names %>% tolower

treasurebill$dtb3 = as.numeric(treasurebill$dtb3)

treasurebill = rename(treasurebill, treasurebill = dtb3)

setorder(treasurebill, date)


##############
## US Index ##
##############

usindex = fread(paste0(path, '/US index.csv'))

names(usindex) = usindex %>% names %>% tolower

usindex = usindex %>% select(c('date', 'open'))

usindex = rename(usindex, usindex = open)

setorder(usindex, date)


####################
## Exchange Rates ##
####################

exchangerate1 = fread(paste0(path, '/USD_INR Historical Data.csv'))
exchangerate2 = fread(paste0(path, '/USD_INR Historical Data (1).csv'))

names(exchangerate1) = exchangerate1 %>% names %>% tolower
names(exchangerate2) = exchangerate2 %>% names %>% tolower

exchangerate = rbind(exchangerate2, exchangerate1)

exchangerate = exchangerate %>% select(c('date', 'price'))

exchangerate = rename(exchangerate, exchangerate = price)

exchangerate$usdperinr = 1/exchangerate$exchangerate

exchangerate = exchangerate %>% select(c('date', 'usdperinr'))

exchangerate = rename(exchangerate, strdate = date)

exchangerate = exchangerate %>% mutate(date = mdy(strdate))

exchangerate = exchangerate %>% select(c('date', 'usdperinr'))

setorder(exchangerate, date)

rm(exchangerate1)
rm(exchangerate2)


###################
## Final Dataset ##
###################

dataset = inner_join(crudeOil, effr, by = 'date')
dataset = merge(dataset, exchangerate, by = 'date')
dataset = inner_join(dataset, gold, by = 'date')
dataset = merge(dataset, igby, by = 'date')
dataset = inner_join(dataset, indiaindex, by = 'date')
dataset = inner_join(dataset, moody, by = 'date')
dataset = inner_join(dataset, treasurebill, by = 'date')
dataset = inner_join(dataset, usindex, by = 'date')

dataset = na.omit(dataset)

dataset$indiaindex = dataset$indiaindex * dataset$usdperinr

# Inflation with base year 2007

dataset$usindex[dataset$date < as.Date('2020-12-12')] = dataset$usindex[dataset$date < as.Date('2020-12-12')] * 1
dataset$usindex[dataset$date < as.Date('2020-01-01') & dataset$date >= as.Date('2019-01-01')] = dataset$usindex[dataset$date < as.Date('2020-01-01') & dataset$date >= as.Date('2019-01-01')] * 1.03
dataset$usindex[dataset$date < as.Date('2019-01-01') & dataset$date >= as.Date('2018-01-01')] = dataset$usindex[dataset$date < as.Date('2019-01-01') & dataset$date >= as.Date('2018-01-01')] * 1.05
dataset$usindex[dataset$date < as.Date('2018-01-01') & dataset$date >= as.Date('2017-01-01')] = dataset$usindex[dataset$date < as.Date('2018-01-01') & dataset$date >= as.Date('2017-01-01')] * 1.07
dataset$usindex[dataset$date < as.Date('2017-01-01') & dataset$date >= as.Date('2016-01-01')] = dataset$usindex[dataset$date < as.Date('2017-01-01') & dataset$date >= as.Date('2016-01-01')] * 1.10
dataset$usindex[dataset$date < as.Date('2016-01-01') & dataset$date >= as.Date('2015-01-01')] = dataset$usindex[dataset$date < as.Date('2016-01-01') & dataset$date >= as.Date('2015-01-01')] * 1.11
dataset$usindex[dataset$date < as.Date('2015-01-01') & dataset$date >= as.Date('2014-01-01')] = dataset$usindex[dataset$date < as.Date('2015-01-01') & dataset$date >= as.Date('2014-01-01')] * 1.11
dataset$usindex[dataset$date < as.Date('2014-01-01') & dataset$date >= as.Date('2013-01-01')] = dataset$usindex[dataset$date < as.Date('2014-01-01') & dataset$date >= as.Date('2013-01-01')] * 1.13
dataset$usindex[dataset$date < as.Date('2013-01-01') & dataset$date >= as.Date('2012-01-01')] = dataset$usindex[dataset$date < as.Date('2013-01-01') & dataset$date >= as.Date('2012-01-01')] * 1.15
dataset$usindex[dataset$date < as.Date('2012-01-01') & dataset$date >= as.Date('2011-01-01')] = dataset$usindex[dataset$date < as.Date('2012-01-01') & dataset$date >= as.Date('2011-01-01')] * 1.18
dataset$usindex[dataset$date < as.Date('2011-01-01') & dataset$date >= as.Date('2010-01-01')] = dataset$usindex[dataset$date < as.Date('2011-01-01') & dataset$date >= as.Date('2010-01-01')] * 1.20
dataset$usindex[dataset$date < as.Date('2010-01-01') & dataset$date >= as.Date('2009-01-01')] = dataset$usindex[dataset$date < as.Date('2010-01-01') & dataset$date >= as.Date('2009-01-01')] * 1.23
dataset$usindex[dataset$date < as.Date('2009-01-01') & dataset$date >= as.Date('2008-01-01')] = dataset$usindex[dataset$date < as.Date('2009-01-01') & dataset$date >= as.Date('2008-01-01')] * 1.23
dataset$usindex[dataset$date < as.Date('2008-01-01')] = dataset$usindex[dataset$date < as.Date('2008-01-01')] * 1.29



dataset$indiaindex[dataset$date < as.Date('2020-12-12')] = dataset$indiaindex[dataset$date < as.Date('2020-12-12')] * 1
dataset$indiaindex[dataset$date < as.Date('2020-01-01') & dataset$date >= as.Date('2019-01-01')] = dataset$indiaindex[dataset$date < as.Date('2020-01-01') & dataset$date >= as.Date('2019-01-01')] * 1.03
dataset$indiaindex[dataset$date < as.Date('2019-01-01') & dataset$date >= as.Date('2018-01-01')] = dataset$indiaindex[dataset$date < as.Date('2019-01-01') & dataset$date >= as.Date('2018-01-01')] * 1.05
dataset$indiaindex[dataset$date < as.Date('2018-01-01') & dataset$date >= as.Date('2017-01-01')] = dataset$indiaindex[dataset$date < as.Date('2018-01-01') & dataset$date >= as.Date('2017-01-01')] * 1.07
dataset$indiaindex[dataset$date < as.Date('2017-01-01') & dataset$date >= as.Date('2016-01-01')] = dataset$indiaindex[dataset$date < as.Date('2017-01-01') & dataset$date >= as.Date('2016-01-01')] * 1.10
dataset$indiaindex[dataset$date < as.Date('2016-01-01') & dataset$date >= as.Date('2015-01-01')] = dataset$indiaindex[dataset$date < as.Date('2016-01-01') & dataset$date >= as.Date('2015-01-01')] * 1.11
dataset$indiaindex[dataset$date < as.Date('2015-01-01') & dataset$date >= as.Date('2014-01-01')] = dataset$indiaindex[dataset$date < as.Date('2015-01-01') & dataset$date >= as.Date('2014-01-01')] * 1.11
dataset$indiaindex[dataset$date < as.Date('2014-01-01') & dataset$date >= as.Date('2013-01-01')] = dataset$indiaindex[dataset$date < as.Date('2014-01-01') & dataset$date >= as.Date('2013-01-01')] * 1.13
dataset$indiaindex[dataset$date < as.Date('2013-01-01') & dataset$date >= as.Date('2012-01-01')] = dataset$indiaindex[dataset$date < as.Date('2013-01-01') & dataset$date >= as.Date('2012-01-01')] * 1.15
dataset$indiaindex[dataset$date < as.Date('2012-01-01') & dataset$date >= as.Date('2011-01-01')] = dataset$indiaindex[dataset$date < as.Date('2012-01-01') & dataset$date >= as.Date('2011-01-01')] * 1.18
dataset$indiaindex[dataset$date < as.Date('2011-01-01') & dataset$date >= as.Date('2010-01-01')] = dataset$indiaindex[dataset$date < as.Date('2011-01-01') & dataset$date >= as.Date('2010-01-01')] * 1.20
dataset$indiaindex[dataset$date < as.Date('2010-01-01') & dataset$date >= as.Date('2009-01-01')] = dataset$indiaindex[dataset$date < as.Date('2010-01-01') & dataset$date >= as.Date('2009-01-01')] * 1.23
dataset$indiaindex[dataset$date < as.Date('2009-01-01') & dataset$date >= as.Date('2008-01-01')] = dataset$indiaindex[dataset$date < as.Date('2009-01-01') & dataset$date >= as.Date('2008-01-01')] * 1.23
dataset$indiaindex[dataset$date < as.Date('2008-01-01')] = dataset$indiaindex[dataset$date < as.Date('2008-01-01')] * 1.29



dataset$goldusdpertroyounce[dataset$date < as.Date('2020-12-12')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2020-12-12')] * 1
dataset$goldusdpertroyounce[dataset$date < as.Date('2020-01-01') & dataset$date >= as.Date('2019-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2020-01-01') & dataset$date >= as.Date('2019-01-01')] * 1.03
dataset$goldusdpertroyounce[dataset$date < as.Date('2019-01-01') & dataset$date >= as.Date('2018-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2019-01-01') & dataset$date >= as.Date('2018-01-01')] * 1.05
dataset$goldusdpertroyounce[dataset$date < as.Date('2018-01-01') & dataset$date >= as.Date('2017-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2018-01-01') & dataset$date >= as.Date('2017-01-01')] * 1.07
dataset$goldusdpertroyounce[dataset$date < as.Date('2017-01-01') & dataset$date >= as.Date('2016-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2017-01-01') & dataset$date >= as.Date('2016-01-01')] * 1.10
dataset$goldusdpertroyounce[dataset$date < as.Date('2016-01-01') & dataset$date >= as.Date('2015-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2016-01-01') & dataset$date >= as.Date('2015-01-01')] * 1.11
dataset$goldusdpertroyounce[dataset$date < as.Date('2015-01-01') & dataset$date >= as.Date('2014-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2015-01-01') & dataset$date >= as.Date('2014-01-01')] * 1.11
dataset$goldusdpertroyounce[dataset$date < as.Date('2014-01-01') & dataset$date >= as.Date('2013-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2014-01-01') & dataset$date >= as.Date('2013-01-01')] * 1.13
dataset$goldusdpertroyounce[dataset$date < as.Date('2013-01-01') & dataset$date >= as.Date('2012-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2013-01-01') & dataset$date >= as.Date('2012-01-01')] * 1.15
dataset$goldusdpertroyounce[dataset$date < as.Date('2012-01-01') & dataset$date >= as.Date('2011-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2012-01-01') & dataset$date >= as.Date('2011-01-01')] * 1.18
dataset$goldusdpertroyounce[dataset$date < as.Date('2011-01-01') & dataset$date >= as.Date('2010-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2011-01-01') & dataset$date >= as.Date('2010-01-01')] * 1.20
dataset$goldusdpertroyounce[dataset$date < as.Date('2010-01-01') & dataset$date >= as.Date('2009-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2010-01-01') & dataset$date >= as.Date('2009-01-01')] * 1.23
dataset$goldusdpertroyounce[dataset$date < as.Date('2009-01-01') & dataset$date >= as.Date('2008-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2009-01-01') & dataset$date >= as.Date('2008-01-01')] * 1.23
dataset$goldusdpertroyounce[dataset$date < as.Date('2008-01-01')] = dataset$goldusdpertroyounce[dataset$date < as.Date('2008-01-01')] * 1.29


dataset$crudeoilusdperbarrel[dataset$date < as.Date('2020-12-12')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2020-12-12')] * 1
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2020-01-01') & dataset$date >= as.Date('2019-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2020-01-01') & dataset$date >= as.Date('2019-01-01')] * 1.03
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2019-01-01') & dataset$date >= as.Date('2018-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2019-01-01') & dataset$date >= as.Date('2018-01-01')] * 1.05
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2018-01-01') & dataset$date >= as.Date('2017-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2018-01-01') & dataset$date >= as.Date('2017-01-01')] * 1.07
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2017-01-01') & dataset$date >= as.Date('2016-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2017-01-01') & dataset$date >= as.Date('2016-01-01')] * 1.10
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2016-01-01') & dataset$date >= as.Date('2015-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2016-01-01') & dataset$date >= as.Date('2015-01-01')] * 1.11
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2015-01-01') & dataset$date >= as.Date('2014-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2015-01-01') & dataset$date >= as.Date('2014-01-01')] * 1.11
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2014-01-01') & dataset$date >= as.Date('2013-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2014-01-01') & dataset$date >= as.Date('2013-01-01')] * 1.13
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2013-01-01') & dataset$date >= as.Date('2012-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2013-01-01') & dataset$date >= as.Date('2012-01-01')] * 1.15
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2012-01-01') & dataset$date >= as.Date('2011-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2012-01-01') & dataset$date >= as.Date('2011-01-01')] * 1.18
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2011-01-01') & dataset$date >= as.Date('2010-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2011-01-01') & dataset$date >= as.Date('2010-01-01')] * 1.20
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2010-01-01') & dataset$date >= as.Date('2009-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2010-01-01') & dataset$date >= as.Date('2009-01-01')] * 1.23
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2009-01-01') & dataset$date >= as.Date('2008-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2009-01-01') & dataset$date >= as.Date('2008-01-01')] * 1.23
dataset$crudeoilusdperbarrel[dataset$date < as.Date('2008-01-01')] = dataset$crudeoilusdperbarrel[dataset$date < as.Date('2008-01-01')] * 1.29

# Writing final dataset
fwrite(dataset, paste0(path, '/dataset.csv'))


