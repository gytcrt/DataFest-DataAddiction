# read the test and train test set
setwd("~/Dropbox/Data and Codebook")

#install.packages("readr")
library(readr)

##Libraries required
#Data work 
#install.packages("data.table")
require(data.table) #Working with large files

#install.packages("xlsx")
#library(xlsx)       #Loading and saving .xlsx files 

# install.packages("plyr")
require(plyr)   #Always load in this order plyr, dpply, lubridate - dpplyr overrides some of methods in plyr. 

#install.packages("dplyr")
require(dplyr) #Use require as it will give an error message if the package doesn't exist

#installed.packages("lubridate")
require(lubridate) #used for working with data information. 

#install.packages("reshape2")
require(reshape2)  #used for melting 


#Formating and printing 
#install.packages("devtools")
library(devtools)
#devtools::install_github("adletaw/captioner")   #Nice library for numbering and captioning tables in conjunction with knitr and pandoc
#devtools::install_github('Rapporter/pander')
require(pander)     #for creating nice output tables.
require(captioner)

#Set up the figure and table numbering
fig_nums<-captioner()
tab_nums<-captioner(prefix = "Table")

#Using pryr abbreviate how to call fig_nums function 
#install.packages("pryr")
require(pryr)
citefig<-pryr::partial(fig_nums,display="cite")
citetab<-pryr::partial(tab_nums,display="cite")

#Turn off caption.prefix as allow captioner to handle this. 
panderOptions('table.caption.prefix', '')
panderOptions('big.mark', ",")
panderOptions('keep.trailing.zeros', TRUE)

# read the table into R
#Load the data 
adwords <- data.table::fread("approved_adwords_v3.csv",header=TRUE, stringsAsFactors = TRUE) 
purchase <-data.table::fread("approved_data_purchase-v5.csv",header=TRUE, stringsAsFactors = TRUE) 
ga <- data.table::fread("approved_ga_data_v2.csv",header=TRUE,  stringsAsFactors = TRUE)


### Initial explaration

### adwords

dim(adwords) # 1451987      28
ad_uni_campaign = unique(adwords$campaign) # 1132
ad_uni_campaign_id = unique(adwords$campaign_id)
ad_uni_ad_group = unique(adwords$ad_group) # 9713
ad_uni_ad_group_id = unique(adwords$adgroup_id) # 9713
ad_uni_keyword = unique(adwords$keyword) # 97222
ad_uni_keystate = unique(adwords$keyword.state) # 3
ad_uni_match = unique(adwords$match.type) # 3

### purchase # 1699911    45

# agregate by campaign
camp_total = adwords %>% group_by(ad_campaign = adwords$campaign) %>%
  summarize(total=n()) %>% arrange(desc(total))

# aggregate by campain and ad_group
camp_by_adgroup = adwords %>% group_by(ad_campaign = adwords$campaign, ad_group=adwords$ad_group) %>%
  summarize(total=n()) %>% arrange(desc(ad_campaign), desc(ad_group), desc(total))

# aggregate by campain and ad_group
camp_by_adgroup_kwstate = adwords %>% group_by(ad_campaign = adwords$campaign, ad_group=adwords$ad_group, camp_by_adgroup_kwstate = adwords$keyword.state) %>%
  summarize(total=n()) %>% mutate(freq = total/sum(total))

camp_by_adgroup_kwstate = adwords %>% group_by(ad_campaign = adwords$campaign, ad_group=adwords$ad_group, camp_by_adgroup_kwstate = adwords$match.type) %>%
  summarize(total=n()) %>% mutate(freq = total/sum(total))


percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

conv.rate2 = sub("%", "", adwords$conv.rate)
conv.rate2 = as.factor(conv.rate2)
conv.rate2 = as.numeric(levels(conv.rate2))[conv.rate2]
conv.rate2 = conv.rate2/100

camp_by_adgroup_kwstate = adwords %>% group_by(ad_campaign = adwords$campaign, ad_group=adwords$ad_group, camp_by_adgroup_kwstate = adwords$match.type, conversions = conv.rate2) %>%
  summarize(total=n(), sum = sum(conversions)) %>% mutate(freq = sum/total)


arrange(camp_by_adgroup_kwstate, desc(freq))

camp_by_adgroup_kwstate[camp_by_adgroup_kwstate$camp_by_adgroup_kwstate == "enabled",]

camp_by_adgroup

sum(camp_by_adgroup[,1] == "Columbus OH")
### ga  4225456 rows and 46

####
# compare the adwords and the ga
# adgroup_id and campaign_id
 
ga_campaign_id = unique(ga$campaign_id)
ga_ad_group_id = unique(ga$adgroup_id) # 9713

# install.packages("VennDiagram")
library(VennDiagram)

# GA campaign ID and AD campaign ID: 282 overlap, AD has 312 unique and GA has 12 unique
grid.newpage()
venn.plot1<-draw.pairwise.venn(
  length(ga_campaign_id),length(ad_uni_campaign_id),length(intersect(ga_campaign_id,ad_uni_campaign_id)),
  category = c("ga campaign id", "ad campaign id"),
  lty = rep("blank",2),
  fill = c("light blue", "pink"),
  alpha = rep(0.5, 2),
  cat.pos = c(0,0),
  cat.dist = rep(0.025, 2))
grid.draw(venn.plot1)


### Ad group ID, has 545 overlapping, 1820 unique ga group ID and 345 ad unique group id
grid.newpage()
venn.plot2<-draw.pairwise.venn(
  length(ga_ad_group_id),length(ad_uni_ad_group_id),length(intersect(ad_uni_ad_group_id,ga_ad_group_id)),
  category = c("ad group id", "ga group id"),
  lty = rep("blank",2),
  fill = c("light blue", "pink"),
  alpha = rep(0.5, 2),
  cat.pos = c(0,0),
  cat.dist = rep(0.025, 2))
grid.draw(venn.plot2)



### postcode heat plot

postcode = purchase$venue_postal_cd_sgmt_1
postcode = as.numeric(levels(postcode))[postcode]

library(ggplot2)

### purcahse and ga joint by event_id

############################ Purchase Data Processing ############################

ga2 = select(ga ,event_id, device_browser, device_devicecategory, device_language,
                            device_mobiledevicebranding, device_operatingsystemversion, geonetwork_metro,
                            geonetwork_region, hits_hitnumber, hits_hour, hits_isinteraction, source, totals_bounces,
                            totals_hits, totals_pageviews, totals_timeonsite, totals_visits)

ga2 %>% group_by(event_id)


##### event_id overlap

pur_event_id =  unique(purchase$event_id)
ga_event_id = unique(ga$event_id)

length(purchase$primary_act_id)  # 1699911
length(unique(purchase$primary_act_id))  # 3206

length(unique(ga$visitid))  #1325337

grid.newpage()
venn.plot3<-draw.pairwise.venn(
  length(pur_event_id),length(ga_event_id),length(intersect(pur_event_id,ga_event_id)),
  category = c("pur_event_id", "ga_event_id"),
  lty = rep("blank",2),
  fill = c("light blue", "pink"),
  alpha = rep(0.5, 2),
  cat.pos = c(0,0),
  cat.dist = rep(0.025, 2))
grid.draw(venn.plot3)

overlap_event_id = intersect(pur_event_id,ga_event_id)

purchase2 = filter(purchase, event_id %in% overlap_event_id) # 284662     45
purchase3 = select(purchase2, event_id:fin_mkt_nm, dist_to_ven)  #  284662     30


## 
ga2 = filter(ga, event_id %in% overlap_event_id) # 284662     45

ga3 = select(ga2, event_id, visitnumber, date, totals_visits, totals_hits, totals_pageviews, totals_timeonsite,
       source, device_browser, device_devicecategory, device_operatingsystem, device_mobiledevicebranding, 
       device_language, geonetwork_continent, geonetwork_region, hits_hour, hits_minute)

ga4 = ga3 %>% 
  mutate(month = substr(ga3$date, 5, 6), year = substr(ga3$date, 1, 4)) 
# 2870867      19

setwd("~/Dropbox/Data and Codebook")
#save(ga4, file = "ga4.RData")

dim(ga4)   # 2870867      18

#dim(select(ga4, event_id, month))  # 2870867       2
#ga5 = group_by(ga4, event_id, month)

# month frequency
ga5 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total = .N) %>%
  group_by (event_id, month, year, total) %>%
  summarise (n=n()) %>%
  mutate (rel.freq = n / total)
#5921    6

# visitnumber frequency by year and month
ga6 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_visitnumber = sum(visitnumber)) %>%
  group_by (event_id, month, year, total_visitnumber) %>%
  summarise (n_visitnumber=sum(visitnumber)) %>%
  mutate (rel.freq_visitnumber = n_visitnumber / total_visitnumber)
# 5921    6

# totals_visits frequency by year and month
ga7 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_visits = sum(totals_visits)) %>%
  group_by (event_id, month, year, total_visits) %>%
  summarise (n_visits=sum(totals_visits)) %>%
  mutate (rel.freq_visits = n_visits / total_visits)
# 5921    6

# totals_hits frequency by year and month
ga8 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_totals_hits = sum(totals_hits)) %>%
  group_by (event_id, month, year, total_totals_hits) %>%
  summarise (n_totals_hits=sum(totals_hits)) %>%
  mutate (rel.freq_totals_hits = n_totals_hits / total_totals_hits)
# 5921    6

# total_pageviews frequency by year and month
ga9 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_pageviews = sum(totals_pageviews)) %>%
  group_by (event_id, month, year, total_pageviews) %>%
  summarise (n_pageviews = sum(totals_pageviews)) %>%
  mutate (rel.freq_totals_pageviews = n_pageviews / total_pageviews)
# 5921    6



# totals_timeonsite frequency by year and month
ga10 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_timeonsite = sum(totals_timeonsite)) %>%
  group_by (event_id, month, year, total_timeonsite) %>%
  summarise (n_timeonsite = sum(totals_timeonsite)) %>%
  mutate (rel.freq_timeonsite = n_timeonsite / total_timeonsite)
# 5921    6

# source frequency by year and month
ga11 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_source = .N) %>%
  group_by (event_id, month, year, total_source, source) %>%
  summarise (n_source = .N) %>%
  mutate (rel.freq_source = n_source / total_source)
# 5921    6

# device_browser frequency by year and month
ga12 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_device_browser= .N) %>%
  group_by (event_id, month, year, total_device_browser, device_browser) %>%
  summarise (n_device_browser = .N) %>%
  mutate (rel.freq_device_browser = n_device_browser / total_device_browser)

# device_devicecategory frequency by year and month
ga13 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_device_devicecategory = .N) %>%
  group_by (event_id, month, year, total_device_devicecategory, device_devicecategory) %>%
  summarise (n_device_devicecategory = .N) %>%
  mutate (rel.freq_device_devicecategory = n_device_devicecategory / total_device_devicecategory)

# device_operatingsystem frequency by year and month
ga14 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_device_operatingsystem = .N) %>%
  group_by (event_id, month, year, total_device_operatingsystem, device_operatingsystem) %>%
  summarise (n_device_operatingsystem = .N) %>%
  mutate (rel.freq_device_operatingsystem = n_device_operatingsystem / total_device_operatingsystem)

# device_operatingsystem frequency by year and month
ga15 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_device_mobiledevicebranding = .N) %>%
  group_by (event_id, month, year, total_device_mobiledevicebranding, device_mobiledevicebranding) %>%
  summarise (n_device_mobiledevicebranding = .N) %>%
  mutate (rel.freq_device_mobiledevicebranding = n_device_mobiledevicebranding / total_device_mobiledevicebranding)


# device_language and geonetwork_continent difference frequency by year and month
ga16 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_device_language = .N) %>%
  group_by (event_id, month, year, total_device_language, device_language) %>%
  summarise (n_device_language = .N) %>%
  mutate (rel.freq_device_device_language = n_device_language / total_device_language)


# geonetwork_continent
ga17 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_geonetwork_continent = .N) %>%
  group_by (event_id, month, year, total_geonetwork_continent, geonetwork_continent) %>%
  summarise (n_geonetwork_continent = .N) %>%
  mutate (rel.freq_geonetwork_continent = n_geonetwork_continent / total_geonetwork_continent)

# geonetwork_region
ga18 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_geonetwork_region = .N) %>%
  group_by (event_id, month, year, total_geonetwork_region, geonetwork_region) %>%
  summarise (n_geonetwork_region = .N) %>%
  mutate (rel.freq_geonetwork_region = n_geonetwork_region / total_geonetwork_region)

# hits_hour
ga19 = ga4 %>%
  group_by (event_id, year) %>%
  mutate (total_hits_hour = .N) %>%
  group_by (event_id, month, year, total_hits_hour, hits_hour) %>%
  summarise (n_hits_hour = .N) %>%
  mutate (rel.freq_hits_hour = n_hits_hour / total_hits_hour)
dim(ga19)


ga_1 = merge(ga5, ga6, by=c("event_id", "year"))
ga_2 = merge(ga_1, ga7, by=c("event_id", "year"))
ga_3 = merge(ga_2, ga8, by=c("event_id", "year"))
ga_3 = select(ga_3, -month.x, -month.y, -month.x, -month.y)
ga_4 = merge(ga_3, ga9, by=c("event_id", "year")) # 5921   20
ga_5 = select(ga_4, -month.x, -month.y)# 5921   20
ga12_3 <- dcast(ga12, event_id + year + month ~ device_browser, value.var = "rel.freq_device_browser")
ga_6 = merge(ga_5, ga12_3, by=c("event_id", "year", "month"))  # 5921   20
ga13_3 <- dcast(ga13, event_id + year + month ~ device_devicecategory, value.var = "rel.freq_device_devicecategory")
ga_7 = merge(ga_6, ga13_3, by=c("event_id", "year", "month"))  # 5921   63
ga14_3 <- dcast(ga14, event_id + year + month ~ device_operatingsystem, value.var = "rel.freq_device_operatingsystem")
ga_8 = merge(ga_7, ga14_3, by=c("event_id", "year", "month"))  # 5921   63
ga15_3 <- dcast(ga15, event_id + year + month ~ device_mobiledevicebranding, value.var = "rel.freq_device_mobiledevicebranding")
ga_9 = merge(ga_8, ga15_3, by=c("event_id", "year", "month"))  # 5921   63
ga16_3 <- dcast(ga16, event_id + year + month ~ device_language, value.var = "rel.freq_device_device_language")
ga_10 = merge(ga_9, ga16_3, by=c("event_id", "year", "month"))  # 5921   63
ga17_3 <- dcast(ga17, event_id + year + month ~ geonetwork_continent, value.var = "rel.freq_geonetwork_continent")
names(ga17_3)[4] = "other"
names(ga_10)[19] = "other19"
names(ga_10)[64] = "other64"
names(ga_10)[279] = "other279"
ga_11 = merge(ga_10, ga17_3, by=c("event_id", "year", "month"))  # 5921   63
ga18_3 <- dcast(ga18, event_id + year + month ~ geonetwork_region, value.var = "rel.freq_geonetwork_region")
ga_12 = merge(ga_11, ga18_3, by=c("event_id", "year", "month"))  # 5921   63
ga19_3 <- dcast(ga19, event_id + year + month ~ hits_hour, value.var = "rel.freq_hits_hour")
ga_13 = merge(ga_12, ga19_3, by=c("event_id", "year", "month"))  # 5921   63

setwd("~/Dropbox/Data and Codebook")
save(ga_13, file = "ga_13.RData")


pur = purchase %>%
  group_by (event_id) %>%
  mutate(total_del = .N) %>%
  group_by(event_id, total_del, delivery_type_cd) %>%
  summarise(total_del_type = .N) %>%
  mutate(rel = total_del_type/total_del)

install.packages("reshape")
library(reshape)

pur2 = as.data.frame(pur)
names(pur2)[5] = "value"
pur2 = melt(pur2, id.var = c("event_id", "delivery_type_cd"))
pur3 = dcast(pur2, event_id + delivery_type_cd ~ variable)
head(pur3)