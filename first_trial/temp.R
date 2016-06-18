rm(list = ls())
source('~/R_modules/dbInterface/dbInterface.R')
clean_zip <- function(zip){
  #zip <- c('ABC12323', '12333-1', ' adfer-12312')
  zip[!(grepl('[0-9]{5}', zip)&!grepl('[A-Za-z]', zip))] <- NA
  zip <- str_extract(zip, '[0-9]{5}')
  zip
}

library(ggmap)
library(zipcode)
library(stringr)
data("zipcode")
head(zipcode)
table(zipcode$state)

query <- "SELECT distinct policyHolderZip, count(*) as count FROM ms1.billinginfo group by policyHolderZip;"
query <- 'SELECT distinct locationBillingZip, count(*) as count FROM ms1.casephysicianlocationphone group by locationBillingZip;'
billing_zip <- get_ryan_query('ms1', query)
hist(log10(billing_zip$count))
#billing_zip$policyHolderZip <- substr(billing_zip$policyHolderZip, 1, 5)
billing_zip$locationBillingZip <- clean_zip(billing_zip$locationBillingZip)
billing_zip <- merge(billing_zip, zipcode, by.x = 'locationBillingZip', by.y = 'zip')
head(billing_zip)
US <- get_map(location='united states', zoom = 4, maptype = "roadmap",
              source='google', color='color')
HI <-get_map(location='hawaii', zoom = 6, maptype = "roadmap",
             source='google', color='color')
AK <-get_map(location='alaska', zoom = 4, maptype = "roadmap",
             source='google', color='color')
PR <-get_map(location='Puerto Rico', zoom = 7, maptype = "roadmap",
             source='google', color='color')
GU <-get_map(location='guam', zoom = 6, maptype = "roadmap",
             source='google', color='color')

ggmap(US) + 
  geom_point(aes(x = longitude, y = latitude, color = log10(count)), size = 2,
             data=billing_zip, alpha=0.2, na.rm = T) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = 2)

ggmap(US) + 
  geom_point(aes(x = longitude, y = latitude, size = count), color = 'blue',
             data=billing_zip, alpha=0.2, na.rm = T)


query <- 'SELECT distinct zip, count(*) as count FROM ms1.location group by zip order by count;'
ms1_location <- get_ryan_query('ms1', query)
head(ms1_location)
ms1_location$zip <- clean_zip(ms1_location$zip)
ms1_location <- merge(ms1_location, zipcode, by.x = 'zip', by.y = 'zip')

length(table(ms1_location$state))
ggmap(map) + 
  geom_point(aes(x = longitude, y = latitude, color = log10(count)), size = 2,
             data = ms1_location, alpha = 0.2, na.rm = T) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = 2)

pdf('20160617_ms1_location_US.pdf')
ggmap(US) + 
  geom_point(aes(x = longitude, y = latitude, size = count), color = 'blue',
             data = ms1_location, alpha = 0.2, na.rm = T)
#dev.off()

#pdf('20160617_ms1_location_AK.pdf')
ggmap(AK) + 
  geom_point(aes(x = longitude, y = latitude, size = count), color = 'blue',
             data = ms1_location, alpha = 0.2, na.rm = T)

#pdf('20160617_ms1_location_HI.pdf')
ggmap(HI) + 
  geom_point(aes(x = longitude, y = latitude, size = count), color = 'blue',
             data = ms1_location, alpha = 0.2, na.rm = T)

ggmap(PR) + 
  geom_point(aes(x = longitude, y = latitude, size = count), color = 'blue',
             data = ms1_location, alpha = 0.2, na.rm = T)

ggmap(GU) + 
  geom_point(aes(x = longitude, y = latitude, size = count), color = 'blue',
             data = ms1_location, alpha = 0.2, na.rm = T)
dev.off()





qmap("baylor university", zoom = 14, source = "osm")
qmap("baylor university", zoom = 14, source = "google", maptype = 'satellite')
qmap("baylor university", zoom = 14, source = "stamen", maptype = 'toner')

qmap("arizona state university", zoom = 14, source = "stamen", maptype = 'watercolor')

qmap("united states of america", zoom = 4, source = "stamen", maptype = 'watercolor')


qmap("houston", zoom = 10, maptype = 58916, api_key = api_key,
     source = "cloudmade")
