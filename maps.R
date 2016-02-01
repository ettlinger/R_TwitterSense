library(twitteR)
library(syuzhet)
library(maps)
library(maptools)
library(sp)
library(ggplot2)

setwd("~/Desktop/Tech/DataInc/")
options(digits=10)
fstate <- function(pointsDF) {
    states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_sp <- map2SpatialPolygons(states, IDs=IDs,
                     proj4string=CRS("+proj=longlat +datum=wgs84"))
    pointsSP <- SpatialPoints(pointsDF, 
                    proj4string=CRS("+proj=longlat +datum=wgs84"))

    indices <- over(pointsSP, states_sp)
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[indices]
}

stateFromLower <-function(x) {
  st.codes<-data.frame(
                      state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                                         "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                                         "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                                         "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                                         "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
                      full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                                       "connecticut","district of columbia","delaware","florida","georgia",
                                       "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                                       "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                                       "missouri","mississippi","montana","north carolina","north dakota",
                                       "nebraska","new hampshire","new jersey","new mexico","nevada",
                                       "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                                       "rhode island","south carolina","south dakota","tennessee","texas",
                                       "utah","virginia","vermont","washington","wisconsin",
                                       "west virginia","wyoming"))
                       )
  st.x<-data.frame(state=x)
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  return(refac.x)
 
}

###############################
#GENERATE TWITTER SENTIMENT MAP
###############################
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "wtB19BoxB0RX5XjwsqsFIIi5m"
consumerSecret <- "bNKfDaoauA1ZgKn3RaoFGMt95LKmXG6cy2SscMizXfDKyUz3sd"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake()
registerTwitterOAuth(twitCred)

#Get American geocoded tweets
tweets<-searchTwitter('s',  n=2000, geocode='37.000,-120.00,3000mi')
df<-twListToDF(tweets)

# Do sentiment analysis on the text
df$sent<-get_nrc_sentiment(gsub("[^[:alnum:]///' ]", "", df$text))
df$good<-df$sent$positive+df$sent$anticipation+df$sent$trust+df$sent$joy+df$sent$surprise
df$bad<-df$sent$negative+df$sent$anger+df$sent$disgust+df$sent$sadness+df$sent$fear

#Clean up geocode data
df$longitude<-as.numeric(df$longitude)
df$latitude<-as.numeric(df$latitude)
df<-df[!is.na(df$latitude),]

# Get state for geocode and summarize
df$state<-fstate(subset(df, select=c('longitude', 'latitude')))
sentsum<-aggregate(cbind(good, bad)~state, df, sum)

# Attach to all_states <- map_data("state")
names(all_states)[names(all_states)=="region"] <- "state"
dfmap<-merge(sentsum,all_states, by='state', all.y=TRUE)

# Clean up data some more
dfmap[is.na(dfmap)] <- 0

# Use ratio -- it turns out, people tend to tweet happily and this avoid population bias (good-bad)
dfmap$diff<-dfmap$good/dfmap$bad
dfmap$diff[is.infinite(dfmap$diff)] <- max(dfmap$diff)
dfmap$diff<-log(dfmap$diff)

#Required to graph
dfmap <- dfmap[order(dfmap$order),]
aggregate(diff~state, dfmap, max)
# Plot it
p <- ggplot()
p <- p + geom_polygon(data=dfmap, aes(x=long, y=lat, group = group, fill=(dfmap$diff)),colour="black") + scale_fill_continuous(low = "thistle2", high = "darkblue", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "log ratio of good/bad",title = "Sentiment of Tweets", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

######################
# Generate disease map
#########################
dat<-read.csv("ProjectTycho_Level2_v1.1.0.csv")
dat$fromdate<-strptime(dat$from_date, "%Y-%m-%d")
dat$todate<-strptime(dat$to_date, "%Y-%m-%d")

# Get one years data
ndat<-subset(dat, dat$fromdate>strptime("2014-01-01", "%Y-%m-%d")

# And aggregate across diseases
tot<-aggregate(number~state, ndat, sum)

# Read in other data set and clean up and merge with first set
pop<-read.csv("NST-EST2015-alldata.csv")
pop<-subset(pop, STATE>0, select=c(5,12,24,30,42,48))
names(tot)[names(tot)=="state"] <- "ab"
tot$state<-stateFromLower(tot$ab)
names(pop)[names(pop)=="NAME"] <- "state"
df<-merge(pop, tot, by="state")
names(df)[names(df)=="number"] <- "DISDEATH2014"

# Calculate some ratios
df$disratio<-log(df$DISDEATH2014/df$POPESTIMATE2014)
df$bratio<-df$DISDEATH2014/df$POPESTIMATE2014
df$dratio<-df$DEATHS2014/df$POPESTIMATE2014
df$dtratio<-df$DISDEATH2014/df$DEATHS2014
df$dmigratio<-log(df$DOMESTICMIG2014/df$POPESTIMATE2014)
df$imigratio<-log(df$INTERNATIONALMIG2014/df$POPESTIMATE2014)
df$migratio<-df$INTERNATIONALMIG2014/df$DOMESTICMIG2014

# Attach to map state data
dmap<-merge(df,all_states, by='state', all.y=TRUE)
dmap <- dmap[order(dmap$order),]

# Plot it
p <- ggplot()
p <- p + geom_polygon(data=dmap, aes(x=long, y=lat, group = group, fill=(dmap$dtratio)),colour="black") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "log ratio of good/bad",title = "Sentiment of Tweets", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
