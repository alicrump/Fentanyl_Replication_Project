library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(lfe)
library(stargazer)
library(geofacet)


# read nflis data
dat = fread("FOIA_ItemLevelData_2001_2005.txt")
#dat2 = fread("FOIA_ItemLevelData_20062010.txt")
#colnames(dat2) = colnames(dat)
dat3 = fread("FOIA_ItemLevelData_20112016.txt")
colnames(dat3) = colnames(dat)

#datmerge = rbindlist(list(dat, dat2, dat3), use.names=FALSE, idcol=FALSE)
rm(dat)

###
d = unique(c(
  unique(dat3$Subst1)[grep("fentanyl|Fentanyl|FENTANYL",
                            unique(dat3$Subst1))],
    unique(dat3$Subst2)[grep("fentanyl|Fentanyl|FENTANYL",
                             unique(dat3$Subst2))], "U-47700"
    ))
h_string = c("Heroin", "heroin", "HEROIN")
d_h = c(d, "Heroin", "heroin", "HEROIN")


dat3$fent = as.numeric(rowSums(`dim<-`(as.matrix(dat3) %in% d, dim(dat3))) >= 1)
dat3$heroin = as.numeric(rowSums(`dim<-`(as.matrix(dat3) %in% c("Heroin"), dim(dat3))) >= 1)
dat3$Date = mdy(dat3$SubmitDate)
dat3$Year = year(dat3$Date)

fentstateyear = dat3 %>% group_by(State, Year) %>% summarize(any = n(), propfent = mean(fent),
                                                  sumfent = sum(fent),sumheroin = sum(heroin))

# other states
dat3o = fread("FOIA_StateLevelData_20112016.txt")
dat3o$fent = as.numeric(rowSums(`dim<-`(as.matrix(dat3o) %in% d, dim(dat3o))) >= 1)
dat3o$heroin = as.numeric(rowSums(`dim<-`(as.matrix(dat3o) %in% c("Heroin"), dim(dat3o))) >= 1)
dat3o$Date = mdy(dat3o$SubmitD1ate)
dat3o$Year = as.numeric(dat3o$CaseReceived_Year)

fentstateyear2 = dat3o %>% group_by(State, Year) %>% summarize(any = sum(CountOfReportedResult),
                                                propfent = mean(fent*CountOfReportedResult),
                                                sumfent = sum(fent*CountOfReportedResult),
                                                sumheroin = sum(heroin*CountOfReportedResult)
                                                )

presod = read.csv("rx_od_state_0616.csv")
presod$ST = state.abb[match(presod$State,state.name)]
presod[is.na(presod$ST), "ST"] = "DC"
presod$Age.Adjusted.Rate = NULL

############### PLOTS


############ FIGURE 1

# fentyear
fentyear = fentstateyear %>% group_by(Year) %>% summarize(sumfent = sum(sumfent))

### incorporate fentanyl from 2017
d17 = read.csv("fent2017nflis.csv")
d17$ST = state.abb[match(d17$State,state.name)]
d17$Year = 2017
d17[is.na(d17$ST), "ST"] = "DC"
#

fentyear = rbind(fentyear, c(2017, sum(d17$allfent)))
ggplot(fentyear, aes(factor(Year), sumfent/1000, fill="Black")) + geom_col(fill="Black") + xlab("") +
  ylab("Fentanyl Test Reports (Thousands) \n") + geom_text(aes(label=sumfent),hjust=0.5, vjust=-0.5, size=6) +
  theme_classic() +  theme(legend.position="", axis.text=element_text(size=14), axis.title.y=element_text(size=14))
ggsave("Figures/Appendix/FIGUREA1_fentseizures_years.png.png", width=12, height=8, units="in", dpi=300)


### incorporate heroin from 2017
h17 = read.csv("heroin2017nflis.csv")
h17$ST = state.abb[match(h17$State,state.name)]
h17$Year = 2017
h17[is.na(h17$ST), "ST"] = "DC"

############# END FIGURE 1

## geo facet - fent count year

fentstateyear_m = rbind(fentstateyear, fentstateyear2)
fentstateyear_m = fentstateyear_m %>% group_by(State, Year) %>% summarize(sumfent = sum(sumfent),
                                                            any = sum(any), sumheroin=sum(sumheroin))
fentstateyear_m$propfent = fentstateyear_m$sumfent/fentstateyear_m$any

###
# get 2017
d17b = d17[,c("ST", "Year", "allfent")]
colnames(d17b)[3] = "sumfent"
d17b$any = NA
d17b = merge(d17b, h17[,2:4])
colnames(d17b)[1] = "State"
colnames(d17b)[5] = "sumheroin"
d17b$propfent = NA


fentstateyear_m = rbind(data.frame(fentstateyear_m), d17b)
##

####################### clean up; end drug seizures processing
rm(dat3, dat3o)
presod_f = fentstateyear_m[fentstateyear_m$State != "PR",]
presod_f$logfent = log(presod_f$sumfent+1)

##

############ merge  od with seizures
################## GET STATE YEAR POP
pop = read.csv("drugpoisonings_multiplecausesofdeath_stateyear_19992017pop.csv")
pop$ST = state.abb[match(pop$State,state.name)]
pop[is.na(pop$ST), "ST"] = "DC"

presod_f = merge(presod_f, pop, by.x=c("State", "Year"), by.y = c("ST", "Year"))
rm(pop)
colnames(presod_f)[1]="ST"
colnames(presod_f)[8] = "State"

########## GET LAT LON
ll = read.csv("latlon.csv")
presod_f = merge(presod_f, ll)

#########

###########
presod_f$fentcapita = presod_f$sumfent/presod_f$Population*100000
# take log of seizures per capita
presod_f$fent_r = log(presod_f$fentcapita+1)
presod_f$perfent = presod_f$propfent*100
#######

#### TGROUP
#presod_f$tgroup = as.numeric(presod_f$Longitude >= -90)
presod_f$tgroup = as.numeric(presod_f$Longitude>-89.978027)
presod_f$tg2 = "West of MS River"
presod_f[presod_f$tgroup==1, "tg2"] = "East of MS River"
####


#### create lagged dvs for mortality rate in 2013 and mortality rate in 2011 
lagdv = presod_f[presod_f$Year==2013, c("State", "Age.Adjusted.Rate")]
colnames(lagdv) = c("State", "MORT_2013")
presod_f = merge(presod_f, lagdv)

lagdv = presod_f[presod_f$Year==2011, c("State", "Age.Adjusted.Rate")]
colnames(lagdv) = c("State", "MORT_2011")
presod_f = merge(presod_f, lagdv, all.x=TRUE, all.y=FALSE)

# create first difference style variables
presod_f$MORT_DIFF_11 = presod_f$Age.Adjusted.Rate - presod_f$MORT_2011
# robustness/placebo
presod_f$MORT_DIFF_13 = presod_f$Age.Adjusted.Rate - presod_f$MORT_2013

# prop heroin & heroin per capita
presod_f$propheroin = presod_f$sumheroin/presod_f$any
presod_f$heroincapita = presod_f$sumheroin/presod_f$Population*100000
presod_f$heroin_r = log(presod_f$heroincapita+1)
presod_f$perheroin = presod_f$propheroin*100

############################## FIGURE 2
ggplot(presod_f, aes(x=Year, 100*fentcapita, fill=tgroup)) + geom_col() + xlab("") +
  ylab("Fentanyl/Analogues per 100k \n") + facet_geo(~State, scales="fixed") +
  theme_bw() + theme(legend.position = "", plot.title = element_text(size=28), axis.text = element_text(size=7)) +
  labs(caption = "Source: National Forensic Laboratory Information System (NFLIS)") +
  ggtitle("\n Drug Seizures with Fentanyl (2011-2017) \n") 
ggsave("Figures/Figure2/figure2_fentcapita_17.png", width=12, height=8, units="in", dpi=300)

### plot seizures count
ggplot(fentstateyear_m, aes(x=Year, sumfent, fill=State)) + geom_col() + xlab("") +
  ylab("Tests Containing Fentanyl/Analogues \n") + facet_geo(~State, scales="fixed") +
  theme_bw() + theme(legend.position = "", plot.title = element_text(size=28), axis.text = element_text(size=7)) +
  labs(caption = "Source: National Forensic Laboratory Information System (NFLIS)") +
  ggtitle("\n Drug Seizures with Fentanyl (2011-2017) \n") 
ggsave("Figures/Figure2S/fentseizures.png", width=12, height=8, units="in", dpi=300)

### plot proportion of all drug seizures containing fentanyl 
ggplot(fentstateyear_m, aes(x=Year, propfent, fill=State)) + geom_col() + xlab("") +
  ylab("Proportion of Tests Containing Fentanyl/Analogues \n") + facet_geo(~State, scales="fixed") +
  theme_bw() + theme(legend.position = "", plot.title = element_text(size=28), axis.text = element_text(size=7)) +
  labs(caption = "Source: National Forensic Laboratory Information System (NFLIS)") +
  ggtitle("\n Drug Seizures with Fentanyl (2011-2016) \n") 
ggsave("Figures/Figure2S/fentseizures_proptotal.png", width=12, height=8, units="in", dpi=300)


############################## END FIGURE 2


############################## 

### IV AND MARGINALS
minst1 = felm(Age.Adjusted.Rate ~ 1 | ST+Year | (fent_r~Longitude:Year) | ST+Year, 
              data=presod_f, weights=presod_f$Population)
presod_f$fenteffectiv = presod_f$fent_r*summary(minst1)$coefficients[1]

## first stage F
summary(minst1$stage1)$iv1fstat["F"]
##

minst2 = felm(Age.Adjusted.Rate ~ fent_r | ST+Year | 0 | ST+Year, 
              data=presod_f, weights=presod_f$Population)
presod_f$fenteffect = presod_f$fent_r*summary(minst2)$coefficients[1]

#################################### TABLE 2
stargazer(minst2, minst1)

# save coefs
ols_coef = summary(minst2)$coefficients[1]
iv_coef = summary(minst1)$coefficients[1]

### estimate deaths
estdeath = function(dat, year, beta=ols_coef) {
  sum(beta*(dat[dat[,"Year"]==year, "fent_r"])*(dat[dat[,"Year"]==year,"Population"]/100000))
}

# marginal effects df
meff = data.frame(cbind(
  rbind(               mean(presod_f[presod_f$Year==2011,"fenteffect"]),
                       mean(presod_f[presod_f$Year==2012,"fenteffect"]),
                       mean(presod_f[presod_f$Year==2013,"fenteffect"]),
                       mean(presod_f[presod_f$Year==2014,"fenteffect"]),
                       mean(presod_f[presod_f$Year==2015,"fenteffect"]),
                       mean(presod_f[presod_f$Year==2016,"fenteffect"]),
                       mean(presod_f[presod_f$Year==2017,"fenteffect"])),
  rbind(mean(presod_f[presod_f$Year==2011,"fenteffectiv"]),
        mean(presod_f[presod_f$Year==2012,"fenteffectiv"]),
        mean(presod_f[presod_f$Year==2013,"fenteffectiv"]),
        mean(presod_f[presod_f$Year==2014,"fenteffectiv"]),
        mean(presod_f[presod_f$Ysear==2015,"fenteffectiv"]),
        mean(presod_f[presod_f$Year==2016,"fenteffectiv"]),
        mean(presod_f[presod_f$Year==2017,"fenteffectiv"])),
  rbind(estdeath(presod_f, 2011),
        estdeath(presod_f, 2012),
        estdeath(presod_f, 2013),
        estdeath(presod_f, 2014),
        estdeath(presod_f, 2015),
        estdeath(presod_f, 2016),
        estdeath(presod_f, 2017)),
  rbind(estdeath(presod_f, 2011, iv_coef),
        estdeath(presod_f, 2012, iv_coef),
        estdeath(presod_f, 2013, iv_coef),
        estdeath(presod_f, 2014, iv_coef),
        estdeath(presod_f, 2015, iv_coef),
        estdeath(presod_f, 2016, iv_coef),
        estdeath(presod_f, 2017, iv_coef))),
  row.names = c(2011:2017))
colnames(meff) = c("OLS", "2SLS", "Model 1 Deaths", "Model 2 Deaths")
xtable::xtable(meff)
xtable::xtable(round(meff[,3:4]), digits=0)


################################## TABLE 2 ROBUSTNESS CHECK (omitting Alaska and Hawaii
minst3 = felm(Age.Adjusted.Rate ~ 1 | ST+Year| (fent_r~Longitude:Year) | ST+Year, 
              data=presod_f[!(presod_f$State %in% c("Alaska", "Hawaii")),],
              weights=presod_f[!(presod_f$State %in% c("Alaska", "Hawaii")),]$Population)
minst4 = felm(Age.Adjusted.Rate ~ fent_r | ST+Year | 0 | ST+Year, 
              data=presod_f[!(presod_f$State %in% c("Alaska", "Hawaii")),],
              weights=presod_f[!(presod_f$State %in% c("Alaska", "Hawaii")),]$Population)

stargazer(minst4, minst3)


############################## TABLE 1 and correlates of fent seizures per capita

### LONGITUDE & FENT
for(y in sort(unique(presod_f$Year))) {
  m = lm(fent_r~Longitude, data=presod_f[presod_f$Year==y,])
  #print(summary(m)$coefficients[2,])
  print(paste(y, ": ", summary(m)$r.squared, sep=""))
  
  m2 = lm(fent_r~Longitude, data=presod_f[presod_f$Year==y & !(presod_f$ST %in% c("AK", "HI")),])
  print(paste(y, ": (NO Hawaii/Alaska) ", summary(m2)$r.squared, sep=""))
}

### LAT
for(y in sort(unique(presod_f$Year))) {
  m = lm(fent_r~Latitude, data=presod_f[presod_f$Year==y,])
  #print(summary(m)$coefficients[2,])
  print(paste(y, ": ", summary(m)$r.squared, sep=""))
  
  m2 = lm(fent_r~Latitude, data=presod_f[presod_f$Year==y & !(presod_f$ST %in% c("AK", "HI")),])
  print(paste(y, ": (NO Hawaii/Alaska) ", summary(m2)$r.squared, sep=""))
}

## Mortality 2013
for(y in sort(unique(presod_f$Year))) {
  m = lm(fent_r~MORT_2013, data=presod_f[presod_f$Year==y,])
  #print(summary(m)$coefficients[2,])
  print(paste(y, ": ", summary(m)$r.squared, sep=""))
  
  m2 = lm(fent_r~MORT_2013, data=presod_f[presod_f$Year==y & !(presod_f$ST %in% c("AK", "HI")),])
  print(paste(y, ": (NO Hawaii/Alaska) ", summary(m2)$r.squared, sep=""))
}

### state year bivariate longitude
ggplot(presod_f[presod_f$Year > 2011 & !(presod_f$ST %in% c("AK", "HI")),], aes(x=Longitude, y=fent_r))  + 
  geom_text(aes(label=ST),hjust=0.0, vjust=0, size=3) + ylab("Fentanyl Exposure") + geom_smooth() +
  facet_wrap(~Year) + theme_classic(base_size = 14)
ggsave("Figures/Appendix/FIGUREA2a_longitude_fent_year.png", width=12, height=8, units="in", dpi=300)

## state year bivariate longitude/heroin
ggplot(presod_f[presod_f$Year > 2011 & !(presod_f$ST %in% c("AK", "HI")),], aes(x=Longitude, y=heroin_r))  + 
  geom_text(aes(label=ST),hjust=0.0, vjust=0, size=3) + ylab("Heroin Exposure") + geom_smooth() +
  facet_wrap(~Year) + theme_classic(base_size = 14)
ggsave("Figures/Appendix/FIGUREA2b_longitude_heroin_year.png", width=12, height=8, units="in", dpi=300)

###

### state year bivariate MORT_2013
ggplot(presod_f[presod_f$Year > 2011 & !(presod_f$ST %in% c("AK", "HI")),], aes(x=MORT_2013, y=fent_r))  + 
  geom_smooth(method="lm") + geom_text(aes(label=ST),hjust=0.0, vjust=0, size=3) + ylab("Fentanyl Exposure") + 
  xlab("Overdose Mortality, 2013") + facet_wrap(~Year) + theme_classic(base_size = 14)
###

################################## TABLE 1

############ FENTANYL EXPOSURE 2017
stargazer(lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2013,]),
          lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2014,]),
          lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2015,]),
          lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2016,]),
          lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2017,]))

# with population weights (not in paper)
stargazer(
lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2013,],
   weights=presod_f[presod_f$Year==2013, "Population"]),
lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2014,],
   weights=presod_f[presod_f$Year==2014, "Population"]),
lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2015,],
   weights=presod_f[presod_f$Year==2015, "Population"]),
lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2016,],
   weights=presod_f[presod_f$Year==2016, "Population"]),
lm(fent_r~Longitude+Latitude+MORT_2013, data=presod_f[presod_f$Year==2017,],
   weights=presod_f[presod_f$Year==2017, "Population"])
)

################################## END TABLE 1


############################# BEGIN FIGURE 2

#### STATE OD YEAR
ggplot(presod_f, aes(x=Year, Age.Adjusted.Rate, fill=tgroup)) + geom_col() + xlab("") +
  ylab("Age-Adjusted Mortality \n") + facet_geo(~State, scales="fixed") +
  ggtitle("\n Trend in Overdose Mortality (2011-2016) \n") +
  theme_bw() + theme(legend.position = "", plot.title = element_text(size=28),  axis.text = element_text(size=7))+
  labs(caption = "Source: CDC WONDER") 
ggsave("Figures/Figure3/odyear.png", width=12, height=8, units="in", dpi=300)

# REGIONALITY OF Changing overdose
ggplot(presod_f, aes(x=Year, y=MORT_DIFF_11, fill=(tgroup))) + geom_col() +
  xlab("") + ylab("Change in OD Mortality (compared with 2011) \n") +
  facet_geo(~State, scales="fixed") +
  ggtitle("\n Regionality of Changing Overdose Mortality \n") +
  theme_bw()  + theme(legend.position = "", plot.title = element_text(size=28), axis.text = element_text(size=7))
ggsave("Figures/Figure3/regionality_deltaod_17.png", width=12, height=8, units="in", dpi=300)

############################# END FIGURE 2

############################# FIGURE 3

# annual plot
ggplot(presod_f[presod_f$Year>2011 ,],
       aes(x=fent_r, y=(Age.Adjusted.Rate), color=tgroup)) +
  geom_text(aes(label=ST),hjust=0.0, vjust=0, size=3)+
  geom_smooth(method="lm", se=FALSE, alpha=0.4, color="Black") + xlab("Log(Fentanyl Seizures Per 100k)") +
  ylab("Age-Adjusted Overdose Mortality") + facet_wrap(~Year, scales="fixed") +
  ggtitle("Fentanyl, Geography, & Overdose Mortality (2012-2017)") +
  theme_bw(base_size = 14) + theme(legend.position = "", plot.title = element_text(size=22),
                     axis.text = element_text(size=11)) #+ labs(caption = "Source: NFLIS/CDC")
ggsave("Figures/Figure4/fentseizures_od_l_capita_linear_17.png", width=12, height=8, units="in", dpi=300)

# east/west MS
ggplot(presod_f[presod_f$Year %in% c("2011", "2017") ,], aes(x=fent_r,
      y=(Age.Adjusted.Rate), group=ST, color=tgroup)) + geom_line(alpha=0.3)+
  geom_text(aes(label=paste(ST, substr(Year, 3, 4), sep="")),hjust=0.1, 
            vjust=-0.5, size=3) + facet_wrap(~tg2) +
  xlab("Log(Fentanyl Seizures Per 100k)") + ylab("Age-Adjusted Overdose Mortality")  +
  ggtitle("Fentanyl & Overdose Mortality (2011-2017)") +
  theme_bw(base_size=14) + theme(legend.position = "", plot.title = element_text(size=22),
                     axis.text = element_text(size=11)) #+ labs(caption = "Source: NFLIS/CDC") 
ggsave("Figures/Figure4/state_logseizod_facetms_17.png", width=12, height=8, units="in", dpi=300)


#############################  END FIGURE 3


############## FIGURE 4S

# some states
sl = c("CA", "NM", "TX", "AL", "ND", "NE", "WV", "OH", "MD", "MA", "NH", "NY", "PA", "FL", 
       "MT", "SD", "NV", "CO", "MS")

ggplot(presod_f[presod_f$Year %in% c("2011", "2017") & presod_f$ST %in% sl,], aes(x=fent_r,
  y=(Age.Adjusted.Rate), group=ST, color=tgroup)) + geom_line(alpha=0.3)+
  geom_text(aes(label=paste(ST, substr(Year, 3, 4), sep="")),hjust=0.1, 
            vjust=-0.5, size=3) +
  xlab("Log(Fentanyl Seizures Per 100k)") + ylab("Age-Adjusted Overdose Mortality \n")  +
  ggtitle("\n Fentanyl & Overdose Mortality (2011-2017) \n") +
  theme_bw() + theme(legend.position = "", plot.title = element_text(size=22),
                     axis.text = element_text(size=7))+ labs(caption = "Source: NFLIS/CDC") 

ggsave("Figures/Figure4S/statenamesfent_alt2_new.png", width=12, height=8, units="in", dpi=300)

# all states
ggplot(presod_f[presod_f$Year %in% c("2011", "2017"),], aes(x=fent_r,
                 y=(Age.Adjusted.Rate), group=ST, color=tgroup)) + geom_line(alpha=0.3)+
  geom_text(aes(label=paste(ST, substr(Year, 3, 4), sep="")),hjust=0.1, 
            vjust=-0.1, size=3) +
  xlab("Log(Fentanyl Seizures Per 100k)") + ylab("Age-Adjusted Overdose Mortality \n")  +
  ggtitle("\n Fentanyl & Overdose Mortality (2011-2017) \n") +
  theme_bw() + theme(legend.position = "", plot.title = element_text(size=22),
                     axis.text = element_text(size=7))+ labs(caption = "Source: NFLIS/CDC") 

ggsave("Figures/Figure4S/statenamesfent_alt2_new_allstates.png", width=12, height=8, units="in", dpi=300)


############## FIGURE 4
# simple linear regression of difference
# linear difference for 17
lb1 <- paste("R^2 == ", round(summary(lm(MORT_DIFF_11 ~ fent_r,
                                         data=presod_f[presod_f$Year==2017,]))$r.squared, 3), "", sep="")

ggplot(presod_f[presod_f$Year==2017,], aes(x=fent_r, y=(MORT_DIFF_11))) +
  geom_smooth(alpha=0.01, method="lm") + xlab("\n Fentanyl seizures per 100k (Natural Logarithm)") +
  ylab("Change in Mortality Rate (2017 - 2011) \n")  + geom_point(size=0.01) + geom_text(aes(label=ST,hjust=1, vjust=0, size=3)) +
  ggtitle("\n Fentanyl & Increased Overdose Mortality (2011 vs 2017) \n") +
  theme_bw() + theme(legend.position = "", plot.title = element_text(size=22),
                     axis.text = element_text(size=7))+
  labs(caption = "Source: NFLIS/CDC") + annotate("text",
                                                 label=lb1, x=0.48, y=22, size=5, parse=TRUE)
ggsave("Figures/Figure5/simple_linear_change_17.png", dpi=300)

# simple linear for level
lb1 <- paste("R^2 == ", round(summary(lm((Age.Adjusted.Rate) ~ fent_r,
                                         data=presod_f[presod_f$Year==2017,]))$r.squared, 3), "", sep="")
# summary(lm(MORT_DIFF_11 ~ log(fentcapita), data=presod_f[presod_f$Year==2016,]))
ggplot(presod_f[presod_f$Year==2017,], aes(x=fent_r, y=(Age.Adjusted.Rate))) +
  geom_smooth(alpha=0.01, method="lm") + xlab("\n Fentanyl seizures per 100k (Natural Logarithm)") +
  ylab("Overdose Mortality Rate, 2017 \n")  + geom_point(size=0.01) + geom_text(aes(label=ST,hjust=1, vjust=0, size=3)) +
  ggtitle("Fentanyl & Overdose Mortality (2017) \n") +
  theme_bw() + theme(legend.position = "", plot.title = element_text(size=22),
                     axis.text = element_text(size=7))+
  labs(caption = "Source: NFLIS/CDC") + annotate("text",
                                                 label=lb1, x=0.48, y=43.74, size=5, parse=TRUE)
ggsave("Figures/Figure5/simple_linear_assoc.png", dpi=300)

############


######################### APPENDIX TABLE A2

######## ROBUSTNESS TEST 1: Prop Fent
### IV AND MARGINALS
minst1_r = felm(Age.Adjusted.Rate ~ 1 | ST+Year | (perfent~Longitude:Year) | ST+Year, 
              data=presod_f, weights=presod_f$Population)

## first stage F
summary(minst1_r$stage1)$iv1fstat["F"]
##

minst2_r = felm(Age.Adjusted.Rate ~ perfent | ST+Year | 0 | ST+Year, 
              data=presod_f, weights=presod_f$Population)

#### prop fent (log)
minst1_r_2 = felm(Age.Adjusted.Rate ~ 1 | ST+Year | (log(100*propfent+1)~Longitude:Year) | ST+Year, 
                data=presod_f, weights=presod_f$Population)

## first stage F
summary(minst1_r_2$stage1)$iv1fstat["F"]
##

minst2_r_2 = felm(Age.Adjusted.Rate ~ log(100*propfent+1) | ST+Year | 0 | ST+Year, 
                data=presod_f, weights=presod_f$Population)


######## ROBUSTNESS TEST 2: Logged sum of fentanyl seizures
### IV AND MARGINALS
minst1_r2 = felm(Age.Adjusted.Rate ~ 1 | ST+Year | (logfent~Longitude:Year) | ST+Year, 
                data=presod_f, weights=presod_f$Population)

## first stage F
summary(minst1_r2$stage1)$iv1fstat["F"]
##

minst2_r2 = felm(Age.Adjusted.Rate ~ logfent | ST+Year | 0 | ST+Year, 
                data=presod_f, weights=presod_f$Population)


### all fentanyl related robustness tests
stargazer(minst1_r, minst2_r, minst1_r_2, minst2_r_2, minst1_r2, minst2_r2,
          out="Tables/TableA2_robustness.html")

#####


##### ROBUSTNESS TEST 3 heroin per capita
### IV AND MARGINALS
minst1_r3 = felm(Age.Adjusted.Rate ~ 1 | ST+Year | (heroin_r~Longitude:Year) | ST+Year, 
                 data=presod_f, weights=presod_f$Population)

## first stage F
summary(minst1_r3$stage1)$iv1fstat["F"]
##

minst2_r3 = felm(Age.Adjusted.Rate ~ heroin_r | ST+Year | 0 | ST+Year, 
                 data=presod_f, weights=presod_f$Population)


##### ROBUSTNESS TEST 4 heroin prop
### IV AND MARGINALS
minst1_r4 = felm(Age.Adjusted.Rate ~ 1 | ST+Year | (propheroin~Longitude:Year) | ST+Year, 
                 data=presod_f, weights=presod_f$Population)

## first stage F
summary(minst1_r4$stage1)$iv1fstat["F"]
##

minst2_r4 = felm(Age.Adjusted.Rate ~ propheroin | ST+Year | 0 | ST+Year, 
                 data=presod_f, weights=presod_f$Population)

####################### EXTRA PLOTS

# proportion fentanyl east/west ms
ggplot(presod_f[presod_f$Year %in% c("2011", "2016") ,], aes(x=propfent*100,
           y=(Age.Adjusted.Rate), group=ST, color=tgroup)) + geom_line(alpha=0.3)+
  geom_text(aes(label=paste(ST, substr(Year, 3, 4), sep="")),hjust=0.1, 
            vjust=-0.5, size=3) + facet_wrap(~tg2) +
  xlab("Proportion of Drug Seizures for Fentanyl") + ylab("Age-Adjusted Overdose Mortality")  +
  ggtitle("Fentanyl & Overdose Mortality (2011-2016)") +
  theme_bw(base_size=14) + theme(legend.position = "", plot.title = element_text(size=22),
                                 axis.text = element_text(size=11)) #+ labs(caption = "Source: NFLIS/CDC") 

# log
ggplot(presod_f[presod_f$Year %in% c("2011", "2016") ,], aes(x=log(propfent*100+1),
                                                             y=(Age.Adjusted.Rate), group=ST, color=tgroup)) + geom_line(alpha=0.3)+
  geom_text(aes(label=paste(ST, substr(Year, 3, 4), sep="")),hjust=0.1, 
            vjust=-0.5, size=3) + facet_wrap(~tg2) +
  xlab("Proportion of Drug Seizures for Fentanyl (log)") + ylab("Age-Adjusted Overdose Mortality")  +
  ggtitle("Fentanyl & Overdose Mortality (2011-2016)") +
  theme_bw(base_size=14) + theme(legend.position = "", plot.title = element_text(size=22),
                                 axis.text = element_text(size=11)) #+ labs(caption = "Source: NFLIS/CDC") 


# heroin plot east/west ms
ggplot(presod_f[presod_f$Year %in% c("2011", "2016") ,], aes(x=heroin_r,
    y=(Age.Adjusted.Rate), group=ST, color=tgroup)) + geom_line(alpha=0.3)+
  geom_text(aes(label=paste(ST, substr(Year, 3, 4), sep="")),hjust=0.1, 
            vjust=-0.5, size=3) + facet_wrap(~tg2) +
  xlab("Log(Heroin Seizures Per 100k)") + ylab("Age-Adjusted Overdose Mortality")  +
  ggtitle("Heroin Seizures & Overdose Mortality (2011-2016)") +
  theme_bw(base_size=14) + theme(legend.position = "", plot.title = element_text(size=22),
                                 axis.text = element_text(size=11)) #+ labs(caption = "Source: NFLIS/CDC") 

ggplot(presod_f[presod_f$Year %in% c("2011", "2016") ,], aes(x=propheroin,
                                  y=(Age.Adjusted.Rate), group=ST, color=tgroup)) + geom_line(alpha=0.3)+
  geom_text(aes(label=paste(ST, substr(Year, 3, 4), sep="")),hjust=0.1, 
            vjust=-0.5, size=3) + facet_wrap(~tg2) +
  xlab("Proportion of Drug Seizures for Heroin") + ylab("Age-Adjusted Overdose Mortality")  +
  ggtitle("Fentanyl & Overdose Mortality (2011-2016)") +
  theme_bw(base_size=14) + theme(legend.position = "", plot.title = element_text(size=22),
                                 axis.text = element_text(size=11)) #+ labs(caption = "Source: NFLIS/CDC") 


