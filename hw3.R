library(readr)
library(tidyverse)

###part1 ###########
covid_hist <-  read_csv("all-states-history.csv")

##part2####

state_info <- read.csv("state_info.csv")


#####part 3 ############
covid_all <- left_join(covid_hist,state_info, by=c("state"="Abbreviation")) #provided in instructions

'%not_in%' <- Negate('%in%')

not_states <- c("AS", 
                "GU",
                "MH",
                "MP",
                "PR",
                "VI")

covid_main <- covid_all[covid_all$state %not_in% not_states,]


######part4#########

covid <- covid_main %>%
  select(date, state, Name, Population, deathIncrease, hospitalizedCurrently, positiveIncrease, totalTestResultsIncrease) %>%
  rename("Date" = date, "StateCode" = state, "StateName" = Name, "Pop" = Population, "NewDeath" = deathIncrease, "HospNow" = hospitalizedCurrently, "NewCase" = positiveIncrease, "NewTest" = totalTestResultsIncrease)

###part5####
na_or_0 <- is.na(covid$NewCase) | covid$NewCase == 0
num_no_case <- sum(na_or_0) 

###part6######
num_NA <- sum(is.na(covid))


###part 7 ####
covid$Pop <- parse_number(covid$Pop)

###part8#####
covid$HospNow[is.na(covid$HospNow)] = 0

#covid %>%
 # filter(StateCode %in% c('NC','VA','SC','TN','GA')) %>%
 # ggplot(aes(x=Date,y=HospNow, col=StateCode)) + geom_line()


#part9#######

covid <- covid %>%
  mutate(HospPerKiloPop = 1000*HospNow/Pop)



##part 10####
#provided in instructions
covid_mon<- covid %>%
  filter(Date>=as.Date("2020-03-01") &
           Date<as.Date("2021-03-01")) %>%
  mutate(Year=format(Date,"%Y"), Month=format(Date,"%m")) %>% 
  group_by(Year, Month, StateCode) %>%
  summarise(Date=max(Date), Pop=first(Pop),
            MDeath=sum(NewDeath), MHosp=sum(HospNow),
            MCase=sum(NewCase), MTest=sum(NewTest)) %>%
  ungroup()

#end provided section

covid_mon$MHosp[is.na(covid_mon$MHosp)] = 0

covid_mon <- covid_mon %>%
  mutate(PosRate = MCase/MTest)

####part 11####

MDeathPerMilPop <- covid_mon$MDeath / covid_mon$Pop / 1000000

MHospPerKiloPop <- covid_mon$MHosp / covid_mon$Pop / 1000
MCasePerKiloPop <- covid_mon$MCase / covid_mon$Pop / 1000
MTestPerKiloPop <- covid_mon$MTest / covid_mon$Pop / 1000

covid_mon <- cbind(covid_mon, MDeathPerMilPop, MHospPerKiloPop, MCasePerKiloPop, MTestPerKiloPop)


##part12####

hospital_days <- covid_mon %>%
  select("Date", "StateCode", "MHosp") %>%
  pivot_wider(names_from = "StateCode", values_from = MHosp)


##part13####
hospital_days <- hospital_days %>%
  mutate(hospital_days, USTotal =  rowSums(hospital_days[,2:52]))
