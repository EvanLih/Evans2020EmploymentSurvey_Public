
library(readxl)
library(magrittr)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(scales)

#reading dataframe 

# colnames <- read_xlsx("Desktop/Github/Evans2020EmploymentSurvey/Full_Data_Employment.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 0, n_max = 1)
# 
# colnames %<>%
#   subset(select = -c(1:9))
# 
# df <- as.data.frame(matrix(ncol = length(colnames), nrow=1, dimnames = list(NULL,colnames)))
# 
# 
# test<-read_xlsx("Desktop/Github/Evans2020EmploymentSurvey/Full_Data_Employment.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 0)
# 
# test <- test[ -c(1), ]
# 
# 
# test %<>% as.data.frame()
# 
# 
# test %<>% 
#   set_names(c(paste("Q", seq_along(test), sep = ""))) %>% 
#   subset(select = -c(Q1:Q9)) %>%
#   rename(last = Q10,
#          first = Q11,
#          email = Q12,
#          international = Q13,
#          whyEvans = Q19,
#          whyEvanstext = Q20,
#          programOption = Q21)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

employData <- read_xlsx("Full_Data_Employment.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

secondSurvey <- read.csv("SP19_EmploymentSurvey_EmailList_Updated.csv")

name2 <- secondSurvey %>%
  select(c(Last.Name, First.Name)) %>%
  rename(lastName = Last.Name, 
         firstName = First.Name)

name2$lastName %<>% as.character()
name2$firstName %<>% as.character()


# completeData <- full_join(employData, name2, by = c("lastName", "firstName"))
# write.csv(completeData,"completeData.csv", row.names = FALSE)

employData <- read.csv("Shannon_complete_Data.csv")

##removing all who are not seeking employment. This means we are removing students who are continuing education, volunteer service, military service, and other and not seeking employment

###Program Option Choices###
dataTables <- createWorkbook()

addWorksheet(dataTables, "Transcript Option")

transcriptOption <- employData %>%
  group_by(programOption) %>%
  filter(!is.na(programOption)) %>%
  summarise(n=n()) %>%
  ungroup %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

writeData(dataTables, sheet = "Transcript Option", x = transcriptOption)




seekingEmp <-
  employData %>%
  filter(currentStatus != "Military service" &
           currentStatus != "Volunteer service (Peace Corps, AmeriCorps, etc.)" &
           currentStatus != "Not currently seeking employment" &
           currentStatus != "Other (please specify)")
  
addWorksheet(dataTables, "Employment Status")
seekingEmptable <- seekingEmp %>%
  group_by(currentStatus) %>%
  summarise(n = n()) %>%
  ungroup %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq))

writeData(dataTables, sheet = "Employment Status", x = seekingEmptable)


##current Status
seekingEmp %>%
  group_by(currentStatus) %>%          # now required with changes to dplyr::count()
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(total = sum(n), rel.freq = n / total)

#salary negotiation usage
#US Location - State

seekingEmp %>%
  group_by(salaryNeg) %>%
  filter(!is.na(salaryNeg)) %>%
  summarise(n=n()) %>%
  ungroup %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

#Country Employment
empCountry <- seekingEmp %>%
  group_by(employCountry) %>%
  filter(!is.na(employCountry)) %>%
  summarise(n=n()) %>%
  ungroup %>%
  mutate(rel.freq = n/sum(n)) %>%
  arrange(desc(rel.freq))

addWorksheet(dataTables, "Employment by Country")
writeData(dataTables, sheet = "Employment by Country", x = empCountry)


#US Location - State
empState <- seekingEmp %>%
  group_by(employState) %>%
  filter(!is.na(employState)) %>%
  summarise(n=n()) %>%
  ungroup %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

write.csv(empState, "Employedstate.csv")

#US Location - City
empCity <- seekingEmp %>%
  group_by(employCity) %>%
  filter(!is.na(employCity)) %>%
  summarise(n=n()) %>%
  ungroup %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

#NP/NGO Sector Breakdown
empSector <- employData %>%
  group_by(employSector) %>%
  filter(!is.na(employSector)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(dataTables, "Employment by Sector")
writeData(dataTables, sheet = "Employment by Sector", x = empSector)

##Top Industry Areas
indArea <- employData %>%
  group_by(employInd) %>%
  filter(!is.na(employInd)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

#Evans Recommend or not?


reccEvans <- employData %>%
  group_by(reccEvans) %>%
  filter(!is.na(reccEvans)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(rel.feq = n/sum(n)) %>%
  arrange(desc(rel.feq))

addWorksheet(dataTables, "Recommend Evans")
writeData(dataTables, sheet = "Recommend Evans", x = reccEvans)





#Why pursue an MPA?
reasonPursue <- employData %>%
  group_by(primaryEnroll) %>%
  filter(!is.na(primaryEnroll)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(rel.feq = n/sum(n)) %>%
  arrange(desc(rel.feq))
addWorksheet(dataTables, "Reason for Pursuing MPA")
writeData(dataTables, sheet = "Reason for Pursuing MPA", x = reasonPursue)

##Evans Engagement###

cdEngageTable <- employData %>%
  group_by(evansResourceUSe) %>%
  filter(!is.na(evansResourceUSe)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(rel.feq = n/sum(n)) %>%
  arrange(desc(rel.feq))

addWorksheet(dataTables, "Reason for Pursuing MPA")
writeData(dataTables, sheet = "Reason for Pursuing MPA", x = reasonPursue)


##Job Title 

##Before and after Salary
salaryChange <- employData %>%
  group_by(employSector) %>%
  filter(!is.na(employSector)) %>%
  filter(!is.na(currentSalary)) %>%
  summarise(Salary = mean(currentSalary),
            PrevSalary = mean(salaryPrior, na.rm = TRUE)) %>%
  mutate(salaryChange  = Salary - PrevSalary,
         salaryChangePerc = salaryChange/Salary) %>%
  arrange(desc(Salary))

addWorksheet(dataTables, "Salary Change by Sector")
writeData(dataTables, sheet = "Salary Change by Sector", x = salaryChange)

employData %>%
  filter(!is.na(employSector)) %>%
  filter(!is.na(currentSalary)) %>%
  summarise(Salary = mean(currentSalary),
            PrevSalary = mean(salaryPrior, na.rm = TRUE)) %>%
  mutate(salaryChange  = Salary - PrevSalary,
         salaryChangePerc = salaryChange/Salary) %>%
  arrange(desc(Salary))


# meansalaryDiff <- mean(seekingEmp$currentSalary, na.rm = TRUE) - mean(seekingEmp$salaryPrior, na.rm = TRUE)
# 
# mediansalaryDiff <- median(seekingEmp$currentSalary, na.rm = TRUE) - median(seekingEmp$salaryPrior, na.rm = TRUE)

#Evans engagement-  will they continue to work with Evans? (of those who responded to this question)

test <- employData %>%
  select(evansEngage) %>%
  is.na %>%
  `!` %>%
  rowSums() %>%
  as.data.frame()

totalEngage <- sum(test$.)

engage <- nrow(subset(employData, evansEngage != "This survey is enough for now, thanks!"))/totalEngage


engageCD <- employData %>%
  select(evansResourceUSe) %>%
  is.na %>%
  `!` %>%
  rowSums() %>%
  as.data.frame()

engageCD <- sum(engageCD$.)
totalengageCD <- 147/199




#all students who are currently employed (exlcuding continuing education)
currEmp.Student <- subset(employData, currentStatus == "Employed, full-time" | currentStatus == "Employed, part-time")


#All who are currently employed AOR seeking education
allEmp <- subset(employData, currentStatus == "Employed, full-time" | currentStatus == "Continuing education")

#All who are currently employed full time
currEmp <- subset(employData, currentStatus == "Employed, full-time")

#Only international Students
intData <- subset(employData, international == "Y") 

intEmploy <-intData %>%
  group_by(currentStatus) %>%
  filter(!is.na(currentStatus)) %>%
  summarise(n = n()) %>%
  ungroup %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq))

write.csv(intEmploy, "intEmploy.csv")

addWorksheet(dataTables, "International Employment")
writeData(dataTables, sheet = "International Employment", x = intEmploy)

#Only USA Students
usaOnly <- subset(employData, international == "N") 

usaOnly <-
  usaOnly %>%
  filter(
           currentStatus != "Military service" &
           currentStatus != "Volunteer service (Peace Corps, AmeriCorps, etc.)" &
           currentStatus != "Not currently seeking employment" &
           currentStatus != "Other (please specify)")

usaEmp <- usaOnly %>%
  group_by(currentStatus) %>%
  filter(!is.na(currentStatus)) %>%
  summarise(n = n()) %>%
  ungroup %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq))

write.csv(usaEmp, "usaEmp.csv")


addWorksheet(dataTables, "USA Employment Status(Seeking)")
writeData(dataTables, sheet = "USA Employment Status(Seeking)", x = usaEmp)


#Only USA Students who are employed
usaEmp <- subset(usaOnly, currentStatus == "Employed, full-time" | currentStatus == "Continuing education")

nrow(allEmp)/nrow(employData)


sectorData <- allEmp %>%
  group_by(employSector) %>%   
  filter(!is.na(employSector)) %>%
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(total = sum(n), rel.freq = n / total) %>%
  arrange(desc(rel.freq))

addWorksheet(dataTables, "Sector Breakdown (All Students)")
writeData(dataTables, sheet = "Sector Breakdown (All Students)", x = sectorData)

#overall salary

employData %>%
  group_by(employSector) %>%
  filter(!is.na(employSector)) %>%
  filter(!is.na(currentSalary)) %>%
  summarise(Salary = mean(currentSalary)) %>%
  arrange(desc(Salary))

salaryData <-
  employData %>%
  filter(
    currentStatus != "Military service" &
      currentStatus != "Volunteer service (Peace Corps, AmeriCorps, etc.)" &
      currentStatus != "Not currently seeking employment" &
      currentStatus != "Other (please specify)" &
      currentStatus != "Employed, part-time" &
      international != "Y")

library(extrafont)
font_import()
loadfonts(device = "win")
ggplot(subset(salaryData, !is.na(employSector) & employSector != "Other"), aes(x = employSector, y = currentSalary)) +
  geom_boxplot(aes(fill = employSector), outlier.shape=NA) +
  theme_minimal() +
  scale_y_continuous(labels = dollar, breaks = seq(0,120000, by = 20000)) +
  scale_fill_manual(values=c("#bebebe", "#552579", "#e8d3a2")) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  theme(text=element_text(size=16,  family="Encode Sans Normal")) +
  coord_flip() +
  labs(y = "Salary Distribution",
       x = "Employment Sector")
  
##Nonprofit/NGO sector

nonProfNGO <- subset(employData, employSector == "Nonprofit/NGO")


#nonprofit domestic vs national
nonprofLoc <- nonProfNGO  %>%
  group_by(international) %>% 
  filter(!is.na(international)) %>%
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(rel.freq = n / sum(n ))
addWorksheet(jobInfo, "Nonprof or NGO International")
writeData(jobInfo, sheet = "Nonprof or NGO International", x = nonprofLoc)

#nonprofit Industry Area
nonProfEmploySector <- nonProfNGO %>%
  group_by(employInd) %>%
  filter(!is.na(employInd)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))
addWorksheet(jobInfo, "Nonprof or NGO Empoy Sector")
writeData(jobInfo, sheet = "Nonprof or NGO Empoy Sector", x = nonProfEmploySector)

sum(nonProfEmploySector$n)


##nonprofit Job Titles
nonProfJobTitle <- nonProfNGO %>%
  group_by(jobTitle) %>%
  filter(!is.na(jobTitle)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Nonprof or NGO Job Title")
writeData(jobInfo, sheet = "Nonprof or NGO Job Title", x = nonProfJobTitle)

##NonProf Employee Info
nonProfEmployInfo <- nonProfNGO %>%
  group_by(currentEmployInfo) %>%
  filter(!is.na(currentEmployInfo)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Nonprof or NGO Employer Info")
writeData(jobInfo, sheet = "Nonprof or NGO Employer Info", x = nonProfEmployInfo)

##Nonprofit location
nonProfNGO  %>%
  group_by(employState) %>%
  filter(!is.na(employState)) %>%
  summarise(n=n()) %>%
  ungroup %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

#salary by subsector
nonProfNGO  %>%
  group_by(empSubsector) %>% 
  filter(!is.na(empSubsector)) %>%
  filter(!is.na(currentSalary)) %>%
  summarise(Salary = mean(currentSalary)) %>%
  arrange(desc(Salary))

###Public Sector###

pubGov <- subset(employData, employSector == "Public/Government")
jobInfo <- createWorkbook()


#Public Governmetn Subsector
pubgovSubsector <- pubGov  %>%
  group_by(empSubsector) %>% 
  filter(!is.na(empSubsector)) %>%
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Public Gov. Subsector")
writeData(jobInfo, sheet = "Public Gov. Subsector", x = pubgovSubsector)

#pubGov Industry Area
pubGovIndArea <- pubGov %>%
  group_by(employInd) %>%
  filter(!is.na(employInd)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Pub Gov Industry Area")
writeData(jobInfo, sheet = "Pub Gov Industry Area", x = pubGovIndArea)

##pubGov International
intpubGov <- pubGov %>%
  group_by(international) %>%
  filter(!is.na(international)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency)) 

addWorksheet(jobInfo, "Pub Gov International")
writeData(jobInfo, sheet = "Pub Gov International", x = intpubGov)


##government job titles Job Titles
pubGovJobTitle <- pubGov %>%
  group_by(jobTitle) %>%
  filter(!is.na(jobTitle)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Pub Gov Job Title")
writeData(jobInfo, sheet = "Pub Gov Job Title", x = pubGovJobTitle)

##Employee Info
pubGovEmployInfo <- pubGov %>%
  group_by(currentEmployInfo) %>%
  filter(!is.na(currentEmployInfo)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Pub Gov Employer Info")
writeData(jobInfo, sheet = "Pub Gov Employer Info", x = pubGovEmployInfo)

##pubGov location
pubGov  %>%
  group_by(employState) %>%
  filter(!is.na(employState)) %>%
  summarise(n=n()) %>%
  ungroup %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

#pubGov domestic vs national
pubGov  %>%
  group_by(international) %>% 
  filter(!is.na(international)) %>%
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(total = sum(n), rel.freq = n / total)

#Location by State 
pubGov  %>%
  group_by(employState) %>% 
  filter(!is.na(employState)) %>%
  summarise(n = n()) %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

pubGov  %>%
  group_by(empSubsector) %>% 
  filter(!is.na(empSubsector)) %>%
  filter(!is.na(currentSalary)) %>%
  summarise(Salary = mean(currentSalary)) %>%
  arrange(desc(Salary))


##Private Sector

privEnt <- subset(employData, employSector == "For-profit/Social Enterprise")

#private domestic vs national
privEntLoc <- privEnt %>%
  group_by(international) %>%
  filter(!is.na(international)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency)) 

addWorksheet(jobInfo, "Private International")
writeData(jobInfo, sheet = "Private International", x = privEntLoc)

privEntSubsector <- privEnt  %>%
  group_by(empSubsector) %>% 
  filter(!is.na(empSubsector)) %>%
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Private Subsector")
writeData(jobInfo, sheet = "Private Subsector", x = privEntSubsector)

#nonprofit Industry Area
privEntIndArea <- privEnt %>%
  group_by(employInd) %>%
  filter(!is.na(employInd)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Private Subsector")
writeData(jobInfo, sheet = "Private Subsector", x = privEntSubsector)
##government job titles Job Titles
privEntJobTitle <- privEnt %>%
  group_by(jobTitle) %>%
  filter(!is.na(jobTitle)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Private Job Titles")
writeData(jobInfo, sheet = "Private Job Titles", x = privEntJobTitle)

##Employee Info
privEntEmployInfo <- privEnt %>%
  group_by(currentEmployInfo) %>%
  filter(!is.na(currentEmployInfo)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(relativeFrequency = n/sum(n))  %>%
  arrange(desc(relativeFrequency))

addWorksheet(jobInfo, "Private Employer Info")
writeData(jobInfo, sheet = "Private Employer Info", x = privEntEmployInfo)

##pubGov location
privEnt  %>%
  group_by(employState) %>%
  filter(!is.na(employState)) %>%
  summarise(n=n()) %>%
  ungroup %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

#pubGov domestic vs national
privEnt  %>%
  group_by(international) %>% 
  filter(!is.na(international)) %>%
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(total = sum(n), rel.freq = n / total)

#Location by State 
privEnt  %>%
  group_by(employState) %>% 
  filter(!is.na(employState)) %>%
  summarise(n = n()) %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

privEnt  %>%
  group_by(empSubsector) %>% 
  filter(!is.na(empSubsector)) %>%
  filter(!is.na(currentSalary)) %>%
  summarise(Salary = mean(currentSalary)) %>%
  arrange(desc(Salary))

searchTime <- employData  %>%
  group_by(jobSearchTime) %>% 
  filter(!is.na(jobSearchTime)) %>%
  filter(!is.na(jobSearchTime)) %>%
  summarise(n = n()) %>%
  mutate(relativeFrequency = n/sum(n)) %>%
  arrange(desc(relativeFrequency))

addWorksheet(dataTables, "Months Seeking Emp")
writeData(dataTables, sheet = "Months Seeking Emp", x = searchTime)

saveWorkbook(dataTables, "Class of 2019 Employment Overview.xlsx")
saveWorkbook(jobInfo, "Class of 2019 Sector Breakdown.xlsx")

