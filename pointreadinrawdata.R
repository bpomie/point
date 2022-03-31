# ================================================
# Read in individual data from data/dataindividual
# ================================================

# Set the path
setwd("~/Documents/GitHub/point/data/dataraw/")
#Get file names of individual data files
my_files <- list.files(pattern = "xlsx$")
# Read each excel file into a list
listExcel = lapply(my_files, read_excel)
# Concatenate the data in each file into one combined data frame
results = do.call(rbind.data.frame, listExcel)
rm(listExcel)

# Recode 9999 to missing data
results[results==9999] <- NA

# ================================================
# Compute AOI hits
# ================================================

# Define AOIs
aoileft = c(5,540,810,1075)
aoiright = c(1110,540,1915,1075)
aoicenter = c(810,390,1110,690)

results <- results %>%
  mutate(gazex = as.numeric(gazex)) %>%
  mutate(gazey = as.numeric(gazey))

# Compute AOI hits for the following AOIs
# tghit (object targeted by the action)
# dthit (distractor object)
# centerhit

results <- results %>%
  mutate(tghit = ifelse((locactiontarget == "left" | locactiontarget == "L"),
                        (ifelse((gazex >= aoileft[1] & gazex <= aoileft[3] & gazey >= aoileft[2] & gazey <= aoileft[4]),1,0)),
                        (ifelse((locactiontarget == "right" | locactiontarget == "R"),
                                ifelse((gazex >= aoiright[1] & gazex <= aoiright[3] & gazey >= aoiright[2] & gazey <= aoiright[4]),1,0),"?")))) %>%
  mutate(dthit = ifelse((locactiontarget == "right" | locactiontarget == "R"),
                        (ifelse((gazex >= aoileft[1] & gazex <= aoileft[3] & gazey >= aoileft[2] & gazey <= aoileft[4]),1,0)),
                        (ifelse((locactiontarget == "left" | locactiontarget == "L"),
                                ifelse((gazex >= aoiright[1] & gazex <= aoiright[3] & gazey >= aoiright[2] & gazey <= aoiright[4]),1,0),"?")))) %>%
  mutate(missingdata = ifelse((is.na(gazex) == 1 & is.na(gazey) == 1), 1, 0)) %>%
  mutate(centerhit = ifelse((gazex >= aoicenter[1] & gazex <= aoicenter[3] & gazey >= aoicenter[2] & gazey <= aoicenter[4]),1,0))

results <- results %>%
  mutate(tghit = as.numeric(tghit)) %>%
  mutate(dthit = as.numeric(dthit))

# Add experiment information (based on participant ID)
results <- results %>%
  mutate(experiment = ifelse((ID<17),1,ifelse((ID<33),2,(ifelse(ID<49,3,4)))))

# ================================================
# Attendance before test
# ================================================

setwd("~/Documents/GitHub/point/data/")
attendancebeforetest <- read_excel("attendancebeforetest.xlsx")
results <- merge(results,attendancebeforetest, by=c("ID","trial","experiment")) 
colnames(results)[16] <- "attBeforeTest"

View(results)
# ================================================
# Save the data
# ================================================

library(openxlsx)

write.xlsx(results, "dataindividual/datacombined.xlsx", sheetName = "data")
