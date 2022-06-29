## Intro --------------------

# The goal of this script is to calculate biomass estimates using taxa coefficients provided by Wisseman of Aquatic Biology Associates, Inc. to a level that's coarse enough to be compared to CFS biomass values.

# load packages
install.packages("tidyverse")
library(tidyverse)

# read in data
df <- read_csv("data/Taxa Traits_Coefficients.csv") 
head(df)
str(df)

# get overview of data 
summary(df)
unique(df$Family) 

#### create template of "lifeStage", "sizeClass" and "length" columns; estimate biomass for each size class
# create vectors for each category
length_mm <- rep(seq(from = 0.5, to = 20, by = 0.5), 3)
lifeStage <- c(rep((0), 40), rep((1), 40), rep((2), 40))
sizeClass <- c(rep((1), 3), rep((2), 12), rep((3), 9), rep((4), 16))

# join into one dataframe
# use as template for future families
lsl <- cbind(lifeStage, sizeClass, length_mm) 
lsl <- as.data.frame((lsl))


# Chironomidae ------------------------------------------------------------

# combine chironomidae subfamilies all into chironomidae
# remove unnecessary columns
df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Chironomidae: Tanypodinae" ~ "Chironomidae",
                             Family == "Chironomidae: Orthocladiinae" ~ "Chironomidae",
                             Family == "Chironomidae: Chironominae" ~ "Chironomidae",
                             Family == "Chironomidae: Podonominae" ~ "Chironomidae",
                             Family == "Chironomidae: Diamesinae" ~ "Chironomidae",
                             Family == "Chironomidae: Chironominae: Tanytarsini" ~ "Chironomidae",
                             Family == "Chironomidae: Prodiamesinae" ~ "Chironomidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

unique(df1$Family1)

# create df of just chironomidae
# check biomass of different lengths using length-mass equation: M = a * L^b
chiro <- df1 %>% 
  filter(Family1 == "Chironomidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 
  
# check coefficient values 
summary(chiro$a)
unique(chiro$a)
summary(chiro$b)
unique(chiro$b)
# only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult
chiroL <- chiro %>% 
  slice(1:5) %>% 
  filter(Stage == "L")
chiroP <- chiro %>% 
  slice(1:5) %>% 
  filter(Stage == "P")
chiroA <- chiro %>% 
  slice(1:5) %>% 
  filter(Stage == "A")

#### create template of "lifeStage", "sizeClass" and "length" columns; estimate biomass for each size class
# create vectors for each category
length_mm <- rep(seq(from = 0.5, to = 20, by = 0.5), 3)
lifeStage <- c(rep((0), 40), rep((1), 40), rep((2), 40))
sizeClass <- c(rep((1), 3), rep((2), 12), rep((3), 9), rep((4), 16))

# join into one dataframe
# use as template for future families
lsl <- cbind(lifeStage, sizeClass, length_mm) 
lsl <- as.data.frame((lsl))

# rename to be Chironomidae specific
lslChiro <- lsl

# add columns for Chironomidae
lslChiro$Family <- "Chironomidae"
lslChiro$TaxonCode <- "00110"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass
chiroBiomass <- lslChiro %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ chiroL$a,
         lifeStage == "1" ~ chiroP$a,
         lifeStage == "2" ~ chiroA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ chiroL$b,
                       lifeStage == "1" ~ chiroP$b,
                       lifeStage == "2" ~ chiroA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)

# summarize biomass 
chiroBiomassSum <- chiroBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Baetidae ----------------------------------------------------------------

#You have to re-run the tidyverse or the %>% won't work 
library(tidyverse)

# re-read the data so all families are included

read.csv("data/Taxa Traits_Coefficients.csv")
df <- read.csv("data/Taxa Traits_Coefficients.csv") 
head(df)
str(df)

# getting family names
summary(df)
unique(df$Family)
            
# combine all Baetidae families, All Baetidae are in the same family category, I left this in as a placeholder for other families
#remove all unnecessary columns
# getting error message for the df %>% for this, it might just be because it is the same thing

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Baetidae" ~ "Baetidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

unique(df1$Family1)

# create df of just baetidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
baeti <- df1 %>% 
  filter(Family1 == "Baetidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(baeti$a)
unique(baeti$a)
summary(baeti$b)
unique(baeti$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
baetiL <- baeti %>% 
  slice(1:5) %>% 
  filter(Stage == "L")
baetiP <- baeti %>% 
  slice(1:5) %>% 
  filter(Stage == "P")
baetiA <- baeti %>% 
  slice(1:5) %>% 
  filter(Stage == "A")

#### create template of "lifeStage", "sizeClass" and "length" columns; estimate biomass for each size class
# create vectors for each category
length_mm <- rep(seq(from = 0.5, to = 20, by = 0.5), 3)
lifeStage <- c(rep((0), 40), rep((1), 40), rep((2), 40))
sizeClass <- c(rep((1), 3), rep((2), 12), rep((3), 9), rep((4), 16))

# join into one dataframe
# use as template for future families
lsl <- cbind(lifeStage, sizeClass, length_mm) 
lsl <- as.data.frame((lsl))

# rename to be Baetidae specific
lslbaeti <- lsl

# add columns for Baetidae
lslbaeti$Family <- "Baetidae"
lslbaeti$TaxonCode <- "00200"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

baetiBiomass <- lslbaeti %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ baetiL$a,
                       lifeStage == "1" ~ baetiP$a,
                       lifeStage == "2" ~ baetiA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ baetiL$b,
                       lifeStage == "1" ~ baetiP$b,
                       lifeStage == "2" ~ baetiA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
baetiBiomassSum <- baetiBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


