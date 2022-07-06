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
  slice(1:2) %>% 
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

# **Note MO: this shouldn't be the case- I did not need to on my end. When you load it back up, see what happens and if this is still necessary.
library(tidyverse)

# re-read the data so all families are included

# **Note MO: you don't need to re-read the data, it is already loaded and saved as "df". You can check this in the environment and we know we haven't altered the raw data load. Part of the idea behind the headings is that everything in the "Intro" section should only need to be done once at the beginning, when you load the script. I would start this code (and subsequent families) as a copy from line 49 (beginning of the chironomidae section, i.e. where you started on line 149). I have another note on line 138 suggesting another part be added to the intro

read.csv("data/Taxa Traits_Coefficients.csv")
df <- read.csv("data/Taxa Traits_Coefficients.csv") 
head(df)
str(df)

# getting family names
summary(df)
unique(df$Family)
            
# combine all Baetidae families, All Baetidae are in the same family category, I left this in as a placeholder for other families
#remove all unnecessary columns

# **Note MO: I think this df -> df1 is something that could be in the intro code, since you don't need to run it again. I believe chironomids are the only ones in which there were subfamilies...

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Baetidae" ~ "Baetidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

unique(df1$Family1)

# create df of just baetidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
baeti <- df2 %>% 
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
# **Note MO: I don't see a pupa here...if none, porbably best to use larva than adult for the value

# **Note MO: the number of rows you will slice here will likely be different for each family. For instance, here there are only 2 rows that contain baetidae (family level) L, P, or A coefficients. You want one for each. As noted before, there is no P, so you will need to make a copy of the L and use it for P (**CHECK WITH JASMINE ABOUT THIS CHOICE)

# filter out three rows for larva, pupa, adult, be sure to change the names again
baetiL <- baeti %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
baetiP <- baetiL # **Note MO: here is where I am making a copy of L and naming it P
baetiP[1,2] <-  "P" # **Note MO: this is changing the value of the cell in the first row, second column ("Stage") from larva (L) to pupa (P) since we just copied the larva row to use for pupa
baetiA <- baeti %>% 
  slice(1:2) %>% 
  filter(Stage == "A")

# **Note MO: this section (lines 178-187) are all already in the intro and don't need to be re-run as long as you haven't closed the script. For conciseness, I would remove this here and just always run the intro when you start. You can always check in the Environment panel what dataframes you have already run.

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
# **Note MO: this error was occurring because there was nothing in the baetiP dataframe because there was no value, see note on 164

# summarize biomass 
baetiBiomassSum <- baetiBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Hydroptilidae -------------------------------------------------

# re-read in data
df <- read_csv("data/Taxa Traits_Coefficients.csv") 
head(df)
str(df)

# get overview of data 
summary(df)
unique(df$Family) 

# combine all Hydroptilidae families, All Hyroptilidae are in the same family category, I left this in as a placeholder for other families
#remove all unnecessary columns

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Hydroptilidae" ~ "Hydroptilidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

unique(df1$Family1)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
hydro <- df1 %>% 
  filter(Family1 == "Hydroptilidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(hydro$a)
unique(hydro$a)
summary(hydro$b)
unique(hydro$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
hydroL <- hydro %>% 
  slice(1:5) %>% 
  filter(Stage == "L")
hydroP <- hydro %>% 
  slice(1:5) %>% 
  filter(Stage == "P")
hydroA <- hydro %>% 
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
lslhydro <- lsl

# add columns for Baetidae
lslhydro$Family <- "Hydroptilidae"
lslhydro$TaxonCode <- "02300"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

hydroBiomass <- lslhydro %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ hydroL$a,
                       lifeStage == "1" ~ hydroP$a,
                       lifeStage == "2" ~ hydroA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ hydroL$b,
                       lifeStage == "1" ~ hydroP$b,
                       lifeStage == "2" ~ hydroA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
baetiBiomassSum <- baetiBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))









