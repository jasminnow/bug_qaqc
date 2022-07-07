## Intro -----------------------------------------------------------------------

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


# Chironomidae -----------------------------------------------------------------

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


# Baetidae ---------------------------------------------------------------------

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


# Hydroptilidae ----------------------------------------------------------------

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
  slice(1:3) %>% 
  filter(Stage == "L")
hydroP <- hydro %>% 
  slice(1:3) %>% 
  filter(Stage == "P")
hydroA <- hydro %>% 
  slice(1:3) %>% 
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

# rename to be Hydroptilidae specific
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
hydroBiomassSum <- hydroBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


#Aphididae ---------------------------------------------------------------------

read.csv("data/Taxa Traits_Coefficients.csv")
df <- read.csv("data/Taxa Traits_Coefficients.csv") 
head(df)
str(df)

# getting family names
summary(df)
unique(df$Family)

#remove all unnecessary columns

# *In Jasmine's table the aphids are under the name Aphidae, I am changing it to aphididae to match the crammer table

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Aphidae" ~ "Aphididae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

unique(df1$Family1)

# create df of just baetidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
aphid <- df1 %>% 
  filter(Family1 == "Aphididae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(aphid$a)
unique(aphid$a)
summary(aphid$b)
unique(aphid$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different
# **Note MO: I don't see a pupa here...if none, probably best to use larva than adult for the value

##So there were only two lifestages, 2 L and 1 A. They also had the same exact values for each other. The code was giving me an error (look at this part of code for gammaridae, I figured it out there first and then came back to this). Since they were they same values, I just had all the lifestages look for the L since it was the first one on the list and had it use those values, since they are all the same

# filter out three rows for larva, pupa, adult, be sure to change the names again
aphidL <- aphid %>% 
  slice(1:1) %>% 
  filter(Stage == "L")
aphidP <- aphid %>% 
  slice(1:1) %>% 
  filter(Stage == "L")
aphidA <- aphid %>% 
  slice(1:1) %>% 
  filter(Stage == "L")

# rename to be Aphididae specific
lslaphid <- lsl

# add columns for Aphididae
lslaphid$Family <- "Aphididae"
lslaphid$TaxonCode <- "05950"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

aphidBiomass <- lslaphid %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ aphidL$a,
                       lifeStage == "1" ~ aphidP$a,
                       lifeStage == "2" ~ aphidA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ aphidL$b,
                       lifeStage == "1" ~ aphidP$b,
                       lifeStage == "2" ~ aphidA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)

# summarize biomass 
aphidBiomassSum <- aphidBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


#Gammaridae --------------------------------------------------------------------

df <- read_csv("data/Taxa Traits_Coefficients.csv") 
head(df)
str(df)

# get overview of data 
summary(df)
unique(df$Family) 

# combine all Gammaridae families, All Gammaridae are in the same family category, Angisogammaridae is more specific than anything crammer has, so I added it to the Gammaridae family
#remove all unnecessary columns

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Anisogammaridae" ~ "Gammaridae",
                             Family == "Gammaridae" ~ "Gammaridae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

unique(df1$Family1)

# create df of just Gammaridae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
gamma <- df1 %>% 
  filter(Family1 == "Gammaridae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(gamma$a)
unique(gamma$a)
summary(gamma$b)
unique(gamma$b)
# !there is only one value for a, one value for b!

# filter out three rows for larva, pupa, adult, be sure to change the names again
## I filtered for U, since the typical lifestages (A, P, L) were not available, not sure if this is something we need to revisit later on
##Since there was only one value, it was giving me an error in line 465-473 saying "error in mutat(): problem while comupting 'a =case_when(...)', lifestage =="0" ~ gammaL$a', lifestage == "1" ~ gammap$a', 'lifestage == "2" ~ gammaA$a' must be length 120 or one, not 3. I had my slice from 1:3 since there were three different data points, but I changed it to 1 since they are all the same a's and b's anyways and the code then worked

gammaL <- gamma %>% 
  slice(1:1) %>% 
  filter(Stage == "U")
gammaP <- gamma %>% 
  slice(1:1) %>% 
  filter(Stage == "U")
gammaA <- gamma %>% 
  slice(1:1) %>% 
  filter(Stage == "U")

#### create template of "lifeStage", "sizeClass" and "length" columns; estimate biomass for each size class
# create vectors for each category
length_mm <- rep(seq(from = 0.5, to = 20, by = 0.5), 3)
lifeStage <- c(rep((0), 40), rep((1), 40), rep((2), 40))
sizeClass <- c(rep((1), 3), rep((2), 12), rep((3), 9), rep((4), 16))

# join into one dataframe
# use as template for future families
lsl <- cbind(lifeStage, sizeClass, length_mm) 
lsl <- as.data.frame((lsl))

# rename to be Hydroptilidae specific
lslgamma <- lsl

# add columns for Baetidae
lslgamma$Family <- "Gammaridae"
lslgamma$TaxonCode <- "21012"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

gammaBiomass <- lslgamma %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ gammaL$a,
                       lifeStage == "1" ~ gammaP$a,
                       lifeStage == "2" ~ gammaA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ gammaL$b,
                       lifeStage == "1" ~ gammaP$b,
                       lifeStage == "2" ~ gammaA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
gammaBiomassSum <- gammaBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))

#Ephemeroptera -----------------------------------------------------------------


df <- read_csv("data/Taxa Traits_Coefficients.csv") 
head(df)
str(df)

# get overview of data 
summary(df)
unique(df$Family) 

# combine all Ephemeroptera families, All Ephemeroptera are in the same family category, once again the other table is more specific than the crammer one, so I have renamed the other familes into the catagory that crammer has for them
#remove all unnecessary columns

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Ephemerellidae" ~ "Ephemeroptera",
                             Family == "Ephemeridae" ~ "Ephemeroptera",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)



# create df of just Ephemeroptera, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
ephem <- df1 %>% 
  filter(Family1 == "Ephemeroptera") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(ephem$a)
unique(ephem$a)
summary(ephem$b)
unique(ephem$b)
# This has 6 unique a and b values!

# filter out three rows for larva, pupa, adult, be sure to change the names again

ephemL <- ephem %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
ephemP <- ephem %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
ephemA <- ephem %>% 
  slice(1:2) %>% 
  filter(Stage == "A")

# rename to be Ephemeroptera specific
lslephem <- lsl

# add columns for Ephemeroptera
lslephem$Family <- "Ephemeroptera"
lslephem$TaxonCode <- "00100"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

ephemBiomass <- lslephem %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ ephemL$a,
                       lifeStage == "1" ~ ephemP$a,
                       lifeStage == "2" ~ ephemA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ ephemL$b,
                       lifeStage == "1" ~ ephemP$b,
                       lifeStage == "2" ~ ephemA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
ephemBiomassSum <- ephemBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Leptohyphidae ----------------------------------------------------------------


# combine all Leptohyphidae families, All Leptohyphidae are in the same family category, I left this in as a placeholder for other families
#remove all unnecessary columns

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Leptophlebiidae" ~ "Leptohyphidae",
                             Family == "Leptohyphidae" ~ "Leptohyphidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)


# create df of just Leptohyphidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
lepto <- df1 %>% 
  filter(Family1 == "Leptohyphidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(lepto$a)
unique(lepto$a)
summary(lepto$b)
unique(lepto$b)
## has 3 diff values for a and b 

# filter out three rows for larva, pupa, adult, be sure to change the names again
## changed the P to an L again since there are no P data points and P is closer in weight than compaired to A
leptoL <- lepto %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
leptoP <- lepto %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
leptoA <- lepto %>% 
  slice(1:2) %>% 
  filter(Stage == "A")

# rename to be Leptohyphidae specific
lsllepto <- lsl

# add columns for Leptohyphidae
lsllepto$Family <- "Leptohyphidae"
lsllepto$TaxonCode <- "00500"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

leptoBiomass <- lsllepto %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ leptoL$a,
                       lifeStage == "1" ~ leptoP$a,
                       lifeStage == "2" ~ leptoA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ leptoL$b,
                       lifeStage == "1" ~ leptoP$b,
                       lifeStage == "2" ~ leptoA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
### So this part of code above will only work if you have one point of data for A, P, L, if you try to add more than one thing it won't work (unless I am doing something else wrong with it)

# summarize biomass 
leptoBiomassSum <- leptoBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Hebridae ---------------------------------------------------------------------


# combine all Hebridae families, All Hebridae are in the same family category, I left this in as a placeholder for other families
#remove all unnecessary columns

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Hebridae" ~ "Hebridae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)


# create df of just Hebridae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
hebri <- df1 %>% 
  filter(Family1 == "Hebridae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(hebri$a)
unique(hebri$a)
summary(hebri$b)
unique(hebri$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
hebriL <- hebri %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
hebriP <- hebri %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
hebriA <- hebri %>% 
  slice(1:2) %>% 
  filter(Stage == "A")

# rename to be Hebridae specific
lslhebri <- lsl

# add columns for Hebridae
lslhebri$Family <- "Hebridae"
lslhebri$TaxonCode <- "05800"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

hebriBiomass <- lslhebri %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ hebriL$a,
                       lifeStage == "1" ~ hebriP$a,
                       lifeStage == "2" ~ hebriA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ hebriL$b,
                       lifeStage == "1" ~ hebriP$b,
                       lifeStage == "2" ~ hebriA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
hebriBiomassSum <- hebriBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Hydropsychidae ---------------------------------------------------------------


# combine all Hydroptilidae families, All Hyroptilidae are in the same family category, I left this in as a placeholder for other families
#remove all unnecessary columns

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Hydropsychidae" ~ "Hydropsychidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)


# create df of just Hydropsychidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
psych <- df1 %>% 
  filter(Family1 == "Hydropsychidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(psych$a)
unique(psych$a)
summary(psych$b)
unique(psych$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
psychL <- psych %>% 
  slice(1:3) %>% 
  filter(Stage == "L")
psychP <- psych %>% 
  slice(1:3) %>% 
  filter(Stage == "P")
psychA <- psych %>% 
  slice(1:3) %>% 
  filter(Stage == "A")

# rename to be Hydropsychidae specific
lslpsych <- lsl

# add columns for Hydropsychidae
lslpsych$Family <- "Hydropsychidae"
lslpsych$TaxonCode <- "02300"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

psychBiomass <- lslpsych %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ psychL$a,
                       lifeStage == "1" ~ psychP$a,
                       lifeStage == "2" ~ psychA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ psychL$b,
                       lifeStage == "1" ~ psychP$b,
                       lifeStage == "2" ~ psychA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
psychBiomassSum <- psychBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Hydracarina ------------------------------------------------------------------

##So in Jasmine's table mites are labeled in their family name as "x", so transfer x to be Hydracarina, higher class that I will use later on is Acari

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Hydracarina",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)


# create df of just Hydracarina, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
hydra <- df1 %>% 
  filter(Family1 == "Hydracarina") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(hydra$a)
unique(hydra$a)
summary(hydra$b)
unique(hydra$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
hydraL <- hydra %>% 
  slice(90) %>% 
  filter(Stage == "U")
hydraP <- hydra %>% 
  slice(90) %>% 
  filter(Stage == "U")
hydraA <- hydra %>% 
  slice(90) %>% 
  filter(Stage == "U")

# rename to be Hydracarina specific
lslhydra <- lsl

# add columns for Hydracarina
lslhydra$Family <- "Hydracarina"
lslhydra$TaxonCode <- "08010"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

hydraBiomass <- lslhydra %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ hydraL$a,
                       lifeStage == "1" ~ hydraP$a,
                       lifeStage == "2" ~ hydraA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ hydraL$b,
                       lifeStage == "1" ~ hydraP$b,
                       lifeStage == "2" ~ hydraA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
hydraBiomassSum <- hydraBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Ceratopogonidae --------------------------------------------------------------

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Ceratopogonidae" ~ "Ceratopogonidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just Ceratopogonidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
cerat <- df1 %>% 
  filter(Family1 == "Ceratopogonidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(cerat$a)
unique(cerat$a)
summary(cerat$b)
unique(cerat$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
ceratL <- cerat %>% 
  slice(1:3) %>% 
  filter(Stage == "L")
ceratP <- cerat %>% 
  slice(1:3) %>% 
  filter(Stage == "P")
ceratA <- cerat %>% 
  slice(1:3) %>% 
  filter(Stage == "A")


# rename to be Ceratopogonidae specific
lslcerat <- lsl

# add columns for Ceratopogonidae
lslcerat$Family <- "Ceratopogonidae"
lslcerat$TaxonCode <- "01910"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

ceratBiomass <- lslcerat %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ ceratL$a,
                       lifeStage == "1" ~ ceratP$a,
                       lifeStage == "2" ~ ceratA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ ceratL$b,
                       lifeStage == "1" ~ ceratP$b,
                       lifeStage == "2" ~ ceratA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
ceratBiomassSum <- ceratBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))

# Sciaridae---------------------------------------------------------------------

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Sciaridae" ~ "Sciaridae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just Sciaridae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
sciar <- df1 %>% 
  filter(Family1 == "Sciaridae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(sciar$a)
unique(sciar$a)
summary(sciar$b)
unique(sciar$b)
## only one unique a and b

# filter out three rows for larva, pupa, adult, be sure to change the names again
## !so there is only the Adult form of this insect! So that is why I had them only filter for A
sciarL <- sciar %>% 
  slice(1:3) %>% 
  filter(Stage == "A")
sciarP <- sciar %>% 
  slice(1:3) %>% 
  filter(Stage == "A")
sciarA <- sciar %>% 
  slice(1:3) %>% 
  filter(Stage == "A")


# rename to be Sciaridae specific
lslsciar <- lsl

# add columns for Sciaridae
lslsciar$Family <- "Sciaridae"
lslsciar$TaxonCode <- "01913"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

sciarBiomass <- lslsciar %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ sciarL$a,
                       lifeStage == "1" ~ sciarP$a,
                       lifeStage == "2" ~ sciarA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ sciarL$b,
                       lifeStage == "1" ~ sciarP$b,
                       lifeStage == "2" ~ sciarA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
sciarBiomassSum <- sciarBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Hymenoptera ------------------------------------------------------------------

##So in Jasmine's table mites are labeled in their family name as "x", so transfer x to be Hymenoptera, higher class that I will use later on is Hymenoptera


df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Hymenoptera",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just Hymenoptera, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
hymen <- df1 %>% 
  filter(Family1 == "Hymenoptera") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(hymen$a)
unique(hymen$a)
summary(hymen$b)
unique(hymen$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
hymenL <- hymen %>% #this is larval sawfiles, not sure if this is what would be preferred 
  slice(43) %>% 
  filter(Stage == "L")
hymenP <- hymen %>%  #no pupa stage for Hymenoptera so using the larval stage again 
  slice(43) %>% 
  filter(Stage == "L")
hymenA <- hymen %>%    
  slice(46) %>% 
  filter(Stage == "A")
#this is an adult Hymenoptera, but there are a wide varietiy, some exculuding ants specicically, so I do not know what is preferred 

# rename to be Hymenoptera specific
lslhymen <- lsl

# add columns for Hymenoptera
lslhymen$Family <- "Hymenoptera"
lslhymen$TaxonCode <- "09000"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

hymenBiomass <- lslhymen %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ hymenL$a,
                       lifeStage == "1" ~ hymenP$a,
                       lifeStage == "2" ~ hymenA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ hymenL$b,
                       lifeStage == "1" ~ hymenP$b,
                       lifeStage == "2" ~ hymenA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
hymenBiomassSum <- hymenBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))



# Plecoptera--------------------------------------------------------------------

##Plecoptera family in this data set is also "x" so will make all the "x" be for Plecoptera and then later on only filter for Plecoptera

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Plecoptera",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just Plecoptera, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
pleco <- df1 %>% 
  filter(Family1 == "Plecoptera") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(pleco$a)
unique(pleco$a)
summary(pleco$b)
unique(pleco$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
plecoL <- pleco %>% #since I had to use all families that were just described as "x" row 174 is the L form of Plecoptera
  slice(174) %>% 
  filter(Stage == "L")
plecoP <- pleco %>% 
  slice(174) %>%   #using L form for P since there is no P information 
  filter(Stage == "L")
plecoA <- pleco %>% 
  slice(173) %>%     #row 173 is the row for adult Plecoptera
  filter(Stage == "A")


# rename to be Plecoptera specific
lslpleco <- lsl

# add columns for Plecoptera
lslpleco$Family <- "Plecoptera"
lslpleco$TaxonCode <- "03000"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

plecoBiomass <- lslpleco %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ plecoL$a,
                       lifeStage == "1" ~ plecoP$a,
                       lifeStage == "2" ~ plecoA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ plecoL$b,
                       lifeStage == "1" ~ plecoP$b,
                       lifeStage == "2" ~ plecoA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
plecoBiomassSum <- plecoBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))



# Araneae ----------------------------------------------------------------------
#Araneae aka spiders, are also are "x" under family, so I am doing the "x" ~ Araneae thing agian

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Araneae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
arane <- df1 %>% 
  filter(Family1 == "Araneae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(arane$a)
unique(arane$a)
summary(arane$b)
unique(arane$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
araneL <- arane %>% 
  slice(12) %>%  #the only bit of data from this table is that is under the "U" category, it is in row 12
  filter(Stage == "U")
araneP <- arane %>% 
  slice(12) %>% 
  filter(Stage == "U")
araneA <- arane %>% 
  slice(12) %>% 
  filter(Stage == "U")


# rename to be Araneae specific
lslarane <- lsl

# add columns for Araneae
lslarane$Family <- "Araneae"
lslarane$TaxonCode <- "110000"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

araneBiomass <- lslarane %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ araneL$a,
                       lifeStage == "1" ~ araneP$a,
                       lifeStage == "2" ~ araneA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ araneL$b,
                       lifeStage == "1" ~ araneP$b,
                       lifeStage == "2" ~ araneA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
araneBiomassSum <- araneBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Coleoptera--------------------------------------------------------------------

#Jamaine's table had much more specific coleptera's than the crammer table, I put all the higher class Coleoptera data as Coleoptera since this table did not have Coleoptera as a family


df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Coleoptera",
                             Family == "Carabidae" ~ "Coleoptera",
                             Family == "Chrysomelidae" ~ "Coleoptera",
                             Family == "Coccinellidae" ~ "Coleoptera",
                             Family == "Elateridae" ~ "Coleoptera",
                             Family == "Scolytidae" ~ "Coleoptera",
                             Family == "Staphylinidae" ~ "Coleoptera",
                             Family == "Cantharidae" ~ "Coleoptera",
                             Family == "Cicindelidae" ~ "Coleoptera",
                             Family == "Curculionidae" ~ "Coleoptera",
                             Family == "Hydraenidae" ~ "Coleoptera",
                             Family == "Silphidae" ~ "Coleoptera",
                             Family == "Amphizoidae" ~ "Coleoptera",
                             Family == "Dytiscidae" ~ "Coleoptera",
                             Family == "Eulichadidae" ~ "Coleoptera",
                             Family == "Haliplidae" ~ "Coleoptera",
                             Family == "Hydrophilidae" ~ "Coleoptera",
                             Family == "Psephenidae" ~ "Coleoptera",
                             Family == "Scirtidae" ~ "Coleoptera",
                             Family == "Dryopidae" ~ "Coleoptera",
                             Family == "Elmidae" ~ "Coleoptera",
                             Family == "Gyrinidae" ~ "Coleoptera",
                             Family == "Helophoridae" ~ "Coleoptera",
                             Family == "Hydroscaphidae" ~ "Coleoptera",
                             Family == "Ptilodactylidae" ~ "Coleoptera",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just Coleoptera, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
coleo <- df1 %>% 
  filter(Family1 == "Coleoptera") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(coleo$a)
unique(coleo$a)
summary(coleo$b)
unique(coleo$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
#using the data where Coleoptera is the order for this data set 
coleoL <- coleo %>% 
  slice(14:16) %>% 
  filter(Stage == "L")
coleoP <- coleo %>% 
  slice(14:16) %>% 
  filter(Stage == "P")
coleoA <- coleo %>% 
  slice(14:16) %>% 
  filter(Stage == "A")


# rename to be Coleoptera specific
lslcoleo <- lsl

# add columns for Coleoptera
lslcoleo$Family <- "Coleoptera"
lslcoleo$TaxonCode <- "06000"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

coleoBiomass <- lslcoleo %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ coleoL$a,
                       lifeStage == "1" ~ coleoP$a,
                       lifeStage == "2" ~ coleoA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ coleoL$b,
                       lifeStage == "1" ~ coleoP$b,
                       lifeStage == "2" ~ coleoA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
coleoBiomassSum <- coleoBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))



# Curculionlidae----------------------------------------------------------------

#they are spelled different, but when I look them up online it seems to both be weevils, I changed the spelling to the cramer version for consistancy 

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Curculionidae" ~ "Curculionlidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
curcu <- df1 %>% 
  filter(Family1 == "Curculionlidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(curcu$a)
unique(curcu$a)
summary(curcu$b)
unique(curcu$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
##only one observation (A) 
curcuL <- curcu %>% 
  slice(1) %>% 
  filter(Stage == "A")
curcuP <- curcu %>% 
  slice(1) %>% 
  filter(Stage == "A")
curcuA <- curcu %>% 
  slice(1) %>% 
  filter(Stage == "A")


# rename to be Curculionlidae specific
lslcurcu <- lsl

# add columns for Curculionlidae
lslcurcu$Family <- "Curculionlidae"
lslcurcu$TaxonCode <- "06310"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

curcuBiomass <- lslcurcu %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ curcuL$a,
                       lifeStage == "1" ~ curcuP$a,
                       lifeStage == "2" ~ curcuA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ curcuL$b,
                       lifeStage == "1" ~ curcuP$b,
                       lifeStage == "2" ~ curcuA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
curcuBiomassSum <- curcuBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Diptera--------------------------------------------------------------------

#Again Jasmine's table is more detailed so I added the more specific Diptera to the list as well

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Diptera",
                             Family == "Phoridae" ~ "Diptera",
                             Family == "Syrphidae" ~ "Diptera",
                             Family == "Cecidomyiidae" ~ "Diptera",
                             Family == "Mycetophilidae" ~ "Diptera",
                             Family == "Scatopsidae" ~ "Diptera",
                             Family == "Sciaridae" ~ "Diptera",
                             Family == "Trichoceridae" ~ "Diptera",
                             Family == "Blephariceridae" ~ "Diptera",
                             Family == "Ceratopogonidae" ~ "Diptera",
                             Family == "Culicidae" ~ "Diptera",
                             Family == "Dixidae" ~ "Diptera",
                             Family == "Empididae" ~ "Diptera",
                             Family == "Muscidae" ~ "Diptera",
                             Family == "Pelecorhynchidae" ~ "Diptera",
                             Family == "Athericidae" ~ "Diptera",
                             Family == "Canacidae" ~ "Diptera",
                             Family == "Chaoboridae" ~ "Diptera",
                             Family == "Deuterophlebiidae" ~ "Diptera",
                             Family == "Dolichopodidae" ~ "Diptera",
                             Family == "Ephydridae" ~ "Diptera",
                             Family == "Oreoleptidae" ~ "Diptera",
                             Family == "Psychodidae" ~ "Diptera",
                             Family == "Ptychopteridae" ~ "Diptera",
                             Family == "Sciomyzidae" ~ "Diptera",
                             Family == "Stratiomyidae" ~ "Diptera",
                             Family == "Tanyderidae" ~ "Diptera",
                             Family == "Tipulidae" ~ "Diptera",
                             Family == "Sarcophagidae" ~ "Diptera",
                             Family == "Simuliidae" ~ "Diptera",
                             Family == "Tabanidae" ~ "Diptera",
                             Family == "Thaumaleidae" ~ "Diptera",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
dipte <- df1 %>% 
  filter(Family1 == "Diptera") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(dipte$a)
unique(dipte$a)
summary(dipte$b)
unique(dipte$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
dipteL <- dipte %>% 
  slice(18:20) %>% 
  filter(Stage == "L")
dipteP <- dipte %>% 
  slice(18:20) %>% 
  filter(Stage == "P")
dipteA <- dipte %>% 
  slice(18:20) %>% 
  filter(Stage == "A")


# rename to be Diptera specific
lsldipte <- lsl

# add columns for Diptera
lsldipte$Family <- "Diptera"
lsldipte$TaxonCode <- "01000"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

dipteBiomass <- lsldipte %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ dipteL$a,
                       lifeStage == "1" ~ dipteP$a,
                       lifeStage == "2" ~ dipteA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ dipteL$b,
                       lifeStage == "1" ~ dipteP$b,
                       lifeStage == "2" ~ dipteA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
dipteBiomassSum <- dipteBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))



# Empididae---------------------------------------------------------------------



df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Empididae" ~ "Empididae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
empid <- df1 %>% 
  filter(Family1 == "Empididae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(empid$a)
unique(empid$a)
summary(empid$b)
unique(empid$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
empidL <- empid %>% 
  slice(1:3) %>% 
  filter(Stage == "L")
empidP <- empid %>% 
  slice(1:3) %>% 
  filter(Stage == "P")
empidA <- empid %>% 
  slice(1:3) %>% 
  filter(Stage == "A")


# rename to be Empididae specific
lslempid <- lsl

# add columns for Empididae
lslempid$Family <- "Empididae"
lslempid$TaxonCode <- "01310"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

empidBiomass <- lslempid %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ empidL$a,
                       lifeStage == "1" ~ empidP$a,
                       lifeStage == "2" ~ empidA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ empidL$b,
                       lifeStage == "1" ~ empidP$b,
                       lifeStage == "2" ~ empidA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
empidBiomassSum <- empidBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Apidae -----------------------------------------------------------------------


df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Apidae" ~ "Apidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
apida <- df1 %>% 
  filter(Family1 == "Apidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(apida$a)
unique(apida$a)
summary(apida$b)
unique(apida$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different
###only one unique number for a and b


# filter out three rows for larva, pupa, adult, be sure to change the names again
apidaL <- apida %>% 
  slice(1) %>% 
  filter(Stage == "A")
apidaP <- apida %>% 
  slice(1) %>% 
  filter(Stage == "A")
apidaA <- apida %>% 
  slice(1) %>% 
  filter(Stage == "A")
###Only one data point for all of Apidae, which is the A form so all the info is coming from there

# rename to be Apidae specific
lslapida <- lsl

# add columns for Apidae
lslapida$Family <- "Apidae"
lslapida$TaxonCode <- "09400"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

apidaBiomass <- lslapida %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ apidaL$a,
                       lifeStage == "1" ~ apidaP$a,
                       lifeStage == "2" ~ apidaA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ apidaL$b,
                       lifeStage == "1" ~ apidaP$b,
                       lifeStage == "2" ~ apidaA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
apidaBiomassSum <- apidaBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))



# Cynipidae --------------------------------------------------------------------

###this is a gall wasp, in Jasmine's table the family name is just "x" so I am going to filter for all "x"s again and find the correct one once we get there :) 

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Cynipidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
cynip <- df1 %>% 
  filter(Family1 == "Cynipidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(cynip$a)
unique(cynip$a)
summary(cynip$b)
unique(cynip$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
cynipL <- cynip %>% 
  slice(42) %>% 
  filter(Stage == "A")
cynipP <- cynip %>% 
  slice(42) %>% 
  filter(Stage == "A")
cynipA <- cynip %>% 
  slice(42) %>% 
  filter(Stage == "A")

###the row with the only Cynipidae data set is row 42, and the only lifestage is A


# rename to be Cynipidae specific
lslcynip <- lsl

# add columns for Cynipidae
lslcynip$Family <- "Cynipidae"
lslcynip$TaxonCode <- "98001"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

cynipBiomass <- lslcynip %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ cynipL$a,
                       lifeStage == "1" ~ cynipP$a,
                       lifeStage == "2" ~ cynipA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ cynipL$b,
                       lifeStage == "1" ~ cynipP$b,
                       lifeStage == "2" ~ cynipA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
cynipBiomassSum <- cynipBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))





#Formicidae --------------------------------------------------------------------



df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Formicidae" ~ "Formicidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
formi <- df1 %>% 
  filter(Family1 == "Formicidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(formi$a)
unique(formi$a)
summary(formi$b)
unique(formi$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
formiL <- formi %>% 
  slice(1) %>% 
  filter(Stage == "A")
formiP <- formi %>% 
  slice(1) %>% 
  filter(Stage == "A")
formiA <- formi %>% 
  slice(1) %>% 
  filter(Stage == "A")

###again only one data data point for this information and it's lifestage is an A

# rename to be Formicidae specific
lslformi <- lsl

# add columns for Formicidae
lslformi$Family <- "Formicidae"
lslformi$TaxonCode <- "09100"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

formiBiomass <- lslformi %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ formiL$a,
                       lifeStage == "1" ~ formiP$a,
                       lifeStage == "2" ~ formiA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ formiL$b,
                       lifeStage == "1" ~ formiP$b,
                       lifeStage == "2" ~ formiA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
formiBiomassSum <- formiBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Scelionidae-------------------------------------------------------------------
### this is a subsection of parasitic wasps, their family name is also just put as "x" so I will filter out the "x" and use the exact parasitic wasp data point later on


df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Scelionidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
sceli <- df1 %>% 
  filter(Family1 == "Scelionidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(sceli$a)
unique(sceli$a)
summary(sceli$b)
unique(sceli$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
sceliL <- sceli %>% 
  slice(40) %>% 
  filter(Stage == "A")
sceliP <- sceli %>% 
  slice(40) %>% 
  filter(Stage == "A")
sceliA <- sceli %>% 
  slice(40) %>% 
  filter(Stage == "A")
###only one data point for this information, in row 40, and adult lifestage

# rename to be Scelionidae specific
lslsceli <- lsl

# add columns for Scelionidae
lslsceli$Family <- "Scelionidae"
lslsceli$TaxonCode <- "09500"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

sceliBiomass <- lslsceli %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ sceliL$a,
                       lifeStage == "1" ~ sceliP$a,
                       lifeStage == "2" ~ sceliA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ sceliL$b,
                       lifeStage == "1" ~ sceliP$b,
                       lifeStage == "2" ~ sceliA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
sceliBiomassSum <- sceliBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))



# Lepidoptera-------------------------------------------------------------------

### Lepidoptera are moths/butterflies. In Jasmine's table the majority of them are in the "x" 'family' but there are two familes named in this data set, the Pyralidae and the Crambidae

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Lepidoptera",
                             Family == "Pyralidae" ~ "Lepidoptera",
                             Family == "Crambidae" ~ "Lepidoptera",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
lepid <- df1 %>% 
  filter(Family1 == "Lepidoptera") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(lepid$a)
unique(lepid$a)
summary(lepid$b)
unique(lepid$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
lepidL <- lepid %>% 
  slice(49:51) %>% 
  filter(Stage == "L")
lepidP <- lepid %>% 
  slice(49:51) %>% 
  filter(Stage == "P")
lepidA <- lepid %>% 
  slice(49:51) %>% 
  filter(Stage == "A")

###the Lepidoptera where the higher class was Lepidoptera in the rows 49-51 so I filtered that area only, they also had all three lifestages!!


# rename to be Lepidoptera specific
lsllepid <- lsl

# add columns for Lepidoptera
lsllepid$Family <- "Lepidoptera"
lsllepid$TaxonCode <- "15000"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

lepidBiomass <- lsllepid %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ lepidL$a,
                       lifeStage == "1" ~ lepidP$a,
                       lifeStage == "2" ~ lepidA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ lepidL$b,
                       lifeStage == "1" ~ lepidP$b,
                       lifeStage == "2" ~ lepidA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
lepidBiomassSum <- lepidBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))

#Pyralidae  --------------------------------------------------------------------


df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Pyralidae" ~ "Pyralidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
pyral <- df1 %>% 
  filter(Family1 == "Pyralidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(pyral$a)
unique(pyral$a)
summary(pyral$b)
unique(pyral$b)
###two diff values for a, 2 for b

# filter out three rows for larva, pupa, adult, be sure to change the names again
pyralL <- pyral %>% 
  slice(1:3) %>% 
  filter(Stage == "L")
pyralP <- pyral %>% 
  slice(1:3) %>% 
  filter(Stage == "P")
pyralA <- pyral %>% 
  slice(1:3) %>% 
  filter(Stage == "A")


# rename to be Pyralidae specific
lslpyral <- lsl

# add columns for Pyralidae
lslpyral$Family <- "Pyralidae"
lslpyral$TaxonCode <- "15100"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

pyralBiomass <- lslpyral %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ pyralL$a,
                       lifeStage == "1" ~ pyralP$a,
                       lifeStage == "2" ~ pyralA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ pyralL$b,
                       lifeStage == "1" ~ pyralP$b,
                       lifeStage == "2" ~ pyralA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
pyralBiomassSum <- pyralBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))



# Orthoptera--------------------------------------------------------------------

###Orthoptera are crickets and such, in Jasmines table some of these are labled under "x" while some are more specific than the cramer table 

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "x" ~ "Orthoptera",
                             Family == "Acrididae" ~ "Orthoptera",
                             Family == "Gryllacrididae" ~ "Orthoptera",
                             Family == "Tridactylidae" ~ "Orthoptera",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
ortho <- df1 %>% 
  filter(Family1 == "Orthoptera") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(ortho$a)
unique(ortho$a)
summary(ortho$b)
unique(ortho$b)
# if only 2 values for "a" and 2 values for "b" where pupae and larvae have the same coefficients and adult is different

# filter out three rows for larva, pupa, adult, be sure to change the names again
orthoL <- ortho %>% 
  slice(56:57) %>% 
  filter(Stage == "L")
orthoP <- ortho %>% 
  slice(56:57) %>% 
  filter(Stage == "L")
orthoA <- ortho %>% 
  slice(56:57) %>% 
  filter(Stage == "A")

###no P for this data set, so am using L the P data 

# rename to be Orthoptera specific
lslortho <- lsl

# add columns for Orthoptera
lslortho$Family <- "Orthoptera"
lslortho$TaxonCode <- "10000"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

orthoBiomass <- lslortho %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ orthoL$a,
                       lifeStage == "1" ~ orthoP$a,
                       lifeStage == "2" ~ orthoA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ orthoL$b,
                       lifeStage == "1" ~ orthoP$b,
                       lifeStage == "2" ~ orthoA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
orthoBiomassSum <- orthoBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))



# Glossosomatidae---------------------------------------------------------------


df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Glossosomatidae" ~ "Glossosomatidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
gloss <- df1 %>% 
  filter(Family1 == "Glossosomatidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(gloss$a)
unique(gloss$a)
summary(gloss$b)
unique(gloss$b)
### two unique numbers from both a and b 

# filter out three rows for larva, pupa, adult, be sure to change the names again
glossL <- gloss %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
glossP <- gloss %>% 
  slice(1:2) %>% 
  filter(Stage == "P")
glossA <- gloss %>% 
  slice(5) %>% 
  filter(Stage == "A")

# rename to be Glossosomatidae specific
lslgloss <- lsl

# add columns for Glossosomatidae
lslgloss$Family <- "Glossosomatidae"
lslgloss$TaxonCode <- "02110"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

glossBiomass <- lslgloss %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ glossL$a,
                       lifeStage == "1" ~ glossP$a,
                       lifeStage == "2" ~ glossA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ glossL$b,
                       lifeStage == "1" ~ glossP$b,
                       lifeStage == "2" ~ glossA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
glossBiomassSum <- glossBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))


# Leptoceridae------------------------------------------------------------------


df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Leptoceridae" ~ "Leptoceridae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just Leptoceridae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
tocer <- df1 %>% 
  filter(Family1 == "Leptoceridae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(tocer$a)
unique(tocer$a)
summary(tocer$b)
unique(tocer$b)
### two unique numbers for a and b 


# filter out three rows for larva, pupa, adult, be sure to change the names again
tocerL <- tocer %>% 
  slice(1:2) %>% 
  filter(Stage == "L")
tocerP <- tocer %>% 
  slice(1:2) %>% 
  filter(Stage == "P")
tocerA <- tocer %>% 
  slice(14) %>% 
  filter(Stage == "A")

###the first two rows only consit of A and P, row 14 had an A lifestage, but it is a different taxon than the other two


# rename to be Leptoceridae specific
lsltocer <- lsl

# add columns for Leptoceridae
lsltocer$Family <- "Leptoceridae"
lsltocer$TaxonCode <- "02600"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

tocerBiomass <- lsltocer %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ tocerL$a,
                       lifeStage == "1" ~ tocerP$a,
                       lifeStage == "2" ~ tocerA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ tocerL$b,
                       lifeStage == "1" ~ tocerP$b,
                       lifeStage == "2" ~ tocerA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)


# summarize biomass 
tocerBiomassSum <- tocerBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))

#Planariidae--------------------------------------------------------------------

df1 <- df %>% 
  mutate(Family1 = case_when(Family == "Planariidae" ~ "Planariidae",
                             TRUE ~ Family)) %>% 
  select(Taxon, Stage, Order, higherClass_JS, Family1, a, b, Notes)

# create df of just Planariidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
plana <- df1 %>% 
  filter(Family1 == "Planariidae") %>% 
  mutate(mass_0.5mm = (a*0.5^b),
         mass_1mm = (a*1^b),
         mass_1.5mm = (a*1.5^b),
         mass_2mm = (a*2^b)) 

# check coefficient values, make sure you change the name in the parenthesis 
summary(plana$a)
unique(plana$a)
summary(plana$b)
unique(plana$b)
### only one unique a and b 

# filter out three rows for larva, pupa, adult, be sure to change the names again
### only one data point and one lifestage "U"
planaL <- plana %>% 
  slice(1) %>% 
  filter(Stage == "U")
planaP <- plana %>% 
  slice(1) %>% 
  filter(Stage == "U")
planaA <- plana %>% 
  slice(1) %>% 
  filter(Stage == "U")


# rename to be Planariidae specific
lslplana <- lsl

# add columns for Planariidae
lslplana$Family <- "Planariidae"
lslplana$TaxonCode <- "22100"

# make ConcCode, which is TaxonCode-LifeStage-SizeClass
# add columns with "a" and "b" coefficients assigned to the correct life stage
# calculate biomass

planaBiomass <- lslplana %>% 
  mutate(ConcCode = paste(TaxonCode, lifeStage, sizeClass, sep = "-")) %>% 
  mutate(a = case_when(lifeStage == "0" ~ planaL$a,
                       lifeStage == "1" ~ planaP$a,
                       lifeStage == "2" ~ planaA$a)) %>% 
  mutate(b = case_when(lifeStage == "0" ~ planaL$b,
                       lifeStage == "1" ~ planaP$b,
                       lifeStage == "2" ~ planaA$b)) %>% 
  mutate(biomass_mg = a * length_mm ^ b)
# getting and error on the last line of code above, says object'a' is not found, unable to continue for now 

# summarize biomass 
planaBiomassSum <- planaBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))










# --------------------------------------------------------------------



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

# create df of just hydroptilidae, !make sure you give it a new name!
# check biomass of different lengths using length-mass equation: M = a * L^b
BLANK <- df1 %>% 
  filter(Family1 == "BLANK") %>% 
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
  slice(1:3) %>% 
  filter(Stage == "L")
hydroP <- hydro %>% 
  slice(1:3) %>% 
  filter(Stage == "P")
hydroA <- hydro %>% 
  slice(1:3) %>% 
  filter(Stage == "A")


# rename to be Hydroptilidae specific
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
hydroBiomassSum <- hydroBiomass %>% 
  group_by(ConcCode, Family) %>% 
  summarize(meanBio_mg = mean(biomass_mg), 
            minBio_mg = min(biomass_mg),
            medBio_mg = median(biomass_mg),
            maxBio_mg = max(biomass_mg))
