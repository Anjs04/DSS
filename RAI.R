# Packages: ---------------------------------------------------------------------------------
# List of packages for session
.packages = c("dplyr", "stringr", "tidyr", "lazyeval", "readr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Import data: ------------------------------------------------------------------------------

RAI = read.csv("Input\\RAI_2016Q4.csv", stringsAsFactors = FALSE, na.strings = c("", " "), strip.white = TRUE)
DSS_PC = read.csv("Input\\DSS_PC_Dec2016.csv", stringsAsFactors = FALSE, na.strings = c("", " "), strip.white = TRUE)
DSS_LGA = read.csv("Input\\DSS_LGA_Dec2016.csv", stringsAsFactors = FALSE, na.strings = c("", " "), strip.white = TRUE)
# Also will need simple income assumptions and number of households receiving that income

# Join RAI and DSS data: -------------------------------------------------------------------
# Split RAI data by whether postcode or LGA given
# Functions for testing whether each element contains only characters or numbers
is.letter <- function(x) grepl("[[:alpha:]]", x)
is.number <- function(x) grepl("[[:digit:]]", x)

#Apply to RAI
RAI_PC = RAI[is.number(RAI$Postcode), ]
RAI_LGA = RAI[is.letter(RAI$Postcode),]

#Test size is same
dim(RAI_PC)[1] + dim(RAI_LGA)[1] == dim(RAI)[1]

# Join on Postcode, all RAI data, not all DSS postcode data
RAI_PC_join = 
  left_join(
    RAI_PC,
    DSS_PC,
    by = c("Postcode" = "Postcode")
  )

# Join on LGA, all RAI data, not all DSS LGA data
# First strip the name out of LGA.name
DSS_LGA =
  DSS_LGA %>%
  separate(LGA.name, into = c("Name", "Junk"), sep = " \\(", 2) %>%
  select(-Junk)

# Places not joined
#RAI
c(
  "CoomaMonaro", #CoomaMonaro has data, Cooma-Monaro does not
  "Hastings",
  "Lithgow City",
  "MidWestern Regional", #MidWestern Regional has data, Mid-Western Regional does not
  "Port MacquarieHastings",
  "Tumut",
  "Unincorporated",
  "Upper Lachlan"
)

#DSS
c(
  "Cooma-Monaro",
  "No-Match", # Port Macquarie-Hastings
  "No-Match", # Lithgow
  "Mid-Western Regional",
  "Port Macquarie-Hastings",
  "No-Match", # Tumut Shire
  "No-Match", # Unincorporated by state
  "Upper Lachlan Shire"
)

# Edit names in DSS_LGA files
DSS_LGA$Name_Match = 
  ifelse(
    DSS_LGA$Name == "Cooma-Monaro", "CoomaMonaro",
    ifelse(
      DSS_LGA$Name == "Mid-Western Regional", "MidWestern Regional",
      ifelse(
        DSS_LGA$Name == "Port Macquarie-Hastings", "Port MacquarieHastings",
        ifelse(
          DSS_LGA$Name == "Upper Lachlan Shire", "Upper Lachlan",
          ifelse(
            DSS_LGA$Name == "Unincorporated NSW", "Unincorporated",
            DSS_LGA$Name
          )
        )
      )
    )
  )

# Now join
RAI_LGA_join =
  left_join(
    RAI_LGA, 
    DSS_LGA,
    by = c("Postcode" = "Name_Match")
  ) %>%
  select(-LGA)

# Stitch together RAI
RAI = bind_rows(RAI_PC_join, RAI_LGA_join)

# Check unmatched values:
View(table(RAI[is.na(RAI$Age.Pension), "Postcode"]))

# Check if they have actual data (x2016_4 more than 0)
View(RAI[is.na(RAI$Age.Pension), ])

# Note: No Double Orphan Pension data by LGA exists

# Manual summary, before function: ---------------------------------------------------------
# RAI-style income assumptions for data above
single = 25000 #26540.8
couple = 45000 #44558.8

# Then find out how to use the RAI to calculate how many are in housing stress
stress_single = 
RAI %>%
  mutate(Age.Pension.Numeric = gsub(",", "", Age.Pension),
         Age.Pension.Numeric = gsub("<", "", Age.Pension.Numeric), #Overestimating for under 5 postcodes
         Age.Pension.Numeric = as.numeric(Age.Pension.Numeric),
         Stress_6 = ifelse(Bedrooms == 1 & Income == single & X2016_4 < 50, Age.Pension.Numeric, 0),
         Stress_5 = ifelse(Bedrooms == 1 & Income == single & X2016_4 < 80 & X2016_4 > 50, Age.Pension.Numeric, 0),
         Stress_4 = ifelse(Bedrooms == 1 & Income == single & X2016_4 < 100 & X2016_4 > 80, Age.Pension.Numeric, 0),
         Stress_3 = ifelse(Bedrooms == 1 & Income == single & X2016_4 < 120 & X2016_4 > 100, Age.Pension.Numeric, 0),
         Stress_2 = ifelse(Bedrooms == 1 & Income == single & X2016_4 < 150 & X2016_4 > 120, Age.Pension.Numeric, 0),
         Stress_1 = ifelse(Bedrooms == 1 & Income == single & X2016_4 > 150, Age.Pension.Numeric, 0)
         )

# For couple
stress_couple = 
  RAI %>%
  mutate(Age.Pension.Numeric = gsub(",", "", Age.Pension),
         Age.Pension.Numeric = gsub("<", "", Age.Pension.Numeric), #Overestimating for under 5 postcodes
         Age.Pension.Numeric = as.numeric(Age.Pension.Numeric),
         Stress_6 = ifelse(Bedrooms == 1 & Income == couple & X2016_4 < 50, Age.Pension.Numeric, 0),
         Stress_5 = ifelse(Bedrooms == 1 & Income == couple & X2016_4 < 80 & X2016_4 > 50, Age.Pension.Numeric, 0),
         Stress_4 = ifelse(Bedrooms == 1 & Income == couple & X2016_4 < 100 & X2016_4 > 80, Age.Pension.Numeric, 0),
         Stress_3 = ifelse(Bedrooms == 1 & Income == couple & X2016_4 < 120 & X2016_4 > 100, Age.Pension.Numeric, 0),
         Stress_2 = ifelse(Bedrooms == 1 & Income == couple & X2016_4 < 150 & X2016_4 > 120, Age.Pension.Numeric, 0),
         Stress_1 = ifelse(Bedrooms == 1 & Income == couple & X2016_4 > 150, Age.Pension.Numeric, 0)
  )

# Convert and check Age.Pension to numeric
write.csv(
  cbind(
    stress_single$Postcode,
    stress_single$Age.Pension.Numeric, 
    stress_single$Age.Pension
  ),
  "Working/Summing.csv"
)

# Sum stress vars
# Note, this calculates the amount of pensioners overall
#...if they were at the single income, or the couple income
#...it doesnt contain info on how many singles/cuople at postcode level.

single_var =
  c(
sum(stress_single$Stress_6, na.rm = TRUE), #Extremely Unaffordable
sum(stress_single$Stress_5, na.rm = TRUE), #Severely Unaffordable
sum(stress_single$Stress_4, na.rm = TRUE), #Unaffordable
sum(stress_single$Stress_3, na.rm = TRUE), #Moderately Unaffordable
sum(stress_single$Stress_2, na.rm = TRUE), #Acceptable
sum(stress_single$Stress_1, na.rm = TRUE)  #Affordable
)

age.Pension = 2570072 #Real, from original data, knowing the actual under 5 values, including 88620 people with unknown postcodes

couple_var =
  c(
sum(stress_couple$Stress_6, na.rm = TRUE), #Extremely Unaffordable
sum(stress_couple$Stress_5, na.rm = TRUE), #Severely Unaffordable
sum(stress_couple$Stress_4, na.rm = TRUE), #Unaffordable
sum(stress_couple$Stress_3, na.rm = TRUE), #Moderately Unaffordable
sum(stress_couple$Stress_2, na.rm = TRUE), #Acceptable
sum(stress_couple$Stress_1, na.rm = TRUE)  #Affordable
)

# In data.frame to output
output =
  data.frame(
    SingleIncome  = round(single_var/2), #Assume 50/50 split of single to couple households
    CoupleIncome  = round(couple_var/2),
    PercentSingle = paste(round(single_var/age.pension*100), "%", sep = ""), #As a percentage of the total
    PercentCouple = paste(round(couple_var/age.pension*100), "%", sep = "")
  )

output = cbind(RAI_meaning, output)
write_csv(output, "Working/Counts_using_public_data.csv")

# After function - Calculate number on other pensions who are in housing stress: --------------------------
# Write above as a reusable function
stress = function(income, payment) { 
  
  bed =
    ifelse(
      deparse(substitute(income)) == "single", 1, 
      ifelse(
        deparse(substitute(income)) == "couple", 2, 0
      )
    )
  print(bed)
  
  RAI %>%
   filter(STE == "NSW") %>%
   mutate(Numeric = str_replace(payment, ",", ""),
           Numeric = str_replace(Numeric, "<", ""), #Overestimating for under 5 postcodes
           Numeric = as.numeric(Numeric),
           Stress_6 = ifelse(Bedrooms == bed & Income == income & X2016_4 < 50 & X2016_4 > 0, Numeric, 0),
           Stress_5 = ifelse(Bedrooms == bed & Income == income & X2016_4 < 80 & X2016_4 > 50, Numeric, 0),
           Stress_4 = ifelse(Bedrooms == bed & Income == income & X2016_4 < 100 & X2016_4 > 80, Numeric, 0),
           Stress_3 = ifelse(Bedrooms == bed & Income == income & X2016_4 < 120 & X2016_4 > 100, Numeric, 0),
           Stress_2 = ifelse(Bedrooms == bed & Income == income & X2016_4 < 150 & X2016_4 > 120, Numeric, 0),
           Stress_1 = ifelse(Bedrooms == bed & Income == income & X2016_4 > 150, Numeric, 0),
           No_Index = ifelse(Bedrooms == bed & Income == income &
                               !(X2016_4 < 50 & X2016_4 > 0) &
                               !(X2016_4 < 80 & X2016_4 > 50) &
                               !(X2016_4 < 100 & X2016_4 > 80) &
                               !(X2016_4 < 120 & X2016_4 > 100) &
                               !(X2016_4 < 150 & X2016_4 > 120) &
                               !(X2016_4 > 150), Numeric, 0
                             )
    )
}

# Test output
View(
  stress(
    single, RAI %>% filter(STE == "NSW") %>% select(Age.Pension)
  )
)
View(
  stress(
    couple, RAI$Age.Pension
  )
)

# Function for table of sums
stress_sum = function(stress, total) {

    var =
    c(
      sum(stress$Stress_6, na.rm = TRUE), #Extremely Unaffordable
      sum(stress$Stress_5, na.rm = TRUE), #Severely Unaffordable
      sum(stress$Stress_4, na.rm = TRUE), #Unaffordable
      sum(stress$Stress_3, na.rm = TRUE), #Moderately Unaffordable
      sum(stress$Stress_2, na.rm = TRUE), #Acceptable
      sum(stress$Stress_1, na.rm = TRUE), #Affordable
      sum(stress$No_Index, na.rm = TRUE)  #Not in any index bracket above
    )
    
      x = data.frame(
          Recipients = round(var), 
          Percent = round(var/sum(var)*100)
        )
      names(x) = c(deparse(substitute(total)),
                   paste(deparse(substitute(total)), "Percent", sep = "_")
      )
      return(x)
}

# Test for age pension
# National counts
stress_sum(stress(single, RAI$Age.Pension), age.pension)
stress_sum(stress(couple, RAI$Age.Pension), age.pension)

# NSW counts
stress_sum(stress(single, RAI$Age.Pension) %>% filter(STE == "NSW"), age.pension)
stress_sum(stress(couple, RAI$Age.Pension) %>% filter(STE == "NSW"), age.pension)

# Now find all the different incomes for all the different payment types
age.pension = 851882
wifepension_partneragepension = 5490 #income same as age pension
wifepension_partnerdisability = 5326 #income same as disability
widowBpension = 376  #income same as age pension
pensionerconcessioncard = 4151075 #Non-specific income, various discounts
disability = 772313 #income same as age pension

singleCSHC = 50000 #52796
coupleCSHC = 85000 #84472
totalCSHC = 285959
# Note: "CSHC holders are a particularly wealthy population": http://ro.uow.edu.au/cgi/viewcontent.cgi?article=1654&context=commpapers

glimpse(stress(singleCSHC, RAI$Commonwealth.Seniors.Health.Card))
stress_sum(stress(singleCSHC, RAI$Commonwealth.Seniors.Health.Card), totalCSHC)

# Since basic payment rates are the same on the DHS website, keep single and couple as they are
output_singlepensioners =
  cbind( 
    stress_sum(
      stress(single, RAI$Age.Pension),
      age.pension
    ),
    # No such thing as single wife pension
    # stress_sum(
    #   stress(single, RAI$Wife.Pension..Partner.on.Age.Pension.),
    #   wifepension_partneragepension
    # ),
    # stress_sum(
    #   stress(single, RAI$Wife.Pension..Partner.on.Disability.Support.Pension.),
    #   wifepension_partnerdisability
    # ),
    stress_sum(
      stress(single, RAI$Widow.B.Pension),
      widowBpension
    ),
    stress_sum(
      stress(single, RAI$Disability.Support.Pension),
      disability
    )
  ) #End of cbind()

output_couplepensioners =
  cbind(
    stress_sum(
      stress(couple, RAI$Age.Pension),
      age.pension
    ),
    stress_sum(
      stress(couple, RAI$Wife.Pension..Partner.on.Age.Pension.), #Counts assuming 50/50 split of single/couple
      wifepension_partneragepension
    ),
    stress_sum(
      stress(couple, RAI$Wife.Pension..Partner.on.Disability.Support.Pension.),
      wifepension_partnerdisability
    ),
    # No such thing as widow couple
    # stress_sum(
    #   stress(couple, RAI$Widow.B.Pension),
    #   widowBpension
    # ),
    stress_sum(
      stress(couple, RAI$Disability.Support.Pension),
      disability
    )
  )

names(output_singlepensioners) = paste(names(output_singlepensioners), "Single", sep = "_")
names(output_couplepensioners) = paste(names(output_couplepensioners), "Couple", sep = "_")

output_allpensioners =
  cbind(
    RAI_meaning,
    output_singlepensioners,
    output_couplepensioners
  )

View(output_allpensioners)

write_csv(output_allpensioners, "Working/Percent_in_Housing_Stress_AUS.csv")

# Checking widow B penison numbers (smallest/easiest)
View(
  stress(single, RAI$Widow.B.Pension) %>%
    filter(No_Index > 0)
)
# We can see that 30 (actually imputed to be 6) widow B pensioners are in areas with no RAI
# Working as intended, RAI data is incomplete.

# Calculate the cost of rent assistance for each grade cumulatively--------------
# - Income is 25000 single 45000 couple regardless of payment type
cra_max_single = 132.20
cra_max_couple = 124.60 # Covers both, as in 62.30 each
budget = 25000000 #$25M previous, has been lost

output_budget = output_allpensioners

#This halves all the counts of people, for the 50/50 split of single/pensioner
#Then halves the count for couples, to count the households
output_budget = 
output_budget %>%
  mutate_at(
    vars(
         -Index_Score,
         -Income_Share,
         -Relative_Unaffordability,
         -contains("Percent")
    ),
    funs(./2)
  ) %>%
  mutate_at(
    vars(
      contains("Couple"),
      -contains("Percent")
    ),
    funs(./2) #Overall divided by 4, converting no. of individuals to households
  )

View(output_budget)

#This converts the counts of people to dollars in rent assistance
cost = 
  output_budget %>%
  mutate_at(
    vars(contains("Single"),
         -contains("Percent")),
    funs(.*cra_max_single)
  ) %>%
  mutate_at(
    vars(contains("Couple"),
         -contains("Percent")),
    funs(.*cra_max_couple)
  )

View(cost)

cost_cumulative = 
  cost %>%
  mutate_at(
    vars(
      -Index_Score,
      -Income_Share,
      -Relative_Unaffordability,
      -contains("Percent")
    ),
    funs(cumsum)    
  )

View(cost_cumulative)

# Calculate the budget with a more detailed estimate of cra
cra_formula = function(income, index) {
  
  # All values are fortnightly payments and expenses
  rent = (income*index)/26
  print(paste("rent:", rent))
  
  rent_min =
    ifelse(
      deparse(substitute(income)) == "single", 117.8, 
      ifelse(
        deparse(substitute(income)) == "couple", 191, 0
      )
    )
  print(paste("rent_min:", rent_min))
  
  cra = (rent - rent_min)*0.75
  print(paste("cra:", cra))
  
  cra_max = ifelse(
    deparse(substitute(income)) == "single", 132.2,
    ifelse(
      deparse(substitute(income)) == "couple", 124.6, 0
    )
  )
  print(paste("cra_max:", cra_max))
  
  cra = ifelse(cra > cra_max, cra_max, cra)
  print(paste("cra:", cra))
  
  return(cra)
  
}

cra_formula(single, 0.3)
cra_formula(couple, 0.3)

# Create summary output just for NSW counts ------------------------------------
output_singlepensioners_NSW =
  cbind( 
    stress_sum(
      stress(single, RAI$Age.Pension) %>% filter(STE == "NSW"),
      age.pension
    ),
    stress_sum(
      stress(single, RAI$Widow.B.Pension) %>% filter(STE == "NSW"),
      widowBpension
    ),
    stress_sum(
      stress(single, RAI$Disability.Support.Pension) %>% filter(STE == "NSW"),
      disability
    )
  )

output_couplepensioners_NSW =
  cbind(
    stress_sum(
      stress(couple, RAI$Age.Pension) %>% filter(STE == "NSW"),
      age.pension
    ),
    stress_sum(
      stress(couple, RAI$Wife.Pension..Partner.on.Age.Pension.) %>% filter(STE == "NSW"),
      wifepension_partneragepension
    ),
    stress_sum(
      stress(couple, RAI$Wife.Pension..Partner.on.Disability.Support.Pension.) %>% filter(STE == "NSW"),
      wifepension_partnerdisability
    ),
    stress_sum(
      stress(couple, RAI$Disability.Support.Pension) %>% filter(STE == "NSW"),
      disability
    )
  )

names(output_singlepensioners_NSW) = paste(names(output_singlepensioners_NSW), "Single", sep = "_")
names(output_couplepensioners_NSW) = paste(names(output_couplepensioners_NSW), "Couple", sep = "_")

output_allpensioners_NSW =
  cbind(
    RAI_meaning,
    output_singlepensioners_NSW,
    output_couplepensioners_NSW
  )

View(output_allpensioners_NSW)

write_csv(output_allpensioners_NSW, "Working/Percent_in_Housing_Stress_NSW.csv")

# Convert to number of households for NSW data-----------------------------------

output_budget_NSW = output_allpensioners_NSW
output_budget_NSW = 
  output_budget_NSW %>%
  mutate_at(
    vars(
      -Index_Score,
      -Income_Share,
      -Relative_Unaffordability,
      -contains("Percent")
    ),
    funs(./2)
  ) %>%
  mutate_at(
    vars(
      contains("Couple"),
      -contains("Percent")
    ),
    funs(./2) #Overall divided by 4, converting no. of individuals to households
  )

View(output_budget_NSW)

#This converts the counts of people to dollars in rent assistance
cost_NSW = 
  output_budget_NSW %>%
  mutate_at(
    vars(contains("Single"),
         -contains("Percent")),
    funs(.*cra_max_single)
  ) %>%
  mutate_at(
    vars(contains("Couple"),
         -contains("Percent")),
    funs(.*cra_max_couple)
  )

View(cost_NSW)

cost_cumulative_NSW = 
  cost_NSW %>%
  mutate_at(
    vars(
      -Index_Score,
      -Income_Share,
      -Relative_Unaffordability,
      -contains("Percent")
    ),
    funs(cumsum)    
  )

View(cost_cumulative_NSW)

#Further info needed-------------------------------------------------------------
# EXCLUSIVE TO RENTERS RECEIVING RENT ASSISTANCE:
# - 2016 breakdown of single / couple for each payment type (2007 sheet in excel)
# - Average income for each payment type
# - By postcode, single/couple counts for each payment type

#Predicting the number of penioners owning their home----------------------------------------
#Important, because 75% of age pensioners owned their home in June 2016
#Therefore, the number will change significantly in Dec 2016, whereas social housing numbers will not.
home_lm = function() {

  newyear = 16.5
  
  print("Disability Pension")
  
  disability = data.frame(
    year = c(12, 13, 14, 15, 16),
    home = c(241002, 230856, 228069, 216007, 201307),
    none = c(586458, 590882, 602385, 598384, 581584)
  )
  
  print(
  predict(
    lm(home ~ year, disability), 
    data.frame(year = newyear)
  )
  )
  
  print(
  predict(
    lm(none ~ year, disability), 
    data.frame(year = newyear) # Quarter 4 2016 numbers
  )
  )
  
  print("Age Pension")
  
  age = data.frame(
    year = c(12, 13, 14, 15, 16),
    home = c(1703543, 1766926, 1807999, 1865649, 1898605),
    none = c(574672, 585213, 596903, 620546, 639556)
  )
  
  print(
    predict(
      lm(home ~ year, age),
      data.frame(year = newyear)
    )
  )
  
  print(
    predict(
      lm(none ~ year, age),
      data.frame(year = newyear)
    )
  )
  
}

home_lm()
#Age total is so close to actual number.
2570072*(1930756/(1930756+644653.2 ))
#Disability is off by 10-20 thousand.
#Adjust:
772313*(199888.5/(199888.5 + 591377.1))

#Percent of Total
#Age
(1930756/(1930756+644653.2 ))
#DSP
(1930756/(1930756+644653.2 ))

#Adjust counts to cover all pensioners with unknown locations----------------------------------
adjustment = function() {
  
  #Equivalent to auto-summing in excel as a new row at the end
  totals_indexed =
  output_allpensioners_NSW %>%
    filter(!Index_Score == "0") %>%
    summarise_if(
      is.numeric,
      sum
    ) %>%
    bind_rows(output_allpensioners_NSW[-dim(output_allpensioners_NSW),], .)

  #Without an attached total row, find percentage/proportion of each column
  percentages =
  output_allpensioners_NSW %>%
    filter(!Index_Score == "0") %>%
    mutate_at(
      vars(contains("Percent")),
      prop.table
    )
  
  #Totals for NSW, including all unknown locations
  age.pension = 851882
  wifepension_partneragepension = 5490 #income same as age pension
  wifepension_partnerdisability = 5326 #income same as disability
  widowBpension = 376  #income same as age pension
  disability = 772313 #income same as age pension
  
  #Apply new percentages to total for NSW
  #Manually, for each pair of columns (with more time, this would be better done using lazyeval package, interp() function)
  percentages$age.pension_Single = 
    percentages$age.pension_Percent_Single * age.pension
  
  percentages$widowBpension_Single =
    percentages$widowBpension_Percent_Single * widowBpension
  
  percentages$disability_Single =
    percentages$disability_Percent_Single * disability
  
  percentages$age.pension_Couple = 
    percentages$age.pension_Percent_Couple * age.pension
  
  percentages$wifepension_partneragepension_Couple =
    percentages$wifepension_partneragepension_Percent_Couple * wifepension_partneragepension
  
  percentages$wifepension_partnerdisability_Couple =
    percentages$wifepension_partnerdisability_Percent_Couple * wifepension_partnerdisability
  
  percentages$disability_Couple =
    percentages$disability_Percent_Couple * disability
  
  View(percentages)
  
  }

adjustment()

# Calculate simple budget

percentages_budget = function() {
  
  #Single/Couple rations sourced from draft report
  x =
    cbind(
      age.pension_budget = 
      data.frame(
        percentages$age.pension_Single * 0.45 * 132.2 +
        percentages$age.pension_Couple * 0.55 * 124.6
      ) * (1 - 0.0003) * (1 - 0.75),
      disability_budget = 
      data.frame(
        percentages$disability_Single * 0.24 * 132.2 +
        percentages$disability_Couple * 0.76 * 124.6
      ) * (1 - 0.0012) * (1 - 0.25),
      wifepension_partneragepension_budget = 
      data.frame(
        percentages$wifepension_partneragepension_Couple * 124.6
      ) * (1 - 0.0003) * (1 - 0.75),
      wifepension_partnerdisability_budget =
      data.frame(
        percentages$wifepension_partnerdisability_Couple * 124.6
      ) * (1 - 0.0012) * (1 - 0.25)
    )
  
  names(x) = c("Age.Pension",
               "Disability.Support.Pension",
               "Wife.Pension_Age.Pension",
               "Wife.Pension_Disability.Support.Pension"
               )
  
  View(x)
  
  write.table(x, "clipboard", sep="\t", row.names = FALSE)

  y = 
    x %>%
      mutate_all(funs(cumsum))
  
  write.table(y, "clipboard", sep="\t", row.names = FALSE)
  
}

percentages_budget()

# What if the incomes increase by $5000? ----------------------------------------------------------------------------------------

# Function for making output_allpensioners
stress_sum_income = function(single, couple) {
  output_singlepensioners_NSW =
    cbind( 
      stress_sum(
        stress(single, RAI$Age.Pension) %>% filter(STE == "NSW"),
        age.pension
      ),
      stress_sum(
        stress(single, RAI$Widow.B.Pension) %>% filter(STE == "NSW"),
        widowBpension
      ),
      stress_sum(
        stress(single, RAI$Disability.Support.Pension) %>% filter(STE == "NSW"),
        disability
      )
    )
  
  output_couplepensioners_NSW =
    cbind(
      stress_sum(
        stress(couple, RAI$Age.Pension) %>% filter(STE == "NSW"),
        age.pension
      ),
      stress_sum(
        stress(couple, RAI$Wife.Pension..Partner.on.Age.Pension.) %>% filter(STE == "NSW"),
        wifepension_partneragepension
      ),
      stress_sum(
        stress(couple, RAI$Wife.Pension..Partner.on.Disability.Support.Pension.) %>% filter(STE == "NSW"),
        wifepension_partnerdisability
      ),
      stress_sum(
        stress(couple, RAI$Disability.Support.Pension) %>% filter(STE == "NSW"),
        disability
      )
    )
  
  names(output_singlepensioners_NSW) = paste(names(output_singlepensioners_NSW), "Single", sep = "_")
  names(output_couplepensioners_NSW) = paste(names(output_couplepensioners_NSW), "Couple", sep = "_")
  
  output_allpensioners_NSW =
    cbind(
      RAI_meaning,
      output_singlepensioners_NSW,
      output_couplepensioners_NSW
    )
  
  return(output_allpensioners_NSW)
  
}

# First reset the single and couple incomes
single = 30000
couple = 50000

# Run the function
output_5000 = stress_sum_income(single, couple)

# Reset
single = 35000
couple = 55000

output_10000 = stress_sum_income(single, couple)

single = 40000
couple = 60000

output_15000 = stress_sum_income(single, couple)

single = 45000
couple = 65000

output_20000 = stress_sum_income(single, couple)

# Adjust to cover the non-indexed values
adjustment_income = function(output_allpensioners_NSW) {
  
  #Equivalent to auto-summing in excel as a new row at the end
  totals_indexed =
    output_allpensioners_NSW %>%
    filter(!Index_Score == "0") %>%
    summarise_if(
      is.numeric,
      sum
    ) %>%
    bind_rows(output_allpensioners_NSW[-dim(output_allpensioners_NSW),], .)
  
  #Without an attached total row, find percentage/proportion of each column
  percentages =
    output_allpensioners_NSW %>%
    filter(!Index_Score == "0") %>%
    mutate_at(
      vars(contains("Percent")),
      prop.table
    )
  
  #Totals for NSW, including all unknown locations
  age.pension = 851882
  wifepension_partneragepension = 5490 #income same as age pension
  wifepension_partnerdisability = 5326 #income same as disability
  widowBpension = 376  #income same as age pension
  disability = 772313 #income same as age pension
  
  #Apply new percentages to total for NSW
  #Manually, for each pair of columns (with more time, this would be better done using lazyeval package, interp() function)
  percentages$age.pension_Single = 
    percentages$age.pension_Percent_Single * age.pension
  
  percentages$widowBpension_Single =
    percentages$widowBpension_Percent_Single * widowBpension
  
  percentages$disability_Single =
    percentages$disability_Percent_Single * disability
  
  percentages$age.pension_Couple = 
    percentages$age.pension_Percent_Couple * age.pension
  
  percentages$wifepension_partneragepension_Couple =
    percentages$wifepension_partneragepension_Percent_Couple * wifepension_partneragepension
  
  percentages$wifepension_partnerdisability_Couple =
    percentages$wifepension_partnerdisability_Percent_Couple * wifepension_partnerdisability
  
  percentages$disability_Couple =
    percentages$disability_Percent_Couple * disability
  
  return(percentages)
  
}

adjustment_income(output_5000)
adjustment_income(output_10000)
adjustment_income(output_15000)
adjustment_income(output_20000)

# Calculate the budget for each of the incomes
percentages_budget_income = function(percentages) {
  
  #Single/Couple rations sourced from draft report
  x =
    cbind(
      age.pension_budget = 
        data.frame(
          percentages$age.pension_Single * 0.45 * 132.2 +
            percentages$age.pension_Couple * 0.55 * 124.6
        ) * (1 - 0.0003) * (1 - 0.75),
      disability_budget = 
        data.frame(
          percentages$disability_Single * 0.24 * 132.2 +
            percentages$disability_Couple * 0.76 * 124.6
        ) * (1 - 0.0012) * (1 - 0.25),
      wifepension_partneragepension_budget = 
        data.frame(
          percentages$wifepension_partneragepension_Couple * 124.6
        ) * (1 - 0.0003) * (1 - 0.75),
      wifepension_partnerdisability_budget =
        data.frame(
          percentages$wifepension_partnerdisability_Couple * 124.6
        ) * (1 - 0.0012) * (1 - 0.25)
    )
  
  names(x) = c("Age.Pension",
               "Disability.Support.Pension",
               "Wife.Pension_Age.Pension",
               "Wife.Pension_Disability.Support.Pension"
  )
  
  View(x)
  
  write.table(x, "clipboard", sep="\t", row.names = FALSE)
  
  y = 
    x %>%
    mutate_all(funs(cumsum))
  
  write.table(y, "clipboard", sep="\t", row.names = FALSE)
  
}

percentages_budget_income(adjustment_income(output_5000))

