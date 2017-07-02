# Packages: ---------------------------------------------------------------------------------
# List of packages for session
.packages = c("dplyr", "stringr", "tidyr", "lazyeval", "readr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Functions: ---------------------------------------------------------------------------------
# RAI-style income assumptions, to nearest $5,000 to match RAI incomes
single = 25000 #26540.8
couple = 45000 #44558.8

# Assumptions for proportion of renters
DSP_renter = 0.155
AGE_renter = 0.156

# Totals for each payment type for NSW
age.pension = 816414
wifepension_partneragepension = 1888
wifepension_partnerdisability = 1616
widowBpension = 6
disability = 249506

#Calculate number of  pensions who are in housing stress: ---------------------------------------
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

# Function for table of sums by level of housing stress
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

# Counts for all Australia
output_singlepensioners =
  cbind( 
    stress_sum(
      stress(single, RAI$Age.Pension),
      age.pension
    ),
    stress_sum(
      stress(single, RAI$Widow.B.Pension),
      widowBpension
    ),
    stress_sum(
      stress(single, RAI$Disability.Support.Pension),
      disability
    )
  )

output_couplepensioners =
  cbind(
    stress_sum(
      stress(couple, RAI$Age.Pension),
      age.pension
    ),
    stress_sum(
      stress(couple, RAI$Wife.Pension..Partner.on.Age.Pension.),
      wifepension_partneragepension
    ),
    stress_sum(
      stress(couple, RAI$Wife.Pension..Partner.on.Disability.Support.Pension.),
      wifepension_partnerdisability
    ),
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

# Calculate the cost of rent assistance for each grade cumulatively----------------------------
# - Income is 25000 single 45000 couple regardless of payment type
cra_max_single = 132.20
cra_max_couple = 124.60

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

#Predicting the number of pensioners owning their home----------------------------------------
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
2570072*(1930756/(1930756+644653.2))
2570072*(644653.2/(1930756+644653.2))
#Disability is off by 10-20 thousand.
#Adjust:
772313*(199888.5/(199888.5 + 591377.1))
772313*(591377.1/(199888.5 + 591377.1))

#Percent of Total
#Age
(1930756/(1930756+644653.2 ))
#DSP
(644653.2/(1930756+644653.2 ))

#Adjust counts to cover all pensioners with unknown locations----------------------------------
adjustment = function() {
  
  #Without an attached unknown index row, find percentage/proportion of each column
  percentages =
    output_allpensioners_NSW %>%
    filter(!Index_Score == "0") %>%
    mutate_at(
      vars(contains("Percent")),
      prop.table
    )
  
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

# Calculate simple budget

percentages_budget = function(percentages) {
  
  #Single/Couple ratios sourced from draft report
  x =
    cbind(
      age.pension_budget = 
        data.frame(
          percentages$age.pension_Single * 0.45 * 132.2 +
            percentages$age.pension_Couple * 0.55 * 124.6
        ) * AGE_renter,
      disability_budget = 
        data.frame(
          percentages$disability_Single * 0.76 * 132.2 +
            percentages$disability_Couple * 0.24 * 124.6
        ) * DSP_renter ,
      wifepension_partneragepension_budget = 
        data.frame(
          percentages$wifepension_partneragepension_Couple * 124.6
        ) * AGE_renter,
      wifepension_partnerdisability_budget =
        data.frame(
          percentages$wifepension_partnerdisability_Couple * 124.6
        ) * DSP_renter
    )
  
  names(x) = c("Age.Pension",
               "Disability.Support.Pension",
               "Wife.Pension_Age.Pension",
               "Wife.Pension_Disability.Support.Pension"
  )
  
  y = 
    x %>%
    mutate_all(funs(cumsum))
  
  return(
    list(x, y)
  )
  
}

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
single = 25000
couple = 45000

output_0 = stress_sum_income(single, couple)

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
  
  #Without an attached total row, find percentage/proportion of each column
  percentages =
    output_allpensioners_NSW %>%
    filter(!Index_Score == "0") %>%
    mutate_at(
      vars(contains("Percent")),
      prop.table
    )
  
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
        ) * AGE_renter,
      disability_budget = 
        data.frame(
          percentages$disability_Single * 0.24 * 132.2 +
            percentages$disability_Couple * 0.76 * 124.6
        )  * DSP_renter,
      wifepension_partneragepension_budget = 
        data.frame(
          percentages$wifepension_partneragepension_Couple * 124.6
        ) * AGE_renter,
      wifepension_partnerdisability_budget =
        data.frame(
          percentages$wifepension_partnerdisability_Couple * 124.6
        )  * DSP_renter
    )
  
  names(x) = c("Age.Pension",
               "Disability.Support.Pension",
               "Wife.Pension_Age.Pension",
               "Wife.Pension_Disability.Support.Pension"
  )
  
  return(x)
  
}

percentages_budget_income(adjustment_income(output_5000))
percentages_budget_income(adjustment_income(output_10000))
percentages_budget_income(adjustment_income(output_15000))
percentages_budget_income(adjustment_income(output_20000))

# Put these numbers in the report
# Make some figures using the sgs excel template

# Run this and then Ctrl+V in Excel
write.excel(percentages_budget_income(adjustment_income(output_0)))
write.excel(percentages_budget_income(adjustment_income(output_5000)))
write.excel(percentages_budget_income(adjustment_income(output_10000)))
write.excel(percentages_budget_income(adjustment_income(output_15000)))
write.excel(percentages_budget_income(adjustment_income(output_20000)))

model_income = function() {
  
  regression_data =
    bind_rows(
      cbind(Stress = RAI_meaning[-7,3], Income = "5000", percentages_budget_income(adjustment_income(output_5000))),
      cbind(Stress = RAI_meaning[-7,3], Income = "10000", percentages_budget_income(adjustment_income(output_10000))),
      cbind(Stress = RAI_meaning[-7,3], Income = "15000", percentages_budget_income(adjustment_income(output_15000))),
      cbind(Stress = RAI_meaning[-7,3], Income = "20000", percentages_budget_income(adjustment_income(output_20000)))
    ) %>%
    gather(-Income, -Stress, key = "Type", value = "Cost")
  
  model = lm(Cost ~ Income + Type + Stress, regression_data)
  
  print(model)
  
}

model_income()

ratio = function(percentages) {
  
  #Single/Couple ratios sourced from draft report
  x =
    cbind(
      age.pension_budget = 
        data.frame(
          percentages$age.pension_Single * 0.45 +
            percentages$age.pension_Couple * 0.55
        ) * AGE_renter,
      disability_budget = 
        data.frame(
          percentages$disability_Single * 0.24 +
            percentages$disability_Couple * 0.76 
        ) * DSP_renter,
      wifepension_partneragepension_budget = 
        data.frame(
          percentages$wifepension_partneragepension_Couple
        ) * AGE_renter,
      wifepension_partnerdisability_budget =
        data.frame(
          percentages$wifepension_partnerdisability_Couple
        ) * DSP_renter
    )
  
  names(x) = c("Age.Pension",
               "Disability.Support.Pension",
               "Wife.Pension_Age.Pension",
               "Wife.Pension_Disability.Support.Pension"
  )
  
  return(x)
  
}

ratio_single = function(percentages) {
  
  #Single/Couple rations sourced from draft report
  x =
    cbind(
      age.pension_budget = 
        data.frame(
          percentages$age.pension_Single * 0.45 
        ) * AGE_renter,
      disability_budget = 
        data.frame(
          percentages$disability_Single * 0.76
        ) * DSP_renter,
      wifepension_partneragepension_budget = 
        data.frame(
          0
        ) * AGE_renter,
      wifepension_partnerdisability_budget =
        data.frame(
          0
        ) * DSP_renter
  
  names(x) = c("Age.Pension",
               "Disability.Support.Pension",
               "Wife.Pension_Age.Pension",
               "Wife.Pension_Disability.Support.Pension"
  )
  
  return(x)
  
}

ratio_couple = function(percentages) {
  
  #Single/Couple rations sourced from draft report
  x =
    cbind(
      age.pension_budget = 
        data.frame(
            percentages$age.pension_Couple * 0.55
        ) * AGE_renter,
      disability_budget = 
        data.frame(
            percentages$disability_Couple * 0.24
        ) * DSP_renter,
      wifepension_partneragepension_budget = 
        data.frame(
          percentages$wifepension_partneragepension_Couple
        ) * AGE_renter,
      wifepension_partnerdisability_budget =
        data.frame(
          percentages$wifepension_partnerdisability_Couple
        ) * DSP_renter  
    )
  
  names(x) = c("Age.Pension",
               "Disability.Support.Pension",
               "Wife.Pension_Age.Pension",
               "Wife.Pension_Disability.Support.Pension"
  )
  
  return(x)
  
}

# Excel tables--------------------------------------------------------------------------
stress_sum(stress(single, RAI$Age.Pension) %>% filter(STE == "NSW"), age.pension)
stress_sum_income(single, couple)
#Counts are same, therefore the discrepancy is in the adjustment or budgeting formulas, which are, respectively:
adjustment()
adjustment_income(output_0)
#Adjustment is the same, therefore the discrepancy is in the budgeting formulas, which are, respectively:
percentages_budget(percentages)
percentages_budget_income(adjustment_income(output_0))
#Difference! Why?!
percentages_budget(adjustment())
#Problem was that the percentages dataframe did not adjust the new counts, only the percentages

# Write each Draft Report table as separate .csv
# Section 2.4 - Counting Recipients
counting_recipients = function() {
  
  list_incomes = list(
    output_0 %>%
      adjustment_income(.) %>%
      ratio(.) %>%
      mutate(Increase = 0),
    
    output_5000 %>%
      adjustment_income(.) %>%
      ratio(.) %>%
      mutate(Increase = 5000),
    
    output_10000 %>%
      adjustment_income(.) %>%
      ratio(.) %>%
      mutate(Increase = 10000),
    
    output_15000 %>%
      adjustment_income(.) %>%
      ratio(.) %>%
      mutate(Increase = 15000),
    
    output_20000 %>%
      adjustment_income(.) %>%
      ratio(.) %>%
      mutate(Increase = 20000)
  )
  
  return(list_incomes)
  
}

write_csv(
  as.data.frame(do.call(rbind, counting_recipients())), 
  "Working/Counts of Pensioners by Income Increments.csv"
  )

# New section comparing stress for singles vs couples for AGE and DSP
counting_recipients_compare = function() {
  
  list = list(
   
    #Single counts
    output_0 %>%
      adjustment_income(.) %>%
      ratio_single(.) %>%
      mutate(Increase = 0) %>%
      mutate(Status = "Single"),
    
    output_5000 %>%
      adjustment_income(.) %>%
      ratio_single(.) %>%
      mutate(Increase = 5000) %>%
      mutate(Status = "Single"),
    
    output_10000 %>%
      adjustment_income(.) %>%
      ratio_single(.) %>%
      mutate(Increase = 10000) %>%
      mutate(Status = "Single"),
    
    output_15000 %>%
      adjustment_income(.) %>%
      ratio_single(.) %>%
      mutate(Increase = 15000) %>%
      mutate(Status = "Single"),
    
    output_20000 %>%
      adjustment_income(.) %>%
      ratio_single(.) %>%
      mutate(Increase = 20000) %>%
      mutate(Status = "Single"),

    #Couple counts
    output_0 %>%
      adjustment_income(.) %>%
      ratio_couple(.) %>%
      mutate(Increase = 0)  %>%
      mutate(Status = "Couple"),
    
    output_5000 %>%
      adjustment_income(.) %>%
      ratio_couple(.) %>%
      mutate(Increase = 5000)  %>%
      mutate(Status = "Couple"),
    
    output_10000 %>%
      adjustment_income(.) %>%
      ratio_couple(.) %>%
      mutate(Increase = 10000)  %>%
      mutate(Status = "Couple"),
    
    output_15000 %>%
      adjustment_income(.) %>%
      ratio_couple(.) %>%
      mutate(Increase = 15000)  %>%
      mutate(Status = "Couple"),
    
    output_20000 %>%
      adjustment_income(.) %>%
      ratio_couple(.) %>%
      mutate(Increase = 20000)  %>%
      mutate(Status = "Couple")
  )
  
  return(list)
  
}

write_csv(
  as.data.frame(
    do.call(rbind, counting_recipients_compare())
    ),
  "Working/Counts by Income and Marital Status.csv"
)

# Since the refinements are in the NSW counts, move section 3.2 before section 2.4

# Section 3.3 - Calculating the budget using the maximum possible assistance
percentages_budget_income(adjustment_income(output_0)) %>% 
  mutate(Combined = rowSums(.))

write_csv(percentages_budget_income(adjustment_income(output_0)) %>% 
            mutate(Combined = rowSums(.)),
          "Working/Budget by Income Increase - Non-cumulative.csv")

# Also Section 3.3 - The Cumulative Budget (alongside non-cumulative budget)
percentages_budget_income(adjustment_income(output_0)) %>%
  mutate_all(funs(cumsum)) %>% 
  mutate(Combined = rowSums(.))

write_csv(percentages_budget_income(adjustment_income(output_0)) %>%
            mutate_all(funs(cumsum)) %>% 
            mutate(Combined = rowSums(.)),
          "Working/Budget by Income Increase - Cumulative.csv")


