# Packages: ---------------------------------------------------------------------------------
# List of packages for session
.packages = c("dplyr", "stringr", "tidyr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)


# Function for importing data via clipboard: ------------------------------------------------
# For copying and pasting excel data into R
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}


# Read in data: ---------------------------------------------------------------------------
# From each tab of the DSS demographics data
AUS_state = read.excel()
AUS_gender = read.excel() 
AUS_marital = read.excel() #stopped fixing headers individually
AUS_age = read.excel()
AUS_indigenous = read.excel()
AUS_postcode = read.excel()
AUS_birthplace = read.excel()
AUS_duration_payment = read.excel()
AUS_duration_incomesupport = read.excel()
AUS_rate = read.excel()
AUS_earnings = read.excel()

# From the tab Age Pension Assets
# Note: numbers are average asset amount
AUS_partnered_homeowner = read.excel()
AUS_partnered_nonhomeowner = read.excel()
AUS_single_homeowner = read.excel()
AUS_single_nonhomeowner = read.excel()

# Clean up: -----------------------------------------------------------------------------
# Extract NSW aged pension data from each and clean up headers

#By State, with NSW data extracted:
fix(state)
state = state[2,4]
state = data.frame(state)
colnames(state) = "NSW"
rownames(state) = "Aged_Pension"

fix(gender)
gender = gender[3,6:8]
rownames(gender) = "Aged_Pension"

fix(marital)
marital = marital[4,6:8]
rownames(marital) = "Aged_Pension"
colnames(marital) = c("NSW_partnered", "NSW_unpartnered", "NSW_unknown")

fix(age) #No point, since all are above 65 for pension


fix(indigenous)
indigenous = indigenous[4,5:6]
rownames(indigenous) = "Aged_Pension"
colnames(indigenous) = c("NSW_indigenous", "NSW_notindigenous")

fix(postcode)
postcode = postcode[,c(1, 4)]

#Not by State, Country only, therefore percentages of total will be needed
fix(birthplace)
birthplace = birthplace[,c(1,4)]

fix(duration_payment)
names(duration_payment) = c("Payment Type",  "Under 1 year", "1-<2 years", '2-<5 years', "5-<10 years", "10 years", "Average.duration.on.payment..weeks")
duration_payment = duration_payment[4,1:7]

fix(duration_incomesupport) #Nothing in here on the aged pension


fix(rate)
names(rate) = c("Payment Type", "Full rate", "Part rate - income test", "Part rate - assets test", "Part rate - total", "Zero rate", "Undetermined/manual rate")
rate = rate[3, 2:7]
rownames(rate) = "Aged_Pension"
rate = rate[,1:6]

fix(earnings)
names(earnings) = c("Payment Type", "No Earnings", "Had Earnings", ">$0-<$100", "$100-<$143", "$143-<$250", "250+")
earnings = earnings[3,1:7]
rownames(earnings) = "Aged_Pension"

# Import Commonwealth Rent Assistance (CRA) data
cra =  88436 #No. of CRA income units for Aged Pensioners in NSW (June 2016)
cra_indigenous =  1617
cra_age = read.excel()
cra_age = cra_age[-1,c(1,6)]
cra_remoteness = read.excel() #For all Australia, not just NSW, NSW CRA data available, but not just for pensioners
cra_over75 = read.excel() #NSW specific
cra_over75over30 = read.excel() #Numbers as percentages, NSW specific
cra_over75over50 = read.excel() #Numbers as percentages, NSW specific

# Calculate new incomes with indigenous, marital, rate, earnings, and cra data------------

#For NSW
indigenous
marital

#For country
rate
earnings
AUS_partnered_homeowner
AUS_partnered_nonhomeowner
AUS_single_homeowner
AUS_single_nonhomeowner


# Goals: 
## - Determine pensioner income
## - Identifying the number of households under housing stress

# Note: DSS demographics data is from Dec 2016


