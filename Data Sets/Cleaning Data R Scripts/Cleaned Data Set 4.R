library(data.table)

# Set your own working directory
setwd("/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/Group Project/Data Sets/hospitalized-patients-with-heart-failure-integrating-electronic-healthcare-records-and-external-outcome-data-1.3")

readmission <- fread('dat.csv', na.strings = c("NA", "missing", "N/A", "", "m", "M", "na", "."))
colnames(readmission)

# Create a new data table with relevant variables
readmission2 <- readmission[, c(1:9,18:20,22:37,39,44:45,55:61,63,166:167)]

# Check the class of the variables
sapply(readmission2, class)

# Factor categorical variables
fcols <- c("V1", "inpatient.number", "DestinationDischarge", "admission.ward", "admission.way", "occupation", "discharge.department", "visit.times", "gender", "type.of.heart.failure", "NYHA.cardiac.function.classification", "myocardial.infarction", "congestive.heart.failure", "peripheral.vascular.disease", "cerebrovascular.disease", "dementia", "Chronic.obstructive.pulmonary.disease", "connective.tissue.disease", "peptic.ulcer.disease", "diabetes", "moderate.to.severe.chronic.kidney.disease", "hemiplegia", "leukemia", "malignant.lymphoma", "solid.tumor", "liver.disease", "AIDS", "type.II.respiratory.failure", "respiratory.support.", "oxygen.inhalation", "outcome.during.hospitalization", "death.within.28.days", "re.admission.within.28.days", "death.within.3.months", "re.admission.within.3.months", "death.within.6.months", "re.admission.within.6.months", "ageCat" )
readmission2[, (fcols):= lapply(.SD, factor), .SDcols = fcols]

# Check for duplicates in columns with unique values
sum(duplicated(readmission2$V1))
sum(duplicated(readmission2$inpatient.number))

# Check for number of NA values
sum(is.na(readmission2))

# Check variables
summary(readmission2)

# occupation has 27 'NA's. Recode to 'Others'.
readmission2[is.na(occupation), occupation:="Others"]

# BMI min=0, max=404 -> impossible. Check for BMI <10 or >75.
readmission2[BMI<10 | BMI>75, BMI] # there are 8 cases
# Re-code the 8 cases to NA
readmission2[BMI<10 | BMI>75, BMI := NA]

# leukemia only has one level --> remove
readmission2[,leukemia:=NULL]

# According to the summary, re.admission.time..days.from.admission. has 1107 NA values. Check why.
readmission2[re.admission.within.28.days=='0' & re.admission.within.3.months=='0' & re.admission.within.6.months=='0', .N, by = is.na(re.admission.time..days.from.admission.)]

# Since 1106 NA values for re.admission.time..days.from.admission. are due to the patient not being readmitted at all, we can re-code these 'NA' values to 0.
readmission2[re.admission.within.28.days=='0' & re.admission.within.3.months=='0' & re.admission.within.6.months=='0',re.admission.time..days.from.admission.:= 0]

# Check for re.admission.time..days.from.admission. < dischargeDay
readmission2[re.admission.time..days.from.admission. < dischargeDay & re.admission.time..days.from.admission. != 0, .(re.admission.time..days.from.admission., dischargeDay)] # there are 3 cases
# Recode the 3 cases to NA
readmission2[re.admission.time..days.from.admission. < dischargeDay & re.admission.time..days.from.admission. != 0, re.admission.time..days.from.admission. := NA]

sum(is.na(readmission2))

## Removing of outliers
# Boxplot for re.admission.time..days.from.admission.
boxplot(readmission2$re.admission.time..days.from.admission., data=readmission2)

# Removing outliers for re.admission.time..days.from.admission.
Q1 <- quantile(readmission2$re.admission.time..days.from.admission., 0.01, na.rm= TRUE)
Q3 <- quantile(readmission2$re.admission.time..days.from.admission., 0.99, na.rm= TRUE)
IQR <- IQR(readmission2$re.admission.time..days.from.admission., na.rm=TRUE)
readmission3 <- subset(readmission2, readmission2$re.admission.time..days.from.admission. > (Q1 - 1.5*IQR) & readmission2$re.admission.time..days.from.admission. < (Q3 + 1.5*IQR))

# Boxplot for BMI
boxplot(readmission3$BMI, data=readmission3)

# Removing outliers for BMI
bQ1 <- quantile(readmission3$BMI, 0.01, na.rm= TRUE)
bQ3 <- quantile(readmission3$BMI, 0.99, na.rm= TRUE)
bIQR <- IQR(readmission3$BMI, na.rm=TRUE)
readmission3 <- subset(readmission3, readmission3$BMI > (bQ1 - 1.5*bIQR) & readmission3$BMI < (bQ3 + 1.5*IQR))

# Remove NA values from data table
readmission3 <- readmission3[complete.cases(readmission3)]
# Verify that NA values have been removed
sum(is.na(readmission3))

# Calculate data loss
(nrow(readmission2)-nrow(readmission3))/nrow(readmission2) #1.34%

summary(readmission3)
readmission3
cleanedDS4 = readmission3
write.csv(cleanedDS4, "/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/Group Project/Data Sets/hospitalized-patients-with-heart-failure-integrating-electronic-healthcare-records-and-external-outcome-data-1.3/cleaned_data.csv")

