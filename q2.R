## Q2: US Department of Education college scores dataset. Downloaded with wget and unzipped. Note that all UNITID numbers are unique: length(unique(DATA$UNITID)) compared to length(DATA$UNITID).

rm(list = ls()) ##Clear workspace
options(digits=10)

## Question 1: Average SAT, 4-year in 2013 for freshmen only
## Import data into R
college2013 <- read.csv("CollegeScorecard_Raw_Data/MERGED2013_14_PP.csv", stringsAsFactors=FALSE)
frosh.sat13 <- subset(college2013, HIGHDEG=="4", select=c(SAT_AVG, UGDS))

frosh.sat13$SAT_AVG <- as.numeric(frosh.sat13$SAT_AVG) ## Make numeric
frosh.sat13$UGDS    <- as.numeric(frosh.sat13$UGDS)
frosh.sat13         <- frosh.sat13[complete.cases(frosh.sat13),] ## Remove NAs

frosh.sat13[["Admitted"]] <- frosh.sat13$UGDS/4 ## Add Enrollment
sum((frosh.sat13$SAT_AVG * frosh.sat13$Admitted) / sum(frosh.sat13$Admitted)) ## 1099.020275

## Question 2: Pearson correlation coefficient, SAT_AVG in 2013 and retention after 2 years
## Calculations exclude students that transferred
rt.2yr13 <- subset(college2013, SAT_AVG!="NULL", select=c(SAT_AVG, ENRL_ORIG_YR2_RT))

rt.2yr13$SAT_AVG          <- as.numeric(rt.2yr13$SAT_AVG)
rt.2yr13$ENRL_ORIG_YR2_RT <- as.numeric(rt.2yr13$ENRL_ORIG_YR2_RT)
rt.2yr13                  <- rt.2yr13[complete.cases(rt.2yr13),]

cor(retention$SAT_AVG, retention$ENRL_ORIG_YR2_RT, use="all.obs", method="pearson") ## 0.6618369648

## Question 3: Percent avg difference completion rates between high & low incomes (2013, 4yr)
## Excludes students that transferred
completion13 <- subset(college2013, HIGHDEG=="4",
                       select=c(LO_INC_COMP_ORIG_YR4_RT,
                                MD_INC_COMP_ORIG_YR4_RT,
                                HI_INC_COMP_ORIG_YR4_RT))
completion13$LO_INC_COMP_ORIG_YR4_RT <- as.numeric(completion13$LO_INC_COMP_ORIG_YR4_RT)
completion13$MD_INC_COMP_ORIG_YR4_RT <- as.numeric(completion13$MD_INC_COMP_ORIG_YR4_RT)
completion13$HI_INC_COMP_ORIG_YR4_RT <- as.numeric(completion13$HI_INC_COMP_ORIG_YR4_RT)
completion13                         <- completion13[complete.cases(completion13),]
colnames(completion13)               <- c("low", "middle", "high")

100 * (mean(completion13$high) - mean(completion13$low)) ## 11.40413657
## Question 4: T-test, 2-sided, compute log based 10 of p-value from above question
t <- t.test(completion$high, completion$low, alternative = "two.sided")
log10(t$p.value) ## -68.1332057

## Question 5: Diversity in schools (most - least) ethnicity
## Excluding race unknown, multi-racial, and non-resident aliens
diversity13 <- college2013[,c("UGDS_WHITE", "UGDS_BLACK",
                              "UGDS_HISP", "UGDS_ASIAN",
                              "UGDS_AIAN", "UGDS_NHPI")]
diversity13$UGDS_WHITE <- as.numeric(diversity13$UGDS_WHITE)
diversity13$UGDS_BLACK <- as.numeric(diversity13$UGDS_BLACK)
diversity13$UGDS_HISP <- as.numeric(diversity13$UGDS_HISP)
diversity13$UGDS_ASIAN <- as.numeric(diversity13$UGDS_ASIAN)
diversity13$UGDS_AIAN <- as.numeric(diversity13$UGDS_AIAN)
diversity13$UGDS_NHPI <- as.numeric(diversity13$UGDS_NHPI)
diversity13[diversity13==0] <- NA
diversity13 <- diversity13[complete.cases(diversity13),]

diversity13[["Max"]] <- apply(diversity13, 1, max)
diversity13[["Min"]] <- apply(diversity13, 1, min)
diversity13[["Index"]] <- diversity13$Max - diversity13$Min
min(diversity13$Index) ## 0.0827

## Average number of women between 2001 to 2010
files <- list.files("CollegeScorecard_Raw_Data", pattern="MERGED20*", full.names=TRUE)
files <- files[2:11]
women <- vector()
for (i in 1:(length(files))){
    data <- read.csv(files[i], stringsAsFactors=FALSE)
    tmp <- as.numeric(data[,"UGDS_WOMEN"])
    women <- cbind(women, tmp)
}
colnames(women) <- as.character(2001:2010)
women[women==0] <- NA
women <- women[complete.cases(women),]
mean(women) ## 0.6574556314

## Probability school in region located in a city, all datasets
files_all <- list.files("CollegeScorecard_Raw_Data", pattern="MERGED*", full.names=TRUE)
region <- vector()
for (i in seq_along(files_all)){
    data2 <- read.csv(files_all[i], stringsAsFactors=FALSE)
    region <- cbind(region, data2[,""])
}
colnames(region) <- as.character(1996:2014)
