library(jsonlite)

# set debug flag
debug <- TRUE  # FALSE to read command line inputs 

if (debug == FALSE ) {
   dataFile <- input[[1]] # 'data.json' path to file
   dataMap  <- input[[2]]  # 'dataMam.json' path to file
}  else  {
   dataFile <-'./cr/301/example.rawData.json'# path to your local rawData and dataMap file you want to test eg.: /Users/gretchenhower/Documents/R Projects/datafile/example.rawData.json
   dataMap  <-'./cr/301/example.dataMap.json'# path to your local rawData and dataMap file you want to test eg.: /Users/gretchenhower/Documents/R Projects/datafile/example.dataMap.json
}

########## JSON FORMATTING HELPER #########
  returnChartDataAndMetaData <- function (
                                 data,
                                 title,
                                 baseSize, 
                                 questionID,
                                 keyOrder="",
                                 xAxisTitle="", 
                                 yAxisTitle="",
                                 chartType="pie",
                                 colors=c("#1f78b4", "#a6cee3"),
                                 orientation="v",
                                 confidenceInterval="false", 
                                 dataValue="percent", 
                                 average='mean',
                                 dataType="1d"){
    metadata <- list(
      'confidenceInterval'=confidenceInterval,
      'dataValue'=dataValue,
      'baseSize'=baseSize,
      'average'=average,
      'chartType'=chartType,
      'orientation'=orientation,
      'dataType'=dataType,
      'colors'=colors,
      'keyOrder'=keyOrder
    )
    returnObject <- list(
      'title'=title,
      'xAxisTitle'=xAxisTitle,
      'yAxisTitle'=yAxisTitle,
      'questionID'=questionID,
      'metadata'=metadata,
      'data'=data
    )
    result <- returnObject
  }






# load survey results into an R data.frame:
example.raw.data <- fromJSON(dataFile)

# load data map into an R data.frame
example.data.map <- fromJSON(dataMap)


example.raw.data.variables <- names(example.raw.data) # 456 variable names

example.data.map.variables <- example.data.map$variables
example.data.map.questions <- example.data.map$questions

# names(example.data.map.variables)
# [1] "vgroup"   "qtitle"   "colTitle" "title"    "rowTitle" "label"    "row"     
# [8] "type"     "col"      "qlabel"   "values"  

# names(example.data.map.questions)
# [1] "qtitle"    "variables" "values"    "qlabel"    "type"      "grouping"

Sub_Cat.variables <-
  c("Sub_Catr1", "Sub_Catr2","Sub_Catr3","Sub_Catr4","Sub_Catr5")
### NOTE this is dynamic as this is user input dependant r1 to r8 
### TODO fetch dynamically Below an example does that work?
result1.names <- names(example.raw.data)
Sub_Cat.variable.index <- grep("Sub_Catr[0-9]+", result1.names) 
Sub_Cat.variables <- result1.names[Sub_Cat.variable.index]


Sub_Cat.values <- example.raw.data[Sub_Cat.variables]
Sub_Cat.n <- length(Sub_Cat.variables)
# get the sub_cat indexes

###Note the same we can apply to the brands

KeyBrands.variable.index <- grep("qKeyBrandsr[0-9]+", result1.names)
KeyBrands.variables <- result1.names[KeyBrands.variable.index]

###Note the same we can apply to the Brands_Eval.variables 
Brands_Eval.variables <- result1.names[KeyBrands.variable.index]


Brands_Eval.variables <-
  c("Brands_Evalr1", "Brands_Evalr2","Brands_Evalr3","Brands_Evalr4","Brands_Evalr5","Brands_Evalr6")
### NOTE this is dynamic as this is user input dependant r1 to r8 
### TODO fetch dynamically 
### this can be taken from qKeyBrands all row labels starting with r+idx Throughout the report for brands 
### we should use this question and its labels qKeyBrands PK note !!!

Brands_Eval.values <- example.raw.data[Brands_Eval.variables]

# KeyBrands:  Which Brands were identified as keyBrands

KeyBrands.variables <-
  c("qKeyBrandsr1", "qKeyBrandsr2","qKeyBrandsr3","qKeyBrandsr4","qKeyBrandsr5","qKeyBrandsr6")
### NOTE this is dynamic as this is user input dependant r1 to r8 
### TODO fetch dynamically 
### this can be taken from qKeyBrands all row labels starting with r+idx Throughout the report for brands 
### we should use this question and its labels qKeyBrands PK note !!!


KeyBrands.values <- example.raw.data[KeyBrands.variables]

KeyBrands.values.1 <- as.numeric(KeyBrands.values[1,])
KeyBrands.index <- which(KeyBrands.values.1 == 1)
KeyBrands.n <- length(KeyBrands.index)


Brand_Usager.variables <-
  c("Brand_Usager1", "Brand_Usager2","Brand_Usager3","Brand_Usager4","Brand_Usager5","Brand_Usager6")
### NOTE this is dynamic as this is user input dependant r1 to r8 
### TODO fetch dynamically 
### this can be taken from qKeyBrands all row labels starting with r+idx Throughout the report for brands 
### we should use this question and its labels qKeyBrands PK note !!!

Brand_Usager.values <- example.raw.data[Brand_Usager.variables]

# so that existing files don't need to be modified:
result1 <- example.raw.data
result2 <- example.data.map

data.map <- example.data.map

# general purpose functions:

Data.Value <- function (variable.name) {
  # Data.Value returns a data.frame
  # variable.name is a list of strings
  
  data.string <- example.raw.data[variable.name]
  result <-
    as.data.frame(lapply(data.string, function(x)
      as.numeric(x)))
}



Row.Value <- function (variable.name, max.valid.row = NULL) {
  # Row.Value returns a sorted list of integers
  #   found as ".*r/[0-9]*/.*" in variable.name
  # variable.name is a list of strings
  
  raw.numeric <- suppressWarnings(as.numeric(sub(
    "c.*", "", sub(".*r", "", variable.name))))
  
  row.value <- sort(union(NULL, raw.numeric))
  
  if (!is.null(max.valid.row)) {
    row.value <- row.value[row.value < max.valid.row]
  }
  
  result <- row.value
}


Row.Variable.Name <- function (prefix, row.value) {
  # Row.Variable.Name returns a sorted list of strings
  #   each string is a variable name for the corresponding row
  # prefix is a string
  # row.value is a vector of integers
  
  result <- sapply(row.value, function(x) paste(prefix, x, sep=""))
}


Data.Level.Count <- function (numeric.data, n.level) {
  # Data.Level.Count returns a list of
  #   n.max : the number of rows in numeric.data
  #   n.valid : for each data column, the number of valid responses
  #   n.response : for each data column, the number of responses at each level
  #   pct.response : for each data column, the percentage ...
  # numeric.data is a dataframe
  # n.level is a positive integer
  
  n.max <- nrow(numeric.data)
  n.valid <- n.max - colSums(is.na(numeric.data))
  
  n.response <- as.data.frame(numeric.data[0,])
  pct.response <- n.response
  
  for (ix in 1:n.level) {
    n.response[ix,] <-
      apply(numeric.data, 2, function(x) sum(x == ix, na.rm = TRUE))
    pct.response[ix,] <- n.response[ix,] / n.valid
  }
  
  result <-
    list(
      "n.max" = n.max,
      "n.valid" = n.valid,
      "n.response" = n.response,
      "pct.response" = pct.response
    )
}

# BEGIN GENDER PIE CHART  First chart on slide 4

Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
    
  result <- list(
    'questionID' = curr.id,
    'baseSize' = n.valid,
    'data' = data.frame('attribute'=c('Male','Female'),'value'=pct.level[report.level])

  )
}


out.slide4.r1c1.S3.gender.PC <- 
  Single.Column(
    curr.id = "S3", 
    n.level = 2,
    report.level = c(1,2))

# BEGIN REGION MAP CHART  Second chart on slide 4

Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  keyOrder <-c(
      "Northeast",
      "South",
      "Midwest",
      "West"
    )
  result <- list(
    "sample size" = n.valid,
    'questionID' = curr.id,
    'baseSize' = n.valid,
    'keyOrder'= keyOrder,
    'data' = data.frame('attribute'=keyOrder,'value'=pct.level[report.level])
  )
}


out.slide4.r1c2.region_quota.MAP <- 
  Single.Column(
    curr.id = "region_quota", 
    n.level = 4,
    report.level = c(1:4))


Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  # Specific to income, recode 'prefer not to answer' as missing
  curr.data[curr.data==7] <- NA
  
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
  pct.level[n.level+1] <- sum(pct.level[3:4])
  pct.level[n.level+2] <- sum(pct.level[5:6])
  #pct.level[n.level+2] <- sum(pct.level[(n.level-1):n.level])
  
  # hard-coded labels:
  keyOrder <-
    c(
      "<$25k",
      "$25-$49k",
      "$50-$99k",
      "$100k+"
    )
  
  result <- list(
    'questionID' = curr.id,
    'baseSize' = n.valid,
    'keyOrder'= keyOrder,
    'data' = data.frame('attribute'=keyOrder,'value'=pct.level[report.level])
  
  )
}


out.slide4.r1c3.D3.income.VSB <- 
  Single.Column(
    curr.id = "D3", 
    n.level = 7,
    report.level = c(1:4))


# BEGIN ETHNICITY STACKED BAR CHART  Third chart on slide 4

# Need both S5 and S6

#S5 race

### NOTE: Not sure this is correct we have ethnicity and then additionally hispanic origin or not
### these are not distinguishable but can be combined. eg.: A African American can be of hispanic origin?

Ethnicity<-function(){
  
  S5.variables <- "S5"
  S5.values <- result1[S5.variables]
  S5.numeric <- lapply(S5.values, function(x) as.numeric(x)) # a list
  S5.numeric.frame <- as.data.frame(S5.numeric)
  
  S5.n.max <- nrow(S5.numeric.frame)
  S5.n.valid <- S5.n.max - colSums(is.na(S5.numeric.frame))
  
  S5.race.valid <- ! is.na(S5.numeric.frame)
  S5.valid.race <- S5.numeric.frame[S5.race.valid]
  
  S5.pct.race.level <-
    unlist(lapply(1:6, function(x) sum(S5.valid.race==x))) / S5.n.valid
  
  #S6 hispanic
  
  S6.variables <- "S6"
  S6.values <- result1[S6.variables]
  S6.numeric <- lapply(S6.values, function(x) as.numeric(x)) # a list
  S6.numeric.frame <- as.data.frame(S6.numeric)
  
  S6.n.max <- nrow(S6.numeric.frame)
  S6.n.valid <- S6.n.max - colSums(is.na(S6.numeric.frame))
  
  S6.hispanic.valid <- ! is.na(S6.numeric.frame)
  S6.valid.hispanic <- S6.numeric.frame[S6.hispanic.valid]
  
  S6.pct.hispanic.level <-
    unlist(lapply(1:2, function(x) sum(S6.valid.hispanic==x))) / S6.n.valid
  
  # Now need to combine
  # Ultimate categories
  # Hispanic (list 3) = S6=1
  # Caucasian (list 1) = S6=2 and S5=1
  # AA (list 2) = S6=2 and S5=2
  # Asian (list 4) = S6=2 and S5=4
  # All other (list 5) = rest (S6=2 and (S5=3 or S5=5)
  
  
  S5.valid.race_collapse<-S5.valid.race
  S5.valid.race_collapse[S5.valid.race==3 | S5.valid.race==5 | S5.valid.race==6]<-5
  S6.hispanic.indexes<-which(S6.valid.hispanic %in% 1)
  S5.valid.race_collapse[S6.hispanic.indexes]<-3
  
  ethnicity.result <-
    unlist(lapply(1:5, function(x) sum(S5.valid.race_collapse==x))) / S5.n.valid
  
  names(ethnicity.result)<-
    c("Caucasian",
      "AA",
      "Hispanic",
      "Asian",
      "All Other")
  result<-ethnicity.result
  
}
out.slide4.r1c4.S5S6.ethnicity.VSB <- Ethnicity()



# Data is in S5.pct.race.level_collapse
# 1=Caucasian
# 2=AA
# 3=Hispanic
# 4=Asian
# 5=Other

#END ETHNICITY STACKED BAR CHART

Single.Column.Infer.N.Level <- function(curr.id, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  data.level <- unique(valid.data)
  n.level <- length(data.level)
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == data.level[x])))/ n.valid
  
  pct.level[pct.level>=5]<-5
  
  # Labels
  #1=1
  #2=2
  #3=3
  #4=4
  #5=5+
  
  names(pct.level) <-
    c(
      "1",
      "2",
      "3",
      "4",
      "5+"
    )
  
  result <- list(
    "sample size" = n.valid,
    "n.level" = n.level,
    "level" = data.level,
    "output.pct.level" = pct.level[report.level]
  )
}

out.slide4.r2c1.D1.HHSize.UK <-  Single.Column.Infer.N.Level(
  curr.id = "D1", 
  report.level = c(1:5))





# BEGIN HOUSEHOLD SIZE DATA  Fifth chart on slide 4
D1.variables <- "D1"
D1.values <- result1[D1.variables]
D1.numeric <- lapply(D1.values, function(x) as.numeric(x)) # a list
D1.numeric.frame <- as.data.frame(D1.numeric)

D1.n.max <- nrow(D1.numeric.frame)
D1.n.valid <- D1.n.max - colSums(is.na(D1.numeric.frame))

D1.HHSize.valid <- ! is.na(D1.numeric.frame)
D1.valid.HHSize <- D1.numeric.frame[D1.HHSize.valid]
D1.valid.HHSize[D1.valid.HHSize>=5]<-5

out.slide4.r2c1.D1.HHSize.UK <-
  unlist(lapply(1:5, function(x) sum(D1.valid.HHSize==x))) / D1.n.valid

# Labels
#1=1
#2=2
#3=3
#4=4
#5=5+

#END HOUSEHOLD SIZE DATA


# BEGIN HOUSEHOLD COMPOSITION HORIZONTAL BAR CHART  Fifth chart on slide 4
# There are 6 non-exclusive categories
# HHcompr5 = Single
# HHcompr3 = Married, No Kids
# HHcompr2 = Married, Kids
# HHcompr4 = Single, Kids
# HHcompr6 = Multi-Gen
# HHcompr7 = Other

Household.Composition <- function() {
  
  HHcompr5.variables <- "HHcompr5"
  HHcompr5.values <- result1[HHcompr5.variables]
  HHcompr5.numeric <- lapply(HHcompr5.values, function(x) as.numeric(x)) # a list
  HHcompr5.numeric.frame <- as.data.frame(HHcompr5.numeric)
  
  HHcompr5.n.max <- nrow(HHcompr5.numeric.frame)
  HHcompr5.n.valid <- HHcompr5.n.max - colSums(is.na(HHcompr5.numeric.frame))
  
  HHcompr5.Single.valid <- ! is.na(HHcompr5.numeric.frame)
  HHcompr5.valid.Single <- HHcompr5.numeric.frame[HHcompr5.Single.valid]
  
  HHcompr5.pct.Single.level <-
    unlist(lapply(1, function(x) sum(HHcompr5.valid.Single==x))) / HHcompr5.n.valid
  
  ####
  
  HHcompr3.variables <- "HHcompr3"
  HHcompr3.values <- result1[HHcompr3.variables]
  HHcompr3.numeric <- lapply(HHcompr3.values, function(x) as.numeric(x)) # a list
  HHcompr3.numeric.frame <- as.data.frame(HHcompr3.numeric)
  
  HHcompr3.n.max <- nrow(HHcompr3.numeric.frame)
  HHcompr3.n.valid <- HHcompr3.n.max - colSums(is.na(HHcompr3.numeric.frame))
  
  HHcompr3.MNokids.valid <- ! is.na(HHcompr3.numeric.frame)
  HHcompr3.valid.MNokids <- HHcompr3.numeric.frame[HHcompr3.MNokids.valid]
  
  HHcompr3.pct.MNokids.level <-
    unlist(lapply(1, function(x) sum(HHcompr3.valid.MNokids==x))) / HHcompr3.n.valid
  
  ####
  
  HHcompr2.variables <- "HHcompr2"
  HHcompr2.values <- result1[HHcompr2.variables]
  HHcompr2.numeric <- lapply(HHcompr2.values, function(x) as.numeric(x)) # a list
  HHcompr2.numeric.frame <- as.data.frame(HHcompr2.numeric)
  
  HHcompr2.n.max <- nrow(HHcompr2.numeric.frame)
  HHcompr2.n.valid <- HHcompr2.n.max - colSums(is.na(HHcompr2.numeric.frame))
  
  HHcompr2.Mkids.valid <- ! is.na(HHcompr2.numeric.frame)
  HHcompr2.valid.Mkids <- HHcompr2.numeric.frame[HHcompr2.Mkids.valid]
  
  HHcompr2.pct.Mkids.level <-
    unlist(lapply(1, function(x) sum(HHcompr2.valid.Mkids==x))) / HHcompr2.n.valid
  
  ####
  
  HHcompr4.variables <- "HHcompr4"
  HHcompr4.values <- result1[HHcompr4.variables]
  HHcompr4.numeric <- lapply(HHcompr4.values, function(x) as.numeric(x)) # a list
  HHcompr4.numeric.frame <- as.data.frame(HHcompr4.numeric)
  
  HHcompr4.n.max <- nrow(HHcompr4.numeric.frame)
  HHcompr4.n.valid <- HHcompr4.n.max - colSums(is.na(HHcompr4.numeric.frame))
  
  HHcompr4.Skids.valid <- ! is.na(HHcompr4.numeric.frame)
  HHcompr4.valid.Skids <- HHcompr4.numeric.frame[HHcompr4.Skids.valid]
  
  HHcompr4.pct.Skids.level <-
    unlist(lapply(1, function(x) sum(HHcompr4.valid.Skids==x))) / HHcompr4.n.valid
  
  ####
  
  HHcompr6.variables <- "HHcompr6"
  HHcompr6.values <- result1[HHcompr6.variables]
  HHcompr6.numeric <- lapply(HHcompr6.values, function(x) as.numeric(x)) # a list
  HHcompr6.numeric.frame <- as.data.frame(HHcompr6.numeric)
  
  HHcompr6.n.max <- nrow(HHcompr6.numeric.frame)
  HHcompr6.n.valid <- HHcompr6.n.max - colSums(is.na(HHcompr6.numeric.frame))
  
  HHcompr6.Multigen.valid <- ! is.na(HHcompr6.numeric.frame)
  HHcompr6.valid.Multigen <- HHcompr6.numeric.frame[HHcompr6.Multigen.valid]
  
  HHcompr6.pct.Multigen.level <-
    unlist(lapply(1, function(x) sum(HHcompr6.valid.Multigen==x))) / HHcompr6.n.valid
  
  ####
  
  HHcompr7.variables <- "HHcompr7"
  HHcompr7.values <- result1[HHcompr7.variables]
  HHcompr7.numeric <- lapply(HHcompr7.values, function(x) as.numeric(x)) # a list
  HHcompr7.numeric.frame <- as.data.frame(HHcompr7.numeric)
  
  HHcompr7.n.max <- nrow(HHcompr7.numeric.frame)
  HHcompr7.n.valid <- HHcompr7.n.max - colSums(is.na(HHcompr7.numeric.frame))
  
  HHcompr7.Other.valid <- ! is.na(HHcompr7.numeric.frame)
  HHcompr7.valid.Other <- HHcompr7.numeric.frame[HHcompr7.Other.valid]
  
  HHcompr7.pct.Other.level <-
    unlist(lapply(1, function(x) sum(HHcompr7.valid.Other==x))) / HHcompr7.n.valid
  
  # Combine these into one vector
  
  hhcomp <- c(HHcompr5.pct.Single.level,HHcompr3.pct.MNokids.level,
              HHcompr2.pct.Mkids.level,HHcompr4.pct.Skids.level,
              HHcompr6.pct.Multigen.level,HHcompr7.pct.Other.level)
  hhcomp.data.frame<-as.data.frame(hhcomp)
  
  # hard-coded labels:
  names(hhcomp) <-
    c(
      "SinglePerson",
      "Married, no kid",
      "Married, kids",
      "Sngl Parent, kids",
      "Multi-Gen",
      "Other"
    )
  
  result<-hhcomp
  
  
}


out.slide4.r2c2.HHCompr.HHComp.HB <- Household.Composition()

#Labels in order
#1=Single Person
#2=Married, no kids
#3=Married, kids
#4=Sngl Parent, kids
#5=Multi-Gen
#6=Other
####

#END HOUSEHOLD COMPOSITION HORIZONTAL BAR CHART


Single.Column.Infer.N.Level <- function(curr.id, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  data.level <- unique(valid.data)
  n.level <- length(data.level)
  
  # recode ages into age groups
  
  
  valid.data[valid.data<=8]<-1
  valid.data[valid.data>8 & valid.data<21]<-2
  valid.data[valid.data>20 & valid.data<38]<-3
  valid.data[valid.data>=38]<-4
  
  
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:4, function(x) sum(valid.data == x)))/ n.valid
  
  
  
  
  
  
  names(pct.level) <-
    c(
      "GenZ",
      "Millenial",
      "GenX",
      "Boomer"
    )
  
  result <- list(
    "sample size" = n.valid,
    "n.level" = n.level,
    "level" = data.level,
    "output.pct.level" = pct.level[report.level]
  )
}

out.slide4.r2c3.S4.generations.VSB <-  Single.Column.Infer.N.Level(
  curr.id = "S4", 
  report.level = c(1:4))


Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
  # hard-coded labels:
  names(pct.level) <-
    c(
      "City",
      "Suburb",
      "Small town",
      "Rural"
    )
  
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
}


out.slide4.r2c4.D4.urbanicity.VSB <- 
  Single.Column(
    curr.id = "D4", 
    n.level = 4,
    report.level = c(1:4))


# generate JSON data for slide 5
#   stacked bar charts for sustainability attitudes
# q37r



Q37 <- function(curr.id, n.level) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  # find the variables that begin with the pattern
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <-
    example.raw.data.variables[curr.variable.index]
  # the variables may not be sorted correctly by attribute index
  
  # get sorted attribute indexes:
  row.value <- Row.Value(curr.variables)
  n.attribute <- length(row.value)
  
  # variables in order of increasing attribute index:
  row.variable <-
    Row.Variable.Name(paste(curr.id, "r", sep = ""), row.value)
  
  # get responses as a data.frame; one column for each attribute
  curr.data <- Data.Value(row.variable)
  
  curr.count <- Data.Level.Count(curr.data, n.level)
  
  curr.pct.response <- curr.count$pct.response
  
  data.map.index <-
    grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index, ]
  level.string <- data.map.variable$values[1][[1]]$title
  
  rownames(curr.pct.response) <- level.string
  colnames(curr.pct.response) <- data.map.variable$rowTitle
  
  result <- list(
    "n.valid" = curr.count$n.valid,
    "pct.response" = curr.pct.response)
}


out.slide5.Q37.sustainability.VSB <-
  Q37(curr.id = "Q37", n.level = 5)

##### BEGIN SLIDE 6 CHART 2 PURCHASE FREQUENCY HORIZONTAL BAR CHART

Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
  pct.level[n.level+1] <- sum(pct.level[1:3])
  pct.level[n.level+2] <- sum(pct.level[(n.level-1):n.level])
  
  # hard-coded labels:
  names(pct.level) <-
    c(
      "Weekly",
      "Every 2-3 weeks",
      "Monthly",
      "NET Monthly or More",
      "Every 2-3 Months",
      "Every 4-6 Months",
      "Every 7-12 Months",
      "<1 x Year"
    )
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
}


out.slide6.c1.Q1.catpurchrec.HB <- 
  Single.Column(
    curr.id = "Q1", 
    n.level = 8,
    report.level = c(1:3, 9, 4:6, 10))

##### END SLIDE 6 CHART 2 PURCHASE FREQUENCY HORIZONTAL BAR CHART


###NOTE: Why is this changing when it worked in the first place?

Q3 <- function(curr.id, n.level) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  # not working for this example because of the fake categories
  # # find the variables that begin with the pattern
  # curr.variable.index <-
  #   grep(curr.pattern, example.raw.data.variables)
  #
  # curr.variables <-
  #   example.raw.data.variables[curr.variable.index]
  
  # hard-code for now; apply Sub_Cat later
  curr.variables <- sapply(1:Sub_Cat.n, function(x)
    paste("Q3r", x, sep = ""))
  ### NOTE: I think this works as you are using the Sub_Cat as the length 
  ###
  
  # pull in the example data:
  curr.data <- Data.Value(curr.variables)
  
  # find the most frequent among sub-categories:
  category.frequency <- do.call(pmin, curr.data)
  category.frequency.valid <- !is.na(category.frequency)
  category.valid.frequency <-
    category.frequency[category.frequency.valid]
  
  # count the number of responses for each variable
  category.frequency.n.max <- length(category.frequency)
  category.frequency.n.valid <- length(category.valid.frequency)
  
  # count the number of respondents at each level:
  category.n.purchase.frequency.level <-
    unlist(lapply(1:n.level, function(x)
      sum(category.valid.frequency == x)))
  
  category.pct.purchase.frequency.level <-
    category.n.purchase.frequency.level /
    category.frequency.n.valid
  
  # append total of levels 1 to 3, and total of levels 7 and 8 ("roll-up")
  # hard-coded
  
  category.pct.purchase.frequency.level[9] <-
    sum(category.pct.purchase.frequency.level[1:3])
  category.pct.purchase.frequency.level[10] <-
    sum(category.pct.purchase.frequency.level[7:8])
  
  output.pct.purchase.frequency <-
    category.pct.purchase.frequency.level[c(1, 2, 3, 9, 4, 5, 6, 10)]
  
  # hard-coded labels:
  names(output.pct.purchase.frequency) <-
    c(
      "Weekly",
      "Every 2-3 weeks",
      "Monthly",
      "NET Monthly or More",
      "Every 2-3 Months",
      "Every 4-6 Months",
      "Every 7-12 Months",
      "<1 x Year"
    )
  
  result <- list("n.valid" = category.frequency.n.valid,
                 "pct.response" = output.pct.purchase.frequency)
}

# call
out.slide6.c2.Q3.catpurchfreq.HB <- Q3(curr.id = "Q3", n.level = 8)

##### BEGIN SLIDE 6a CHART 1 CONSUMPTION RECENCY HORIZONTAL BAR CHART

Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
  pct.level[n.level+1] <- sum(pct.level[1:3])
  pct.level[n.level+2] <- sum(pct.level[(n.level-1):n.level])
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
}


out.slide6A.c1.Q2.catconsrec.HB <- 
  Single.Column(
    curr.id = "Q2", 
    n.level = 8,
    report.level = c(1:3, 9, 4:6, 10))

##### END SLIDE 6a CHART 1 CONSUMPTION RECENCY HORIZONTAL BAR CHART


###### BEGIN SLIDE 6a CHART 2 CONSUMPTION

Q4 <- function(curr.id, n.level) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  # not working for this example because of the fake categories
  # # find the variables that begin with the pattern
  # curr.variable.index <-
  #   grep(curr.pattern, example.raw.data.variables)
  #
  # curr.variables <-
  #   example.raw.data.variables[curr.variable.index]
  
  # hard-code for now; apply Sub_Cat later
  curr.variables <- sapply(1:Sub_Cat.n, function(x)
    paste("Q4r", x, sep = ""))
  ###NOTE: this should work as we are using lenth of Sub_Cat

  # pull in the example data:
  curr.data <- Data.Value(curr.variables)
  
  # find the most frequent among sub-categories:
  category.frequency <- do.call(pmin, curr.data)
  category.frequency.valid <- !is.na(category.frequency)
  category.valid.frequency <-
    category.frequency[category.frequency.valid]
  
  # count the number of responses for each variable
  category.frequency.n.max <- length(category.frequency)
  category.frequency.n.valid <- length(category.valid.frequency)
  
  # count the number of respondents at each level:
  category.n.purchase.frequency.level <-
    unlist(lapply(1:n.level, function(x)
      sum(category.valid.frequency == x)))
  
  category.pct.purchase.frequency.level <-
    category.n.purchase.frequency.level /
    category.frequency.n.valid
  
  # append total of levels 1 to 5, and total of levels 9 and 10 ("roll-up")
  # hard-coded
  
  category.pct.purchase.frequency.level[11] <-
    sum(category.pct.purchase.frequency.level[1:5])
  category.pct.purchase.frequency.level[12] <-
    sum(category.pct.purchase.frequency.level[9:10])
  
  
  output.pct.purchase.frequency <-
    category.pct.purchase.frequency.level[c(1, 2, 3, 4, 5, 11, 6, 7,8,12)]
  
  # hard-coded labels:
  names(output.pct.purchase.frequency) <-
    c(
      "Daily",
      "Several Times/Week",
      "Weekly",
      "Every 2-3 weeks",
      "Monthly",
      "NET Monthly or More Frequently",
      "Every 2-3 Months",
      "Every 4-6 Months",
      "Every 7-12 Months",
      "<1 x Year"
    )
  
  result <- list("n.valid" = category.frequency.n.valid,
                 "pct.response" = output.pct.purchase.frequency)
}

# call
out.slide6a.c2.Q4.catconsfreq.HB <- Q4(curr.id = "Q4", n.level = 10)


Slide7.Q3 <- function(curr.id, n.level) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <-
    example.raw.data.variables[curr.variable.index]
  
  # assemble the sub-categories in order
  row.value.1 <- Row.Value(curr.variables, 99)
  row.value <- row.value.1[1:Sub_Cat.n]
  n.attribute <- Sub_Cat.n
  
  # variables in order of increasing attribute index:
  row.variable <-
    Row.Variable.Name(paste(curr.id, "r", sep = ""), row.value)
  
  # pull in the example data:
  curr.data <- Data.Value(row.variable)
  
  curr.count <- Data.Level.Count(curr.data,  n.level)
  
  curr.pct.response <- curr.count$pct.response
  
  # roll up some of the levels for output
  out.pct.response <-
    rbind(
      curr.pct.response[1:3, ],
      colSums(curr.pct.response[1:3, ]),
      curr.pct.response[4:6, ],
      colSums(curr.pct.response[7:8, ])
    )
  
  rownames(out.pct.response) <-
    c(
      "Weekly",
      "Every 2-3 Weeks",
      "Monthly",
      "NET Monthly +",
      "Every 2-3 Months",
      "Every 4-6 Months",
      "Every 7-12 Months",
      "<1 x Year"
    )
  
  data.map.index <- match(row.variable, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index, ]
  
  colnames(out.pct.response) <- data.map.variable$rowTitle
  
  result <- list("n.valid" = curr.count$n.valid,
                 "pct.response" = out.pct.response)
}


out.slide7.Q3.subcatpurchfreq.HB <-
  Slide7.Q3(curr.id = "Q3", n.level = 8)

####END SLIDE 7

Slide7a.Q4 <- function(curr.id, n.level) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <-
    example.raw.data.variables[curr.variable.index]
  
  # assemble the sub-categories in order
  row.value.1 <- Row.Value(curr.variables, 99)
  row.value <- row.value.1[1:Sub_Cat.n]
  
  # variables in order of increasing attribute index:
  row.variable <-
    Row.Variable.Name(paste(curr.id, "r", sep = ""), row.value)
  
  # pull in the example data:
  curr.data <- Data.Value(row.variable)
  
  curr.count <- Data.Level.Count(curr.data,  n.level)
  
  curr.n.response <- curr.count$n.response
  curr.pct.response <- curr.count$pct.response
  
  # roll up some of the levels for output
  out.n.response <-
    rbind(
      curr.n.response[1:5, ],
      colSums(curr.n.response[1:5, ]),
      curr.n.response[6:8, ],
      colSums(curr.n.response[9:10, ])
    )
  
  out.pct.response <-
    rbind(
      curr.pct.response[1:5, ],
      colSums(curr.pct.response[1:5, ]),
      curr.pct.response[6:8, ],
      colSums(curr.pct.response[9:10, ])
    )
  
  out.row.names <- 
    c(
      "Daily",
      "Several Times/Week",
      "Weekly",
      "Every 2-3 Weeks",
      "Monthly",
      "NET Monthly or More",
      "Every 2-3 Months",
      "Every 4-6 Months",
      "Every 7-12 Months",
      "<1 x Year"
    )
  
  rownames(out.n.response) <- out.row.names
  rownames(out.pct.response) <- out.row.names
  
  data.map.index <- match(row.variable, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index, ]
  
  colnames(out.n.response) <- data.map.variable$rowTitle
  colnames(out.pct.response) <- data.map.variable$rowTitle
  
  # statistical significance testing of each brand vs each other brand
  # at each consumption frequency level
  # Pearson's Chi-Square
  
  significance <- out.n.response
  significance[,] <- ""
  
  curr.n.valid <- curr.count$n.valid
  out.n.row <- nrow(out.n.response)
  
  letters <- rawToChar(as.raw(65:(64 + Sub_Cat.n)))
  
  for (rx in 1:out.n.row) {
    for (cx in 1:(Sub_Cat.n-1)) {
      n.1 <- curr.n.valid[cx]
      s.1 <- out.n.response[rx, cx]
      
      letter.1 <- substr(letters, cx, cx)
      
      for (dx in (cx+1):Sub_Cat.n) {
        n.2 <- curr.n.valid[dx]
        s.2 <- out.n.response[rx, dx]
        
        letter.2 <- substr(letters, dx, dx)
        
        curr.test <- 
          prop.test(c(s.1, s.2), c(n.1, n.2), alternative = "greater")
        curr.p <- curr.test$p.value
        
        if (curr.p < 0.05) {
          significance[rx, cx] <-
            paste(significance[rx, cx], letter.2, sep = "")
        }
        
        if (curr.p > 0.95) {
          significance[rx, dx] <-
            paste(significance[rx, dx], letter.1, sep = "")
        }
      }
    }
  }
  
  result <- list("n.valid" = curr.n.valid,
                 "pct.response" = out.pct.response,
                 "significance" = significance)
}


out.slide7a.Q4.subcatconsfreq.HB <-
  Slide7a.Q4(curr.id = "Q4", n.level = 10)

####END SLIDE 7a

# Slide 9 Q6 importance

# Q6 Importance of 23 attributes

Q6 <- function(curr.id, n.level, report.level) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  # find the variables that begin with the pattern
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <-
    example.raw.data.variables[curr.variable.index]
  # the variables may not be sorted correctly by attribute index
  
  # get sorted attribute indexes:
  row.value <- Row.Value(curr.variables)
  n.attribute <- length(row.value)
  
  # variables in order of increasing attribute index:
  row.variable <-
    Row.Variable.Name(paste(curr.id, "r", sep = ""), row.value)
  
  # get responses as a data.frame; one column for each attribute
  curr.data <- Data.Value(row.variable)
  
  curr.count <- Data.Level.Count(curr.data, n.level)
  
  curr.pct.response <- curr.count$pct.response
  
  data.map.index <-grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index,]
  level.string <- data.map.variable$values[1][[1]]$title
  
  # attach row names
  
  # sort by response percentage of top two levels
  
  report.pct.response.select <- curr.count$pct.response[report.level,]
  row.names(report.pct.response.select) <- level.string[report.level]
  
  
  report.pct.response.top.2 <- 
    report.pct.response.select[1,] +
    report.pct.response.select[2,]
  row.names(report.pct.response.top.2) <-
    paste(n.level-1, "+", n.level, sep="")
  
  top.2.order <- order(report.pct.response.top.2, decreasing = TRUE)
  
  report.pct.response <- 
    rbind(report.pct.response.select, report.pct.response.top.2)
  
  curr.row.title <- data.map.variable$rowTitle[top.2.order]
  
  result <- list(
    "n.valid" = curr.count$n.valid[top.2.order],
    "n.response" = curr.count$n.response[, top.2.order],
    "pct.response" = report.pct.response[, top.2.order],
    "top.2.order" = top.2.order,
    "question" = curr.row.title
  )
}


out.slide9.Q6.importance.VSB <- Q6(curr.id = "Q6", n.level = 3, report.level = 2:3)

#### BEGIN SLIDE 10 CHART 2 AIDED BRAND AWARENESS

# aided brand awareness
# slide 10, chart 2
# q8r
# vertical bar chart (sorted means)

Q8 <- function(curr.id, max.valid.row) {
  curr.pattern <- paste(curr.id, "r.+", sep="")
  
  # find the variables that begin with the pattern
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <-
    example.raw.data.variables[curr.variable.index]
  # the variables may not be sorted correctly by brand index
  # and may include unwanted brands (in particular, 'None of the Above')
  
  row.value <- Row.Value(curr.variables, max.valid.row)
  n.attribute <- length(row.value)
  
  # variables in order of increasing attribute index:
  row.variable <-
    Row.Variable.Name(paste(curr.id, "r", sep = ""), row.value)
  
  # get responses as a data.frame; one column for each attribute
  curr.data <- Data.Value(row.variable)
  
  curr.count <- Data.Level.Count(curr.data, 1)
  
  curr.n.response <- curr.count$n.response
  curr.pct.response <- curr.count$pct.response
  
  data.map.index <-grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index,]
  brand.name <- data.map.variable$rowTitle[1:n.attribute]
  
  #   brand.name <- t(as.data.frame(data.map.variable$rowTitle[1:n.attribute]))
  
  colnames(curr.n.response) <- t(brand.name)
  colnames(curr.pct.response) <- t(brand.name)
  aided.order <- order(curr.pct.response, decreasing = TRUE)
  
  key.brands.pct.response <- curr.pct.response[which(KeyBrands.values[1,]==1)]
  key.brands.aided.order <- order(key.brands.pct.response, decreasing = TRUE)
  
  result <- list(
    "n.valid" = curr.count$n.valid[aided.order],
    "n.response" = curr.n.response[, aided.order],
    "pct.response" = curr.pct.response[, aided.order],
    "aided.order" = aided.order,
    "key.brands.aided.order" = key.brands.aided.order
  )
}


out.slide10.c2.Q8.aidaware.VB <- Q8(curr.id = "Q8", max.valid.row = 99)



Brand.Funnel <- function(aided.sort.order,aided.sorted.results) {
  # Need to source previous slide to get order and aided awareness data
  
  
  # Brand funnel Sub_Cat
  
  # the survey variables of interest (aided brand awareness):
  
  
  
  # Q9 Brand Funnel
  q9r.variables <-
    c("Q9r1", "Q9r2", "Q9r3", "Q9r4", "Q9r5", "Q9r6")
### NOTE this is dynamic as this is user input dependant r1 to r8 
### TODO fetch dynamically 
### this can be taken from qKeyBrands all row labels starting with r+idx Throughout the report for brands

  # subset of the full dataframe containing just these variables:
  q9r.values <- result1[q9r.variables]
  q9r.values[is.na(q9r.values)]<-0
  
  # convert the response strings into numeric values (0/1):
  q9r.numeric <- lapply(q9r.values, function(x)
    as.numeric(x))
  
  # convert the list of column values into a dataframe:
  q9r.numeric.frame <- as.data.frame(q9r.numeric)
  
  # Create each chart point
  # Aided awareness already computed and stored in q8r.means
  
  # Consider = top5 = scale points 2-6 based to total
  q9r.consider.numeric.frame<-q9r.numeric.frame
  q9r.consider.numeric.frame[q9r.consider.numeric.frame==1]<-0
  q9r.consider.numeric.frame[q9r.consider.numeric.frame>1]<-1
  
  # Ever Tried = top4 = scale points 3-6 based to total
  q9r.evertried.numeric.frame<-q9r.numeric.frame
  q9r.evertried.numeric.frame[q9r.evertried.numeric.frame<3]<-0
  q9r.evertried.numeric.frame[q9r.evertried.numeric.frame>2]<-1
  
  # Lapsed = scale point 4 based to total
  q9r.lapsed.numeric.frame<-q9r.numeric.frame
  q9r.lapsed.numeric.frame[q9r.lapsed.numeric.frame<4 | q9r.lapsed.numeric.frame>4]<-0
  q9r.lapsed.numeric.frame[q9r.lapsed.numeric.frame==4]<-1
  
  # Current User = scale points 5 and 6 based to total
  q9r.currentuser.numeric.frame<-q9r.numeric.frame
  q9r.currentuser.numeric.frame[q9r.currentuser.numeric.frame<5]<-0
  q9r.currentuser.numeric.frame[q9r.currentuser.numeric.frame>=5]<-1
  
  # Loyalist = scale point 6 based to total
  q9r.loyalist.numeric.frame<-q9r.numeric.frame
  q9r.loyalist.numeric.frame[q9r.loyalist.numeric.frame<6]<-0
  q9r.loyalist.numeric.frame[q9r.loyalist.numeric.frame==6]<-1
  
  # Resp data for sig testing
  #q8r.numeric.frame,q9r.consider.numeric.frame,q9r.evertried.numeric.frame,q9r.lapsed.numeric.frame,
  #                  q9r.currentuser.numeric.frame,q9r.loyalist.numeric.frame
  
  
  # Data summary
  
  temp1 <-as.vector(aided.sorted.results)
  temp2 <- as.vector(1:6)
  temp2[aided.sort.order] <- temp1
  q8r.means <- as.data.frame(temp2)
  
  q9r.consider.pct.level <- colMeans(q9r.consider.numeric.frame)
  q9r.evertried.pct.level <- colMeans(q9r.evertried.numeric.frame)
  q9r.lapsed.pct.level <- colMeans(q9r.lapsed.numeric.frame)
  q9r.currentuser.pct.level <- colMeans(q9r.currentuser.numeric.frame)
  q9r.loyalist.pct.level <- colMeans(q9r.loyalist.numeric.frame)
  q9r.trialconversion.pct.level <- q9r.evertried.pct.level/q8r.means
  q9r.retainedtriers.pct.level <- q9r.currentuser.pct.level/q9r.evertried.pct.level
  
  # All brands in one matrix, columns sorted by aided awareness
  
  #q8r.bar.chart.data.sorted <-
  #  q8r.bar.chart.data[order(q8r.bar.chart.data$Mean, 
  #                           decreasing = TRUE),]
  
  Q9.result<-rbind('Aided Aware' = q8r.means,
                   'Consider'= q9r.consider.pct.level,
                   'Ever Tried' = q9r.evertried.pct.level,
                   'Lapsed'=q9r.lapsed.pct.level,
                   'Loyalist'=q9r.loyalist.pct.level,
                   'Retained Triers'=q9r.retainedtriers.pct.level)
  
  Q9.result.sorted<-Q9.result[,aided.sort.order]
  
  colnames(Q9.result.sorted) <- colnames(aided.sorted.results)
  
  result<-Q9.result.sorted
  #result<-rbind(aided.results,Q9.result.sorted)
  
}
out.slide11.Q8Q9.brandfunnel.HB<-Brand.Funnel(out.slide10.c2.Q8.aidaware.VB$aided.order,
                                              out.slide10.c2.Q8.aidaware.VB$pct.response)

# We want to use output from slide 10 -- call into slide 11 wrapper function
# aided awareness results already sorted
# sort order based on aided awareness

#out.slide10.c2.Q8.aidaware.VB$aided.order
#out.slide10.c2.Q8.aidaware.VB$pct.response

# Hmm this doesn't work -- CAN'T FIGURE OUT HOW TO SORT

# q9.brandfunnel.results.sorted<-q9.brandfunnel.results[order(q9r.brandfunnel.results[1,],decreasing = TRUE,]

# Next need to do sig testing  NEEDS TO BE DONE

# Slide 15/17 and 16/18 Category Brand Performance
# Q15, divided into two (top 13 and bottom 10) based on importance



# Slide 15/17 and 16/18 Category Brand Performance
# Q15, divided into two (top 13 and bottom 10) based on importance

#source('Initialization20200626.R')


Q15 <- function (curr.id, question.order, aided.order) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  # find the variables that begin with the pattern
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <- example.raw.data.variables[curr.variable.index]
  # 115 strings of the form Q15rXcY
  # Y = 99 is not used
  # want to reorganize as a table
  
  # find and eliminate the '99's
  valid.variable.index <- grep("99", curr.variables, invert = TRUE)
  curr.variables <- curr.variables[valid.variable.index]
  
  r.values <- sort(union(NULL,
                         as.numeric(sub(
                           "c.*", "", sub(".*r", "", curr.variables)
                         ))))
  
  c.values <- sort(union(NULL,
                         as.numeric(sub(
                           ".*c", "", curr.variables
                         ))))
  
  n.attribute <- length(r.values)
  n.brand <- length(c.values)
  
  brand.name <- list()
  attribute <- list()
  
  curr.n.valid <- matrix(0, n.attribute, n.brand)
  curr.n.response <- matrix(0, n.attribute, n.brand)
  
  for (rx in 1:n.attribute) {
    prefix <- paste(curr.id, "r", r.values[rx], sep = "")
    for (cx in 1:n.brand) {
      curr.variable <- paste(prefix, "c", c.values[cx], sep = "")
      curr.map.index <-
        grep(curr.variable, example.data.map.variables$label)
      curr.map.entry <- example.data.map.variables[curr.map.index, ]
      curr.response <- example.raw.data[curr.variable]
      curr.n.valid[rx, cx] <-
        sum(!is.na(curr.response), na.rm = TRUE)
      curr.n.response[rx, cx] <-
        sum(curr.response == "1", na.rm = TRUE)
      
      if (rx == 1) {
        brand.name[cx] <- curr.map.entry$colTitle
      }
      
      if (cx == 1) {
        attribute[rx] <- curr.map.entry$rowTitle
      }
    }
  }
  
  brand.name.vector <- as.vector(brand.name)
  curr.brand.sample.size <- as.data.frame(curr.n.valid[1,])
  colnames(curr.brand.sample.size) <- "Sample Size"
  rownames(curr.brand.sample.size) <- brand.name.vector
  
  curr.pct.response <-
    as.data.frame(curr.n.response / curr.n.valid)
  rownames(curr.pct.response) <- attribute
  colnames(curr.pct.response) <- brand.name
  
  sort.n.valid <- curr.n.valid[question.order, aided.order]
  sorted.n.response <- curr.n.response[question.order, aided.order]
  sorted.pct.response.1 <- curr.pct.response[question.order,]
  sorted.pct.response <- sorted.pct.response.1[, aided.order]
  sorted.brand.sample.size <- curr.brand.sample.size[aided.order,]
  
  # significance testing of each brand vs each other brand for each question
  # Pearson's Chi-Square
  
  significance <- sorted.pct.response
  significance[, ] <- "" # clear the contents of significance
  
  letters <- rawToChar(as.raw(65:(64 + n.brand)))
  
  for (rx in 1:n.attribute) {
    # fill the data.frame with significance results
    for (cx in 1:(n.brand - 1)) {
      n.1 <- sort.n.valid[rx, cx]
      s.1 <- sorted.n.response[rx, cx]
      
      letter.1 <- substr(letters, cx, cx)
      
      for (dx in (cx + 1):n.brand) {
        n.2 <- sort.n.valid[rx, dx]
        s.2 <- sorted.n.response[rx, dx]
        
        letter.2 <- substr(letters, dx, dx)
        
        curr.test <- prop.test(c(s.1, s.2), c(n.1, n.2),
                               alternative = "greater")
        
        curr.p <- curr.test$p.value
        
        if (curr.p < 0.05) {
          significance[rx, cx] <-
            paste(significance[rx, cx], letter.2, sep = "")
        }
        
        if (curr.p > 0.95) {
          significance[rx, dx] <-
            paste(significance[rx, dx], letter.1, sep = "")
        }
        
        # if (curr.p < 0.025) {
        #   significance[rx, dx] <-
        #     paste(significance[rx, dx], letter.1, sep = "")
        # }
        # 
        # if (curr.p > 0.975) {
        #   significance[rx, cx] <-
        #     paste(significance[rx, cx], letter.2, sep = "")
        # }
      }
    }
  }
  
  result <- list(pct.response = sorted.pct.response,
                 brand.sample.size = sorted.brand.sample.size,
                 significance = significance)
}


# need to process importance first, in order to sort the questions
#source('slide9_Q6_importance_VSB_20200628.r')
# q6.result$top.2.order contains the question importance order indexes

#source('slide10_c2_Q8_aidaware_VB_20200628.r')
# q8.result$key.brands.aided.order contains the aided brand awareness order
# indexes for the key brands


#### out.slide1516.Q15.brandperf.UK <-
####   Q15(
####     curr.id = "Q15",
####     question.order = out.slide9.q6.importance.VSB$top.2.order,
####     aided.order = 1:KeyBrands.n
####   )

#out.slide9.q6.importance.VSB$top.2.order

# if key brands need to be sorted by aided awareness:
# aided.order = out.slide10.c2.Q8.aidaware.VB$key.brands.aided.order

# Is this a crummy way to put together the data?

#Q15.for.reporting<-curr.pct.response[2:3,]
#Q15.top2<-curr.pct.response[2,]+curr.pct.response[3,]
#Q15.for.reporting<-rbind(Q15.for.reporting,Q15.top2)

# NEEDS TO BE SORTED AND MATCHED UP WITH CURR.ROW.TITLE

# data.map.1 <- data.map[[1]]
# curr.label.index <- grep(curr.pattern, data.map.1$label)
# curr.data.map <- data.map.1[curr.label.index,]
# curr.row.title <- curr.data.map[["rowTitle"]]

# need to sort the attributes in order of (slide 9) Q6 importance,
#   and split into two screens
# need to sort the brands left to right by aided awareness

# need to sig test
# need to pass in sample sizes
# need shortened labels, maybe




####FORMATING EXAMPLES###


## we need to reformat
#                                 data,
#                                 title,
#                                 baseSize, 
#                                 questionID,
#                                 keyOrder="",
#                                 xAxisTitle="", 
#                                 yAxisTitle="",
#                                 chartType="pie",
#                                 colors=c("#1f78b4", "#a6cee3"),
#                                 orientation="v",
#                                 confidenceInterval="false", 
#                                 dataValue="percent", 
#                                 average='mean',
#                                 dataType="1d"
genderFormatted <- returnChartDataAndMetaData(
  out.slide4.r1c1.S3.gender.PC[['data']],
  'Gender',
  out.slide4.r1c1.S3.gender.PC[['baseSize']],
  out.slide4.r1c1.S3.gender.PC[['questionID']],
  c('Male','Female'),
  '',
  '',
  'pie',
  c('#00b1ac','#99ca3c')
)

regionFormatted <- returnChartDataAndMetaData(
  out.slide4.r1c2.region_quota.MAP[['data']],
  'Region',
  out.slide4.r1c2.region_quota.MAP[['baseSize']],
  out.slide4.r1c2.region_quota.MAP[['questionID']],
  out.slide4.r1c2.region_quota.MAP[['keyOrder']],
  '',
  '',
  'bar',
  c('#00b1ac','#0398d3','#99ca3c','#36d2b4'),
  'h'
)


### NOTE we need one output object
processedData <- list(
    gender=genderFormatted, #
    region=regionFormatted, #
    D3=out.slide4.r1c3.D3.income.VSB, #
    ethnicity=out.slide4.r1c4.S5S6.ethnicity.VSB,#
    houseHoldSize=out.slide4.r2c1.D1.HHSize.UK, #
    houseHoldComposition=out.slide4.r2c2.HHCompr.HHComp.HB,#
    generation=out.slide4.r2c3.S4.generations.VSB,#
    urban=out.slide4.r2c4.D4.urbanicity.VSB,#
    sustainability=out.slide5.Q37.sustainability.VSB,#
    recency=out.slide6.c1.Q1.catpurchrec.HB, # slide 6 recency
    frequency=out.slide6.c2.Q3.catpurchfreq.HB, # slide 6 frequency
    recency2=out.slide6A.c1.Q2.catconsrec.HB, # slide 6 recency again?
    categoryFrequency=out.slide6a.c2.Q4.catconsfreq.HB, # frequencies of categories?
    subCategoryPurchaseFrequency=out.slide7.Q3.subcatpurchfreq.HB, # this was already working in the previous version
    subCategoryConsumptionFrequency=out.slide7a.Q4.subcatconsfreq.HB, # this was already working in previous version
    attributeImportance=out.slide9.Q6.importance.VSB, #
    aidedAwareness=out.slide10.c2.Q8.aidaware.VB,#
    slide11=out.slide11.Q8Q9.brandfunnel.HB #, ####slide1516=out.slide1516.Q15.brandperf.UK,
)

processedDataJSON <- toJSON(processedData, pretty=TRUE, auto_unbox=TRUE)
if (debug){
   lapply(processedDataJSON, write,"./RscriptTests/crauV2.json")
}




