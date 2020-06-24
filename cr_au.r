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

# load survey results into R dataframe:
result1 <- fromJSON(dataFile)
# load the json data map into R
result2 <- fromJSON(dataMap)

# result2 contains two elements (each element is some type of data structure)
result2.1 <- result2[[1]]
result2.1.label <- result2.1[["label"]]
result2.1.rowtitle <- result2.1[["rowTitle"]]

### NOTE:
### getting the Main category name for further reuse this will always be Main_Catr1
Main_Cat.index <- grep("Main_Catr1", result2.1.label)
Main_Cat <- result2.1.rowtitle[Main_Cat.index]

#####################################
### Slides Purchase Recency START ###
#####################################

q1.variables <- "Q1"
q1.values <- result1[q1.variables]
q1.numeric <- lapply(q1.values, function(x) as.numeric(x)) # a list
q1.numeric.frame <- as.data.frame(q1.numeric)

# count the number of responses for each variable

q1.n.max <- nrow(q1.numeric.frame)
q1.n.valid <- q1.n.max - colSums(is.na(q1.numeric.frame))

q1.recency.valid <- ! is.na(q1.numeric.frame)
q1.valid.recency <- q1.numeric.frame[q1.recency.valid]

q1.pct.purchase.recency.level <-
  unlist(lapply(1:8, function(x) sum(q1.valid.recency==x))) / q1.n.valid

# append total of levels 1 to 3, and total of levels 7 and 8

q1.pct.purchase.recency.level[9] <-
  sum(q1.pct.purchase.recency.level[1:3])
q1.pct.purchase.recency.level[10] <-
  sum(q1.pct.purchase.recency.level[7:8])

keyOrder <-c("Past Week", "Past 2-3 Weeks", "Past Month", 
               "NET Monthly or More Recently",
               "Past 2-3 Months")

q1.meta.data <- list(
  confidenceInterval =
    'false',
  dataValue = 'percent',
  baseSize = max(q1.n.valid),
  average = 'mean',
  chartType = 'bar',
  orientation = 'h',
  dataType = '1d',
  keyOrder = keyOrder,
  colors = c(replicate(3, "Green"), "Grey", "Green")
)

q1.bar.chart.data <- data.frame("attribute" = keyOrder, "value"=q1.pct.purchase.recency.level[c(1,2,3,9,4)])
### NOTE:
### We need this format attribute and value as a pair for our front end charts to render
question.id <- "Q1"
x.axis.title <- ""
y.axis.title <- ""

result2.1.rowtitle <- result2.1[["rowTitle"]]
chart.title <- paste("Purchase Recency -",Main_Cat) # "Main_Cat" # hard-coded; example.dataMap.json does not associate
### NOTE:
### Main_Cat can be accessed via a hidden question labelled Main_Cat from the dataMap

q1.bar.chart.list <- list(
  'title' = chart.title,
  'xAxisTitle' = x.axis.title,
  'yAxisTitle' = y.axis.title,
  'questionID' = question.id,
  'metadata' = q1.meta.data,
  'data' = q1.bar.chart.data
)


############################################
### Sub Categories Start ###################
############################################

result1.names <- names(result1)
result1.q3r.index <- grep("Q3r[0-9]+", result1.names) 
result1.q3r.names <- result1.names[result1.q3r.index]

# the q3r variable names for sub-category purchase frequency
# hard-coded; how would we algorithmically identify the relevant variables?
### NOTE:
### Sub_Cat hidden question holds all sub categories consecutive questions like Q3 and Q4 have the addition of the red herrings 
### hence use Sub_Cat eg. for each row in Sub_Cat ret the row label eg.: r1-r5 and then feth the needed one for Q3 or Q4 eg.Q3r1, Q3r2
### think this is case sensitive Q3 rather than q3 ?
# q3r.variables <- c("q3r1", "q3r2", "q3r3", "q3r4", "q3r5")
q3r.values <- result1[result1.q3r.names]

q3r.numeric <- lapply(q3r.values, function(x) as.numeric(x)) # a list
q3r.numeric.frame <- as.data.frame(q3r.numeric)



q3r.category.frequency <- do.call(pmin, q3r.numeric.frame)
q3r.category.frequency.valid <- !is.na(q3r.category.frequency)
q3r.category.valid.frequency <-
q3r.category.frequency[q3r.category.frequency.valid]

# count the number of responses for each variable

q3r.category.frequency.n.max <- length(q3r.category.frequency)
q3r.category.frequency.n.valid <- length(q3r.category.valid.frequency)

# 8 purchase frequency levels (hard-coded)

q3r.category.n.purchase.frequency.level <-
  unlist(lapply(1:8, function(x) sum(q3r.category.valid.frequency==x)))

q3r.category.pct.purchase.frequency.level <-
  q3r.category.n.purchase.frequency.level /
  q3r.category.frequency.n.valid

# append total of levels 1 to 3, and total of levels 7 and 8

q3r.category.pct.purchase.frequency.level[9] <-
  sum(q3r.category.pct.purchase.frequency.level[1:3])
q3r.category.pct.purchase.frequency.level[10] <-
  sum(q3r.category.pct.purchase.frequency.level[7:8])

# which columns contain the purchase frequency levels
q3r.label.index <- grep("Q3r[0-9]+", result2.1.label)

# collect the corresponding subcategory names
result2.1.rowtitle <- result2.1[["rowTitle"]]
result2.1.rowtitle.q3r <- result2.1.rowtitle[q3r.label.index]

# only the first five are real; the other four are fake
# hard-code selection of the valid names
### NOTE: 
### the number of real vs fake can be determined from the number of rows of Sub_Cat rows.length

result2.1.q3r.valid.name <- result2.1.rowtitle.q3r[1:5] # the number of real vs fake can be determined from the number of rows of Sub_Cat rows.length

q3r.meta.data <- list(
  confidenceInterval =
    'false',
  dataValue = 'percent',
  baseSize = 1015, #  max(q3r.n.valid),  ###NOTE: this throws an error cannot be found
  average = 'mean',
  chartType = 'bar',
  orientation = 'h',
  dataType = '1d',
  keyOrder = c("Weekly", "Every 2-3 Weeks", "Monthly", 
               "NET Monthly or More Frequently",
               "Every 2-3 Months", "Every 4-6 Months", "Every 7-12 Months",
               "<1 x Year"),
  colors = c(replicate(3, "Green"), "Grey", replicate(4, "Green"))
)

q3r.category.bar.chart.data <- q3r.category.pct.purchase.frequency.level[c(1,2,3,9,4,5,6,10)]

question.id <- "Q3"
x.axis.title <- ""
y.axis.title <- ""

chart.title <- Main_Cat 
### NOTE:
### we can derive the main category value from the hidden question Main_Cat
### Not sure I understand this chart why it only has the main category as title 
### don't we need one chart per row of Q3 ?

q3r.category.bar.chart.list <- list(
  'title' = chart.title,
  'xAxisTitle' = x.axis.title,
  'yAxisTitle' = y.axis.title,
  'questionID' = question.id,
  'metadata' = q3r.meta.data,
  'data' = q3r.category.bar.chart.data)




################################################
#### Purchase Frequency sub categories START ###
################################################

# the q3r variable names for sub-category purchase frequency
# hard-coded; how would we algorithmically identify the relevant variables?
### NOTE:
### Sub_Cat hidden question holds all sub categories consecutive questions like Q3 and Q4 have the addition of the red herrings 
### hence use Sub_Cat eg. for each row in Sub_Cat ret the row label eg.: r1-r5 and then feth the needed one for Q3 or Q4 eg.Q3r1, Q3r2
q3r.variables <- c("Q3r1", "Q3r2", "Q3r3", "Q3r4", "Q3r5")
q3r.values <- result1[q3r.variables]
q3r.numeric <- lapply(q3r.values, function(x) as.numeric(x)) # a list
q3r.numeric.frame <- as.data.frame(q3r.numeric)

# count the number of responses for each variable

q3r.n.max <- nrow(q3r.numeric.frame)
q3r.n.valid <- q3r.n.max - colSums(is.na(q3r.numeric.frame))

# 8 purchase frequency levels (hard-coded)

# allocate a data.frame with named columns
q3r.n.purchase.frequency.level <- as.data.frame(q3r.numeric.frame[0,], row.names=NULL)
q3r.pct.purchase.frequency.level <- as.data.frame(q3r.numeric.frame[0,], row.names=NULL)

# q3r.n.purchase.frequency.level <- data.frame(q3r.numeric.frame[,])

for (ix in 1:8) {
  q3r.n.purchase.frequency.level[ix,] <-
    apply(q3r.numeric.frame, 2, function(x)
      sum(x == ix, na.rm = TRUE))
  
  q3r.pct.purchase.frequency.level[ix,] <-
    q3r.n.purchase.frequency.level[ix,] / q3r.n.valid
}

# append total of levels 1 to 3, and total of levels 7 and 8

q3r.pct.purchase.frequency.level <-
  rbind(
    q3r.pct.purchase.frequency.level,
    colSums(q3r.pct.purchase.frequency.level[1:3,]),
    colSums(q3r.pct.purchase.frequency.level[7:8, ])
  )

# which columns contain the purchase frequency levels
q3r.label.index <- grep("Q3r[0-9]+", result2.1.label)

# collect the corresponding subcategory names
result2.1.rowtitle <- result2.1[["rowTitle"]]
result2.1.rowtitle.q3r <- result2.1.rowtitle[q3r.label.index]

# only the first five are real; the other four are fake
# hard-code selection of the valid names
### NOTE:
### Sub_Cat hidden question holds all sub categories consecutive questions like Q3 and Q4 have the addition of the red herrings 
### hence use Sub_Cat eg. for each row in Sub_Cat ret the row label eg.: r1-r5 and then feth the needed one for Q3 or Q4 eg.Q3r1, Q3r2
### The lenght of Sub_Cat rows can be used for valid names in case not already handelled via reading the rows and row values from Sub_Cat
result2.1.q3r.valid.name <- result2.1.rowtitle.q3r[1:5]

keyOrder <- c("Weekly", "Every 2-3 Weeks", "Monthly", "NET Monthly +",
               "Every 2-3 Months", "Every 4-6 Months", "Every 7-12 Months",
               "<1 x Year")

q3r.meta.data <- list(
  confidenceInterval =
    'false',
  dataValue = 'percent',
  baseSize = max(q3r.n.valid),
  average = 'mean',
  chartType = 'bar',
  orientation = 'h',
  dataType = '1d',
  keyOrder = keyOrder,
  colors = c(replicate(3, "Green"), "Grey", replicate(4, "Green"))
)
q3r.bar.chart.data <- q3r.pct.purchase.frequency.level[c(1,2,3,9,4,5,6,10),]
### NOTE:
### We need to map this to attribute and value for our reporting display engine to be able to interpret this

question.id <- "Q3"
x.axis.title <- ""
y.axis.title <- ""

purchaseFrequencySubCategory = list()
### NOTE: the below needs to be dynamic based of number of rows for question Sub_Cat
for (ix in 1:5) {
  chart.title <- result2.1.q3r.valid.name[ix]
  
  q3r.bar.chart.list <- list(
    'title' = chart.title,
    'xAxisTitle' = x.axis.title,
    'yAxisTitle' = y.axis.title,
    'questionID' = question.id,
    'metadata' = q3r.meta.data,
    'data' = data.frame("attribute" = keyOrder, "value"=q3r.bar.chart.data[, ix])
    ### NOTE:
    ### We need to map this to attribute and value for our reporting display engine to be able to interpret this

  )

  purchaseFrequencySubCategory[[ix]] <- q3r.bar.chart.list

}


################################################
#### Purchase Frequency sub categories END ###
################################################


######################
### Slide Main and sub Categories  END   ###
### Purchase Recency END ###
######################


##################################
#### Consumption Recency START ###
##################################


q2.variables <- "Q2"
q2.values <- result1[q2.variables]
q2.numeric <- lapply(q2.values, function(x) as.numeric(x)) # a list
q2.numeric.frame <- as.data.frame(q2.numeric)

# count the number of responses for each variable

q2.n.max <- nrow(q2.numeric.frame)
q2.n.valid <- q2.n.max - colSums(is.na(q2.numeric.frame))

q2.recency.valid <- ! is.na(q2.numeric.frame)
q2.valid.recency <- q2.numeric.frame[q2.recency.valid]

# only 4 levels; hard-coded

q2.pct.consumption.recency.level <-
  unlist(lapply(1:4, function(x) sum(q2.valid.recency==x))) / q2.n.valid

# append total of levels 1 to 3

q2.pct.consumption.recency.level[5] <-
  sum(q2.pct.consumption.recency.level[1:3])

keyOrder <- c("Past Week", "Past 2-3 Weeks", "Past Month", 
               "NET Monthly or More Recently",
               "Past 2-3 Months")
q2.meta.data <- list(
  confidenceInterval =
    'false',
  dataValue = 'percent',
  baseSize = max(q2.n.valid),
  average = 'mean',
  chartType = 'bar',
  orientation = 'h',
  dataType = '1d',
  keyOrder = keyOrder,
  colors = c(replicate(3, "Green"), "Grey", "Green")
)
#data.frame("attribute" = keyOrder, "value"=q1.pct.purchase.recency.level[c(1,2,3,9,4)])
q2.bar.chart.data <- data.frame("attribute" = keyOrder, "value"=q2.pct.consumption.recency.level[c(1,2,3,5,4)])

question.id <- "Q2"
x.axis.title <- ""
y.axis.title <- ""

chart.title <- paste("Consumption Recency -",Main_Cat) # do not hard-code
#Main_Cat comes from the question Main_Cat

q2.bar.chart.list <- list(
  'title' = chart.title,
  'xAxisTitle' = x.axis.title,
  'yAxisTitle' = y.axis.title,
  'questionID' = question.id,
  'metadata' = q2.meta.data,
  'data' = q2.bar.chart.data
)

###################################################
#### sub Categories Consumption Frequency START ###
###################################################

## currently failing on 
# q4r.n.level ```Error in lapply(1:q4r.n.level, function(x) sum(q4r.category.valid.frequency ==  : 
#   object 'q4r.n.level' not found
# Calls: unlist -> lapply
# Execution halted

## therefore all skipped until fix is found
if(TRUE == FALSE) {
result1.q4r.index <- grep("Q4r.+", result1.names)
result1.q4r.names <- result1.names[result1.q4r.index]

# the q4r variable names for sub-category consumption frequency
# hard-coded; how would we algorithmically identify the relevant variables?
## NOTE: To get the number of rows we can use the number of rows from the question Sub_Cat which has all sub categories without red herrings
# only 5 valid sub-categories; hard-coded here

q4r.variables <- c("Q4r1", "Q4r2", "Q4r3", "Q4r4", "Q4r5")
##this above needs to be dynamic please see a example on q8r.variables <-result2.1.rowLabels[q8r.label.index]
## NOTE: To get the number of rows we can use the number of rows from the question Sub_Cat which has all sub categories without red herrings

q4r.values <- result1[q4r.variables]

q4r.numeric <- lapply(q4r.values, function(x) as.numeric(x)) # a list
q4r.numeric.frame <- as.data.frame(q4r.numeric)

q4r.n.max <- nrow(q4r.numeric.frame)
q4r.n.valid <- q4r.n.max - colSums(is.na(q4r.numeric.frame))

q4r.category.frequency <- do.call(pmin, q4r.numeric.frame)
q4r.category.frequency.valid <- !is.na(q4r.category.frequency)
q4r.category.valid.frequency <-
  q4r.category.frequency[q4r.category.frequency.valid]

# count the number of responses for each variable

q4r.category.frequency.n.max <- length(q4r.category.frequency)
q4r.category.frequency.n.valid <- length(q4r.category.valid.frequency)

# 10 consumption frequency levels (hard-coded)



q4r.category.n.consumption.frequency.level <-
  unlist(lapply(1:q4r.n.level, function(x) sum(q4r.category.valid.frequency==x)))

q4r.category.pct.consumption.frequency.level <-
  q4r.category.n.consumption.frequency.level /
  q4r.category.frequency.n.valid

# append total of levels 1 to 3, and total of levels 7 and 8

q4r.category.pct.consumption.frequency.level[11] <-
  sum(q4r.category.pct.consumption.frequency.level[1:5])
q4r.category.pct.consumption.frequency.level[12] <-
  sum(q4r.category.pct.consumption.frequency.level[9:10])

# q4r.label <- grep("q4r.+", result1.label)

# which columns contain the consumption frequency levels
q4r.label.index <- grep("q4r[0-9]+", result2.1.label)

# collect the corresponding subcategory names
result2.1.rowtitle <- result2.1[["rowTitle"]]
result2.1.rowtitle.q4r <- result2.1.rowtitle[q4r.label.index]

# only the first five are real; the other four are fake
# hard-code selection of the valid names

result2.1.q4r.valid.name <- result2.1.rowtitle.q4r[1:5]

q4r.meta.data <- list(
  confidenceInterval =
    'false',
  dataValue = 'percent',
  baseSize = max(q4r.n.valid),
  average = 'mean',
  chartType = 'bar',
  orientation = 'h',
  dataType = '1d',
  keyOrder = c("Daily", "Several Times/Week", "Weekly", "Every 2-3 Weeks", 
               "Monthly", "NET Monthly or More",
               "Every 2-3 Months", "Every 4-6 Months", "Every 7-12 Months",
               "<1 x Year"),
  colors = c(replicate(5, "Green"), "Grey", replicate(3, "Green"), "Black")
)

q4r.category.bar.chart.data <- 
  q4r.category.pct.consumption.frequency.level[c(1,2,3,4,5,11,6,7,8,12)]

question.id <- "Q4"
x.axis.title <- ""
y.axis.title <- ""

chart.title <- "Consumption Frequency" 
# hard-coded; example.dataMap.json does not associate
# category name ("Bread") with Q4

q4r.category.bar.chart.list <- list(
  'title' = chart.title,
  'xAxisTitle' = x.axis.title,
  'yAxisTitle' = y.axis.title,
  'questionID' = question.id,
  'metadata' = q4r.meta.data,
  'data' = q4r.category.bar.chart.data
)
}


result1.q4r.index <- grep("Q4r.+", result1.names)
result1.q4r.names <- result1.names[result1.q4r.index]

# the q4r variable names for sub-category consumption frequency
# hard-coded; how would we algorithmically identify the relevant variables?

# only 5 valid sub-categories; hard-coded here

q4r.variables <- c("Q4r1", "Q4r2", "Q4r3", "Q4r4", "Q4r5")

q4r.values <- result1[q4r.variables]
q4r.numeric <- lapply(q4r.values, function(x) as.numeric(x)) # a list
q4r.numeric.frame <- as.data.frame(q4r.numeric)

# count the number of responses for each variable

q4r.n.max <- nrow(q4r.numeric.frame)
q4r.n.valid <- q4r.n.max - colSums(is.na(q4r.numeric.frame))

# 10 consumption frequency levels (hard-coded)

q4r.n.level <- 10

# allocate a data.frame with named columns
q4r.n.consumption.frequency.level <- as.data.frame(q4r.numeric.frame[0,], row.names=NULL)
q4r.pct.consumption.frequency.level <- as.data.frame(q4r.numeric.frame[0,], row.names=NULL)

# q4r.n.consumption.frequency.level <- data.frame(q4r.numeric.frame[,])

for (ix in 1:q4r.n.level) {
  q4r.n.consumption.frequency.level[ix,] <-
    apply(q4r.numeric.frame, 2, function(x)
      sum(x == ix, na.rm = TRUE))
  
  q4r.pct.consumption.frequency.level[ix,] <-
    q4r.n.consumption.frequency.level[ix,] / q4r.n.valid
}

# append total of levels 1 to 5, and total of levels 9 and 10

q4r.pct.consumption.frequency.level <-
  rbind(
    q4r.pct.consumption.frequency.level,
    colSums(q4r.pct.consumption.frequency.level[1:5,]),
    colSums(q4r.pct.consumption.frequency.level[9:10, ])
  )

# load the json data map into R
# result2 contains two elements (each element is some type of data structure)

# first element contains the brand names in one of the columns:

# q4r.label <- grep("Q4r.+", result1.label)


result2.1 <- result2[[1]]
result2.1.label <- result2.1[["label"]]

# which columns contain the consumption frequency levels
q4r.label.index <- grep("Q4r[0-9]+", result2.1.label)

# collect the corresponding subcategory names
result2.1.rowtitle <- result2.1[["rowTitle"]]
result2.1.rowtitle.q4r <- result2.1.rowtitle[q4r.label.index]

# only the first five are real; the other four are fake
# hard-code selection of the valid names

result2.1.q4r.valid.name <- result2.1.rowtitle.q4r[1:5]
keyOrder <- c("Daily", "Several Times/Week", "Weekly", "Every 2-3 Weeks", 
    "Monthly", "NET Monthly or More",
               "Every 2-3 Months", "Every 4-6 Months", "Every 7-12 Months",
               "<1 x Year")
q4r.meta.data <- list(
  confidenceInterval =
    'false',
  dataValue = 'percent',
  baseSize = max(q4r.n.valid),
  average = 'mean',
  chartType = 'bar',
  orientation = 'h',
  dataType = '1d',
  keyOrder = keyOrder,
  colors = c(replicate(5, "Green"), "Grey", replicate(3, "Green"), "Black")
)

q4r.bar.chart.data <- q4r.pct.consumption.frequency.level[c(1,2,3,4,5,11,6,7,8,12),]
### NOTE:
### We need the attribute and value as keys for our chart display engine to be able to render these charts
question.id <- "Q4"
x.axis.title <- ""
y.axis.title <- ""

consumptionFrequencySubCategory = list()
### NOTE: the below needs to be dynamic based of number of rows for question Sub_Cat
for (ix in 1:5) {
  chart.title <- result2.1.q4r.valid.name[ix]
  
  q4r.bar.chart.list <- list(
    'title' = chart.title,
    'xAxisTitle' = x.axis.title,
    'yAxisTitle' = y.axis.title,
    'questionID' = question.id,
    'metadata' = q4r.meta.data,
    'data' = data.frame("attribute" = keyOrder, "value"=q4r.bar.chart.data[, ix])
    ### NOTE:
    ### We need to map this to attribute and value for our reporting display engine to be able to interpret this

  )
    
  consumptionFrequencySubCategory[[ix]] <- q4r.bar.chart.list
}




#################################################
#### sub Categories Consumption Frequency END ###
#################################################

##################################
#### Consumption Recency END   ###
##################################


######################
### Slide 12 START ###
######################

# which columns contain the aided brand awareness data
q8r.label.index <- grep("Q8r[0-9]+", result2.1.label)

# collect the corresponding brand names
result2.1.rowtitle.q8r <- result2.1.rowtitle[q8r.label.index]

# collect the number of datapoints (brands) according to label naming convention r1-rXXX dynamic based on user input
result2.1.rowLabels <- result2.1[["label"]]
q8r.variables <-result2.1.rowLabels[q8r.label.index]


# subset of the full dataframe containing just these variables (user defined brands):
q8r.values <- result1[q8r.variables]

# some respondents have empty responses for everything; exclude them:
q8r.values.valid <- na.omit(q8r.values)
# (this seems like an error)

# convert the response strings into numeric values (0/1):
q8r.numeric <- lapply(q8r.values.valid, function(x)
  as.numeric(x))

# convert the list of column values into a dataframe:
q8r.numeric.frame <- as.data.frame(q8r.numeric)

# compute means; to be reported in output for bar chart:
q8r.means <- colMeans(q8r.numeric.frame)


q8r.bar.chart.data <-
  data.frame("attribute" = result2.1.rowtitle.q8r, "value" = q8r.means)
## for our Front End D3 components we need to keep the lables as 'attribute' and 'value' instead of "Brand Name" and "Mean"
## additionally we would prefer the "_row" to be removed eg.: rownames(q8r.bar.chart.data) <- NULL
rownames(q8r.bar.chart.data) <- NULL
#sort descending
q8r.bar.chart.data <- q8r.bar.chart.data[order( -q8r.means),]
q8r.meta.data <- list(
  confidenceInterval =
    'false',
  dataValue = 'percent',
  average = 'mean',
  chartType = 'bar',
  orientation = 'h',
  dataType = '1d'
)

chart.title <- "Aided Brand Awareness"
question.id <- "Q8"
x.axis.title <- ""
y.axis.title <- ""

q8r.bar.chart.list <- list('title' = chart.title,
                           'xAxisTitle' = x.axis.title,
                           'yAxisTitle' = y.axis.title,
                           'questionID' = question.id,
                           'metadata' = q8r.meta.data,
                           'data' = q8r.bar.chart.data)

q8r.bar.chart.json <- toJSON(q8r.bar.chart.list, pretty = TRUE,
                             auto_unbox = TRUE)




######################
### Slide 12 END ###
######################





### Note: As a end output we would expect one list containing all cahrts eg.:
### processedData <- list(aidedAwareness=q8r.bar.chart.list, demoGender=qXYZ.bar.chart.list, demoRegion=qXYZ.bar.chart.list,...)
### processedDataJSON <- toJSON(processedData, pretty = TRUE, auto_unbox = TRUE )

processedData <- list(
  purchaseRecency=q1.bar.chart.list, #slide subcategorie slides
  purchaseFrequency=q3r.category.bar.chart.list, # subcategory slides
  consumptionRecency=q2.bar.chart.list, # consumption recency main category
  aidedAwareness=q8r.bar.chart.list #slide 12 chart 1
  )
for(x in 1:length(purchaseFrequencySubCategory)){
  processedData [paste('purchaseFrequency',x)]<- purchaseFrequencySubCategory[x]
}

for(x in 1:length(consumptionFrequencySubCategory)){
  processedData[paste('consumptionFrequency',x)] <- consumptionFrequencySubCategory[x]
}


processedDataJSON <- toJSON(processedData, pretty=TRUE, auto_unbox=TRUE)


if (debug){
   lapply(processedDataJSON, write,"./RscriptTests/crau.json")
}