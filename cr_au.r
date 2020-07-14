library(jsonlite)
require(ca)	# load correspondence analysis package

CR.trial <- FALSE

# set debug flag
debug <- TRUE  # FALSE to read command line inputs

if (CR.trial) {
  data.path <- switch(
    location.index,
    "C:/OneDrive/OneDrive - C+R Research/CR_Data/2020/DIY A&U Project/R Code/For delivery/for delivery 0627/",
    "/Users/gretchenhower/Documents/R Projects/datafile/",
    "At the top of this file, set location.index <- 3 and replace this string with your location"
  )
  
  setwd(data.path)
  
  # load survey results into an R data.frame:
  example.raw.data <- fromJSON("example.rawData.json")
  
  # load data map into an R data.frame
  example.data.map <- fromJSON("example.dataMap.json")
  
  setwd(curr.path)
} else {
  if (debug == FALSE) {
    dataFile <- input[[1]] # 'data.json' path to file
    dataMap  <- input[[2]]  # 'dataMam.json' path to file
  }  else  {
    # dataFile <-'./cr/301/example.rawData.json'# path to your local rawData and dataMap file you want to test eg.: /Users/gretchenhower/Documents/R Projects/datafile/example.rawData.json
    # dataMap  <-'./cr/301/example.dataMap.json'# path to your local rawData and dataMap file you want to test eg.: /Users/gretchenhower/Documents/R Projects/datafile/example.dataMap.json
    dataFile <-
      './cr/301/example.rawData.200603.json'# path to your local rawData and dataMap file you want to test eg.: /Users/gretchenhower/Documents/R Projects/datafile/example.rawData.json
    dataMap  <-
      './cr/301/example.dataMap.200603.json'# path to your local rawData and dataMap file you want to test eg.: /Users/gretchenhower/Documents/R Projects/datafile/example.dataMap.json
  }
  
  # load survey results into an R dataframe:
  example.raw.data <- fromJSON(dataFile)
  
  # load data map into an R data.frame
  example.data.map <- fromJSON(dataMap)
}

example.raw.data.variables <-
  names(example.raw.data) # 456 variable names

example.data.map.variables <- example.data.map$variables
example.data.map.questions <- example.data.map$questions

##This seems to be missing as I am getting errors result1 not found
# load survey results into R dataframe:
result1 <- example.raw.data 
# load the json data map into R
result2 <- example.data.map



########## JSON FORMATTING HELPER START #########
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

########## JSON FORMATTING HELPER END #########


##### DATAFRAME HELPERS START #######
### DataFrame Helpers start ###
createDataFrameOfTwoLists <- function(list1, list2) {
  df <- data.frame()[1:1, ]
  for(i in 1:length(list1)){
    df[[list1[i]]] <- list2[i]
  }
  rownames(df) <- NULL
  result <- df
}


# names(example.data.map.variables)
# [1] "vgroup"   "qtitle"   "colTitle" "title"    "rowTitle" "label"    "row"
# [8] "type"     "col"      "qlabel"   "values"

# names(example.data.map.questions)
# [1] "qtitle"    "variables" "values"    "qlabel"    "type"      "grouping"


# NOTE: CAHNGE needed this needs to be dynamic as we can have a variable number of sub categories?
# I believe currently the maximum is set to 5 so the below covers all cases but I am not sure if it will
# be easier to just calculate these on project by project basis? These might be only 3 or 5 etc. 

Sub_Cat.variables <-
  c("Sub_Catr1",
    "Sub_Catr2",
    "Sub_Catr3",
    "Sub_Catr4",
    "Sub_Catr5")

### NOTE we need this to by dynamic so we read this from the dataMap
result1.names <- names(example.raw.data)
Sub_Cat.variable.index <- grep("Sub_Catr[0-9]+", result1.names) 
Sub_Cat.variables <- result1.names[Sub_Cat.variable.index]

Sub_Cat.values <- example.raw.data[Sub_Cat.variables]
Sub_Cat.n <- length(Sub_Cat.variables)
# get the sub_cat indexes


# NOTE: CAHNGE needed this needs to be dynamic as we can have a variable number of Brands on a project basis up to 8?
# Currently maximum 8 brands are possible to be added on the storefront at the moment
# Please change to fetch this dynamically from the dataMap
### This number of rows will be the same as the qKeyBrands question plus the additional 3 red herrings + none of the above option
### if this is purely needed for indexes qKeyBrands could be used as well
Brands_Eval.variables <-
  c(
    "Brands_Evalr1",
    "Brands_Evalr2",
    "Brands_Evalr3",
    "Brands_Evalr4",
    "Brands_Evalr5",
    "Brands_Evalr6"
  )

Brands_Eval.values <- example.raw.data[Brands_Eval.variables]
### This Brands_Eval.values and Brands_Eval.variables seems not be used anywhere can this be removed?

# KeyBrands:  Which Brands were identified as keyBrands
# NOTE: CAHNGE needed this needs to be dynamic as we can have a variable number of Brands on a project basis up to 8?
# Currently maximum 8 brands are possible to be added on the storefront at the moment
# Please change to fetch this dynamically from the dataMap
KeyBrands.variables <-
  c(
    "qKeyBrandsr1",
    "qKeyBrandsr2",
    "qKeyBrandsr3",
    "qKeyBrandsr4",
    "qKeyBrandsr5",
    "qKeyBrandsr6"
  )

### Note we need this to be dynamic
KeyBrands.variable.index <- grep("qKeyBrandsr[0-9]+", result1.names)
KeyBrands.variables <- result1.names[KeyBrands.variable.index]

KeyBrands.values <- example.raw.data[KeyBrands.variables]

KeyBrands.values.1 <- as.numeric(KeyBrands.values[1, ])
KeyBrands.index <- which(KeyBrands.values.1 == 1)
KeyBrands.n <- length(KeyBrands.index)

# NOTE: CAHNGE needed this needs to be dynamic as we can have a variable number of Brands on a project basis up to 8?
# Currently maximum 8 brands are possible to be added on the storefront at the moment
# Please change to fetch this dynamically from the dataMap
### This number of rows (Brand_Usage) will be the same as the qKeyBrands question plus the additional 3 red herrings + none of the above option
### if this is purely needed for indexes qKeyBrands could be used as well

Brand_Usager.variables <-
  c(
    "Brand_Usager1",
    "Brand_Usager2",
    "Brand_Usager3",
    "Brand_Usager4",
    "Brand_Usager5",
    "Brand_Usager6"
  )

Brand_Usager.values <- example.raw.data[Brand_Usager.variables]
### This Brand_Usager.values and Brand_Usager.variables seems not be used anywhere can this be removed?

# data.map <- example.data.map

# general purpose functions:

Data.Value <- function (variable.name) {
  # Data.Value returns a data.frame
  # variable.name is a list of strings
  
  data.string <- example.raw.data[variable.name]
  result <-
    suppressWarnings(as.data.frame(lapply(data.string, function(x)
      as.numeric(x))))
}



Row.Value <- function (variable.name, max.valid.row = NULL) {
  # Row.Value returns a sorted list of integers
  #   found as ".*r/[0-9]*/.*" in variable.name
  # variable.name is a list of strings
  
  raw.numeric <- suppressWarnings(as.numeric(sub("c.*", "", sub(
    ".*r", "", variable.name
  ))))
  
  row.value <- sort(union(NULL, raw.numeric))
  
  if (!is.null(max.valid.row)) {
    row.value <- row.value[row.value < max.valid.row]
  }
  
  result <- row.value
}


Row.Type.Index <- function(row.string) {
  # row.string is a list of strings of the form ".*r/[0-9]*/.*"
  # Row.Type.Index returns a list with indexes of the variable names that are
  # (1) numeric, (2) non-numeric
  
  row.id <- sub("c.*", "", sub(".*r", "", row.string))
  row.numeric <- suppressWarnings(as.numeric(row.id))
  
  result <- list("numeric" = which(!is.na(row.numeric)),
                 "other" = which(is.na(row.numeric)))
}


Row.Variable.Name <- function (prefix, row.value) {
  # Row.Variable.Name returns a sorted list of strings
  #   each string is a variable name for the corresponding row
  # prefix is a string
  # row.value is a vector of integers
  
  result <- sapply(row.value, function(x)
    paste(prefix, x, sep = ""))
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
  
  n.response <- as.data.frame(numeric.data[0, ])
  pct.response <- n.response
  
  for (ix in 1:n.level) {
    n.response[ix, ] <-
      apply(numeric.data, 2, function(x)
        sum(x == ix, na.rm = TRUE))
    pct.response[ix, ] <- n.response[ix, ] / n.valid
  }
  
  result <-
    list(
      "n.max" = n.max,
      "n.valid" = n.valid,
      "n.response" = n.response,
      "pct.response" = pct.response
    )
}


Short.Label.Paren <- function (s) {
  # delete everything from the left parenthesis on, and also any trailing spaces
  # preceding the parenthesis
  
  paren.pos <- regexpr("[(]", s)
  
  if (paren.pos <= 0) {
    result <- s
  } else {
    result <- trimws(substr(s, 1, paren.pos - 1))
  }
  
  
  # paren.pos <- regexpr("[(]", s);
  # short.1 <- substr(s, 1, paren.pos-1)
  # short.2 <- trimws(short.1)
  # result <- short.2
  
}


Exact.Match.Pattern <- function(pattern) {
  result <- paste('^', pattern, '$', sep = "")
}


Row.Pattern <- function(id) {
  row.pattern <- paste(id, "r.+", sep = "")
}


Variable.From.Question <-
  function(pattern,
           fixed = FALSE,
           sort.index = FALSE) {
    #   pattern <- paste(id, "r.+", sep = "")
    if (fixed) {
      pattern <- Exact.Match.Pattern(pattern)
    }
    
    data.map.index <-
      grep(pattern, example.data.map.variables$label)
    data.map.variable <-
      example.data.map.variables[data.map.index,]
    
    variable.index <-
      grep(pattern, example.raw.data.variables, fixed = fixed)
    n.variable <- length(variable.index)
    variable <- example.raw.data.variables[variable.index]
    # note:  the variables may not be sorted row index
    
    output <- list(
      #    "id" = id,
      "pattern" = pattern,
      "n.variable" = n.variable,
      "variable.index" = variable.index,
      "variable" = variable,
      "data.map.index" = data.map.index,
      "data.map.variable" = data.map.variable
    )
    
    if (sort.index) {
      # to be coded
    }
    
    result <- output
  }


Label.And.Data.From.ID <- function(id) {
  # returns:
  #   label: a list of labels (e.g., 'Q10r1', ...)
  #   data: a data frame with named columns in 'canonical' order
  
  id.pattern <- Row.Pattern(id)
  id.variable <- Variable.From.Question(id.pattern)
  
  id.data <- Data.Value(id.variable$variable)
  names.from.data <- names(id.data)
  # these column names may be unsorted
  
  label.from.map <- id.variable$data.map.variable$label
  row.title.from.map <- id.variable$data.map.variable$rowTitle
  
  # put the column names in data map order
  data.index <- match(label.from.map, names.from.data)
  sort.data <- id.data[, data.index]
  colnames(sort.data) <- row.title.from.map
  
  result <- list(
    "label" = label.from.map,
    "data.map" = id.variable$data.map.variable,
    "data" = sort.data
  )
}



Rollup.From.ID <- function(id, level.rollup, rollup.desc) {
  # returns a list of
  #   n.level : original number of levels
  #   n.rollup.level : number of rolled up levels
  #   n.respondent : list of number of respondents
  #   pct.reponse : data.frame of rolled up level percentages
  
  pattern.id <- Row.Pattern(id)
  
  label.and.data <- Label.And.Data.From.ID(id)
  
  id.data <- label.and.data$data
  data.map <- label.and.data$data.map
  level.label <- as.data.frame(data.map$values)[, 2]
  
  n.level <- max(unlist(level.rollup))
  data.count <- Data.Level.Count(id.data, n.level)
  data.pct <- data.count$pct.response
  
  n.rollup.level = length(level.rollup)
  
  rollup.pct <- as.data.frame(data.pct[0,])
  
  for (ix in 1:n.rollup.level) {
    ix.rollup <- unlist(level.rollup[ix])
    
    rollup.pct[ix,] = colSums(data.pct[ix.rollup,])
  }
  
  rownames(rollup.pct) <- rollup.desc
  
  result <- list(
    "n.level" = n.level,
    "level.label" = level.label,
    "n.rollup.level" = n.rollup.level,
    "n.respondent" = data.count$n.valid,
    "pct.response" = rollup.pct
  )
}


Data.Labeled.Level.Count <-
  function (numeric.data, level.label, sort = FALSE) {
    # Data.Level.Count returns a list of
    #   n.max : the number of rows in numeric.data
    #   n.valid : for each data column, the number of valid responses
    #   n.response : for each data column, the number of responses at each level
    #   pct.response : for each data column, the percentage ...
    # numeric.data is a dataframe
    # level.label is a list of n.level labels
    
    n.level <- length(level.label)
    n.max <- nrow(numeric.data)
    n.valid <- n.max - colSums(is.na(numeric.data))
    
    column.name <- colnames(numeric.data)
    
    n.response <- as.data.frame(numeric.data[0, ])
    pct.response <- n.response
    
    for (ix in 1:n.level) {
      n.response[ix, ] <-
        apply(numeric.data, 2, function(x)
          sum(x == ix, na.rm = TRUE))
      pct.response[ix, ] <- n.response[ix, ] / n.valid
    }
    
    if (sort) {
      order.index <- order(as.vector(n.response), "decreasing" = TRUE)
      n.response <- as.data.frame(n.response[order.index,])
      pct.response <- as.data.frame(pct.response[order.index,])
      level.label <- level.label[order.index]
    } else {
      order.index <- 1:n.level
    }
    
    rownames(n.response) <- level.label
    rownames(pct.response) <- level.label
    colnames(n.response) <- column.name
    colnames(pct.response) <- column.name
    
    result <-
      list(
        "n.max" = n.max,
        "n.valid" = n.valid,
        "n.response" = n.response,
        "pct.response" = pct.response,
        "sort.index" = order.index
      )
  }


Rollup.From.ID.Sorted <-
  function (id,
            level.rollup,
            rollup.desc,
            rollup.sort.index) {
    # sort.index points to one of the rolled up levels
    
    rollup.raw <- Rollup.From.ID(id, level.rollup, rollup.desc)
    
    order.index <-
      order(rollup.raw$pct.response[rollup.sort.index,],
            "decreasing" = TRUE)
    
    result <- list(
      "n.level" = rollup.raw$n.level,
      "level.label" = rollup.raw$level.label,
      "n.rollup.level" = rollup.raw$n.rollup.level,
      "n.respondent" = rollup.raw$n.respondent[order.index],
      "pct.response" = rollup.raw$pct.response[, order.index],
      "sort.index" = order.index
    )
  }


Rollup.From.ID.Set.Order <- function (id,
                                      level.rollup,
                                      rollup.desc,
                                      column.index) {
  # sort.index points to one of the rolled up levels
  
  rollup.raw <- Rollup.From.ID(id, level.rollup, rollup.desc)
  
  result <- list(
    "n.level" = rollup.raw$n.level,
    "level.label" = rollup.raw$level.label,
    "n.rollup.level" = rollup.raw$n.rollup.level,
    "n.respondent" = rollup.raw$n.respondent[column.index],
    "pct.response" = rollup.raw$pct.response[, column.index]
  )
}


Single.Column.Data <- function(id) {
  id.variable <- Variable.From.Question(Exact.Match.Pattern(id))
  id.data <- Data.Value(id.variable$variable)
  
  id.label <- id.variable$data.map.variable$label
  value.map <- as.data.frame(id.variable$data.map.variable$values)
  colnames(id.data) <- id.variable$data.map.variable$title
  result <- list(
    "label" = id.label,
    "n.level" = nrow(value.map),
    "level description" = value.map,
    "data" = id.data
  )
}


Single.Stacked.Bar <- function(id, sort.order = 'decreasing') {
  id.data <- Single.Column.Data(id)
  column.name <- colnames(id.data$data)
  level.count <- Data.Level.Count(id.data$data, id.data$n.level)
  colnames(level.count$n.response) = column.name
  colnames(level.count$pct.response) = column.name
  
  result <- level.count
}


Letter.Labels <- function(n.letter) {
  letters <- rawToChar(as.raw(65:(64 + n.letter)))
}


Multinomial.Significance.Test <- function(n.observed, p.value) {
  # n.observed is a single column data.frame
  # can perform the test as a binomial for each pair with null = 1/2
  
  n.alternative <- nrow(n.observed)
  p.1 <- 0.5 * (1 - p.value)
  
  significance <- n.observed
  significance[, ] <- ""
  
  letters <- Letter.Labels(n.alternative)
  
  for (ix in 1:(n.alternative - 1)) {
    n.1 <- n.observed[ix, 1]
    letter.1 <- substr(letters, ix, ix)
    
    for (jx in (ix + 1):n.alternative) {
      n.2 <- n.observed[jx, 1]
      letter.2 <- substr(letters, jx, jx)
      
      curr.test <- chisq.test(c(n.1, n.2))
      
      if (curr.test$p.value <= p.1) {
        if (n.1 > n.2) {
          significance[ix, 1] <-
            paste(significance[ix, 1], letter.2, sep = "")
        } else {
          significance[jx, 1] <-
            paste(significance[jx, 1], letter.1, sep = "")
        }
      }
    }
  }
  
  result <- significance
}


Single.Column.With.Significance <-
  function(curr.id, p.value = 0.90) {
    id.data <- Single.Column.Data(curr.id)
    curr.data <- Data.Value(curr.id)
    
    curr.count <-
      Data.Labeled.Level.Count(curr.data, id.data$`level description`$title,
                               sort = TRUE)
    
    significance <-
      Multinomial.Significance.Test(curr.count$n.response, p.value)
    
    result <- list(
      "sample size" = curr.count$n.valid,
      "output.pct.level" = curr.count$pct.response,
      "significance" = significance
    )
  }



Convert.To.Data.Frame <-
  function(x,
           row.name = NULL,
           col.name = NULL) {
    df <- as.data.frame(x)
    if (!is.null(row.name)) {
      rownames(df) <- row.name
    }
    if (!is.null(col.name)) {
      colnames(df) <- col.name
    }
    result <- df
  }




# BEGIN GENDER PIE CHART  First chart on slide 4

S3.Single.Column <- function(curr.id, n.level, report.level) {
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



# BEGIN REGION MAP CHART  Second chart on slide 4

region.Single.Column <- function(curr.id, n.level, report.level) {
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


D3.Single.Column <- function(curr.id, n.level, report.level) {
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
  jsonDataKeyOrder <-
    c("attribute",
      "<$25k",
      "$25-$49k",
      "$50-$99k",
      "$100k+"
    )

  
  valuesOrder <- c("Income", pct.level[report.level] )
  
  data <- createDataFrameOfTwoLists(jsonDataKeyOrder,valuesOrder)

  

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
    'data' = data
  
  )
}


# BEGIN ETHNICITY STACKED BAR CHART  Third chart on slide 4

# Need both S5 and S6

#S5 race

Ethnicity <- function() {
  S5.variables <- "S5"
  S5.values <- result1[S5.variables]
  S5.numeric <-
    lapply(S5.values, function(x)
      as.numeric(x)) # a list
  S5.numeric.frame <- as.data.frame(S5.numeric)
  
  S5.n.max <- nrow(S5.numeric.frame)
  S5.n.valid <- S5.n.max - colSums(is.na(S5.numeric.frame))
  
  S5.race.valid <- !is.na(S5.numeric.frame)
  S5.valid.race <- S5.numeric.frame[S5.race.valid]
  
  S5.pct.race.level <-
    unlist(lapply(1:6, function(x)
      sum(S5.valid.race == x))) / S5.n.valid
  
  #S6 hispanic
  
  S6.variables <- "S6"
  S6.values <- result1[S6.variables]
  S6.numeric <-
    lapply(S6.values, function(x)
      as.numeric(x)) # a list
  S6.numeric.frame <- as.data.frame(S6.numeric)
  
  S6.n.max <- nrow(S6.numeric.frame)
  S6.n.valid <- S6.n.max - colSums(is.na(S6.numeric.frame))
  
  S6.hispanic.valid <- !is.na(S6.numeric.frame)
  S6.valid.hispanic <- S6.numeric.frame[S6.hispanic.valid]
  
  S6.pct.hispanic.level <-
    unlist(lapply(1:2, function(x)
      sum(S6.valid.hispanic == x))) / S6.n.valid
  
  # Now need to combine
  # Ultimate categories
  # Hispanic (list 3) = S6=1
  # Caucasian (list 1) = S6=2 and S5=1
  # AA (list 2) = S6=2 and S5=2
  # Asian (list 4) = S6=2 and S5=4
  # All other (list 5) = rest (S6=2 and (S5=3 or S5=5)
  
  
  S5.valid.race_collapse <- S5.valid.race
  S5.valid.race_collapse[S5.valid.race == 3 |
                           S5.valid.race == 5 |
                           S5.valid.race == 6] <- 5
  S6.hispanic.indexes <- which(S6.valid.hispanic %in% 1)
  S5.valid.race_collapse[S6.hispanic.indexes] <- 3
  
  ethnicity.result <-
    unlist(lapply(1:5, function(x)
      sum(S5.valid.race_collapse == x))) / S5.n.valid
  
  names(ethnicity.result) <-
    c("Caucasian",
      "AA",
      "Hispanic",
      "Asian",
      "All Other")
  result <- ethnicity.result
  
}



# Data is in S5.pct.race.level_collapse
# 1=Caucasian
# 2=AA
# 3=Hispanic
# 4=Asian
# 5=Other

#END ETHNICITY STACKED BAR CHART

HHSize.Single.Column.Infer.N.Level <-
  function(curr.id, report.level) {
    curr.data <- Data.Value(curr.id)
    
    ix.valid <- which(!is.na(curr.data))
    n.valid <- length(ix.valid)
    valid.data <- curr.data[ix.valid, ]
    
    data.level <- unique(valid.data)
    n.level <- length(data.level)
    
    # count the number of responses for each variable
    pct.level <-
      unlist(lapply(1:n.level, function(x)
        sum(valid.data == data.level[x]))) / n.valid
    
    pct.level[pct.level >= 5] <- 5
    
    # Labels
    #1=1
    #2=2
    #3=3
    #4=4
    #5=5+
    
    names(pct.level) <-
      c("1",
        "2",
        "3",
        "4",
        "5+")
    
    result <- list(
      "sample size" = n.valid,
      "n.level" = n.level,
      "level" = data.level,
      "output.pct.level" = pct.level[report.level]
    )
  }






# BEGIN HOUSEHOLD SIZE DATA  Fifth chart on slide 4
D1.variables <- "D1"
D1.values <- result1[D1.variables]
D1.numeric <- lapply(D1.values, function(x)
  as.numeric(x)) # a list
D1.numeric.frame <- as.data.frame(D1.numeric)

D1.n.max <- nrow(D1.numeric.frame)
D1.n.valid <- D1.n.max - colSums(is.na(D1.numeric.frame))

D1.HHSize.valid <- !is.na(D1.numeric.frame)
D1.valid.HHSize <- D1.numeric.frame[D1.HHSize.valid]
D1.valid.HHSize[D1.valid.HHSize >= 5] <- 5


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
  HHcompr5.numeric <-
    lapply(HHcompr5.values, function(x)
      as.numeric(x)) # a list
  HHcompr5.numeric.frame <- as.data.frame(HHcompr5.numeric)
  
  HHcompr5.n.max <- nrow(HHcompr5.numeric.frame)
  HHcompr5.n.valid <-
    HHcompr5.n.max - colSums(is.na(HHcompr5.numeric.frame))
  
  HHcompr5.Single.valid <- !is.na(HHcompr5.numeric.frame)
  HHcompr5.valid.Single <-
    HHcompr5.numeric.frame[HHcompr5.Single.valid]
  
  HHcompr5.pct.Single.level <-
    unlist(lapply(1, function(x)
      sum(HHcompr5.valid.Single == x))) / HHcompr5.n.valid
  
  ####
  
  HHcompr3.variables <- "HHcompr3"
  HHcompr3.values <- result1[HHcompr3.variables]
  HHcompr3.numeric <-
    lapply(HHcompr3.values, function(x)
      as.numeric(x)) # a list
  HHcompr3.numeric.frame <- as.data.frame(HHcompr3.numeric)
  
  HHcompr3.n.max <- nrow(HHcompr3.numeric.frame)
  HHcompr3.n.valid <-
    HHcompr3.n.max - colSums(is.na(HHcompr3.numeric.frame))
  
  HHcompr3.MNokids.valid <- !is.na(HHcompr3.numeric.frame)
  HHcompr3.valid.MNokids <-
    HHcompr3.numeric.frame[HHcompr3.MNokids.valid]
  
  HHcompr3.pct.MNokids.level <-
    unlist(lapply(1, function(x)
      sum(HHcompr3.valid.MNokids == x))) / HHcompr3.n.valid
  
  ####
  
  HHcompr2.variables <- "HHcompr2"
  HHcompr2.values <- result1[HHcompr2.variables]
  HHcompr2.numeric <-
    lapply(HHcompr2.values, function(x)
      as.numeric(x)) # a list
  HHcompr2.numeric.frame <- as.data.frame(HHcompr2.numeric)
  
  HHcompr2.n.max <- nrow(HHcompr2.numeric.frame)
  HHcompr2.n.valid <-
    HHcompr2.n.max - colSums(is.na(HHcompr2.numeric.frame))
  
  HHcompr2.Mkids.valid <- !is.na(HHcompr2.numeric.frame)
  HHcompr2.valid.Mkids <-
    HHcompr2.numeric.frame[HHcompr2.Mkids.valid]
  
  HHcompr2.pct.Mkids.level <-
    unlist(lapply(1, function(x)
      sum(HHcompr2.valid.Mkids == x))) / HHcompr2.n.valid
  
  ####
  
  HHcompr4.variables <- "HHcompr4"
  HHcompr4.values <- result1[HHcompr4.variables]
  HHcompr4.numeric <-
    lapply(HHcompr4.values, function(x)
      as.numeric(x)) # a list
  HHcompr4.numeric.frame <- as.data.frame(HHcompr4.numeric)
  
  HHcompr4.n.max <- nrow(HHcompr4.numeric.frame)
  HHcompr4.n.valid <-
    HHcompr4.n.max - colSums(is.na(HHcompr4.numeric.frame))
  
  HHcompr4.Skids.valid <- !is.na(HHcompr4.numeric.frame)
  HHcompr4.valid.Skids <-
    HHcompr4.numeric.frame[HHcompr4.Skids.valid]
  
  HHcompr4.pct.Skids.level <-
    unlist(lapply(1, function(x)
      sum(HHcompr4.valid.Skids == x))) / HHcompr4.n.valid
  
  ####
  
  HHcompr6.variables <- "HHcompr6"
  HHcompr6.values <- result1[HHcompr6.variables]
  HHcompr6.numeric <-
    lapply(HHcompr6.values, function(x)
      as.numeric(x)) # a list
  HHcompr6.numeric.frame <- as.data.frame(HHcompr6.numeric)
  
  HHcompr6.n.max <- nrow(HHcompr6.numeric.frame)
  HHcompr6.n.valid <-
    HHcompr6.n.max - colSums(is.na(HHcompr6.numeric.frame))
  
  HHcompr6.Multigen.valid <- !is.na(HHcompr6.numeric.frame)
  HHcompr6.valid.Multigen <-
    HHcompr6.numeric.frame[HHcompr6.Multigen.valid]
  
  HHcompr6.pct.Multigen.level <-
    unlist(lapply(1, function(x)
      sum(HHcompr6.valid.Multigen == x))) / HHcompr6.n.valid
  
  ####
  
  HHcompr7.variables <- "HHcompr7"
  HHcompr7.values <- result1[HHcompr7.variables]
  HHcompr7.numeric <-
    lapply(HHcompr7.values, function(x)
      as.numeric(x)) # a list
  HHcompr7.numeric.frame <- as.data.frame(HHcompr7.numeric)
  
  HHcompr7.n.max <- nrow(HHcompr7.numeric.frame)
  HHcompr7.n.valid <-
    HHcompr7.n.max - colSums(is.na(HHcompr7.numeric.frame))
  
  HHcompr7.Other.valid <- !is.na(HHcompr7.numeric.frame)
  HHcompr7.valid.Other <-
    HHcompr7.numeric.frame[HHcompr7.Other.valid]
  
  HHcompr7.pct.Other.level <-
    unlist(lapply(1, function(x)
      sum(HHcompr7.valid.Other == x))) / HHcompr7.n.valid
  
  # Combine these into one vector
  
  hhcomp <- c(
    HHcompr5.pct.Single.level,
    HHcompr3.pct.MNokids.level,
    HHcompr2.pct.Mkids.level,
    HHcompr4.pct.Skids.level,
    HHcompr6.pct.Multigen.level,
    HHcompr7.pct.Other.level
  )
  hhcomp.data.frame <- as.data.frame(hhcomp)
  
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
  
  result <- hhcomp
  
  
}


#Labels in order
#1=Single Person
#2=Married, no kids
#3=Married, kids
#4=Sngl Parent, kids
#5=Multi-Gen
#6=Other
####

#END HOUSEHOLD COMPOSITION HORIZONTAL BAR CHART


Age.Single.Column.Infer.N.Level <- function(curr.id, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  data.level <- unique(valid.data)
  n.level <- length(data.level)
  
  # recode ages into age groups
  
  
  valid.data[valid.data <= 8] <- 1
  valid.data[valid.data > 8 & valid.data < 21] <- 2
  valid.data[valid.data > 20 & valid.data < 38] <- 3
  valid.data[valid.data >= 38] <- 4
  
  
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:4, function(x)
      sum(valid.data == x))) / n.valid
  
  
  
  
  
  
  names(pct.level) <-
    c("GenZ",
      "Millenial",
      "GenX",
      "Boomer")
  
  result <- list(
    "sample size" = n.valid,
    "n.level" = n.level,
    "level" = data.level,
    "output.pct.level" = pct.level[report.level]
  )
}



urban.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  # hard-coded labels:
  names(pct.level) <-
    c("City",
      "Suburb",
      "Small town",
      "Rural")
  
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}



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
  data.map.variable <- example.data.map.variables[data.map.index,]
  level.string <- data.map.variable$values[1][[1]]$title
  
  rownames(curr.pct.response) <- level.string
  colnames(curr.pct.response) <- data.map.variable$rowTitle
  
  result <- list("n.valid" = curr.count$n.valid,
                 "pct.response" = curr.pct.response)
}


##### BEGIN SLIDE 6 CHART 2 PURCHASE FREQUENCY HORIZONTAL BAR CHART

Q1.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  pct.level[n.level + 1] <- sum(pct.level[1:3])
  pct.level[n.level + 2] <- sum(pct.level[(n.level - 1):n.level])
  
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
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}


##### END SLIDE 6 CHART 2 PURCHASE FREQUENCY HORIZONTAL BAR CHART


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

##### BEGIN SLIDE 6a CHART 1 CONSUMPTION RECENCY HORIZONTAL BAR CHART

Q2.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  pct.level[n.level + 1] <- sum(pct.level[1:3])
  pct.level[n.level + 2] <- sum(pct.level[(n.level - 1):n.level])
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}


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
    category.pct.purchase.frequency.level[c(1, 2, 3, 4, 5, 11, 6, 7, 8, 12)]
  
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
      curr.pct.response[1:3,],
      colSums(curr.pct.response[1:3,]),
      curr.pct.response[4:6,],
      colSums(curr.pct.response[7:8,])
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
  
  data.map.index <-
    match(row.variable, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index,]
  
  colnames(out.pct.response) <- data.map.variable$rowTitle
  
  result <- list("n.valid" = curr.count$n.valid,
                 "pct.response" = out.pct.response)
}


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
      curr.n.response[1:5,],
      colSums(curr.n.response[1:5,]),
      curr.n.response[6:8,],
      colSums(curr.n.response[9:10,])
    )
  
  out.pct.response <-
    rbind(
      curr.pct.response[1:5,],
      colSums(curr.pct.response[1:5,]),
      curr.pct.response[6:8,],
      colSums(curr.pct.response[9:10,])
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
  
  data.map.index <-
    match(row.variable, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index,]
  
  colnames(out.n.response) <- data.map.variable$rowTitle
  colnames(out.pct.response) <- data.map.variable$rowTitle
  
  # statistical significance testing of each brand vs each other brand
  # at each consumption frequency level
  # Pearson's Chi-Square
  
  significance <- out.n.response
  significance[, ] <- ""
  
  curr.n.valid <- curr.count$n.valid
  out.n.row <- nrow(out.n.response)
  
  letters <- rawToChar(as.raw(65:(64 + Sub_Cat.n)))
  
  for (rx in 1:out.n.row) {
    for (cx in 1:(Sub_Cat.n - 1)) {
      n.1 <- curr.n.valid[cx]
      s.1 <- out.n.response[rx, cx]
      
      letter.1 <- substr(letters, cx, cx)
      
      for (dx in (cx + 1):Sub_Cat.n) {
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
  
  result <- list(
    "n.valid" = curr.n.valid,
    "pct.response" = out.pct.response,
    "significance" = significance
  )
}

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
  
  data.map.index <-
    grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index, ]
  level.string <- data.map.variable$values[1][[1]]$title
  
  # attach row names
  
  # sort by response percentage of top two levels
  
  report.pct.response.select <-
    curr.count$pct.response[report.level, ]
  row.names(report.pct.response.select) <-
    level.string[report.level]
  
  
  report.pct.response.top.2 <-
    report.pct.response.select[1, ] +
    report.pct.response.select[2, ]
  row.names(report.pct.response.top.2) <-
    paste(n.level - 1, "+", n.level, sep = "")
  
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


#### BEGIN SLIDE 10 CHART 2 AIDED BRAND AWARENESS

# aided brand awareness
# slide 10, chart 2
# q8r
# vertical bar chart (sorted means)

Q8 <- function(curr.id, max.valid.row) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
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
  
  data.map.index <-
    grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index, ]
  brand.name <- data.map.variable$rowTitle[1:n.attribute]
  
  #   brand.name <- t(as.data.frame(data.map.variable$rowTitle[1:n.attribute]))
  
  colnames(curr.n.response) <- t(brand.name)
  colnames(curr.pct.response) <- t(brand.name)
  aided.order <- order(curr.pct.response, decreasing = TRUE)
  
  key.brands.pct.response <-
    curr.pct.response[which(KeyBrands.values[1, ] == 1)]
  key.brands.aided.order <-
    order(key.brands.pct.response, decreasing = TRUE)
  
  result <- list(
    "n.valid" = curr.count$n.valid[aided.order],
    "n.response" = curr.n.response[, aided.order],
    "pct.response" = curr.pct.response[, aided.order],
    "aided.order" = aided.order,
    "key.brands.aided.order" = key.brands.aided.order
  )
}




Brand.Funnel <- function(aided.sort.order, aided.sorted.results) {
  # Need to source previous slide to get order and aided awareness data
  
  
  # Brand funnel Sub_Cat
  
  # the survey variables of interest (aided brand awareness):
  
  
  
  # Q9 Brand Funnel
  q9r.variables <-
    c("Q9r1", "Q9r2", "Q9r3", "Q9r4", "Q9r5", "Q9r6")
  
  # subset of the full dataframe containing just these variables:
  q9r.values <- result1[q9r.variables]
  q9r.values[is.na(q9r.values)] <- 0
  
  # convert the response strings into numeric values (0/1):
  q9r.numeric <- lapply(q9r.values, function(x)
    as.numeric(x))
  
  # convert the list of column values into a dataframe:
  q9r.numeric.frame <- as.data.frame(q9r.numeric)
  
  # Create each chart point
  # Aided awareness already computed and stored in q8r.means
  
  # Consider = top5 = scale points 2-6 based to total
  q9r.consider.numeric.frame <- q9r.numeric.frame
  q9r.consider.numeric.frame[q9r.consider.numeric.frame == 1] <- 0
  q9r.consider.numeric.frame[q9r.consider.numeric.frame > 1] <- 1
  
  # Ever Tried = top4 = scale points 3-6 based to total
  q9r.evertried.numeric.frame <- q9r.numeric.frame
  q9r.evertried.numeric.frame[q9r.evertried.numeric.frame < 3] <- 0
  q9r.evertried.numeric.frame[q9r.evertried.numeric.frame > 2] <- 1
  
  # Lapsed = scale point 4 based to total
  q9r.lapsed.numeric.frame <- q9r.numeric.frame
  q9r.lapsed.numeric.frame[q9r.lapsed.numeric.frame < 4 |
                             q9r.lapsed.numeric.frame > 4] <- 0
  q9r.lapsed.numeric.frame[q9r.lapsed.numeric.frame == 4] <- 1
  
  # Current User = scale points 5 and 6 based to total
  q9r.currentuser.numeric.frame <- q9r.numeric.frame
  q9r.currentuser.numeric.frame[q9r.currentuser.numeric.frame < 5] <-
    0
  q9r.currentuser.numeric.frame[q9r.currentuser.numeric.frame >= 5] <-
    1
  
  # Loyalist = scale point 6 based to total
  q9r.loyalist.numeric.frame <- q9r.numeric.frame
  q9r.loyalist.numeric.frame[q9r.loyalist.numeric.frame < 6] <- 0
  q9r.loyalist.numeric.frame[q9r.loyalist.numeric.frame == 6] <- 1
  
  # Resp data for sig testing
  #q8r.numeric.frame,q9r.consider.numeric.frame,q9r.evertried.numeric.frame,q9r.lapsed.numeric.frame,
  #                  q9r.currentuser.numeric.frame,q9r.loyalist.numeric.frame
  
  
  # Data summary
  
  temp1 <- as.vector(aided.sorted.results)
  temp2 <- as.vector(1:6)
  temp2[aided.sort.order] <- temp1
  q8r.means <- as.data.frame(temp2)
  
  q9r.consider.pct.level <- colMeans(q9r.consider.numeric.frame)
  q9r.evertried.pct.level <- colMeans(q9r.evertried.numeric.frame)
  q9r.lapsed.pct.level <- colMeans(q9r.lapsed.numeric.frame)
  q9r.currentuser.pct.level <-
    colMeans(q9r.currentuser.numeric.frame)
  q9r.loyalist.pct.level <- colMeans(q9r.loyalist.numeric.frame)
  q9r.trialconversion.pct.level <-
    q9r.evertried.pct.level / q8r.means
  q9r.retainedtriers.pct.level <-
    q9r.currentuser.pct.level / q9r.evertried.pct.level
  
  # All brands in one matrix, columns sorted by aided awareness
  
  #q8r.bar.chart.data.sorted <-
  #  q8r.bar.chart.data[order(q8r.bar.chart.data$Mean,
  #                           decreasing = TRUE),]
  
  Q9.result <- rbind(
    'Aided Aware' = q8r.means,
    'Consider' = q9r.consider.pct.level,
    'Ever Tried' = q9r.evertried.pct.level,
    'Lapsed' = q9r.lapsed.pct.level,
    'Loyalist' = q9r.loyalist.pct.level,
    'Retained Triers' = q9r.retainedtriers.pct.level
  )
  
  Q9.result.sorted <- Q9.result[, aided.sort.order]
  
  colnames(Q9.result.sorted) <- colnames(aided.sorted.results)
  
  result <- Q9.result.sorted
  #result<-rbind(aided.results,Q9.result.sorted)
  
}

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
      curr.map.entry <- example.data.map.variables[curr.map.index,]
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
  curr.brand.sample.size <- as.data.frame(curr.n.valid[1, ])
  colnames(curr.brand.sample.size) <- "Sample Size"
  rownames(curr.brand.sample.size) <- brand.name.vector
  
  curr.pct.response <-
    as.data.frame(curr.n.response / curr.n.valid)
  rownames(curr.pct.response) <- attribute
  colnames(curr.pct.response) <- brand.name
  
  sort.n.valid <- curr.n.valid[question.order, aided.order]
  sorted.n.response <- curr.n.response[question.order, aided.order]
  sorted.pct.response.1 <- curr.pct.response[question.order, ]
  sorted.pct.response <- sorted.pct.response.1[, aided.order]
  sorted.brand.sample.size <- curr.brand.sample.size[aided.order, ]
  
  # significance testing of each brand vs each other brand for each question
  # Pearson's Chi-Square
  
  significance <- sorted.pct.response
  significance[,] <- "" # clear the contents of significance
  
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
  
  result <- list(
    pct.response = sorted.pct.response,
    brand.sample.size = sorted.brand.sample.size,
    significance = significance
  )
}


#############  BEGIN SLIDE 8 q5  ##############

Q5 <- function(curr.id, n.top, n.level) {
  # n.top is the number of (most frequent) channels to report individually
  
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  data.map.index <-
    grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index,]
  level.string <- data.map.variable$values[1][[1]]$title
  
  channel.title <- data.map.variable$rowTitle
  channel.label <-
    sapply(channel.title, function(x)
      Short.Label.Paren(x), USE.NAMES = FALSE)
  n.channel <- length(channel.label)
  
  # find the variables that begin with the pattern
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <-
    example.raw.data.variables[curr.variable.index]
  # the variables may not be sorted correctly by attribute index
  
  n.variable <- length(curr.variables)
  
  if (n.variable != n.channel) {
    paste("Warning:",
          n.channel,
          "channels !=",
          n.variable,
          "variables")
  }
  
  # need to work with the variables in whatever order they get picked up
  # because there are valid channels are indicated by strings (rather than
  # numbers) after the 'r'
  
  # get the numeric and non-numeric indexes
  row.index <- Row.Type.Index(curr.variables)
  
  # get responses as a data.frame; one column for each attribute
  curr.data <- Data.Value(curr.variables)
  
  curr.count <- Data.Level.Count(curr.data, n.level)
  
  curr.n.valid <- curr.count$n.valid
  curr.n.response.2or3 <- curr.count$n.response[2:3,]
  rownames(curr.n.response.2or3) <- level.string[2:3]
  curr.n.response.2and3 <-
    t(as.data.frame(colSums(curr.n.response.2or3)))
  rownames(curr.n.response.2and3) <- "Occasionally/Frequently"
  
  curr.n.response <-
    rbind(curr.n.response.2or3, curr.n.response.2and3)
  colnames(curr.n.response) <- channel.label
  
  # set aside 'Other' columns for aggregation into 'All Other' column
  
  other.index <- grep("Other", channel.label)
  not.other.index <- grep("Other", channel.label, invert = TRUE)
  n.not.other <- length(not.other.index)
  channel.order <-
    order(curr.n.response.2and3[not.other.index], decreasing = TRUE)
  
  top.sort.index <- not.other.index[channel.order[1:n.top]]
  sort.n.response <- curr.n.response[, top.sort.index]
  sort.n.valid <- curr.n.valid[top.sort.index]
  
  other.sort.index <-
    c(not.other.index[channel.order[(n.top + 1):n.not.other]],
      other.index)
  other.data <- curr.data[, other.sort.index]
  
  # get maximum by column
  other.data.max <-
    as.data.frame(do.call(pmax, c(other.data, list(na.rm = TRUE))))
  colnames(other.data.max) <- "All Other"
  other.n.valid <- sum(!is.na(other.data.max),  na.rm = TRUE)
  
  other.n.response <- as.data.frame(other.data.max[0, ])
  for (ix in 1:n.level) {
    other.n.response[ix, ] <-
      apply(other.data.max, 2, function(x)
        sum(x == ix, na.rm = TRUE))
  }
  
  other.n.response.2or3 <- other.n.response[2:3, ]
  other.response.report <-
    as.data.frame(c(other.n.response.2or3, sum(other.n.response.2or3)))
  colnames(other.response.report) <- "All Other"
  
  report.n.response <-
    cbind(sort.n.response[, 1:n.top], other.response.report)
  report.n.valid <- sort.n.valid
  report.n.valid[n.top + 1] <- other.n.valid
  report.pct.response <- report.n.response / report.n.valid
  
  result <- list("n.valid" = report.n.valid,
                 "pct.response" = report.pct.response)
}




#############  END SLIDE 8 q5  ##############




#############  BEGIN SLIDES 12-13  ##############

# generate data for slides 12 & 13
#   brand purchase + usage behaviors
#   slide 13, chart 1 (not on slide 12) : horizontal stacked bar chart; Q10
#   slide 13, chart 2 (slide 12, chart 1) : horizontal stacked bar chart; Q11
#   slide 13, chart 3 (slide 12, chart 2) : vertical stacked bar chart; Q14

level.rollup.Q10 <- list(1, c(2, 3), c(4, 5), 6, 7)
rollup.desc.Q10 <- c("Past Week",
                     "Past 2 - 4 Weeks",
                     "Past 2 - 6 Months",
                     "Past 7 - 12 Months",
                     " > 1 yr")

level.rollup.Q11 = list(1, 2, c(3, 4), c(5, 6), c(7, 8))
rollup.desc.Q11 <- c("Today",
                     "Past Week",
                     "Past 2 - 4 Weeks",
                     "Past 2 - 6 Months",
                     "> 7 months")



#############  END SLIDES 12-13  ##############


#############  BEGIN SLIDE 14  ##############

# slide 14, Brand Behaviors & Attitudes
#   Q12, Q13
#   horizontal stacked bar charts
#   Base size footnote with piped in brand names and N=(Ever Tried Q9 T4B)

level.rollup.Q12 = list(4, 5, c(4, 5))
rollup.desc.Q12 <- c("Like a Lot",
                     "Love it",
                     "Like/Lov it")



#############  END SLIDE 14  ##############

#############  BEGIN SLIDE 19  ##############

# slide 19, brand perceptual map, Q15
# scatterplot data

#source("Slides15 16 Q15 Brand Performance 20200628.R")

Brand.Perceptual.Map <- function (q15.summary) {
  q15.map <- ca(q15.summary)	# Run correspondence analysis
  
  attribute.coordinates <- q15.map$rowcoord
  brand.coordinates <- q15.map$colcoord
  
  scatterplot.data <-
    rbind(attribute.coordinates[, 1:2],
          brand.coordinates[, 1:2])
  
  colnames(scatterplot.data) <- c("x", "y")
  result <- list("Brand Perceptual Map" = scatterplot.data)
  
}





#############  END SLIDE 19  ##############

##################### BEGIN SLIDE 22-23 ####################################################

# NOTE:  this file needs to be sourced BEFORE slide 21
#
# generate data for slides 22 & 23
#   horizontal bar charts for last purchase occasion
#   physical / online purchase types
#   uses Q18 through Q22


Location.Prep.Index <- function(ix) {
  result <- list("n" = length(ix),
                 "index" = ix)
}


Location.Index <- function(location.data,
                           channel.data,
                           prep.data,
                           location,
                           location.level,
                           channel.level)
{
  is.location <- location.data == location.level |
    (is.na(location.data)
     & channel.data == channel.level)
  
  planned <- which(is.location & prep.data == "1")
  spontaneous <- which(is.location & prep.data == "2")
  
  result <- list(
    "location" = location,
    "n.location" = sum(is.location, na.rm = TRUE),
    "planned" = Location.Prep.Index(planned),
    "spontaneous" = Location.Prep.Index(spontaneous)
  )
}


Respondent.Location.Prep <-
  function(id.channel, id.location, id.prep) {
    variable.channel <-
      Variable.From.Question(Exact.Match.Pattern(id.channel))
    variable.location <-
      Variable.From.Question(Exact.Match.Pattern(id.location))
    variable.prep <-
      Variable.From.Question(Exact.Match.Pattern(id.prep))
    
    last.purchase.channel.data <-
      Data.Value(variable.channel$variable)
    last.purchase.location.data <-
      Data.Value(variable.location$variable)
    last.purchase.prep.data <-
      Data.Value(variable.prep$variable)
    
    last.purchase.channel.count <-
      Data.Labeled.Level.Count(
        last.purchase.channel.data,
        as.data.frame(variable.channel$data.map.variable$values)$title
      )
    
    location.physical <- Location.Index(
      last.purchase.location.data,
      last.purchase.channel.data,
      last.purchase.prep.data,
      "physical",
      1,
      11
    )
    
    location.online <- Location.Index(
      last.purchase.location.data,
      last.purchase.channel.data,
      last.purchase.prep.data,
      "online",
      2,
      12
    )
    
    result <-
      list(
        "n.respondent" = last.purchase.channel.count$n.valid,
        "physical" = location.physical,
        "online" = location.online,
        "last.purchase.channel.count" = last.purchase.channel.count
      )
  }


Last.Purchase.Reason.Data <- function(id.reason) {
  variable.reason <- Variable.From.Question(id.reason)
  
  reason.data <- Data.Value(variable.reason$variable)
  
  names.from.data <- names(reason.data)
  
  row.from.map <- variable.reason$data.map.variable$row
  variable.from.map <-
    sapply(row.from.map, function(x)
      paste("Q22", x, sep = ""))
  reason.from.map <- variable.reason$data.map.variable$rowTitle
  
  # map.index <- match(names.from.data, variable.from.map)
  # temp1 <- variable.from.map[map.index]
  data.index <- match(variable.from.map, names.from.data)
  # temp2 <- names.from.data[data.index]
  
  sort.reason.data <- reason.data[, data.index]
  colnames(sort.reason.data) <- reason.from.map
  
  # map.index <- match(names.from.data, variable.from.map)
  #
  # # must get the variables in the same order
  # colnames(reason.data) <-
  #   reason.from.map[map.index]
  
  result <- sort.reason.data
}


Fulfillment.Data <- function(id.fulfill, respondent.index) {
  variable.fulfill <- Variable.From.Question(id.fulfill)
  fulfill.data <- Data.Value(variable.fulfill$variable)
  fulfill.data.select <-
    as.data.frame(fulfill.data[respondent.index,])
  colnames(fulfill.data.select) <- "Fulfillment Type"
  
  level.values <-
    as.data.frame(variable.fulfill$data.map.variable$values)
  level.string <- level.values[, "title"]
  n.level <- length(level.string)
  
  fulfill.count <- Data.Level.Count(fulfill.data.select, n.level)
  
  pct.response <- fulfill.count$pct.response
  rownames(pct.response) <- level.string
  colnames(pct.response) <- "Fulfillment Type"
  
  result <- list("n.respondent" = fulfill.count$n.valid,
                 "pct.response" = pct.response)
}


Horizontal.Bar.Chart.Subset <-
  function(prep.string,
           location.map,
           all.data,
           n.location) {
    # prepare inputs for a horizontal bar chart with a subset of the input data
    # sort reasons by frequency
    # eliminate reasons with no response
    
    prep.map <- location.map[[prep.string]]
    
    select.data <- all.data[prep.map$index, ]
    
    select.count <- Data.Level.Count(select.data, 1)
    
    order.pct.response <-
      order(select.count$pct.response, decreasing = TRUE)
    
    sort.n.response <- select.count$n.response[order.pct.response]
    n.positive.reason <- sum(sort.n.response > 0, na.rm = TRUE)
    
    order.positive.pct.response <-
      order.pct.response[1:n.positive.reason]
    else.index <- match('Something else', colnames(sort.n.response))
    
    if (is.numeric(else.index) & (else.index < n.positive.reason)) {
      order.positive.pct.response <-
        c(order.positive.pct.response[-else.index],
          order.positive.pct.response[else.index])
    }
    
    sort.pct.response <-
      select.count$pct.response[order.positive.pct.response]
    sort.n.valid <-
      select.count$n.valid[order.positive.pct.response]
    
    result <- list(
      "prep" = prep.string,
      "pct.prep" = prep.map$n / n.location,
      "n.respondent" = max(sort.n.valid, na.rm = TRUE),
      "n.valid" = sort.n.valid,
      "pct.response" = sort.pct.response
    )
  }



Purchase.Prep <-
  function(location.string,
           respondent.map,
           all.data,
           fulfill = FALSE) {
    n.all.respondent <- respondent.map$n.respondent
    
    location.map <- respondent.map[[location.string]]
    n.location <- location.map$n.location
    
    planned <-
      Horizontal.Bar.Chart.Subset("planned", location.map, all.data, n.location)
    spontaneous <-
      Horizontal.Bar.Chart.Subset("spontaneous", location.map, all.data, n.location)
    
    if (fulfill) {
      online.repondent.index <- c(location.map$planned$index,
                                  location.map$spontaneous$index)
      fulfillment.output <-
        Fulfillment.Data("Q20", online.repondent.index)
      
      output <- list(
        "location" = location.string,
        "pct location" = location.map$n.location / n.all.respondent,
        "planned output" = planned,
        "spontaneous output" = spontaneous,
        "Fullfillment" = fulfillment.output
      )
    } else {
      output <- list(
        "location" = location.string,
        "pct location" = location.map$n.location / n.all.respondent,
        "planned output" = planned,
        "spontaneous output" = spontaneous
      )
    }
    
    result <- output
  }

# global that needs to be created before generating slide21:
respondent.location.prep <-
  Respondent.Location.Prep("Q18", "Q19", "Q21")

# global used by slides 22 & 23 (below)
last.purchase.data <- Last.Purchase.Reason.Data("Q22")



# need to force 'Something Else' to be last
##################### END SLIDE 22-23 ####################################################


#############  BEING SLIDE 21 7 CHARTS  ##############

# BEGIN Q16 HORIZONTAL BAR CHART  First chart on slide 21

Q16.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  # # hard-coded labels:
  # names(pct.level) <-
  #   c(
  #     "Male",
  #     "Female"
  #   )
  
  
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}

# Sort and label
# sig test






#END Q16 HORIZONTAL BAR CHART


########################################################################


# BEGIN Q17 HORIZONTAL BAR CHART  Second chart on slide 4
Q17.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  # # # hard-coded labels:
  names(pct.level) <-
    c(
      "Stock Up",
      "Running Low/Fill In",
      "Same Day Use",
      "Special Occ",
      "Store browsing",
      "Sale visit",
      "Had coupon"
    )
  
  pct.level.order <- order(pct.level, decreasing = TRUE)
  pct.level.sorted <- pct.level[pct.level.order]
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level.sorted[report.level])
}

# Sort and label
# sig test






#END Q17 HORIZONTAL BAR CHART

########################################################################

# BEGIN Q19 Q18 Vertical BAR CHART  Third chart on slide 4
# need to process slides 22 & 23 first; create these globals:
#   respondent.location.prep
#   out.slide22.Q21.22.lastpurchphys.HB
#   out.slide23.Q21.22.lastpurchonline.HB

# respondent.location.prep$\physical|online\$n.location


Sort.Single.Column <- function(raw) {
  n.raw <- nrow(raw)
  
  row.name <- row.names(raw)
  column.name <- colnames(raw)
  
  sort.index <- order(raw, "decreasing" = TRUE)
  
  sorted <- as.data.frame(raw[sort.index,])
  rownames(sorted) <- row.name[sort.index]
  colnames(sorted) <- column.name
  
  result <- list("n" = n.raw,
                 "sort.index" = sort.index,
                 "sorted" = sorted)
}


Sort.Single.Column.Exclude.Other <- function (raw) {
  sorted <- Sort.Single.Column(raw)
  
  row.name <- rownames(sorted$sorted)
  column.name <- colnames(raw)
  
  ix.other <-
    grep("other", tolower(row.name)) # tolower : case insensitive search
  
  sort.index <-
    c(sorted$sort.index[-ix.other], sorted$sort.index[ix.other])
  
  raw.row.name <- row.names(raw)
  
  sort.excluded <- as.data.frame(raw[sort.index, ])
  rownames(sort.excluded) <- raw.row.name[sort.index]
  colnames(sort.excluded) <- column.name
  
  result <- list("n" = sorted$n,
                 "sort.index" = sort.index,
                 "sorted" = sort.excluded)
}


Single.Column.Rollup <-
  function(all.data, level.rollup, other.label = "All Other") {
    sorted.data <- all.data$sorted
    sorted.row.names <- rownames(sorted.data)
    
    n.level <- all.data$n
    n.rollup.level = length(level.rollup)
    
    rollup <- as.data.frame(sorted.data[0,])
    rollup.row.names <- NULL
    
    for (ix in 1:n.rollup.level) {
      ix.rollup <- unlist(level.rollup[ix])
      
      rollup[ix,] = sum(sorted.data[ix.rollup,])
      
      if (ix == n.rollup.level) {
        rollup.row.names[ix] <- other.label
      } else {
        rollup.row.names[ix] <-
          Short.Label.Paren(sorted.row.names[ix.rollup])
      }
    }
    
    rownames(rollup) <- rollup.row.names
    colnames(rollup) <- colnames(sorted.data)
    
    result <- list(
      "n.level" = n.level,
      "n.rollup.level" = n.rollup.level,
      "rollup" = rollup
    )
  }


Top.Purchase.Channels <- function(source.data, n.top) {
  # source data is a list of
  #   $n.respondent : total number of valid respondents
  #   $physical / $online lists containing:
  #     $n.location contains the number of respondents using that location
  #   $last.purchase.channel.count; list containing
  #     $pct.response, a data.frame containing
  #       locations (row names) & percentages (single column of values)
  
  sorted <-
    Sort.Single.Column.Exclude.Other(source.data$last.purchase.channel.count$pct.response)
  
  rollup <-
    Single.Column.Rollup(sorted, c(1:n.top, list((n.top + 1):sorted$n)))
  
  chart.data <- rbind(
    "TTL IN-STORE" = source.data$physical$n.location / source.data$n.respondent,
    "TTL ONLINE" = source.data$online$n.location / source.data$n.respondent,
    rollup$rollup
  )
  
  result <- list("sample.size" = source.data$n.respondent,
                 "pct.response" = chart.data)
}



#################################################################################

#### BEGIN Q24 PRICE PAID HORIZONTAL BAR CHART


Trip.Type <- function() {
  # SEPARATE VARIABLES, report only first 3
  
  ##
  
  Q24r.variables <- c("Q24r1", "Q24r2", "Q24r3")
  Q24r.values <- result1[Q24r.variables]
  Q24r.numeric <-
    lapply(Q24r.values, function(x)
      as.numeric(x)) # a list
  Q24r.numeric.frame <- as.data.frame(Q24r.numeric)
  
  # count the number of responses for each variable
  
  Q24r.n.max <- nrow(Q24r.numeric.frame)
  Q24r.n.valid <- Q24r.n.max - colSums(is.na(Q24r.numeric.frame))
  
  # 8 purchase frequency levels (hard-coded)
  
  # allocate a data.frame with named columns
  Q24r.n.purchase.frequency.level <-
    as.data.frame(Q24r.numeric.frame[0, ], row.names = NULL)
  Q24r.pct.purchase.frequency.level <-
    as.data.frame(Q24r.numeric.frame[0, ], row.names = NULL)
  
  # Q24r.n.purchase.frequency.level <- data.frame(Q24r.numeric.frame[,])
  
  for (ix in 1:1) {
    Q24r.n.purchase.frequency.level[ix, ] <-
      apply(Q24r.numeric.frame, 2, function(x)
        sum(x == ix, na.rm = TRUE))
    
    Q24r.pct.purchase.frequency.level[ix, ] <-
      Q24r.n.purchase.frequency.level[ix, ] / Q24r.n.valid
  }
  
  ### label
  
  names(Q24r.pct.purchase.frequency.level) <-
    c("Regular", "On Sale", "Coupon")
  
  result <- Q24r.pct.purchase.frequency.level
  
}




# label

#### END Q24 HORIZONTAL BAR CHART



##############################################################

#### BEGIN Q23 SUBSTITUTABILITY PIE CHART


Q23.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  # hard-coded labels:
  names(pct.level) <-
    c("Go Elsewhere for Brand",
      "Buy Another Brand",
      "Purchase Nothing in Category")
  
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}






################################################################

#### BEGIN Q26 SUBSTITUTABILITY PIE CHART




Q26.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  # hard-coded labels:
  names(pct.level) <-
    c(
      "Everyday",
      "Regular Occasion/Little Special",
      "Special Family Occasion",
      "Party/Entertaining",
      "Other"
    )
  
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}







#### END Q26 BAR CHART

#########################################################



#### BEGIN q25 INTENDED USER HORIZONTAL BAR CHART

Intended.User <- function() {
  Q25r.variables <- c("Q25r1", "Q25r2", "Q25r3", "Q25r4", "Q25r5")
  Q25r.values <- result1[Q25r.variables]
  Q25r.numeric <-
    lapply(Q25r.values, function(x)
      as.numeric(x)) # a list
  Q25r.numeric.frame <- as.data.frame(Q25r.numeric)
  
  # count the number of responses for each variable
  
  Q25r.n.max <- nrow(Q25r.numeric.frame)
  Q25r.n.valid <- Q25r.n.max - colSums(is.na(Q25r.numeric.frame))
  
  # 8 purchase frequency levels (hard-coded)
  
  # allocate a data.frame with named columns
  Q25r.n.purchase.frequency.level <-
    as.data.frame(Q25r.numeric.frame[0, ], row.names = NULL)
  Q25r.pct.purchase.frequency.level <-
    as.data.frame(Q25r.numeric.frame[0, ], row.names = NULL)
  
  # Q25r.n.purchase.frequency.level <- data.frame(Q25r.numeric.frame[,])
  
  for (ix in 1:1) {
    Q25r.n.purchase.frequency.level[ix, ] <-
      apply(Q25r.numeric.frame, 2, function(x)
        sum(x == ix, na.rm = TRUE))
    
    Q25r.pct.purchase.frequency.level[ix, ] <-
      Q25r.n.purchase.frequency.level[ix, ] / Q25r.n.valid
    
    
    # hard-coded labels:
    names(Q25r.pct.purchase.frequency.level) <-
      c("Self",
        "Spouse/SO",
        "Other Adult",
        "Kids 0-12",
        "Kids 13-17")
  }
  result <- Q25r.pct.purchase.frequency.level
  
  
  
}





#### END q25 INTENDED USER BAR CHART





############################################################

#################### END SLIDE 21 ####################################################




##################### BEGIN SLIDE 24 8 CHARTS ############################################


#####################################################################

# BEGIN Q27 LAST BRAND CONSUMED CHART  First chart on slide 24
#  with significance testing





#####################################################################

# BEGIN Q29 MEAL TYPE HB CHART  second chart on slide 24

Q29.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  # hard-coded labels:
  names(pct.level) <-
    c(
      "Pre-Bkfst",
      "Bkfst",
      "Bkfst-Lunch",
      "Lunch",
      "Lunch-Dinner",
      "Dinner",
      "After Dinner"
    )
  
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}


#### End Q29 chart



#######################################################################

# BEGIN Q32 USAGEL TYPE HB CHART  second chart on slide 24

Q32.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  # hard-coded labels:
  names(pct.level) <-
    c("On Its Own",
      "In a Recipe",
      "w Meal/Snack")
  
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}


#### End 32 chart



###############################################################################

# BEGIN Q33 OCCASION TYPE HB CHART  FOURTH chart on slide 24

Q33.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  # hard-coded labels:
  names(pct.level) <-
    c("Everyday",
      "Reg, but Special",
      "Spcl Fam Occ.",
      "Party",
      "Other")
  
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level[report.level])
}


#### End Q33 chart



###################################################################

# BEGIN Q28 DAY OF WEEK HB CHART  FIFTH chart on slide 24

Q28.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  pct.level.order <- c(2:7, 1)
  
  pct.level.sorted <- pct.level[pct.level.order]
  
  # hard-coded labels:
  names(pct.level.sorted) <-
    c("M",
      "T",
      "W",
      "TH",
      "F",
      "SA",
      "SU")
  
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level.sorted)
}


#### End Q28 chart



######################################################################

# BEGIN Q30 LOCATION HB CHART  SIXTH chart on slide 24

Q30.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid, ]
  
  # count the number of responses for each variable
  pct.level <-
    unlist(lapply(1:n.level, function(x)
      sum(valid.data == x))) / n.valid
  
  pct.level.order <- order(pct.level[1:5], decreasing = TRUE)
  pct.level.sorted <- pct.level[pct.level.order]
  
  
  
  # hard-coded labels:
  names.Q30 <-
    c("Home",
      "School",
      "Work",
      "Others' Home",
      "In Transit")
  names.Q30.sorted <- names.Q30[pct.level.order]
  
  names(pct.level.sorted) <- names.Q30.sorted
  #### I DON'T KNOW HOW TO SORT LABELS
  
  #consec.names<-t(as.data.frame(consec.names))
  
  #names(pct.level.sorted)<-consec.names[pct.level.sorted]
  
  # temp1<-consec.names[pct.level.sorted]
  
  
  ###### GET NAMES FROM MAP
  
  result <- list("sample size" = n.valid,
                 output.pct.level = pct.level.sorted[report.level])
}


#### End 30 chart



#######################################################################################



Q31.new <- function(curr.id) {
  label.and.data <- Label.And.Data.From.ID(curr.id)
  
  curr.data <- label.and.data$data
  
  curr.count <- Data.Level.Count(curr.data, 1)
  
  curr.n.valid <- curr.count$n.valid
  
  curr.pct.response <- curr.count$pct.response
  
  #colnames(curr.pct.response) <- data.map.variable$rowTitle
  
  
  
  names(curr.pct.response) <-
    c("Self Only",
      "Other Adult",
      "Kids 0-12",
      "Kids 13-17",
      "Someone Else")
  
  
  #sort.pct.response <- sort(curr.pct.response, 'decreasing' = TRUE)
  
  result <- list(
    "n.valid" = curr.n.valid,
    'question' = label.and.data$data.map$qtitle[1],
    "pct.response" = curr.pct.response
  )
}
















Q31 <- function(curr.id) {
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  data.map.index <-
    grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index,]
  
  # find the variables that begin with the pattern
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <-
    example.raw.data.variables[curr.variable.index]
  # the variables may not be sorted correctly by index
  # but they will be sorted by result, so don't sort here
  
  n.variable <- length(curr.variables)
  
  # get responses as a data.frame; one column for each attribute
  curr.data <- Data.Value(curr.variables)
  
  curr.count <- Data.Level.Count(curr.data, 1)
  
  curr.n.valid <- curr.count$n.valid
  
  curr.pct.response <- curr.count$pct.response
  
  colnames(curr.pct.response) <- data.map.variable$rowTitle
  
  
  
  result <- list(
    "n.valid" = curr.n.valid,
    'question' = data.map.variable$qtitle[1],
    "pct.response" = curr.pct.response
  )
}





####END SLIDE 24 Q31

################################################################




Q35 <- function(curr.id) {
  label.and.data <- Label.And.Data.From.ID(curr.id)
  
  curr.data <- label.and.data$data
  
  curr.count <- Data.Level.Count(curr.data, 1)
  
  curr.n.valid <- curr.count$n.valid
  
  curr.pct.response <- curr.count$pct.response
  
  # colnames(curr.pct.response) <- example.data.map.variables$rowTitle
  
  sort.pct.response <- sort(curr.pct.response, 'decreasing' = TRUE)
  
  result <- list(
    "n.valid" = curr.n.valid,
    'question' = label.and.data$data.map$qtitle[1],
    "pct.response" = sort.pct.response[1:3]
  )
  
}




####END SLIDE 24 Q35

#########################################################################

############### END SLIDE 24 8 CHARTS ##############################

# generate JSON data for slide 25, Q34
#   horizontal bar charts, reason for choosing brand
#   sorted by response percentage


Q34.new <- function(curr.id) {
  label.and.data <- Label.And.Data.From.ID(curr.id)
  
  curr.data <- label.and.data$data
  
  curr.count <- Data.Level.Count(curr.data, 1)
  
  curr.n.valid <- curr.count$n.valid
  
  curr.pct.response <- curr.count$pct.response
  
  # colnames(curr.pct.response) <- data.map.variable$rowTitle
  
  sort.pct.response <- sort(curr.pct.response, 'decreasing' = TRUE)
  
  result <- list(
    "n.valid" = curr.n.valid,
    'question' = label.and.data$data.map$qtitle[1],
    "pct.response" = sort.pct.response
  )
}



##############  END SLIDE 25 ###################################

####### END REPORT END REPORT END REPORT ##############

##########  START CALLS  ###########

out.slide4.r1c1.S3.gender.PC <-
  S3.Single.Column(curr.id = "S3",
                   n.level = 2,
                   report.level = c(1, 2))

out.slide4.r1c2.region_quota.MAP <-
  region.Single.Column(curr.id = "region_quota",
                       n.level = 4,
                       report.level = c(1:4))


out.slide4.r1c3.D3.income.VSB <-
  D3.Single.Column(curr.id = "D3",
                   n.level = 7,
                   report.level = c(1:4))

out.slide4.r1c4.S5S6.ethnicity.VSB <- Ethnicity()

out.slide4.r2c1.D1.HHSize.UK <-
  HHSize.Single.Column.Infer.N.Level(curr.id = "D1",
                                     report.level = c(1:5))

out.slide4.r2c1.D1.HHSize.UK <-
  unlist(lapply(1:5, function(x)
    sum(D1.valid.HHSize == x))) / D1.n.valid

out.slide4.r2c2.HHCompr.HHComp.HB <- Household.Composition()

out.slide4.r2c3.S4.generations.VSB <-
  Age.Single.Column.Infer.N.Level(curr.id = "S4",
                                  report.level = c(1:4))

out.slide4.r2c4.D4.urbanicity.VSB <-
  urban.Single.Column(curr.id = "D4",
                      n.level = 4,
                      report.level = c(1:4))

out.slide5.Q37.sustainability.VSB <-
  Q37(curr.id = "Q37", n.level = 5)

out.slide6.c1.Q1.catpurchrec.HB <-
  Q1.Single.Column(
    curr.id = "Q1",
    n.level = 8,
    report.level = c(1:3, 9, 4:6, 10)
  )

out.slide6.c2.Q3.catpurchfreq.HB <- Q3(curr.id = "Q3", n.level = 8)

out.slide6A.c1.Q2.catconsrec.HB <-
  Q2.Single.Column(
    curr.id = "Q2",
    n.level = 8,
    report.level = c(1:3, 9, 4:6, 10)
  )

out.slide6a.c2.Q4.catconsfreq.HB <- Q4(curr.id = "Q4", n.level = 10)

out.slide7.Q3.subcatpurchfreq.HB <-
  Slide7.Q3(curr.id = "Q3", n.level = 8)

out.slide7a.Q4.subcatconsfreq.HB <-
  Slide7a.Q4(curr.id = "Q4", n.level = 10)

out.slide9.Q6.importance.VSB <-
  Q6(curr.id = "Q6",
     n.level = 3,
     report.level = 2:3)

out.slide10.c2.Q8.aidaware.VB <-
  Q8(curr.id = "Q8", max.valid.row = 99)

out.slide11.Q8Q9.brandfunnel.HB <-
  Brand.Funnel(
    out.slide10.c2.Q8.aidaware.VB$aided.order,
    out.slide10.c2.Q8.aidaware.VB$pct.response
  )

# aided awareness results already sorted
# sort order based on aided awareness

#out.slide10.c2.Q8.aidaware.VB$aided.order
#out.slide10.c2.Q8.aidaware.VB$pct.response

# q9.brandfunnel.results.sorted <-
#   q9.brandfunnel.results[order(q9r.brandfunnel.results[1, ], decreasing = TRUE]

# q6.result$top.2.order contains the question importance order indexes

# q8.result$key.brands.aided.order contains the aided brand awareness order
# indexes for the key brands

out.slide1516.Q15.brandperf.UK <-
  Q15(
    curr.id = "Q15",
    question.order = out.slide9.Q6.importance.VSB$top.2.order,
    aided.order = 1:KeyBrands.n
  )

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
#


out.slide8.Q5.wheretheyshop.VSB <-
  Q5(curr.id = "Q5",
     n.top = 7,
     n.level = 3)

out.slide13.c1.Q10.brandpurchrec.hsb <-
  Rollup.From.ID(id = "Q10",
                 level.rollup = level.rollup.Q10,
                 rollup.desc = rollup.desc.Q10)

out.slide13.c2.Q11.recencyofbrand.hsb <-
  Rollup.From.ID(id = "Q11",
                 level.rollup = level.rollup.Q11,
                 rollup.desc = rollup.desc.Q11)

out.slide13.c3.Q14.satisbrandavail.vsb <-
  Single.Stacked.Bar(id = "Q14")

out.slide14.c1.q12.brandaffinity.HSB <-
  Rollup.From.ID.Sorted(
    id = "Q12",
    level.rollup = level.rollup.Q12,
    rollup.desc = rollup.desc.Q12,
    rollup.sort.index = 3
  )

out.slide14.c2.q13.brandreclikelihood.HSB <-
  Rollup.From.ID.Set.Order(
    id = "Q13",
    level.rollup = 1:5,
    rollup.desc = out.slide14.c1.q12.brandaffinity.HSB$level.label,
    column.index = out.slide14.c1.q12.brandaffinity.HSB$sort.index
  )

out.slide19.q15.pmap.SP <-
  Brand.Perceptual.Map(out.slide1516.Q15.brandperf.UK$pct.response)

out.slide22.Q21.22.lastpurchphys.HB <-
  Purchase.Prep("physical", respondent.location.prep, last.purchase.data)

out.slide23.Q21.22.lastpurchonline.HB <-
  Purchase.Prep("online",
                respondent.location.prep,
                last.purchase.data,
                fulfill = TRUE)


out.slide21.r1c1.Q16.lastbrandpurch.HB <-
  Q16.Single.Column(curr.id = "Q16",
                    n.level = 7,
                    report.level = c(1:7))

out.slide21.r1c2.Q17.triptype.HB <-
  Q17.Single.Column(curr.id = "Q17",
                    n.level = 7,
                    report.level = c(1:7))

out.slide21.r1c3.Q19Q18.channels.VB <-
  Top.Purchase.Channels(respondent.location.prep, n.top = 4)

out.slide21.r2c1.Q24.pricepaid.VB <- Trip.Type()

out.slide21.r2c2.Q23.subst.PC <-
  Q23.Single.Column(curr.id = "Q23",
                    n.level = 3,
                    report.level = c(1:3))

out.slide21.r2c3.Q26.occtype.VSB <-
  Q26.Single.Column(curr.id = "Q26",
                    n.level = 5,
                    report.level = c(1:5))

out.slide21.r2c4.Q25.user.HB <- Intended.User()


out.slide24.r1c1.q27.lastbrandcons.HB <-
  Single.Column.With.Significance(curr.id = "Q27")

out.slide24.r1c2.Q29.mealtype.HB <-
  Q29.Single.Column(curr.id = "Q29",
                    n.level = 7,
                    report.level = c(1:7))

out.slide24.r1c3.Q32.usagetype.HB <-
  Q32.Single.Column(curr.id = "Q32",
                    n.level = 3,
                    report.level = c(1:3))

out.slide24.r1c4.Q33.occtype.HB <-
  Q33.Single.Column(curr.id = "Q33",
                    n.level = 5,
                    report.level = c(1:5))

out.slide24.r2c1.Q28.dayofweek.HB <-
  Q28.Single.Column(curr.id = "Q28",
                    n.level = 7,
                    report.level = c(1:7))

out.slide24.r2c2.Q30.location.HB <-
  Q30.Single.Column(curr.id = "Q30",
                    n.level = 6,
                    report.level = c(1:5))

out.slide24.r2c3.Q31.whowith.HB <-
  Q31.new(curr.id = "Q31")

out.slide24.r2c3.Q31.withwhom.HB <-
  Q31(curr.id = "Q31")

out.slide24.r2c4.Q35.reasons.HB <-
  Q35(curr.id = "Q35")

out.slide25.Q34.reasonchoosebrand.HB <-
  Q34.new(curr.id = "Q34")


###JSON FORMATTING EXAMPLE START ###
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

inComeFormatted <- returnChartDataAndMetaData(
  out.slide4.r1c3.D3.income.VSB[['data']],
  'Income',
  out.slide4.r1c3.D3.income.VSB[['baseSize']],
  out.slide4.r1c3.D3.income.VSB[['questionID']],
  out.slide4.r1c3.D3.income.VSB[['keyOrder']],
  '',
  '',
  'stackedBar',
  c("#42b2ac","#bddbe8", "#98ca3c", "#56d2b4"),
  'v'
)
###JSON FORMATTING EXAMPLE END ###





### Note: As a end output we would expect one list containing all charts eg.:
### processedData <- list(aidedAwareness=q8r.bar.chart.list, demoGender=qXYZ.bar.chart.list, demoRegion=qXYZ.bar.chart.list,...)
### processedDataJSON <- toJSON(processedData, pretty = TRUE, auto_unbox = TRUE )

# 7/13/20:  the final output is currently a list of lists
# each element in processedData is a "chart list" that holds output data for the 
# corresponding chart, including sample size(s), chart data, auxiliary
# information displayed in the chart, etc.
# Unfortunately, at this time, we do not have a consistent format for the
# contents of the "chart lists".  If you can provide us with a desired format,
# including naming conventions, we can do that.

processedData <- list(
  "gender" = genderFormatted,
  "region" = regionFormatted ,
  "income" = inComeFormatted,
  "slide4.r1c4.S5S6.ethnicity.VSB" = out.slide4.r1c4.S5S6.ethnicity.VSB,
  "slide4.r2c1.D1.HHSize.UK " = out.slide4.r2c1.D1.HHSize.UK ,
  "slide4.r2c1.D1.HHSize.UK" = out.slide4.r2c1.D1.HHSize.UK,
  "slide4.r2c2.HHCompr.HHComp.HB" = out.slide4.r2c2.HHCompr.HHComp.HB,
  "slide4.r2c3.S4.generations.VSB" = out.slide4.r2c3.S4.generations.VSB,
  "slide4.r2c4.D4.urbanicity.VSB" = out.slide4.r2c4.D4.urbanicity.VSB,
  "slide5.Q37.sustainability.VSB" = out.slide5.Q37.sustainability.VSB,
  "slide6.c1.Q1.catpurchrec.HB" = out.slide6.c1.Q1.catpurchrec.HB,
  "slide6.c2.Q3.catpurchfreq.HB " = out.slide6.c2.Q3.catpurchfreq.HB,
  "slide6A.c1.Q2.catconsrec.HB" = out.slide6A.c1.Q2.catconsrec.HB,
  "slide6a.c2.Q4.catconsfreq.HB" = out.slide6a.c2.Q4.catconsfreq.HB,
  "slide7.Q3.subcatpurchfreq.HB" = out.slide7.Q3.subcatpurchfreq.HB,
  "slide7a.Q4.subcatconsfreq.HB" = out.slide7a.Q4.subcatconsfreq.HB,
  "slide9.Q6.importance.VSB" = out.slide9.Q6.importance.VSB,
  "slide10.c2.Q8.aidaware.VB" = out.slide10.c2.Q8.aidaware.VB,
  "slide11.Q8Q9.brandfunnel.HB" = out.slide11.Q8Q9.brandfunnel.HB,
  "slide1516.Q15.brandperf.UK" = out.slide1516.Q15.brandperf.UK,
  "slide8.Q5.wheretheyshop.VSB" = out.slide8.Q5.wheretheyshop.VSB,
  "slide13.c2.Q11.recencyofbrand.hsb" = out.slide13.c2.Q11.recencyofbrand.hsb,
  "slide13.c3.Q14.satisbrandavail.vsb" = out.slide13.c3.Q14.satisbrandavail.vsb,
  "slide14.c1.q12.brandaffinity.HSB" = out.slide14.c1.q12.brandaffinity.HSB,
  "slide14.c2.q13.brandreclikelihood.HSB" = out.slide14.c2.q13.brandreclikelihood.HSB,
  "slide19.q15.pmap.SP" = out.slide19.q15.pmap.SP,
  "slide22.Q21.22.lastpurchphys.HB" = out.slide22.Q21.22.lastpurchphys.HB,
  "slide23.Q21.22.lastpurchonline.HB" = out.slide23.Q21.22.lastpurchonline.HB,
  "slide21.r1c1.Q16.lastbrandpurch.HB" = out.slide21.r1c1.Q16.lastbrandpurch.HB,
  "slide21.r1c2.Q17.triptype.HB" = out.slide21.r1c2.Q17.triptype.HB,
  "slide21.r1c3.Q19Q18.channels.VB" = out.slide21.r1c3.Q19Q18.channels.VB,
  "slide21.r2c1.Q24.pricepaid.VB" = out.slide21.r2c1.Q24.pricepaid.VB,
  "slide21.r2c2.Q23.subst.PC" = out.slide21.r2c2.Q23.subst.PC,
  "slide21.r2c3.Q26.occtype.VSB" = out.slide21.r2c3.Q26.occtype.VSB,
  "slide21.r2c4.Q25.user.HB" = out.slide21.r2c4.Q25.user.HB,
  "slide24.r1c1.q27.lastbrandcons.HB" = out.slide24.r1c1.q27.lastbrandcons.HB,
  "slide24.r1c2.Q29.mealtype.HB" = out.slide24.r1c2.Q29.mealtype.HB,
  "slide24.r1c3.Q32.usagetype.HB" = out.slide24.r1c3.Q32.usagetype.HB,
  "slide24.r1c4.Q33.occtype.HB" = out.slide24.r1c4.Q33.occtype.HB,
  "slide24.r2c1.Q28.dayofweek.HB" = out.slide24.r2c1.Q28.dayofweek.HB,
  "slide24.r2c2.Q30.location.HB" = out.slide24.r2c2.Q30.location.HB,
  "slide24.r2c3.Q31.whowith.HB " = out.slide24.r2c3.Q31.whowith.HB,
  "slide24.r2c3.Q31.withwhom. HB" = out.slide24.r2c3.Q31.withwhom.HB,
  "slide24.r2c4.Q35.reasons.HB" = out.slide24.r2c4.Q35.reasons.HB,
  "slide25.Q34.reasonchoosebrand.HB" = out.slide25.Q34.reasonchoosebrand.HB
)

# for (x in 1:length(purchaseFrequencySubCategory)) {
#   processedData [paste('purchaseFrequency', x)] <-
#     purchaseFrequencySubCategory[x]
# }
#
# for (x in 1:length(consumptionFrequencySubCategory)) {
#   processedData[paste('consumptionFrequency', x)] <-
#     consumptionFrequencySubCategory[x]
# }





if (debug) {
  processedDataJSON <-
  toJSON(processedData, pretty = TRUE, auto_unbox = TRUE)
  lapply(processedDataJSON, write, "./RscriptTests/crau200603.json")
} else { ## NOTE we need this in this format as for our reporting framework the last R script output needs to be that processedDataJSON
  processedDataJSON <-
  toJSON(processedData, pretty = TRUE, auto_unbox = TRUE)
}