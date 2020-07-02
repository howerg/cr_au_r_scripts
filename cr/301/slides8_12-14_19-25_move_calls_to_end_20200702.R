#############  BEGIN SLIDE 8 q5  ##############

Q5 <- function(curr.id, n.top, n.level) {
  # n.top is the number of (most frequent) channels to report individually
  
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  data.map.index <-
    grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index, ]
  level.string <- data.map.variable$values[1][[1]]$title
  
  channel.title <- data.map.variable$rowTitle
  channel.label <- 
    sapply(channel.title, function(x) Short.Label.Paren(x), USE.NAMES = FALSE)
  n.channel <- length(channel.label)
  
  # find the variables that begin with the pattern
  curr.variable.index <-
    grep(curr.pattern, example.raw.data.variables)
  
  curr.variables <-
    example.raw.data.variables[curr.variable.index]
  # the variables may not be sorted correctly by attribute index
  
  n.variable <- length(curr.variables)
  
  if (n.variable != n.channel) {
    paste("Warning:", n.channel, "channels !=", n.variable, "variables")
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
  curr.n.response.2or3 <- curr.count$n.response[2:3, ]
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
  
  other.sort.index <- c(not.other.index[channel.order[(n.top + 1):n.not.other]],
                        other.index)
  other.data <- curr.data[, other.sort.index]
  
  # get maximum by column
  other.data.max <- as.data.frame(do.call(pmax, c(other.data, list(na.rm = TRUE))))
  colnames(other.data.max)<- "All Other"
  other.n.valid <- sum(!is.na(other.data.max),  na.rm = TRUE)
  
  other.n.response <- as.data.frame(other.data.max[0,])
  for (ix in 1:n.level) {
    other.n.response[ix,] <-
      apply(other.data.max, 2, function(x) sum(x == ix, na.rm = TRUE))
  }
  
  other.n.response.2or3 <- other.n.response[2:3,]
  other.response.report <- as.data.frame(
    c(other.n.response.2or3, sum(other.n.response.2or3)))
  colnames(other.response.report) <- "All Other"
  
  report.n.response <- cbind(sort.n.response[,1:n.top], other.response.report)
  report.n.valid <- sort.n.valid
  report.n.valid[n.top+1] <- other.n.valid
  report.pct.response <- report.n.response / report.n.valid
  
  result <- list(
    "n.valid" = report.n.valid,
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

require(ca)	# load correspondence analysis package


Brand.Perceptual.Map <- function (q15.summary) {
  q15.map<-ca(q15.summary)	# Run correspondence analysis
  
  attribute.coordinates <- q15.map$rowcoord
  brand.coordinates <- q15.map$colcoord
  
  scatterplot.data <- 
    rbind(
      attribute.coordinates[, 1:2],
      brand.coordinates[, 1:2]
    )
  
  colnames(scatterplot.data) <- c("x", "y")
  result <- list(
    "Brand Perceptual Map" = scatterplot.data
  )
  
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
      Data.Labeled.Level.Count(last.purchase.channel.data,
                               as.data.frame(variable.channel$data.map.variable$values)$title)
    
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
    as.data.frame(fulfill.data[respondent.index, ])
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
    
    select.data <- all.data[prep.map$index,]
    
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
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
  # # hard-coded labels:
  # names(pct.level) <-
  #   c(
  #     "Male",
  #     "Female"
  #   )
  
  
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
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
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
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
  pct.level.sorted<-pct.level[pct.level.order]
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level.sorted[report.level]
  )
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
  
  sorted <- as.data.frame(raw[sort.index, ])
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
  
  sort.excluded <- as.data.frame(raw[sort.index,])
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
    
    rollup <- as.data.frame(sorted.data[0, ])
    rollup.row.names <- NULL
    
    for (ix in 1:n.rollup.level) {
      ix.rollup <- unlist(level.rollup[ix])
      
      rollup[ix, ] = sum(sorted.data[ix.rollup, ])
      
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
  
  result <- list(
    "sample.size" = source.data$n.respondent,
    "pct.response" = chart.data)
}



#################################################################################

#### BEGIN Q24 PRICE PAID HORIZONTAL BAR CHART


Trip.Type <- function() {
  
  # SEPARATE VARIABLES, report only first 3
  
  ##
  
  Q24r.variables <- c("Q24r1", "Q24r2", "Q24r3")
  Q24r.values <- result1[Q24r.variables]
  Q24r.numeric <- lapply(Q24r.values, function(x) as.numeric(x)) # a list
  Q24r.numeric.frame <- as.data.frame(Q24r.numeric)
  
  # count the number of responses for each variable
  
  Q24r.n.max <- nrow(Q24r.numeric.frame)
  Q24r.n.valid <- Q24r.n.max - colSums(is.na(Q24r.numeric.frame))
  
  # 8 purchase frequency levels (hard-coded)
  
  # allocate a data.frame with named columns
  Q24r.n.purchase.frequency.level <- as.data.frame(Q24r.numeric.frame[0,], row.names=NULL)
  Q24r.pct.purchase.frequency.level <- as.data.frame(Q24r.numeric.frame[0,], row.names=NULL)
  
  # Q24r.n.purchase.frequency.level <- data.frame(Q24r.numeric.frame[,])
  
  for (ix in 1:1) {
    Q24r.n.purchase.frequency.level[ix,] <-
      apply(Q24r.numeric.frame, 2, function(x)
        sum(x == ix, na.rm = TRUE))
    
    Q24r.pct.purchase.frequency.level[ix,] <-
      Q24r.n.purchase.frequency.level[ix,] / Q24r.n.valid
  }
  
  ### label
  
  names(Q24r.pct.purchase.frequency.level)<-c("Regular","On Sale","Coupon")
  
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
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
  # hard-coded labels:
  names(pct.level) <-
    c(
      "Go Elsewhere for Brand",
      "Buy Another Brand",
      "Purchase Nothing in Category"
    )
  
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
}






################################################################

#### BEGIN Q26 SUBSTITUTABILITY PIE CHART




Q26.Single.Column <- function(curr.id, n.level, report.level) {
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
      "Everyday",
      "Regular Occasion/Little Special",
      "Special Family Occasion",
      "Party/Entertaining",
      "Other"
    )
  
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
}







#### END Q26 BAR CHART

#########################################################



#### BEGIN q25 INTENDED USER HORIZONTAL BAR CHART

Intended.User <- function() {
  
  
  
  Q25r.variables <- c("Q25r1", "Q25r2", "Q25r3", "Q25r4", "Q25r5")
  Q25r.values <- result1[Q25r.variables]
  Q25r.numeric <- lapply(Q25r.values, function(x) as.numeric(x)) # a list
  Q25r.numeric.frame <- as.data.frame(Q25r.numeric)
  
  # count the number of responses for each variable
  
  Q25r.n.max <- nrow(Q25r.numeric.frame)
  Q25r.n.valid <- Q25r.n.max - colSums(is.na(Q25r.numeric.frame))
  
  # 8 purchase frequency levels (hard-coded)
  
  # allocate a data.frame with named columns
  Q25r.n.purchase.frequency.level <- as.data.frame(Q25r.numeric.frame[0,], row.names=NULL)
  Q25r.pct.purchase.frequency.level <- as.data.frame(Q25r.numeric.frame[0,], row.names=NULL)
  
  # Q25r.n.purchase.frequency.level <- data.frame(Q25r.numeric.frame[,])
  
  for (ix in 1:1) {
    Q25r.n.purchase.frequency.level[ix,] <-
      apply(Q25r.numeric.frame, 2, function(x)
        sum(x == ix, na.rm = TRUE))
    
    Q25r.pct.purchase.frequency.level[ix,] <-
      Q25r.n.purchase.frequency.level[ix,] / Q25r.n.valid
    
    
    # hard-coded labels:
    names(Q25r.pct.purchase.frequency.level) <-
      c(
        "Self",
        "Spouse/SO",
        "Other Adult",
        "Kids 0-12",
        "Kids 13-17"
      )
  } 
  result <-Q25r.pct.purchase.frequency.level
  
  
  
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
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
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
  
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
}


#### End Q29 chart



#######################################################################

# BEGIN Q32 USAGEL TYPE HB CHART  second chart on slide 24

Q32.Single.Column <- function(curr.id, n.level, report.level) {
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
      "On Its Own",
      "In a Recipe",
      "w Meal/Snack"
    )
  
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
}


#### End 32 chart



###############################################################################

# BEGIN Q33 OCCASION TYPE HB CHART  FOURTH chart on slide 24

Q33.Single.Column <- function(curr.id, n.level, report.level) {
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
      "Everyday",
      "Reg, but Special",
      "Spcl Fam Occ.",
      "Party",
      "Other"
    )
  
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level[report.level]
  )
}


#### End Q33 chart



###################################################################

# BEGIN Q28 DAY OF WEEK HB CHART  FIFTH chart on slide 24

Q28.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
  pct.level.order<-c(2:7,1)
  
  pct.level.sorted<-pct.level[pct.level.order]
  
  # hard-coded labels:
  names(pct.level.sorted) <-
    c(
      "M",
      "T",
      "W",
      "TH",
      "F",
      "SA",
      "SU"
    )
  
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level.sorted
  )
}


#### End Q28 chart



######################################################################

# BEGIN Q30 LOCATION HB CHART  SIXTH chart on slide 24

Q30.Single.Column <- function(curr.id, n.level, report.level) {
  curr.data <- Data.Value(curr.id)
  
  ix.valid <- which(!is.na(curr.data))
  n.valid <- length(ix.valid)
  valid.data <- curr.data[ix.valid,]
  
  # count the number of responses for each variable
  pct.level <- 
    unlist(lapply(1:n.level, function(x) sum(valid.data == x)))/ n.valid
  
  pct.level.order<-order(pct.level[1:5],decreasing=TRUE)
  pct.level.sorted<-pct.level[pct.level.order]
  
  
  
  # hard-coded labels:
  names.Q30 <-
    c(
      "Home",
      "School",
      "Work",
      "Others' Home",
      "In Transit"
    )
  names.Q30.sorted<-names.Q30[pct.level.order]
  
  names(pct.level.sorted)<-names.Q30.sorted
  #### I DON'T KNOW HOW TO SORT LABELS
  
  #consec.names<-t(as.data.frame(consec.names))
  
  #names(pct.level.sorted)<-consec.names[pct.level.sorted]
  
  # temp1<-consec.names[pct.level.sorted]
  
  
  ###### GET NAMES FROM MAP 
  
  result <- list(
    "sample size" = n.valid,
    output.pct.level = pct.level.sorted[report.level]
  )
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
    c(
      "Self Only",
      "Other Adult",
      "Kids 0-12",
      "Kids 13-17",
      "Someone Else"
    )
  
  
  #sort.pct.response <- sort(curr.pct.response, 'decreasing' = TRUE)
  
  result <- list(
    "n.valid" = curr.n.valid,
    'question' = label.and.data$data.map$qtitle[1],
    "pct.response" = curr.pct.response)
}
















Q31 <- function(curr.id) {
  
  curr.pattern <- paste(curr.id, "r.+", sep = "")
  
  data.map.index <-
    grep(curr.pattern, example.data.map.variables$label)
  data.map.variable <- example.data.map.variables[data.map.index, ]
  
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
    "pct.response" = curr.pct.response)
}





####END SLIDE 24 Q31

################################################################




Q35 <- function(curr.id) {
  
  label.and.data <- Label.And.Data.From.ID(curr.id)
  
  curr.data <- label.and.data$data
  
  curr.count <- Data.Level.Count(curr.data, 1)
  
  curr.n.valid <- curr.count$n.valid
  
  curr.pct.response <- curr.count$pct.response
  
  colnames(curr.pct.response) <- data.map.variable$rowTitle
  
  sort.pct.response <- sort(curr.pct.response, 'decreasing' = TRUE)
  
  result <- list(
    "n.valid" = curr.n.valid,
    'question' = label.and.data$data.map$qtitle[1],
    "pct.response" = sort.pct.response[1:3])
  
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
  
  colnames(curr.pct.response) <- data.map.variable$rowTitle
  
  sort.pct.response <- sort(curr.pct.response, 'decreasing' = TRUE)
  
  result <- list(
    "n.valid" = curr.n.valid,
    'question' = label.and.data$data.map$qtitle[1],
    "pct.response" = sort.pct.response)
}



##############  END SLIDE 25 ###################################

####### END REPORT END REPORT END REPORT ##############
  
##########  START CALLS  ###########

out.slide8.Q5.wheretheyshop.VSB <-
  Q5(curr.id = "Q5", n.top = 7, n.level = 3)

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
  Q16.Single.Column(
    curr.id = "Q16", 
    n.level = 7,
    report.level = c(1:7))

out.slide21.r1c2.Q17.triptype.HB <- 
  Q17.Single.Column(
    curr.id = "Q17", 
    n.level = 7,
    report.level = c(1:7))

out.slide21.r1c3.Q19Q18.channels.VB <-
  Top.Purchase.Channels(respondent.location.prep, n.top = 4)

out.slide21.r2c1.Q24.pricepaid.VB<- Trip.Type()

out.slide21.r2c2.Q23.subst.PC <- 
  Q23.Single.Column(
    curr.id = "Q23", 
    n.level = 3,
    report.level = c(1:3))

out.slide21.r2c3.Q26.occtype.VSB <- 
  Q26.Single.Column(
    curr.id = "Q26", 
    n.level = 5,
    report.level = c(1:5))

out.slide21.r2c4.Q25.user.HB <- Intended.User()




out.slide24.r1c1.q27.lastbrandcons.HB <-
  Single.Column.With.Significance(curr.id = "Q27")

out.slide24.r1c2.Q29.mealtype.HB <- 
  Q29.Single.Column(
    curr.id = "Q29", 
    n.level = 7,
    report.level = c(1:7))

out.slide24.r1c3.Q32.usagetype.HB <- 
  Q32.Single.Column(
    curr.id = "Q32", 
    n.level = 3,
    report.level = c(1:3))

out.slide24.r1c4.Q33.occtype.HB <- 
  Q33.Single.Column(
    curr.id = "Q33", 
    n.level = 5,
    report.level = c(1:5))

out.slide24.r2c1.Q28.dayofweek.HB <- 
  Q28.Single.Column(
    curr.id = "Q28", 
    n.level = 7,
    report.level = c(1:7))

out.slide24.r2c2.Q30.location.HB <- 
  Q30.Single.Column(
    curr.id = "Q30", 
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
