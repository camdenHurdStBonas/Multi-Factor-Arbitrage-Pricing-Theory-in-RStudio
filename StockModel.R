stockModel <- function(Ticker.df) {
  
  #
  percentChangeList <- function(df){
    
    #
    final_rates <- list()
    
    #
    for (i in 2:length(df)){
      
      #
      rate = log(df[i]/df[i-1])
      
      #
      final_rates <- c(final_rates, rate)
    }
    
    #
    return(as.numeric(final_rates))
    
  }
  
  # Create an environment to store local variables
  obj <- new.env()
  
  #sets the variable "path" to the name of Working Directory of My Created Folder 
  obj$path <- "/Users/camdenhurd/desktop/desktop/Finance/Portfolio/Rstudio"
  
  #Set the Working Directory to My Created Folder using the variable "path"
  setwd(obj$path)
  
  #French and fama data frame from https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
  obj$French.Fama.Factors <- read.csv("Fama.French.Factors.csv")
  
  obj$MRP <- obj$French.Fama.Factors$Mkt.RF
  
  obj$SMB <- obj$French.Fama.Factors$SMB
  
  obj$HML <- obj$French.Fama.Factors$HML
  
  obj$RMW <- obj$French.Fama.Factors$RMW
  
  obj$CMA <- obj$French.Fama.Factors$CMA
  
  obj$RF <- obj$French.Fama.Factors$RF
  
  #Stock historical monthly returns on the adjusted closing price in percentage points https://finance.yahoo.com/quote/Ticker?p=Ticker&.tsrc=fin-srch
  obj$Ticker.Returns <- round(percentChangeList(Ticker.df$Adj.Close),4)*100 #math.R
  obj$Ticker.Volume.ROC <- round(percentChangeList(Ticker.df$Volume),4)*100 #math.R
  
  
  
  if((length(obj$Ticker.Returns)) < length(obj$RF)) {
    obj$i <- (length(obj$Ticker.Returns))
    
    obj$j <- (length(obj$RF)-obj$i)
    
    obj$k <- length(obj$RF)
    
    obj$l <- 1
  }
  else {
    obj$i <- (length(obj$Ticker.Returns))
    
    obj$j <- 1
    
    obj$k <- length(obj$RF)
    
    obj$l <- obj$i - obj$k
  }
  
  obj$Ticker.Net <- obj$Ticker.Returns[obj$l:obj$i] - obj$RF[obj$j:obj$k]
  
  #Renaming and converting the variables into decimals
  obj$'Excess Returns' <- obj$Ticker.Net/100
  obj$'Market Risk Premium' <- obj$MRP[obj$j:obj$k]/100
  obj$'Small Minus Big' <- obj$SMB[obj$j:obj$k]/100
  obj$'High Minus Low' <- obj$HML[obj$j:obj$k]/100
  obj$'Robust Minus Weak' <- obj$RMW[obj$j:obj$k]/100
  obj$'Conservative Minus Aggressive' <- obj$CMA[obj$j:obj$k]/100
  obj$'Volume ROC' <- obj$Ticker.Volume.ROC[obj$l:obj$i]/100
  
  obj$keys <- list(
    'MRP' = obj$'Market Risk Premium',
    'SMB' = obj$'Small Minus Big',
    'HML' = obj$'High Minus Low',
    'RMW' = obj$'Robust Minus Weak',
    'CMA' = obj$'Conservative Minus Aggressive',
    'Vol.ROC' = obj$'Volume ROC'
  )
  
  # function getModel returns the CAPM model
  obj$getModel <- function(userKeys) {
    
    subset <- obj$keys[userKeys]
    
    # Combine the subsetted data into a data frame
    subset_data <- as.data.frame(do.call(cbind, subset))
      
    model <- lm(obj$`Excess Returns` ~.,data = subset_data)
    
    #returns the model
    return(model)
    
  }
  
  }
