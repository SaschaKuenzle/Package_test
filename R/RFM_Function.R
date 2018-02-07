#' Calculate RFM
#'
# Description
#' Calculate the weighted RFM Score: recency, frequency, monetary value for every customer
#
#  Arguments
#'@param  data - A data.table containing the transaction records.
#'@param  WeightRecency - The weight for recency.
#'@param  WeightFrequency - The weight for frequency.
#'@param  WeightMonetary - The weight for monetary.
#'
#'@details
#'  \code{data} contains the transactional data
#'
#  Return
#'@return Returns the score and group

RFM_Function <- function(data, WeightRecency = 1/3, WeightFrequency = 1/3, WeightMonetary = 1/3) {

  # Calculate Recency, Frequency and Monetary Value Score to rank customer according to relative importance

  print("Scaling sum of weights to 1")
  WeightRecency2 <- WeightRecency / (WeightRecency + WeightFrequency + WeightMonetary)
  WeightFrequency2 <- WeightFrequency / (WeightRecency + WeightFrequency + WeightMonetary)
  WeightMonetary2 <- WeightMonetary / (WeightRecency + WeightFrequency + WeightMonetary)
  print(paste0("Frequency Weight: ", WeightFrequency2))
  print(paste0("Recency Weight: ", WeightRecency2))
  print(paste0("Monetary Weight: ", WeightMonetary2))

  # Convert to date
  #data[, TransDate := lubridate::dmy(TransDate, tz = "UTC")]

  # Get Max date
  max.Date <- max(data$TransDate)

  # Create RFM Vars
  temp <- data[, list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = sum(PurchAmount)
  ), by = Customer]

  # Calculate Scores
  temp <- temp[, list(
    Customer,
    RecencyScore = as.numeric(cut2(-recency, g = 3)),
    FrequencyScore = as.numeric(cut2(frequency, g = 3)),
    MonetaryScore = as.numeric(cut2(monetary, g = 3))
  ), by = Customer]

  # Final Score and Group
  temp[, Score := WeightRecency2 * RecencyScore + WeightFrequency2 * FrequencyScore + WeightMonetary2 * MonetaryScore, by = Customer]
  temp[, Group := round(Score), by = Customer]

  return(temp)
}
