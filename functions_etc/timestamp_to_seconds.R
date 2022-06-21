#a function to take a timestamp string in "00:00:00.000" format and covert to numeric seconds

function(timestampStr){
  require(lubridate)
  
  return(as.numeric(seconds(hms(timestampStr))))
  
  
}