#' Match the timestep of two dataframes
#'
#' Match the timestep of two different time series dataframes so they have the same number of steps. Works with long and wide-from data.
#' @param df1 dataframe; Data frame with timestep to be matched with.
#' @param df2 dataframe; Data frame with data to be matched to the timestep of df1
#' @return Dataframe of observation data subset to the same timestep as modelled data
#' @export
match_tstep <- function(df1, df2){
  if(df1[1,1] == df1[2,1]){
    df = data.frame(DateTime = unique(df1[,1]))
    df = merge(df, df2, by = 1)
    return(df)
  }else{
    tim1 = df1[,1]
    tim2 = df2[,1]
    ind = c()
    pb = txtProgressBar(min = 0, max = length(tim1), style = 3)
    for(i in 1:length(tim1)){
      ind = append(ind, which(tim2 == tim1[i]))
      setTxtProgressBar(pb, i)
    }
    close(pb)
    df2.mat = df2[ind,]
    return(df2.mat)
  }
}
