centered <- function(df, var_names){
  for (name in var_names){
    df[, name] <- df[, name]-mean(df[, name])
  }
  return (df)
}