source("sumodbBanzukeSimpleQuery.R")

df <- expand.grid(
	year = 1991:2017,
	month = seq(1, 11, by = 2)
)

basho <- sprintf(
  "%s%02d",
  df$year,
  df$month
)

sapply(
	basho,
	function(x) {
	  df <- sumodbBanzukeSimpleQuery(x)
	  if (is.data.frame(df)) write_csv(df, paste(x, "csv", sep = "."))
	}
)
