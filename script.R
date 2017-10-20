library(keras)
library(tidyverse)


# data set
day1 <- do.call(
	rbind,
	lapply(
		list.files("results", "^\\d{6}\\.csv$", full.names = TRUE),
		read_csv
	)
) %>% 
	filter(day == 1) %>% 
	transmute(
		basho = as.character(basho),
		rank1 = sub("HD$", "", rank1),
		rank2 = sub("HD$", "", rank2)
	)


# rank string -> ordinal integer
rank2int <- Vectorize(
	function(rank_string) as.integer(
		sprintf(
			"%d%02d%d",
			rank_string %>% 
				sub("\\d+.*$", "", .) %>% 
				match(c("Y", "O", "S", "K", "M", "J", "Ms", "Sd", "Jd", "Jk")),
			rank_string %>% 
				gsub("\\D", "", .) %>% 
				as.integer(),
			rank_string %>% 
				sub("^.+\\d+", "", .) %>% 
				sub("HD$", "", .) %>% 
				match(c("e", "w"))
		)
	)
)


# all makuuchi ranks found in data set
makuuchi <- tibble(rank_string = c(day1$rank1, day1$rank2) %>% unique()) %>% 
	arrange(rank2int(rank_string)) %>% 
	filter(rank2int(rank_string) < 6000) %>% 
	pull(rank_string)


# encodes & decodes rank strings
one_hot <- function(ranks) as.integer(
	1:(length(makuuchi) + 1) %in% match(ranks, makuuchi, nomatch = length(makuuchi) + 1)
)

rev_one_hot <- function(m) apply(m, 1, which.max) %>% makuuchi[.]


# arranges matchups as high rank vs low rank & sorts by high rank
banzuke_sort <- function(banzuke) banzuke %>% 
	transmute(
		rank_high = ifelse(rank2int(rank1) < rank2int(rank2), rank1, rank2),
		rank_low = ifelse(rank2int(rank1) > rank2int(rank2), rank1, rank2)
	) %>% 
	arrange(rank2int(rank_high))


# basho (yyyy.mm) -> matrix with 0's and 1's
basho2xy <- function(b) {
	tmp <- day1 %>% 
		filter(basho == b) %>% 
		banzuke_sort()
	
	do.call(
		rbind,
		lapply(
			1:nrow(tmp),
			function(i) c(
				tmp %>% 
					filter(row_number() >= i) %>% 
					gather(var, val) %>% 
					pull(val) %>% 
					tail(-1) %>% 
					one_hot(),
				tmp[i, 2] %>% one_hot()
			)
		)
	)
}


# process & split data set
xy <- do.call(
	rbind,
	lapply(
		day1 %>% pull(basho) %>% unique(),
		basho2xy
	)
)

x_train <- xy[-(3184:3204), 1:(length(makuuchi) + 1)]
y_train <- xy[-(3184:3204), (length(makuuchi) + 2):((length(makuuchi) + 1) * 2)]

x_test <- xy[3184:3204, 1:(length(makuuchi) + 1)]
y_test <- xy[3184:3204, (length(makuuchi) + 2):((length(makuuchi) + 1) * 2)]


# model
model <- keras_model_sequential() %>% 
	layer_dense(
		units = (length(makuuchi) + 1) * 64,
		activation = "relu",
		input_shape = length(makuuchi) + 1
	) %>% 
	#layer_dropout(rate = .25) %>% 
	layer_dense(
		units = (length(makuuchi) + 1) * 16,
		activation = "relu",
	) %>% 
	#layer_dropout(rate = .25) %>% 
	layer_dense(
		units = (length(makuuchi) + 1) * 4,
		activation = "relu"
	) %>% 
	#layer_dropout(rate = .25) %>% 
	layer_dense(
		units = length(makuuchi) + 1,
		activation = "softmax"
	)

#my_loss <- function(y_true, y_pred) 1 - sum(y_true * y_pred)

model %>% 
	compile(
		optimizer = optimizer_adam(lr = 0.0001),
		loss = "categorical_crossentropy",# my_loss,
		metrics = "accuracy"
	)

model %>% 
	fit(
		x_train,
		y_train,
		epochs = 10,
		callbacks = callback_tensorboard(),
		validation_split = .2
	)


# moment of truth
result <- "Y1e"

x <- x_test[1, ]

while (sum(x) > 0) {
	matchup <- x %>% 
		t() %>% 
		predict(model, .) %>% 
		which.max() %>% 
		makuuchi[.]
	
	up_next <- x %>% 
		which.max() %>% 
		makuuchi[.]
	
	x <- x * (1 - one_hot(matchup)) * (1 - one_hot(up_next))
	
	result <- c(result, matchup, up_next)
}

result %>% 
	matrix(ncol = 2, byrow = TRUE) %>% 
	as.tibble() %>% 
	View()
