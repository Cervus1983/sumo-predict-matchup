library(keras)
library(tidyverse)


rank2int <- Vectorize(
  function(rank_string) as.integer(
    sprintf(
      "%d%02d%d",
      rank_string %>% sub("\\d+.*$", "", .) %>% match(c("Y", "O", "S", "K", "M", "J", "Ms", "Sd", "Jd", "Jk")),
      rank_string %>% gsub("\\D", "", .) %>% as.integer(),
      rank_string %>% sub("^.+\\d+", "", .) %>% sub("HD$", "", .) %>% match(c("e", "w"))
    )
  )
)


results <- do.call(
  rbind,
  lapply(
    list.files("results", "^\\d{6}\\.csv$", full.names = TRUE),
    read_csv
  )
) %>% 
  # remove play-offs
  filter(day < 16)


day1 <- results %>% 
  filter(day == 1) %>% 
  transmute(
    basho,
    rank1 = sub("HD$", "", rank1),
    rank2 = sub("HD$", "", rank2)
  )

day1 <- rbind(
  day1 %>% filter(rank2int(rank1) < 6000) %>% select(basho, rank.x = rank1, rank.y = rank2),
  day1 %>% filter(rank2int(rank2) < 6000) %>% select(basho, rank.x = rank2, rank.y = rank1)
) %>% 
  arrange(basho, rank2int(rank.x)) %>% 
  mutate(rank.y = ifelse(rank2int(rank.y) < 6000, rank.y, "J"))


ranks <- day1 %>% 
  select(rank.y) %>% 
  unique() %>% 
  arrange(rank2int(rank.y)) %>% 
  pull(rank.y)


rank2categorical <- Vectorize(
  function(rank_string) rank_string %>% 
    match(., ranks) %>% 
    `-`(1) %>% 
    to_categorical(num_classes = length(ranks))
)


banzuke <- sapply(
  day1 %>% pull(basho) %>% unique(),
  function(b) day1 %>% 
    filter(basho == b) %>% 
    pull(rank.x) %>% 
    rank2categorical() %>% 
    apply(1, max)
)


x <- do.call(
  rbind,
  lapply(
    1:nrow(day1),
    function(i) c(
      day1[i, ]$rank.x %>% rank2categorical(),
      day1 %>% pull(basho) %>% unique() %>% match(day1[i, ]$basho, .) %>% banzuke[, .]
    )
  )
)


y <- do.call(
  rbind,
  lapply(
    1:nrow(day1),
    function(i) day1[i, ]$rank.y %>% rank2categorical() %>% t()
  )
)


model <- keras_model_sequential() %>% 
	layer_dense(
		units = dim(x)[2] * 20,
		activation = "relu",
		input_shape = dim(x)[2]
	) %>% 
	layer_dense(
		units = dim(x)[2] * 10,
		activation = "relu",
	) %>% 
  #layer_dropout(rate = .25) %>% 
	layer_dense(
		units = dim(x)[2] * 5,
		activation = "relu"
	) %>% 
  #layer_dropout(rate = .5) %>% 
  layer_dense(
		units = dim(y)[2],
		activation = "softmax"
	)

model %>% 
  compile(
  	optimizer = "adam",
  	loss = "categorical_crossentropy",
  	metrics = "accuracy"
  )

model %>% 
  fit(
  	head(x, -37),
  	head(y, -37),
  	epochs = 10,
  	callbacks = callback_tensorboard(),
  	validation_split = .2
  )

model %>% evaluate(tail(x, 37), tail(y, 37))


# duplicate rank.y.pred
day1_pred <- day1 %>% 
  tail(37) %>% 
  mutate(rank.y.pred = NA)

m <- model %>% predict(tail(x, 37))

m <- t(t(m) * c(head(banzuke[, 160], -1), 1))


while (day1_pred %>% filter(!complete.cases(.)) %>% nrow() > 0) {
  max_probability <- which(m == max(m), arr.ind = TRUE)
  
  if (is.na(day1_pred[max_probability[1], ]$rank.y.pred)) {
    day1_pred[max_probability[1], ]$rank.y.pred <- ranks[max_probability[2]]
    day1_pred[day1_pred$rank.x == ranks[max_probability[2]], ]$rank.y.pred <- day1_pred[max_probability[1], ]$rank.x
  }
  
  m[max_probability[1], max_probability[2]] <- 0
}
