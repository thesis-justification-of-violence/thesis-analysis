elsoc <- readRDS("input/data/procjv_long.RDS")

text_object <- # create text object
  create.text.object(
    "sj_ceo_rec",
    "jv_carab2",
    demo = FALSE
  )
list2env(text_object, envir = globalenv()) # transfer list elements to global environment

fit <- list() # Create empty list for storing lavaan objects

models <- c("a1", "a2", "b1", "b2", "c1", "c2", "d1", "d2") # vector with models names

for (i in models){
  fit[[i]] <- lavaan(model = c(bwcomp,get(i),varcov),
                     data = elsoc, 
                     estimator = "MLR", 
                     missing = "FIML",
                     meanstructure = T, 
                     int.ov.free = T) # Execute estimate
}

fit$b1 %>% summary()
