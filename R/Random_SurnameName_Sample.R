# Taking a random sample of names from census frequent names.
random_surname_sample <- Names_2010Census[sample(nrow(Names_2010Census), 20), ]
random_surname_sample_names <- random_surname_sample$name
random_surname_sample_names
