devtools::load_all("teal.devel")
session <- new.env()
session$userData <- new.env()
session$userData$chunks <- chunks$new()

x <- 5

myval %<chunk>%  {x + 3}

mysum %chunk>% sum(x, myval)

eval_chunk("mysum", session$userData$chunks)
