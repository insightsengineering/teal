p <- new_chunks()

# rlang

p %<chunks>% call2(
    title <- "AAA"
plot(title = title)
)
# plot(title = title)


title <- "Hello"
p %<chunks>% call2(
    plot(title = !title)
)
# plot(title = "Hello")



server{



  output$plot <- renderPlot({
        a <- input$fontsize
        ANL <- datasets$get_data()

        p %<chunks% {
          p <- ggplot(ANL)
        }

        if (!is.null(a)) {
          p %<chunks>% {
            p + theme(fontsize = !a)
          }
        }


      })

}





ASL <- read_bce("asl.sas7bat")
AAE <- read_bce("aae.sas7bdat")
ANL <- full_join(...) # this comes from teal


# we have dataname in a variable

# 1
p <- ggplot(ANL)
p <- p + theme(fontsize = 3)


# 2
p <- ggplot(ANL) + theme(fontsize = 3)


# !3
fs <- 3
p <- ggplot(ANL)
p <- p + theme(fontsize = fs)




# chunks
# substitute or not

# stack
# 1 add new call (new line or expression)
# 2 add to last call (add to previous)
# 3 evaluate remaining (2 afer 3 is not allowed)


ch %<chunks_init% (
      AAE_FILTERED %>% filter(AESERIOUS > 5)
      )

e <- new.env()
ch <- eval_remaining(ch, e)

validate(need(nrow(e$AAE_FILTERED) > 4, "need data"))

ch %<chunks1>% {
  ggplot(ANL)
}

ch %<chunks2>% {
  + theme(fontsize = !a)
}

ch %<chunks2>% {
  + facet_wrap()
}

ch
# chunks with calls
# 1. AAE_FILTERED %>% filter(AESERIOUS > 5)

e <- new.env()
eval(bquote(a <- 1), envir = e)
e$a
eval(bquote(a <- a + 1), envir = e)
e$a

lapply(stack, function(cl) {
      eval(cl, envir = 3)
    })


new_call <- combine_calls(stack)

eval(new_call, envir = e)

a <- bquote(a <- 1)
b <- bquote(b <- 1)

library(pryr)
ast(a)
a[[length(a) + 1]] <- b

call(a, b)


a <- 3
plot(a, a)
