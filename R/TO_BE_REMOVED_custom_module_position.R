mod <- example_module(label, datanames, transformers)

transform_id <- function(id) {NS(gsub("-module$", "", id), "data_transform")}

custom_module_position <- function(mod, position = 1) {

  insert <- str2lang("ui_transform_data(transform_id(id), transformers)")

  line <- which(grepl('encoding', as.character(body(mod$ui))))
  element <- which(grepl('div(', as.character(body(mod$ui)[[line]]), fixed = TRUE))
  encoding <- body(mod$ui)[[line]][[element]]

  # This loop is here, because regular append breaks the last call and treats it like a call over a call
  position <- position + 1
  new_encoding <- list()

  if (position == length(encoding) + 1) {
    for(i in 1:length(encoding)) {
      new_encoding <- c(new_encoding, encoding[[i]])
    }
    new_encoding <- c(new_encoding, insert)
  } else {
    for(i in 1:(length(encoding))) {
      if(i == position) {
        new_encoding[[i]] <- insert
        new_encoding[[i+1]] <- encoding[[i]]
      } else {
        new_encoding <- c(new_encoding, encoding[[i]])
      }
    }
  }


  # Below does not work as it breaks last element and treat is as a call over a call
  # encoding_substitute <-
  #   c(encoding[[1:(position-1)]], insert, encoding[[(position):length(encoding)]])

  # Below does not work as it breaks last element and treat is as a call over a call
  # append(body(mod$ui)[[line]][[element]], insert, after = position)

  body(mod$ui)[[line]][[element]] <- as.call(new_encoding)
  attr(mod$transformers, "custom_ui") <- TRUE
  mod
}

mod$ui
custom_module_position(mod, position = 1)$ui
custom_module_position(mod, position = 2)$ui
custom_module_position(mod, position = 3)$ui
