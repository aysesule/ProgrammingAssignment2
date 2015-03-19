cacheSet <- function(x = NULL) {
  cache1 <- x
  getfunction <- function() cache1
  
  setfunction <- function(y = NULL) {
    cache1 <<- y 
  }
  
  list(get = getfunction, set = setfunction)
}

cacheGetSet <- function(cache, x = NULL, ...) {
  cache2 <- cache$get()
  message("Checking cache...")
  if(is.null(cache2)) {
    message("No previously cached value.")
  } else { 
    print(cache2)
  }
  message("Setting new cached value...")
  cache$set(x)
  print(cache$get())
  message("Finished.")       
}
