makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  print(get)
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}
cacheSolve <- function(x) {
  m <- x$getsol()
  if(!is.null(m)) {
    message("getting inversed matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsol(m)
  m
}


