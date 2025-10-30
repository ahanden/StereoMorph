rotateImg <- function(in_file, out_file, rotate = NULL, flip = NULL) {

  check_system_command <- function(command) {
    tryCatch(
      { 
        system2(command=command, args="--version", stdout=TRUE, stderr=FALSE)
      },
      error=function(e) success <- FALSE,
    )
  }

  if (check_system_command("magick")) {
    command <- "magick"
  } else if (check_system_command("convert")) {
    command <- "convert"
  } else {
    stop("Can't find magick or convert")
  }

  args <- c()
  if (!is.null(rotate)) {
    args <- c(args, "-rotate", rotate)
  }
  if (flip %in% c("flip", "flop")) {
    args <- c(args, paste0("-", flip))
  }

  args <- c(shQuote(in_file), args, shQuote(out_file))
  system2(command=command, args=args , stdout=TRUE, stderr=TRUE)
}
