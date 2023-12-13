######################## Dispersal function ########################################

# The function takes the current location of an individual and returns a new location
# based on a log-normal dispersal kernel or an exponential dispersal kernel

disperse <- function(cur_loc, d_sd, d_mean) {
  dis_sd <- d_sd
  dis_mean <- d_mean

  if (switch$kernel_type == 0) {
    sigma <- sqrt(log(1 + (dis_sd * dis_sd) / (dis_mean * dis_mean)))
    mu <- log(dis_mean) - 0.5 * sigma * sigma

    distance <- rlnorm(1, mu, sigma)
  } else if (switch$kernel_type == 1) {
    distance <- rexp(1, 1 / dis_mean)
  } else {
    print("please check switches")
  }

  direction <- runif(1, min = 0, max = 2 * pi)

  new_x <- cur_loc[1] + round(cos(direction) * distance)
  new_y <- cur_loc[2] + round(sin(direction) * distance)

  new_loc <- c(new_x, new_y)
  return(new_loc)
}