

# eff 

eff = data.frame(ppt = c(1:50))
eff$x = rnorm(n = 50, mean = 50, sd = 15)
eff$slope = -5
eff$yint = 0

eff = eff %>% 
  mutate(y = x*slope)

plot(eff$x, eff$y)
# drop off 
# eff 
drop = data.frame(ppt = c(51:100))
drop$x = runif(n = 50, min = 65, max = 100)
drop$slope = 0
drop$yint = 0

drop$y = x*slope + yint

cdf = rbind(eff,drop)

drop %>% 
  ggplot(aes(x, y)) + geom_point() + geom_smooth()