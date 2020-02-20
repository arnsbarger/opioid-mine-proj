
x = seq(2000,2020,1)

run = x -2000

preA = 10*run + 100
postA = 30*run + 100
A = c(preA[1:12], postA[13:21] - preA[13])

A = data.frame(x = x, y = c(preA[1:12], postA[13:21] - preA[13]), which = "A")
A_counter = data.frame(x = x, y = c(rep(NA,11), preA[12:21]), which = "A_counter")

BASE <- 
ggplot() + 
    geom_line(data = A, aes(x = x, y = y), linetype = 1) + 
    geom_line(data = A_counter, aes(x = x, y = y), linetype = 2)


bad_control = data.frame(x = x, y = 2*run + 200) 
BASE + geom_line(data = bad_control, aes(x = x, y = y), color  = "red")


preB = 10*run + 70
postB = 30*run -230
B = data.frame(x = x, y = c(preB[1:16], postB[17:21]) + 20)

BASE + geom_line(data = B, aes(x = x, y = y), linetype = 1, color = "blue")
