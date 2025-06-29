#install.packages(c('hexbin', 'RColorBrewer', 'scales', 'ggplot2'))

library(RColorBrewer)
library(ggplot2)
library(scales)
library(KernSmooth)

data(diamonds)

mydiamonds <- data.frame(lprice = log(diamonds$price, 2),
                            lcarat = log(diamonds$carat, 2))

# calculate lm
mylm <- lm(lprice ~ lcarat, data = mydiamonds)

# calculate NW kernel
myks <- ksmooth(x = mydiamonds$lcarat, y = mydiamonds$lprice,
                    kernel = 'normal', n.points = 200)

# calculate alternate smoother (also NW kernel, but using different defaults)
h <- dpill(x = mydiamonds$lcarat, y = mydiamonds$lprice)
fit <- locpoly(x = mydiamonds$lcarat, y = mydiamonds$lprice, bandwidth = h)

my_colors1 = brewer.pal(7, 'Blues')

d <- ggplot(mydiamonds, aes(lcarat, lprice))
d + geom_hex(binwidth = c(.15, .15)) +
    scale_fill_continuous(low = my_colors1[2], high = my_colors1[7]) +
    labs(x = "Log2 Carat" , y = "Log2 Price", title = "Diamonds Dataset") +
    geom_abline(intercept = coef(mylm)[1], slope = coef(mylm)[2],
                    col="red", lwd = 1.2, lty = 'dashed') +
    ylim(8.2, 16) +
    geom_line(data = data.frame(myks), aes(x = x, y = y), col = 'black', lwd = 1.1)
    #geom_line(data = data.frame(fit), aes(x = x, y = y), col = 'black', lwd = 1.1)
    #uncomment the above line to activate alternate smoother
