# save plot to file
d <- ggplot(mydiamonds, aes(lcarat, lprice))

png("Basic Hexbin.png", 850, 850)
d <- ggplot(mydiamonds, aes(lcarat, lprice))
d + geom_hex(binwidth = c(.15, .15)) +
    scale_fill_continuous(low = my_colors1[2], high = my_colors1[7]) +
    labs(x = "Log2 Carat" , y = "Log2 Price", title = "Diamonds Dataset")
dev.off()

png("smooth_diamonds.png", 850, 850)
d + geom_hex(binwidth = c(.15, .15)) +
    scale_fill_continuous(low = my_colors1[2], high = my_colors1[7]) +
    labs(x = "Log2 Carat" , y = "Log2 Price", title = "Diamonds Dataset") +
    geom_abline(intercept = coef(mylm)[1], slope = coef(mylm)[2],
                    col="red", lwd = 1.2, lty = 'dashed') +
    ylim(8.2, 16) +
    theme(plot.title = element_text(size=14,face="bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")) +
    geom_line(data = data.frame(myks), aes(x = x, y = y), col = 'black', lwd = 1.1)
    #geom_line(data = data.frame(fit), aes(x = x, y = y), col = 'black', lwd = 1.1)
    #uncomment the above line to activate alternate smoother
dev.off()
