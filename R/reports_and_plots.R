#temp factors plot

data %>%
  subset(WI > 500) %>%
  ggplot(aes(x=year)) + geom_line(aes(y=WI, color = 'wI')) + geom_line(aes(y=HI, color = 'HI')) + geom_line(aes(y=BEDD, color='BEDD')) + geom_line(aes(y=FFD, color = 'FFD')) +  geom_smooth(aes(y=WI), method = 'lm') + theme_bw()


#precip plot

#index plot
