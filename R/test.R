# Size, margins, colors

month <- "1"

months <- 12
for (nMonth in 0:nMonths) {
  tMonth <- now() + months(nMonth)
  tMonthLabel <- month(tMonth,label = TRUE, abbr = FALSE)
  monthInitial <- substr(tMonthLabel, 1,1)

# Pick a color for background. Base on which month

# Create background

# Save background

# Create calendar

# set color based on contrast with background
dateFont <- "purple"
wdayFont <- element_text(color = dateFont, size = 11)
quoteFont <- element_text(color = dateFont, size = 11, face = "italic")
dayLabs <- c("S","M","T","W","H","F","S")

# Set a quote for bottom
calQuote <- "Drink your Ovaltine"

cal = calendar(begin = first_day_in_month(tMonth), length = days(days_in_month(begin) - 1))
baseCal <- qplot(lubridate::wday(date, label=TRUE),
                 weekrow(date), label=day, data = cal, geom = "text", color=I(dateFont)) +
  geom_tile(alpha=0) + scale_y_reverse()

finalCal <- baseCal + theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            axis.title.x = wdayFont,
                            axis.title.y = element_blank(),
                            axis.text.y = element_blank(),
                            axis.text.x = wdayFont,
                            axis.ticks = element_blank()
                            ) +
  scale_x_discrete(position = "top", labels = dayLabs) + labs(x = toupper(tMonthLabel))+
  annotate("text", x = 4, y = 4.5, label = calQuote)

imgfile <- tempfile(, fileext=".png")
download.file("https://avatars1.githubusercontent.com/u/626539?v=3&u=e731426406dd3f45a73d96dd604bc45ae2e7c36f&s=140",
              destfile=imgfile, mode='wb')

annotate("text",x=4, y = -10, label = monthInitial) + annotate("text",x=4, y = -10, label = monthInitial)


# Save calendar
ggsave("plots/january.png")

file <- tempfile()
ggsave(file, device = "png")
unlink(file)
