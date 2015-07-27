setwd("/Users/yuanfengwang/Documents/others/codetest_3489908/namesbystate")
library(dplyr)

# load the data
files <- list.files("/Users/yuanfengwang/Documents/others/codetest_3489908/namesbystate", pattern = ".TXT$")

name_data <- data.frame()
for (f in files) {
  print(paste("reading", f))
  name_data <- rbind(name_data, read.csv(f, header=F))
}
colnames(name_data) <- c("state", "gender", "year", "name", "count")
dim(name_data)
summary(name_data)
save.image("name_data.RData")
gc()

# STATISTICS BY STATE
name_data_state <- group_by(name_data, state)
summary_bystate <- summarise(name_data_state,
                   count = n(),
                   min_year = min(year),
                   max_year = max(year))
summary_bystate

#visualize by state
library(rgdal)

# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
states <- readOGR("~/Documents/others/cb_2013_us_state_20m/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m", verbose = FALSE)

neStates <- subset(states, states$STUSPS %in% c(
  "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))

leaflet(neStates) %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    color = ~colorQuantile("YlOrRd", states$AWATER)(AWATER)
  )

# BY STATE AND GENDER
name_data_state_gender <- group_by(name_data, state, gender)
summary_bystate_gender <- summarise(name_data_state_gender,
                             num_newbore = sum(count))
summary_bystate_gender

diff_num_newborn <- (summary_bystate_gender$num_newbore[seq(1, nrow(summary_bystate_gender), 2)]
 - summary_bystate_gender$num_newbore[1+seq(1, nrow(summary_bystate_gender), 2)])
sum(diff_num_newborn >0)

# BY GENDER AND NAME
name_data_gender_name <- group_by(name_data, gender, name)
summary_bygendername <- summarise(name_data_gender_name,
                            num_newborn = sum(count))

# most popular names of all time
arrange(filter(summary_bygendername, gender=="F"), desc(num_newborn))
arrange(filter(summary_bygendername, gender=="M"), desc(num_newborn))


# most gender ambigious name of 2013

name_data_2013F <- group_by(filter(name_data, year==2013, gender=="F"), name)
summary_2013F <- summarise(name_data_2013F,
                           num_newbore = sum(count))
name_data_2013M <- group_by(filter(name_data, year==2013, gender=="M"), name)
summary_2013M <- summarise(name_data_2013M,
                           num_newbore = sum(count))
common_name <- inner_join(summary_2013F, summary_2013M, by=c("name"))
overlapping_percentage <- pmin(common_name$num_newbore.x,common_name$num_newbore.y)/ (common_name$num_newbore.x+common_name$num_newbore.y)
common_name$name[order(overlapping_percentage)[seq(nrow(common_name), nrow(common_name)-10, -1)]]

# most gender ambigious name of 1945


name_data_1945F <- group_by(filter(name_data, year==1945, gender=="F"), name)
summary_1945F <- summarise(name_data_1945F,
                           num_newbore = sum(count))
name_data_1945M <- group_by(filter(name_data, year==1945, gender=="M"), name)
summary_1945M <- summarise(name_data_1945M,
                           num_newbore = sum(count))
common_name <- inner_join(summary_1945F, summary_1945M, by=c("name"))
overlapping_percentage <- pmin(common_name$num_newbore.x,common_name$num_newbore.y)/ (common_name$num_newbore.x+common_name$num_newbore.y)
common_name$name[order(overlapping_percentage)[seq(nrow(common_name), nrow(common_name)-10, -1)]]

# percentage of names
name_data_before1980_byname <- group_by(filter(name_data, year<1980), name)
summary_before1980_byname <- summarise(name_data_before1980_byname,
                                       num_newbore = sum(count))

percentage_before1980 <- data.frame(name=summary_before1980_byname$name, percentage=summary_before1980_byname$num_newbore/sum(name_data_before1980_byname$count))


name_data_after1980_byname <- group_by(filter(name_data, year>=1980), name)
summary_after1980_byname <- summarise(name_data_after1980_byname,
                                       num_newbore = sum(count))

percentage_after1980 <- data.frame(name=summary_after1980_byname$name, percentage=summary_after1980_byname$num_newbore/sum(name_data_after1980_byname$count))

fill_na <- function(v, repl) {
  if( !is.vector(v)) stop("please input a vector")
  v[is.na(v)] <- repl
  return(v)
}

all_perc <- left_join( percentage_after1980, percentage_before1980, by=c("name"))
all_perc$diff_perc <- all_perc$percentage.x - fill_na(all_perc$percentage.y,0)

arrange(all_perc, desc(diff_perc))[1:10,]

all_perc2 <- left_join(percentage_before1980,percentage_after1980, by=c("name"))
all_perc2$diff_perc <- all_perc2$percentage.x - fill_na(all_perc2$percentage.y,0)

arrange(all_perc2, desc(diff_perc))[1:10,]

# number of distinct names by year

name_data_year_gender <- group_by(name_data, year, gender)
summary_year_gender <- summarise(name_data_year_gender,
                                 cnt_distinct_name = n_distinct(name),
                                 num_newborn = sum(count))

# plot the total number of distinct names by year
F_cnt <- filter(summary_year_gender,gender=="F")
plot(F_cnt$year, F_cnt$cnt_distinct_name, xlab="year", ylab="Number of distinct names")
M_cnt <- filter(summary_year_gender,gender=="M")
points(M_cnt$year, M_cnt$cnt_distinct_name, col=2)
legend(x=1910, y=6000, col=c("black", "red"), pch=1, legend = c("Female", "Male"))

# plot the total number of newborn
plot(F_cnt$year, F_cnt$num_newborn, xlab="year", ylab="Number of newborn")
points(M_cnt$year, M_cnt$num_newborn, col=2)
legend(x=1910, y=max(M_cnt$num_newborn)*0.9, col=c("black", "red"), pch=1, legend = c("Female", "Male"))


# read immigration data
require(gdata)
immigration_data <- read.table("../Immigrants_Population_2013.tsv", header=F)
colnames(immigration_data) <- c("year", "number", "percentage")
immigration_data<- filter(immigration_data, year>1905)
plot(immigration_data$year, immigration_data$number, xlab="Year", ylab="Population", main="Immigrant Population")


t <- inner_join(immigration_data, F_cnt, by= c("year"))
summary(t)
cor(t$cnt_distinct_name, t$number, method="pearson")
