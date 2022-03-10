library(coronavirus)
library(data.table)
library(ggplot2)
library(dplyr)
library("plyr")
library(tidyverse)

# Read the csv data and print some usefull information
vac = read.csv("...\\covid19_vaccine.csv",header=TRUE) # Insert your path
vac = data.table(vac)
vac
print(paste("Vaccine dataset dimensions:  Rows=",dim(vac)[1],"  Columns=" ,dim(vac)[2]))
earliest = min(vac[, date])
print(paste('Earliest date:',earliest,'. Countries included in dataset: ', dim(vac[, .N, by=country_region])[1]))
head(vac)
# Delete unused columns
vac[, c("code3","fips" , "province_state", "iso2", "iso3", "uid", "combined_key", 'continent_code'):=NULL]
vac = na.omit(vac)
omit = na.omit(vac, invert=FALSE)
dim(omit)
dim(vac)
tail(vac)
#Create new entries ####
vac[,'partially_vaccinated_ratio'] = vac[,'people_partially_vaccinated']/vac[,'population']
vac[,'fully_vaccinated_ratio'] = vac[,'people_fully_vaccinated']/vac[,'population']
# Convert to percentages
vac[,'partially_vaccinated_ratio'] = vac[, round(partially_vaccinated_ratio,3)*100]
vac[,'fully_vaccinated_ratio'] = vac[, round(fully_vaccinated_ratio,3)*100]


##### EXPLORATORY ANALYSIS BEGINS ######
last_day = vac[, max(report_date_string)]
print(paste("Last day of update is :",last_day))

### TOP 20 Most vaccinated Countries ###
# Group by country, sort by fully vaccinated ratio.
vac_ratios = vac[report_date_string==last_day, .(country_region, continent_name,fully_vaccinated_ratio,
                 partially_vaccinated_ratio, population),keyby=fully_vaccinated_ratio]
vac_ratios = vac_ratios[,-1]
# Grab the 25 most vaccinated.
vac_ratios
tail_r = tail(vac_ratios,25)
# Collapse the two variable ratios into a single df
cr_df <- tail_r %>% 
  select(country_region, partially_vaccinated_ratio, fully_vaccinated_ratio) %>%
  gather(key="variable", value="value", -country_region)
# Plot
ggplot(cr_df, aes( x=value, y=country_region,fill=variable)) +
  geom_bar(position="dodge",stat="identity", width=0.7) +
  theme_gray()+
  theme(axis.title.y = element_text(size = 11, face="bold"), axis.title.x = element_text(size = 11, face="bold"),
        title = element_text(size = 12, face="bold"),
        legend.position="top") +
  scale_fill_manual("Ratio type", values = c('navyblue','goldenrod1')) +
  scale_x_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  ggtitle("Top 25 Most Vaccinated Countries")+
  xlab("Continent")+ ylab('Vaccination Ratio (%)')+
  labs(caption = "(based on data from covid19_vaccine dataset)",
       plot.caption.position = "bottomright")
# Pie chart with the continent distribution of these countries 
make_pie_chart <- function(data, title){
    # Compute the position of labels
    data <- data %>% 
    arrange(desc(continent_name)) %>%
    mutate(prop = N / sum(data$N) *100.0) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop ) 
    data
    # Plot
    ggplot(data, aes( x='', y=prop,fill=continent_name)) +
    geom_bar(stat="identity", width=1, color='white') +
    theme_void()+
    theme(title = element_text(size = 14, face="bold"), legend.key.size = unit(1.5, 'cm')) +
    coord_polar("y", start=0) +
    ggtitle(title) + 
    geom_text(aes(y = ypos, x=1.2, label = sprintf('%1.1f',prop)), color = "black", size=5) +
    scale_fill_brewer(palette="Set1") }
freq = tail_r[, .N, by=continent_name]
make_pie_chart(data=freq, title='Continent distribution (%) of 25 most vaccinated countries')
# Pie chart with the continent distribution of lowest ratio countries
# Grab the 20 most vaccinated.
head_r = head(vac_ratios,25)
freq2 = head_r[, .N, by=continent_name]
make_pie_chart(data=freq2, title='Continent distribution (%) of 25 least vaccinated countries')
##### Ratios and Population  ####
# exclude population outliers china, India and keep ratio>68%
pop = vac[report_date_string==last_day & country_region!='China' & country_region!='India' & fully_vaccinated_ratio>60,
          .(country_region, fully_vaccinated_ratio, partially_vaccinated_ratio), keyby=(population/10^6)]

ggplot(pop, aes(x=(population), y=fully_vaccinated_ratio)) +
  geom_point(color='blue')+
  geom_smooth(color='red', size=1.5) +
  scale_x_continuous(breaks = round(seq(0, 360, by = 30),1)) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 11, face="bold"), axis.title.x = element_text(size = 11, face="bold"),
        title = element_text(size = 12, face="bold")) +
  ggtitle("Population vs Vaccination Rate  (countries with vaccinated ratio>60%)")+
  xlab("Population (in millions)")+ ylab("fully vaccinated ratio (%)") + 
  labs(caption = "China (86%) has been excluded for graphical reasons")

###### Whisker plot #######
ggplot(pop, aes(x=(population), y=fully_vaccinated_ratio)) +
  geom_boxplot(aes(group = cut_width(population, 10)),fill = "darkgoldenrod1",
                   colour = "navyblue",outlier.colour = "aquamarine", outlier.shape = 1)+
  geom_point(size=0.8, color='chartreuse1') +
  scale_x_continuous(breaks = round(seq(0, 360, by = 30),1)) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  theme_dark() +
  theme(axis.title.y = element_text(size = 10, face="bold"), axis.title.x = element_text(size = 11, face="bold"),
        title = element_text(size = 11, face="bold")) +
  ggtitle("Population vs Vaccination Rate  (countries with vaccinated ratio>60%)")+
  xlab("Population (in millions)")+ ylab("fully vaccinated ratio (%)") + 
  labs(subtitle="Maximum box width = 10 million",caption = "China (86%) has been excluded for graphical reasons")

###### Per continent  #######
continent_ratios =  vac[report_date_string==last_day,
                        .(population,people_fully_vaccinated, people_partially_vaccinated),
                        by=continent_name]
continent_ratios[, .N, by=continent_name]
vac[report_date_string==last_day & continent_name=='North America', .N,]
# Sum population and fully vac people for every continent then calculate continent ratios
continent_ratios = continent_ratios[, lapply(.SD, sum), by=.(continent_name)] 
continent_ratios[,'fully_vac_ratio'] = continent_ratios[,'people_fully_vaccinated']/continent_ratios[,'population']
continent_ratios[,'fully_vac_ratio'] = continent_ratios[, round(fully_vac_ratio,3)*100]
continent_ratios[,'partially_vac_ratio'] = continent_ratios[,'people_partially_vaccinated']/continent_ratios[,'population']
continent_ratios[,'partially_vac_ratio'] = continent_ratios[, round(partially_vac_ratio,3)*100]
continent_ratios
# Collapse the two variable ratios into a single df
c_df <- continent_ratios %>% 
  select(continent_name, partially_vac_ratio, fully_vac_ratio) %>%
  gather(key="variable", value="value", -continent_name)
c_df
# Plot
ggplot(c_df, aes( x=continent_name, y=value,fill=variable)) +
  geom_bar(position="dodge",stat="identity", width=0.7) +
  theme_gray()+
  theme(axis.title.y = element_text(size = 11, face="bold"), axis.title.x = element_text(size = 11, face="bold"),
        title = element_text(size = 12, face="bold"),
        legend.position="top") +
  scale_fill_manual("Ratio type", values = c('navyblue','goldenrod1')) +
  scale_y_continuous(breaks = round(seq(0, 60, by = 10),1)) +
  geom_text(aes(label=value, vjust=-.3, hjust=0.6)) +
  ggtitle("Vaccination Ratios per Continent")+
  xlab("Continent")+ ylab('Vaccination Ratio (%)')+
  labs(caption = "(based on data from covid19_vaccine dataset)")


##### Single Countries ###### 
###### PLOTTARE LINES diaforetika xrwmmata thn e3eli3h 5 xwrwn.
australia =  vac[country_region=='Australia', ]
greece =  vac[country_region=='Greece', ]
malta = vac[country_region=='Malta', ]
# Collapse the two variable ratios into a single df
df <- australia_per_month %>% 
  select(Month, partly_vac_ratio, fully_vac_ratio) %>%
  gather(key="variable", value="value", -Month)

dates = greece[,date]
dates
ggplot(data = greece, aes(x = 1:374, y = cumsum(fully_vaccinated_ratio), group=1)) + geom_line() + 
  geom_point() + theme(axis.text.x = element_text(angle=90, hjust = 1)) + 
  scale_x_discrete(labels = date) + xlab("Date")


##### Ratios during each month for Australia ######
# Get data grouped by month for australia
seasonality = australia[, .(doses_admin,population,people_partially_vaccinated,
                                people_fully_vaccinated), by=.(Month = paste0(month(date),'-',year(date)))]
sapply(seasonality, class)
seasonality[ , 'partly_vac_ratio'] = seasonality[, people_partially_vaccinated]/seasonality[,'population']
seasonality[ , 'partly_vac_ratio'] = round(seasonality[ , 'partly_vac_ratio'],3)*100
seasonality[ , 'fully_vac_ratio'] = seasonality[, people_fully_vaccinated]/seasonality[,'population']
seasonality[ , 'fully_vac_ratio'] = round(seasonality[ , 'fully_vac_ratio'],3)*100
# Keep only the first observation from each month
seasonality = seasonality[, .SD[c(1)], by=Month]
# Define level order (ggplot breaks the month order)
level_order <- c('12-2020','1-2021','2-2021','3-2021','4-2021','5-2021','6-2021','7-2021',
                 '8-2021','9-2021','10-2021','11-2021','12-2021','1-2022')
# Collapse the two variable ratios into a single df
df <- seasonality %>% 
  select(Month, partly_vac_ratio, fully_vac_ratio) %>%
  gather(key="variable", value="value", -Month)
# Plot
ggplot(df, aes(x=factor(Month, level=level_order), y=value, group=variable))+
  geom_line(aes(color=variable, linetype=variable))+
  scale_color_manual(values = c("chartreuse2", "blue1")) +
  geom_point(aes(color=variable),size=1.5) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  ylab('Vaccination Ratio (%)') + xlab('Month-Year') + ggtitle('Vaccination Ratio by Month for Australia')+
  labs(caption = "(based on data from covid19_vaccine dataset)") + theme_gray() +
  theme_bw() +
  theme(axis.title.y = element_text(size = 11, face="bold"), axis.title.x = element_text(size = 11, face="bold"),
        title = element_text(size = 12, face="bold"))  


##### Ratios during each month for Malta ######
# Get data grouped by month for malta
malta_per_month = malta[, .(doses_admin,population,people_partially_vaccinated, people_fully_vaccinated),
                        by=.(Month = paste0(month(date),'-',year(date)))]
sapply(malta_per_month, class)
malta_per_month[ , 'partly_vac_ratio'] = malta_per_month[, people_partially_vaccinated]/malta_per_month[,'population']
malta_per_month[ , 'partly_vac_ratio'] = round(malta_per_month[ , 'partly_vac_ratio'],3)*100
malta_per_month[ , 'fully_vac_ratio'] = malta_per_month[, people_fully_vaccinated]/malta_per_month[,'population']
malta_per_month[ , 'fully_vac_ratio'] = round(malta_per_month[ , 'fully_vac_ratio'],3)*100
# Keep only the first observation from each month
malta_per_month = malta_per_month[, .SD[c(1)], by=Month]
# Define level order (ggplot breaks the month order)
level_order <- c('12-2020','1-2021','2-2021','3-2021','4-2021','5-2021','6-2021','7-2021',
                 '8-2021','9-2021','10-2021','11-2021','12-2021','1-2022')
# Collapse the two variable ratios into a single df
df <- malta_per_month %>% 
  select(Month, partly_vac_ratio, fully_vac_ratio) %>%
  gather(key="variable", value="value", -Month)
# Plot
ggplot(df, aes(x=factor(Month, level=level_order), y=value, group=variable))+
  geom_line(aes(color=variable, linetype=variable))+
  scale_color_manual(values = c("goldenrod2", "darkorchid1")) +
  geom_point(aes(color=variable),size=1.5) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  ylab('Partially Vaccinated Ratio (%)') + xlab('Month-Year') + ggtitle('Partial Vaccination Ratio by Month for Malta')+
  labs(caption = "(based on data from covid19_vaccine dataset)") + theme_bw() +
  theme(axis.title.y = element_text(size = 11, face="bold"), axis.title.x = element_text(size = 11, face="bold"),
        title = element_text(size = 12, face="bold")) 


##### Ratios during each month for Greece ######
# Get data grouped by month for Greece
greece_per_month = greece[, .(doses_admin,population,people_partially_vaccinated, people_fully_vaccinated),
                              by=.(Month = paste0(month(date),'-',year(date)))]
sapply(greece_per_month, class)
greece_per_month[ , 'partly_vac_ratio'] = greece_per_month[, people_partially_vaccinated]/greece_per_month[,'population']
greece_per_month[ , 'partly_vac_ratio'] = round(greece_per_month[ , 'partly_vac_ratio'],3)*100
greece_per_month[ , 'fully_vac_ratio'] = greece_per_month[, people_fully_vaccinated]/greece_per_month[,'population']
greece_per_month[ , 'fully_vac_ratio'] = round(greece_per_month[ , 'fully_vac_ratio'],3)*100
# Keep only the first observation from each month
greece_per_month = greece_per_month[, .SD[c(1)], by=Month]
# Define level order (ggplot breaks the month order)
level_order <- c('12-2020','1-2021','2-2021','3-2021','4-2021','5-2021','6-2021','7-2021',
                 '8-2021','9-2021','10-2021','11-2021','12-2021','1-2022')
# Collapse the two variable ratios into a single df
df <- greece_per_month %>% 
  select(Month, partly_vac_ratio, fully_vac_ratio) %>%
  gather(key="variable", value="value", -Month)
# Plot
ggplot(df, aes(x=factor(Month, level=level_order), y=value, group=variable))+
  geom_line(aes(color=variable, linetype=variable))+
  scale_color_manual(values = c("magenta2", "cyan2")) +
  geom_point(aes(color=variable),size=1.5) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  ylab('Partially Vaccinated Ratio (%)') + xlab('Month-Year') + ggtitle('Partial Vaccination Ratio by Month for Greece')+
  labs(caption = "(based on data from covid19_vaccine dataset)") + theme_bw() +
  theme(axis.title.y = element_text(size = 11, face="bold"), axis.title.x = element_text(size = 11, face="bold"),
        title = element_text(size = 12, face="bold")) 

