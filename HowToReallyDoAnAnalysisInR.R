# Part 1: http://sharpsightlabs.com/blog/shipping-analysis-r-data-wrangling/

library(tidyverse)
library(stringr)
library(forcats)
library(ggmap)
library(rvest)

html.worldPorts <- read_html("https://en.wikipedia.org/wiki/List_of_busiest_container_ports")
df.worldPorts <- html_table(html_nodes(html.worldPorts, "table")[[2]], fill = TRUE)
glimpse(df.worldPorts)

# rename columns to lower case
colnames(df.worldPorts) <- colnames(df.worldPorts) %>% tolower()
colnames(df.worldPorts)

# get geocodes for these ports
geocodes.worldPorts <- geocode(df.worldPorts$port)
geocodes.worldPorts

# merge geocodes and data
df.worldPorts <- cbind(df.worldPorts, geocodes.worldPorts)
head(df.worldPorts)

# no missing geocodes
df.worldPorts[df.worldPorts$port=="Tanjung Pelepas",]
df.worldPorts[is.na(df.worldPorts$lon),]

# convert variables to factors
str(df.worldPorts)
df.worldPorts <- mutate(df.worldPorts,
                        economy = as.factor(str_trim(economy)),
                        port = as.factor(str_trim(port))
                      )

# relabel port names that are too long
levels(df.worldPorts$port)
df.worldPorts <- mutate(df.worldPorts,
                        portLabel = fct_recode(port,
                                               "Saigon" = "Ho Chi Minh City (Saigon)",
                                               "New York" = "New York and New Jersey",
                                               "Jakarta" = "Tanjung Priok (Jakarta)",
                                               "Bremen" = "Bremen/Bremerhaven",
                                               "Istanbul" = "Ambarli (Istanbul)",
                                               "Tangiers" = "Tanger-Med (Tangiers)",
                                               "Dubai" = "Jebel Ali (Dubai)",
                                               "Ningbo/Z-shan" = "Ningbo-Zhoushan"
                                               ))

# reshape the data in long format:
# rank, port, economy, portLabel, lon, lat, year, volume
df.worldPortsReshaped <-
  df.worldPorts %>% gather(year, volume, 4:14)
head(df.worldPorts)
head(df.worldPortsReshaped)
levels(as.factor(df.worldPortsReshaped$year))

# convert year to factor
df.worldPortsReshaped <- mutate(df.worldPortsReshaped, year = as.factor(year))

# manually rename the levels of the year
df.worldPortsReshaped <- mutate(df.worldPortsReshaped,
                                year = fct_recode(year
                                                  ,"2014" = "2014[1]"
                                                  ,"2013" = "2013[2]"
                                                  ,"2012" = "2012[3]"
                                                  ,"2011" = "2011[4]"
                                                  ,"2010" = "2010[5]"
                                                  ,"2009" = "2009[6]"
                                                  ,"2008" = "2008[7]"
                                                  ,"2007" = "2007[8]"
                                                  ,"2006" = "2006[9]"
                                                  ,"2005" = "2005[10]"
                                                  ,"2004" = "2004[11]"
                                                  )
)
levels(df.worldPortsReshaped$year)
head(df.worldPortsReshaped)

# change volume variable to numeric
df.worldPortsReshaped <- mutate(df.worldPortsReshaped,
                                volume = as.numeric(str_replace(volume, ",", "")))
glimpse(df.worldPortsReshaped)

# calculate shipping volume rankings
# drop the old rank first
df.worldPortsReshaped <- select(df.worldPortsReshaped, -1)
glimpse(df.worldPortsReshaped)
df.worldPortsReshaped <- df.worldPortsReshaped %>%
  group_by(year) %>%
  mutate(rank = min_rank(desc(volume))) %>%
  ungroup()
glimpse(df.worldPortsReshaped)
df.worldPortsReshaped %>% print(n=100)
df.worldPortsReshaped %>% filter(year == "2014") %>% print(n=100)

# create Continent manually
df.worldPortsReshaped <- df.worldPortsReshaped %>%
  mutate(
    continent = fct_collapse(
      economy
      ,
      South_America = c("Brazil", "Panama")
      ,
      North_America = c("Canada", "United States")
      ,
      Asia = c(
        "Japan",
        "China",
        "Hong Kong",
        "India",
        "Indonesia"
        ,
        "Malaysia",
        "Oman",
        "Philippines",
        "Saudi Arabia"
        ,
        "Singapore",
        "South Korea",
        "Sri Lanka"
        ,
        "Taiwan",
        "Thailand",
        "United Arab Emirates",
        "Vietnam"
      )
      ,
      Europe = c(
        "Belgium",
        "Germany",
        "Italy",
        "Malta",
        "Netherlands"
        ,
        "Spain",
        "Turkey",
        "United Kingdom"
      )
      ,
      Africa = c("Egypt", "Morocco")
    )
  )
glimpse(df.worldPortsReshaped)
head(df.worldPortsReshaped, 50)

# verify
df.worldPortsReshaped %>%
  group_by(continent, economy) %>%
  summarise(1) %>%
  print.data.frame()

# reorder the variables
head(df.worldPortsReshaped)
df.worldPortsReshaped <- select(df.worldPortsReshaped, rank, year, continent, economy, port, portLabel, lon, lat, volume)
head(df.worldPortsReshaped)

# final spot check against the Wikipedia table
df.worldPortsReshaped %>% filter(year == '2012', port == 'Guangzhou') %>% select(volume)       # 14744 OK
df.worldPortsReshaped %>% filter(year == '2007', port == 'Guangzhou') %>% select(volume)       # 9200 OK
df.worldPortsReshaped %>% filter(year == '2005', port == 'Rotterdam') %>% select(volume)       # 9287 OK
df.worldPortsReshaped %>% filter(year == '2005', port == 'Yingkou') %>% select(volume)         # 634 OK
df.worldPortsReshaped %>% filter(year == '2004', port == 'Yingkou') %>% select(volume)         # NA OK
df.worldPortsReshaped %>% filter(year == '2007', port == 'Keelung') %>% select(volume)         # NA OK
df.worldPortsReshaped %>% filter(year == '2014', port == 'Seattle/Tacoma') %>% select(volume)  # 3456 OK
df.worldPortsReshaped %>% filter(year == '2009', port == 'Nagoya') %>% select(volume)          # 2113 OK

# Part 2: http://sharpsightlabs.com/blog/shipping-analysis-r-data-visualization/

# get the data (if not yet available)
url.worldPorts <- url("http://sharpsightlabs.com/wp-content/datasets/world_ports.RData")
load(url.worldPorts)
glimpse(url.worldPorts)

#=========================================
# CREATE THEMES
# We'll create two themes:
#
# 1. theme.porttheme
#    - this will be a general theme that
#      we'll apply to most of our charts
#      to format the text, titles, etc
#
# 2. theme.smallmult
#    - we'll apply this exclusively to
#      "small multiple" charts
#      (AKA, trellis charts).  We need this
#      because the axis text needs to be
#      smaller for the small multiples
#=========================================


#----------------------------------------
# GENERAL THEME
# - we'll use this for most of our charts
#   and build on it when we need to
#----------------------------------------
theme.porttheme <-
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank())

#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
# - there are several bar charts that
#   are very wide, and need some
#   special formatting
#------------------------------------

theme.widebar <-
  theme.porttheme +
  theme(plot.title = element_text(size = 30)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(legend.title = element_blank(), legend.background = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = c(.9,.55)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))

#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
#  - we'll use this for small multiple
#    charts.  these also have some
#    special formatting requirements
#------------------------------------

theme.smallmult <-
  theme.porttheme +
  theme(axis.text = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90))

# Chart the volume of shipping by port for 2014
df.worldPortsReshaped %>%
  filter(year == 2014) %>%
  ggplot(aes(x=reorder(portLabel, desc(volume)), y=volume)) +
  geom_bar(stat = "identity", fill = "dark red") +
  labs(title = "Busiest container ports in the world") +
  labs(subtitle = "2014, in order of shipping volume") +
  labs(x="Port", y="Shipping\nVolume") +
  scale_y_continuous(labels = scales::comma_format()) +
  theme.widebar
# Flip it
df.worldPortsReshaped %>%
  filter(year == 2014, rank <= 25) %>%
  ggplot(aes(x=reorder(portLabel, volume), y=volume)) +
  geom_bar(stat = "identity", fill = "dark red") +
  geom_text(aes(label = volume), hjust = 1.1, color = "#FFFFFF") +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip() +
  labs(title = "Shanghai, Singapore had much higher volume\nthan other high-volume ports in 2014") +
  labs(x="Port", y="Shipping Volume\n(1000 TEUs)") +
  theme.porttheme

# Ports in China
df.worldPortsReshaped %>%
  mutate(chinaFlag = ifelse(economy == "China", "China", "Not China")) %>%
  filter(year == 2014) %>%
  ggplot(aes(x=reorder(portLabel, desc(volume)), y=volume)) +
  geom_bar(stat = "identity", aes(fill = chinaFlag)) +
  labs(title = "Roughly 20% of the busiest ports\nwere in China in 2014") +
  labs(x="Port", y="Shipping\nVolume\n(1000 TEUs)") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("dark red","#999999")) +
  theme.widebar

