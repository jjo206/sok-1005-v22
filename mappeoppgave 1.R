library(tidyverse)
library(ggplot2)
library(readr)
library(zoo)

##-----------
#Oppgave 1

library(readr)
uahncdc_lt_6_0 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
View(uahncdc_lt_6_0)

##---------

lower_troposphere <- uahncdc_lt_6_0 %>%
  filter(Year <= 2021)


##---------


lower_troposphere[ , 1:29] <- apply(lower_troposphere[ , 1:29], 2,          
                                    function(x) as.numeric(as.character(x)))


##----------

lower_troposphere_2 <- lower_troposphere %>% 
  mutate(RollmeanGlobe = rollmean(Globe, k = 13, fill = NA))


str(lower_troposphere)


##----------

lower_troposphere_2 %>% 
  ggplot(aes(x = Year, y = Globe))+
  geom_point(alpha = 0.3, color = 'dark blue')+
  geom_line(alpha = 0.2, color = 'dark blue')+
  geom_smooth(aes(y = RollmeanGlobe, col = "13-måneders \n glidende \n gjennomsnitt"), span = 0.1)+
  #geom_dl(label= 'hei', method="maxvar.points", inherit.aes=T)
  #geom_line(aes(y = RollmeanGlobe), color = 'red')+
  #stat_smooth(aes(y = RollmeanGlobe), color = 'red')+
  theme_bw()+
  geom_hline(yintercept= 0, alpha = 0.5)+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-0.7, 0.9), 
                     breaks = scales::breaks_width(0.1))+
  scale_x_continuous(breaks = scales::breaks_width(2))+
  labs(title = 'Nedre Troposfære', subtitle = 'Temperaturer 1978-2021', x = 'År', col = '')

##----------

lower_troposphere %>% 
  ggplot(aes(x = Year, y = Globe))+
  geom_line()+
  geom_line()+
  theme_bw()+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-0.7, 0.9), 
                     breaks = scales::breaks_width(0.1))+
  scale_x_continuous(breaks = scales::breaks_width(2))




#Oppg 2


library(tidyverse)
library(ggplot2)
library(readr)
library(zoo)
library(dplyr)
library(cowplot)


##----LOWER TROPOSPHERE----

library(readr)
uahncdc_lt_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", 
                             col_types = cols(Year = col_number(), 
                                              Mo = col_number(), Globe = col_number(), 
                                              Land = col_number(), Ocean = col_number(), 
                                              NH = col_number(), Land_1 = col_number(), 
                                              Ocean_1 = col_number(), SH = col_number(), 
                                              Land_2 = col_number(), Ocean_2 = col_number(), 
                                              Trpcs = col_number(), Land_3 = col_number(), 
                                              Ocean_3 = col_number(), NoExt = col_number(), 
                                              Land_4 = col_number(), Ocean_4 = col_number(), 
                                              SoExt = col_number(), Land_5 = col_number(), 
                                              Ocean_5 = col_number(), NoPol = col_number(), 
                                              Land_6 = col_number(), Ocean_6 = col_number(), 
                                              SoPol = col_number(), Land_7 = col_number(), 
                                              Ocean_7 = col_number(), USA48 = col_number(), 
                                              USA49 = col_number(), AUST = col_number()))

lower_troposphere <- uahncdc_lt_6_0 %>%
  filter(Year <= 3000)

lower_troposphere <- lower_troposphere %>% 
  mutate(NoPol1 = NoPol)

lower_troposphere[ , 1:30] <- apply(lower_troposphere[ , 1:30], 2,            
                                    function(x) as.numeric(as.character(x)))


lower_troposphere_3 <- lower_troposphere %>%
  mutate('Atmospheric layer' = 'Lower\n troposphere')


lower_troposphere_3 <- lower_troposphere_3 %>% relocate(`Atmospheric layer`, .before = Year)


##--- MID TROPOSPHERE-----

uahncdc_mt_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")

mid_troposphere <- uahncdc_mt_6_0 %>% 
  filter(Year <= 3000)

mid_troposphere <- mid_troposphere %>% 
  mutate(NoPol2 = NoPol)

mid_troposphere[ , 1:30] <- apply(mid_troposphere[ , 1:30], 2,            
                                  function(x) as.numeric(as.character(x)))

mid_troposphere_3 <- mid_troposphere %>%
  mutate('Atmospheric layer' = 'Mid\n troposphere')

mid_troposphere_3 <- mid_troposphere_3 %>% relocate(`Atmospheric layer`, .before = Year)


##---TROPOPAUSE-----


uahncdc_tp_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")

tropopause <- uahncdc_tp_6_0 %>% 
  filter(Year <= 3000)

tropopause <- tropopause %>% 
  mutate(NoPol3 = NoPol)

tropopause[ , 1:30] <- apply(tropopause[ , 1:30], 2,           
                             function(x) as.numeric(as.character(x)))

tropopause_3 <- tropopause %>%
  mutate('Atmospheric layer' = 'Tropopause')

tropopause_3 <- tropopause_3 %>% relocate(`Atmospheric layer`, .before = Year)


##---LOWER STRATOSPHERE----

uahncdc_ls_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

lower_stratosphere <- uahncdc_ls_6_0 %>% 
  filter(Year <= 3000)

lower_stratosphere <- lower_stratosphere%>% 
  mutate(NoPol4 = NoPol)


lower_stratosphere[ , 1:30] <- apply(lower_stratosphere[ , 1:30], 2,            
                                     function(x) as.numeric(as.character(x)))


lower_stratosphere_3 <- lower_stratosphere %>%
  mutate('Atmospheric layer' = 'Lower\n stratosphere')

lower_stratosphere_3 <- lower_stratosphere_3 %>% relocate(`Atmospheric layer`, .before = Year)


##---ALLE4---------


alle4 <- bind_rows(lower_troposphere, mid_troposphere, tropopause, lower_stratosphere)

alle4 <- alle4 %>% 
  arrange(Year, Mo)


alle4$date <- paste(alle4$Year, alle4$Mo, sep=".")


alle4[ , 1:34] <- apply(alle4[ , 1:34], 2,            
                        function(x) as.numeric(as.character(x)))


##---ALLE4_2----------

alle4_2 <- bind_rows(lower_troposphere_3, mid_troposphere_3, tropopause_3, lower_stratosphere_3)


alle4_2$date <- paste(alle4_2$Year, alle4_2$Mo, sep=".")

alle4_2 <- alle4_2 %>% 
  mutate(RollmeanNoPol = rollmean(NoPol, k = 13, fill = NA))

alle4_2[ , 2:36] <- apply(alle4_2[ , 2:36], 2,            
                          function(x) as.numeric(as.character(x)))

alle4_2 <- alle4_2 %>% 
  arrange(Year, Mo)


##---GJENNOMSNITT----

alle4 %>% 
  ggplot()+
  geom_point(aes(x = date, y = NoPol1), color = 'dark blue', alpha = 0.6)+
  geom_point(aes(x = date, y = NoPol2), color = 'dark green', alpha = 0.6)+
  geom_point(aes(x = date, y = NoPol3), color = 'dark violet', alpha = 0.6)+
  geom_point(aes(x = date, y = NoPol4), color = 'red', alpha = 0.6)+
  geom_line(aes(x = date, y = NoPol4), color = 'red', alpha = 0.2, data = alle4[!is.na(alle4$NoPol4),])+
  geom_line(aes(x = date, y = NoPol2), color = 'dark green', alpha = 0.2, data = alle4[!is.na(alle4$NoPol2),])+
  geom_line(aes(x = date, y = NoPol1), color = 'dark blue', alpha = 0.2, data = alle4[!is.na(alle4$NoPol1),])+
  geom_line(aes(x = date, y = NoPol3), color = 'dark violet', alpha = 0.2, data = alle4[!is.na(alle4$NoPol3),])+
  theme_bw()+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(1))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(2))



##---PLOT GRID------

library(cowplot)

p1 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol1))+
  geom_point(aes(x = date, y = NoPol1), color = 'dark green',  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol1), color = 'dark green', alpha = 0.6, data = alle4[!is.na(alle4$NoPol1),])+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))

p2 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol2))+
  geom_point(aes(x = date, y = NoPol2), color = 'dark blue',  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol2), color = 'dark blue', alpha = 0.6, data = alle4[!is.na(alle4$NoPol2),])+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))

p3 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol3))+
  geom_point(aes(x = date, y = NoPol3), color = 'red',  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol3), color = 'red', alpha = 0.6, data = alle4[!is.na(alle4$NoPol3),])+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))

p4 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol4))+
  geom_point(aes(x = date, y = NoPol4), color = 'dark violet',  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol4), color = 'dark violet', alpha = 0.6, data = alle4[!is.na(alle4$NoPol4),])+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))


#----- mhh

p5 <- alle4_2 %>% 
  ggplot(aes(x = date, y = NoPol))+
  #geom_point()+
  geom_smooth(aes(y = RollmeanNoPol), color = 'brown', span = 0.1)+
  #geom_point(aes(x = date, y = NoPol), color = 'dark violet',  alpha = 0.1)+
  #geom_line(aes(x = date, y = NoPol), color = 'dark violet', alpha = 0.6, data = alle4[!is.na(alle4$NoPol4),])+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))




plot_grid(p1,p2,p3,p4,p5, labels = c('Lower Troposphere', 'Mid-troposohere', 'Tropopause', 'Lower stratosphere', 'Average of all four'))



##----------

alle4_2 %>% 
  ggplot(aes(col = `Atmospheric layer`))+
  geom_point(aes(x = date, y = NoPol),  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol), alpha = 0.6)+
  #geom_line(aes(yintercept=mean(NoPol)), colour="grey50")+
  #geom_smooth(aes(x = date, y = NoPol), color = 'grey50', span = 0.1)+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(4))+
  facet_wrap(~ `Atmospheric layer`)+
  theme(strip.background = element_rect(fill="brown"))+
  labs(subtitle = 'Atmosfæriske temperaturer 1978-2021', x = 'År', col = 'Atmospheric\n layer')+
  theme_bw()