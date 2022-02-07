install.packages('jsonlite')

library('jsonlite')
library(tidyverse)
library(ggplot2)
library(scales)

covid_deaths <- fromJSON(readLines('https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json'))

# Oppgave 1

# Her gjør jeg om til prosent ved hjelp av scale_x_continuous og plotter

covid_deaths %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label=name, color=name)) +
  geom_point()+
  geom_text(hjust=-0.3, vjust=0, size=3)+
  labs(title = "Dødsfall per 100k i forhold til antall vaksinerte") +
  xlab("Antall vaksinerte i prosent")+
  ylab("Dødsfall per  100k")+
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent, limits = c(0.45, 0.8),
                     breaks = c(0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8))


# Oppgave 2

lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = covid_deaths)

covid_deaths %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label=name)) +
  geom_point()+
  geom_text(hjust=-0.2, vjust=0, size=2.4)+
  theme_bw() +
  geom_smooth(method = lm, color='orange')+
  labs(title = "Dødsfall per 100k i forhold til antall vaksinerte") +
  xlab("Antall vaksinerte i prosent")+
  ylab("Dødsfall per  100k")+
  scale_x_continuous(labels = scales::percent, limits = c(0.45, 0.8),
                     breaks = c(0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8))

# Som vi kan se på intercept, som er 
# der linja krysser på Y aksen, er det forventet at det vil være
# 31,15 dødsfall når 0% av befolkningen er vaksinert.

# Vi ser en klar sammenheng mellom antall vaksinerte, og antall dødsfall. 
# I stater med større prosent av befolkningen vaksinert, er det også ferre dødsfall.

# Vi ser også at denne har et negativt stigningstall, som vi også får oppgitt 
# med lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = covid_deaths),
# der vi får ut -36,66.
