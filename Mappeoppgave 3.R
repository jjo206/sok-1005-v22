## - Jobbet sammen med Erlend Kristensen Olsen, Morten Ivarrud og Tobias Westvik Pedersen


library(tidyverse)
library(rvest)

Vintertest <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

Vtest <- html_table(html_nodes(Vintertest, "table")[[1]], header = TRUE)

#-- Oppgave 1

#-- filtrerer tabell 

Vtest <- Vtest %>% 
  rename(WLTP = `WLTP-tall`) 


Vtest <- Vtest %>% 
  mutate(Avvik = str_remove_all(Avvik, "%"),
         STOPP = str_remove_all(STOPP, "km"),
         WLTP  = substr(WLTP, 1, 3)) %>%
  rename(stopp = STOPP,
         model = `Modell (temp. varierte fra 0° til -10°)`,
         avvik = Avvik) %>% 
  subset(!stopp == "x")


Vtest$WLTP <- as.numeric(Vtest$WLTP)

Vtest$stopp <- as.numeric(Vtest$stopp)


##-- Plotter

plot <- Vtest %>% 
  ggplot(aes(x= WLTP, y=stopp)) +
  geom_point(size=1.7, alpha= 0.9, color="dark red") +
  scale_x_continuous(breaks = seq(200, 600, 100), limits=c(200, 600)) + 
  scale_y_continuous(breaks = seq(200, 600, 100), limits=c(200, 600)) +
  theme_bw() +
  geom_abline(intercept = 0, slope= 1, size = 0.8, color="black") +
  labs(title= "temp. varierte fra 0° til -10°") +
  ylab("Stopp") +
  xlab("Wltp") 
  
plot



#-- Oppgave 2

plot + geom_smooth(method=lm, se = TRUE,
                   size = 0.3,
                   alpha = 0.4,
                   aes(color="blue")) + scale_color_identity(name = "Regresjonslinje",
                                                              breaks = c("blue"),
                                                              labels = c("Blå Linje"),
                                                              guide = "legend")

#-- Finner 'stigningstallet'

lm(stopp ~ WLTP, data = Vtest)



## Som vi kan se utifra tallene og plotten vi har fått ut, ser vi at 
## kjørelengden på de elektriske bilene ikke holder opp til det leverandøren
## skriver at bilen kan yte. 
## Med de tallene vi har ser vi at sammenlagt har de elektriske bilene 
## som er med i testen en 13,29% lavere kjørelengde enn det leverandøren
## sier bilen kan kjøre.


## - Kilder:

## - https://ggplot2.tidyverse.org/reference/scale_continuous.html
## - https://ggplot2.tidyverse.org/reference/geom_abline.html
## - https://statisticsglobe.com/convert-character-to-numeric-in-r/
## - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/subset
