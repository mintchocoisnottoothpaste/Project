# Library

library(gganimate)
library(colorspace)
pal <- choose_palette()
#####
MaleIncDR_age   <-cbind(group=c("65+"),MaleIncDR_age[-seq(1,21),])
MaleIncDR_age_case   <-cbind(group=c("65+"),MaleIncDR_age_case[-seq(1,21),])
MaleIncDR_young <-cbind(group=c("65-"),MaleIncDR_young[-seq(1,21),])
MaleIncDR_young_case <-cbind(group=c("65-"),MaleIncDR_young_case[-seq(1,21),])

MaleInc              <-rbind(MaleIncDR_age,MaleIncDR_young[-seq(1,21),])
MaleInc_case         <-rbind(MaleIncDR_age_case,MaleIncDR_young_case[-seq(1,21),])

data<-cbind(MaleInc,Case=MaleInc_case[,4])


ggplot(data, aes(Year, Case, size = IR, colour = Cancer)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = pal(19)) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~group)



anim <-ggplot(data, aes(Case, IR, size = IR, colour = Cancer)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = pal(19)) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~group) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}')+
  transition_time(Year) +
  ease_aes('linear')


animate(anim , renderer = gifski_renderer())
