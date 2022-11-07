library(ggplot2)
library(dplyr)
library(knitr)

mam = MASS::mammals
mam$name = row.names(mam)

lbl = c('Pig', 'Rat', 'African elephant', 'Chimpanzee', 'Cat', 'Human', 'Little brown bat')
lbl_data = mam[mam$name %in% lbl,]

ggplot(mam) +
  geom_point(aes(x=body, y=brain)) +
  scale_y_continuous('Gehirngewicht (g) ', trans='log10') +
  scale_x_continuous('Körpergewicht (kg)', trans='log10') +
  geom_text(aes(label=name, x=body, y=brain),color='blue', data=lbl_data)





mam$r = mam$brain/mam$body
head(mam[order(mam$r, decreasing=T),],n=10)
tail(mam[order(mam$r, decreasing=T),],n=10)



ggplot(MASS::Rabbit) + geom_line(aes(y=BPchange, x=Dose)) + facet_grid(Animal ~ Treatment)

ggplot(MASS::Rabbit) + geom_histogram(aes(x=BPchange)) + facet_grid(Dose ~ Treatment)





vacc = read.table('https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv', sep='\t', header=T)

deliv = read.table('https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv', sep='\t', header=T)

vaccState = read.table('https://impfdashboard.de/static/data/germany_vaccinations_by_state.tsv', sep='\t', header=T)

deliv$date = as.Date(deliv$date)
vacc$date = as.Date(vacc$date)


head(vacc[c(1,2,3)], n=3)
head(deliv[c(1,2,3)], n=3)
head(vaccState[c(1,2,3)], n=3)


data_set_size = length(vacc$date)

astra = (c(vacc$dosen_astra_kumulativ,0)-c(0,vacc$dosen_astra_kumulativ))[1:data_set_size]

comirnaty = (c(vacc$dosen_biontech_kumulativ,0)-c(0,vacc$dosen_biontech_kumulativ))[1:data_set_size]

johnson = (c(vacc$dosen_johnson_kumulativ,0)-c(0,vacc$dosen_johnson_kumulativ))[1:data_set_size]

moderna = (c(vacc$dosen_moderna_kumulativ,0)-c(0,vacc$dosen_moderna_kumulativ))[1:data_set_size]
novavax = (c(vacc$dosen_novavax_kumulativ,0)-c(0,vacc$dosen_novavax_kumulativ))[1:data_set_size]
valneva = (c(vacc$dosen_valneva_kumulativ,0)-c(0,vacc$dosen_valneva_kumulativ))[1:data_set_size]


vacc2 = data.frame(Datum=as.Date(vacc$date, "%d.%m.%Y"), Hersteller=rep(c('astra', 'comirnaty', 'johnson', 'moderna','novavax','valneva'),each=data_set_size), "Impfdosen am Tag"=c(astra,comirnaty, johnson, moderna, novavax, valneva))


vacc2 = vacc2[order(vacc2$Datum),]

ggplot(vacc2) + geom_line(aes(x=Datum, y=Impfdosen.am.Tag, group=Hersteller, color=Hersteller))


deliv2 = data.frame(deliv |> group_by(impfstoff, date) |> summarise(dosen = sum(dosen)))

ggplot(vacc2) + geom_line(aes(x=Datum, y=ave(Impfdosen.am.Tag, Hersteller, FUN=cumsum), group=Hersteller, color=Hersteller))

ggplot(deliv2) + geom_line(aes(x=date, y=ave(dosen, impfstoff, FUN=cumsum), group=impfstoff, color=impfstoff))

ggplot() +
  geom_line(aes(x=as.Date(Datum, '%d.%m.%Y'), y=ave(Impfdosen.am.Tag, Hersteller, FUN=cumsum), group=Hersteller, color=Hersteller), data=vacc2)+
  geom_line(aes(x=as.Date(date), y=ave(dosen, impfstoff, FUN=cumsum), group=impfstoff, color=impfstoff), data=deliv2)