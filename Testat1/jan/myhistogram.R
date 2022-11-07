myhistogram = function(x, n=30, min=NA, max=NA){
  border_min = min
  border_max = max
  if(is.na(min)) {
    border_min = min(x) 
  }
  if(is.na(max)) {
    border_max = max(x)
  }
  interval = border_max-border_min
  if(is.na(max)){
    border_max = border_max+interval*.1 # der Bereich wird um 10% vergrößert damit der max-wert auch enthalten ist
    if(is.na(min)){
      border_min = border_min-interval*.1 # der Bereich wird um 10% vergrößert damit der es symmetrisch ist
    }
  }
  
  borders = seq(border_min, border_max, length.out=n+1)
  counts = c()
  for (i in 2:length(borders)){
    lower = x[x<borders[i]]
    between = lower[lower>=borders[i-1]]
    counts[i-1] = length(between)
  }
  outside = c(x[x<border_min], x[x>=border_max])
  if(length(outside) > 0) {
    warning('Zahl(en) außerhalb Intervalgrenzen: ', outside)
  }
  list(borders,counts)
}


# 1.3

set.seed(1)
x = rnorm(0,1,n=1000)
h = myhistogram(x,n=20)

mitten = (h[[1]][2:length(h[[1]])] + h[[1]][1:(length(h[[1]])-1)])/2
p = data.frame(x = mitten, y=h[[2]])
ggplot(p, aes(x=x, weight=y)) + geom_bar()