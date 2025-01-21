# MScStats2019-sess-5-Rgraphics

## Answers - Ex1. 
ggplot(mtcars, aes(x = factor(cyl), y = hp)) + 
  geom_boxplot() +
  geom_jitter(aes(colour = factor(carb))) +
  labs(title = "Boxplot by Stella",
       x = "Number of cylinders",
       y = "Gross Horse Power", 
       colour = "Number of carburators") +
  facet_grid(.~gear)
           

## Answers - Ex2. 
ggplot(data=mtcars) + 
  geom_bar(aes(x = cyl, fill = factor(am)),
           stat= "count") + 
  scale_fill_manual(values = c("0" = "blue","1" = "red"),
                    name  ="Transmission",
                    breaks=c("0", "1"),
                    labels=c("Automatic", "Manual")) +
  labs(x = "Number of cylinders", 
       y = "Number of cars")  


