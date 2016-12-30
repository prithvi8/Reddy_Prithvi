#lifeExp-gdpPercap

df<-gapminder%>%select(continent,lifeExp,pop,gdpPercap,year)%>%arrange(gdpPercap)%>%filter(year==2002)

g<-ggplot(df,aes(gdpPercap,lifeExp,size=pop,label="Model",color=continent))

g+geom_point()+facet_grid(.~continent)+geom_smooth(method="lm")  

#reg1

mod<-lm(lifeExp~log(gdpPercap),gapminder)
res<-residuals(mod)
plot(res~predict(mod)) 



summary(mod)                



