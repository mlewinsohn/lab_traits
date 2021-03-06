# Work on lab traits data
library(rethinking)
require(reshape2)
require(ggplot2)

# read in dataframe
df = read.csv("~/Documents/stats-club/lab_traits_responses_cleaned_binary.csv", header=TRUE)

# beer hour attendee and coffee snob are also somewhat correlated

# lifts weights vs. sourdough starter and cares about plants
select <- c("Name","worried_about_plants","sourdough_starter","lifts_weights","beard")
x <- df[,select]

select <- c("Name","uses_pipette","beard")
x <- df[,select]

select <- c("Name","favorite_language_python","obligate_lunch_buyer")
x <- df[,select]

select <- c("Name","likes_snowpiercer","enjoys_baking")
x <- df[,select]

# let's start with a simple regression model, where we want to use just an intercept to predict whether people love sloths or not
# we first fit the simplest model, one with intercept only
plants_intercept <- map(
  alist(
    worried_about_plants ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ), data=df
)
precis(plants_intercept)

# now fit a model with just liking plants as a function of weights, sourdough starter, and beard
plants_weights_starter_beard <- map(
  alist(
    worried_about_plants ~ dbinom(1, p),
    logit(p) <- a + bweights*lifts_weights + bsourdough*sourdough_starter + bbeard*beard,
    a ~ dnorm(0, 10),
    bweights ~ dnorm(0,10),
    bsourdough ~ dnorm(0,10),
    bbeard ~ dnorm(0,10)
  ), data=df
)
precis(m2)

# output the overall probability of caring about plants in the lab; 0.62, with a 95% intereval of 0.205 to 0.913
logistic(0.51) 
logistic(c(-1.35,2.36))

# let's now exponentiate these predictors to get odds ratios; these are relative effects
exp(c(7.43,0.41,14.45))
exp(c(-4.28,-8.95,0.39))

# liking plants as a function of weights and beard
plants_weights_beard <- map(
  alist(
    worried_about_plants ~ dbinom(1, p),
    logit(p) <- a + bweights*lifts_weights + bbeard*beard,
    a ~ dnorm(0, 10),
    bweights ~ dnorm(0,10),
    bbeard ~ dnorm(0,10)
  ), data=df
)
precis(plants_weights_beard)
compare(plants_intercept,plants_weights_starter_beard,plants_weights_beard)
plot(compare(plants_intercept,plants_weights_starter_beard,plants_weights_beard))
plot(precis(plants_weights_starter_beard))

# now do some implied predictions
# dummy data 
d.pred <- data.frame(
  lifts_weights = c(0,1,0,0,1,1,0,1),  # no, yes, yes, yes, yes, no
  beard = c(0,0,0,1,0,1,1,1),   # no, yes, no, yes, no, yes
  sourdough_starter = c(0,0,1,0,1,0,1,1) # yes, no, no, yes, yes, no
)

plants.ensemble <- ensemble(plants_weights_beard, plants_weights_starter_beard,data=d.pred)

# summarize
pred.p <- apply(plants.ensemble$link, 2, mean)
pred.p.PI <- apply(plants.ensemble$link, 2, PI)

# plot 
# empty plot frame with good axes
plot(0,0,type="n",xlab="lifts weights/has beard/has sourdough starter", ylab="P(care about plants)", ylim=c(0,1), xaxt="n",xlim=c(1,8))
axis(1, at=1:8, labels=c("-w/-b/-s","+w","+s","+b","+w/+s","+w/+b","+b/+s","+w/+b/+s"))

# plot raw data
p <- by(df$worried_about_plants, 
        list(df$lifts_weights, df$has_beard, df$sourdough_starter,df$index), mean)
for (index in 1:16)
  lines(1:16, as.vector(p[,,index]), col=rangi2, lwd=1.5)

# now superimpose posterior predictions
lines(1:8, pred.p)
shade(pred.p.PI, 1:8)


##################################################################################
### SNOWPIERCER ##################################################################

# now fit the model for snowpiercer
m4 <- map(
  alist(
    likes_snowpiercer ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ), data=df
)
precis(m4)

m5 <- map(
  alist(
    likes_snowpiercer ~ dbinom(1, p),
    logit(p) <- a + bbaking*enjoys_baking,
    a ~ dnorm(0, 10),
    bbaking ~ dnorm(0,10)
  ), data=df
)
precis(m5)
plot(precis(m5))

compare(m4,m5)

# output the overall probability of liking snowpiercer; 0.59 (0.25, 0.86)
logistic(0.38) 
logistic(c(-1.06,1.82))

# let's now exponentiate these predictors to get odds ratios; these are relative effects
exp(c(-2.65,0.38-2.65,-4.83,-0.47))




# attends coffe run
m6 <- map(
  alist(
    attends_coffee_run ~ dbinom(1, p),
    logit(p) <- a + 
      bnever_saw_snowpiercer*never_saw_snowpiercer+ 
      bdislikes_snowpiercer*dislikes_snowpiercer+ 
      blikes_snowpiercer*likes_snowpiercer+ 
      bfavorites_ipa*favorites_ipa+ 
      bfavorites_ciders*favorites_ciders+ 
      bhates_ciders_the_most*hates_ciders_the_most+ 
      bhates_ipa_the_most*hates_ipa_the_most+ 
      brunner*runner+ 
      bobligate_lunch_buyer*obligate_lunch_buyer+ 
      bobligate_lunch_packer*obligate_lunch_packer+ 
      bdislikes_california*dislikes_california+ 
      blikes_california*likes_california+ 
      bsourdough_starter*sourdough_starter+ 
      bcoffee_snob*coffee_snob+ 
      buses_pipette*uses_pipette+ 
      bfavorite_language_python*favorite_language_python+ 
      blifts_weights*lifts_weights+ 
      bbeer_hour_attendee*beer_hour_attendee+ 
      bdislikes_allsorts*dislikes_allsorts+ 
      blikes_allsorts*likes_allsorts+ 
      benjoys_baking*enjoys_baking+ 
      bwears_hat*wears_hat+ 
      bmisses_arnold_office*misses_arnold_office+ 
      bnon_natural_hair_color*non_natural_hair_color+ 
      bbeard*beard+ 
      brock_climbs*rock_climbs+ 
      bworried_about_plants*worried_about_plants+ 
      bthinks_sun_is_evil*thinks_sun_is_evil+ 
      bhardcore_outside_lunch*hardcore_outside_lunch+ 
      bloved_sloths*loved_sloths+ 
      blives_outside_us*lives_outside_us,
    
    # add in priors
    a ~ dnorm(0,10),
    bnever_saw_snowpiercer ~ dnorm(0,1),
    bdislikes_snowpiercer ~ dnorm(0,1),
    blikes_snowpiercer ~ dnorm(0,1),
    bfavorites_ipa ~ dnorm(0,1),
    bfavorites_ciders ~ dnorm(0,1),
    bhates_ciders_the_most ~ dnorm(0,1),
    bhates_ipa_the_most ~ dnorm(0,1),
    brunner ~ dnorm(0,1),
    battends_coffee_run ~ dnorm(0,1),
    bobligate_lunch_buyer ~ dnorm(0,1),
    bobligate_lunch_packer ~ dnorm(0,1),
    bdislikes_california ~ dnorm(0,1),
    blikes_california ~ dnorm(0,1),
    bsourdough_starter ~ dnorm(0,1),
    bcoffee_snob ~ dnorm(0,1),
    buses_pipette ~ dnorm(0,1),
    bfavorite_language_python ~ dnorm(0,1),
    blifts_weights ~ dnorm(0,1),
    bbeer_hour_attendee ~ dnorm(0,1),
    bdislikes_allsorts ~ dnorm(0,1),
    blikes_allsorts ~ dnorm(0,1),
    benjoys_baking ~ dnorm(0,1),
    bwears_hat ~ dnorm(0,1),
    bmisses_arnold_office ~ dnorm(0,1),
    bnon_natural_hair_color ~ dnorm(0,1),
    bbeard ~ dnorm(0,1),
    brock_climbs ~ dnorm(0,1),
    bworried_about_plants ~ dnorm(0,1),
    bthinks_sun_is_evil ~ dnorm(0,1),
    bhardcore_outside_lunch ~ dnorm(0,1),
    bloved_sloths ~ dnorm(0,1),
    blives_outside_us ~ dnorm(0,1)
  ), data=df
)
precis(m6)
plot(precis(m6))


m7 <- map(
  alist(
    likes_snowpiercer ~ dbinom(1, p),
    logit(p) <- a + 
      battends_coffee_run+
      bfavorites_ipa*favorites_ipa+ 
      bfavorites_ciders*favorites_ciders+ 
      bhates_ciders_the_most*hates_ciders_the_most+ 
      bhates_ipa_the_most*hates_ipa_the_most+ 
      brunner*runner+ 
      bobligate_lunch_buyer*obligate_lunch_buyer+ 
      bobligate_lunch_packer*obligate_lunch_packer+ 
      bdislikes_california*dislikes_california+ 
      blikes_california*likes_california+ 
      bsourdough_starter*sourdough_starter+ 
      bcoffee_snob*coffee_snob+ 
      buses_pipette*uses_pipette+ 
      bfavorite_language_python*favorite_language_python+ 
      blifts_weights*lifts_weights+ 
      bbeer_hour_attendee*beer_hour_attendee+ 
      bdislikes_allsorts*dislikes_allsorts+ 
      blikes_allsorts*likes_allsorts+ 
      benjoys_baking*enjoys_baking+ 
      bwears_hat*wears_hat+ 
      bmisses_arnold_office*misses_arnold_office+ 
      bnon_natural_hair_color*non_natural_hair_color+ 
      bbeard*beard+ 
      brock_climbs*rock_climbs+ 
      bworried_about_plants*worried_about_plants+ 
      bthinks_sun_is_evil*thinks_sun_is_evil+ 
      bhardcore_outside_lunch*hardcore_outside_lunch+ 
      bloved_sloths*loved_sloths+ 
      blives_outside_us*lives_outside_us,
    
    # add in priors
    a ~ dnorm(0,10),
    bfavorites_ipa ~ dnorm(0,1),
    bfavorites_ciders ~ dnorm(0,1),
    bhates_ciders_the_most ~ dnorm(0,1),
    bhates_ipa_the_most ~ dnorm(0,1),
    brunner ~ dnorm(0,1),
    battends_coffee_run ~ dnorm(0,1),
    bobligate_lunch_buyer ~ dnorm(0,1),
    bobligate_lunch_packer ~ dnorm(0,1),
    bdislikes_california ~ dnorm(0,1),
    blikes_california ~ dnorm(0,1),
    bsourdough_starter ~ dnorm(0,1),
    bcoffee_snob ~ dnorm(0,1),
    buses_pipette ~ dnorm(0,1),
    bfavorite_language_python ~ dnorm(0,1),
    blifts_weights ~ dnorm(0,1),
    bbeer_hour_attendee ~ dnorm(0,1),
    bdislikes_allsorts ~ dnorm(0,1),
    blikes_allsorts ~ dnorm(0,1),
    benjoys_baking ~ dnorm(0,1),
    bwears_hat ~ dnorm(0,1),
    bmisses_arnold_office ~ dnorm(0,1),
    bnon_natural_hair_color ~ dnorm(0,1),
    bbeard ~ dnorm(0,1),
    brock_climbs ~ dnorm(0,1),
    bworried_about_plants ~ dnorm(0,1),
    bthinks_sun_is_evil ~ dnorm(0,1),
    bhardcore_outside_lunch ~ dnorm(0,1),
    bloved_sloths ~ dnorm(0,1),
    blives_outside_us ~ dnorm(0,1)
  ), data=df
)
precis(m7)
plot(precis(m7))



p3.lifts_weights <- logistic(post_m3$a + post_m3$bweights)
p3.no_lifts_weights <- logistic(post_m3$a)

p3.beard <- logistic(post_m3$a + post_m3$bbeard)
p3.no_beard <- logistic(post_m3$a)   

# let's plot this
require(reshape2)
df_plants_m2 <- melt(data.frame(p3.lifts_weights,p3.no_lifts_weights))
df_plants_m2$model = "3 predictors"
df_plants_m3 <- melt(data.frame(p3.beard,p3.no_beard))
df_plants_m3$model = "2 predictors"
df_both = rbind(df_plants_m2,df_plants_m3)

# this is pretty interesting. So basically, if you lift weights, you are almost 100% likely to care about the office plants; if you have a beard, you are very unlikely to care about the office plants
ggplot(df_both)+ geom_density(aes(x=value, color=variable))+scale_color_manual(values=c("red","blue","green","black"))

# now that we have a distribution, we can output a HPDI
HPDI(p3.lifts_weights, prob=0.95)
HPDI(p3.no_lifts_weights, prob=0.95)
HPDI(p3.beard, prob=0.95)
HPDI(p3.no_beard, prob=0.95)

# calculate the difference between Marshallese and not Marshallese, and summarize the quantiles. This result suggests that Marshallese tips are 29% more likely to have descendants than non-Marshallese
diff.beard <- p3.no_beard - p3.beard
quantile(diff.beard, c(0.025,0.5,0.975))
diff.weights <- p3.lifts_weights - p3.no_lifts_weights
quantile(diff.weights, c(0.025,0.5,0.975))
