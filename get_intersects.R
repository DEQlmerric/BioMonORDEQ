

# Calculate linear regression from Ref_Cal sites ------------------------------------------------------------------
# data source is the ref dataframe frim the EDA graphs script

ref2 <- ref |> 
  filter(model_status == 'Ref_Cal')

#Calculate linear regression
# format is [y axis] ~ [X axis]

fit <- lm(MMI ~ Continuous_BCG_Level, data = joined_OE_BCG_MMI)


#Plot it out to make sure model iis being calculated correctly
plot(joined_OE_BCG_MMI$Continuous_BCG_Level, joined_OE_BCG_MMI$MMI)
abline(fit, col = "red")



#To get linear regression intersect, we need a dataframe of values. For now, I'm only interested in BCG level 4.5
Impaired = data.frame(Continuous_BCG_Level=4.5)
Attain = data.frame(Continuous_BCG_Level=3.5)
#calculate the linear regression intercept
predict(fit, Impaired)



# Calculate quantiles ---------------------------------------------------------------------------------------------
# This section will calculate exact quantile from ALL identified reference sites
#If you want to limit to model build sites,  use ref2 instead of ref

percentile <- ecdf(ref2$MMI)
percentile(predict(fit, newdata))




# plot MMI histogram with linear regression intercept value -------------------------------------------------------


ggplot(data = ref2,aes(x=MMI))+
  geom_histogram()+
  geom_vline(xintercept = predict(fit, Impaired), color = "red")+
  geom_vline(xintercept = predict(fit, Attain), color = "forestgreen")
  


