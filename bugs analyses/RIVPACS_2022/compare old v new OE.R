

# compare old O/E vs new (RIV24) O/E

sumbugs <- read.csv("//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/SUM_BUGS_2023-08-07.csv")
oe_old <- sumbugs %>% 
  select(Sample, MLocID, StationDes, Date, Habitat, Activity_Type, OoverE, Eco2, Eco3) %>%
  rename(OE_old = OoverE)

new.oe <- read.csv("//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/RIV24_OEscores_2023-12-14.csv")
oe_new <- new.oe %>% 
  select(Sample, OoverE) %>%
  rename(OE_new = OoverE)


OE_all <- oe_new %>%
  left_join(oe_old, by="Sample") %>%
  select(Sample, MLocID, StationDes, Date, Habitat, Activity_Type, Eco2, Eco3, OE_old, OE_new)


# compare old and new O/E values
plot(OE_all$OE_old, OE_all$OE_new, ylim=c(0,1.4))
abline(a=0, b=1, col='red')

ggplot(OE_all, aes(x = OE_old, y = OE_new, color = Eco2)) +
  geom_point() + geom_abline(intercept = 0, slope = 1)


----- need to get all OE into one column as a result and old/new as a parameter

pivot_longer(!OE_all, names_to = "income", values_to = "count")

OE.long <- OE_all %>%
  pivot_longer(
    cols = starts_with("OE"),
    names_to = "model",
    #names_prefix = "OE",
    values_to = "OE",
    values_drop_na = TRUE )


p1 <- ggplot(OE.long, aes(x=model, y=OE, fill=Eco2)) + 
  geom_boxplot() +
  facet_wrap(~Eco3)


@@@@@@ = bring in ref sites from new model and see how they compare to old model


one.table <- read.csv(('Reference/one.table_rule.all.csv'))
one.table <- one.table %>% select (MLocID, Ref2020_FINAL, owner)



OE_all_one.table <- OE_all %>%
                      left_join(one.table, by = 'MLocID')

ggplot(OE_all_one.table, aes(x = OE_old, y = OE_new, color = Ref2020_FINAL)) +
  geom_point() + geom_abline(intercept = 0, slope = 1)+
  facet_wrap(~Ref2020_FINAL)





OE_ref_long <- OE_all_one.table %>%
  pivot_longer(
    cols = starts_with("OE"),
    names_to = "model",
    #names_prefix = "OE",
    values_to = "OE",
    values_drop_na = TRUE )

ggplot(OE_ref_long, aes(x=model, y=OE, fill=Ref2020_FINAL)) + 
  geom_boxplot() +
  facet_wrap(~Habitat)


