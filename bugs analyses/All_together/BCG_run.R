library(BCGcalc)
library(readxl)



run_BCG <- function(df_metric){

  df_metric <- BCG_metrics
# 1.B. Metric Membership

df.rules <- read_excel(system.file("./extdata/Rules.xlsx"
                                   , package="BCGcalc")
                       , sheet="Rules") 
# Run function
df.Metric.Membership <- BCG.Metric.Membership(df_metric, df.rules)
# Show Results
# View(df.Metric.Membership)
# # Save Results
# write.table(df.Metric.Membership, "Metric.Membership.Test.tsv"
#             , row.names=FALSE, col.names=TRUE, sep="\t")

# 1.C. Level Assignment
# Run Function
df.Level.Membership <- BCG.Level.Membership(df.Metric.Membership, df.rules)

# 1.D. Level Membership
# Run Function
df.Levels <- BCG.Level.Assignment(df.Level.Membership)

# 1.E. Flags
# Import QC Checks
df.checks <- read_excel(system.file("./extdata/MetricFlags.xlsx"
                                    , package="BCGcalc")
                        , sheet="Flags") 
# Run Function


# Run Function
df.flags <- qc.checks(df_metric, df.checks)
# Change terminology; PASS/FAIL to NA/flag
df.flags[,"FLAG"][df.flags[,"FLAG"]=="FAIL"] <- "flag"
df.flags[, "FLAG"][df.flags[,"FLAG"]=="PASS"] <- NA
# long to wide format
df.flags.wide <- reshape2::dcast(df.flags, SAMPLEID ~ CHECKNAME, value.var="FLAG")
# Calc number of "flag"s by row.
df.flags.wide$NumFlags <- rowSums(df.flags.wide=="flag", na.rm=TRUE)
# Rearrange columns
NumCols <- ncol(df.flags.wide)
df.flags.wide <- df.flags.wide[, c(1, NumCols, 2:(NumCols-1))]
# Merge Levels and Flags
df.Levels.Flags <- merge(df.Levels, df.flags.wide,by.x="SampleID", by.y="SAMPLEID", all.x=TRUE)

BCG <- list(Metric.Membership = df.Metric.Membership,
            Level.Membership = df.Level.Membership,
            Levels  = df.Levels,
            Levels.Flags = df.Levels.Flags)

return(BCG)

}