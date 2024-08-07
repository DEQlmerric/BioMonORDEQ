BCG.Level.Membership_fix <- function (df.metric.membership, df.rules, col_SAMPLEID = "SAMPLEID", 
          col_INDEX_NAME = "INDEX_NAME", col_INDEX_CLASS = "INDEX_CLASS", 
          col_LEVEL = "LEVEL", col_METRIC_NAME = "METRIC_NAME", col_RULE_TYPE = "RULE_TYPE", 
          col_EXC_RULE = "EXC_RULE", col_MEMBERSHIP = "MEMBERSHIP", 
          ...) 
{
  
  #testing
  # df.metric.membership = df.Metric.Membership
  # df.rules = df.rules
  # col_SAMPLEID = "SAMPLEID"
  # col_INDEX_NAME = "INDEX_NAME"
  # col_INDEX_CLASS = "INDEX_CLASS"
  # col_LEVEL = "LEVEL"
  # col_METRIC_NAME = "METRIC_NAME"
  # col_RULE_TYPE = "RULE_TYPE"
  # col_EXC_RULE = "EXC_RULE"
  # col_MEMBERSHIP = "MEMBERSHIP"

  boo_QC <- FALSE
  if (isTRUE(boo_QC)) {
    df.metric.membership <- df_met_memb
    df.rules <- df_rules
    col_SAMPLEID <- "SAMPLEID"
    col_INDEX_NAME <- "INDEX_NAME"
    col_INDEX_CLASS <- "INDEX_CLASS"
    col_LEVEL <- "LEVEL"
    col_METRIC_NAME <- "METRIC_NAME"
    col_RULE_TYPE <- "RULE_TYPE"
    col_EXC_RULE <- "EXC_RULE"
    col_MEMBERSHIP <- "MEMBERSHIP"
    a <- c(col_INDEX_NAME, col_INDEX_CLASS, col_LEVEL, col_METRIC_NAME, 
           col_RULE_TYPE, col_EXC_RULE)
  }
  `%>%` <- dplyr::`%>%`
  if (exists("col_SITE_TYPE")) {
    col_INDEX_CLASS <- col_SITE_TYPE
    msg <- "The parameter 'col_SITE_TYPE' was deprecated in v2.0.0.9001. \n\n    Use 'col_INDEX_CLASS' instead."
    message(msg)
  }
  df.metric.membership <- as.data.frame(df.metric.membership)
  df.rules <- as.data.frame(df.rules)
  names(df.metric.membership) <- toupper(names(df.metric.membership))
  names(df.rules) <- toupper(names(df.rules))
  col_INDEX_CLASS_ORIG <- paste0(col_INDEX_CLASS, "_ORIG")
  df.metric.membership[, col_INDEX_CLASS_ORIG] <- df.metric.membership[, 
                                                                       col_INDEX_CLASS]
  df.metric.membership[, col_INDEX_CLASS] <- tolower(df.metric.membership[, 
                                                                          col_INDEX_CLASS])
  df.rules[, col_INDEX_CLASS] <- tolower(df.rules[, col_INDEX_CLASS])
  df.metric.membership[, col_EXC_RULE] <- toupper(df.metric.membership[, 
                                                                       col_EXC_RULE])
  df.rules[, col_EXC_RULE] <- toupper(df.rules[, col_EXC_RULE])
  df.metric.membership[, col_RULE_TYPE] <- toupper(df.metric.membership[, 
                                                                        col_RULE_TYPE])
  df.rules[, col_RULE_TYPE] <- toupper(df.rules[, col_RULE_TYPE])
  col.drop <- c("NUMERIC_RULES", "SYMBOL", "LOWER", "UPPER", 
                "INCREASE", "DESCRIPTION")
  col.keep <- names(df.metric.membership)[!(names(df.metric.membership) %in% 
                                              col.drop)]
  
  ## This merge doresn't work correctly
  df.merge <- merge(df.metric.membership[, col.keep], df.rules, 
                    by.x = c(col_INDEX_NAME, col_INDEX_CLASS, col_LEVEL, 
                             col_METRIC_NAME, col_RULE_TYPE, col_EXC_RULE), by.y = c(col_INDEX_NAME, 
                                                                                     col_INDEX_CLASS, col_LEVEL, col_METRIC_NAME, col_RULE_TYPE, 
                                                                                     col_EXC_RULE))
  nrow_metmemb <- nrow(df.metric.membership)
  nrow_merge <- nrow(df.merge)
  if (nrow_metmemb != nrow_merge) {
    msg <- paste("Rules and Metric Membership not matching.", 
                 paste0(nrow_metmemb, " = rows Metric Membership"), 
                 paste0(nrow_merge, " = rows after merge with Rules"), 
                 sep = "\n")
    message(msg)
  }
  if (nrow(df.merge) == 0) {
    msg <- "Merging of Metric Membership and Rules data frames failed.\n    Check columns col_INDEX_NAME, col_INDEX_CLASS, col_LEVEL, col_METRIC_NAME, col_RULE_TYPE, and col_EXC_RULE."
    stop(msg)
  }
  names(df.merge)[names(df.merge) == col_SAMPLEID] <- "SAMPLEID"
  names(df.merge)[names(df.merge) == col_INDEX_NAME] <- "INDEX_NAME"
  names(df.merge)[names(df.merge) == col_INDEX_CLASS] <- "INDEX_CLASS"
  names(df.merge)[names(df.merge) == col_LEVEL] <- "LEVEL"
  names(df.merge)[names(df.merge) == col_RULE_TYPE] <- "RULE_TYPE"
  names(df.merge)[names(df.merge) == col_MEMBERSHIP] <- "MEMBERSHIP"
  names(df.merge)[names(df.merge) == col_EXC_RULE] <- "EXC_RULE"
  df_er_median <- dplyr::filter(df.merge, EXC_RULE == "MEDIAN")
  
  
  #This does not return metric name or many other fields needed when rejoined
  df_er_median_calc <- dplyr::summarise(dplyr::group_by(df_er_median, 
                                                        SAMPLEID, INDEX_NAME, INDEX_CLASS,INDEX_CLASS_ORIG, LEVEL, RULE_TYPE), 
                                        .groups = "drop_last", MEMBERSHIP = median(MEMBERSHIP, 
                                                                                   na.rm = TRUE))
  df.merge <- dplyr::filter(df.merge, EXC_RULE != "MEDIAN" | 
                              is.na(EXC_RULE))
  #####
  df.merge <- dplyr::bind_rows(df.merge, df_er_median_calc)
  #######
  df_er_small2 <- dplyr::filter(df.merge, EXC_RULE == "SMALL2")
  df_er_small2_calc <- dplyr::group_by(df_er_small2, SAMPLEID, 
                                       INDEX_NAME, INDEX_CLASS, LEVEL) %>% dplyr::arrange(MEMBERSHIP) %>% 
    dplyr::filter(dplyr::row_number() == 2)
  df.merge <- dplyr::filter(df.merge, EXC_RULE != "SMALL2" | 
                              is.na(EXC_RULE))
  df.merge <- dplyr::bind_rows(df.merge, df_er_small2_calc)
  df_er_small2a <- dplyr::filter(df.merge, EXC_RULE == "SMALL2A")
  df_er_small2a_calc <- dplyr::group_by(df_er_small2a, SAMPLEID, 
                                        INDEX_NAME, INDEX_CLASS, LEVEL) %>% dplyr::arrange(MEMBERSHIP) %>% 
    dplyr::filter(dplyr::row_number() == 2)
  df.merge <- dplyr::filter(df.merge, EXC_RULE != "SMALL2A" | 
                              is.na(EXC_RULE))
  df.merge <- dplyr::bind_rows(df.merge, df_er_small2a_calc)
  df_er_small2b <- dplyr::filter(df.merge, EXC_RULE == "SMALL2B")
  df_er_small2b_calc <- dplyr::group_by(df_er_small2b, SAMPLEID, 
                                        INDEX_NAME, INDEX_CLASS, LEVEL) %>% dplyr::arrange(MEMBERSHIP) %>% 
    dplyr::filter(dplyr::row_number() == 2)
  df.merge <- dplyr::filter(df.merge, EXC_RULE != "SMALL2B" | 
                              is.na(EXC_RULE))
  df.merge <- dplyr::bind_rows(df.merge, df_er_small2b_calc)
  df_er_small3_rule0 <- dplyr::filter(df.merge, EXC_RULE == 
                                        "SMALL3" & RULE_TYPE == "RULE0")
  df_er_small3_rule1 <- dplyr::filter(df.merge, EXC_RULE == 
                                        "SMALL3" & RULE_TYPE == "RULE1")
  df_er_small3_rule1_calc <- dplyr::group_by(df_er_small3_rule1, 
                                             SAMPLEID, INDEX_NAME, INDEX_CLASS, LEVEL) %>% dplyr::arrange(MEMBERSHIP) %>% 
    dplyr::filter(dplyr::row_number() == 3) %>% dplyr::mutate(RULE_TYPE = "RULE0")
  df_er_small3_rule0_calc <- dplyr::bind_rows(df_er_small3_rule0, 
                                              df_er_small3_rule1_calc)
  df_er_small3_calc <- dplyr::group_by(df_er_small3_rule0_calc, 
                                       SAMPLEID, INDEX_NAME, INDEX_CLASS, LEVEL) %>% dplyr::arrange(MEMBERSHIP) %>% 
    dplyr::filter(dplyr::row_number() == 3)
  df.merge <- dplyr::filter(df.merge, EXC_RULE != "SMALL3" | 
                              is.na(EXC_RULE))
  df.merge <- dplyr::bind_rows(df.merge, df_er_small3_calc)
  suppressWarnings(df.lev <- dplyr::summarise(dplyr::group_by(df.merge, 
                                                              SAMPLEID, INDEX_NAME, INDEX_CLASS, INDEX_CLASS_ORIG, 
                                                              LEVEL), .groups = "drop_last", MembCalc_Rule2_min = min(MEMBERSHIP[RULE_TYPE == 
                                                                                                                                   "RULE2"], na.rm = TRUE), MembCalc_Rule1_max = max(MEMBERSHIP[RULE_TYPE == 
                                                                                                                                                                                                  "RULE1"], na.rm = TRUE), MembCalc_Rule0_min = min(MEMBERSHIP[RULE_TYPE == 
                                                                                                                                                                                                                                                                 "RULE0"], na.rm = TRUE), MembCalc_Exc0_min = min(MEMBERSHIP[EXC_RULE == 
                                                                                                                                                                                                                                                                                                                               "EXCMEM0"], na.rm = TRUE), MembCalc_Exc1_max = max(MEMBERSHIP[EXC_RULE == 
                                                                                                                                                                                                                                                                                                                                                                                               "EXCMEM1"], na.rm = TRUE), MembCalc_Exc2_min = min(MEMBERSHIP[EXC_RULE == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                               "EXCMEM2"], na.rm = TRUE)))
  names(df.lev)[names(df.lev) == "SAMPLEID"] <- toupper(col_SAMPLEID)
  names(df.lev)[names(df.lev) == "INDEX_NAME"] <- toupper(col_INDEX_NAME)
  names(df.lev)[names(df.lev) == "INDEX_CLASS"] <- toupper(col_INDEX_CLASS)
  names(df.lev)[names(df.lev) == "LEVEL"] <- toupper(col_LEVEL)
  names(df.lev)[names(df.lev) == "RULE_TYPE"] <- toupper(col_RULE_TYPE)
  names(df.lev)[names(df.lev) == "EXC_RULE"] <- toupper(col_EXC_RULE)
  df.lev <- as.data.frame(df.lev)
  df.lev[!is.finite(df.lev[, "MembCalc_Rule2_min"]), "MembCalc_Rule2_min"] <- NA
  df.lev[!is.finite(df.lev[, "MembCalc_Rule1_max"]), "MembCalc_Rule1_max"] <- NA
  df.lev[!is.finite(df.lev[, "MembCalc_Rule0_min"]), "MembCalc_Rule0_min"] <- 0
  df.lev[!is.finite(df.lev[, "MembCalc_Exc0_min"]), "MembCalc_Exc0_min"] <- NA
  df.lev[!is.finite(df.lev[, "MembCalc_Exc1_max"]), "MembCalc_Exc1_max"] <- NA
  df.lev[!is.finite(df.lev[, "MembCalc_Exc2_min"]), "MembCalc_Exc2_min"] <- NA
  suppressWarnings(df.lev[, "MembCalc_Rule12_max"] <- apply(df.lev[, 
                                                                   c("MembCalc_Rule2_min", "MembCalc_Rule1_max")], 1, max, 
                                                            na.rm = TRUE))
  df.lev[!is.finite(df.lev[, "MembCalc_Rule12_max"]), "MembCalc_Rule12_max"] <- NA
  df.lev[, "Level.Membership"] <- apply(df.lev[, c("MembCalc_Rule12_max", 
                                                   "MembCalc_Rule0_min")], 1, min, na.rm = TRUE)
  boo_exceptions <- FALSE
  if (isTRUE(boo_exceptions)) {
    boo_CT_F1_L4 <- df.lev[, col_INDEX_NAME] == "BCG_CT_2015" & 
      df.lev[, col_INDEX_CLASS] == "fish01" & df.lev[, 
                                                     col_LEVEL] == 4
    df.lev[boo_CT_F1_L4, "MembCalc_Exc1_max"] <- max(c(df.lev[boo_CT_F1_L4, 
                                                              "MembCalc_Exc1_max"], df.lev[boo_CT_F1_L4, "MembCalc_Exc0_min"]), 
                                                     na.rm = TRUE)
    df.lev[!is.finite(df.lev[, "MembCalc_Exc1_max"]), "MembCalc_Exc1_max"] <- NA
    df.lev[boo_CT_F1_L4, "Level.Membership"] <- min(c(df.lev[boo_CT_F1_L4, 
                                                             "MembCalc_Exc1_max"], df.lev[boo_CT_F1_L4, "MembCalc_Exc2_min"]), 
                                                    na.rm = TRUE)
    boo_CT_F23_L4 <- df.lev[, col_INDEX_NAME] == "BCG_CT_2015" & 
      (df.lev[, col_INDEX_CLASS] == "fish02" | df.lev[, 
                                                      col_INDEX_CLASS] == "fish03") & df.lev[, col_LEVEL] == 
      2
    df.lev[boo_CT_F23_L4, "Level.Membership"] <- min(c(df.lev[boo_CT_F23_L4, 
                                                              "MembCalc_Exc0_min"], df.lev[boo_CT_F23_L4, "MembCalc_Exc1_max"]), 
                                                     na.rm = TRUE)
  }
  df.lev[!is.finite(df.lev[, "Level.Membership"]), "Level.Membership"] <- NA
  df.lev[, col_LEVEL] <- paste0("L", df.lev[, col_LEVEL])
  names(df.lev)[names(df.lev) == col_SAMPLEID] <- "SAMPLEID"
  names(df.lev)[names(df.lev) == col_INDEX_NAME] <- "INDEX_NAME"
  names(df.lev)[names(df.lev) == col_INDEX_CLASS] <- "INDEX_CLASS"
  names(df.lev)[names(df.lev) == col_LEVEL] <- "LEVEL"
  df.lev.wide <- reshape2::dcast(df.lev, SAMPLEID + INDEX_NAME + 
                                   INDEX_CLASS + INDEX_CLASS_ORIG ~ LEVEL, value.var = "Level.Membership")
  names(df.lev.wide)[names(df.lev.wide) == "SAMPLEID"] <- toupper(col_SAMPLEID)
  names(df.lev.wide)[names(df.lev.wide) == "INDEX_NAME"] <- toupper(col_INDEX_NAME)
  names(df.lev.wide)[names(df.lev.wide) == "INDEX_CLASS"] <- toupper(col_INDEX_CLASS)
  names(df.lev.wide)[names(df.lev.wide) == "LEVEL"] <- toupper(col_LEVEL)
  col.Levels <- c(paste0("L", 1:6))
  col.Other <- names(df.lev.wide)[!(names(df.lev.wide) %in% 
                                      col.Levels)]
  col.Levels.Present <- names(df.lev.wide)[(names(df.lev.wide) %in% 
                                              col.Levels)]
  col.Levels.Absent <- col.Levels[!col.Levels %in% names(df.lev.wide)]
  df.lev.wide[, col.Levels.Absent] <- 0
  df.subtotal <- df.lev.wide[, c(col.Other, col.Levels)]
  col.rename <- names(df.subtotal) %in% col.Levels
  col.sub <- paste0(names(df.subtotal)[col.rename], ".Sub")
  names(df.subtotal)[col.rename] <- col.sub
  df.rules.numruleslev <- dplyr::summarize(dplyr::group_by(df.rules, 
                                                           INDEX_NAME, INDEX_CLASS), .groups = "drop_last", rules_lev_min = min(LEVEL, 
                                                                                                                                na.rm = TRUE), rules_lev_max = max(LEVEL, na.rm = TRUE))
  df.subtotal <- merge(df.subtotal, df.rules.numruleslev, 
                       by = c("INDEX_NAME", "INDEX_CLASS"), all.x = TRUE)
  col.ruleslev <- c("rules_lev_min", "rules_lev_max")
  rnd_dig <- 8
  df.subtotal[, "L1"] <- df.subtotal[, "L1.Sub"]
  df.subtotal[, "L2"] <- apply(df.subtotal[, c("L1", "L2.Sub")], 
                               1, function(x) min(round(1 - x[1], rnd_dig), x[2], na.rm = TRUE))
  df.subtotal[, "L3"] <- apply(df.subtotal[, c("L1", "L2", 
                                               "L3.Sub")], 1, function(x) min(round(1 - sum(x[1], x[2], 
                                                                                            na.rm = TRUE), rnd_dig), x[3], na.rm = TRUE))
  df.subtotal[, "L4"] <- apply(df.subtotal[, c("L1", "L2", 
                                               "L3", "L4.Sub")], 1, function(x) min(round(1 - sum(x[1], 
                                                                                                  x[2], x[3], na.rm = TRUE), rnd_dig), x[4], na.rm = TRUE))
  df.subtotal[, "L5"] <- apply(df.subtotal[, c("L1", "L2", 
                                               "L3", "L4", "L5.Sub")], 1, function(x) min(round(1 - 
                                                                                                  sum(x[1], x[2], x[3], x[4], na.rm = TRUE), rnd_dig), 
                                                                                          x[5], na.rm = TRUE))
  boo_L5fix <- df.subtotal[, "rules_lev_max"] == 4
  if (sum(boo_L5fix) > 0) {
    df.subtotal[boo_L5fix, "L5"] <- apply(df.subtotal[boo_L5fix, 
                                                      c("L1", "L2", "L3", "L4")], 1, function(x) round(1 - 
                                                                                                         sum(x[1], x[2], x[3], x[4], x[5], na.rm = TRUE), 
                                                                                                       rnd_dig))
  }
  df.subtotal[, "L6"] <- apply(df.subtotal[, c("L1", "L2", 
                                               "L3", "L4", "L5")], 1, function(x) round(1 - sum(x[1], 
                                                                                                x[2], x[3], x[4], x[5], na.rm = TRUE), rnd_dig))
  df.results <- df.subtotal[, !(names(df.subtotal) %in% c(col.sub, 
                                                          col.ruleslev))]
  df.results[, col_INDEX_CLASS] <- df.results[, col_INDEX_CLASS_ORIG]
  col_drop_ICORIG <- !names(df.results) %in% col_INDEX_CLASS_ORIG
  df.results <- df.results[, col_drop_ICORIG]
  return(df.results)
}
