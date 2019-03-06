dat2 <- PhD_Dataset_Insurance

dat2 <- na.omit(dat2)

logitInsurance_insurance <- glm(INSURANCE_DEPENDENT ~ 
                              dat2$INSURANCE_INCREASE_COST +
                              dat2$INSURANCE_INCREASE_PERCENTAGE +
                              dat2$INSURANCE_PREMIUM_DISCOUNT +
                              dat2$INSURANCE_COVERED +
                              dat2$INSURANCE_COVERAGE_STORMWATER +
                              dat2$INSURANCE_COVERAGE_RIVERINE +
                              dat2$INSURANCE_COVERAGE_SEA +
                              dat2$INSURANCE_COVERAGE_CONFUSING +
                              dat2$INSURANCE_COST +
                              dat2$INSURANCE_SIMPLE +
                              dat2$INSURANCE_EFFECTIVE +
                              dat2$INSURANCE_PREMIUM +
                              dat2$INSURANCE_COST_DECISION +
                              dat2$INSURANCE_DEDUCTIBLE +
                              dat2$INSURANCE_RECOMMENDATION +
                              dat2$INSURANCE_EXCESS +
                              dat2$INSURANCE_GOVT_SUBSIDIZE +
                              dat2$INSURANCE_LOAN +
                              dat2$INSURANCE_FURTHER_COSTS +
                              dat2$INSURANCE_RECOVERY +
                              dat2$INSURANCE_OPTIONS,
                              data = dat2)

summary(logitInsurance_insurance)

step(logitInsurance_insurance, direction = 'backward', trace = FALSE)

logitInsurance_insurance_backward <- glm(INSURANCE_DEPENDENT ~ 
                                  dat$INSURANCE_COVERAGE_RIVERINE +
                                  dat$INSURANCE_RECOMMENDATION +
                                  dat$INSURANCE_GOVT_SUBSIDIZE +
                                  dat$INSURANCE_FURTHER_COSTS,
                                  data = dat2)

summary(logitInsurance_insurance_backward)

vif(logitInsurance_insurance_backwward)

Insurance_backward_table <- xtable(logitInsurance_insurance_backward)
print.xtable(Insurance_backward_table, type="html", file="PhD_Paper_Insurance_table.html")

# Barplots for IVs

id_insurance_increase_cost <- table(dat2$INSURANCE_INCREASE_COST)

barplot(main = "Insurance Increase Cost", table(dat2$INSURANCE_INCREASE_COST))

id_insurance_increase_percentage <- table(dat2$INSURANCE_INCREASE_PERCENTAGE)

barplot(main = "Insurance Increase Percentage", table(dat2$INSURANCE_INCREASE_PERCENTAGE))

id_insurance_premium_discount <- table(dat2$INSURANCE_PREMIUM_DISCOUNT)

barplot(main = "Insurance Premium Discount", table(dat2$INSURANCE_PREMIUM_DISCOUNT))

id_insurance_covered <- table(dat2$INSURANCE_COVERED)

barplot(main = "Insurance Covered", table(dat2$INSURANCE_COVERED))

id_insurance_coverage_storwater <- table(dat2$INSURANCE_COVERAGE_STORMWATER)

barplot(main = "Insurance Coverage Stormwater", table(dat2$INSURANCE_COVERAGE_STORMWATER))

id_insurance_coverage_riverine <- table(dat2$INSURANCE_COVERAGE_RIVERINE)

barplot(main = "Insurance Coverage Riverine", table(dat2$INSURANCE_COVERAGE_RIVERINE))

id_insurance_coverage_sea <- table(dat2$INSURANCE_COVERAGE_SEA)

barplot(main = "Insurance Coverage Sea", table(dat2$INSURANCE_COVERAGE_SEA))

id_insurance_coverage_confusing <- table(dat2$INSURANCE_COVERAGE_CONFUSING)

barplot(main = "Insurance Coverage Confusing", table(dat2$INSURANCE_COVERAGE_CONFUSING))

id_insurance_cost <- table(dat2$INSURANCE_COST)

barplot(main = "Insurance Cost", table(dat2$INSURANCE_COST))

id_insurance_simple <- table(dat2$INSURANCE_SIMPLE)

barplot(main = "Insurance Simple", table(dat2$INSURANCE_SIMPLE))

id_insurance_effective <- table(dat2$INSURANCE_EFFECTIVE)

barplot(main = "Insurance Effective", table(dat2$INSURANCE_EFFECTIVE))

id_insurance_premium <- table(dat2$INSURANCE_PREMIUM)

barplot(main = "Insurance Premium", table(dat2$INSURANCE_PREMIUM))

id_insurance_cost_decision <- table(dat2$INSURANCE_COST_DECISION)

barplot(main = "Insurance Cost Decision", table(dat2$INSURANCE_COST_DECISION))

id_insurance_deductible <- table(dat2$INSURANCE_DEDUCTIBLE)

barplot(main = "Insurance Deductible", table(dat2$INSURANCE_DEDUCTIBLE))

id_insurance_recommendation <- table(dat2$INSURANCE_RECOMMENDATION)

barplot(main = "Insurance Recommendation", table(dat2$INSURANCE_RECOMMENDATION))

id_insurance_excess <- table(dat2$INSURANCE_EXCESS)

barplot(main = "Insurance Excess", table(dat2$INSURANCE_EXCESS))

id_insurance_govt_subsidize <- table(dat2$INSURANCE_GOVT_SUBSIDIZE)

barplot(main = "Insurance Govt Subsidize", table(dat2$INSURANCE_GOVT_SUBSIDIZE))

id_insurance_loan <- table(dat2$INSURANCE_LOAN)

barplot(main = "Insurance Loan", table(dat2$INSURANCE_LOAN))

id_insurance_further_costs <- table(dat2$INSURANCE_FURTHER_COSTS)

barplot(main = "Insurance Further Costs", table(dat2$INSURANCE_FURTHER_COSTS))

id_insurance_recovery <- table(dat2$INSURANCE_RECOVERY)

barplot(main = "Insurance Recovery", table(dat2$INSURANCE_RECOVERY))

id_insurance_options <- table(dat2$INSURANCE_OPTIONS)

barplot(main = "Insurance Options", table(dat2$INSURANCE_OPTIONS))

# DV

id_insurance_dependent <- table(dat2$INSURANCE_DEPENDENT)

barplot(main = "Insurance Dependent", table(dat2$INSURANCE_DEPENDENT))

# Graphs

damage_and_subsidize <- dat2[ ,c("TIMES_DAMAGED_INDEPENDENT", "INSURANCE_GOVT_SUBSIDIZE")]

damage_table <- table(dat2$TIMES_DAMAGED_INDEPENDENT)
subsidize_table <- table(dat2$INSURANCE_GOVT_SUBSIDIZE)

plot(damage_table)
plot(subsidize_table)

damaged_0 <- subset(damage_and_subsidize, TIMES_DAMAGED_INDEPENDENT==0)
damaged_1 <- subset(damage_and_subsidize, TIMES_DAMAGED_INDEPENDENT==1)
damaged_2 <- subset(damage_and_subsidize, TIMES_DAMAGED_INDEPENDENT==2)

damaged_0_table <- table(damaged_0)
damaged_1_table <- table(damaged_1)
damaged_2_table <- table(damaged_2)

damaged_0_barplot <- barplot(damaged_0_table)
damaged_1_barplot <- barplot(damaged_1_table)
damaged_2_barplot <- barplot(damaged_2_table)

barplot_combined <- rbind(damaged_0_table, damaged_1_table, damaged_2_table)

barplot(barplot_combined, 
        beside = TRUE, 
        main = "Times Damaged by Govt. Subsidize",
        names.arg = c("Not agree at all", 2, 3, 4, "Completely agree"),
        ylab = "Number of Responses",
        cex.names = .75,
        legend.text = TRUE)
       
dat4 <- PhD_Dataset

id_insurance_govt_resp <- table(dat4$RESPONSIBILITY_GOVERNMENT)

barplot(main = "Responsbility Government", table(dat4$RESPONSIBILITY_GOVERNMENT))

id_insurance_ind_resp <- table(dat4$RESPONSIBILITY_INDIVIDUAL)

barplot(main = "Responsibility Individual", table(dat4$RESPONSIBILITY_INDIVIDUAL))

barplot_combined_resp <- rbind(id_insurance_govt_resp, id_insurance_ind_resp)

barplot(barplot_combined_resp, 
        beside = TRUE, 
        main = "Flood Damage Responsibility",
        names.arg = c("Not agree at all", 2, 3, 4, "Completely agree"),
        ylab = "Number of Responses",
        cex.names = .75,
        legend.text = TRUE,
        args.legend = list(x = "topleft"))

id_insurance_land_better_usage <- table(dat4$LAND_USE_BETTER_USAGE)

barplot(main = "Land Use Alternatives", table(dat4$LAND_USE_BETTER_USAGE))

id_insurance_land_engineering <- table(dat4$LAND_USE_ENGINEERING)

barplot(main = "Land Use Engineering", table(dat4$LAND_USE_ENGINEERING))

id_insurance_land_buffer <- table(dat4$LAND_USE_BUFFER)

barplot(main = "Land Use Buffer", table(dat4$LAND_USE_BUFFER))

barplot_combined_land <- rbind(id_insurance_land_better_usage, id_insurance_land_engineering, id_insurance_land_buffer)

barplot(barplot_combined_land, 
        beside = TRUE, 
        main = "Land Use Alternatives",
        names.arg = c("Not agree at all", 2, 3, 4, "Completely agree"),
        ylab = "Number of Responses",
        cex.names = .75,
        legend.text = TRUE,
        args.legend = list(x = "topleft"))

id_insurance_assist_govt <- table(dat4$ASSISTANCE_GOVT)

barplot(main = "Expected Govt. Assistance", table(dat4$ASSISTANCE_GOVT))

id_insurance_assist_commut <- table(dat4$ASSISTANCE_COMMUNITY)

barplot(main = "Expected Community Assistance", table(dat4$ASSISTANCE_COMMUNITY))

id_insurance_assist_insurer <- table(dat4$ASSISTANCE_INSURER)

barplot(main = "Expected Insurer Assistance", table(dat4$ASSISTANCE_INSURER))

id_insurance_assist_relatives <- table(dat4$ASSISTANCE_RELATIVES)

barplot(main = "Expected Relatives Assistance", table(dat4$ASSISTANCE_RELATIVES))

barplot_combined_assist <- rbind(id_insurance_assist_govt, id_insurance_assist_commut, id_insurance_assist_insurer, id_insurance_assist_relatives)

barplot(barplot_combined_assist, 
        beside = TRUE, 
        main = "Expected Assistance",
        names.arg = c("Not agree at all", 2, 3, 4, "Completely agree"),
        ylab = "Number of Responses",
        cex.names = .75,
        legend.text = TRUE,
        args.legend = list(x = "top"))

id_insurance_cost <- table(dat4$INSURANCE_COST)

barplot(main = "Insurance Cost", table(dat4$INSURANCE_COST))

id_insurance_effective <- table(dat4$INSURANCE_EFFECTIVE)

barplot(main = "Insurance Effective", table(dat4$INSURANCE_EFFECTIVE))

id_insurance_simple <- table(dat4$INSURANCE_SIMPLE)

barplot(main = "Insurance Simple", table(dat4$INSURANCE_SIMPLE))

barplot_combined_insurance <- rbind(id_insurance_cost, id_insurance_effective, id_insurance_simple)

barplot(barplot_combined_insurance, 
        beside = TRUE, 
        main = "Flood Insurance",
        names.arg = c("Not agree at all", 2, 3, 4, "Completely agree"),
        ylab = "Number of Responses",
        cex.names = .75,
        legend.text = TRUE,
        args.legend = list(x = "topright"))

