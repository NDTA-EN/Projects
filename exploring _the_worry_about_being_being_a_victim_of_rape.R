install.packages("survey")
install.packages("tidyverse")
install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggeffects")
install.packages("chisq.posthoc.test")
install.packages("car")
install.packages("forestplot")
library("survey")
library("tidyverse")
library("haven")
library("dplyr")
library("ggplot2")
library("ggeffects")
library("chisq.posthoc.test")
library("car")
library("forestplot")

csew <- read_sav("csew1314teachingopen.sav")
ls(csew)
str(csew)
summary(csew)
nrow(csew)
nrow(csew_q1)
nrow(csew_q2)
sum(weights(csew_design))

csew$wrapedF <- as.character(as_factor(csew$wraped))
csew$ethgrp2aF <- as.character(as_factor(csew$ethgrp2a))

csew_q1 <- csew %>% 
  mutate(ethgrp2a_combined = ifelse(ethgrp2aF %in% c("Asian or Asian British", "Black or Black British", "Chinese or Other", "Mixed"),"Minority",
                                    ifelse(ethgrp2aF == "White", "White", NA_character_)), 
         wraped_combined = ifelse(wrapedF %in% c("Very worried", "Fairly worried"), "Worried", "Not worried")) %>% 
  filter(!is.na(ethgrp2a_combined), !is.na(wraped_combined), wrapedF != "Refusal", wrapedF != "Don't Know", wrapedF != "Not applicable")

table(csew_q1$wrapedF, useNA = "ifany")
table(csew_q1$wraped_combined, useNA = "ifany")
table(csew_q1$ethgrp2aF, useNA = "ifany")
table(csew_q1$ethgrp2a_combined, useNA = "ifany")

table(as_factor(csew$wraped), useNA = "ifany")
table(csew$sex, is.na(csew$wraped))

csew_design <- svydesign(data = csew_q1, id =~1, nest = TRUE, weights = ~IndivWgtx) # creation of weighted data set

# Univariate Analysis of wraped and ethgrp2a

tbl_wrapevic <- svytable(~wrapedF, design = csew_design)
round(prop.table(svytable(~wrapedF, design = csew_design)), 2)*100

svytable(~wraped_combined, design = csew_design)
round(prop.table(svytable(~wraped_combined, design = csew_design)), 2)*100

svytable(~ethgrp2aF, design = csew_design)
round(prop.table(svytable(~ethgrp2aF, design = csew_design)), 2)*100

svytable(~ethgrp2a_combined, design = csew_design)
round(prop.table(svytable(~ethgrp2a_combined, design = csew_design)), 2)*100

# Univariate Interpretations

# As we can see from the svytable above, there are 2060 respondents that answered the question about how worried they are being a victim rape
# From this number, we can gleam that most respondents answered with "not worried at all" at 52%. 
# In contrast to this the opposite extreme, "Very Worried" falls sits at 7% showing it as the least picked option.

# From the combined rape table above, we can see that an overwhelming majority of respondents are "Not Worried" about rape (84%) or 1733 people
# Only 16% or 326 respondents are worried about being a victim of rape

# As we can see from the function above, there were 2060 respondents in total. 262 (13%) of these were from minority backgrounds and 1798 (87%) where part of the majority

# Once we breakdown these categories, we're looking at 1798 White respondents. With the minority groups split between Asian, Black, Chinese or Other, Mixed
# All proportionately making up, 7%, 3%, 2%, 1% respectively

# Univariate Visualisation

ggplot(as.data.frame(svytable(~wrapedF, design = csew_design)), aes(x = wrapedF, y = Freq, fill = (wrapedF))) + 
  geom_bar(stat = "identity") + labs(x = "Worry categories", y = "Level", title = "Worry about being a victim of rape") + theme_bw() + coord_flip() # bar vs hist

ggplot(as.data.frame(svytable(~ethgrp2aF, design = csew_design)), aes(x = ethgrp2aF, y = Freq, fill = (ethgrp2aF))) + 
  geom_bar(stat = "identity") + labs(x = "Ethnic Groups", y = "Population") + theme_bw() + coord_flip() # bar vs pie

ggplot(as.data.frame(svytable(~ethgrp2aF, design = csew_design)), aes(x = "", y = Freq, fill = (ethgrp2aF))) +
  geom_bar(stat = "identity", width = 1, colour = "white") + coord_polar("y", start = 0) + theme_void() + position = position_stackvjust = 0.5
  
# Q1 Bivariate - Does a higher proportion of minorities worry about being raped?

con_tbl_q1 <- svytable(~wraped_combined + ethgrp2a_combined, design = csew_design)
round(prop.table(con_tbl_q1, 2), 2)*100

q1_chisq_result <- svychisq(~wraped_combined + ethgrp2a_combined, design = csew_design)
q1_chisq_result

observed_counts<-q1_chisq_result$observed
round(prop.table(observed_counts, 2), 2)*100

expected_counts <- q1_chisq_result$expected
round(prop.table(expected_counts, 2), 2)*100

# Q1 Interpretations

# As can be gleamed from the table. The majority answered the survey with 85% "Not worried" and 15% "Worried"
# We can also see that Minority respondents answered, with 78% "Not worried" and 22% "Worried".
# showing a high preposition of the minority population sample are worried about being victims of rape. Partially answering my 1st question

# From running a Chi-squared test, we can see that the p-value = 0.03; meaning that there is evidence of an effect on the number of worried people when we look at minorities
# In consistence with this finding is a large f-statistic at 4.70 with corresponds with a significant p-value

# When we look at the difference between the observed and expected counts, we can see that for the majority theirs a difference of 1% in both "not worried" and "worried"
# For minorities this difference changes to 6%

# Putting all of this together, we can say that there is a higher likelihood that a "higher proportion of minorities worry about being raped".

# Q1 Visualisation

q1_ethnicgrp <- c("Minority", "Minority", "Majority", "Majority")
condition <- c("Not worried", "Worried", "Not worried", "Worried")
value <- as_vector(prop.table(con_tbl_q1, 2)*100)
q1_dataframe <- data.frame(Group = q1_ethnicgrp, Worry = condition, Percent = value)

ggplot(q1_dataframe, aes(x = Group, y = Percent.Freq, fill = Worry)) + geom_bar(stat = "identity", width = 0.5) + 
  labs(x = "Ethnicity Split", y = "Percentage", title = "Perportion of worry \nbetween the Majority and Minority") +
  scale_fill_discrete(name = "") + scale_fill_brewer(palette = "Pastel1") + theme_minimal() + theme(legend.title = element_blank()) + coord_flip()

# Q2 Bivariate - Is there one ethnicity that worries more then others?

csew_q2 <- csew %>% mutate(wraped_combined = ifelse(wrapedF %in% c("Very worried", "Fairly worried"), "Worried", "Not Worried")) %>%
  filter(!is.na(ethgrp2aF), ethgrp2aF != "Refusal", ethgrp2aF != "Don't Know", wrapedF != "Refusal", wrapedF != "Don't Know", wrapedF != "Not applicable", !is.na(wrapedF))

table(csew_q2$ethgrp2aF, useNA = "ifany")
table(csew_q2$wrapedF, useNA = "ifany")
table(csew_q2$wraped_combined, useNA = "ifany")

csew_design_q2 <- svydesign(data = csew_q2, id =~1, nest = TRUE, weights = ~IndivWgtx) # creation of weighted data set for Q2

con_tbl_q2 <- svytable(~wraped_combined + ethgrp2aF, design = csew_design)
round(prop.table(con_tbl_q2, 2), 2)*100

q2_chisq_result <- svychisq(~wraped_combined + ethgrp2aF, design = csew_design)
q2_chisq_result

q2_observed_counts<-q2_chisq_result$observed
round(prop.table(q2_observed_counts, 2), 2)*100

q2_expected_counts <- q2_chisq_result$expected
round(prop.table(q2_expected_counts, 2), 2)*100

# Q2 Interpretations

# As seen in the frequency table, the Asian respondents answered with 77% "Not worried" and 23% "Worried.
# The Black respondents answered with 73% "Not worried" and 27% "Worried". Chinese or Other respondents answered with 86% "Not worried" and 14% "Worried".
# Mixed respondents answered with 91% "Not worried" and 9% "Worried". White respondents answered with 85% "Not worried" and 15% "Worried".
# From these responses we can say that the Black British population has a higher preportion of worry about being a victim of rape, followed by the Asian British population.

# Through the Chisq test, we can see that the p-value is 0.04 indicating a statisticly significance and chance that a change occurs
# Answering the question of "Is there one ethnicity that worries more then others?"

# Looking at the expected and observed results, it shows that the differences for the groups are as follows:
# Asian: 7% | Black: 11% | Chinese or Other: 2% | Mixed: 7% | White: 1%
# Showing a the largest difference for both Asian and Black British respondents

# Q2 Visualisation

q2_ethnicgrp <- c("Asian or Asian British", "Asian or Asian British", "Black or Black British", "Black or Black British", 
                  "Chinese or Other", "Chinese or Other", "Mixed", "Mixed", "White", "White")
q2_condition <- c("Not worried", "Worried", "Not worried", "Worried",
                  "Not worried", "Worried", "Not worried", "Worried", "Not worried", "Worried")
q2_value <- as.vector(prop.table(con_tbl_q2, 2)*100)
q2_dataframe <- data.frame(Group = q2_ethnicgrp, Worry = q2_condition, Percent = q2_value)

ggplot(q2_dataframe, aes(x = Group, y = Percent, fill = Worry)) + geom_bar(stat = "identity") + 
  labs(x = "Ethnic Groups", y = "Percentage", title = "Worry about Rape by Ethnicity") + theme_minimal() + 
  theme(axis.text.x = element_text(hjust = 1), legend.position = "bottom") + scale_fill_brewer(palette = "Pastel1") +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1))

# Logistic Regression

csew_q2$ethgrp2aF <- relevel(factor(csew_q2$ethgrp2aF), ref = "White")
csew_q2$wrapedBI <- ifelse(csew_q2$wraped_combined == "Worried", 1, 0)

csew_q2$sexF <- as.character(as_factor(csew_q2$sex))
csew_q2$sexF <- relevel(factor(csew_q2$sexF), ref = "Male")

csew_q2$agegrp7F <- as.character(as_factor(csew_q2$agegrp7))
csew_q2$agegrp7F <- as_factor(csew_q2$agegrp7)

csew_q2$livharm1F <- as.character(as_factor(csew_q2$livharm1))

csew_q2$educat3F <- as.character(as_factor(csew_q2$educat3))

csew_q2$rural2F <- as.character(as_factor(csew_q2$rural2))

csew_design_q2 <- svydesign(data = csew_q2, id =~1, nest = TRUE, weights = ~IndivWgtx)

logis1 <- svyglm(wrapedBI ~ ethgrp2aF, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis1)
coef(logis1)
exp(coef(logis1)) # gives log odds
exp(coef(logis1) - 1)*100

logis_fit2 <- svyglm(wrapedBI ~ ethgrp2aF + sexF, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis_fit2)
coef(logis_fit2)
vif(logis_fit2)

logis_fit3 <- svyglm(wrapedBI ~ ethgrp2aF + agegrp7F, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis_fit3)
coef(logis_fit3)
vif(logis_fit3)

logis_fit4 <- svyglm(wrapedBI ~ ethgrp2aF + sexF + agegrp7F, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis_fit4)
vif(logis_fit4)

logis_fit5 <- svyglm(wrapedBI ~ ethgrp2aF + sexF + agegrp7F + livharm1F, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis_fit5)
vif(logis_fit5)

logis_fit6 <- svyglm(wrapedBI ~ ethgrp2aF + sexF + agegrp7F + educat3F, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis_fit6)
vif(logis_fit6)

logis_fit7 <- svyglm(wrapedBI ~ ethgrp2aF + sexF + agegrp7F + rural2F, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis_fit7)
vif(logis_fit7)

logis_fit8 <- svyglm(wrapedBI ~ ethgrp2aF + sexF + agegrp7F + livharm1F + educat3F + rural2F, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis_fit8)
vif(logis_fit8)

logis_fit10 <- svyglm(wrapedBI ~ ethgrp2aF * sexF * agegrp7F, design = csew_design_q2, family = quasibinomial(link = "logit"))
summary(logis_fit10)

# Logistic Regression Interpretation

# As seen in the first logistic regression (logis1), when testing ethgp2a and worry about being a victim of rape. 
# We can see that only the Black or Black British group are statistically significant
# Using the exp(coef) functions allows the veiwing of the log odds; Asian or Asian British and Black or Black British being above 1 shows a significance change

# Logistic Regression Visualisation

confint(logis1)
coef(logis1)
exp(forest_data_v1)
exp(coef(logis1))
exp(confint(logis1))

row_names <- list(list("Asian or Asian British", "Black or Black British", "Chinese or Other", "Mixed"))

forest_data_v1 <- data.frame(
  coef = c(1.7457043, 2.1194933, 0.9148007, 0.5422716),
  low = c(0.9482127, 1.0320776, 0.3235196, 0.1507987),
  high = c(3.213924, 4.352630, 2.586737, 1.950007)
  )

forest_data_v1 |> forestplot(labeltext = row_names,
                          mean = coef, lower = low, upper = high, zero = 1, cex = 2, lineheight = "auto", xlab = "Likelihood of worring about being a victim of rape compared to white ethnicity", boxsize = 0.25, graph.pos = "right",
                          col = fpColors( box = "chartreuse3", zero = "red"), title = "Increase in worry of being a victim of rape") |> 
                          fp_add_header("Reference Group: White") |> fp_set_style(lines = gpar(col = "black", lty = "solid"))

confint(logis_fit2)
coef(logis_fit2)
exp(coef(logis_fit2))
exp(confint(logis_fit2))

forest_data_v2 <- data.frame(
  coef = c(2.04672767, 2.92754293, 1.20188140, 0.58150828),
  low = c(1.05835360, 1.24328888, 0.39727185, 0.16384306),
  high = c(3.95812342, 6.89341608, 3.63609681, 2.06387670)
)

forest_data_v2 |> forestplot(labeltext = row_names,
                             mean = coef, lower = low, upper = high, zero = 1, cex = 2, lineheight = "auto", xlab = "Likelihood of worring about being a victim of rape compared to white ethnicity (Inclusion of sex)", boxsize = 0.25, graph.pos = "right",
                             col = fpColors( box = "cornflowerblue", zero = "red"), title = "Increase in worry of being a victim of rape") |> 
  fp_add_header("Reference Group: White") |> fp_set_style(lines = gpar(col = "black", lty = "solid"))

confint(logis_fit3)
coef(logis_fit3)
exp(coef(logis_fit3))
exp(confint(logis_fit3))

forest_data_v3 <- data.frame(
  coef = c(1.7283775, 2.0009911, 0.9196247, 0.5433112),
  low = c(0.9329173, 0.9943963, 0.3124576, 0.1486721),
  high = c(3.2020939, 4.0265289, 2.7066379, 1.9854897)
)

forest_data_v3 |> forestplot(labeltext = row_names,
                             mean = coef, lower = low, upper = high, zero = 1, cex = 2, lineheight = "auto", xlab = "Likelihood of worring about being a victim of rape compared to white ethnicity (Inclusion of age)", boxsize = 0.25, graph.pos = "right",
                             col = fpColors( box = "darkgoldenrod1", zero = "red"), title = "Increase in worry of being a victim of rape") |> 
  fp_add_header("Reference Group: White") |> fp_set_style(lines = gpar(col = "black", lty = "solid"))

confint(logis_fit4)
coef(logis_fit4)
exp(coef(logis_fit4))
exp(confint(logis_fit4))

forest_data_v4 <- data.frame(
  coef = c(1.99628390, 2.71422743, 1.18288814, 0.56445120),
  low = c(1.03528311, 1.17420025, 0.37017732, 0.15235377),
  high = c(3.8493329, 6.2740836, 3.7798759, 2.0912195)
)

forest_data_v4 |> forestplot(labeltext = row_names,
                             mean = coef, lower = low, upper = high, zero = 1, cex = 2, lineheight = "auto", xlab = "Likelihood of worring about being a victim of rape compared to white ethnicity (Inclusion of sex and age)", boxsize = 0.25, graph.pos = "right",
                             col = fpColors( box = "cyan3", zero = "red"), title = "Increase in worry of being a victim of rape") |> 
  fp_add_header("Reference Group: White") |> fp_set_style(lines = gpar(col = "black", lty = "solid"))

confint(logis_fit5)
coef(logis_fit5)
exp(coef(logis_fit5))
exp(confint(logis_fit5))

forest_data_v5 <- data.frame(
  coef = c(2.09439760, 2.66229828, 1.17374915, 0.58278701),
  low = c(1.05968760, 1.13846062, 0.36215101, 0.16345614),
  high = c(4.1394287, 6.2258035, 3.8041785, 2.0778706)
)

forest_data_v5 |> forestplot(labeltext = row_names,
                             mean = coef, lower = low, upper = high, zero = 1, cex = 2, lineheight = "auto", xlab = "Likelihood of worring about being a victim of rape compared to white ethnicity (Inclusion of sex and age)", boxsize = 0.25, graph.pos = "right",
                             col = fpColors( box = "coral", zero = "red"), title = "Increase in worry of being a victim of rape") |> 
  fp_add_header("Reference Group: White") |> fp_set_style(lines = gpar(col = "black", lty = "solid"))

confint(logis_fit6)
coef(logis_fit6)
exp(coef(logis_fit6))
exp(confint(logis_fit6))

forest_data_v6 <- data.frame(
  coef = c(1.97554781, 2.81695770, 1.29920847, 0.60486278),
  low = c(1.01815157, 1.17036423, 0.43309456, 0.17250495),
  high = c(3.8332104, 6.7801548, 3.8973998, 2.1208608)
)

forest_data_v6 |> forestplot(labeltext = row_names,
                             mean = coef, lower = low, upper = high, zero = 1, cex = 2, lineheight = "auto", xlab = "Likelihood of worring about being a victim of rape compared to white ethnicity (Inclusion of sex and age)", boxsize = 0.25, graph.pos = "right",
                             col = fpColors( box = "purple", zero = "red"), title = "Increase in worry of being a victim of rape") |> 
  fp_add_header("Reference Group: White") |> fp_set_style(lines = gpar(col = "black", lty = "solid"))

confint(logis_fit7)
coef(logis_fit7)
exp(coef(logis_fit7))
exp(confint(logis_fit7))

forest_data_v7 <- data.frame(
  coef = c(1.83197331, 2.48182390, 1.14305802, 0.51575848),
  low = c(0.94759498, 1.06833384, 0.35456987, 0.13892139),
  high = c(3.54173068, 5.76547295, 3.68497652, 1.91480102)
)

forest_data_v7 |> forestplot(labeltext = row_names,
                             mean = coef, lower = low, upper = high, zero = 1, cex = 2, lineheight = "auto", xlab = "Likelihood of worring about being a victim of rape compared to white ethnicity (Inclusion of sex and age)", boxsize = 0.25, graph.pos = "right",
                             col = fpColors( box = "darkgreen", zero = "red"), title = "Increase in worry of being a victim of rape") |> 
  fp_add_header("Reference Group: White") |> fp_set_style(lines = gpar(col = "black", lty = "solid"))

confint(logis_fit8)
coef(logis_fit8)
exp(coef(logis_fit8))
exp(confint(logis_fit8))

forest_data_v8 <- data.frame(
  coef = c(1.88603652, 2.54191425, 1.26497982, 0.55415253),
  low = c(0.95134662, 1.04104905, 0.41766610, 0.15977250),
  high = c(3.73905122, 6.20655485, 3.83122776, 1.92201428)
)

forest_data_v8 |> forestplot(labeltext = row_names,
                             mean = coef, lower = low, upper = high, zero = 1, cex = 2, lineheight = "auto", xlab = "Likelihood of worring about being a victim of rape compared to white ethnicity (Inclusion of sex and age)", boxsize = 0.25, graph.pos = "right",
                             col = fpColors( box = "pink", zero = "red"), title = "Increase in worry of being a victim of rape") |> 
  fp_add_header("Reference Group: White") |> fp_set_style(lines = gpar(col = "black", lty = "solid"))
