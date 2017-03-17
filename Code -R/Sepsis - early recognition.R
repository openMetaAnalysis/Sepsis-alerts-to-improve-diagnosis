library(meta)      # Used for function metagen
library(gemtc)     # Used for function blobbogram
library(grid)

##Conversions
# Hayden
CI_Width = 73.6 - 44
N = 238
SE = CI_Width/2/1.96 #Chinn, 2000 PMID 11113947
SD = SE * N^0.5  #Varies with sample size. Also called "S"
VAR = SD ^ 2

dataframe <- read.table(textConnection('
  Study,      Year, PMID, Design,             OR,   CI.l, CI.u, Outcome,                 Criteria,                      Action,                         style
  "Berger",   2010, 23616849, "Before-after", 2.64, 2.17, 3.22, "Lactate level obtained",  "SIRS=2 or more",                   "EHR popup urges lactate level",        "normal"
  "Berger",   2010, 23616849, "Before-after", 0.9,  0.60, 1.35, "Mortality",               "SIRS=2 or more",                   "EHR popup urges lactate level",        "normal"
  "Nelson",   2011, 21227543, "Before-after", 1.7,  0.9,  3.2,  "Lactate level obtained",  "SIRS=2 or more and SBP <90 mmHg x2", "Caregivers paged; recs placed in EHR", "normal"
  "Nelson",   2011, 21227543, "Before-after", 2.9,  1.1,  7.7,  "Blood culture obtained",  "SIRS=2 or more and SBP <90 mmHg x2", "Caregivers paged; recs placed in EHR", "normal"
  "Narayanan",2016, 26573784, "Before-after", 3.47, 1.92, 6.26, "Antibiotics",             "SIRS=2 or more; second alert if severe criteria", "SSC 3 hr bundle if severe", "normal"
  "Narayanan",2016, 26573784, "Before-after", 0.64, 0.26, 1.57, "Mortality",               "SIRS=2 or more; second alert if severe criteria", "SSC 3 hr bundle if severe", "normal"
  "Hayden",   2016, 26386734, "Before-after", 1.58, 0.70, 3.58, "Mortality",               "2 or more SIRS + MD ok; second alert if SBP < 90 + MD ok", "Multifactorial", "normal"
  "Hayden",   2016, 26386734, "Before-after", 6.3557, 3.8886, 10.3881, "Antibiotics",      "SIRS=2 or more + MD ok; second alert if SBP < 90 + MD ok", "Multifactorial", "normal"
  "Hayden",   2016, 26386734, "Before-after", 13.86, 4.72, 40.66, "Lactate level obtained","SIRS=2 or more + MD ok; second alert if SBP < 90 + MD ok", "Multifactorial", "normal"
  "Rosenqvist",2017,28276800, "Before-after", 26.64, 11.12, 63.82,"Antibiotics",           "Severe vitals and fever",    "Multifactorial", "normal"
  "Rosenqvist",2017,28276800, "Before-after", 3, 0.99, 9.10,"Blood culture obtained","Severe vitals and fever",    "Multifactorial", "normal"
  "Rosenqvist",2017,28276800, "Before-after", 8.43, 3.01, 23.63,"Lactate level obtained","Severe vitals and fever",    "Multifactorial", "normal"
  "Rosenqvist",2017,28276800, "Before-after", 1.33, 0.54, 3.29,"Mortality","Severe vitals and fever",    "Multifactorial", "normal"
'), header=TRUE, sep=",",strip.white=TRUE)

colnames(dataframe)[9] <- "Criteria to fire alert" 
colnames(dataframe)[10] <- "Action triggerd by alert" 

dataframe$Study <- paste(dataframe$Study, ", ",dataframe$Year, sep="")

#Ln transformations
meta1 <- dataframe
meta1$OR.Ln <- log(meta1$OR)
meta1$CI.l.Ln <- log(meta1$CI.l)
meta1$CI.u.Ln <- log(meta1$CI.u)

# OR conversion to effect size
# From:
# Chinn, 2000. From http://pubmed.gov/11113947
# Replicating Chinn using her data
effect.size <- log(1.32)/1.81
SE <-(log(5.68) - log(0.31))/2/1.96/1.81
# Now using our data
## METAGEN APPEARS TO HAVE CHINN'S ADJUSTMENT ALREADY INTEGRATED
meta1$CI.Ln_Width <- meta1$CI.u.Ln - meta1$CI.l.Ln
meta1$SE.Ln <- meta1$CI.Ln_Width/2/1.96 
# Package meta appears to already do the conversion of OR to effect size.
meta1$effect.size <- meta1$OR.Ln#/1.81
meta1$effect.size.SE <- meta1$SE.Ln#/1.81
#SD from SE: http://handbook.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
# Not needed in this analysis
# meta$SD.Ln <- meta$SE.Ln * meta$N^0.5  #Varies with sample size
# meta$variance= meta$SD.Ln^2
# SMD = MEAN.DIFF / SD

#-------------------------
# Meta-analysis from library meta
meta1 <- metagen(meta1$effect.size, meta1$effect.size.SE, sm="OR", byvar=Outcome, data=meta1, comb.fixed=FALSE, backtransf = TRUE, studlab=meta1$Study)
forest(meta1,print.p=FALSE, xlim=c(0.1,10), leftcols=c("studlab","Criteria to fire alert","Action triggerd by alert"), just.addcols.left = "left", colgap.left = "5mm", print.tau2=FALSE,col.diamond="blue", col.diamond.lines="blue", print.I2.ci=TRUE, overall=FALSE, label.left = NULL, label.right=NULL, calcwidth.random = FALSE, calcwidth.pooled = FALSE)
grid.text("ED Interventions to improve recognition of severe sepsis", 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
