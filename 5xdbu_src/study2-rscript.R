#######################
#Description of script#
#######################

#This script was used to analyze the analyze the data of Study 1 in the paper "Effects of grammatical gender on gender inferences: evidence from
#French hybrid nouns"
#To run this code, the following additional files are necessary: 
#   study2-data.csv            (contains the data)
#   model-study2.RData         (contains the Bayesian model of the data)

#########################
#Description of the data#
#########################

#The dataframe contains 12 columns: 
# * Participant: a number for each participant in the study 
# * ParticipantGender: participant's gender (female, male)
# * ParticipantAge: participant's age
# * InterviewTime: study duration for each participant (in second)
# * Noun: target noun (there are 28 common nouns, as well as additional proper names)
# * Age: age of the noun's referent according to participants, evaluated along a 7-point Likert scale
#           1: young ("âge jeune")
#           2, 3, 4, 5, 6: intermediate values for the Age variable
#           7: old ("âge avancé")
# * Gender: social gender of the noun's referent according to participants, evaluated along a 7-point Likert scale
#           1: male ("homme")
#           2, 3, 4, 5, 6: intermediate degrees of certainty about the referent's gender
#           7: female ("femme")
# * Education: level of education of the noun's referent according to participants, evaluated along a 7-point Likert scale
#           1: not advanced ("peu avancé")
#           2, 3, 4, 5, 6: intermediate degrees of education
#           7: advanced ("avancé")
# * ItemType: 
#           experimental: experimental items (masculine/feminine common nouns)
#           filler: filler items (proper names)
# * GramGender (grammatical gender): 
#           feminine: feminine grammatical gender for the noun
#           masculine: masculine grammatical gender for the noun
#           (no value): for filler items (proper nouns) 
# * Item: 
#           noun pair that includes the target noun (e.g. star/as, bête/phénomène, etc.). These pairs consist of a feminine noun and a masculine with similar semantics. There are 14 such pairs.
# * Sentence: 
#           sentence in which the target noun appeared



##########################################################
#Packages to be loaded for data manipulation and analysis#
##########################################################

if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")} #to reshape the dataset from wide to long format
if (!require("plyr")) {install.packages("plyr"); require("plyr")}#to rename levels of a variable
if (!require("stringi")) {install.packages("stringi"); require("stringi")}#to find first occurrence of character in a string
if (!require("stringr")) {install.packages("stringr"); require("stringr")}#for function str_match
if (!require("data.table")) {install.packages("data.table"); require("data.table")}#provides function for renaming columns of dataframe
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")} #for plots
if (!require("utils")) {install.packages("utils"); require("utils")} #provides the expand.grid function 

#For Bayesian ordinal regression using brms (interface to Stan)
if (!require("brms")) {install.packages("brms"); require("brms")}
packageVersion("brms")
if (!require("bayestestR")) {install.packages("bayestestR"); require("bayestestR")} #to calculate 95% credibility interval for Bayesian posterior samples 

###############
#Load the data#
###############

#load the data
datafile = read.csv("study2-data.csv", sep =",", header=TRUE)
head(datafile)
dim(datafile) 

###############
#Visualization#
###############

#Remove the filler items 
subset.for.plot <- subset(datafile, !(ItemType=="filler"))
subset.for.plot <- droplevels(subset.for.plot)

#Prepare data for plotting average with Likert scale
data.likert <- data.frame(xtabs(~GramGender+Gender, subset.for.plot))

#Plot
plot.likert <- ggplot(data.likert, aes_string(x="`GramGender`", y="`Freq`", fill="`Gender`")) + geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45)) + coord_flip() + 
  ggtitle("Influence of grammatical gender in the interpretation")+
  ylab("Percent")+
  xlab("Grammatical gender")+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="bottom")

#Save the plot
jpeg("study2-plot.jpeg", units="in", width=16, height=8, res=300)
plot.likert
dev.off()

######################
#Statistical analysis#                                                                        
######################

#Make GramGender a factor with two levels (feminine, masculine)
subset.for.plot$GramGender <- as.factor(subset.for.plot$GramGender)

#Contrast coding for modeling
contrasts(subset.for.plot$GramGender) <- contr.treatment(2,1)  #baseline = feminine

#The fitted model can be directly loaded
load("model-study2.RData")

#Or it can be run again using the following code
#The model uses the same parameter settings as in Study 1
model.study2 <- brm(
  formula = Gender ~ 1 + GramGender  + (GramGender|Participant) + (1|Item) + (1|Noun),
  data = subset.for.plot,
  family = cumulative("probit"), 
  iter = 4000,   #increase the number from default 2000 to 4000
  chains = 4,
  control = list(adapt_delta = 0.999999999, max_treedepth = 16), #adapt_delta increased to address the problem of divergent transitions, increase max_treedepth from 10 to 15 to address transitions after warmup that exceeded the maximum treedepth. 
  inits = 0, #to avoid the pb mentioned here: https://discourse.mc-stan.org/t/initialization-error-try-specifying-initial-values-reducing-ranges-of-constrained-values-or-reparameterizing-the-model/4401
  save_pars = save_pars(all = TRUE),
  set.seed(1702) #to be able to reproduce the analysis
  #, 
)
#save(model.study2, file = "model-study2.RData")

#Summary of the model
summary(model.study2)
#Grammatical Gender affects gender inferences (the effect is significant)

#Plot
conditional_effects(model.study2, categorical = TRUE)

###############
#Visualization#                                                                        
###############

#change name
datafile.for.model <- subset.for.plot

#Number of samples
length(posterior_samples(model.study2)[[1]])

#Construct a dataset with posterior samples and calculated posteriors for all cells in the factorial design. 
#There are 8000 samples in the regression, 2 cells in the factorial design.
#-> The dataframe must contain 8000 * 2= 16 000 rows
# 8000 samples for each cell. 
# the function expand.grid is provided by the package utils
post.data <- expand.grid(GramGender = levels(datafile.for.model$GramGender))

#number of sample
nsamples = 8000

#repeating each row nsamples times
post.data <- data.frame(post.data[rep(seq_len(nrow(post.data)), each = nsamples), ])
nrow(post.data)
head(post.data)

#rename column as GramGender
colnames(post.data)[colnames(post.data)=="post.data.rep.seq_len.nrow.post.data....each...nsamples...."] <- "GramGender"

#Add the posterior_samples values as a column in the dataframe. 
#Repeat it as many times as there are cell in the factorial design, namely 2. 
#factor estimates
post.data$b_GramGender2 <- rep(posterior_samples(model.study2)[['b_GramGender2']], 2)
#thresholds estimates
post.data$b_Intercept1 <- rep(posterior_samples(model.study2)[['b_Intercept[1]']], 2)
post.data$b_Intercept2 <- rep(posterior_samples(model.study2)[['b_Intercept[2]']], 2)
post.data$b_Intercept3 <- rep(posterior_samples(model.study2)[['b_Intercept[3]']], 2)
post.data$b_Intercept4 <- rep(posterior_samples(model.study2)[['b_Intercept[4]']], 2)
post.data$b_Intercept5 <- rep(posterior_samples(model.study2)[['b_Intercept[5]']], 2)
post.data$b_Intercept6 <- rep(posterior_samples(model.study2)[['b_Intercept[6]']], 2)

#Calculate for each cell/combination of predictor values the sum of the effects weighted by the contrast coding scheme.
#This assumes the following baseline level for GramGender: "feminine"
post.data$equation.predictors <- rep(0, nrow(post.data))
post.data$Resp1 <- rep(0, nrow(post.data))
post.data$Resp2 <- rep(0, nrow(post.data))
post.data$Resp3 <- rep(0, nrow(post.data))
post.data$Resp4 <- rep(0, nrow(post.data))
post.data$Resp5 <- rep(0, nrow(post.data))
post.data$Resp6 <- rep(0, nrow(post.data))
post.data$Resp7 <- rep(0, nrow(post.data))

#Look at the header of the dataframe
head(post.data)

#Loop
for (i in 1:nrow(post.data)){
  post.data$equation.predictors[i] <- post.data$b_GramGender2[i] * ifelse(post.data$GramGender[i]=="masculine", 1, 0)
  
  #cf Buerkner and Vuorre p 79 Equation (5)
  post.data$Resp1[i] <- pnorm(post.data$b_Intercept1[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp2[i] <- pnorm(post.data$b_Intercept2[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept1[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp3[i] <- pnorm(post.data$b_Intercept3[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept2[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp4[i] <- pnorm(post.data$b_Intercept4[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept3[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp5[i] <- pnorm(post.data$b_Intercept5[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept4[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp6[i] <- pnorm(post.data$b_Intercept6[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept5[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp7[i] <- 1 - pnorm(post.data$b_Intercept6[i] - post.data$equation.predictors[i], 0, 1)
}

########################################################################
#Plot the posterior using the 7-point Likert scale as response variable#
########################################################################

#take subset of the data with just the relevant columns
post.data2 <- subset(post.data, select = c(GramGender, Resp1, Resp2, Resp3, Resp4, Resp5, Resp6, Resp7))

#Put the data in long format to have a single Resp variable with 7 levels. 
post.data.long <- reshape2::melt(post.data2, id.vars=c("GramGender"))
head(post.data.long)
dim(post.data.long)

#Rename variable as Response and value as Probability
colnames(post.data.long)[colnames(post.data.long)=="variable"] <- "Response"
colnames(post.data.long)[colnames(post.data.long)=="value"] <- "Probability"

#Treat all variables as factors
post.data.long$GramGender <- as.factor(post.data.long$GramGender)

#Rename Resp1, etc. as 1, 2,3...
post.data.long$Response <- revalue(post.data.long$Response, c("Resp1"="1", "Resp2"="2",
                                                              "Resp3"="3", "Resp4"="4",
                                                              "Resp5"="5", "Resp6"="6",
                                                              "Resp7"="7"
))

#Summarize with ddply (cf. http://www.cookbook-r.com/Manipulating_data/Summarizing_data/) 
#Uses the function ci() from the bayestestR package to compute the credibility interval on the posterior samples. 
summary.posterior <- ddply(post.data.long, c("GramGender", "Response"), summarise,
                           N    = length(Probability),
                           mean = mean(Probability),
                           hdi_l95 = ci(Probability, method = "ETI", ci=0.95)[[2]],
                           hdi_u95 = ci(Probability, method = "ETI", ci=0.95)[[3]]
)

#Plot the posterior distribution for the 7 responses as a function of Grammatical Gender and Group
plot.ordinal.regression <- ggplot(summary.posterior, aes(x=GramGender, y=mean, color=Response)) +
  geom_point(aes(color=Response), size=3,position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin=hdi_l95, ymax=hdi_u95), width=.3, position = position_dodge(width = 0.9)) +
  geom_line() +
  theme_bw() + labs(y="Posterior probability", x="Grammatical gender") +
  scale_color_grey(name = "Inferred gender") +
  theme_classic(base_size = 20)

plot.ordinal.regression

#Save the plot as jpg figure
jpeg("plot-ordinal-regression-study2.jpeg", units="in", width=13, height=8, res=300)
plot.ordinal.regression
dev.off()

################################################################################
#Plot the posterior using the latent (continuous) variable as response variable#
################################################################################

#take subset of the data with just the relevant columns
post.data2 <- subset(post.data, select = c(GramGender, equation.predictors))
#Put the data in long format t have a single Resp variable with 7 levels. 
post.data.long <- reshape2::melt(post.data2, id.vars=c("GramGender"))
head(post.data.long)
dim(post.data.long)

#Treat all variables as factors
post.data.long$GramGender <- as.factor(post.data.long$GramGender)

#Summarise with ddply (cf. http://www.cookbook-r.com/Manipulating_data/Summarizing_data/) 
#Uses the function ci() from the bayestestR package to compute the credibility interval on the posterior samples. 
summary.posterior <- ddply(post.data.long, c("GramGender"), summarise,
                           N    = length(value),
                           mean = mean(value),
                           hdi_l95 = ci(value, method = "ETI", ci=0.95)[[2]],
                           hdi_u95 = ci(value, method = "ETI", ci=0.95)[[3]]
)

#Find the mean of the posterior distribution for Intercept3 and Intercept4
#These two values will delimit the range of values of the underlying continuous variable that correspond
#to an unbiased response (=4 on the 7-point Likert scale)
Intercept3 <- mean(post.data$b_Intercept3)
Intercept4 <- mean(post.data$b_Intercept4)

#Plot the posterior distribution of the latent continuous variable Inferred gender as a function of Grammatical gender
#Show the range of values for the variable Inferred gender that correspond to a unbiased response (=4)
post.data.long.subset <- subset(post.data.long, GramGender=="masculine")
head(post.data.long.subset)

study2.density.plot <- ggplot(post.data.long.subset, aes_string(x ="value")) + xlim(-2, 2) +
  geom_density(alpha=0.4, fill="lightgray") +
  geom_vline(xintercept=mean(post.data.long.subset$value), linetype="dashed", size=0.7, color="lightgray") +
  geom_vline(xintercept=0, linetype="dashed", size=0.7, color="gray24") +
  geom_vline(xintercept=Intercept3, linetype="solid", size=1.3) +
  geom_vline(xintercept=Intercept4, size=1.3) +
  scale_fill_grey() + scale_color_grey() + xlab("Inferred gender") + 
  ylab("Posterior density") +
  annotate(geom="text", x=-1.5, y=1.65, label="Male bias", size=20/.pt, fontface=2) + 
  annotate(geom="text", x=1.7, y=1.65, label="Female bias", size=20/.pt, fontface=2) + 
  annotate(geom="text", x=0.4, y=1.65, label="No bias", size=20/.pt, fontface=2) +
  annotate(geom="text", x=-0.1, y=1, label="feminine gender", angle='90', size=20/.pt, color="gray24") + 
  annotate(geom="text", x=-1.3, y=1, label="masculine gender", angle='90', size=20/.pt, color="lightgray") +
  theme_classic(base_size = 20)

#Save the plot
jpeg("study2-density-plot2.jpeg", units="in", width=9, height=6, res=300)
study2.density.plot 
dev.off()



