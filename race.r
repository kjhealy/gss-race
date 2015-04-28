###--------------------------------------------------
### gss 2000 race q by race
###--------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)

theme_set(theme_minimal())

actual.df <- data.frame(Respondent.Race="Actual Value", Guess=c(75.1, 12.3, 12.5, 3.6, 1.8),
                        Group=c("White", "Black", "Hispanic", "Asian", "Jewish"))

data <- read.csv("data/gss-race-pct2.csv", header=TRUE)
data.m <- gather(data, Group, Guess, White:Asian)
data.m$Group <- factor(data.m$Group, levels=c("White", "Black", "Hispanic", "Asian", "Jewish"), ordered=TRUE)

data.m$Guess <- as.numeric(data.m$Guess)
cc <- complete.cases(data.m)
data.cc <- data.m[cc,]

data.sum <- data.cc %>% group_by(Respondent.Race, Group) %>% summarize(Guess=median(Guess))
data.all <- rbind(data.sum, actual.df)

p <- ggplot(subset(data.all, Respondent.Race!="Other"), aes(x=Group, y=Guess, color=Respondent.Race, shape=Respondent.Race))

jit <- position_jitter(width=0.1, height=0)
pdf(file="figures/gss-group-pctby-race.pdf", height=4, width=7)
p1 <- p + geom_jitter(size=4, position=jit) +
        coord_flip() + theme(legend.position="top") +
            labs(color="Race of Respondent", shape="Race of Respondent", x="", y="Median Guess (Percent)\n") + scale_color_manual(values=c(my.colors("bly")[1:2], "black")) + ggtitle("What Percentage of the US Population is ...?")
print(p1)
credit("Data: General Social Survey 2000 for respondents, US Census 2000 for actual values.")
dev.off()

ggsave("figures/gss-group-pctby-race.png", p1, width = 7, height = 4, dpi = 300)
