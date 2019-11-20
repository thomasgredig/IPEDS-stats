# IPEDS data: explore tuition
# (c) 2019 Thomas Gredig
#############################
library(ggplot2)
library(reshape)
path.DATA = 'data/'
path.FIGS = 'images/'
file.PREFIX = 'tuition'

# load the data
dir(path.DATA, pattern='csv$')
d = read.csv(file.path(path.DATA,"ic2018_ay.csv"), stringsAsFactors = FALSE)
q = which(substr(names(d),1,1)!='X')
d[,q] = sapply(d[,q],as.numeric)

str(d)

ggplot(d, aes(x=TUITION2/1e3)) + 
  geom_histogram(aes(y=..density..), 
                 colour="black", fill="orange", bins=100) +
  geom_density(alpha=0.2, size = 1.5,fill='grey') +
  scale_x_continuous(limits=c(0,70), breaks=0:7*10) + 
  xlab('In-state average tuition for full-time undergraduates (k$)') +
  theme_bw()
ggsave(file.path(path.FIGS,paste0(file.PREFIX,'-inState.png')),
       width=5, height=4, dpi=150)



ggplot(d, aes(x=TUITION3)) + 
  geom_histogram(aes(y=..density..), 
                 colour="black", fill="white", bins=50) +
  geom_density(alpha=0.2, fill='red') +
  scale_x_continuous(limits=c(0,70000)) + 
  xlab('Out-of-state average tuition for full-time undergraduates ($)') +
  theme_bw()

ggplot(d, aes(x=TUITION6)) + 
  geom_histogram(aes(y=..density..), 
                 colour="black", fill="white", bins=50) +
  geom_density(alpha=0.2, fill='red') +
  scale_x_continuous(limits=c(0,70000)) + 
  xlab('In-state average tuition full-time graduates ($)') +
  theme_bw()


ggplot(d, aes(x=TUITION7)) + 
  geom_histogram(aes(y=..density..), 
                 colour="black", fill="white", bins=50) +
  geom_density(alpha=0.2, fill='red') +
  scale_x_continuous(limits=c(0,70000)) + 
  xlab('Out-of-state average tuition full-time graduates ($)') +
  theme_bw()


w = data.frame(
  d$UNITID,
  UG = (d$TUITION3 -  d$TUITION2) / d$TUITION2,
  G = (d$TUITION7 - d$TUITION6) / d$TUITION6
)

w1 = subset(w, UG>0.1 & G>0.1)
names(w1)
w2 = melt(w1, id = c('d.UNITID'))
head(w2)
ggplot(w2, aes(x=value, color=variable, fill=variable)) + 
  geom_density(alpha=0.2, size=2) +
  xlab('ratio') +
  theme_bw()
