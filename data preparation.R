library(ggplot2)

#setwd("/\\myhome.itap.purdue.edu/puhome/pu.data/Desktop/Case Study")
#setwd('C:/Users/Tony/SkyDrive/Documents/Actuary/SOA/Case Study/2018/R')
setwd('/Users/cherish/Desktop/2018SOA')
ind <- read.table('ind.csv', header = T, sep = ',')

# remove empty record
# ind <- ind[!is.na(ind$Age) & !is.na(ind$Gender) & !is.na(ind$Credits) & !is.na(ind$Earnings) 
#     & !is.na(ind$Health), ]
# rownames(ind) <- 1:nrow(ind)
# write.csv(ind, file="nonemp_ind.csv")

ind$Earnings[which(is.na(ind$Earnings[1:20000]))] <- 0
ind <- ind[!is.na(ind$Age),]
rownames(ind) <- 1:nrow(ind)

# coerce gender to F and M
ind$Gender <- ifelse(ind$Gender == 1, 'F', 'M')

# replace care level NA by 0
ind$Care.Level[is.na(ind$Care.Level)] <- 0
table(ind$Care.Level)

write.csv(ind, file="nonemp_ind.csv")

# coerce variables to factors
cols <- c('Gender', 'Health', 'Care.Level', 'child')
ind[cols] <- lapply(ind[cols], factor)

# age statistics
pop.sample <- c(sum(ind$Age>80)/nrow(ind), sum(ind$Age>=65)/nrow(ind), sum(ind$Age<65)/nrow(ind)
                , sum(ind$Age<=40 & ind$Age>=20)/nrow(ind), sum(ind$Age<20)/nrow(ind))
names(pop.sample) <- c("Age>80", "Age>=65", "Age<65", "20<=Age<=40", "Age<20")
round(pop.sample,3)

# care statistics
#Total percentage
table(ind$Health) / nrow(ind)
#1:Receiving Home Health Care through Social LTC Program
table(ind$Care.Level[ind$Health==1])
#2:Receiving Facility Health Care through Social LTC Program
table(ind$Care.Level[ind$Health==2])

# population pyramid
source('pyramid.R')
pyramid(ind, 1, 2)

# Age vs Health
ggplot(ind[ind$Age>=65,], aes(x=Health, y=Age)) + 
  geom_boxplot(alpha=0.5, fill='blue') +
  labs(title='Age vs. Health',
       subtitle='by Gender') +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

ggplot(ind[ind$Age>=65,], aes(x=Health, y=Age, fill=Gender)) + 
  geom_boxplot(alpha=0.5) +
  labs(title='Age vs. Health',
       subtitle='by Gender') +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))


ggplot(ind[ind$Age>=65&ind$Health==2,], aes(x=Care.Level, y=Age, fill=Gender)) + 
  geom_boxplot(alpha=0.5) +
  labs(title='Age vs. Care Level',
       xlab='Care Level') +
  theme(plot.title = element_text(hjust = 0.5))

# Age distribution for those with child
ggplot(ind[which(ind$child==1),], aes(Age)) +
  geom_histogram(alpha=0.5, binwidth = 2, fill='blue', color='black') +
  scale_x_continuous(breaks=seq(15,60,5),labels=abs(seq(15,60,5))) +
  labs(title='Histogram for Age with child') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ind[!is.na(ind$child),], aes(Age, fill=child)) +
  geom_histogram(alpha=0.5, binwidth = 5, color='black') +
  scale_x_continuous(breaks=seq(15,90,5),labels=abs(seq(15,90,5))) +
  labs(title='Histogram for Age with child') +
  theme(plot.title = element_text(hjust = 0.5))

# income distribution
Income <- subset(ind, !is.na(ind$Earnings))
ggplot(Income, aes(Earnings)) + 
  geom_histogram(alpha=0.5, bins=100, color='black')  +
  labs(title='Histogram for Income', xlab='Income') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Income, aes(Earnings, fill=Gender)) + 
  stat_density(adjust=1/2, size=0.5, alpha=0.6, position='identity') +
  labs(title='Density for Income by Gender', xlab='Income') +
  theme(plot.title = element_text(hjust = 0.5))

# Income vs Age
# Age < 65
ggplot(Income[Income$Age<65,], aes(x=Age, y=Earnings)) +
  geom_point(alpha=0.5, color='blue') +
  geom_line(stat= "smooth", method=lm, size=1, alpha= 0.7) +
  labs(title='Income vs. Age',
       subtitle='for younger than 65 years',
       y='Income') +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

# Age >= 65
ggplot(Income[Income$Age>=65,], aes(x=Age, y=Earnings)) +
  geom_point(alpha=0.5, color='blue') +
  geom_line(stat= "smooth", method=lm, size=1, alpha= 0.7) +
  labs(title='Income vs. Age',
       subtitle='for 65 years or older',
       y='Income') +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

# Income vs Health by Gender
ggplot(Income, aes(x=Health, y=Earnings, fill=Gender)) +
  geom_boxplot() +
  labs(title='Income vs. Health',
       subtitle='by Gender',
       y='Income') +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

# Income vs Care Level
ggplot(Income[Income$Health==2,], aes(x=Care.Level, y=Earnings,fill=Gender)) +
  geom_boxplot(alpha=0.5) +
  labs(title='Income vs. Health',
       y='Income',
       x='Care Level') +
  theme(plot.title = element_text(hjust = 0.5))

# care level distribution
# home
ggplot(ind[which(ind$Health==1),], aes(factor(1), fill=Care.Level))+
  geom_bar(width = 1)+
  coord_polar("y") +
  theme_void() +
  labs(title = 'Home Care Level Distribution',fill = 'Care Level')
# facility
ggplot(ind[which(ind$Health==2),], aes(factor(1), fill=Care.Level))+
  geom_bar(width = 1)+
  coord_polar("y") +
  theme_void() +
  labs(title = 'Facility Care Level Distribution',fill = 'Care Level')

