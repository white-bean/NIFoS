library(ggplot2)

df = read.csv('³«¿±¼Û_¼Ò.csv', header=T, encoding='utf-8')

## boxplot
ggplot(df, aes(`Á¤ÇÕÈ½¼ö`, `TreeHeight`))+geom_boxplot()
ggplot(df, aes(`Á¤ÇÕÈ½¼ö`, `DBH`))+geom_boxplot()

## ANOVA
t = aov(`TreeHeight`~factor(`Á¤ÇÕÈ½¼ö`), data=df)
summary(t)


# Bland-Altman plot
df = read.csv('³«¿±¼Û_¼Ò_paired.csv')
#df = df[!(df$DBH.29>130),]
data1 = df$TreeHeight.29
data2 = df$TreeHeight.Field

BA = function(data1, data2, df){
  n=length(data1)
  df$avg = (data1+data2)/2
  df$diff = data1-data2
  
  hist(df$diff)
  
  diff.mean = mean(df$diff)
  diff.sd = sd(df$diff)
  
  lower = diff.mean-1.96*diff.sd
  upper = diff.mean+1.96*diff.sd
  
  t=qt(0.975, n-1)
  
  # Æò±ÕÂ÷ÀÌ ½Å·Ú±¸°£
  m.upper = diff.mean+t*sqrt(diff.sd**2/n)
  m.lower = diff.mean-t*sqrt(diff.sd**2/n)
  
  # ÀÏÄ¡µµÇÑ°è ½Å·Ú±¸°£
  u.upper = upper+t*sqrt(3*diff.sd**2/n)
  u.lower = upper-t*sqrt(3*diff.sd**2/n)
  l.upper = lower+t*sqrt(3*diff.sd**2/n)
  l.lower = lower-t*sqrt(3*diff.sd**2/n)
  
  ggplot(df, aes(x = avg, y = diff)) +
    geom_point(size=2, shape=1) + 
    geom_hline(yintercept = diff.mean, size=1, color='black') +
    geom_hline(yintercept = m.upper, color = "blue", alpha=0.5) +
    geom_hline(yintercept = m.lower, color = "blue", alpha=0.5) +
    geom_hline(yintercept = lower, color = "red", linetype="dashed") +
    geom_hline(yintercept = upper, color = "red", linetype="dashed") +
    geom_hline(yintercept = u.upper, color = "orangered", alpha=0.5) +
    geom_hline(yintercept = u.lower, color = "orangered", alpha=0.5) +
    geom_hline(yintercept = l.upper, color = "orangered", alpha=0.5) +
    geom_hline(yintercept = l.lower, color = "orangered", alpha=0.5) +
    geom_hline(yintercept = 0, color = "black", linetype="dashed", alpha=0.5) +
    geom_ribbon(aes(ymin=m.lower, ymax=m.upper),alpha=0.2)+
    geom_ribbon(aes(ymin=u.lower, ymax=u.upper),alpha=0.2)+
    geom_ribbon(aes(ymin=l.lower, ymax=l.upper),alpha=0.2)+
    
    ggtitle("³«¿±¼Û_¼Ò_TreeHeight between 29 and Fieldsurvey") +
    ylab("difference between 29, Fieldsurvey") +
    xlab("Average between 29, Fieldsurvey") +
    
    geom_text(aes(x=max(avg)-0.2,y=diff.mean+0.4,label="MEAN"),col="black")+ 
    geom_text(aes(x=max(avg)-0.2,y=upper+0.4),label="+1.96s",col="red")+ 
    geom_text(aes(x=max(avg)-0.2,y=lower+0.4),label="-1.96s",col="red")+
    
    geom_text(aes(x=max(avg)-0.1,y=diff.mean-0.4,label=sprintf("%0.1f", round(diff.mean, digits = 1))),col="black")+ 
    geom_text(aes(x=max(avg)-0.1,y=upper-0.4),label=sprintf("%0.1f", round(upper, digits = 1)),col="red")+ 
    geom_text(aes(x=max(avg)-0.1,y=lower-0.4),label=sprintf("%0.1f", round(lower, digits = 1)),col="red")+
    
    theme_classic()
}

BA(data1, data2, df)




# scatter plot
plot(data1, data2)
