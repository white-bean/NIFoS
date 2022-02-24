library(ggplot2)

# [1] "TreeID"        "TreeHeight.9"  "DBH.9"         "TreeHeight.13" "DBH.13"        "TreeHeight.17" "DBH.17"       
# [8] "TreeHeight.29" "DBH.29"  

df = read.csv('C:/Users/user/Desktop/¼ÒÁß¹Ð/paired/Àã³ª¹«_Áß/Àã³ª¹«_Áß_paired.csv')
#df = df[!(df$DBH.29>130),]

#plot(data1, data2)

BA = function(a, b, type, df){
  data1 = df[,paste0(type, '.', a)]
  data2 = df[,paste0(type, '.', b)]  

  n=length(data1)
  df$avg = (data1+data2)/2
  df$diff = data1-data2
  
  hist(df$diff)
  
  diff.mean = mean(df$diff, na.rm=T)
  diff.sd = sd(df$diff, na.rm=T)
  
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
    geom_point(size=2) + 
    geom_hline(yintercept = diff.mean, size=1, color='black') +
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_hline(yintercept = lower, color = "red", linetype="dashed") +
    geom_hline(yintercept = upper, color = "red", linetype="dashed") +

    
    geom_line(aes(y=m.lower), colour="black") +
    geom_line(aes(y=m.upper), colour="black") +
    geom_line(aes(y=u.lower), colour="black") +
    geom_line(aes(y=u.upper), colour="black") +
    geom_line(aes(y=l.lower), colour="black") +
    geom_line(aes(y=l.upper), colour="black") +
    
    geom_ribbon(aes(ymin=m.lower, ymax=m.upper),alpha=0.1)+
    geom_ribbon(aes(ymin=u.lower, ymax=u.upper),alpha=0.1)+
    geom_ribbon(aes(ymin=l.lower, ymax=l.upper),alpha=0.1)+
    
    ggtitle(paste0("³«¿±¼Û_Áß_", type, " between ", a, " and ", b)) +
    ylab(paste0("differeces between ", a, ', ',b)) +
    xlab(paste0("Average between ", a, ', ',b)) +
    
    geom_text(aes(x=max(avg, na.rm=T)+0.5,y=diff.mean+0.3,label="MEAN"),col="black")+ 
    geom_text(aes(x=max(avg, na.rm=T)+0.5,y=upper+0.3),label="+1.96s",col="red")+ 
    geom_text(aes(x=max(avg, na.rm=T)+0.5,y=lower+0.3),label="-1.96s",col="red")+

    geom_text(aes(x=max(avg, na.rm=T)+0.6,y=diff.mean-0.3,label=sprintf("%0.1f", round(diff.mean, digits = 1))),col="black")+ 
    geom_text(aes(x=max(avg, na.rm=T)+0.6,y=upper-0.3),label=sprintf("%0.1f", round(upper, digits = 1)),col="red")+ 
    geom_text(aes(x=max(avg, na.rm=T)+0.6,y=lower-0.3),label=sprintf("%0.1f", round(lower, digits = 1)),col="red")+
    
    theme_classic()+
    
    scale_y_continuous(limits = c(-6.1, 6.1))
}
BA(alist[2], alist[5], type[1], df)

alist = list('9', '13', '17', '29', 'Field')
type = list('TreeHeight', 'DBH')


data1 = df[,paste0(type[1], '.', alist[1])]
data2 = df[,paste0(type[1], '.', alist[2])]


## ICC
library(irr)

for(i in 1:2){
  for(j in 1:4){
    t1 = paste0(type[i], '.', alist[j])
    for(k in (j+1):5){
      t2 = paste0(type[i], '.', alist[k])
      print(paste0(t1, t2))
      print(icc(df[, c(t1, t2)]))
    }
  }
}

