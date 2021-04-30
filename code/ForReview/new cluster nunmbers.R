kfreq %>% group_by(kFreq) %>% count()
# 1  4428
# 2     2  1127
# 3     3  9878
# 4     4  9774

kfreq %>% group_by(kFreq,gage)%>% unique() %>% count()

c1 = kfreq[kfreq$kFreq==1,'gage']
length(unique(c1))
#437 
c2 = kfreq[kfreq$kFreq==2,'gage']
length(unique(c2))
#246
c3 = kfreq[kfreq$kFreq==3,'gage']
length(unique(c3))
#812
c4 = kfreq[kfreq$kFreq==4,'gage']
length(unique(c4))
#668

