# x = rev(rep(6:10, 1:5))
x = c(1,10,10,10,1,2,1,1,1,10)
rle_x = rle(x)
# rle_x = rle_x[which(rle_x$values>8)]

# Compute endpoints of run
end = cumsum(rle_x$lengths)
start = c(1, lag(end)[-1] + 1)

# Display results
data.frame(start, end)

tt = start[which(rle_x$values==10)]


x[tt]
###################################################
nf_start = rle(data$X_00060_00003)

end = cumsum(nf_start$lengths)
start = c(1, lag(end)[-1] + 1)

nf_start = start[which(nf_start$values==0)]


data$X_00060_00003[nf_start]
data$Date[nf_start]


