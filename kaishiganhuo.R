load("~/Dropbox/Data and Codebook/tm4.RData")

names(tm4)

head(tolower(tm4$venue_state))

# convert the name of the state to be lower case
names(tm4)[305:354] = tolower(names(tm4)[305:354])

statName = tolower(tm4$venue_state)[1]
tm4[colnames(tm4)==statName]

tm4_frac = sample_frac(tm4, 0.01)
tm4_frac = as.data.frame(tm4_frac)

tm4[is.na(tm4)] = 0

# === 1
sameStat10 = c()
rnum = dim(tm4)[1]
for (i in 1:rnum) {
  statName = tolower(tm4$venue_state[i])
  colInd = which(colnames(tm4)==statName)
  if (length(colInd) != 0) {
    if(tm4[i, colInd] == 1){
      sameStat10 = c(sameStat10, 1)
    } else {
      sameStat10 = c(sameStat10, 0)
    }
  } else {
    sameStat10 = c(sameStat10, 0)
  }
}
sameStat10

# between 0.9 and 1
sameStat9 = c()
rnum = dim(tm4)[1]
for (i in 1:rnum) {
  statName = tolower(tm4$venue_state[i])
  colInd = which(colnames(tm4)==statName)
  if (length(colInd) != 0) {
    if(tm4[i, colInd] >= 0.9 & tm4[i, colInd] < 1){
      sameStat9 = c(sameStat9, 1)
    } else {
      sameStat9 = c(sameStat9, 0)
    }
  } else {
    sameStat9 = c(sameStat9, 0)
  }
}
sameStat9

# between 0.7 and 0.9
sameStat79 = c()
rnum = dim(tm4)[1]
for (i in 1:rnum) {
  statName = tolower(tm4$venue_state[i])
  colInd = which(colnames(tm4)==statName)
  if (length(colInd) != 0) {
    if(tm4[i, colInd] >= 0.9 & tm4[i, colInd] < 0.9){
      sameStat79 = c(sameStat79, 1)
    } else {
      sameStat79 = c(sameStat79, 0)
    }
  } else {
    sameStat79 = c(sameStat79, 0)
  }
}
sameStat79

# between 0.5 and 0.7
sameStat57 = c()
rnum = dim(tm4)[1]
for (i in 1:rnum) {
  statName = tolower(tm4$venue_state[i])
  colInd = which(colnames(tm4)==statName)
  if (length(colInd) != 0) {
    if(tm4[i, colInd] >= 0.5 & tm4[i, colInd] < 0.7){
      sameStat57 = c(sameStat57, 1)
    } else {
      sameStat57 = c(sameStat57, 0)
    }
  } else {
    sameStat57 = c(sameStat57, 0)
  }
}
sum(sameStat57)

# between 0.3 and 0.5
sameStat35 = c()
rnum = dim(tm4)[1]
for (i in 1:rnum) {
  statName = tolower(tm4$venue_state[i])
  colInd = which(colnames(tm4)==statName)
  if (length(colInd) != 0) {
    if(tm4[i, colInd] >= 0.3 & tm4[i, colInd] < 0.5){
      sameStat35 = c(sameStat35, 1)
    } else {
      sameStat35 = c(sameStat35, 0)
    }
  } else {
    sameStat35 = c(sameStat35, 0)
  }
}
sum(sameStat35)


# between 0.3 and 0.5
sameStat13 = c()
rnum = dim(tm4)[1]
for (i in 1:rnum) {
  statName = tolower(tm4$venue_state[i])
  colInd = which(colnames(tm4)==statName)
  if (length(colInd) != 0) {
    if(tm4[i, colInd] >= 0.1 & tm4[i, colInd] < 0.3){
      sameStat13 = c(sameStat13, 1)
    } else {
      sameStat13 = c(sameStat13, 0)
    }
  } else {
    sameStat13 = c(sameStat13, 0)
  }
}
sum(sameStat13)


# <0.1
sameStat1 = c()
rnum = dim(tm4)[1]
for (i in 1:rnum) {
  statName = tolower(tm4$venue_state[i])
  colInd = which(colnames(tm4)==statName)
  if (length(colInd) != 0) {
    if(tm4[i, colInd] < 0.1){
      sameStat1 = c(sameStat1, 1)
    } else {
      sameStat1 = c(sameStat1, 0)
    }
  } else {
    sameStat1 = c(sameStat1, 0)
  }
}
sum(sameStat1)

tm4New = cbind(tm4, sameStat10, sameStat9, sameStat79, sameStat57, sameStat35, sameStat13, sameStat1)

### price mean for the diff seg
mean(tm4New[tm4New$sameStat10 == 1, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat9 == 1, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat79 == 1, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat57 == 1, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat35 == 1, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat13 == 1, ]$price_mean, na.rm = TRUE)


### max for the diff seg
max(tm4New[tm4New$sameStat10 == 1, ]$price_mean)
max(tm4New[tm4New$sameStat9 == 1, ]$price_mean, na.rm = TRUE)
max(tm4New[tm4New$sameStat79 == 1, ]$price_mean, na.rm = TRUE)
max(tm4New[tm4New$sameStat57 == 1, ]$price_mean, na.rm = TRUE)
max(tm4New[tm4New$sameStat35 == 1, ]$price_mean, na.rm = TRUE)
max(tm4New[tm4New$sameStat13 == 1, ]$price_mean, na.rm = TRUE)

### min for the diff seg
sum(tm4New[tm4New$sameStat10 == 1, ]$quantity_sum, na.rm = TRUE)
sum(tm4New[tm4New$sameStat9 == 1, ]$quantity_sum, na.rm = TRUE)
sum(tm4New[tm4New$sameStat79 == 1, ]$quantity_sum, na.rm = TRUE)
sum(tm4New[tm4New$sameStat57 == 1, ]$quantity_sum, na.rm = TRUE)
sum(tm4New[tm4New$sameStat35 == 1, ]$quantity_sum, na.rm = TRUE)
sum(tm4New[tm4New$sameStat13 == 1, ]$quantity_sum, na.rm = TRUE)

### min for the diff seg
mean(tm4New[tm4New$sameStat10 == 1, ]$quantity_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat9 == 1, ]$quantity_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat79 == 1, ]$quantity_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat57 == 1, ]$quantity_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat35 == 1, ]$quantity_mean, na.rm = TRUE)
mean(tm4New[tm4New$sameStat13 == 1, ]$quantity_mean, na.rm = TRUE)



### price mean device
mean(tm4New[tm4New$desktop == 1, ]$price_mean)
mean(tm4New[tm4New$mobile == 1, ]$price_mean)
mean(tm4New[tm4New$tablet == 1, ]$price_mean)

# apple
mean(tm4New[tm4New$Android != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$Apple  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$BlackBerry  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$Nokia  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$OPPO  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$Samsung  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$Sharp  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$Smartbook  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$ZTE  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$Vertu  != 0, ]$price_mean, na.rm = TRUE)




mean(tm4New$Android * tm4New$price_mean, na.rm = TRUE)
mean(tm4New$Apple * tm4New$price_mean, na.rm = TRUE)

mean(tm4New$BlackBerry * tm4New$price_mean, na.rm = TRUE)
mean(tm4New$Nokia.y * tm4New$price_mean, na.rm = TRUE)

mean(tm4New$Vertu* tm4New$price_mean, na.rm = TRUE)


mean(tm4New[tm4New$"Chrome OS"  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$"Firefox OS"  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$"Linux"  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$"Macintosh"  != 0, ]$price_mean, na.rm = TRUE)
mean(tm4New[tm4New$"Windows"  != 0, ]$price_mean, na.rm = TRUE)

mean(tm4New$Linux * tm4New$price_mean, na.rm = TRUE)
mean(tm4New$"Macintosh" * tm4New$price_mean, na.rm = TRUE)
mean(tm4New$"Windows" * tm4New$price_mean, na.rm = TRUE)












