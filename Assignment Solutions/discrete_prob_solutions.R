## Q1
nrow(permutations(8,3))
nrow(permutations(3,3))
nrow(permutations(3,3))/nrow(permutations(8,3))

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
winners<-replicate(10000, {
  top<-sample(runners,3,replace = FALSE)
  all(top %in% c("Jamaica", "Jamaica", "Jamaica"))
})

mean(winners)

## Q2
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(2,1))
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(3,1))
nrow(combinations(6,1))*nrow(combinations(6,3))*nrow(combinations(3,1))

ncombos <- function(entrees) {
  nrow(combinations(entrees,1))*nrow(combinations(6,2))*nrow(combinations(3,1))
}
sapply(1:12,ncombos)

nsides <- function(sides) {
  nrow(combinations(6,1))*nrow(combinations(sides,2))*nrow(combinations(3,1))
}
sapply(2:12,nsides)

## Q3
data("esoph")
head(esoph)
str(esoph)
names(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
levels(esoph$alcgp)
alc <- filter(esoph,alcgp=="0-39g/day")
(alc)
sum(alc$ncontrols)
sum(alc$ncases)/sum(alc$ncases+alc$ncontrols)

## Q4

levels(esoph$tobgp)
cancer <- filter(esoph,ncases>0)
head(cancer)
g10 <- filter(cancer,tobgp %in% c("10-19","20-29","30+"))
g10patients <- sum(g10$ncases)
cancerpatients <- sum(cancer$ncases)
g10patients/cancerpatients

control <- filter(esoph,ncontrols>0)
head(control)
g10 <- filter(control,tobgp %in% c("10-19","20-29","30+"))
g10patients <- sum(g10$ncontrols)
controlpatients <- sum(control$ncontrols)
g10patients/controlpatients

## Q5
a <- sum(filter(filter(esoph,ncases>0),alcgp=="120+")$ncases)/sum(filter(esoph,ncases>0)$ncases)
b <- sum(filter(filter(esoph,ncases>0),tobgp=="30+")$ncases)/sum(filter(esoph,ncases>0)$ncases)
c <- sum(filter(filter(esoph,ncases>0),alcgp=="120+"&tobgp=="30+")$ncases)/sum(filter(esoph,ncases>0)$ncases)
d <- a + b - c

## Q6
a6 <- sum(filter(filter(esoph,ncontrols>0),alcgp=="120+")$ncontrols)/sum(filter(esoph,ncontrols>0)$ncontrols)
b<-a/a6
c6 <- sum(filter(filter(esoph,ncontrols>0),tobgp=="30+")$ncontrols)/sum(filter(esoph,ncontrols>0)$ncontrols)
d6 <- sum(filter(filter(esoph,ncontrols>0),alcgp=="120+"&tobgp=="30+")$ncontrols)/sum(filter(esoph,ncontrols>0)$ncontrols)
e6 <- a6 + c6 - d6
f <- d/e6

