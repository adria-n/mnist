# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer', n= ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('train-images.idx3-ubyte')
  test <<- load_image_file('t10k-images.idx3-ubyte')
  
  train$y <<- load_label_file('train-labels.idx1-ubyte')
  test$y <<- load_label_file('t10k-labels.idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

load_mnist()

trainx <- train$x[1:10000,]
trainy <- train$y[1:10000]
for (i in 1:10000){
  for(k in 1:784){
    if(trainx[i,k]/255 > 0.5){
      trainx[i,k] <- 1
    }
    else if (trainx[i,k]/255 < 0.5){
      trainx[i,k] <- 0
    }
  }
}

playsetx <- trainx[1:100,]
playsety <- trainy[1:100]

playsety <- playsety + 1

playset <- list(playsetx, playsety)


sum(playsetx[playsety == 1,i] == 1)


pixelcount1 <- matrix(0, nrow = 784, ncol = 10)
pixelcount0 <- matrix(0, nrow = 784, ncol = 10)
for (d in 1:784){
  for (c in 1:10){
    pixelcount1[d,c] <- sum(playsetx[playsety ==c,d] == 1)
    pixelcount0[d,c] <- sum(playsetx[playsety ==c,d] == 0)
  }
}

x <- t(pixelcount1)
y <- t(pixelcount0)


thetamap <- (x + 1)/(y + x + 2)
image(thetamap*255)
thetamap0 <- pixelcount1/(pixelcount0 + pixelcount1)

for(i in 1:10){
  message('image(t(matrix(rev(thetamap[ ,',i, ']), nrow = 28, ncol = 28)))')
}
imagematrix <- list()
for(i in 1:10){
  imagematrix[[i]] <- matrix(thetamap[i,], nrow = 28, ncol = 28)
}

t <- imagematrix[[7]]
image(t)

image(t)

t2 <- apply(t, 2, rev)



