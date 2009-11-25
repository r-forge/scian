gCIN = function (data, col, row) 
{
    dat <- subset(data, (data[, 2] == col) & (data[, 3] == row))
    return(as.numeric(dat[4:(length(dat) - 1)]))
}