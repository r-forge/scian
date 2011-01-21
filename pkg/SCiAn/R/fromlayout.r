fromlayout = function (layout, data) 
{

	data<-layout[,1][match(names(data),layout[,2])]
	OK<-!is.na(Nam)
	out<-as.data.frame(cbind(Cond=Nam[OK],Val=data[OK]))
    results <- lapply(split(out$Val,out$Cond),function(x)as.numeric(as.vector(x)))
    return(results)
}