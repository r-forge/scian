joincin = function(list)
{
	out <- formatDF(list[[1]])
	for(i in 2:length(list))
	{
		out <- as.data.frame(rbind(out,formatDF(list[[i]])))
	}
	return(out)
}