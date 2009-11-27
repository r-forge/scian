joincin = function(list)
{
	for(i in 2:length(list))
	{
		list[[1]] <- as.data.frame(rbind(list[[1]],list[[i]]))
	}
	return(list[[1]])
}