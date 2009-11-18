textimage <- function(m,...)
{
	image(c(1:nrow(m)),c(1:ncol(m)),m,...)
	for(c in 1:ncol(m))
	{
		for(r in 1:nrow(m))
		{
			text(r,c,round(m[r,c],2))
		}
	}
}