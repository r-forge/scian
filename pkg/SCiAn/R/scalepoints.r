scalepoints <- function(points,low=0,up=1)
{
	new.points <- points - min(points)
	new.points <- new.points / max(new.points)
	new.points <- new.points * (up-low)
	new.points <- new.points + low
	return(new.points)
}
