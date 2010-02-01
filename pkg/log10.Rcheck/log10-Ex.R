### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx",
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
               outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           grid::pushViewport(grid::viewport(width=grid::unit(1, "npc") -
                              grid::unit(1, "lines"), x=0, just="left"))
           grid::grid.text(sprintf("help(\"%s\")", nameEx()),
                           x=grid::unit(1, "npc") + grid::unit(0.5, "lines"),
                           y=grid::unit(0.8, "npc"), rot=90,
                           gp=grid::gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
## at least one package changes these via ps.options(), so do this
## before loading the package.
## Use postscript as incomplete files may be viewable, unlike PDF.
## Choose a size that is close to on-screen devices, fix paper
grDevices::ps.options(width = 7, height = 7, paper = "a4", reset = TRUE)
grDevices::postscript("log10-Ex.ps")

assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
options(warn = 1)
library('log10')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("addlog")
### * addlog

flush(stderr()); flush(stdout())

### Name: addlog
### Title: Add elements on a decimal log x-y plot
### Aliases: addlog

### ** Examples
 
a <- seq(from=1,to=100,by=1)^2
logplot(a,log='y')
addlog(a+1e3,log='y')



cleanEx(); nameEx("hcp1")
### * hcp1

flush(stderr()); flush(stdout())

### Name: hcp1
### Title: High contrast palette 1
### Aliases: hcp1

### ** Examples
 
data(volcano)
logfill(volcano,log='',pal=hcp1)



cleanEx(); nameEx("hcp2")
### * hcp2

flush(stderr()); flush(stdout())

### Name: hcp2
### Title: High contrast palette 2
### Aliases: hcp2

### ** Examples
 
data(volcano)
logfill(volcano,log='',pal=hcp2)



cleanEx(); nameEx("hcp3")
### * hcp3

flush(stderr()); flush(stdout())

### Name: hcp3
### Title: High contrast palette 3
### Aliases: hcp3

### ** Examples
 
data(volcano)
logfill(volcano,log='',pal=hcp3)



cleanEx(); nameEx("logfill")
### * logfill

flush(stderr()); flush(stdout())

### Name: logfill
### Title: Decimal x-y filledcontour plot
### Aliases: logfill

### ** Examples
 
data(volcano)
par(mfcol=c(1,2),pty='s')
logfill(volcano)
logfill(volcano,log='')



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("loglm")
### * loglm

flush(stderr()); flush(stdout())

### Name: loglm
### Title: Add a linear model to a plot
### Aliases: loglm

### ** Examples
 
a <- seq(from=1,to=10,by=0.1)
b <- a + abs(rnorm(length(a),0,5))
c <- 2*a + abs(rnorm(length(a),0,5))
logplot(a,b,log='xy',pch=19,ylim=c(1,100))
addlog(a,c,log='xy',pch=19,col='grey')
model <- lm(b~a)
model.2 <- lm(c~a)
loglm(model,col='red',log='xy')
loglm(model.2,col='blue',log='xy',range=c(2,8),lty=3)



cleanEx(); nameEx("logplot")
### * logplot

flush(stderr()); flush(stdout())

### Name: logplot
### Title: Decimal x-y plot
### Aliases: logplot

### ** Examples
 
a <- seq(from=1,to=100,by=0.1)^2
logplot(a,log='y')



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
