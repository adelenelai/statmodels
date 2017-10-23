marg <- function(dat){

marginals<- dat[, c("DT50", "Silt", "Clay", "Temp", "pHCaCl2", "Moisture", "OrgCont",  "cec", "Biomass_start")] 
    #breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
    #bwidth <- breaks[2]-breaks[1] from here https://stackoverflow.com/questions/14200027/how-to-adjust-binwidth-in-ggplot2
ggplot(melt(marginals), aes(x=value)) + 
geom_histogram(bins=20) + 
facet_wrap(~ variable, scales="free", ncol=3) + 
ggtitle("Marginal Distributions") + 
labs(x="Variable", y="Frequency") + 
theme(plot.title = element_text(hjust=0.5)) 
}