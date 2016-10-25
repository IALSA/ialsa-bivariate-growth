# pair <-  "grip_lmimmed"  
# ds <- results
kb_profile_prep <- function(ds){  
# ds <- ds[ds$outcome_pair==pair, ] #subset an outcome pair
# compose groupings for melting
physical_intercept <-  p_GAMMA_00  
cognitive_intercept <- p_GAMMA_10   
physical_slope <-  c_GAMMA_00
cognitive_slope <- c_GAMMA_10

select_components <- c(physical_intercept,physical_slope, cognitive_intercept, cognitive_slope)
# subset
if( sum(is.na(ds$Error))>0L ){
  d <- ds[is.na(ds$Error), ] # 
}else{
  d <- ds
}
d <- d[ , c("wave_count", "subgroup","physical_measure","cognitive_measure","converged", "trust_all", select_components)]
d <- d %>% tidyr::gather_("parameter","value", select_components)
# d <- d %>% tidyr::gather_("") # gather into measure: 
d$parameter <- as.character(d$parameter)
d <- d[order(d$wave_count), ]
head(d)

for(i in seq_along(d$wave_count)){
  d[i, "process"] <- strsplit(d[i,"parameter"], split="_")[[1]][1]
  d[i, "term"] <- as.character(strsplit(d[i,"parameter"], split="_")[[1]][3])
  d[i, "record"] <- strsplit(d[i,"parameter"], split="_")[[1]][4]
  d[i, "parameter"] <- strsplit(d[i,"parameter"], split="_")[[1]][2]
} # close fore loop
head(d)
table(d$term)

d$process <- plyr::revalue(d$process, c("p"="physical", "c"="cognitive"))
d$term <- plyr::revalue(d$term, c("00"="intercept", "10"="slope"))
str(d)
d$processF <- factor(d$process, levels=c("physical","cognitive"), labels=c("Physical","Cognitive"))
d$termF <- factor(d$term, levels=c("intercept","slope"), labels=c("Initial status","Rate of Change"))
d$subgroupF <- factor(d$subgroup, levels=c("male","female"), labels=c("Men","Women"))
str(d)

d <- tidyr::spread(d,record,value)
d$display <- paste0(round(d$est,1)," (",round(d$se,2) ,")")
# d$Coefficient <- d$est
d$Waves <- d$wave_count
# d$Pvalue <- d$pval
head(d)


# d$covar_covered <- factor(d$covar_covered)
head(d)
return(d)
} # close dual_grid_prep
# dual_grid_prep(ds)

kb_profile_graph <- function(ds, term, vertical, horizontal, display_value1, display_value2, border ){
# dual_grid_graph <- function(ds, term="slope", vertical="wave_count" , horizontal="est" ,  display_value1="est" , border=5 ){
  d <- kb_profile_prep(ds)
  d <- d[d$term==term,] 
  processP <- unique(d$physical_measure)
  processC <- unique(d$cognitive_measure)
  max_scale <- max(d[[horizontal]])
  min_scale <- min(d[[horizontal]])
  step_scale <- abs(max_scale - min_scale)/10
  # border <- 10
  top  <- max_scale + (step_scale*border)
  bottom <- min_scale - (step_scale*border)
#   if(min_scale < 0){
#   top_limit <- bottom
#   bottom_limit <- top
#   }else{
  top_limit <- top
  bottom_limit <- bottom
#   }
  
  g <- ggplot2::ggplot(d,aes_string( x = horizontal, y = vertical, color="trust_all"))
  g <- g + ggtitle(  paste0( processP, " <-> ", processC, " ( ", term, " )" )   )
  # g <- g + geom_point()
  g <- g + geom_text(aes_string(label=display_value1), angle=0, hjust=1.3, 
                     # color="black", 
                     alpha = .8, size=baseSize-7)
  g <- g + geom_text(aes_string(label=display_value2), angle=0, hjust=-0.2, 
                     # color="black", 
                     alpha = .5, size=baseSize-8)
  g <- g + scale_color_manual(name="Trust all parameters", values=c("TRUE"="black","FALSE"="#e41a1c"))
  # g <- g + facet_grid(processF ~ subgroupF
  g <- g + facet_grid(subgroupF ~ processF)
  g <- g + geom_path(color="#984ea3", size=.8)
  g <- g + scale_x_continuous(limits=c(bottom_limit,top_limit ))
  g <- g + scale_y_continuous(limits=c(4,17), breaks=seq(4,16,by=2))
 
  # g <- g + theme(legend.position=c(0,0))
  # g <- g + coord_flip()
  g <- g + main_theme
  g <- g + theme(legend.position="bottom")
return(g)
} # close function
# 
# g <- kb_profile_graph(ds, term="slope", vertical="wave_count", horizontal="pval", 
#                        display_value1="pval" , display_value2="wald", border=5)
# g <- g + geom_vline(xintercept=0, color="firebrick3",linetype="dashed", size=.5, alpha=.5 )
# g


vpLayout <- function(rowIndex, columnIndex) { return( viewport(layout.pos.row=rowIndex, layout.pos.col=columnIndex) ) }

kb_profiles <- function(ds, vertical="Waves",  border=5){
  d <- kb_profile_prep(ds)
  # d <- d[d$term==term,] 
  processP <- unique(d$physical_measure)
  processC <- unique(d$cognitive_measure)
  
#   a <- kb_profile_graph(ds, term="intercept", vertical=vertical, horizontal="est", 
#                        display_value1="pval" , display_value2="wald", border=border)
#   b <- kb_profile_graph(ds, term="slope", vertical=vertical, horizontal="est",
#                        display_value1="pval", display_value2="wald", border=border) 
#   c <- kb_profile_graph(ds, term="intercept", vertical=vertical, horizontal="pval", 
#                        display_value1="est", display_value2="se", border=border)
#   d <- kb_profile_graph(ds, term="slope", vertical=vertical, horizontal="pval", 
#                        display_value1="est", display_value2="se", border=border)
#   a <- a + xlab("P-value") + ylab("Waves analyzed")
#   b <- b + xlab("P-value") + ylab("Waves analyzed")
#   c <- c + xlab("Point estimate") + ylab("Waves analyzed")
#   d <- d + xlab("Point estimate") + ylab("Waves analyzed")
  
  a <- kb_profile_graph(ds, term="intercept", vertical=vertical, horizontal="est", 
                       display_value1="est" , display_value2="se", border=border)
  b <- kb_profile_graph(ds, term="slope", vertical=vertical, horizontal="est",
                       display_value1="est", display_value2="se", border=border) 
  c <- kb_profile_graph(ds, term="intercept", vertical=vertical, horizontal="pval", 
                       display_value1="pval", display_value2="wald", border=border)
  d <- kb_profile_graph(ds, term="slope", vertical=vertical, horizontal="pval", 
                       display_value1="pval", display_value2="wald", border=border)
  a <- a + xlab("Point Estimate | Standard Error") + ylab("Waves analyzed") 
  b <- b + xlab("Point Estimate | Standard Error") + ylab("Waves analyzed")
  c <- c + xlab("p-value | Wald test") + ylab("Waves analyzed")
  d <- d + xlab("p-value | Wald test") + ylab("Waves analyzed")

  b <- b + geom_vline(xintercept=0, color="firebrick3",linetype="dashed", size=.5, alpha=.5 )
  d <- d + geom_vline(xintercept=0, color="firebrick3",linetype="dashed", size=.5, alpha=.5 )
  
  c <- c + scale_x_continuous(breaks=seq(0,1,by=.2), limits=c(-.2,1))
  d <- d + scale_x_continuous(breaks=seq(0,1,by=.2), limits=c(-.2,1)) 
  
  # b <- b + scale_y_continuous(limits=c(-5,5 )) 
  grid.newpage()    
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid.layout(nrow=2, ncol=2,
                        widths=unit(c(.5, .5) ,c("null", "null")),
                        heights=unit(c(.5,.5), c("null", "null"))
  )
  pushViewport(viewport(layout=layout))
  print(a, vp=viewport(layout.pos.col=1, layout.pos.row=1))
  print(b, vp=viewport(layout.pos.col=2, layout.pos.row=1))
  print(c, vp=viewport(layout.pos.col=1, layout.pos.row=2))
  print(d, vp=viewport(layout.pos.col=2, layout.pos.row=2))
  
  popViewport(0)
   # return(c)
} # close kb_profiles
# kb_profiles(ds,  vertical="Waves",  border=5)

  
  