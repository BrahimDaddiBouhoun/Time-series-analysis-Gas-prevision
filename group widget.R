install.packages('devtools')
install_github("jeromefroe/circlepackeR")
install.packages('ggraph')
install.packages('data.tree')


l=c()
for (i in 1:ncol(sonel0.05)) {
  l=c(l,length(del_zeros(sonel0.05[,i])))
}
l

m=c()
for (i in 1:ncol(sonel0.05)) {
  m=c(m,distru[del_zeros(sonel0.05[,i])])
}

data <- data.frame(
  root=rep("root", 249),
  group=c(rep("group A",l[1]), rep("group B",l[2]), rep("group C",l[3]), rep("group D",l[4]),
          rep("group E",l[5]), rep("group F",l[6]), rep("group G",l[7]), rep("group H",l[8]),
          rep("group I",l[9]), rep("group J",l[10]),rep("group K",l[11]),rep("group L",l[12]),
          rep("group M",l[13]),rep("group N",l[14]),rep("group O",l[15]),rep("group P",l[16]),
          rep("group Q",l[17]),rep("group R",l[18]),rep("group S",l[19]),rep("group T",l[20]),
          rep("group U",l[21]),rep("group V",l[22]),rep("group W",l[23])), 
  subgroup= m,
  value=c(rep(l[1],l[1]),rep(l[2],l[2]),rep(l[3],l[3]),rep(l[4],l[4]),rep(l[5],l[5]),
          rep(l[6],l[6]),rep(l[7],l[7]),rep(l[8],l[8]),rep(l[9],l[9]),rep(l[10],l[10]),
          rep(l[11],l[11]),rep(l[12],l[12]),rep(l[13],l[13]),rep(l[14],l[14]),rep(l[15],l[15]),
          rep(l[16],l[16]),rep(l[17],l[17]),rep(l[18],l[18]),rep(l[19],l[19]),rep(l[20],l[20]),
          rep(l[21],l[21]),rep(l[22],l[22]),rep(l[23],l[23]))
)


# Change the format. This use the data.tree library. This library needs a column that looks like root/group/subgroup/..., so I build it
require(data.tree)
data$pathString <- paste("world", data$group, data$subgroup, sep = "/")
population <- as.Node(data)

# Make the plot
require(circlepackeR)
p=circlepackeR(population,size = "value")
p
# save the widget
library(htmlwidgets)
saveWidget(p, file=paste0( getwd(), "/circular_sonel0.05.html"))
