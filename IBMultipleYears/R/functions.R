#library(jsonlite)
# theUrl<-"http://ec2-54-149-181-220.us-west-2.compute.amazonaws.com/IBNetworkExplorer/graph_array.json"
# cdata <-toJSON(fromJSON(theUrl))
#' @param Data are in JSON format, e.g., cdata=read.csv(file ="http://ec2-54-149-181-220.us-west-2.compute.amazonaws.com/IBToolbox/data.csv",header=TRUE,sep = ",", quote = "'")
#' @return Network centrality measures: bet(normalized), degs(normalized), indegs(normalized), outdegs(normalized), clos, indegs1(not normalized),outdegs1(not normalized)
#' @export

get_network_properties <- function(cdata) {
 library(doMC)
  library(dplyr)
  library(jsonlite)
  library(network)
  ##library(sna)
  library(igraph)
  library(RJSONIO)
    doMC::registerDoMC(cores=2) # or however many cores you have access to
    data<-jsonlite::fromJSON(cdata, simplifyDataFrame = TRUE)
    distinct_years = data %>% distinct(year)
    sorted<-sort(distinct_years$year)
    plength <- length(sorted)
vector = c()
   foreach(i=1:plength,.packages='igraph') %dopar% {
##for (i in 1:plength) {
  year<-sorted[i]
     d<-data[data["year"] == year, ]
     org = cbind(d$country, d$host)
     ##graph_from_edgelist(org, directed = TRUE)
     ##get.adjacency(graph_from_edgelist(org, directed = TRUE))
     ##nrelations <- network(org, directed = TRUE)
     ##relations <- as.matrix(nrelations)
     org1 = graph.data.frame(org, directed = TRUE, vertices = NULL)
    year_data<-get.measures(org1)
     year_alpha<-get.alpha(org1)
     year_groups<-get.groups(org1)
     if (is.null(year_groups)) {
       ##vector[i] <-"hhh"
       ##gm<-"l$membership"
       ##gmod<-"l$modularity"
       vector[i] <- cbind("l$membership","l$modularity",year,year_data,year_alpha)
     } else {
       l <- jsonlite::fromJSON(year_groups, simplifyDataFrame = TRUE)
       ##vector[i] <-"hhh"
       ##gm<-l$membership
       ##gmod<-l$modularity
       vector[i] <- cbind(l$membership,l$modularity,year,year_data,year_alpha)
     }
     ##vector[i] <- c(gm,gmod,year,year_data,year_alpha)
     ##return(vector[i])
     }
##return(vector)
}

get.measures <- function(org1){
  bet <- betweenness(org1,normalized = TRUE)
  degs <- degree(org1, v = V(org1))
  indegs <- degree(org1, v = V(org1), mode = "in",normalized = TRUE)
  outdegs <- degree(org1, v = V(org1), mode = "out",normalized = TRUE)
  clos <- closeness(org1, v = V(org1), mode = "all",normalized = TRUE)
  indegs1 <- degree(org1, v = V(org1), mode = "in")
  outdegs1 <- degree(org1, v = V(org1), mode = "out")
  df <- data.frame(bet, degs, indegs, outdegs, clos, indegs1, outdegs1)
  return(df)
}

get.alpha <- function (org1) {
  ac <- alpha.centrality(org1, nodes = V(org1), alpha = 0.5, loops = FALSE, exo = 1, weights = NULL, tol = 1e-07, sparse = TRUE)
  return(data.frame(ac))
}

get.groups <- function (org1){
  ##dg0<-graph.adjacency(relations, weighted = TRUE, mode = "directed")
  dg <- decompose.graph(org1)
  if (length(dg)<2) {
    E(org1)$weight <- runif(ecount(org1))
    groups <- spinglass.community(org1, weights=E(org1)$weight,spins = 116)
    return(toJSON(groups,force=TRUE))
  }
}
