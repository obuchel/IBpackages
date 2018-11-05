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
  doMC::registerDoMC(cores=24) # or however many cores you have access to
  data<-jsonlite::fromJSON(cdata, simplifyDataFrame = TRUE)
    d<-data
    org = cbind(d$country, d$host)
    org1 = graph.data.frame(org, directed = TRUE, vertices = NULL)
    year_data<-get.measures(org1)
    year_alpha<-get.alpha(org1)
    year_groups<-get.groups(org1)
    if (is.null(year_groups)) {
      vector <- cbind("l$membership","l$modularity",year_data,year_alpha)
    } else {
      l <- jsonlite::fromJSON(year_groups, simplifyDataFrame = TRUE)
      vector <- cbind(l$membership,l$modularity,year_data,year_alpha)
    }
return(vector)
  ##}
}

get.measures <- function(org1){
  bet <- igraph::betweenness(org1,normalized = TRUE)
  degs <- igraph::degree(org1, v = V(org1))
  indegs <- igraph::degree(org1, v = V(org1), mode = "in",normalized = TRUE)
  outdegs <- igraph::degree(org1, v = V(org1), mode = "out",normalized = TRUE)
  clos <- igraph::closeness(org1, v = V(org1), mode = "all",normalized = TRUE)
  indegs1 <- igraph::degree(org1, v = V(org1), mode = "in")
  outdegs1 <- igraph::degree(org1, v = V(org1), mode = "out")
  df <- data.frame(bet, degs, indegs, outdegs, clos, indegs1, outdegs1)
  return(df)
}

get.alpha <- function (org1) {
  ac <- alpha.centrality(org1, nodes = V(org1), alpha = 0.5, loops = FALSE, exo = 1, weights = NULL, tol = 1e-07, sparse = TRUE)
  return(data.frame(ac))
}

get.groups <- function (org1){
  dg <- decompose.graph(org1)
  if (length(dg)<2) {
    E(org1)$weight <- runif(ecount(org1))
    groups <- spinglass.community(org1, weights=E(org1)$weight,spins = 116)
    return(toJSON(groups,force=TRUE))
  }
}
