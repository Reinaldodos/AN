library(igraph);library(tidyr); library(data.table)
setwd("~/Dropbox/Carto & Stats/R/AN")
source("Assemblée Nationale.R")
SIMIL <- function (test1, test2)
{
  print(c(test1, test2), quote = FALSE)
  Scrutins = toto[toto$Scrutin %in% toto[toto$Député   == test1]$Scrutin &
                    toto$Scrutin %in% toto[toto$Député   == test2]$Scrutin]$Scrutin %>%
    unique()

  if (length(Scrutins)==0) {
    return(NULL)
  }
  else
  {
    foo = toto[toto$Scrutin %in% Scrutins &
                 toto$Député    %in% c(test1, test2)] %>%
      spread(key = Député, value = Vote) %>%
      data.table()
    foo[,Scrutin:= NULL]
    foo$Equal = NA
    foo = as.data.frame(foo)
    for (n in 1:nrow(foo))
    {
      foo[n,3] = identical(foo[n,1], foo[n,2])
    }

    simil = nrow(foo[foo$Equal == TRUE,]) / nrow(foo)
    result = cbind(test1, test2, simil) %>% data.table()
    return(result)
  }
}

Députés = toto$Député   %>% unique() %>% as.list()
DONE = list()

while (length(Députés) > 1)
{
  test1 = Députés[[1]]
  Députés[[1]] = NULL
  DONE[[length(DONE) + 1]] = lapply(Députés, function(x)
    SIMIL(test1, x)) %>% rbindlist()
}

RESULT = DONE %>% rbindlist()

# RESULT = fread(input = "Paires.csv")
colnames(RESULT) = c("test1", "test2", "weight")

g = graph.data.frame(RESULT, directed = FALSE)
plot(g,layout=layout.fruchterman.reingold,edge.width=E(g)$weight/2)
groupes = cluster_fast_greedy(graph = g, membership = TRUE)
plot(groupes, g, col = membership(groupes), mark.groups = communities(groupes))
output = cbind(groupes$names, groupes$membership) %>% data.table()
