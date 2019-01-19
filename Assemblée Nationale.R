library(rvest)
library(data.table)
library(tidyr)
library(foreach)

nb_scrutin = read_html("http://www2.assemblee-nationale.fr/scrutins/liste/(legislature)/14") %>%
  html_node("td.denom") %>%
  html_text() %>%
  as.numeric()

scrutins = as.list(
  paste(
    "http://www2.assemblee-nationale.fr/scrutins/detail/(legislature)/14/(num)/",
    1:nb_scrutin,
    sep = ""
  )
)

CHECK = function(x)
{
  if (ncol(x) < 3)
  {
    x = NULL
  }
  return(x)
}

VOTE <- function (scrutins, num_scrutin)
{
  print(num_scrutin, quote = FALSE)
  scrutin = scrutins[[num_scrutin]]
  Contre = read_html(scrutin) %>%  html_nodes(".Contre li") %>%  html_text() %>% cbind("Contre", num_scrutin) %>% CHECK()
  Pour = read_html(scrutin) %>%  html_nodes(".Pour li") %>%  html_text() %>% cbind("Pour", num_scrutin) %>% CHECK()
  Abstention = read_html(scrutin) %>%  html_nodes(".Abstention li") %>%  html_text() %>% cbind("Abstention", num_scrutin) %>% CHECK()
  result = rbind(Pour, Contre, Abstention) %>% data.table() %>% sapply(trimws) %>% data.table()
  colnames(result) = c("Député", "Vote", "Scrutin")
  return(result)
}

INPUT = foreach(num_scrutin = 1:nb_scrutin) %do% 
  VOTE(scrutins, num_scrutin) %>% 
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  unique()
condition = INPUT$Député %in% c(
  "présents ou ayant délégué leur droit de vote",
  "membres du groupe",
  "membre du groupe",
  "présent ou ayant délégué son droit de vote"
)
INPUT = INPUT[!condition]
INPUT$Scrutin = as.numeric(INPUT$Scrutin)

# saveRDS(object = INPUT, file = "Desktop/Assemblée Nationale")
toto = INPUT
