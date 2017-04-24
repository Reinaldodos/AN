require(rvest)
require(data.table)
url = "http://elections.interieur.gouv.fr/presidentielle-2017/"
Regions =
  url %>%
  read_html() %>%
  html_nodes(css = ".span12 a") %>%
  html_attr(name = "href")

Dep_Fetch <- function(url, region)
{
  region_Name =
    paste(url, region, sep = "") %>%
    read_html() %>%
    html_nodes(css = "a:nth-child(2)") %>%
    html_text
  
  Dep =
    paste(url, region, sep = "") %>%
    read_html() %>%
    html_nodes(css = ".span8 a") %>%
    html_attr(name = "href")
  return(list(region_Name, Dep))
}

Nom_Villes_Fetch <- function(url, dept)
{
  dept_Name =
    paste(url, substr(
      x = dept,
      start = 4,
      stop = nchar(dept)
    ), sep = "") %>%
    read_html() %>%
    html_nodes(css = "a:nth-child(3)") %>%
    html_text
  
  Nom_villes =
    paste(url, substr(
      x = dept,
      start = 4,
      stop = nchar(dept)
    ), sep = "") %>%
    read_html() %>%
    html_nodes(css = ".span8 a") %>%
    html_attr(name = "href")
  return(list(dept_Name, Nom_villes))
}

Commune_Fetch <- function(url, nom_ville)
{
  Communes =
    paste(url, substr(
      x = nom_ville,
      start = 7,
      stop = nchar(nom_ville)
    ), sep = "") %>%
    read_html() %>%
    html_nodes(css = ".tableau-communes a") %>%
    html_attr(name = "href")
  
  return(Communes)
}

Scores_Fetch <- function(url, commune)
{
  Commune_name =
    paste(url, substr(
      x = commune,
      start = 7,
      stop = nchar(commune)
    ), sep = "") %>%
    read_html() %>%
    html_nodes(css = ".pub-fil-ariane a:nth-child(4)") %>%
    html_text
  
  Scores =
    paste(url, substr(
      x = commune,
      start = 7,
      stop = nchar(commune)
    ), sep = "") %>%
    read_html() %>%
    html_table()
  return(list(Commune_name, Scores[[2]]))
}

require(foreach)

SCORE =
  foreach(region = Regions) %do%
  {
    Dep = Dep_Fetch(url = url, region = region)
    print(Dep[[1]])
    foreach(dept = Dep[[2]]) %do%
    {
      Nom_villes = Nom_Villes_Fetch(url, dept)
      print(Nom_villes[[1]])
      foreach(nom_ville = Nom_villes[[2]]) %do%
      {
        Communes = Commune_Fetch(url, nom_ville)
        foreach(commune = Communes) %do%
        {
          score = Scores_Fetch(url, commune)
          require(dplyr)
          input =
            score[[2]] %>% select(`Liste des candidats`, Voix)
          print(score[[1]])
          SCORE =
            cbind.data.frame(Dep[[1]],
                             Nom_villes[[1]],
                             score[[1]],
                             input)
          return(SCORE)
        } %>% rbindlist()
      } %>% rbindlist()
    } %>% rbindlist()
  } %>% rbindlist()

SCORE =rbindlist(SCORE)
names(SCORE) = 
  c(
    "Région",
    "Département",
    "Commune",
    "Candidat",
    "Voix"
  )

