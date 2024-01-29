library(RPostgres)

sqlQuery <- function (query) {
  
  # creating DB connection object with RMysql package
  con <- dbConnect(RPostgres::Postgres(), host = 'localhost',
                   dbname = "isseabd",
                   user = "postgres",
                   password = "postgres")
  
  # close db connection after function call exits
  on.exit(dbDisconnect(con))
  
  # send Query to Obtain result set
  rs <- dbSendQuery(con, query)
  
  # get elements from result sets and convert to dataframe
  result <- dbFetch(rs, -1)
  
  # clear result object
  dbClearResult(rs)
  
  # return the dataframe
  return(result)
}


################# Connexion
utilisateur <- sqlQuery("select * from utilisateur")

#nbr d'ancien eleves
nbEl <- sqlQuery("SELECT count(id_user) FROM etudiant;")

#nbr de travaileurs
nbTr <- sqlQuery("SELECT count(DISTINCT id_user) FROM parcours_pro")

# Nombre de fréquentations
nbFr <- sqlQuery("SELECT count(DISTINCT id_user) FROM etude")

# Nombre de non situé
chom <- sqlQuery("select count(id_user) from etudiant where id_user not in (select id_user from parcours_pro) and id_user not in (select id_user from etude)
")

# Nombre travailleurs par filière
travaille <- sqlQuery("select filiere, count(p.id_user) from 
                      filiere f left join etudiant e on f.id_filiere = e.id_filiere 
                      left join parcours_pro p on e.id_user = p.id_user 
                      group by filiere
                      order by count desc")

# Nombre travailleurs par filière
secto <- sqlQuery("select secteur_activite
from organisation o left join parcours_pro p on o.id_organisation = p.id_organisation
")

# filiere et secteur
filier_secto <- sqlQuery("select filiere, secteur_activite, count(p.id_user)
from filiere f join etudiant e on f.id_filiere = e.id_filiere
join parcours_pro p on e.id_user = p.id_user
join organisation o on p.id_organisation = o.id_organisation
group by filiere, secteur_activite")

##############################################" Salaire

# intervalls salaire
intsalaire <- sqlQuery("select min(salaire), max(salaire), round(avg(salaire)) from parcours_pro")

# Tableau salaire
tabsalaire <- sqlQuery("select f.filiere, secteur_activite,min(salaire) as minimum, round(avg(salaire))as moyen, max(salaire) as maximum
from parcours_pro p left join organisation o on p.id_organisation = o.id_organisation
left join etudiant e  on p.id_user = e.id_user
left join filiere f on f.id_filiere = e.id_filiere
group by f.filiere, secteur_activite
order by f.filiere, secteur_activite")


salaire_sect<- sqlQuery("select secteur_activite,min(salaire) as minimum, round(avg(salaire))as moyen, max(salaire) as maximum
from parcours_pro p left join organisation o on p.id_organisation = o.id_organisation
left join etudiant e  on p.id_user = e.id_user
group by secteur_activite
order by moyen")

salair_filiere<- sqlQuery("select f.filiere, min(salaire) as minimum, round(avg(salaire))as moyen, max(salaire) as maximum
from parcours_pro p left join organisation o on p.id_organisation = o.id_organisation
left join etudiant e  on p.id_user = e.id_user
left join filiere f on f.id_filiere = e.id_filiere
group by f.filiere
order by moyen")


######################################################################### Temps

temps <- sqlQuery("SELECT f.filiere, round(AVG(EXTRACT(YEAR FROM p.date_entree) - e.annee_dipl),2) AS avg_years
                  FROM filiere f 
                  LEFT JOIN etudiant e ON f.id_filiere = e.id_filiere 
                  LEFT JOIN parcours_pro p ON e.id_user = p.id_user 
                  GROUP BY f.filiere order by avg_years;")



temps_filiere <- sqlQuery("SELECT annee_dipl, f.filiere, round(AVG(EXTRACT(YEAR FROM p.date_entree) - e.annee_dipl),2) AS avg_years
FROM filiere f 
LEFT JOIN etudiant e ON f.id_filiere = e.id_filiere 
LEFT JOIN parcours_pro p ON e.id_user = p.id_user 
GROUP BY annee_dipl, f.filiere
order by annee_dipl, filiere")

############################"localisation
locali<- sqlQuery("select pays, count(distinct p.id_user) from
pays pa join ville v on pa.id_pays = v.id_pays
join parcours_pro p on p.id_ville = v.id_ville
group by pays order by count")


###################################################### Profil
profil_el<- sqlQuery("select * from etudiant")

######################################################## 

# Fermer la connexion à la base de données
# dbDisconnect(con)
nbEl=42
nbTr = 33
nbFr=7
chom = 4

# 
# 
# aapl <- quantmod::getSymbols("AAPL",
#                              src = "yahoo",
#                              from = "2020-01-01",
#                              auto.assign = FALSE
# )
# 
# # Plot prices and volume with relative height.
# highchart(type = "stock") %>%
#   hc_title(text = "AAPLE") %>%
#   hc_add_series(aapl, yAxis = 0, showInLegend = FALSE) %>%
#   hc_add_yAxis(nid = 1L, title = list(text = "Prices"), relative = 2) %>%
#   hc_add_series(aapl[, "AAPL.Volume"], yAxis = 1, type = "column", showInLegend = FALSE) %>%
#   hc_add_yAxis(nid = 2L, title = list(text = "Volume"), relative = 1)
