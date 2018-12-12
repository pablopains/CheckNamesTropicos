####################################################################################################
#                                     Karst Flora Research Group                                   #
#                                                                                                  #
# Checks the scientific name of the plants according to tropicos.org                               #
#                                                                                                  #
# Pablo Hendrigo Alves de Melo (pablopains@yahoo.com.br)                                           #  
#                                                                                                  #
# Last update : 12-12-2018                                                                         #  
####################################################################################################

check.names.tropicos <- function(scientificnamewithoutauthors='', scientificnamewithauthors = '')
{
  colunas.res <- c("nameid",
                   "scientificname",
                   "scientificnamewithauthors",
                   "family",
                   "rankabbreviation",
                   "nomenclaturestatusname",
                   "author",
                   "displayreference",
                   "displaydate",
                   "totalrows") 
  
  nomes.busca.tp1 <- tp_search(name = as.character(scientificnamewithoutauthors), 
                               key = key.tp,
                               type =  "exact")
  
  # nao encontrado
  if (nomes.busca.tp1[1] == "No names were found")
  { 
    tropicos.name <- data.frame(
                             nameid = NA,
                             scientificname = NA,
                             scientificnamewithauthors = NA,
                             family = NA,
                             rankabbreviation = NA,
                             nomenclaturestatusname = "No names were found",
                             author = NA,
                             displayreference = NA,
                             displaydate = NA,
                             totalrows = NA,
                             stringsAsFactors = FALSE)

    return(tropicos.name)                       
  }
  
  index.tp <- nomes.busca.tp1$rankabbreviation %in% c("gen.", "fam.", "sect.", "var.", "subsp.","fo.","cv.", "ser.")
  nomes.busca.tp1 <- nomes.busca.tp1[!index.tp, colunas.res]
  
  index.tp <- nomes.busca.tp1$nomenclaturestatusname %in% c("No opinion", "nom. cons.", "Legitimate")
  nomes.busca.tp1 <- nomes.busca.tp1[index.tp, colunas.res] 
  
  # homonimos
  if (NROW(nomes.busca.tp1)>1)
  {
    
    index.tp <- nomes.busca.tp1$nomenclaturestatusname %in% c("nom. cons.", "Legitimate")
    nomes.busca.tp1 <- nomes.busca.tp1[index.tp, colunas.res] 
    
    if (NROW(nomes.busca.tp1)>1)
    {
      homonyms <- ""
      x <- as.character(nomes.busca.tp1$scientificnamewithauthors)
      for (i in x)
      {homonyms <- paste0(homonyms, ifelse(homonyms=="","","; ") ,as.character(i))}
      
      tropicos.name <- data.frame(
                               nameid = NA,
                               scientificname = NA,
                               scientificnamewithauthors = NA,
                               family = NA,
                               rankabbreviation = NA,
                               nomenclaturestatusname = paste0(" homonyms: (", NROW(nomes.busca.tp1),") ", homonyms),
                               author = NA,
                               displayreference = NA,
                               displaydate = NA,
                               totalrows = NA,
                               stringsAsFactors = FALSE)
      return(tropicos.name)                       
    }  
    
    if (NROW(nomes.busca.tp1)==0)
    {
      nomes.busca.tp1 <- tp_search(name = as.character(scientificnamewithoutauthors), 
                                   key = key.tp,
                                   type =  "exact")
      
      index.tp <- nomes.busca.tp1$nomenclaturestatusname %in% c("No opinion")
      nomes.busca.tp1 <- nomes.busca.tp1[index.tp, colunas.res] 
      
      if (NROW(nomes.busca.tp1)>1)
      {
        homonyms <- ""
        x <- as.character(nomes.busca.tp1$scientificnamewithauthors)
        for (i in x)
        {homonyms <- paste0(homonyms, ifelse(homonyms=="","","; ") ,as.character(i))}
        
        tropicos.name <- data.frame(
                                 nameid = NA,
                                 scientificname = NA,
                                 scientificnamewithauthors = NA,
                                 family = NA,
                                 rankabbreviation = NA,
                                 nomenclaturestatusname = paste0(" homonyms: (", NROW(nomes.busca.tp1),") ", homonyms),
                                 author = NA,
                                 displayreference = NA,
                                 displaydate = NA,
                                 totalrows = NA,
                                 stringsAsFactors = FALSE)
        return(tropicos.name)                       
      }  
    }  
    
  }
  
  # Invalido ou rejeitado, recarregar registros para testar nomes válidos
  if (NROW(nomes.busca.tp1)==0)
  {
    nomes.busca.tp1 <- tp_search(name = as.character(scientificnamewithoutauthors), 
                                 key = key.tp,
                                 type =  "exact")
    index.tp <- nomes.busca.tp1$rankabbreviation %in% c("gen.", "fam.", "sect.", "var.", "subsp.","fo.","cv.", "ser.")
    nomes.busca.tp1 <- nomes.busca.tp1[!index.tp, colunas.res]
    
    # Invalido ou rejeitado
    if (nomes.busca.tp1$nomenclaturestatusname %in% c("Invalid") == TRUE)
    {
      tropicos.name <- data.frame(
                               nameid = NA,
                               scientificname = NA,
                               scientificnamewithauthors = NA,
                               family = NA,
                               rankabbreviation = NA,
                               nomenclaturestatusname = "invalid or rejected name",
                               author = NA,
                               displayreference = NA,
                               displaydate = NA,
                               totalrows = NA,
                               stringsAsFactors = FALSE)
      return(tropicos.name)                       
    }  
    
  }
  
  # verificando se há mais nomes aceitos
  accnames.tp <-  tp_accnames(id = nomes.busca.tp1$nameid, key = key.tp )$acceptednames
  id.accnames.tp <- unique(accnames.tp$nameid)
  
  # mais de um nome aceito
  if (length(id.accnames.tp)>=1 & scientificnamewithauthors == '')
  { 
    nomes.accept <- as.character(nomes.busca.tp1$scientificnamewithauthors)
    x = unique(accnames.tp[accnames.tp$nameid %in% id.accnames.tp, c("scientificname") ])
    for (i in x)
    { nomes.accept <- paste0(nomes.accept, "; " ,as.character(i))}
    
    tropicos.name <- data.frame(
                             nameid = NA,  
                             scientificname = NA,
                             scientificnamewithauthors = NA,
                             family = NA,
                             rankabbreviation = NA,
                             nomenclaturestatusname = paste0(length(id.accnames.tp)+1, " names accepted: ", nomes.accept),
                             author = NA,
                             displayreference = NA,
                             displaydate = NA,
                             totalrows = NA,
                             stringsAsFactors = FALSE)
    return(tropicos.name)                       
  }
  
  # quando informar autor para separar homonimos
  if (length(id.accnames.tp)>=1 & scientificnamewithauthors != '')
  { 
    nomes.busca.tp1 <- nomes.busca.tp1[nomes.busca.tp1$scientificnamewithauthors == scientificnamewithauthors,]
    if (NROW(nomes.busca.tp1)==0)
    { 
      tropicos.name <- data.frame(
        nameid = NA,
        scientificname = NA,
        scientificnamewithauthors = NA,
        family = NA,
        rankabbreviation = NA,
        nomenclaturestatusname = "No scientificname with authors were found",
        author = NA,
        displayreference = NA,
        displaydate = NA,
        totalrows = NA,
        stringsAsFactors = FALSE)
      
      return(tropicos.name)                       
    }
  }
  
  return(nomes.busca.tp1)
}

# check.names.tropicos(scientificnamewithoutauthors, scientificnamewithauthors)
# check.names.tropicos(scientificnamewithoutauthors, '')
