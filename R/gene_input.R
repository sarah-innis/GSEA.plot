#' View Available Geneset Databases
#'
#' This function takes one input, the common name of the geneset database you are interested in and returns the name of the rda file that can be used in the GSEA analysis function.
#' If the input is "all" then all database common names are returned.
#'
#' @param database_name Either enter "all" or a string containing the name of one of the existing geneset databases
#' @return Either the names of all existing gene set databases or the file name for the specified database
#' @export


database_key=function(database_name=""){
  data(key)
  if (database_name=="all"){
    return(key[,2])
  }
  else {
    index=which(key[,2]==database_name)
    database_of_interest=key[index,1]
    database_of_interest=as.character(database_of_interest)
    return(database_of_interest)
  }
}

#' View Gene Sets within Database
#'
#' This function takes one input, the file name for the database, and returns the names of the sets within the database.
#'
#' @param database_file Name of the rda file for the geneset database within the package
#' @return The names of the genesets within the database
#' @export

get_genesets=function(database_file=""){
temp=database_file
max.Ng <- length(temp)
names <- vector(length = max.Ng, mode = "character")
gs.count <- 1


for (i in 1:max.Ng) {
  gene.set.size <- length(unlist(strsplit(temp[[i]], "\t"))) - 2
  gs.line <- noquote(unlist(strsplit(temp[[i]], "\t")))
  gene.set.name <- gs.line[1]
  names[gs.count] <- gene.set.name
  gs.count <- gs.count + 1
}
return(names)
}

#' Create a Geneset Database
#'
#' This function takes a list of geneset databases. It returns a list that can be saved as a new database.
#' @param databases a list of geneset database files that contain the gene sets
#' @param geneset_names a list of lists. The first list corresponds to the first db_file and so on and says what genesets to include in new database. If equal to "all" all genesets in that database will be included
#' @return a dataframe that can be saved to the package or used directly by the GSEAplots function
#' @export

combine_databases=function(databases="",geneset_names=""){
  library(rlist)
  if (geneset_names[[1]]=="all"){
    new_db=databases[[1]]
  }
  else {
    db1_names=get_genesets(databases[[1]])
    index=which(db1_names==geneset_names[[1]][[1]])
    new_db=databases[[1]][[index]]
    for (j in 2:length(geneset_names[[1]])){
      index=which(db1_names==geneset_names[[1]][[j]])
      new_db=list.append(new_db,databases[[1]][[index]])
    }
  }
  for (i in 2:length(databases)){
    if (geneset_names[[i]]=="all"){
      new_db=list.append(new_db,databases[[i]])
    }
    else{
      db_names=get_genesets(databases[[i]])
      index=which(db_names==geneset_names[[i]][[1]])
      new_db=list.append(new_db, databases[[i]][[index]])
      for (j in 2:length(geneset_names[[i]])){
        index=which(db_names==geneset_names[[i]][[j]])
        new_db=list.append(new_db,databases[[i]][[index]])
    }
    }
  }
  return(new_db)
}

#' Format New Geneset Daatbase
#'
#' When given a list with keys as a set name and the data corresponding to a key, the gene symbols corresponding to that key. This function will reformat the data to be saved into the package and to be a working input to the GSEA.plots function.
#' @param database a list names are the sets within the database and contained within those are genesymbols
#' @return formatted_db a database ready to be saved to the package and used in later GSEA analyses
#' @export
create_geneset_db=function(database=""){
  formatted_db=list()
  null_elements=c()
  names=names(database)
  for (i in 1:length(database)){
    if (is.null(database[[i]])){
      null_elements=rbind(null_elements,i)
    }
  }
  if (length(null_elements)>0){
  database=database[-null_elements]
  names=names[-null_elements]
  }

  for (i in 1: length(database)){
    names(database[[i]])=NULL
    q=paste(database[[i]],collapse="\t")
    formatted_db[[i]]=paste(names[[i]],'NA',q,sep="\t")
  }

  w=unlist(formatted_db)
  return(formatted_db)
}

