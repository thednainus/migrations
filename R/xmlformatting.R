#Scripts modified from https://github.com/emvolz-phylodynamics/sarscov2Rutils/tree/sarscov2Rutils

#function to add the date trait to the xml script
.date_trait <- function(sts){
  paste( collapse=',' , paste( sep = '=', names(sts), unname(sts)) )
}

#function to add the sequence node to the xml beast2 file
.seq_format <- function( d ){
  y = '<sequence id="NAME" spec="Sequence" taxon="NAME" totalcount="4" value="SEQUENCE"/>'
  seqnames <- unlist(lapply(rownames(d), function(x) .add_deme(x)))
  rownames(d) <- seqnames
  seqd = lapply(  seqnames , function(sid){
    y1 = gsub( pattern = 'NAME', replace=sid, y )
    seq = paste( collapse='', as.character( d[sid, ] )[1,]  )
    y2 = gsub( pattern ='SEQUENCE', replace= seq, y1 )
    y2
  })
  paste( seqd, collapse = '\n' )
}

#function to add the sequence node to the xml beast1 file
.seq_format_beast1 <- function( d ){
  y = '\t\t\t\t<sequence>\n\t\t\t\t\t\t<taxon idref="NAME"/>\n\t\t\t\t\t\t"SEQUENCE"\n\t\t\t\t</sequence>'
  seqnames <- unlist(lapply(rownames(d), function(x) .add_deme(x)))
  rownames(d) <- seqnames
  seqd = lapply(  seqnames , function(sid){
    y1 = gsub( pattern = 'NAME', replace=sid, y )
    seq = noquote(paste( collapse='', as.character( d[sid, ] )[1,]  ))
    y2 = gsub( pattern ='SEQUENCE', replace= seq, y1 )
    y3 = gsub("\t\"", "", y2)
    y4 = gsub("\"\n", "\n", y3)
    y4
  })
  paste( seqd, collapse = '\n' )
}


#function to add the trait node to the xml beast1 file
.trait_format_beast1 <- function( d ){
  y =  '\t\t\t\t<taxon id="NAME">\n\t\t\t\t\t\t<date value="DATE" direction="forwards" units="years"/>\n\t\t\t\t\t\t<attr name="location">\n\t\t\t\t\t\t\t\tLOCATION\n\t\t\t\t\t\t</attr>\n\t\t\t\t</taxon>'
  seqnames <- unlist(lapply(rownames(d), function(x) .add_deme(x)))
  rownames(d) <- seqnames

  seqd = lapply(  seqnames , function(sid){
    y1 = gsub( pattern = 'NAME', replace=sid, y )
    date_dec <- .get_date(sid)
    #seq = paste( collapse='', as.character( d[sid, ] )[1,]  )
    y2 = gsub( pattern ='DATE', replace= date_dec, y1 )
    loc <- tail(str_split(sid, "_")[[1]], 1)
    y3 <- gsub( pattern ='LOCATION', replace= loc, y2 )
  })
  paste( seqd, collapse = '\n' )
}

#function to add the "deme" to the sequence name as the simulated sequence data
#do not have the deme name appended to their name.
#this is necessary to run phydyn
.add_deme <- function(x){

  texts <- str_split(x, "_")
  location <- texts[[1]][2]

  if( location == "region"){

    seq_deme_name <- paste(x, "I", sep = "_")

  } else if(location == "src"){

    seq_deme_name <- paste(x, "src", sep = "_")

  }

  return(seq_deme_name)
}


#function to get the sampling date using the sequence name
.get_date <- function(x){

  texts <- str_split(x, "_")
  location <- texts[[1]][2]

  if( location == "region"){

    sts <- texts[[1]][5]

  } else if(location == "src"){

    sts <- texts[[1]][4]

  }

  return(sts)

}


#' Generates a runnable XML file to run phydyn in beast2
#'
#' This function will insert the sequence data, tip dates to a XML skeleton
#'
#' The names of the sequence data and trees must match and must be in the
#' standard form for inferring tip dates. The XML skeleton must have these
#' entries to be overwritten:
#' SEQUENCES
#' DATE_TRAIT
#'
#' A XML file will be generated for each sequence alignment provided
#'
#' @param xmlfn The file name of the XML skeleton
#' @param fastafn The file name of the sequence data in FASTA format
#'
#' @return Character string of runnable XML individual XMLs for each sequence
#'    alignment will be written to disk
#' @export
format_xml_phydyn <- function(xmlfn , fastafn){
  library( ape )

  #~ 	Attributes to overwrite:
  #~ SEQUENCES
  #~ DATE_TRAIT

  d = read.dna( fastafn, format = 'fasta' )
  seqdata = .seq_format( d )

  sids = rownames(d)
  sids <- unlist(lapply(sids, function(x) .add_deme(x)))

  sts <- unlist(lapply(sids, function(x) .get_date(x)))
  names(sts) <- sids
  datedata = .date_trait( sts )

  x = readLines( xmlfn )
  xmlofn = gsub(xmlfn, pattern = "_TEMPLATE", replacement = "")

  xk1 = gsub(x, pattern = "DATE_TRAIT", replacement = datedata)
  xk2 = gsub(xk1, pattern = "SEQUENCES", replacement = seqdata)

  filename <- tail(str_split(xmlofn, "/")[[1]], n = 1)
  writeLines(xk2, con = filename)
  invisible(xk2)

}


#' Generates a runnable XML file to run discrete phylogeography in beast1
#'
#' This function will insert the sequence data, tip dates and
#' location to a XML skeleton
#'
#' The names of the sequence data and trees must match and must be in the
#' standard form for inferring tip dates. The XML skeleton must have these
#' entries to be overwritten:
#' SEQUENCES
#' TRAITS
#'
#' A XML file will be generated for each sequence alignment provided
#'
#' @param xmlfn The file name of the XML skeleton
#' @param fastafn The file name of the sequence data in FASTA format
#'
#' @return Character string of runnable XML individual XMLs for each sequence
#'    alignment will be written to disk
#' @export
format_xml_beast1 <- function(xmlfn , fastafn){
  library( ape )

  #~ 	Attributes to overwrite:
  #~ SEQUENCES
  #~ TRAITS

  d = read.dna( fastafn, format = 'fasta' )
  seqdata = .seq_format_beast1( d )
  traits_data = .trait_format_beast1(d)

  #sts <- unlist(lapply(sids, function(x) .get_date(x)))
  #names(sts) <- sids
  #traits_data = .date_trait( sts )

  x = readLines( xmlfn )
  xmlofn = gsub(xmlfn, pattern = "_TEMPLATE", replacement = "")

  xk1 = gsub(x, pattern = "TRAITS", replacement = traits_data)
  xk2 = gsub(xk1, pattern = "SEQUENCES", replacement = seqdata)

  filename <- tail(str_split(xmlofn, "/")[[1]], n = 1)
  writeLines(xk2, con = filename)
  invisible(xk2)

}
