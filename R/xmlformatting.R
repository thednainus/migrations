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
#' @return Character string of runnable XML Individual XMLs for each starting
#'    tree will be written to disk
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
