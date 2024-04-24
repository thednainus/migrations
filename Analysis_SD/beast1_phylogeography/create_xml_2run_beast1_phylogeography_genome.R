#script to read an alignment and add the bits missing to create a xml file
#to run discrete phylogeography in beast1

library(ape)
library(stringr)
library(SDBenefits)

#get file with sequences in FASTA format
fastafn <- list.files(path = ".", pattern = "*.fasta")
#fastafn <- "test_ali/model0_ali_1000bp.fasta"


#load xml TEMPLATE to run beast2 phydyn
xmlfn <- system.file("xml_template/discrete_phylogeography_genome_TEMPLATE.xml", package="SDBenefits")

#save xml file to disk
format_xml_beast1(xmlfn, fastafn)
