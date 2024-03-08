#script to read an alignment and add the bits missing to create a xml file
#to run phydyn (https://github.com/mrc-ide/PhyDyn)
#in beast2 (https://www.beast2.org/)

library(ape)
library(stringr)
library(SDBenefits)

#get file with sequences in FASTA format
fastafn <- list.files(path = ".", pattern = "*.fasta")
#fastafn <- "test_ali/model0_ali_1000bp.fasta"
fastafn <- "test_ali/test_ali.fasta"

#load xml TEMPLATE to run beast2 phydyn
xmlfn <- system.file("xml_template/beast_phydyn_strictClock_TEMPLATE.xml", package="SDBenefits")

#save xml file to disk
format_xml_phydyn(xmlfn, fastafn)
