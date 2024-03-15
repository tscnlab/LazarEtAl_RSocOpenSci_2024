## Zenodo deposit; see vignette("codecheck_overview.Rmd")

library("codecheck")
library(yaml)
library(zen4R)
## This assumes your working directory is the codecheck directory
metadata = read_yaml( "../codecheck.yml")

## To interact with the Zenodo API, you need to create a token.  This should
## not be shared, or stored in this script.  Here I am using the Unix password
## tool pass to retrieve the token.
my_token = system("pass show codechecker-token", intern=TRUE)

## make a connection to Zenodo API
zenodo <- ZenodoManager$new(token = my_token)


## If you wish to create a new record on zenodo, run the following line once
## and then store the URL of the record in  ../codecheck.yml
## This will generate a new record every time that you run it and 
## save the new record ID in the codecheck configuration file:
## record = create_zenodo_record(zenodo); metadata = read_yaml( "../codecheck.yml")

record = get_zenodo_record(metadata$report)
codecheck:::set_zenodo_metadata(zenodo, record, metadata)
set_zenodo_metadata1(zenodo, record, metadata)

## If you have already uploaded the certificate once, you will need to
## delete it via the web page before uploading it again.
## codecheck:::set_zenodo_certificate(zenodo, record, "codecheck.pdf")

## You may also create a ZIP archive of of any data or code files that
## you think should be included in the CODECHECK's record.

## Now go to zenodo and check the record (the URL is printed
## by set_zenodo_metadata() ) and then publish.

set_zenodo_metadata1 <- 
function (zen, record, metadata) 
{
    draft <- zen$getDepositionById(record)
    if (is.null(draft)) {
        draft <- zen$getRecordById(record)
    }
    if (is.null(draft)) 
        stop("Neither deposition nor record found for ID ", record)
    draft$setPublicationType("report")
    draft$setCommunities(communities = c("codecheck"))
    draft$setTitle(paste("CODECHECK certificate", metadata$certificate))
    draft$setLanguage(language = "eng")
    draft$metadata$creators = NULL
    num_creators = length(metadata$codechecker)
    for (i in 1:num_creators) {
        draft$addCreator(name = metadata$codechecker[[i]]$name, 
            orcid = metadata$codechecker[[i]]$ORCID)
    }
    description_text = paste("CODECHECK certificate for paper:", 
        metadata$paper$title)
    repo_url = gsub("[<>]", "", metadata$repository)
    description_text = paste(description_text, sprintf("<p><p>Repository: <a href=\"%s\">%s</a>", 
        repo_url, repo_url))
    draft$setDescription(description_text)
    draft$setKeywords(keywords = c("CODECHECK"))
    draft$setNotes(notes = c("See file LICENSE for license of the contained code. The report document codecheck.pdf is published under CC-BY 4.0 International."))
    draft$setAccessRight(accessRight = "open")
    ##draft$setLicense(licenseId = "other-open")
    draft$addRelatedIdentifier(relation = "isSupplementTo", identifier = metadata$repository)
    draft$addRelatedIdentifier(relation = "isSupplementTo", identifier = metadata$paper$reference)
    draft <- zen$depositRecord(draft)
    cat(paste0("Check your record online at https://zenodo.org/deposit/", 
        record, "\n"))
}
