library(tidyverse)
library(rvest)
library(stringr)

countries <- read_html("https://en.wikipedia.org/wiki/Member_state_of_the_European_Union") %>%
    html_table(fill=TRUE) %>%   # generates a list
    nth(2) %>%                  # get second element of the list
    as_tibble() %>%             # make it a tibble (data_frame)
    pull(`Country name`) %>% 
    str_trim() %>% 
    str_replace("\\[.*\\]", "") %>% # omit footnotes
    str_replace(" ", "") %>%        # collapse words
    str_replace("Republic", "ia") %>% 
    tolower() %>% 
    c(., "iceland", "norway", "switzerland")

links <- paste0("http://www.parties-and-elections.eu/", countries, ".html")

first_row_to_names <- function(x) {
    names(x) <- x[1, ]
    names(x)[which(names(x) == "" | is.na(names(x)))] <- paste0("v", 1:length(which(names(x) == "" | is.na(names(x)))))
    names(x)[which(duplicated(names(x)))] <- paste0(names(x)[which(duplicated(names(x)))], ".1")
    x <- x[-1, ]
    return(x)
}

archive <- map_df(countries, function(country) {
    country_page <- paste0("http://www.parties-and-elections.eu/", country, ".html")
    last_two <- read_html(country_page) %>%
        html_nodes("table:nth-child(6) td") %>%
        html_text()
    
    cat("Processing", country, "archives \n")
    archive_links <- read_html(country_page) %>% 
        html_nodes(".bottom") %>% 
        html_attr("href") %>% 
        paste0("http://www.parties-and-elections.eu/", .)
    
    archive <- map_df(archive_links, function(a_link) {
        tab0 <- read_html(a_link) %>%
            html_nodes("td td div table:nth-child(3) td") %>%
            html_text()
        tab0[1] <- "percent"
        no_yrs <- tab0 %>% str_subset("((19)|(20))\\d{2}") %>% length()
        votes <- tab0 %>%
            c("party", .) %>%
            matrix(ncol = no_yrs + 2, byrow = TRUE) %>%
            as_tibble() %>%
            first_row_to_names() %>%
            filter(percent == "%") %>%
            filter(!(party == "Others" | party == "Turnout")) %>%
            select(-percent) %>%
            gather(key = year, value = vote_share, -party) %>%
            arrange(party) %>%
            filter(!str_detect(vote_share, "^[A-Z]")) %>% # omit election if party ran in coalition
            mutate(vote_share = if_else(str_detect(vote_share, "\\d"), 
                                        as.numeric(str_replace(vote_share, ",", ".")),
                                        0),
                   party = str_replace(party, "(.*)\\r\\n\\W*(.*)", "\\1 \\2")) # from rebranding_scrape, jic

        notes <- read_html(a_link) %>% 
            html_nodes("td td td div font") %>% 
            html_text() %>% 
            gsub(x = ., ".*Abbreviations:\\W(.*)©.*", "\\1") %>% 
            gsub(x = ., "\\r\\n", "")
        if (country=="iceland") notes <- gsub(pattern="BF \\(([12])\\)", replacement="BF\\1", x=notes)
        if (country=="denmark") notes <- gsub(pattern="\\((since [0-9]{4})\\)", replacement="\\1", x=notes)
        # if (country=="italy" & (links2[i] %in% links3)) notes <- gsub(pattern="^(.*)\\*.*", replacement="\\1", x=notes)
        notes0 <- notes
        notes <- unlist(strsplit(notes, "\\)"))  
        # the following is only lightly edited from rebranding_scrape.R (i.e., ooold school)
        if (length(notes) > 1) { # as long as there is at least one rebranded party . . .
            notes <- gsub(pattern="^; ", replacement="", x=notes)
            
            changed_parties <- gsub(pattern="^(.*):[^;]* \\(.*", replacement="\\1", x=notes) # First, get the current acronym
            changed_parties <- gsub(pattern=".*;\\W*(.*)", replacement="\\1", x=changed_parties)
            changed_parties <- changed_parties[1:length(changed_parties)-1] # because of leftover tail of string after splitting	
            changed_parties <- gsub(pattern=".*;\\W*(.*)", replacement="\\1", x=changed_parties)
            changed_parties <- gsub(pattern="^\\W*(.*)", replacement="\\1", x=changed_parties)
            
            c_p <- gsub(pattern="^(.*):[^;]* (\\(.*)", replacement="\\1 \\2", x=notes)  # then get all old acronyms
            c_p <- c_p[1:length(c_p)-1] # because of leftover tail of string after splitting	
            c_p <- gsub(pattern=".*\\((.*)", replacement="\\1, ", x=c_p) # drop anything preceding an open paren (e.g., parties w/o name changes)
            c_p <- gsub(pattern="[^,]*,\\W+([^,;]*)[,;][^,]*", replacement=" \\1,", x=c_p) # retain only acronyms
            c_p <- gsub(pattern=".*:[^,]*,([^,;]*)[,;][^,]*", replacement=" \\1,", x=c_p) # retain only acronyms, second pass
            c_p <- gsub(pattern=" (.*),", replacement="(\\1)", x=c_p) # drop trailing comma and surround with parens
            if(country=="germany") c_p <- gsub(pattern=" Party for Unity,", replacement="", x=c_p) # kludge for Germany
            if(country=="norway") c_p <- gsub(pattern="\\( FMS\\)", replacement="(FMS, RV)", x=c_p) # kludge for Norway
            if(country=="sweden") c_p <- gsub(pattern="\\(KDS, KDS\\)", replacement="(KDS)", x=c_p) # kludge for Sweden
            c_p <- gsub("((\\b\\w+), \\2,)", "\\2,", x=c_p) # to delete repeated acronyms when name changes but acronym is retained		
            changed_parties <- paste(changed_parties, c_p, sep=" ") # put new and old acronyms together
            
            changed_parties <- gsub(pattern="(^(.*\\b) \\(.*)((?<!\\-)\\b\\2\\b(?!(\\-|\\+)))(.*)", replacement="\\1\\5", x=changed_parties, perl=TRUE) # this bit gets rid of an old acronym if current party name has same acronym
            changed_parties <- gsub(pattern="\\(, ", replacement="(", x=changed_parties)
            changed_parties <- gsub(pattern=" \\($", replacement="", x=changed_parties)
            changed_parties <- gsub(pattern=", ,", replacement=",", x=changed_parties)
            changed_parties <- gsub(pattern=", \\)", replacement=")", x=changed_parties)
            changed_parties <- gsub(pattern=",\\)", replacement=")", x=changed_parties)
            changed_parties <- gsub(pattern="\\(\\)", replacement=")", x=changed_parties)
            changed_parties <- gsub(pattern="\\)\\W*", replacement=")", x=changed_parties)
            changed_parties <- gsub(pattern=" \\)$", replacement="", x=changed_parties)
            
            if(country=="unitedkingdom") changed_parties[which(changed_parties=="GP (EP)")] <- "GP" # catches bug in Britian table
            if(country=="norway") changed_parties[which(changed_parties=="SV (SF)")] <- "SV (SF, SV)" # catches bug in Norway table
            if(country=="sweden") changed_parties[which(changed_parties=="V (SKP, VPK)")] <- "V (SKP, VKP)" # catches bug in Sweden table
            if(country=="moldova") changed_parties[which(changed_parties=="PPCD (FPM, FPCD)")] <- "PPCD (FPCD, FPM)" # catches bug in Moldova table
            if(country=="moldova") changed_parties[which(changed_parties=="PFD (CI + Christian-Democratic Party of Moldova)")] <- "PFD (CI-PDCM)" # kludge for Moldova 
            if(country=="hungary") changed_parties[which(changed_parties=="FIDESZ (FIDESZ)")] <- "FIDESZ" # kludge for Hungary 
            if(country=="latvia") changed_parties[which(changed_parties=="LSDSP (DT, LSDA, A)")] <- "LSDSP (DT, LSDA)" # kludge for Latvia 
            if(country=="poland") changed_parties[which(changed_parties=="ChD-SP (SP, ChD)")] <- "ChD-SP (ChD)" # kludge for Poland 
            if(country=="romania") changed_parties[which(changed_parties=="CDR 2000:(CDR 2000)")] <- "CDR" # kludge for Romania 
            if(country=="slovakia") changed_parties[which(changed_parties=="LS-HZDS (HZDS, HZDS-RSS)")] <- "LS-HZDS (HZDS-RSS, HZDS)" # bug fix for Slovakia 
            if(country=="italy") changed_parties[which(changed_parties=="1968: Unified Socialist Party, PSU (PSIUP, PSU)")] <- "PSI (PSIUP, PSU)" # kludge for Italy 
            if(country=="spain") changed_parties[which(changed_parties=="CiU (PDPC + Centre Union)")] <- "CiU (PDPC, UDC)" # kludge for Spain 
            if(country=="switzerland") changed_parties[which(changed_parties=="PS (AP)")] <- "FPS (AP)" # kludge for Switzerland 
            if(country=="estonia") changed_parties[which(changed_parties=="EPPL (EPL)")] <- "EPPL" # kludge for Estonia 
            if(country=="bulgaria") changed_parties[which(changed_parties=="BZNS-NP (BRSDP-O)")] <- "BZNS-NP (BZNS-NP/BRSDP-O)" # bug fix for Bulgaria 
            
            change_years <- str_extract_all(notes, "([0-9]{4}|[0-9]{4}\\-[0-9]{4})")
            change_years <- change_years[1:length(change_years)-1] # because of leftover tail of string after splitting
            last_year <- lapply(change_years, function(x) gsub(pattern=".*\\-([0-9]{4})$","\\1", x)[length(x)])
            change_years <- lapply(change_years, function(x) gsub(pattern="\\-[0-9]{4}$","", x))
            
            years2 <- unique(change_years)
            last_change <- lapply(last_year, function(x) years2[which(years2==x)+1])
            
            change_years <- lapply(change_years, function(x) paste(x, collapse=","))
            change_years <- lapply(change_years, function(x) gsub(pattern="^[0-9]{4},?","", x)) # first year of party isn't a *re-*branding
            change_years <- paste(change_years, last_change, sep=",")
            change_years <- lapply(change_years, function(x) gsub(pattern="^,","", x)) 
            change_years <- strsplit(as.character(change_years), ",")
            
            changes <- cbind(party=rep(changed_parties, sapply(change_years, length)), year=unlist(change_years), change=1)
            
            c_data <- merge(as.data.frame(votes), as.data.frame(changes), by=c("party", "year"), all=TRUE)
            c_data$change <- as.numeric(c_data$change)
            c_data$change[is.na(c_data$change)] <- 0
            c_data$country <- gsub(pattern="\\b([a-z])", replacement="\\U\\1", x=as.character(country), perl=TRUE)
        } else { # if no rebranded parties . . .
            c_data <- votes
            c_data$change <- 0
            c_data$country <- gsub(pattern="\\b([a-z])", replacement="\\U\\1", x=as.character(country), perl=TRUE)
        }
        c_data <- as_tibble(c_data)
        return(c_data)
    })

    return(archive)
})
