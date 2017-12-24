library(tidyverse)
library(rvest)

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

first_row_to_names <- function(x) {
    names(x) <- x[1, ]
    names(x)[which(names(x) == "" | is.na(names(x)))] <- paste0("v", 1:length(which(names(x) == "" | is.na(names(x)))))
    names(x)[which(duplicated(names(x)))] <- paste0(names(x)[which(duplicated(names(x)))], ".1")
    x <- x[-1, ]
    return(x)
}

correct_for_alliances <- function(df) {
    # first correct for alliances indicated by merged cells
    correct_merged <- function(df, vote_variable) { 
        seat_variable <- paste0("X", str_extract(vote_variable, "\\d") %>% as.numeric() + 1)
        alliance_members <- tibble(members = rle(df[[vote_variable]])$length,
                                   vote_total = rle(df[[vote_variable]])$values)
        alliance_members2 <- alliance_members[rep(1:nrow(alliance_members),
                                                  alliance_members$members), ] %>% 
            mutate(vvv = df[[vote_variable]],
                   seats = str_extract(df[[seat_variable]], "\\d+") %>% as.numeric(),
                   a_code = as.numeric(as.factor(df[[vote_variable]]))) %>% 
            group_by(a_code, members) %>% 
            mutate(seats = if_else(is.na(seats), 0, seats),
                   alliance_seats = sum(seats),
                   votes = if_else(!(alliance_seats == 0 | !str_detect(vvv, "\\d")), 
                                   suppressWarnings(vote_total %>% 
                                                        str_trim() %>% 
                                                        str_replace(",", ".") %>% 
                                                        str_replace("%", "") %>% 
                                                        as.numeric() * (seats/alliance_seats)) %>% 
                                       round(1) %>% 
                                       as.character(),
                                   vvv %>% 
                                       str_trim() %>% 
                                       str_replace(",", ".") %>% 
                                       str_replace("%", "")))
        df[[vote_variable]] <- alliance_members2$votes
        return(df)
    }
    df <- df %>% 
        correct_merged("X4") %>% 
        correct_merged("X6")
    
    # then, correct for alliances listed by acronym in year 1
    ally_names1 <- df %>% pull(X6) %>% str_subset("[A-Z]") %>% unique()
    if (length(ally_names1 > 0)) {
        alliances1 <- map_df(ally_names1, function(a_name) {
            df %>% 
                filter(str_detect(party, a_name)) %>% 
                transmute(alliance_name1 = party,
                          alliance_seats1 = str_extract(X7, "\\d+") %>% as.numeric(),
                          alliance_votes1 = X6 %>% 
                              str_trim() %>% 
                              str_replace(",", ".") %>% 
                              str_replace("%", "") %>% 
                              as.numeric(),
                          ally1 = a_name)
        } )                    
        df <- df %>%
            left_join(alliances1, by = c("X6" = "ally1")) %>% 
            mutate(X7 = as.numeric(str_extract(X7, "\\d+")),
                   X6 = if_else(!is.na(alliance_name1), 
                                round(alliance_votes1 * (X7/alliance_seats1), 1) %>% as.character(),
                                X6))
    }
    
    # and then correct for alliances listed by acronym in year 2
    ally_names2 <- df %>% pull(X4) %>% str_subset("[A-Z]") %>% unique()
    if (length(ally_names2 > 0)) {
        alliances2 <- map_df(ally_names2, function(a_name) {
            df %>% 
                filter(str_detect(party, a_name)) %>% 
                transmute(alliance_name2 = party,
                          alliance_seats2 = str_extract(X5, "\\d+") %>% as.numeric(),
                          alliance_votes2 = X4 %>% 
                              str_trim() %>% 
                              str_replace(",", ".") %>% 
                              str_replace("%", "") %>% 
                              as.numeric(),
                          ally2 = a_name)
        } )                    
        df <- df %>%
            left_join(alliances2, by = c("X4" = "ally2")) %>% 
            mutate(X5 = as.numeric(str_extract(X5, "\\d+")),
                   X4 = if_else(!is.na(alliance_name2), 
                                round(alliance_votes2 * (X5/alliance_seats2), 1) %>% as.character(),
                                X4))
    }

    return(df)
}

change_data <- map_df(countries, function(country) {
    cat("Processing", country, "\n")
    
    country_page <- paste0("http://www.parties-and-elections.eu/", country, ".html") %>% 
        read_html()
    
    last_two_years <- country_page %>%
        html_nodes("table:nth-child(6) td") %>%
        html_text() %>%
        str_trim() %>% 
        str_subset("^\\d{4}") %>% 
        str_replace_all("\\s", "")
    
    last_two0 <- country_page %>%
        html_nodes(xpath = "//table[@border = '1']") %>%
        html_table(fill = TRUE) %>% 
        first() %>% 
        select(X2, X4:X7) %>% 
        filter(!(str_detect(X2, "Others") | str_detect(X2, "Turnout") | str_detect(X2, "Unfilled"))) %>% 
        filter(str_detect(X2, "\\(")) %>% 
        mutate(current = str_extract(X2, "((?<=\\()(.*)(?=\\)))|((?<=\\()Open\\s+[-+/A-Z]{2,}(?=\\)))") %>% 
                   str_replace("Open\\s+", "OPEN ") %>% 
                   str_replace("Č", "C") %>% 
                   str_replace("Ú", "U"),
               old = str_extract(X2, "(?<![\\S])[-+/A-Z]{2,}(?![^]])"),
               party = if_else(is.na(old), current, paste0(current, " (", old, ")"))) %>%
        correct_for_alliances() %>% 
        select(X4, X6, party) 
    names(last_two0)[1:2] <- last_two_years
    last_two <- last_two0 %>%
        gather(key = election, value = vote_share, -party) %>% 
        mutate(vote_share = suppressWarnings(as.numeric(vote_share %>% 
                                           str_trim() %>% 
                                           str_replace(",", ".") %>% 
                                           str_replace("%", ""))),
               year = str_extract(election, "\\d{4}"),
               country = gsub(pattern="\\b([a-z])", replacement="\\U\\1", x=as.character(country), perl=TRUE) %>% 
                   str_replace("kingdom", " Kingdom"),
               change = as.numeric(str_detect(party, "\\(") & election == last_two_years[1]),
               recent = 1) %>% 
        select(country, party, election, year, vote_share, change, recent) %>% 
        mutate(bridge_name = if_else(str_detect(party, "\\("), 
                                     str_extract(party, "(?<=\\()(.*)(?=\\))"),
                                     party))
    
    archive_links <- country_page %>% 
        html_nodes(".bottom") %>% 
        html_attr("href") %>% 
        paste0("http://www.parties-and-elections.eu/", .)
    
    archive_votes <- map_df(archive_links, function(a_link) {
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
            filter(str_detect(percent, "%")) %>%
            filter(!(str_detect(party, "Others") | str_detect(party, "Turnout") | str_detect(party, "Miscellaneous"))) %>%
            select(-percent) %>%
            group_by(party) %>% 
            mutate(name_count = as.factor(party) %>% as.numeric() %>% cumsum()) %>%
            ungroup() %>% 
            mutate(party = if_else(name_count > 1,
                                   paste0(party, name_count),
                                   party)) %>% 
            select(-name_count) %>% 
            gather(key = election, value = vote_share, -party) %>%
            arrange(party) %>%
            filter(!str_detect(vote_share, "^[A-Z]")) %>% # omit election if party ran in coalition
            mutate(vote_share = if_else(str_detect(vote_share, "\\d"), 
                                        suppressWarnings(vote_share %>% 
                                                             str_trim() %>%
                                                             str_replace(",", ".") %>% 
                                                             str_replace_all("[()]", "") %>%
                                                             as.numeric()),
                                        0),
                   party = str_replace(party, "(.*)\\r\\n\\W*(.*)", "\\1 \\2"),
                   year = str_extract(election, "\\d{4}")) 
    })
    
    archive_votes <- archive_votes %>% 
        mutate(party1 = str_extract(party, "^\\S*")) %>% 
        select(-party)

    # Fix continuity problems across two-page archives
    if (country == "denmark") {
        archive_votes <- archive_votes %>% 
            mutate(party1 = str_replace(party1, "KRF", "KD"))
    }
    if (country == "greece") {
        archive_votes <- archive_votes %>% 
            mutate(party1 = str_replace(party1, "^EK$", "EDIK"))
    }    
    if (country == "italy") {
        archive_votes <- archive_votes %>% 
            mutate(party1 = str_replace(party1, "^(PCI)|(DS)|(ULIVO)", "PD"),
                   str_replace(party1, "^(DL)|(PPI)", "DL"))
    }
    
    election_years <- archive_votes %>% 
        pull(year) %>%
        c(last_two_years) %>% 
        unique()
    
    archive_changes <- map_df(archive_links, function(a_link) {    
        notes <- read_html(a_link) %>% 
            html_nodes("td td td div font") %>% 
            html_text() %>% 
            paste(collapse = "") %>% 
            gsub(x = ., ".*Abbreviations:\\W(.*)(©|Wolfram).*", "\\1") %>% 
            gsub(x = ., "\\r\\n", "")
        if (country=="denmark") notes <- gsub(pattern="\\((since [0-9]{4})\\)", replacement="\\1", x=notes)
        if (country=="ireland") notes <- gsub(pattern="\\(Family[^)]*\\)", replacement="", x=notes)
        notes <- unlist(strsplit(notes, "\\)"))  
        # the following is only lightly edited from rebranding_scrape.R (i.e., ooold school, and I didn't check if all fixes are still needed)
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
            if(country=="belgium") c_p <- gsub(pattern="\\+ Liberal Party", replacement="+ PL", x=c_p) # kludge for Belgium
            if(country=="germany") c_p <- gsub(pattern=" Party for Unity,", replacement="", x=c_p) # kludge for Germany
            if(country=="norway") c_p <- gsub(pattern="\\( FMS\\)", replacement="(FMS, RV)", x=c_p) # kludge for Norway
            if(country=="sweden") c_p <- gsub(pattern="\\(KDS, KDS\\)", replacement="(KDS)", x=c_p) # kludge for Sweden
            c_p <- gsub("((\\b\\w+), \\2,)", "\\2,", x=c_p) # to delete repeated acronyms when name changes but acronym is retained		
            c_p <- gsub(",\\)", ")", x=c_p)
            changed_parties <- paste(changed_parties, c_p, sep=" ") # put new and old acronyms together
            
            # this bit gets rid of an old acronym if current party name has same acronym
            changed_parties <- gsub(pattern="(^(.*\\b) \\(.*)((?<!\\-)\\b\\2\\b(?!(\\-|\\+)))(.*)",
                                    replacement="\\1\\5", x=changed_parties, perl=TRUE) 
            changed_parties <- gsub(pattern="\\(, ", replacement="(", x=changed_parties)
            changed_parties <- gsub(pattern=" \\($", replacement="", x=changed_parties)
            changed_parties <- gsub(pattern=", ,", replacement=",", x=changed_parties)
            changed_parties <- gsub(pattern=", \\)", replacement=")", x=changed_parties)
            changed_parties <- gsub(pattern=",\\)", replacement=")", x=changed_parties)
            changed_parties <- gsub(pattern="\\(\\)", replacement=")", x=changed_parties)
            changed_parties <- gsub(pattern="\\)\\W*", replacement=")", x=changed_parties)
            changed_parties <- gsub(pattern=" \\)$", replacement="", x=changed_parties)
            
            changed_parties <- gsub(pattern = "\\s+", replacement = " ", x = changed_parties) # fix whitespace
            
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
            
            change_years <- str_extract_all(notes, "([0-9]{4}\\-[0-9]{4}|[0-9]{4})")
            change_years <- change_years[1:length(change_years)-1] # because of leftover tail of string after splitting
            change_years <- lapply(change_years, function(x) gsub(pattern="^([0-9]{4})$","\\1-\\1", x))
            last_year <- lapply(change_years, function(x) gsub(pattern=".*\\-([0-9]{4})$","\\1", x)[length(x)])
            change_years <- lapply(change_years, function(x) gsub(pattern="\\-[0-9]{4}$", "", x))
            
            last_change <- lapply(last_year, function(x) election_years[which(election_years==x)+1])
            
            change_years <- lapply(change_years, function(x) paste(x, collapse=","))
            change_years <- paste(change_years, last_change, sep=",")
            change_years <- lapply(change_years, function(x) gsub(pattern="^,","", x)) 
            change_years <- strsplit(as.character(change_years), ",")
            
            changes <- cbind(party=rep(changed_parties, sapply(change_years, length)), year=unlist(change_years), change=1) %>% 
                as_tibble()
        }
        return(changes)
    })
    
    if (country == "belgium") {
        archive_changes <- archive_changes %>% 
            mutate(party = str_replace(party, "FDF \\(FDF-RW\\)", "FDF (FDF-RW, FDF-PLDP)") %>% 
                       str_replace("^VU.*", "VU (CVV, VU-ID21)"))
    }
    if (country == "greece") {
        archive_changes <- archive_changes %>% 
            mutate(party = str_replace(party, "^(EK \\(EK-KP\\))|(EDIK \\(EK\\))", "EDIK (EK-KP, EK)"))
    }
    if (country == "italy") {
        archive_changes <- archive_changes %>% 
            mutate(party = str_replace(party, "(^PCI.*)|(^DS.*)|(^ULIVO.*)", "PD (FDP, PCI, PDS, DS, ULIVO)") %>% 
                       str_replace("^LN.*", "LN (LL, LN-MPA)") %>% 
                       str_replace("^(DL.*)|(PPI.*)", "DL (PRODI, PPI)"))
    }
    
    archive_changes <- archive_changes %>% 
        mutate(party1 = str_extract(party, "^\\S*")) %>% 
        distinct()
    
    c_data0 <- left_join(archive_votes, archive_changes %>%
                             select(party1, party) %>% 
                             distinct(), by = "party1") %>% 
        left_join(archive_changes %>% 
                      select(-party), by = c("party1", "year")) %>% 
        mutate(party = if_else(is.na(party), party1, party),
               change = if_else(is.na(change), 0L, as.integer(change)))
    
    bridge <- c_data0 %>% 
        transmute(bridge_name = str_extract(party, "^[^(\\s]*") %>% 
                      str_replace("OPEN", "OPEN VLD") %>% 
                      str_replace("DIE", "DIE LINKE"),
               archive_party = party) %>% 
        distinct() %>% 
        full_join(last_two %>% 
                      transmute(bridge_name = if_else(party == "PSD (USL)", "PSD", bridge_name), # fix for Romania
                                l2_party = party) %>% 
                      distinct(), by = "bridge_name") %>% 
        mutate(cons_party = if_else(bridge_name == l2_party, archive_party,        # no changes in last two, so archive_party works
                                if_else(bridge_name == archive_party, l2_party,    # no changes in archive, so l2_party works
                                        paste0(str_replace(l2_party, "\\)", ", "), # changes in both, so combine them
                                               str_extract(archive_party, "(?<=\\().*")))))
     
    c_data <- c_data0 %>% 
         mutate(country = gsub(pattern="\\b([a-z])", replacement="\\U\\1", x=as.character(country), perl=TRUE) %>% 
                    str_replace("kingdom", " Kingdom"),
                bridge_name = str_extract(party, "^[^(\\s]*")) %>% 
        left_join(bridge, by = "bridge_name") %>% 
        mutate(party = if_else(!is.na(cons_party), cons_party, party)) %>% 
        select(country, party, election, year, vote_share, change) %>% 
        bind_rows(last_two %>% 
                      mutate(bridge_name = if_else(party == "PSD (USL)", "PSD", bridge_name),
                             change = if_else(party == "PSD (USL)" & year == 2012, 1, change)) %>%  # fix for Romania
                      left_join(bridge, by = "bridge_name") %>% 
                      mutate(party = if_else(!is.na(cons_party), cons_party, party),
                             vote_share = if_else(is.na(vote_share), 0, vote_share)) %>% 
                      select(country, party, election, year, vote_share, change)) %>% 
        mutate(year = as.numeric(year)) %>% 
        arrange(country, party, election) %>% 
        distinct()
    
    if (country == "belgium") {
        c_data <- c_data %>%
            mutate(vote_share = if_else((party == "VU (CVV, VU-ID21)" & year == 1954), 3.9, vote_share))
    }
    if (country == "denmark") {
        c_data <- c_data %>%
            mutate(change = if_else((party == "KD (KRF)" & year == 1979), 0, change))
    }
    if (country == "italy") {
        c_data <- c_data %>% 
            mutate(party = str_replace(party, "^FI$", "FI (PDL)") %>% 
                       str_replace("(^PRC$)|(SEL \\(SA\\))", "SEL (PRC, SA)"),
                   change = if_else((party == "PD (FDP, PCI, PDS, DS, ULIVO)" & (year == 1948 |
                                                                                     year == 1950 |
                                                                                     year == 1994 |
                                                                                     year == 1996 |
                                                                                     year == 2006 |
                                                                                     year == 2008)) |
                                        (party == "LN (LL, LN-MPA)" & (year == 1992 |
                                                                           year == 2006 |
                                                                           year == 2008)) |
                                        (party == "FI (PDL)" & (year == 2008 |
                                                                    year == 2013)) |
                                        (party == "DL (PRODI, PPI)" & (year == 1996 |
                                                                           year == 1998)) |
                                        (party == "SEL (PRC, SA)" & (year == 2008 |
                                                                         year == 2013)),
                                    1,
                                    change))
    }
    
    return(c_data)
}) 

change_data0 <- change_data

max_names <- change_data0 %>% 
    pull(party) %>% 
    strsplit("( \\()|,") %>% 
    map_int(~ length(.x)) %>% 
    max()

change_data <- change_data0 %>% 
    mutate(party = str_replace(party, "Ö", "O") %>% 
               str_replace("Ü", "U") %>% 
               str_replace("\\+([A-Z])", "-\\1") %>% 
               str_replace(",+", ",")
           ) %>% 
    group_by(country, party) %>% 
    mutate(running_vote = cumsum(vote_share)) %>% 
    filter(!running_vote == 0) %>% # drop party-elections when party has not yet received any votes (not formed yet)
    mutate(change = if_else(vote_share == running_vote, 0, change)) %>% # first election is not a *re*branding
    arrange(country, party, -year) %>% 
    mutate(running_vote = cumsum(vote_share)) %>%  
    filter(!running_vote == 0 ) %>% # drop party-elections when party never receives votes again (disbanded)
    select(-running_vote) %>% 
    ungroup() %>% 
    arrange(country, party, year) %>% 
    # create variables capturing every acronym the party has ever had separately
    separate(party, into = paste0("name", 1:max_names), sep = "( \\()|,|\\)", remove = FALSE, fill = "right") %>% 
    mutate_at(vars(matches("name\\d+")), funs(if_else(. == "", NA_character_, .))) %>% 
    select(-matches(paste0("name", max_names)))

# save
write_csv(change_data, "data/change_data.csv")
save(change_data, file="data/change_data.rda")


