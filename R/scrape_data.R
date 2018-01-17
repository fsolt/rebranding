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
    correct_merged <- function(df, vote_variable) { 
        seat_variable <- paste0("X", str_extract(vote_variable, "\\d") %>% as.numeric() + 1)
        
        # fix for alliances indicated by lead cell (limited by country just to ensure it doesn't break others)
        if (any(str_detect(df[[seat_variable]], "\\(")) & (country == "spain")) {
            df <- df %>%
                mutate(vv = df[[vote_variable]],
                       vv1 = df[[vote_variable]],
                       vv1 = if_else(str_detect(df[[seat_variable]], "\\(") &
                                         str_detect(df[[vote_variable]], "-"),
                                     NA_character_,
                                     vv1)) %>% 
                fill(vv1) %>% 
                mutate(vv = if_else(str_detect(df[[seat_variable]], "\\(") &
                                        str_detect(df[[vote_variable]], "-"),
                                    vv1,
                                    if_else(vv1 == lead(vv1) & vv1 != last(vv1), NA_character_, vv)))
            df[[vote_variable]] <- df$vv
        }
        
        # correct for alliances indicated by merged cells (limited by country to avoid 'correcting' coincidental similar results)
        if (country == "bulgaria" | country == "croatia" | country == "greece" |
            country == "hungary" | country == "portugal" | country == "spain") {
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
        }
        
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
        return(df)
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
               party = str_replace(party, "Ž", "Z")) %>% 
        select(country, party, election, year, vote_share, change) %>% 
        mutate(bridge_name = if_else(str_detect(party, "\\("), 
                                     str_extract(party, "(?<=\\()(.*)(?=\\))"),
                                     party))
    
    # Fix continuity problems for Cyprus
    if (country == "cyprus") {
        last_two <- last_two %>%
            mutate(bridge_name = if_else(party == "KS-EDEK (EDEK)", "KS-EDEK", bridge_name))
    }
    
    # Fix continuity problems for Norway
    if (country == "norway") {
        last_two <- last_two %>%
            mutate(party = if_else(party == "AP", "AP (DNA)", party),
                   bridge_name = if_else(party == "AP (DNA)", "DNA", bridge_name))
    }
    
    # Fix continuity problems for Romania
    if (country == "romania") {
        last_two <- last_two %>%
            mutate(bridge_name = if_else(party == "PSD (USL)", "PSD", bridge_name),
                   bridge_name = if_else(party == "PDL (ARD)", "PDL", bridge_name),
                   change = if_else((party == "PSD (USL)" | party == "PDL (ARD)") & year == 2012, 1,
                                    change))
    }
    
    # Fix continuity problems for Spain
    if (country == "spain") {
        last_two <- last_two %>%
            mutate(bridge_name = if_else(party == "IU (UP)", "IU", bridge_name),
                   bridge_name = if_else(party == "CDC", "CiU", bridge_name),
                   change = if_else((party == "IU (UP)" | party == "CDC") & 
                                        (year == 2015 | year == 2016), 
                                    1,
                                    change))
    }
    
    # Fix for duplicated 2011 election in Switzerland
    if (country == "switzerland") {
        last_two <- last_two %>%
            filter(!year == "2011")
    }
    
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
                                                             str_replace_all("[()*]", "") %>%
                                                             as.numeric()),
                                        0),
                   party = str_replace(party, "(.*)\\r\\n\\W*(.*)", "\\1 \\2"),
                   year = str_extract(election, "\\d{4}"))
        return(votes)
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
            mutate(party1 = str_replace(party1, "^(PCI)|(DS)|(ULIVO)", "PD") %>% 
                   str_replace("^(DL)|(PPI)", "DL")) %>% 
            filter(vote_share > 0)
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
            if(country=="poland") changed_parties[which(changed_parties=="UP (SP, SDPL-SDPL, et al.)")] <- "UP (SP, SDPL)" # kludge for Poland 
            if(country=="romania") changed_parties[which(changed_parties=="CDR 2000:(CDR 2000)")] <- "CDR" # kludge for Romania 
            if(country=="slovakia") changed_parties[which(changed_parties=="LS-HZDS (HZDS, HZDS-RSS)")] <- "LS-HZDS (HZDS-RSS, HZDS)" # bug fix for Slovakia 
            if(country=="italy") changed_parties[which(changed_parties=="1968: Unified Socialist Party, PSU (PSIUP, PSU)")] <- "PSI (PSIUP, PSU)" # kludge for Italy 
            if(country=="spain") changed_parties[which(changed_parties=="CiU (PDPC + Centre Union)")] <- "CiU (PDPC, UDC)" # kludge for Spain 
            if(country=="switzerland") changed_parties[which(changed_parties=="PS (AP)")] <- "FPS (AP)" # kludge for Switzerland 
            if(country=="estonia") changed_parties[which(changed_parties=="EPPL (EPL)")] <- "EPPL" # kludge for Estonia 
            if(country=="bulgaria") changed_parties[which(changed_parties=="BZNS-NP (BRSDP-O)")] <- "BZNS-NP (BZNS-NP/BRSDP-O)" # bug fix for Bulgaria 
            
            change_years <- str_extract_all(notes, "([0-9]{4}\\-[0-9]{4}|[0-9]{4})")
            change_years <- change_years[1:length(change_years)-1] # because of leftover tail of string after splitting
            change_years <- lapply(change_years, function(x) gsub(pattern="^([0-9]{4})$","\\1-\\1", x)) # put single years in yyyy-yyyy format
            last_years <- lapply(change_years, function(x) gsub(pattern=".*\\-([0-9]{4})$","\\1", x)) #[length(x)])
            change_years <- lapply(change_years, function(x) gsub(pattern="\\-[0-9]{4}$", "", x))
            
            next_changes <- map(last_years, function(x) {map(x, function(xx) election_years[which(election_years==xx)+1])}) 
            next_changes <- lapply(next_changes, function(x) paste(x, collapse=","))
            
            change_years <- lapply(change_years, function(x) paste(x, collapse=","))
            change_years <- paste(change_years, next_changes, sep=",")
            change_years <- lapply(change_years, function(x) gsub(pattern="^,","", x)) 
            change_years <- strsplit(as.character(change_years), ",")
            
            changes <- cbind(party=rep(changed_parties, sapply(change_years, length)), year=unlist(change_years), change=1) %>% 
                as_tibble() %>% 
                distinct() %>% 
                arrange(party, year)
            
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
    
    if (country == "france") {
        c_data0 <- c_data0 %>% 
            mutate(party = str_replace(party, "FG \\(PCF\\)", "PCF"),
                   party1 = str_replace(party1, "FG", "PCF"))
    }
    
    if (country == "norway") {
        c_data0 <- c_data0 %>% 
            filter(!party == "H,")
    }
    
    bridge <- c_data0 %>% 
        transmute(bridge_name = str_extract(party, "^[^(\\s]*") %>% 
                      str_replace("OPEN", "OPEN VLD") %>% 
                      str_replace("DIE", "DIE LINKE"),
               archive_party = party) %>% 
        distinct() %>% 
        full_join(last_two %>% 
                      transmute(l2_party = party,
                                bridge_name = bridge_name) %>% 
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
    strsplit("( \\()|,|\\)") %>% 
    map_int(~ length(.x)) %>% 
    max()

change_data <- change_data0 %>% 
    mutate(country = countrycode::countrycode(country, "country.name", "country.name") %>% 
               if_else(str_detect(., "Kingdom"), "United Kingdom", .),
           party = str_replace(party, "Ö", "O") %>% 
               str_replace("Ü", "U") %>% 
               str_replace("\\+([A-Z])", "-\\1") %>% 
               str_replace(",+", ",")) %>% 
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
    select(-matches("name\\d")) %>% 
    # create variables capturing every acronym the party has ever had separately
    separate(party, into = paste0("name", 1:(max_names+1)), sep = "( \\()|,|\\)", remove = FALSE, fill = "right") %>% 
    mutate_at(vars(matches("name\\d+")), funs(if_else(. == "", NA_character_, .))) %>% 
    select(-matches(paste0("name", max_names+1))) %>% 
    group_by(country, party, year) %>% 
    mutate(elections = seq(n()),
           year1 = year + (elections - 1)/10) %>% 
    ungroup() %>% 
    select(-year) %>% 
    rename(year = year1)

# save
write_csv(change_data, "data/change_data.csv")
save(change_data, file="data/change_data.rda")


# Bormann and Golder Democratic Electoral System Data
des_link <- "http://mattgolder.com/files/research/es_data-v3.zip"
download.file(des_link, "data/es_data-v3.zip")
unzip("data/es_data-v3.zip", exdir = "data/es_data-v3") 

es <- read_csv("data/es_data-v3/es_data-v3.csv") %>% 
    mutate(country = str_replace(country, "West Germany", "Germany") %>% 
               countrycode::countrycode("country.name", "country.name"))

# ParlGov
#Get ParlGov data
enep <- read_csv("http://www.parlgov.org/static/data/development-utf-8/viewcalc_election_parameter.csv",
                 col_types = "iddddddT") %>% 
    transmute(election_id = election_id,
              enep = enp_votes) %>% 
    left_join(read_csv("http://www.parlgov.org/static/data/development-cp1252/view_election.csv",
                       col_types = "cccDdiicccdiiiii") %>%
                  select(election_id, country_name, election_date) %>% 
                  distinct(),
              by = "election_id") %>% 
    mutate(country = countrycode::countrycode(country_name, "country.name", "country.name"),
           year = lubridate::year(election_date)) %>% 
    filter(country %in% (countries %>% countrycode::countrycode("country.name", "country.name"))) %>% 
    mutate(country = if_else(str_detect(country, "Kingdom"), "United Kingdom", country))

last_cabinet <- read_csv("http://www.parlgov.org/static/data/development-cp1252/view_cabinet.csv") %>% 
    arrange(country_name, election_date, start_date) %>% 
    group_by(country_name, election_id) %>% 
    filter(start_date == max(start_date)) %>% 
    transmute(previous_cabinet_id = cabinet_id,
              party_id = party_id,
              party_name_short = party_name_short,
              party_name_english = party_name_english,
              prime_minister_last = prime_minister,
              cabinet_party_last = cabinet_party) %>% 
    ungroup() %>% 
    select(-election_id)

pg <- read_csv("http://www.parlgov.org/static/data/development-cp1252/view_election.csv",
               col_types = "cccDdiicccdiiiii") %>% 
    filter(election_type == "parliament") %>% 
    select(election_id, previous_cabinet_id, election_date) %>% 
    distinct() %>% 
    left_join(last_cabinet, by = "previous_cabinet_id") %>% 
    mutate(country = countrycode::countrycode(country_name, "country.name", "country.name"),
           party = party_name_short,
           year = lubridate::year(election_date)) %>% 
    filter(country %in% (countries %>% countrycode::countrycode("country.name", "country.name"))) %>% 
    mutate(country = if_else(str_detect(country, "Kingdom"), "United Kingdom", country),
           party_name_short = if_else(party_name_short=="Gruene", "GRUNE", party_name_short)) %>% 
    filter(cabinet_party_last == 1)

# Austria done

# Belgium done
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="BSP-PSB"] <- "BSP/PSB"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="CVP"] <- "CVP/PSC"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="Ecolo"] <- "ECOLO"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="AGL-Gr"] <- "GROEN"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="KPB-PCB"] <- "KPB/PCB"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="LD|LDD"] <- "LDD"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="Pp"] <- "PP"

# Bulgaria Parlgov data starts in 1991; done
pg$party[pg$country_name=="Bulgaria" & pg$party_name_short=="Ataka"] <- "ATAKA"
pg$party[pg$country_name=="Bulgaria" & pg$party_name_short=="KzB|DL"] <- "KB"
pg$party[pg$country_name=="Bulgaria" & pg$party_name_short=="ZS-AS"] <- "BZNS-AS"
pg$party[pg$country_name=="Bulgaria" & pg$party_name_short=="ZNS"] <- "BZNS"
pg$party[pg$country_name=="Bulgaria" & pg$party_name_short=="BNS"] <- "NS"

# Croatia done
pg$party[pg$country_name=="Croatia" & pg$party_name_short=="HDSS"] <- "HDS"
pg$party[pg$country_name=="Croatia" & pg$party_name_short=="MB"] <- "MB365"
pg$party[pg$country_name=="Croatia" & pg$party_name_short=="Most"] <- "MOST"
pg$party[pg$country_name=="Croatia" & pg$party_name_short=="NS"] <- "NSR"
pg$party[pg$country_name=="Croatia" & pg$party_name_short=="SPH"] <- "SDP"
pg$party[pg$country_name=="Croatia" & pg$party_name_short=="ZiZi"] <- "ZZ"

# Cyprus Parlgov data starts in 1976; done
pg$party[pg$country_name=="Cyprus" & pg$party_name_short=="EK"] <- "EVROKO"

# Czech Republic done
pg$party[pg$country_name=="Czech Republic" & pg$party_name_short=="KSCM" & pg$year==1992] <- "LB"

# Denmark done
pg$party[pg$country_name=="Denmark" & pg$party_name_short=="En-O"] <- "EL"

# Estonia done
pg$party[pg$country_name=="Estonia" & pg$party_name_short=="ERP"] <- "RP"

# France done
pg$party[pg$country_name=="France" & pg$party_name_short=="UMP|LR"] <- "LR"
pg$party[pg$country_name=="France" & pg$party_name_short=="UG"] <- "UDR"
pg$party[pg$country_name=="France" & pg$party_name_short=="UDF|MD"] <- "MoDem"
pg$party[pg$country_name=="France" & pg$party_name_short=="IR|DL"] <- "RI"

# Finland done
pg$party[pg$country_name=="Finland" & pg$party_name_short=="KE|SLK"] <- "LIB"
pg$party[pg$country_name=="Finland" & pg$party_name_short=="SP|P"] <- "PS"
pg$party[pg$country_name=="Finland" & pg$party_name_short=="DL|VAS"] <- "VAS"

# Germany done
pg$party[pg$country_name=="Germany" & pg$party_name_short=="B90/Gru"] <- "GRUNE"
pg$party[pg$country_name=="Germany" & pg$party_name_short=="B90/Gr"] <- "GRUNE"
pg$party[pg$country_name=="Germany" & pg$party_name_short=="CDU+CSU"] <- "CDU"
pg$party[pg$country_name=="Germany" & pg$party_name_short=="Li/PDS"] <- "LINKE"

# Greece Parlgov data starts in 1974; done
pg$party[pg$country_name=="Greece" & pg$party_name_short=="TP"] <- "TO POTAMI"
pg$party[pg$country_name=="Greece" & pg$party_name_short=="ChA"] <- "XA"

# Hungary done
pg$party[pg$country_name=="Hungary" & pg$party_name_short=="EGYUTT"] <- "E14"
pg$party[pg$country_name=="Hungary" & pg$party_name_short=="FI-MPSZ"] <- "FIDESZ"

# Iceland done
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Ab"] <- "AB"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="B-H"] <- "BF2"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Bf-87"] <- "BF3"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="SDU"] <- "BJ"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Th-Ff"] <- "TV"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Pi"] <- "P"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="SF"] <- "SFVM"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="KL"] <- "SK"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="V"] <- "VIÐREISN"

# Ireland done
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="PBPA"] <- "AAA-PBP"
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="Green"] <- "GP"
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="DS"] <- "SD"

# Italy done
pg$party[pg$country_name=="Italy" & pg$party_name_short=="BN"] <- "BNL"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="DL-M"] <- "DL"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="PpP"] <- "PRODI"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="FI-PdL"] <- "FI"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="IdV"] <- "IDV"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="MpA"] <- "MPA"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="SL"] <- "SEL"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="UC"] <- "UDC"

## From here on down, only parties that have ever been part of government are matched
# Latvia done
pg$party[pg$country_name=="Latvia" & pg$party_name_short=="LRa"] <- "LRA"
pg$party[pg$country_name=="Latvia" & pg$party_name_short=="NA/TB/LNNK"] <- "NA"

# Lithuania done
pg$party[pg$country_name=="Lithuania" & pg$party_name_short=="LKDP"] <- "LKD"
pg$party[pg$country_name=="Lithuania" & pg$party_name_english=="LVLS"] <- "LZS"
pg$party[pg$country_name=="Lithuania" & pg$party_name_short=="TS-LK"] <- "TS-LKD"
pg$party[pg$country_name=="Lithuania" & pg$party_name_short=="TT-LDP"] <- "TT"

# Luxembourg done
pg$year[pg$country_name=="Luxembourg" & pg$year==1968] <- 1969

# Malta done

# Netherlands done

# Norway done
pg$party[pg$country_name=="Norway" & pg$party_name_short=="KrF"] <- "KRF" 
pg$party[pg$country_name=="Norway" & pg$party_name_short=="Sp"] <- "SP" 

# Poland done
pg$party[pg$country_name=="Poland" & pg$party_name_short=="D|W|U"] <- "UD" 

# Portugal done

# Romania done
pg$party[pg$country_name=="Romania" & pg$party_name_short=="PNT-CD"] <- "PNTCD" 
pg$party[pg$country_name=="Romania" & pg$party_name_short=="PC"] <- "PSD+PC"
pg$party[pg$country_name=="Romania" & pg$party_name_short=="PSDR"] <- "PDSR"

# Slovakia done
pg$party[pg$country_name=="Slovakia" & pg$party_name_short=="DUS"] <- "DU" 
pg$party[pg$country_name=="Slovakia" & pg$party_name_short=="SaS"] <- "SAS" 
pg$party[pg$country_name=="Slovakia" & pg$party_name_short=="Smer"] <- "SMER" 

# Slovenia
pg$party[pg$country_name=="Slovenia" & pg$party_name_short=="DeSUS"] <- "DESUS" 
pg$party[pg$country_name=="Slovenia" & pg$party_name_short=="Zares"] <- "ZARES"
pg$party[pg$country_name=="Slovenia" & pg$party_name=="LZJ-PS"] <- "PS"
pg$party[pg$country_name=="Slovenia" & pg$party_name=="ZL-SD"] <- "ZLSD"

# Spain done
pg$party[pg$country_name=="Spain" & pg$party_name_short=="AP-P"] <- "PP" 

# Sweden done
pg$party[pg$country_name=="Sweden" & pg$party_name_short=="SAP"] <- "S" 

# Switzerland done
pg$party[pg$country_name=="Switzerland" & pg$party_name_short=="CVP-PDC"] <- "CVP"
pg$party[pg$country_name=="Switzerland" & pg$party_name_short=="FDP-PRD"] <- "FDP"
pg$party[pg$country_name=="Switzerland" & pg$party_name_short=="SP-PS"] <- "SP" 
pg$party[pg$country_name=="Switzerland" & pg$party_name_short=="SVP-UDC"] <- "SVP" 

# United Kingdom done
pg$party[pg$country_name=="United Kingdom" & pg$party_name_short=="Con"] <- "CON" 
pg$party[pg$country_name=="United Kingdom" & pg$party_name_short=="Lab"] <- "LAB" 
pg$party[pg$country_name=="United Kingdom" & pg$party_name_short=="Lib"] <- "LIB" 

change <- change_data %>%
    inner_join(pg, by = c("country" = "country", "name1" = "party", "year"))

for (i in 2:max_names) {
    temp <- merge(change_data, pg, 
                  by.x = c("country", paste0("name",i), "year"), 
                  by.y = c("country", "party", "year"),
                  all = FALSE)
    change <- bind_rows(change, temp)
}

change2 <- change_data %>% 
    anti_join(change, by = c("country", "party", "year")) %>% 
    bind_rows(change)

######
#Merge ParlGov data with Parties-and-Elections data
change <- merge(change.data, pg, by.x=c("country","name1", "year"), by.y=c("country_name", "party_name_short", "year"))

for(i in 2:6) {   
    temp <- merge(change.data, pg, by.x=c("country",paste0("name",i), "year"), by.y=c("country_name", "party_name_short", "year"))
    change <- rbind(change, temp)
}
rm(temp)