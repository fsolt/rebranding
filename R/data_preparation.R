library(tidyverse)
library(rio)

change_data <- import("data/change_data.rda") %>% 
    filter(!(party == "S" & country == "Denmark" & year == 2011)) %>% # doubled obs
    mutate(party = if_else(party == "OPEN", "OPEN VLD (VLD, PVV)", party), # Belgium
           change = if_else(party == "OPEN VLD (VLD, PVV)" & (year == 1995 | year == 2003), 1, change),
           party = if_else(party == "SD" & country == "Denmark", "S", party),
           party = if_else(party == "NC" | party == "UDI (PR, PR-UDSR)" & year > 2002, "UDI (PR+NC, NC)", party),
           change = if_else(party == "UDI (PR+NC, NC)" & year == 2012, 1, change),
           name1 = if_else(party == "UDI (PR+NC, NC)", "UDI", name1),
           name2 = if_else(party == "UDI (PR+NC, NC)", "PR+NC", name2),
           name3 = if_else(party == "UDI (PR+NC, NC)", "NC", name3),
           party = if_else(party == "DIE", "DIE LINKE (PDS)", party),
           change = if_else(party == "DIE LINKE (PDS)" & year == 2005, 1, change),
           party = if_else(party == "BF" & year == 1987 & country == "Iceland", "Bf-87", party),
           name1 = if_else(party == "Bf-87" & year == 1987 & country == "Iceland", "Bf-87", name1),
           party = if_else(party == "SF" & country == "Iceland", "SFVM", party),
           name1 = if_else(party == "SFVM" & country == "Iceland", "SFVM", name1),
           party = if_else(party == "IDV" & country == "Italy", "IDV (RC)", party),
           change = if_else(party == "PNM (PSIUP)" & year == 1948, 1, change),
           party = if_else(str_detect(party, "RETE 1994-2001,"), "RETE", party),
           change = if_else(party == "AP (DNA)" & year == 2013, 1, change),
           change = if_else(party == "SZ (SZS)" & year == 1992, 1, change),
           party = if_else(party == "CC (AIC, CC-NC-PNC)" | party == "CC-PNC", "CC-PNC (AIC, CC-NC-PNC, CC)", party),
           change = if_else(party == "CC-PNC (AIC, CC-NC-PNC, CC)" & year == 2015, 1, change)) %>% 
    filter(!(party == "KMU" & country == "Estonia") &               # these are added back below
               !(party == "RE 1990:(Independents)" & country == "Estonia") &
               !(party == "UDI (PR, PR-UDSR)" & country == "France") &
               !(str_detect(party, "AB 1979") & country == "Iceland") &
               !(party == "NPSI-DCA (PS)" & country == "Italy") &
               !(party == "RnP (PSI, SI, GS)" & country == "Italy") &
               !(party == "VERDI (UDN)" & country == "Italy") &
               !(party == "PCTVL (L)" & country == "Latvia") &
               !(party == "AWS" & country == "Poland") &
               !(party == "SLD (PPR, PZPR, LiD, ZL)" & year == 2011 & change == 0) &
               !(party == "CDU (APU)") &
               !(party == "FRS" & year == 1980) &
               !(country == "Cyprus" & year == 1976) &                    # Coalition not standardized
               !(country == "Portugal" & (year == 1979 | year==1980)) &   # Coalition not standardized
               !((country == "Poland" | country == "Slovakia") & year < 1948)
           ) %>% 
    mutate(election = str_replace(election, "\\*|(?<=\\d)I\\b", "") %>% str_replace("\\.1", "II")) %>% 
    select(-prime_minister_last, -cabinet_party_last, -election_id, -enep1) 

# Standardizing the coalition results in Cyprus's 1976 and Portugal's 1979 & 1980 elections
cyprus1976 <- tibble(country = "Cyprus",
                     party = c("AKEL", "DIKO (DP)", "KS-EDEK (EDEK, EDEK, SK-EDEK, KISOS)", "DISY (EK)"),
                     name1 = c("AKEL", "DIKO", "KS-EDEK", "DISY"),
                     name2 = c(NA, "DP", "EDEK", "EK"),
                     name3 = c(NA, NA, "EDEK", NA),
                     name4 = c(NA, NA, "SK-EDEK", NA),
                     name5 = c(NA, NA, "KISOS", NA),
                     election = "1976",
                     vote_share = round(c(71.2*c(9/34, 21/34, 4/34), 27.6), 1),
                     change = 0,
                     year = 1976)

portugal1979 <- tibble(country = "Portugal",
                       party = c("CDS-PP (CDS)", "PCP", "MDP", "PPM (PPM-MPT)", "PS", "PSD (PPD)", "UDP"),
                       name1 = c("CDS-PP", "PCP", "MDP", "PPM", "PS", "PSD", "UDP"),
                       name2 = c("CDS", NA, NA, "PPM-MPT", NA, "PPD", NA),
                       election = "1979",
                       vote_share = round(c(42.5*43/121+.4, 18.8*44/48, 18.8*4/48, 42.5*5/121, 27.3, 42.5*73/121+2.4, 2.2), 1),
                       change = 0,
                       year = 1979)

portugal1980 <- tibble(country = "Portugal",
                       party = c("CDS-PP (CDS)", "PCP", "MDP", "PPM (PPM-MPT)", "PS", "PSD (PPD)", "UDP"),
                       name1 = c("CDS-PP", "PCP", "MDP", "PPM", "PS", "PSD", "UDP"),
                       name2 = c("CDS", NA, NA, "PPM-MPT", NA, "PPD", NA),
                       election = "1980",
                       vote_share = round(c(44.9*46/126+.2, 16.8*39/41, 16.8*2/41, 44.9*6/126, 27.3*63/71+1.1, 44.9*74/126+2.5, 1.4), 1),
                       change = 0,
                       year = 1980)

portugalCDU <- tibble(country = "Portugal",
                      party = c(rep(c("PCP", "MDP"), times = 2), rep(c("PCP", "PEV"), times = 9)),
                      year = c(rep(c(1983, 1985, 1987, 1991, 1995, 1999, 2002, 2005, 2009, 2011, 2015), each = 2)),
                      name1 = party,
                      election = as.character(year),
                      vote_share = round(c(18.1*c(41/44, 3/44), #1983
                                           15.5*c(35/38, 3/38), #1985
                                           12.1*c(29/31, 2/31), #1987
                                           8.8*c(15/17, 2/17),  #1991
                                           8.6*c(13/15, 2/15),  #1995
                                           9.0*c(15/17, 2/17),  #1999
                                           7.0*c(10/12, 2/12),  #2002
                                           7.6*c(12/14, 2/14),  #2005
                                           7.9*c(13/15, 2/15),  #2009
                                           7.9*c(14/16, 2/16),  #2011
                                           8.3*c(15/17, 2/17)  #2015
                                           ), 1),
                      change = 0)
        
old_change_data <- import("data/old_change_data.RData") %>% 
    mutate_if(is.factor, as.character) %>% 
    anti_join(change_data, by = c("country", "party", "year")) %>% 
    rename(vote_share = votes) %>%
    mutate(party = if_else(party == "HSLS", "HSLS (HSLS-DC)", party),
           party = if_else(party == "HSS", "HSS (HSS-HSLS)", party),
           party = if_else(party == "IDS", "IDS (DA-IDS-RDS)", party),
           party = if_else(party == "BP" & country == "Germany", "BP (FU)", party),
           party = if_else(party == "NPD (DRP)", "NPD (DKP-DRP, DRP)", party),
           party = if_else(party == "ZENTRUM", "ZENTRUM (CVP)", party),
           party = if_else(party == "EDA", "EDA (PAME)", party),
           party = if_else(party == "LSDP", "LSDP (LSDP-NS)", party),
           party = if_else(party == "SZS", "SZ (SZS)", party)) %>% 
    filter(party == "PVV/PLP (LP-PL)" |
               party == "BSD (DAR, BE, KR)" |
               party == "KPB" |
               party == "RZS (OZS, NS-BZNS)" |
               party == "HNS" |
               (party == "HSLS (HSLS-DC)" & year == 2000) |
               (party == "HSS (HSS-HSLS)" & (year == 1995 | year == 2000)) |
               (party == "IDS (DA-IDS-RDS)" & (year == 1995 | year == 2000 | year == 2003)) |
               party == "AKEL" |
               (party == "DP" & country == "Cyprus") |
               party == "EKES" |
               (party == "SP" & country == "Denmark") |
               party == "EVP (EKP, VV, O, ESDTP)" |
               party == "KMU (KK)" |
               (party == "PK" & country == "Estonia") |
               party == "VEE" |
               party == "KD (SKL)" |
               party == "LIB (KEP, SKP, LKP)" |
               (party == "LV" & country == "France") |
               party == "MPF" |
               party == "PR (MDC)" |
               party == "PR (PR-UDSR, RGR)" |
               party == "PSU" |
               (party == "BP (FU)" & vote_share == 0) |
               party == "GB/BHE (GDP)" |
               (party == "NPD (DKP-DRP, DRP)" & year == 1994) |
               party == "REP" |
               party == "SSW" |
               (party == "ZENTRUM" & vote_share == 0) |
               (party == "EDA (PAME)" & vote_share == 0) |
               party == "EPEK (PADE)" |
               party == "KF" |
               (party == "KKE" & vote_share == 0) |
               party == "MDF" |
               party == "MKMP (MSZMP, MP)" |
               party == "MRP" |
               party == "MSZDP (SZDP)" |
               party == "PDP" |
               party == "NLP" |
               (party == "SF" & country == "Ireland" & vote_share == 0) |
               party == "DE" & year == 2004 |
               party == "LAM" |
               party == "LD-FT (MSFT)" |
               party == "NPSI-DCA (SL, PS)" |
               party == "PS (PSI, SI, GS, RnP)" |
               party == "RAD (LP, LPS, LB)" |
               party == "SVP" |
               (party == "UV" & country == "Italy") |
               party == "VERDI (LV)" |
               party == "KDS (LKDS)" |
               party == "LPP/LC" |
               (party == "LSP (LKP)" & vote_share == 0) |
               party == "PCTVL (LS)" |
               (party == "VL" & country == "Latvia") |
               party == "AWPL (LLS)" |
               (party == "KDS" & country == "Lithuania") |
               party == "KKSS (NKS)" |
               (party == "LDP" & country == "Lithuania") |
               party == "LLL" |
               party == "LRS" |
               (party == "LSDP (LSPD-NS)" & year == 2000) |
               party == "LTS" |
               party == "LZP" |
               party == "KPL" |
               (party == "CD" & country == "Netherlands") |
               party == "NKP" |
               party == "AWSP (AWS)" |
               party == "ChD-SP (ChD)" |
               party == "PPChD (PChD)" |
               party == "ROP (RP)" |
               country == "Romania" |
               party == "KDH" |
               party == "MOS" |
               party == "PSNS" |
               (party == "SZ (SZS)" & vote_share == 0) |
               party == "KD (KDS)" |
               party == "LdT" |
               party == "SOL" |
               party == "SF" |
               party == "UUP") %>% 
    mutate(election = str_replace(as.character(year), "\\.1", "II"),
           year = floor(year))
    
ncd <- bind_rows(change_data, old_change_data, cyprus1976, portugal1979, portugal1980, portugalCDU) 
    
# get ParlGov data on incumbent status
last_cabinet <- read_csv("http://www.parlgov.org/static/data/development-cp1252/view_cabinet.csv",
                         col_types = "ccDDciiiiicccdiiiii") %>% 
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

# Most recent elections in Parties and Elections not yet included in ParlGov, 
# so they don't have a previous_cabinet_id although that cabinet is already in ParlGov
# (Austria 2017; Czech Rep 2017; Germany 2017; Iceland 2017; Norway 2017; Romania 2016)
most_recent <- read_csv("http://www.parlgov.org/static/data/development-cp1252/view_cabinet.csv",
                        col_types = "ccDDciiiiicccdiiiii") %>% 
    arrange(country_name, election_date, start_date) %>% 
    filter(country_name %in% c("Austria", "Czech Republic", "Germany", "Iceland", "Norway", "Romania")) %>% 
    group_by(country_name) %>% 
    filter(start_date == max(start_date)) %>% 
    ungroup() %>% 
    transmute(country = countrycode::countrycode(country_name, "country.name", "country.name"),
              year = if_else(country_name == "Romania", 2016, 2017),
              party_id = party_id,
              party_name_short = party_name_short,
              party_name_english = party_name_english, 
              party = if_else(party_name_short=="Gruene", "GRUNE", party_name_short),
              prime_minister_last = prime_minister,
              cabinet_party_last = cabinet_party) %>% 
    filter(cabinet_party_last == 1)

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
    filter(cabinet_party_last == 1) %>% 
    group_by(country, year) %>% 
    arrange(election_date) %>% 
    mutate(election1 = as.numeric(as.factor(election_date)),
           year1 = year + (election1 - 1)/10) %>% 
    ungroup() %>% 
    select(-year, -election1) %>% 
    rename(year = year1) %>% 
    filter(year >= 1945) %>% 
    bind_rows(most_recent) %>% 
    arrange(country, year, party)

# Generate party abbreviations that match those in change_data0
check_matches <- function(cc) {
    pg_parties <- pg %>% filter(country_name==cc) %>% pull(party) %>% unique() %>% sort()
    
    cd_data <- ncd %>% filter(country==cc) 
    
    max_names <- cd_data %>% 
        pull(party) %>% 
        strsplit("( \\()|, |\\)") %>% 
        map_int(~ length(.x)) %>% 
        max()
    
    cd_parties <- cd_data %>% pull(name1) %>% unique()
    
    for (i in 2:max_names) {
        temp <- cd_data %>%
            mutate_at(vars(ends_with(paste(i))), funs(party_name = identity)) %>% 
            pull(party_name) %>%
            unique()
        cd_parties <- c(cd_parties, temp)
    }
    
    print(paste("unmatched_pg for", cc, ":", paste(setdiff(pg_parties, cd_parties), collapse = "; ")))
    print(paste("unmatched_cd for", cc, ":", paste(setdiff(cd_parties, pg_parties), collapse = "; ")))
}

# Austria done

# Belgium done
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="AGL-Gr"] <- "GROEN"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="Ecolo"] <- "ECOLO"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="KPB-PCB"] <- "KPB/PCB"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="PSC-CDH"] <- "CDH"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="PSC-CVP"] <- "CVP"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="PVV|VLD"] <- "PVV/PLP"
pg$party[pg$country_name=="Belgium" & pg$party_name_short=="SPa+Spi"] <- "SP.A"

# Bulgaria Parlgov data starts in 1991; done
pg$party[pg$country_name=="Bulgaria" & pg$party_name_short=="KzB|DL"] <- "KB"
pg$party[pg$country_name=="Bulgaria" & pg$party_name_short=="ZNS"] <- "BZNS"

# Croatia done
pg$party[pg$country_name=="Croatia" & pg$party_name_short=="Most"] <- "MOST"
pg$party[pg$country_name=="Croatia" & pg$party_name_short=="SPH"] <- "SDP"

# Cyprus Parlgov data starts in 1976; done

# Czech Republic done
pg$party[pg$country_name=="Czech Republic" & pg$party_name_short=="KDS"] <- "ODS-KDS"

# Denmark done
pg$party[pg$country_name=="Denmark" & pg$party_name_short=="DS"] <- "DU"
pg$party[pg$country_name=="Denmark" & pg$party_name_short=="KrF"] <- "KRF"
pg$party[pg$country_name=="Denmark" & pg$party_name_short=="RF"] <- "KRF"
pg$party[pg$country_name=="Denmark" & pg$party_name_short=="Sd"] <- "S"

# Estonia done
pg$party[pg$country_name=="Estonia" & pg$party_name_short=="EKK"] <- "KMU"
pg$party[pg$country_name=="Estonia" & pg$party_name_short=="ERa"] <- "ERL"
pg$party[pg$country_name=="Estonia" & pg$party_name_short=="ERe"] <- "RE"
pg$party[pg$country_name=="Estonia" & pg$party_name_short=="RKI"] <- "KE"
pg$party[pg$country_name=="Estonia" & pg$party_name_short=="SDE|M"] <- "M"

# France done
pg$party[pg$country_name=="France" & pg$party_name_short=="CDP"] <- "CD"
pg$party[pg$country_name=="France" & pg$party_name_short=="G"] <- "UDR"
pg$party[pg$country_name=="France" & pg$party_name_short=="IR|DL"] <- "RI"
pg$party[pg$country_name=="France" & pg$party_name_short=="PRL"] <- "CNIP"
pg$party[pg$country_name=="France" & pg$party_name_short=="REM"] <- "LREM"
pg$party[pg$country_name=="France" & pg$party_name_short=="UDF|MD"] <- "MoDem"
pg$party[pg$country_name=="France" & pg$party_name_short=="UDSR"] <- "PR-UDSR"
pg$party[pg$country_name=="France" & pg$party_name_short=="UMP|LR"] <- "LR"
pg$party[pg$country_name=="France" & pg$party_name_short=="V"] <- "LV"

# Finland done
pg$party[pg$country_name=="Finland" & pg$party_name_short=="DL|VAS"] <- "VAS"
pg$party[pg$country_name=="Finland" & pg$party_name_short=="KE|SLK"] <- "LIB"
pg$party[pg$country_name=="Finland" & pg$party_name_short=="RKP-SFP"] <- "SFP"
pg$party[pg$country_name=="Finland" & pg$party_name_short=="SP|P"] <- "PS"
pg$party[pg$country_name=="Finland" & pg$party_name_short=="SSDP"] <- "SDP"

# Germany done
pg$party[pg$country_name=="Germany" & pg$party_name_short=="B90/Gru"] <- "GRUNE"
pg$party[pg$country_name=="Germany" & pg$party_name_short=="CDU+CSU"] <- "CDU"

# Greece Parlgov data starts in 1974; done
pg$party[pg$country_name=="Greece" & pg$party_name_short=="AE"] <- "ANEL"

# Hungary done
pg$party[pg$country_name=="Hungary" & pg$party_name_short=="Fi-MPSz"] <- "FIDESZ"
pg$party[pg$country_name=="Hungary" & pg$party_name_short=="FKgP"] <- "FKGP"
pg$party[pg$country_name=="Hungary" & pg$party_name_short=="SzDSz"] <- "SZDSZ"

# Iceland done
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="A"] <- "S"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Ab"] <- "AB"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Graen"] <- "VG"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Sfvm"] <- "SFVM"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="SA-S"] <- "SF"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Sam"] <- "S"
pg$party[pg$country_name=="Iceland" & pg$party_name_short=="Sj"] <- "SSF"

# Ireland done
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="CnP"] <- "CnaP"
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="CnT"] <- "CnaT"
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="DLP"] <- "DL"
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="Green"] <- "GP"
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="Lab"] <- "LAB"
pg$party[pg$country_name=="Ireland" & pg$party_name_short=="NL"] <- "NLP"

# Italy done
#pg$party[pg$country_name=="Italy" & pg$party_name_short=="BN"] <- "BNL"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="FdV"] <- "VERDI"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="FI-PdL"] <- "FI"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="ID"] <- "DL"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="IdV"] <- "IDV"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="NPSI"] <- "NPSI-DCA"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="PpP"] <- "PRODI"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="P|SDI"] <- "PSI"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="PdCI"] <- "PDCI"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="R"] <- "RAD"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="RiI"] <- "RI"
pg$party[pg$country_name=="Italy" & pg$party_name_short=="UC"] <- "UDC"

# Latvia done
pg$party[pg$country_name=="Latvia" & pg$party_name_short=="NA/TB/LNNK"] <- "NA"
pg$party[pg$country_name=="Latvia" & pg$party_name_short=="PS"] <- "V"

# Lithuania done
pg$party[pg$country_name=="Lithuania" & pg$party_name_short=="LiCS-TPP"] <- "LiCS"
pg$party[pg$country_name=="Lithuania" & pg$party_name_short=="LKDP"] <- "LKD"
pg$party[pg$country_name=="Lithuania" & pg$party_name_short=="TS-LK"] <- "TS-LKD"
pg$party[pg$country_name=="Lithuania" & pg$party_name_short=="TT-LDP"] <- "TT"

# Luxembourg done
pg$year[pg$country_name=="Luxembourg" & pg$year==1968] <- 1969

# Malta done

# Netherlands done

# Norway done
pg$party[pg$country_name=="Norway" & pg$party_name_short=="Fr"] <- "FRP" 
pg$party[pg$country_name=="Norway" & pg$party_name_short=="KrF"] <- "KRF" 
pg$party[pg$country_name=="Norway" & pg$party_name_short=="Sp"] <- "SP" 

# Poland done
pg$party[pg$country_name=="Poland" & pg$party_name_short=="D|W|U"] <- "UD" 

# Portugal done

# Romania done
pg$party[pg$country_name=="Romania" & pg$party_name_short=="PNT-CD"] <- "PNTCD" 

# Slovakia done
pg$party[pg$country_name=="Slovakia" & pg$party_name_short=="DUS"] <- "DU" 
pg$party[pg$country_name=="Slovakia" & pg$party_name_short=="SaS"] <- "SAS" 
pg$party[pg$country_name=="Slovakia" & pg$party_name_short=="Smer"] <- "SMER" 

# Slovenia
pg$party[pg$country_name=="Slovenia" & pg$party_name_short=="DeSUS"] <- "DESUS" 
pg$party[pg$country_name=="Slovenia" & pg$party_name_short=="LZJ-PS"] <- "PS"
pg$party[pg$country_name=="Slovenia" & pg$party_name_short=="ZL-SD"] <- "ZLSD"

# Spain done
pg$party[pg$country_name=="Spain" & pg$party_name_short=="AP-P"] <- "PP" 

# Sweden done
pg$party[pg$country_name=="Sweden" & pg$party_name_short=="SAP"] <- "S" 

# Switzerland done
pg$party[pg$country_name=="Switzerland" & pg$party_name_short=="FDP-PRD"] <- "FDP"
pg$party[pg$country_name=="Switzerland" & pg$party_name_short=="KK/CVP"] <- "CVP"
pg$party[pg$country_name=="Switzerland" & pg$party_name_short=="SP-PS"] <- "SP" 
pg$party[pg$country_name=="Switzerland" & pg$party_name_short=="SVP-UDC"] <- "SVP" 

# United Kingdom done
pg$party[pg$country_name=="United Kingdom" & pg$party_name_short=="Con"] <- "CON" 
pg$party[pg$country_name=="United Kingdom" & pg$party_name_short=="Lab"] <- "LAB" 
pg$party[pg$country_name=="United Kingdom" & pg$party_name_short=="Lib"] <- "LIB" 

# Merge ParlGov data on incumbent governments with change_data0 using all past names of each party
change <- ncd %>%
    inner_join(pg %>%
                   select(country, party, year, prime_minister_last, cabinet_party_last, election_id),
               by = c("country", "name1" = "party", "year"))

for (i in 2:max_names) {
    temp <- inner_join(ncd %>%
                           mutate_at(vars(ends_with(paste(i))), funs(party_name = identity)),
                       pg %>%
                           select(country, party, year, prime_minister_last, cabinet_party_last, election_id), 
                       by = c("country", "party_name" = "party", "year")) %>% 
        select(-party_name)
    change <- bind_rows(change, temp)
}
rm(temp)

# Re-add parties that were not part of incumbent governments 
change2 <- ncd %>% 
    anti_join(change %>%
                  select(country, party, year, prime_minister_last, cabinet_party_last),
              by = c("country", "party", "year")) %>% 
    bind_rows(change) %>% 
    mutate(prime_minister_last = if_else(is.na(prime_minister_last), 0L, prime_minister_last),
           cabinet_party_last = if_else(is.na(cabinet_party_last), 0L, cabinet_party_last)) %>% 
    distinct() %>% 
    arrange(country, party, year)

# EU + 3 countries only
countries <- read_html("https://en.wikipedia.org/wiki/Member_state_of_the_European_Union") %>%
    html_table(fill=TRUE) %>%   # generates a list
    nth(2) %>%                  # get second element of the list
    as_tibble() %>%             # make it a tibble (data_frame)
    pull(`Country name`) %>% 
    str_trim() %>% 
    str_replace("\\[.*\\]", "") %>% # omit footnotes
    # str_replace(" ", "") %>%        # collapse words
    # str_replace("Republic", "ia") %>% 
    # tolower() %>% 
    c(., "Iceland", "Norway", "Switzerland")

# add effective number of electoral parties using least component approach (see Taagepera 1997, 147-148)
new_change_data <- change2 %>% 
    group_by(country, year) %>% 
    mutate(election_id = mean(election_id, na.rm = TRUE),
           enep_omit = 1/sum((vote_share/100)^2),
           other_share = 1 - sum((vote_share/100)),
           share_min = min(other_share^2, other_share * min(setdiff(vote_share/100, 0))) %>% 
               max(0),
           enep_min = 1/(sum((vote_share/100)^2) + share_min),
           enep1 = (enep_omit + enep_min)/2) %>% 
    ungroup() %>% 
    select(-enep_omit, -other_share, -share_min, -enep_min) %>% 
    arrange(country, party, year) %>% 
    filter(country %in% countries & !str_detect(party, "^Ind"))

save(new_change_data, file="data/new_change_data.rda")



