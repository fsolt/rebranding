library(tidyverse)
library(rio)

change_data <- import("data/change_data.rda") %>% 
    filter(!(party == "S" & country == "Denmark" & year == 2011)) %>% # doubled obs
    mutate(party = if_else(party == "OPEN", "OPEN VLD (VLD, PVV)", party), # Belgium
           change = if_else(party == "OPEN VLD (VLD, PVV)" & (year == 1995 | year == 2003), 1, change),
           party = if_else(party == "SD" & country == "Denmark", "S", party),
           party = if_else(party == "DIE", "DIE LINKE (PDS)", party),
           change = if_else(party == "DIE LINKE (PDS)" & year == 2005, 1, change),
           party = if_else(party == "IDV" & country == "Italy", "IDV (RC)", party),
           change = if_else(party == "PNM (PSIUP)" & year == 1948, 1, change),
           party = if_else(str_detect(party, "RETE 1994-2001,"), "RETE", party),
           change = if_else(party == "AP (DNA)" & year == 2013, 1, change),
           party = if_else(party == "CDU (APU)", "CDU (PCP, APU)", party),
           change = if_else(party == "SZ (SZS)" & year == 1992, 1, change),
           party = if_else(party == "CC (AIC, CC-NC-PNC)" | party == "CC-PNC", "CC-PNC (AIC, CC-NC-PNC, CC)", party),
           change = if_else(party == "CC-PNC (AIC, CC-NC-PNC, CC)" & year == 2015, 1, change)) %>% 
    filter(!(party == "KMU" & country == "Estonia") &               # these are added back below
               !(party == "NPSI-DCA (PS)" & country == "Italy") &
               !(party == "RnP (PSI, SI, GS)" & country == "Italy") &
               !(party == "VERDI (UDN)" & country == "Italy") &
               !(party == "PCTVL (L)" & country == "Latvia") &
               !(party == "AWS" & country == "Poland") &
               !(party == "SLD (PPR, PZPR, LiD, ZL)" & year == 2011 & change == 0) &
               !(party == "CDU (PCP, APU)" & year > 2009)) %>% 
    mutate(election = str_replace(election, "\\*|(?<=\\d)I\\b", "") %>% str_replace("\\.1", "II")) %>% 
    select(-prime_minister_last, -cabinet_party_last, -election_id, -enep1)
        
    
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
               party == "CDU (PCP, APU)" |
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
    

ncd <- bind_rows(change_data, old_change_data) %>% 
    





