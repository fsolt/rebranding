library(tidyverse)
library(rio)

change_data <- import("data/change_data.rda") %>% 
    mutate(party = if_else(party == "OPEN", "OPEN VLD (VLD, PVV)", party), # Belgium
           change = if_else(party == "OPEN VLD (VLD, PVV)" & (year == 1995 | year == 2003), 1, change),
           party = if_else(party == "SD" & country == "Denmark", "S", party),
           party = if_else(party == "DIE", "DIE LINKE (PDS)", party),
           change = if_else(party == "DIE LINKE (PDS)" & year == 2005, 1, change),
           party = if_else(party == "IDV" & country == "Italy", "IDV (RC)", party),
           change = if_else(party == "PNM (PSIUP)" & year == 1948, 1, change),
           party = if_else(str_detect(party, "RETE 1994-2001,"), "RETE", change)) %>% 
    filter(!(party == "KMU" & country == "Estonia") &               # these are added back below
               !(party == "NPSI-DCA (PS)" & country == "Italy") &
               !(party == "RnP (PSI, SI, GS)" & country == "Italy") &
               !(party == "VERDI (UDN)" & country == "Italy")) %>% 
    distinct() # to catch doubled obs for S 2011 in Denmark

old_change_data <- import("data/old_change_data.RData") %>% 
    anti_join(change_data, by = c("country", "party", "year")) %>% 
    mutate(party = if_else(party == "HSLS", "HSLS (HSLS-DC)", party),
           party = if_else(party == "HSS", "HSS (HSS-HSLS)", party),
           party = if_else(party == "IDS", "IDS (DA-IDS-RDS)", party),
           party = if_else(party == "BP" & country == "Germany", "BP (FU)", party),
           party = if_else(party == "NPD (DRP)", "NPD (DKP-DRP, DRP)", party),
           party = if_else(party == "ZENTRUM", "ZENTRUM (CVP)", party),
           party = if_else(party == "EDA", "EDA (PAME)", party)) %>% 
    rename(vote_share = votes) %>% 
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
               party == "VERDI (LV)"
               )







