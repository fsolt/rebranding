library(tidyverse)
library(rio)

old_change_data <- import("data/old_change_data.RData") %>% 
    mutate(party = if_else(party == "HSLS", "HSLS (HSLS-DC)", party),
           party = if_else(party == "HSS", "HSS (HSS-HSLS)", party),
           party = if_else(party == "IDS", "IDS (DA-IDS-RDS)", party)) %>% 
    filter(party == "PVV/PLP (LP-PL)" |
               party == "BSD (DAR, BE, KR)" |
               party == "KPB" |
               party == "RZS (OZS, NS-BZNS)" |
               party == "HNS" |
               party == "HSLS (HSLS-DC)" & year == 2000 |
               party == "HSS (HSS-HSLS)" & (year == 1995 | year == 2000) |
               party == "IDS (DA-IDS-RDS)" & (year == 1995 | year == 2000 | year == 2003) |
               party == "AKEL" |
               party == "DP" & country == "Cyprus" |
               party == "EKES" |
               party == "SP" & country == "Denmark" |
               party == "EVP (EKP, VV, O, ESDTP)" |
               party == "KMU (KK)" |
               party == "PK" & country == "Estonia" |
               party == "VEE" |
               
               )
 
change_data <- import("data/change_data.rda") %>% 
    mutate(party = if_else(party == "OPEN", "OPEN VLD (VLD, PVV)", party), # Belgium
           change = if_else(party == "OPEN VLD (VLD, PVV)" & (year == 1995 | year == 2003), 1, change),
           party = if_else(party == "SD" & country == "Denmark", "S", party)) %>% 
    filter(!(party == "KMU" & country == "Estonia")) %>% 
    distinct() # to catch doubled obs for S 2011 in Denmark

#   Belgium
# X PVV/PLP (LP-PL) missed in new
# X OPEN in new should be OPEN VLD (VLD, PVV) with changes in 1995 and 2003
#   Bulgaria
# X BSD (DAR, BE, KR) missed in new
# X KPB missed in new
# X RZS (OZS, NS-BZNS) 2005 missed in new
#   Croatia
# X HNS 1995 and 2000 missed in new
# X HSLS (HSLS-DC) should be 0 votes, 0 change in 2000
# X HSS (HSS-HSLS) should be 0 votes, 0 change in 1995 and 2000
# X IDS (DA-IDS-RDS) should be 0 and 0 in 1995, 2000, and 2003
#   Denmark
# X SD should be S; drop duplicate 2011 observation
#   Estonia






