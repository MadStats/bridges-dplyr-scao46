install.packages("ggplot2")
install.packages("plyr")
install.packages("choroplethr")
install.packages("dplyr")
install.packages("choroplethrMaps")

library(plyr)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)
library(readr)
library(data.table)


# I like downloading straight from url's.  
# If I was planning to share this code in a long time, I would worry about stability of the accessibility (url/data/format) and maybe backup locally.
# For class, downloading is straightforward.

# I like fread (from data.table) and read_csv (from readr).  
# in my experience, fread is faster and deals with data entry errors a little more elegantly.
# downside of fread: it saves to format data.table.  
#   if you know how to use it, this can be super-duper fast.
# we will be using dplyr instead, which is merely super fast. 
# So, I always convert to a tibble (as.tbl) after an fread.  tibbles are basically data.frames that print nicer.

dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
tmp1 = read_csv(dest)
tmp2 = read_csv(dest, col_types = "c")  # could make them all characters...
classes = sapply(tmp, class)


states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()


# this is one possibility...
# for(i in 2:51){
#   dest=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
#   dat[[i]] = fread(dest, colClasses = classes ) %>% as.tbl  # for loop to define data could be really bad.  Does list make it ok?  idk.
# }
# lapply(dat, dim)
# colnames(dat[[1]])


# # gosh, it would be nice if these worked!
# x = fread("https://www.fhwa.dot.gov/bridge/nbi/2016allstatesallrecsdel.zip")
# x = read_csv("https://www.fhwa.dot.gov/bridge/nbi/2016allstatesallrecsdel.zip")


# Hadley typically has good stuff! 
# his code chunk from ftp://cran.r-project.org/pub/R/web/packages/tidyr/vignettes/tidy-data.html
# library(plyr)
# paths <- dir("data", pattern = "\\.csv$", full.names = TRUE)
# names(paths) <- basename(paths)
# ldply(paths, read.csv, stringsAsFactors = FALSE)




dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  
#save(x16, file = ".Rdata")

# let's dig into the values and see if there are any crazy things going on...
M = x16
#M = M[,-14]
is.na(M) %>% rowSums %>% hist
# == hist(rowSums(is.na(M)))
is.na(M) %>% colSums %>% hist(breaks = 100)
fun = function(x){ return(which(x>20)) }
(bad =  is.na(M) %>% colSums %>% fun)
M = M[,-bad]
jold =1
for(j in jold:ncol(M)){
  nc = nchar(M[,j], keepNA = T)
  print(j)
  print(summary(nc))
  print(sum(is.na(nc)))
  print("")
}
colnames(M)[j]
M = M[,-j]
jold = j

colnames(M)
# [1] "STATE_CODE_001"          "STRUCTURE_NUMBER_008"    "RECORD_TYPE_005A"        "ROUTE_PREFIX_005B"       "SERVICE_LEVEL_005C"      "ROUTE_NUMBER_005D"       "DIRECTION_005E"          "HIGHWAY_DISTRICT_002"   
# [9] "COUNTY_CODE_003"         "PLACE_CODE_004"          "FEATURES_DESC_006A"      "FACILITY_CARRIED_007"    "MIN_VERT_CLR_010"        "KILOPOINT_011"           "LRS_INV_ROUTE_013A"      "LAT_016"                
# [17] "LONG_017"                "DETOUR_KILOS_019"        "TOLL_020"                "MAINTENANCE_021"         "OWNER_022"               "FUNCTIONAL_CLASS_026"    "YEAR_BUILT_027"          "TRAFFIC_LANES_ON_028A"  
# [25] "TRAFFIC_LANES_UND_028B"  "ADT_029"                 "YEAR_ADT_030"            "DESIGN_LOAD_031"         "APPR_WIDTH_MT_032"       "MEDIAN_CODE_033"         "DEGREES_SKEW_034"        "STRUCTURE_FLARED_035"   
# [33] "RAILINGS_036A"           "TRANSITIONS_036B"        "APPR_RAIL_036C"          "APPR_RAIL_END_036D"      "HISTORY_037"             "NAVIGATION_038"          "NAV_VERT_CLR_MT_039"     "NAV_HORR_CLR_MT_040"    
# [41] "OPEN_CLOSED_POSTED_041"  "SERVICE_ON_042A"         "SERVICE_UND_042B"        "STRUCTURE_KIND_043A"     "STRUCTURE_TYPE_043B"     "APPR_KIND_044A"          "APPR_TYPE_044B"          "MAIN_UNIT_SPANS_045"    
# [49] "APPR_SPANS_046"          "HORR_CLR_MT_047"         "MAX_SPAN_LEN_MT_048"     "STRUCTURE_LEN_MT_049"    "LEFT_CURB_MT_050A"       "RIGHT_CURB_MT_050B"      "ROADWAY_WIDTH_MT_051"    "DECK_WIDTH_MT_052"      
# [57] "VERT_CLR_OVER_MT_053"    "VERT_CLR_UND_REF_054A"   "VERT_CLR_UND_054B"       "LAT_UND_REF_055A"        "LAT_UND_MT_055B"         "LEFT_LAT_UND_MT_056"     "DECK_COND_058"           "SUPERSTRUCTURE_COND_059"
# [65] "SUBSTRUCTURE_COND_060"   "CHANNEL_COND_061"        "CULVERT_COND_062"        "OPR_RATING_METH_063"     "INV_RATING_METH_065"     "STRUCTURAL_EVAL_067"     "DECK_GEOMETRY_EVAL_068"  "UNDCLRENCE_EVAL_069"    
# [73] "POSTING_EVAL_070"        "WATERWAY_EVAL_071"       "APPR_ROAD_EVAL_072"      "DATE_OF_INSPECT_090"     "FRACTURE_092A"           "UNDWATER_LOOK_SEE_092B"  "SPEC_INSPECT_092C"       "STRAHNET_HIGHWAY_100"   
# [81] "PARALLEL_STRUCTURE_101"  "TRAFFIC_DIRECTION_102"   "HIGHWAY_SYSTEM_104"      "FEDERAL_LANDS_105"       "DECK_STRUCTURE_TYPE_107" "SURFACE_TYPE_108A"       "MEMBRANE_TYPE_108B"      "DECK_PROTECTION_108C"   
# [89] "NATIONAL_NETWORK_110"    "BRIDGE_LEN_IND_112"      "SCOUR_CRITICAL_113"      "FUTURE_ADT_114"          "YEAR_OF_FUTURE_ADT_115"  "FED_AGENCY"              "DATE_LAST_UPDATE"        "TYPE_LAST_UPDATE"       
# [97] "DEDUCT_CODE"             "PROGRAM_CODE"            "PROJ_NO"                 "STATUS_WITH_10YR_RULE"   "SUFFICIENCY_ASTERC"      "SUFFICIENCY_RATING"      "STATUS_NO_10YR_RULE"    

keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "TOLL_020" , "ADT_029", "YEAR_ADT_030" ,"YEAR_BUILT_027" ,
           "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", "FRACTURE_092A" )

# x = M[,match(keep, colnames(M))]
M = as.tbl(M)
x = select(M, one_of(keep))  # see chapter 5 (section 4) in r4ds.

s = filter(x, STATE_CODE_001 == 34)
s
#what will influence the DECK_COND_058

library(ggplot2)


# make function to rate bridge as NA, good, bad, fail, using 
# colnames(s)[7:10]
# "SUPERSTRUCTURE_COND_059" "SUBSTRUCTURE_COND_060"   "CHANNEL_COND_061"        "CULVERT_COND_062"  
# good = 5:9
# bad = 2:4
# fail = 0:1

# cond "condition" is the minimum of the given ratings. 
s = mutate(s, cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                            na.rm = T))

rateIt = function(cond){
  # gives a good to fail rating for cond.
  rate = rep("good", length(cond))
  rate[cond<5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

s$rate = rateIt(s$cond)
table(s$cond)
table(s$rate)
s = filter(s, cond>1)

s %>% group_by(YEAR_BUILT_027) %>%
  summarize(prop = mean(rate=="good")) %>%
  ggplot(mapping = aes(x = YEAR_BUILT_027, y = prop)) + 
  geom_point()
s %>% group_by(YEAR_BUILT_027) %>%
  summarize(meanADT = mean(ADT_029)) %>%
  ggplot(mapping = aes(x = YEAR_BUILT_027, y = meanADT)) + 
  geom_point()

ggplot(data = s, mapping = aes(x = rate,fill = FRACTURE_092A)) + 
  geom_bar()

