

library(targets)

###############
# Run analysis
# tar_make()

######################################
# Run analysis with parallel computing
# tar_make_clustermq(workers=16)

# Visualize analysis
# tar_visnetwork(targets_only = TRUE, label = 'time') # visualizes analysis pipeline

# Examine object from analysis
# tar_read(printsByPopulation, branches=1)

# Diagnostics 
# View(tar_meta()) # useful tool for diagnostics
# View(tar_meta(targets_only = TRUE)) # simplified


# Parameters for SOCPROG LAR visualizations

# SOCPROG - plot nicely moving average selected values (all individuals), after comma is with restrictions to remove YAs
# all - 4000, 2000
# ff - 1000, 1500
# mm - 1200, 1200
# mf - 700, 1000
# fm - 500, 1000

# SOCPROG - plot nicely moving average selected values (reliable only)
# all - 2000, 1500
# ff - 250, 300
# mm - 1000, 1000
# mf - 500, 500
# fm - 500, 500


# Calculate number of days that residents and transients are observed, on average

# g <- tar_read(group_df_all_old)
# g[,day:=as.Date(groupTime),]
# g[,numPeriods:=length(unique(as.Date(groupTime))),by=c('Title', 'year')]
# g[,daySpan:=as.numeric(max(day)-min(day)), by=c('Title','year')]
# 
# # number of days observed within year
# hist(g$numPeriods)
# 
# # span of days observed within year
# hist(g$daySpan)
# 
# # unique dataframe
# d <- unique(g[,c('Title','year','residency','numPeriods','daySpan')])
# 
# # calculate means
# d[,mean(numPeriods, na.rm=TRUE),by=residency] # resident 2.2, transient 1.6
# d[,mean(daySpan, na.rm=TRUE),by=residency] # resident 9.4, transient 3.9


# Prepare public data

# #write.csv(tar_read(k_data), './Public-Data/k_data.csv') # Not necessary
# write.csv(tar_read(k_data_relatedness), './Public-Data/k_data_relatedness.csv')
# write.csv(tar_read(k_data_unks_included), './Public-Data/k_data_unks_included.csv')
# 
# write.csv(tar_read(k_data_mm), './Public-Data/k_data_mm.csv')
# write.csv(tar_read(k_data_ff), './Public-Data/k_data_ff.csv')
# write.csv(tar_read(k_data_fm), './Public-Data/k_data_fm.csv')


# Group size summary statistics -------------------------------------------

# g <- tar_read(group_df_all_old) # Note, adults only
# groups <- unique(g[,c('myGroups','gs'),])
# mean(groups$gs) # mean 2.43 adults per group
# sd(groups$gs) # sd 1.89
# range(groups$gs)


# Sex composition summary statistics --------------------------------------

# g[,includes_unk_sex:="Unknown" %in% sex, by='myGroups'] # any individuals of unknown sex in group
# g[,solitary:=gs==1, by='myGroups'] # whether group had 1 adult only (note there may have been calves or juvs!)
# g[,mixed:=all(("Male" %in% sex) & ("FemaleJ" %in% sex)),by='myGroups'] # whether both males and females were observed
# groups <- unique(g[,c('myGroups','gs','includes_unk_sex','solitary','mixed'),])
# 
# groups[,.N,by=solitary] # 640 "solitary" -- pretty high ######
# groups[solitary==FALSE,.N,by=includes_unk_sex] # 456/(456+545) # 45.6% of groups with >1 adult included adults of unknown sex
# groups[solitary==FALSE,.N,by=mixed] # 357/(644+357) # 35.7% of groups with >1 adult included both males and females



# Raw proportions for residency patterns ----------------------------------

# full <- tar_read(k_data_unks_included)
# 
# full[dSex=='M-M' & likely.k!='1' & dRes=='R-R',.N,] / full[dSex=='M-M' & dRes=='R-R',.N,] # 25% of R-R male-male dyads weak or strong
# full[dSex=='M-M' & likely.k!='1' & dRes=='R-T',.N,] / full[dSex=='M-M' & dRes=='R-T',.N,] # 8% R-T male-male dyads weak or strong
# 
# full[dSex=='F-M' & likely.k!='1' & dRes=='R-R',.N,] / full[dSex=='F-M' & dRes=='R-R',.N,] # 14% of R-R female-male dyads weak or strong
# full[dSex=='F-M' & likely.k!='1' & dRes=='R-T',.N,] / full[dSex=='F-M' & dRes=='R-T',.N,] # 6% R-T female-male dyads weak or strong
# 
# full[dSex=='F-F' & likely.k!='1' & dRes=='R-R',.N,] / full[dSex=='F-F' & dRes=='R-R',.N,] # 15% of R-R female-male dyads weak or strong
# full[dSex=='F-F' & likely.k!='1' & dRes=='R-T',.N,] / full[dSex=='F-F' & dRes=='R-T',.N,] # 8% R-T female-male dyads weak or strong
# 



