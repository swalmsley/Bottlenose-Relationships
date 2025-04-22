
# Custom functions to be called in '_targets.R'


###########################################################################
# Basic functions
###########################################################################


# Read data ---------------------------------------------------------------
read_data <- function(path) {
  d <- data.table(read.csv(path))
  return(d)
}

read_data_excel <- function(path) {
  d <- data.table(read_xlsx(path))
  return(d)
}



# Save plot ---------------------------------------------------------------
save_figure <- function(path, w, h, call) {
  png(path, width=w, height=h, units='in', res=1000)
  print(call)
  dev.off()
}
save_figure_transparent <- function(path, w, h, call) {
  png(path, width=w, height=h, units='in', res=1000, bg = 'transparent')
  print(call)
  dev.off()
}


# In-line effect ----------------------------------------------------------
inline <- function(fit, var, p, CI) {
  
  est <- summary(fit,prob=p)$fixed[paste(var),'Estimate']
  low <- summary(fit,prob=p)$fixed[paste(var),'l-90% CI']
  high <- summary(fit,prob=p)$fixed[paste(var),'u-90% CI']
  
  # report credible interval
  if (CI) output <- paste(paste(format_number(est), ', ', sep = ''), 'CI ', paste(format_number(low), format_number(high), sep = ' -- '), sep = '')
  
  # report probability of directional effect
  if (!CI) output <- paste(paste(format_number(est), ', ', sep = ''), p_dir <- paste('pd = ', round(as.numeric(p_direction(fit, parameters = paste(var)))*100,1), '%',sep=''), sep = '')
  
  return(output)
  
}
# fit <- tar_read(m3)
# var <- 'numSamplingPeriodsByYear'
# p = 0.9
# CI = FALSE



# Helper for inline effect ------------------------------------------------
format_number <- function(num) {
  if (abs(num) < 0.01) {

    # Convert to scientific notation with custom formatting
    formatted_number <- format(num, scientific = TRUE, trim = TRUE, digits = 2)
    # Extract mantissa and exponent
    parts <- strsplit(formatted_number, "e", fixed = TRUE)[[1]]
    # Print in the desired format
    out <- capture.output(cat(sprintf("%.2f x 10^%+d\n", as.numeric(parts[1]), as.numeric(parts[2]))))
    return(out)

  } else {
    return(round(num, 2))
  }
}


###########################################################################
# Project-specific functions
###########################################################################



prep_SOCPROG_supplemental <- function(meta) {

  meta[,ID:=Title,]
  output <- unique(meta[,c('ID','sex'),])
  return(output)

}
# meta <- tar_read(nbw_meta_raw)



prep_SOCPROG_input <- function(g, reliable_only) {

  # cutting off any observations of individuals deemed non-reliable at a given point in time
  if (reliable_only==TRUE) (g <- g[reliability=='Reliable',,])

  g[,Date:=groupTime,]
  g[,ID:=Title,]
  g[,Grp:=myGroups,]

  output <- g[,c('Date','Grp','ID'),]
  return(output)

}
# g <- tar_read(group_df_all)




plot_socprog_LAR_results_panel <- function(path, type, title) {

  # Get the list of Excel files in the directory
  files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)

  # Read all files and add a column with the file name
  data_list <- lapply(files, function(file) {
    # Read the Excel file
    data <- read_excel(file)

    # Add a column with the file name
    data$file_name <- sub("\\.xlsx$", "", basename(file))

    return(data)
  })

  # Combine all data frames into one data.table
  all_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

  # Select desired data
  df <- all_data[file_name==type,,]
  remaining <- all_data[file_name!='type' & Type=='LAR',,] # other curves to plot in background for reference

  # Errrobar data
  error_df <- df[Type %in% c('Jacknife_LAR','Jacknife_Null'),,]
  error_df[,ymin:=min(Y), by=c('X','Type')]
  error_df[,ymax:=max(Y), by=c('X','Type')]
  error_df <- unique(error_df[,c('X','ymin','ymax','Type')])
  error_df[Type=='Jacknife_LAR',Type:='LAR',]
  error_df[Type=='Jacknife_Null',Type:='Null',]

  # # Pull out points at which line crosses null
  # LAR <- df[Type=='LAR',,]
  # Null <- df[Type=='Null']
  # # First cross-point of LAR and Null association rate within association rate of 0.01 (sometimes doesn't cross, gives approx. value or conservative estimate)
  # inds <- which(LAR$Y <= Null$Y + 0.01)
  # if (sum(inds)>0) (v <- as.character(round(LAR$X[min(inds)]/365)))
  # if (length(inds)==0) (v <- paste(as.character(round(LAR$X[max(which(LAR$Y <= Null$Y + 0.02))]/365)), '+', sep=''))
  # Actually, because this finds first intersection, likely underestimates return to null -- best to just squint at figures probably

  # Plot LAR result
  ggplot(df[Type %in% c('LAR','Null'),], aes(x=log(X), y=Y, group=Type, color=Type, linetype=Type)) +
    geom_line(inherit.aes=FALSE, data=remaining, aes(x=log(X), y=Y, group=file_name),linewidth=0.6, color='grey', alpha=0.5)+
    geom_line(linewidth=1) +
    scale_color_viridis(discrete=TRUE, option=proj_color, begin=0, end=0.5)+
    geom_errorbar(inherit.aes=FALSE, data=error_df, aes(x=log(X), ymin=ymin, ymax=ymax, color=Type), width=0.2, size=0.5)+
    labs(x='Lag (days)', y='Association rate') +
    xlim(1, log(7600))+
    ylim(-0.001, 0.32)+
    ggtitle(title)+
    theme_classic() +
    # annotate('text',x=7,y=0.3,label=paste('Duration \u2248 ', as.character(v), 'years'), size=4)+
    scale_x_continuous(breaks=c(log(10),log(100),log(1000),log(10000)), labels=c(10, 100, 1000, 10000))+
    theme(legend.position = "none")

}
# path <- './SOCPROG/SP_results'
# type='fm'
# title = 'All relationships'

# path <- './SOCPROG/SP_results/Reliable'
# type='all_reliable'
# title = 'all'



# Process photo-ID observations -------------------------------------------
process_photoID <- function(lv_data, chosen_side, chosen_canyons){
  
  # ## Manual corrections based on 2024 fieldwork 
  lv_data[Title=='6536',Title:='2048',]
  ###### For revision: Consider re-running SOCPROG analyses or can wait for potential additional sex class information

  # extract side information
  lv_data[,side:=ifelse(grepl("Left Side", Keyword.export),"Left","Right"),] # extract side from keyword list
  lv_data[,numSides:=length(unique(side)),by=Title] # identify IDs that are 2-sided

  # fix up date information
  lv_data[,dateTime:=as.POSIXct(Date.Original),] # reformat dateTimes 
  lv_data[,day:=format(dateTime, format = "%Y-%m-%d")]
  lv_data[,year:=year(day),] # create a column of unique dates for use later
  lv_data[,numDaysByYear:=length(unique(day)),by=year]

  # remove observations with multiple or unknown whales
  lv_data <- lv_data[Title!="see crops",,]
  lv_data <- lv_data[Title!="unk",,]

  # add extra columns to indicate number of observations
  lv_data[,obsByYear:=.N,by=year] # count number of observations by year
  lv_data[,numDaysByTitle:=length(unique(day)),by=Title] # count number of days each individual was detected

  # add columns
  lv_data[, canyon:=NA,]
  lv_data[, canyon:=ifelse(grepl('Gully', Keyword.export),'Gully',NA),]
  lv_data[, canyon:=ifelse(grepl('Shortland', Keyword.export),'Shortland',canyon),]
  lv_data[, canyon:=ifelse(grepl('Haldimand', Keyword.export),'Haldimand',canyon),]
  # two photos lacking canyon ID but they are either see crops or unknown Title -- can be ignored

  # subset by location and side
  output <- lv_data[canyon%in%c(chosen_canyons),,]
  output <- output[side==chosen_side,,]

  return(output)

}
# lv_data <- tar_read(raw_photoData)
# chosen_side <- 'Left'
# chosen_canyons <- 'Gully'



build_binary_association_data <- function(photoData) {

  # Step 1 - build dyadic dataframe (row = day-dyad combinations where at least one individual in dyad was observed)

  photoData <- data.table(photoData)

  # Note: currently hard-coding days as sampling periods
  dyads <- data.table(t(combn(photoData[,unique(Title),], 2))) # generate all dyads
  colnames(dyads) <- c('A', 'B') # rename ID columns
  df <- data.table(A = as.character(), B = as.character(), sampling_day = as.character(), A_observed = as.integer(), B_observed = as.integer())   # set up empty data frame for filling

  for (d in unique(photoData$day)) { # create a new row for each observation day and each possible dyad in the dataset
    temp <- dyads[, sampling_day:=d, ]
    temp[, A_observed:=ifelse(A %in% photoData[day==d,unique(Title),],1,0)]
    temp[, B_observed:=ifelse(B %in% photoData[day==d,unique(Title),],1,0)]
    temp <- temp[A_observed==1 | B_observed==1,,] # prune out rows (day-dyad combinations) where neither member of dyad was observed
    df <- rbindlist(list(df, temp))
  }

  df[,index:=.I,] # add index for row number
  df[,bothPresent:=ifelse((A_observed==1 & B_observed==1),1,0),]

  df[,dyad:=paste(pmin(A,B),pmax(A,B),sep='_'),by=c('A','B')] # add useful dyad names


  # Step 2 - calculate associations

  df[bothPresent==1,together:=binary_association(photoData[day==sampling_day,,], A, B, 10), by='index']
  df[is.na(together),together:=0,] # note on this in brainstorming section - currently assuming that presence on same day but not seen together is equivalent to not being present on same day

  # Step 3 - formatting and standardization
  # could pull in NBW metadata here if we wanted

  df[,day:=sampling_day,]
  df[,sampling_day:=NULL,]

}
# photoData <- tar_read(side)



# Compute binary associations ---------------------------------------------
binary_association <- function(photos_sampling_period, ID1, ID2, time_limit) {

  # Pre-filter the data
  ID1_obs <- photos_sampling_period[(Title %in% ID1),,] # observations of ID 1 within sampling period
  ID2_obs <- photos_sampling_period[(Title %in% ID2),,] # observations of ID 2 within sampling period

  # Proceed only if there are any observations for ID1 or ID2 in the given sampling period (otherwise return 0 right away)
  if (nrow(ID1_obs) == 0 || nrow(ID2_obs) == 0) return(0)

  # Calculate the time difference between all combinations of observations
  time_diff <- outer(ID1_obs$dateTime, ID2_obs$dateTime, FUN = function(x, y) abs(as.numeric(difftime(x, y, units = 'mins'))))

  # Check if any time difference is within the time_limit
  if (any(time_diff <= time_limit)) return(1)

  return(0)

}



# Permute associations ----------------------------------------------------
permute_associations <- function(side, nIter, nbw_meta_raw) {

  result <- data.table(iteration=1:nIter, pNever=as.numeric())

  for (i in 1:nIter) {

    shuffled <- side
    shuffled[, Title:=sample(Title, replace = FALSE), by=year] # shuffle Titles within years

    associations_raw <- build_binary_association_data(shuffled)
    associations <- overlappingIDs(associations_raw, nbw_meta_raw)

    associations[,sumTogether:=sum(together),by=dyad]
    prop_never <- associations[sumTogether==0,length(unique(dyad)),] / associations[,length(unique(dyad)),]

    result[i, pNever:=prop_never, ]

  }

  return(result)

}
# side <- tar_read(side)
# nIter <- 100
# nbw_meta_raw <- tar_read(nbw_meta_raw)




# Build NBW metadata ------------------------------------------------------
# takes raw LV output as input ('dt')
build_nbw_metadata <- function(dt, chosen_side) {

  # format dates
  dt[,dateTime:=as.POSIXct(Date.Original),]
  dt[,day:=format(dateTime, format = "%Y-%m-%d")]
  dt[,year:=year(day),]

  # remove photographs that were processed as crops
  dt <- dt[Title!="see crops",,]
  dt <- dt[Title!="unk",,]
  
  ###### manual edits
  ## merged ID
  dt[Title=='6536',Title:='2048',]
  ## photo-ID error fix
  dt[Title==6527 & year==2021,ageClass:='Juvenile',]
  
  # DO RESIDENCY BEFORE restricting by side 
  dt[,residency:=ifelse(length(unique(year))==1, 'Transient', 'Resident'),by=Title]
  dt[residency=='Transient' & year %in% c(1988, 2023),residency:='Unknown',] ######

  # add left and right columns
  dt[grepl("Left", Keyword.export), side := "left"]
  dt[grepl("Right", Keyword.export), side := "right"]

  # subset to chosen side
  dt <- dt[side==chosen_side,,]

  # reliability
  dt[,reliability:='Not reliable',]
  dt[grepl('Notch|Back Indent', Keyword.export), reliability:='Reliable',by=c('Title','year')]

  # add columns with sex details
  dt=dt%>%mutate(sex = ifelse(grepl("FemaleJ", Keyword.export), "Female-Juvenile", NA))
  dt=dt%>%mutate(sex = ifelse(grepl("Male", Keyword.export), "Male", sex))
  dt$sex <- factor(dt$sex,levels=c("Female-Juvenile","Male"))

  # Step 1: Assign 'Calf' or 'Adult' based on 'Keyword.export'
  dt[, ageClass := ifelse(grepl('samCalf', Keyword.export), 'Calf', 'Adult')]
  
  # Step 2: Further refine 'ageClass' for non-'Calf' rows to include 'Juvenile'
  dt[ageClass == 'Adult' & grepl('samJuv', Keyword.export), ageClass := 'Juvenile']
  
  # Step 3: Create 'young' column indicating if the individual is either 'Calf' or 'Juvenile'
  dt[, young := ageClass %in% c('Calf', 'Juvenile')]

  # # add minimum age by year
  dt[,minYear:=min(year),by=Title]
  dt[,maxYear:=max(year),by=Title]
  dt[,yearSpan:=1+(maxYear-minYear),by=Title]
  dt[,catalogueAge:=year-minYear,] # Note: considering first year as "0"

  dt[, FirstYearAgeClass := unique(ageClass[minYear == year]), by = Title]

  dt[FirstYearAgeClass=='Calf',minimumAge:=catalogueAge,]
  dt[FirstYearAgeClass=='Juvenile',minimumAge:=catalogueAge+1,]
  dt[FirstYearAgeClass=='Adult',minimumAge:=catalogueAge+3,]

  dt[,minAge_in_2030:=(2030-year)+minimumAge,]

  # add day of year
  dt[,yday:=yday(day),]

  # diagnostic tests
  test_that("single sex classification for each individual", {
    expect_true(unique(dt[,length(unique(sex)),by=Title]$V1)==1)
  })

  # test_that("single minimum age for each individual in each year", {
  #   expect_true(unique(dt[,length(unique(minimumAge)),by=c('Title', 'year'),]$V1)==1)
  # })
  #
  # sample one observation per individual per year and clean up dataset
  subsampled <- dt[dt[ , .I[sample(.N,1)] , by = c('Title','year')]$V1]
  meta <- subsampled[,c('Title', 'side', 'year', 'sex', 'minYear', 'maxYear', 'yearSpan', 'ageClass', 'yday', 'minimumAge', 'minAge_in_2030', 'residency', 'reliability'), ]

  return(meta)

}
# dt <- tar_read(raw_photoData)
# chosen_side = 'left'



# Add columns to association data denoting whether each ID was known in the catalogue during each year in question
overlappingIDs <- function(dt, meta) {

  list <- unique(meta[,c('Title','minYear','maxYear'),]) # key with min and max year detected for each Title

  dt[,year:=year(day),]
  dt[,A_in_catalogue:=((year >= list[Title==A,minYear,]) & (year <= list[Title==A,maxYear,])),by=A] # assign column for if A present in catalogue
  dt[,B_in_catalogue:=((year >= list[Title==B,minYear,]) & (year <= list[Title==B,maxYear,])),by=B] # assign column for if B present in catalogue
  dt[,both_in_catalogue:=(A_in_catalogue & B_in_catalogue),by=.I] # assign column for if both present in catalogue

  restricted <- dt[both_in_catalogue==1,,]
  return(restricted)
}
# dt <- tar_read(associations_raw)
# meta <- tar_read(nbw_meta_raw)

# dt <- tar_read(group_associations_no_demo_restriction_day)
# meta <- tar_read(nbw_meta_raw)


# Pull out edges
edge_list <- function(fit, include_zeros) {

  # identify non-zero dyads
  m_data <- data.table(fit$data)
  m_data[,nAssociations:=sum(together),by=dyad]
  nz_dyads <- m_data[nAssociations>=1,unique(dyad),]

  # extract all edge samples
  all_edge_samples <- data.frame(coef(fit, summary=FALSE))
  colnames(all_edge_samples) <- str_extract(colnames(all_edge_samples), "\\d+_\\d+")

  # apply transformation to all samples
  edge_samples <- data.frame(lapply(all_edge_samples, inv_logit), check.names = FALSE)

  # extract edge list
  edges <- data.table(data.frame(dyad=colnames(edge_samples),
                                 edge=colMeans(edge_samples),
                                 edge_sd=apply(edge_samples, 2, sd)))

  # exclude non-zero dyads if desired
  edges <- merge(edges, unique(m_data[,c('dyad', 'nAssociations'),]), by='dyad')
  if (!include_zeros) (edges <- edges[dyad %in% nz_dyads,,])

  return(edges)

}
# fit <- tar_read(brms_fit)
# include_zeros <- FALSE

# fit <- tar_read(brms_fit_group)
# include_zeros <- FALSE




focal_edges <- function(focal, edge_list) {

  #pull out A and B
  edge_list$A <- sub("^(\\d+)_.*", "\\1", edge_list$dyad) # pull out ID A
  edge_list$B <- sub("^\\d+_(\\d+)$", "\\1", edge_list$dyad) # pull out ID B

  # extract relevant edges for focal individual
  focal_edge_list <- edge_list[A==focal | B==focal,,]
  return(focal_edge_list)

}
# focal <- '54'
# edge_list <- tar_read(nz_edges)



max_edge <- function(edge_list) {

  #pull out A and B
  edge_list$A <- sub("^(\\d+)_.*", "\\1", edge_list$dyad) # pull out ID A
  edge_list$B <- sub("^\\d+_(\\d+)$", "\\1", edge_list$dyad) # pull out ID B

  # unique IDs
  IDs <- unique(c(edge_list$A,edge_list$B))

  ID_df <- data.table(Title=IDs)

  for (i in ID_df$Title) {

    edges <- focal_edges(i, edge_list)

    ID_df[Title==i,maxEdge:=max(edges$edge),]

  }

  return(ID_df)
}
# edge_list <- tar_read(edges_df_group)



edge_sex <- function(edges, meta) {

  edges$A <- sub("^(\\d+)_.*", "\\1", edges$dyad) # pull out ID A
  edges$B <- sub("^\\d+_(\\d+)$", "\\1", edges$dyad) # pull out ID B

  # Incorporate sex information

  edges[,sexA:=meta[Title==A,unique(sex),],by=A]
  edges[,sexB:=meta[Title==B,unique(sex),],by=B]
  edges[is.na(sexA),sexA:='Unk',]
  edges[is.na(sexB),sexB:='Unk',]
  edges[,dyadicSex:=paste(sort(c(sexA,sexB))[1], sort(c(sexA,sexB))[2], sep="_"),by=.I]

  edges[dyadicSex=='Male_Male',dSex:='M-M',]
  edges[dyadicSex=='Female-Juvenile_Male',dSex:='F-M',]
  edges[dyadicSex=='Male_Unk',dSex:='M-Unk',]
  edges[dyadicSex=='Female-Juvenile_Unk',dSex:='F-Unk',]
  edges[dyadicSex=='Female-Juvenile_Female-Juvenile',dSex:='F-F',]
  edges[dyadicSex=='Unk_Unk',dSex:='Unk-Unk',]

  return(edges)

}
# edges <- tar_read(edges_ma_nz)
# meta <- tar_read(nbw_meta_raw)



edge_age_diff <- function(edges, meta) {

  edges$A <- sub("^(\\d+)_.*", "\\1", edges$dyad) # pull out ID A
  edges$B <- sub("^\\d+_(\\d+)$", "\\1", edges$dyad) # pull out ID B

  # incorporate age information (looking for differences in minimum age)

  edges[,absAgeA:=meta[Title==A,unique(minAge_in_2030),],by=A]
  edges[,absAgeB:=meta[Title==B,unique(minAge_in_2030),],by=B]

  edges[,ageDiff:=abs(absAgeA-absAgeB),by=c('A','B')]

  return(edges)

}
# edges <- tar_read(nz_edges)
# meta <- tar_read(nbw_meta_raw)



edge_residency <- function(edges, meta) {

  edges$A <- sub("^(\\d+)_.*", "\\1", edges$dyad) # pull out ID A
  edges$B <- sub("^\\d+_(\\d+)$", "\\1", edges$dyad) # pull out ID B

  # incorporate residency information

  # Note technically this will fail if an individual is seen in 1988 and 2023 only (did not occur)
  edges[,resA:=meta[Title==A,unique(residency),], by=A]
  edges[,resB:=meta[Title==B,unique(residency),], by=B]

  # leave unknowns in for other analyses, cut them out when modelling
  # edges <- edges[resA!='Unknown' & resB!='Unknown']

  edges[,dyadicResidency:=paste(sort(c(resA,resB))[1], sort(c(resA,resB))[2], sep="_"),by=dyad]

  # main combinations
  edges[dyadicResidency=='Resident_Resident',dRes:='R-R',]
  edges[dyadicResidency=='Resident_Transient',dRes:='R-T',]
  edges[dyadicResidency=='Transient_Transient',dRes:='T-T',]
  # combinations with unknowns (rarer), and no T-Us 
  edges[dyadicResidency=='Resident_Unknown',dRes:='R-U',]
  edges[dyadicResidency=='Unknown_Unknown',dRes:='U-U',]
  
  edges$dRes <- factor(edges$dRes, levels=c('R-R','R-T','T-T', 'R-U', 'U-U'))

  return(edges)

}
# edges <- tar_read(nz_edges_age_ma)
# meta <- tar_read(nbw_meta_raw)



edgePlot <- function(df) {

  # make nicer labels
  df[dSex=='F-F',dSex:='Female-Female',]
  df[dSex=='F-M',dSex:='Female-Male',]
  df[dSex=='M-M',dSex:='Male-Male',]

  g <- ggplot(df[!(is.na(dSex)) & !(dSex %in% c('Unk-Unk','M-Unk','F-Unk'))], aes(x=edge, group=dSex, fill=dSex)) +
    facet_wrap(~dSex,ncol=3)+
    geom_histogram(aes(y = after_stat(!!str2lang("density")), group=dSex),colour=1,fill='white')+
    geom_density(alpha=0.8,adjust=1)+
    labs(x='Strength of association', y='Density')+
    theme_classic()+
    scale_fill_viridis(discrete=TRUE, option='A')+
    theme(axis.text.y=element_text(size=12), strip.text = element_text(size=12), legend.position = 'none')

  means <- df[!(is.na(dSex)) & !(dSex %in% c('Unk-Unk','M-Unk','F-Unk')), .(mean = mean(edge, na.rm = TRUE)), by = dSex]
  max_density <- max(density(df$edge, na.rm = TRUE)$y)

  g <- g + geom_text(data = means, aes(label = paste("μ =", round(mean, 2)), x = Inf, y = max_density), hjust = "right", vjust = 'top')

  g

}
# df <- tar_read(edges_df)
# df <- tar_read(nz_df)



age_effect_plot <- function(fit) {

  # data
  data <- data.table(fit$data)

  # # New data to predict for
  newdata <- expand.grid(dSex = unique(data$dSex),
                         ageDiff = 1:max(data$ageDiff),
                         edge_sd = 0.001) # we can leave out A and B as we are making re_formula NULL

  pred <- data.table(epred_draws(fit, newdata, re_formula=NA, ndraws = 500)) # aesthetic choice
  pred_means <- pred[, .(mean_epred=mean(.epred)),by=.(dSex, ageDiff)] # extract means for each combination predictors (if not interested in plotting whole ribbon!)
  

  pred$dSex <- factor(pred$dSex, levels=c('F-F','F-M','M-M'))

  dsex_names <- c(`M-M` = 'Male-Male',
                  `F-M` = 'Female-Male',
                  `F-F` = 'Female-Female')

  p <- ggplot(pred, aes(x=ageDiff, y=.epred, group=.draw, fill=dSex, color=dSex)) +
    facet_wrap(~dSex, labeller = as_labeller(dsex_names)) +
    geom_point(inherit.aes = FALSE, data=data, aes(x=ageDiff, y=edge), alpha=0.1)+
    geom_line(alpha=0.1, color='grey75') +
    geom_line(inherit.aes = FALSE, data=pred_means, aes(x=ageDiff,y=mean_epred, color=dSex), linewidth=1.75) +
    scale_fill_viridis(discrete=TRUE, option='A')+
    scale_color_viridis(discrete=TRUE, option='A')+
    labs(x='Difference in minimum age', y='Strength of association')+
    theme_classic() +
    theme(strip.text=element_text(size=12),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.position = "none")
  p

  return(p)

}
# fit <- tar_read(mAge)
# fit <- tar_read(m2_nz)


relatedness_effect_plot <- function(fit) {

  # data
  data <- data.table(fit$data)

  # New data to predict for
  newdata <- expand.grid(dSex = unique(data$dSex), # we can leave out A and B as we are making re_formula NULL
                         wang = seq(from=min(data$wang), to = max(data$wang), length.out=100),
                         edge_sd = 0.001) # setting SD to very small for visualization -- can experiment to see if makes a difference

  pred <- data.table(epred_draws(fit, newdata, re_formula=NA, ndraws = 500)) # aesthetic choice
  pred_means <- pred[, .(mean_epred=mean(.epred)),by=.(dSex, wang)] # extract means for each combination predictors (if not interested in plotting whole ribbon!)


  pred$dSex <- factor(pred$dSex, levels=c('F-F','F-M','M-M'))

  dsex_names <- c(`M-M` = 'Male-Male',
                  `F-M` = 'Female-Male',
                  `F-F` = 'Female-Female')

  p <- ggplot(pred, aes(x=wang, y=.epred, group=.draw, fill=dSex, color=dSex)) +
    facet_wrap(~dSex, labeller = as_labeller(dsex_names)) +
    geom_line(alpha=0.05, linewidth=2, color='grey75') +
    geom_line(inherit.aes = FALSE, data=pred_means, aes(x=wang,y=mean_epred, color=dSex), linewidth=1.75) +
    geom_point(inherit.aes = FALSE, data=data, aes(x=wang, y=edge), alpha=0.5)+
    scale_fill_viridis(discrete=TRUE, option='A')+
    scale_color_viridis(discrete=TRUE, option='A')+
    labs(x='Relatedness', y='Strength of association')+
    theme_classic() +
    # ylim(0,0.05)+
    theme(strip.text=element_text(size=12),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.position = "none")
  p

  return(p)

}
# fit <- tar_read(m1s)
# fit <- tar_read(m1)
# fit <- tar_read(m1q)




extract_iNEXT_data <- function(associations) {

  # Could compare this to discovery curve approach, maybe?

  # extract IDs of realized associations (together)
  together <- associations[together==1,,]
  ids <- unique(c(associations$A, associations$B))

  # build list for output
  sorted <- list()

  for (i in ids) {

    # extract associations for given individual
    temp <- together[A==i | B==i,,]
    temp[,numAssociations:=.N,by=dyad]
    unique <- unique(temp[,c('dyad','numAssociations')])

    # send to output
    if (nrow(unique)>0) (sorted[[i]] <- unique[,numAssociations,][order(-unique[,numAssociations,])]) # only if some associations detected
  }
  return(sorted)
}
# associations <- tar_read(group_associations_demo)



fit_mixture_model_all <- function(assoc, nbw_meta_raw, numCategories, cn) {

  # calculate numerator and denominator
  assoc[,numerator:=sum(together), by=dyad] # numerator - number of sampling days together
  assoc[,denominator:=.N, by=dyad] # denominator - number of sampling periods when at least one observed

  # fit model using socmixmods
  model.fitting = binom_assoc_mixt(Den = assoc$denominator,
                                   Num = assoc$numerator,
                                   edge.ids = assoc$dyad,
                                   minK = 1,
                                   maxK = numCategories,
                                   criterion = cn,
                                   run.all.K = TRUE,
                                   verbose = FALSE)

  return(model.fitting)

}
# assoc <- tar_read(group_associations_demo)
# assoc <- assoc[year==2021,,] # just to reduce fitting time, not necessary
# dyadSex <- 'F-M'
# nbw_meta_raw <- tar_read(nbw_meta_raw)
# numCategories <- 5
# cn <- 'BIC'




edge_sex_associations <- function(edges, meta) {
  
  # edges$A <- sub("^(\\d+)_.*", "\\1", edges$dyad) # pull out ID A
  # edges$B <- sub("^\\d+_(\\d+)$", "\\1", edges$dyad) # pull out ID B
  
  # Incorporate sex information
  
  edges[,sexA:=meta[Title==A,unique(sex),],by=A]
  edges[,sexB:=meta[Title==B,unique(sex),],by=B]
  edges[is.na(sexA),sexA:='Unk',]
  edges[is.na(sexB),sexB:='Unk',]
  edges[,dyadicSex:=paste(sort(c(sexA,sexB))[1], sort(c(sexA,sexB))[2], sep="_"), by=.I] 
  
  edges[dyadicSex=='Male_Male',dSex:='M-M',]
  edges[dyadicSex=='Female-Juvenile_Male',dSex:='F-M',]
  edges[dyadicSex=='Male_Unk',dSex:='M-Unk',]
  edges[dyadicSex=='Female-Juvenile_Unk',dSex:='F-Unk',]
  edges[dyadicSex=='Female-Juvenile_Female-Juvenile',dSex:='F-F',]
  edges[dyadicSex=='Unk_Unk',dSex:='Unk-Unk',]
  
  ###### Test this very carefully!
  # reconstruct sex and match to meta, for example
  ####### why is this different from edge_sex?
  
}
# edges <- tar_read(group_associations_demo) # not really edges but will do for now
# edges <- edges[year==2021,,]
# meta <- tar_read(nbw_meta_raw)



fit_mixture_models <- function(assoc, dyadSex, nbw_meta_raw, numCategories, cn) {

  # calculate numerator and denominator
  assoc[,numerator:=sum(together), by=dyad] # numerator - number of sampling days together
  assoc[,denominator:=.N, by=dyad] # denominator - number of sampling periods when at least one observed

  # incorporate dyadic sex information and subset associations of interest
  assoc <- edge_sex_associations(assoc, nbw_meta_raw)
  assoc <- assoc[dSex==dyadSex,,]

  # fit model using socmixmods
  model.fitting = binom_assoc_mixt(Den = assoc$denominator,
                                   Num = assoc$numerator,
                                   edge.ids = assoc$dyad,
                                   minK = 1,
                                   maxK = numCategories,
                                   criterion = cn,
                                   run.all.K = TRUE,
                                   verbose = FALSE)

  return(model.fitting)

}
# assoc <- tar_read(group_associations_demo)
# assoc <- assoc[year==2021,,] # just to reduce fitting time, not necessary
# dyadSex <- 'F-M'
# nbw_meta_raw <- tar_read(nbw_meta_raw)
# numCategories <- 5
# cn <- 'BIC'



# build pairwise relatedness estimates
calculate_relatedness <- function() {
  
  # Genetic data available at DOI https://doi.org/10.17605/OSF.IO/97AVS
  
  ## NOTE
  # There are significant issues with the current version of the genetic data (2022)
  
  # The data include several duplicated genetic profiles
  # Sometimes alternate IDs are used in the microsat sheet, which may not match what is used in the listview_biopsy spreadsheet
  # Until the data can be updated, it should be handled with caution
  # Simply merging based on "code" and "SampleID" in various datasets is likely to result in missed IDs
  
  # Note: Several important manual corrections are explained as follows:
  
  # 1. Duplicate of 5061 (seems unambiguous)
  # NBW04-2015 (GPL 004-2015) - ID 6112 and NBW06-2015 (GPL 006-2015) - ID 5061, genetically identical
  # Conclusion: BOTH 5061
  # Evidence: 5061 present at both biopsies in question, while 6112 was not, and one of the photos listed for NBW04-2015 is 5061
  # Action: Remove Sample NBW04-2015, which is currently listed as ID 6112, from consideration
  
  # 2. Potential duplicate of 124 (somewhat less clear who the sampled animal is)
  # HamSH0203 - ID 3 and HamSH9715 - ID 124, genetically identical
  # Conclusion: Probable that both are 124
  # Evidence: Individual 124 observed within an hour of 3's first potential biopsy occasion (August 8, 2002), 3 not observed at 124's biopsy occasion
  # However, seems there might be two biopsy photos in LV_biopsy for HamSH0203, on different days or even trips in August 2002?
  # So, Based on the first date (August 8 2002), likely that 124 is correct individual for both samples, BUT if second date is correct for HamSH0203 sample (August 27, 2002), somewhat less clear
  # Action: Remove Sample HamSH0203, which is currently listed as ID 3, from consideration

  # 3. Manual change of retired ID
  # ID 6422 was combined with 6246, this change will be made manually in "id", see below
  
  # Based on advice from T. Frasier and C. Konrad, will be analyzing relatedness within Scotian Shelf population
  
  # LV data from biopsy catalogue 
  lvb <- data.table(read_xlsx('./input/Microsats/LV_Biopsy_all.xlsx'))
  lvb <- unique(lvb[,c('Title','Caption'),])
  
  # Split the "Caption" column into two columns
  lvb[, c("Caption_1", "Caption_2") := tstrsplit(Caption, ", ", fixed = TRUE)]
  
  # Reshape the data into a long format
  lvb <- melt(lvb,
              id.vars = "Title", 
              measure.vars = c("Caption_1", "Caption_2"), 
              variable.name = "Caption_Index", 
              value.name = "Caption", 
              na.rm = TRUE)
  # Take unique
  lvb <- unique(lvb[,c('Title','Caption')])
  
  # Manual edits
  lvb[Caption=='NBW1-2013',Caption:='NBW01-2013',]
  lvb[Caption=='NBW2-2013',Caption:='NBW02-2013',]
  lvb[Caption=='NBW3-2013',Caption:='NBW03-2013',]
  lvb[Caption=='NBW6-2013',Caption:='NBW06-2013',]
  
  # Manual additions ("Alternate code" shown in listview_biopsy sheet)
  lvb[Caption=='NBW19-01',Caption:='BKW0955',]
  lvb[Caption=='NBW19-02',Caption:='BKW0956',]
  lvb[Caption=='NBW19-03',Caption:='BKW0957',]
  lvb[Caption=='NBW19-04',Caption:='BKW0958',]
  lvb[Caption=='NBW19-05',Caption:='BKW0959',]
  lvb[Caption=='NBW19-06',Caption:='BKW0960',]
  lvb[Caption=='NBW19-07',Caption:='BKW0961',]
  lvb[Caption=='NBW19-08',Caption:='BKW0962',]
  
  lvb[Title=='WL002_1996SS',Title:='9907',]
  
  # update with more recent ID
  lvb[Title==6422,Title:=6246,,]
  
  # Pull in microsatellite data
  microsats <- data.table(read.csv('input/Microsats/TableS1_msat_genalex_2021_trimmed.csv', fileEncoding = 'UTF-8-BOM'))
  microsats <- microsats[SampleID!='NBW04-2015',,] # Duplicate of 5016 (not shown as duplicate in data, needs to be corrected in repository)
  microsats <- microsats[SampleID!='HamGY0305'] # Fine but duplicate of 480 (does not need to be corrected, just duplicate)
  microsats <- microsats[SampleID!='NBW06-2013'] # Fine but duplicate of 4513 (does not need to be corrected, just duplicate)
  microsats <- microsats[SampleID!='NBW02-2013'] # Fine but duplicate of 92 (does not need to be corrected, just duplicate)
  microsats <- microsats[SampleID!='HamSH0203'] # Fine but duplicate of 92 (does not need to be corrected, just duplicate)
  
  # Merge in IDs for microsats
  # just lvb codes in microsats
  lvb <- lvb[lvb$Caption %in% microsats$SampleID]
  
  m <- merge(microsats, lvb, all.x=TRUE, all.y=TRUE, by.x='SampleID', by.y='Caption')
  
  m <- m[!is.na(Title),,] # remove microsattelite data with no associated individual title
  m <- m[!(Title %in% c("unk", "NF25", "NF64", "NF65", "NF30", "NF79", "NF50", "NF59")),,] # remove NL and WL IDs
  
  key <- unique(m[,c('SampleID','Title')])

  # Calculate relatedness
  m[,POP:=NULL,] # remove population column
  m_data <- m[, Title:=NULL,] # remove Title column
  
  NBW <- coancestry(m_data, wang=2)
  spread <- data.table(NBW$relatedness.ci95) # examine 95% confidence intervals
  pairs <- data.table(NBW$relatedness)
  pairs <- pairs[,c("pair.no", "ind1.id", "ind2.id", "wang")]
  output <- merge(pairs, spread[,c('pair.no','wang.low','wang.high'),], by='pair.no') 

  output <- merge(output, key, by.x='ind1.id', by.y='SampleID')
  output <- merge(output, key, by.x='ind2.id', by.y='SampleID')
  output[,dyad:=paste(sort(c(Title.x, Title.y))[1], sort(c(Title.x,Title.y))[2], sep="_"),by=c('Title.x','Title.y')]

  return(output)

}


# Pull out biopsy sex data ------------------------------------------------
biopsySexTitles <- function(d) {
  
  d[grepl("biopsy-Female", Keyword.export), biopsySex:='Y',]
  d[grepl("biopsy-Male", Keyword.export), biopsySex:='Y',]
  titles <- unique(d[Title!='unk' & biopsySex=='Y',Title,])
  
  return(titles)
  
  
}
#d <- tar_read(raw_photoData)


dyadic_biopsy <- function(df, biopsyTitles) {
  
  df[,biopsyA:=ifelse(A %in% biopsyTitles,1,0),by=A]
  df[,biopsyB:=ifelse(B %in% biopsyTitles,1,0),by=B]
  
  df[,dyadic_biopsy:=sum(biopsyA, biopsyB),by=.I]
  df$dyadic_biopsy <- as.factor(df$dyadic_biopsy)
  
  return(df)
  
  
}
# df <- tar_read(k_data)
# biopsyTitles <- tar_read(biopsyTitles)




# Plot mixture model
socMod_plot_edit <- function(mixture_result, title, manual_k=NA) {

  if (is.na(manual_k)) (chosen <- get_bestmodel(mixture_result, verbose=FALSE))
  if (!is.na(manual_k)) (chosen <- mixture_result$all.models[[manual_k]])

  dat <- by_edge(chosen)
  dat$SRI = chosen$n/chosen$d
  dat$likely.k = as.factor(dat$likely.k)

  if ((length(unique(dat$likely.k))) > 3) (rel_colors <- viridis(length(unique(dat$likely.k)), begin=1, end=0))

  p <- ggplot(dat, aes(SRI, fill = likely.k, alpha = 0.5))+
    geom_density(bw=0.01, linewidth=0.05)+
    scale_color_manual(values=rel_colors) +
    scale_fill_manual(values=rel_colors) +
    ggtitle(paste(title))+
    theme_classic()+
    xlim(-0.02,1)+
    # xlim(0,0.7)+
    labs(y='Density',x='Association index')+
    # scale_y_continuous(trans = scales::pseudo_log_trans(base = 10))+
    theme(legend.position='none')
  p

  return(p)

}
# mixture_result <- tar_read(fm_mixtures)
# title <- 'Female-Male'
# manual_k <- 3




# mixture model result table
mixture_table <- function(mixture_result) {

  m <- mixture_result

  d <- data.frame(paste(round(m$k.mean,2)), paste(round(m$k.freq,2)))
  d <- data.frame(sprintf("%.2f", m$k.mean), sprintf("%.2f", m$k.freq))

  colnames(d) <- c('Category mean','Frequency')

  return(nice_table(d))

}
# mixture_result <- get_bestmodel(tar_read(ff_mixtures_t))



multinomial_preds <- function(fit, var, id_label) {

  data <- data.table(fit$data)

  # New data for prediction
  newdata <- expand.grid(var = seq(from=min(data[[var]]), to=max(data[[var]]), length.out=50))
  setnames(newdata, "var", var)

  # Get posterior predictions
  pred <- data.table(epred_draws(fit, newdata, re_formula=NA, ndraws = 500))

  # Compute mean and credible intervals
  pred_summary <- pred[, .(
    mean_epred = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95)
  ),  by = c(".category", var)]

  # Define custom category labels
  category_labels <- c(`1` = "Absent", `2` = "Weak", `3` = "Strong")  # Adjust as needed

  # Convert .category to a factor with readable labels
  pred_summary[, .category := factor(.category, levels = names(category_labels), labels = category_labels)]

  # Add label
  pred_summary$id <- id_label

  return(pred_summary)

}
# fit <- tar_read(mk_Age_mm)
# var <- 'ageDiff'
# id_label <- 'MM'





plot_age_effect <- function(fits, var, xlab, chosen_categories) {
  
  # Calculate predictions
  p1 <- multinomial_preds(fits[[1]], var, 'mm') # important that order is retained - add test at end of function
  p2 <- multinomial_preds(fits[[2]], var, 'fm')
  p3 <- multinomial_preds(fits[[3]], var, 'ff')
  
  # Bind predictions from different models together
  d <- rbind(p1, p2, p3)
  
  # fix up sex labels
  d[id=='mm',id:='Male-Male',]
  d[id=='fm',id:='Female-Male',]
  d[id=='ff',id:='Female-Female',]
  d$id <- factor(d$id, levels=c('Male-Male','Female-Male','Female-Female'))
  
  # Plot
  p <- ggplot(d[.category %in% chosen_categories, ], aes(x = get(var), y = mean_epred, color = .category, linetype = id)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(.category)), colour=NA, alpha=c(rep(0.2,50), rep(0.05, 50), rep(0.05, 50)))+ # Fixed alpha for ribbons
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = rel_colors[chosen_categories == levels(d$.category)]) +
    scale_fill_manual(values = rel_colors[chosen_categories == levels(d$.category)]) +
    scale_linetype_manual(name = "", values = c("solid", "dashed", "dotted")) +
    labs(x = xlab, y = "Probability of forming strong relationship") +
    guides(fill = FALSE, color = FALSE, alpha = FALSE, linetype = guide_legend(override.aes = list(fill = c(NA, NA, NA)))) +  
    theme_classic() +
    xlim(0,28)+
    theme(legend.position = c(0.7, 0.7),  # Move legend inside the plot
          legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
          legend.text = element_text(size=8),
          legend.key = element_blank(),
          axis.title = element_text(size=8))
  
  p
  return(p)
  
}
# fits <- list(tar_read(mk_Age_mm), tar_read(mk_Age_fm), tar_read(mk_Age_ff))
# var <- 'ageDiff'
# xlab <- "Difference in Minimum Age"
# chosen_categories <- 'Strong'




sample_size_table <- function(data) {
  
  # Assuming your data is stored in a data frame called 'data'
  
  # Count unique groups by year
  data[, Number_of_Groups:=length(unique(myGroups)), by='year']
  
  # Add the number of individuals by year
  data[, Number_of_Individuals:=length(unique(Title)), by='year']
  
  # Add the number of unique days by year
  data[, Number_of_Days:=length(unique(as.Date(groupTime))), by='year']
  
  # Pull out and rename columns for clarity
  result <- unique(data[,c('year','Number_of_Groups','Number_of_Individuals','Number_of_Days'),])
  colnames(result) <- c("Year", "Number of Groups", "Number of Individuals", "Number of Sampling Days")
  
  result$Year <- as.character(result$Year)
  
  # Print the result
  return(nice_table(result))
  
}
# data <- tar_read(groups)




plot_multinomial <- function(fit, var, xlab, ylim) {

  # Extract data
  data <- data.table(fit$data)

  # New data for prediction
  newdata <- expand.grid(var = seq(from=min(data[[var]]), to=max(data[[var]]), length.out=50))
  setnames(newdata, "var", var)

  # Get posterior predictions
  pred <- data.table(epred_draws(fit, newdata, re_formula=NA, ndraws = 500))

  # Compute mean and credible intervals
  pred_summary <- pred[, .(
    mean_epred = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95)
  ),  by = c(".category", var)]

  # Define custom category labels
  category_labels <- c(`2` = "Weak", `3` = "Strong")  # Adjust as needed

  # Convert .category to a factor with readable labels
  pred_summary[, .category := factor(.category, levels = names(category_labels), labels = category_labels)]

  # Plot
  p <- ggplot(pred_summary[.category%in%category_labels], aes(x = get(var), y = mean_epred, group = .category, color = as.factor(.category))) +
    geom_line(linewidth = 1.5) +  # Mean prediction
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(.category)), alpha = 0.5) +  # Uncertainty
    scale_color_manual(values=rel_colors[2:3]) +
    scale_fill_manual(values=rel_colors[2:3]) +
    labs(x = xlab,
         y = "Probability of forming relationship",
         color = "Relationship",
         fill = "Relationship") +
    theme_classic() +
    ylim(ylim[1],ylim[2])

  p
  return(p)
}
# fit <- tar_read(mk_Age_mm)
# var <- 'ageDiff'
# xlab <- "Difference in Minimum Age"
# ylim <- c(0,1)

# fit <- tar_read(mk_rel_mm)
# var <- 'wang'
# xlab <- "Genetic relatedness"
# ylim <- c(0,1)




plot_multinomial_categoricalPredictor <- function(fit, var, xlab) {

  # Extract data
  data <- data.table(fit$data)

  # New data for prediction
  newdata <- expand.grid(unique(data[[var]]))
  setnames(newdata, "Var1", var)

  # Get posterior predictions
  pred <- data.table(epred_draws(fit, newdata, re_formula=NA, ndraws = 500))

  # Compute mean and credible intervals
  pred_summary <- pred[, .(
    mean_epred = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95)
  ),  by = c(".category", var)]

  # Define custom category labels
  category_labels <- c(`2` = "Weak", `3` = "Strong")  # Adjust as needed

  # Convert .category to a factor with readable labels
  pred_summary[, .category := factor(.category, levels = names(category_labels), labels = category_labels)]

  # Plot
  p <- ggplot(pred_summary[.category%in%category_labels], aes(x = get(var), y = mean_epred, group = .category, color = as.factor(.category))) +
    geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=1.25, width=0, alpha=1, position = position_dodge(width = 0.25))+
    geom_point(size=2.25, position = position_dodge(width = 0.25))+
    scale_color_manual(values=rel_colors[2:3]) +
    scale_fill_manual(values=rel_colors[2:3]) +
    scale_x_discrete(labels=c("Resident_Resident" = "Residents",
                              "Resident_Transient" = "Mixed",
                              "Transient_Transient" = "Transients")) +
    labs(x = xlab,
         y = "Probability of forming relationship",
         color = "Relationship",
         fill = "Relationship") +
    theme_classic() +
    theme(axis.text.x=element_text(size=14))

  p

  return(p)
}
# fit <- tar_read(mk_res_mm)
# var <- 'dyadicResidency'
# xlab <- ""




multinomial_preds_categorical <- function(fit, var, id_label) {
  
  # Extract data
  data <- data.table(fit$data)
  
  # New data for prediction
  newdata <- expand.grid(unique(data[[var]]))
  setnames(newdata, "Var1", var)
  
  # Get posterior predictions
  pred <- data.table(epred_draws(fit, newdata, re_formula=NA, ndraws = 500))
  
  # Compute mean and credible intervals
  pred_summary <- pred[, .(
    mean_epred = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95)
  ),  by = c(".category", var)]
  
  # Define custom category labels
  category_labels <- c(`1` = "Absent", `2` = "Weak", `3` = "Strong")  # Adjust as needed
  
  # Convert .category to a factor with readable labels
  pred_summary[, .category := factor(.category, levels = names(category_labels), labels = category_labels)]
  
  # Add label
  pred_summary$id <- id_label
  
  return(pred_summary)
  
}
# fit <- tar_read(mk_res_mm)
# var <- 'dyadicResidency'
# id_label <- 'MM'




plot_residency_effect <- function(fit, var, xlab) {
  
  # Calculate predictions
  p1 <- multinomial_preds_categorical(fits[[1]], var, 'mm') # important that order is retained - add test at end of function
  p2 <- multinomial_preds_categorical(fits[[2]], var, 'fm')
  p3 <- multinomial_preds_categorical(fits[[3]], var, 'ff')
  
  # Bind predictions from different models together
  d <- rbind(p1, p2, p3)
  
  # fix up sex labels
  d[id=='mm',id:='Male-Male',]
  d[id=='fm',id:='Female-Male',]
  d[id=='ff',id:='Female-Female',]
  d$id <- factor(d$id, levels=c('Male-Male','Female-Male','Female-Female'))
  
  
  # Plot
  p <- ggplot(d[.category%in%chosen_categories], aes(x = get(var), y = mean_epred, group = .category, color = as.factor(.category))) +
    facet_wrap(~.category + id, scales='free')+
    geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=1.25, width=0, alpha=1, position = position_dodge(width = 0.25))+
    geom_point(size=2.25, position = position_dodge(width = 0.25))+
    scale_color_manual(values=rel_colors[chosen_categories == levels(d$.category)]) +
    scale_fill_manual(values=rel_colors[chosen_categories == levels(d$.category)]) +
    scale_x_discrete(labels=c("Resident_Resident" = "Residents",
                              "Resident_Transient" = "Mixed",
                              "Transient_Transient" = "Transients")) +
    labs(x = xlab,
         y = "Probability of forming relationship",
         color = "Relationship",
         fill = "Relationship") +
    theme_classic() +
    theme(axis.text.x=element_text(size=14))
  
  p
  
  return(p)
}
# fits <- list(tar_read(mk_res_mm), tar_read(mk_res_fm), tar_read(mk_res_ff))
# var <- 'dyadicResidency'
# xlab <- ""
# chosen_categories <- c('Absent', 'Weak', 'Strong')




relationship_sankey_mm <- function(d) {
  
  # exclude unknowns
  d <- d[dyadicResidency%in% c('Resident_Resident','Resident_Transient','Transient_Transient'),,]
  
  # focus on males
  d <- d[dSex=='M-M']

  dd <- d %>% make_long(dyadicResidency, likely.k)
  
  ggplot(dd, aes(x = x,
                 next_x = next_x,
                 node = node,
                 next_node = next_node,
                 fill = factor(node),
                 label = node)) +
    scale_fill_manual(values=c('grey60', 'grey80','grey80', rel_colors))+
    geom_sankey(flow.alpha=0.75) +

    annotate('text', label = 'Resident-Resident', x=0.9, y=-550, hjust=1) +
    annotate('text', label = 'Resident-Transient', x=0.9, y=870, hjust=1) +
    annotate('text', label = 'Transient-Transient', x=0.9, y=1410, hjust=1) +

    annotate('text', label = 'Absent', x=2.1, y=-750, hjust=0) +
    annotate('text', label = 'Weak', x=2.1, y=620, hjust=0) +
    annotate('text', label = 'Strong', x=2.1, y=1360, hjust=0) +
    
    labs(x='', y='')+
    scale_x_discrete(labels=c('Dyadic residency','Relationship type'))+
    ggtitle('Male-Male relationships')+
    
    # geom_sankey_label() +
    theme_sankey(base_size = 16)+
    theme(legend.position = 'none')
  
}
# d <- tar_read(k_data)




relationship_sankey_ff <- function(d) {
  
  # exclude unknowns
  d <- d[dyadicResidency%in% c('Resident_Resident','Resident_Transient','Transient_Transient'),,]
  
  # focus on females
  d <- d[dSex=='F-F']
  
  dd <- d %>% make_long(dyadicResidency, likely.k)
  
  ggplot(dd, aes(x = x,
                 next_x = next_x,
                 node = node,
                 next_node = next_node,
                 fill = factor(node),
                 label = node)) +
    scale_fill_manual(values=c('grey60', 'grey80','grey80', rel_colors))+
    geom_sankey(flow.alpha=0.75) +
    
    annotate('text', label = 'Resident-Resident', x=0.9, y=-2000, hjust=1) +
    annotate('text', label = 'Resident-Transient', x=0.9, y=1400, hjust=1) +
    annotate('text', label = 'Transient-Transient', x=0.9, y=3170, hjust=1) +
    
    annotate('text', label = 'Absent', x=2.1, y=-750, hjust=0) +
    annotate('text', label = 'Weak', x=2.1, y=2500, hjust=0) +
    annotate('text', label = 'Strong', x=2.1, y=3265, hjust=0) +
    
    labs(x='', y='')+
    scale_x_discrete(labels=c('Dyadic residency','Relationship type'))+
    ggtitle('Female-Female relationships')+
    
    # geom_sankey_label() +
    theme_sankey(base_size = 16)+ # helps to turn off this to adjust labels
    theme(legend.position = 'none')
  
}
# d <- tar_read(k_data)




# Simple relatedness plot
simple_relatedness_plot <- function(relatedness_df, k_data_relatedness) {

  # remove erroneous value
  relatedness_df <- relatedness_df[wang<=0.6,,]
  k_data_relatedness <- k_data_relatedness[wang<=0.6,,]

  # relatedness and edge weights
  cor <- cor.test(relatedness_df$wang, relatedness_df$edge)
  g1 <- ggplot(relatedness_df, aes(x=wang, y=edge)) +
    geom_point(size=2, alpha=0.5) +
    geom_smooth(method='lm') +
    labs(x='Wang relatedness', y='Mean edge weight')+
    annotate('text', label = paste('Correlation = ', round(cor$estimate,3), sep=''), x=-0.5, y=0.15) +
    theme_classic()
  # g1

  # relatedness and relationship categories
  k_data_relatedness[likely.k>2, likely.k:=3,] # simplify stronger relationship types for visualization
  g2 <- ggplot(k_data_relatedness, aes(x=as.factor(likely.k), y=wang))+
    geom_jitter(width = 0.15, alpha=1)+
    geom_boxplot(alpha=0.75, size=1) +
    scale_x_discrete(labels=c('Rarely interact (k=1)', 'Weak relationship (k=2)', 'Strong relationship (k>2)'))+
    labs(x='', y='Wang relatedness') +
    theme_classic() +
    theme(axis.text.x = element_text(size=10))
  # g2

  g1 | g2

}
# relatedness_df <- tar_read(relatedness_df)
# k_data_relatedness <- tar_read(k_data_relatedness)




# Extract network traits --------------------------------------------------
extract_global_trait <- function(fit) {

  # extract all edge samples
  all_edge_samples <- data.frame(coef(fit, summary=FALSE)[[1]])
  colnames(all_edge_samples) <- str_extract(colnames(all_edge_samples), "\\d+_\\d+")

  # extract year group and proper dyad names
  d <- data.table(fit$data)

  # number of draws
  N_draws <- 100

  # can add restrictions here later
  edge_samples <- all_edge_samples

  # create empty matrix to store metric samples
  metric_samples <- matrix(0, N_draws)

  # randomly select draws to use in advance
  chosen_draws <- sample(1:nrow(edge_samples), size = N_draws, replace = FALSE)

  # loop through draws of network
  for (i in 1:N_draws) {

    # select draw
    draw <- chosen_draws[i]

    # pull draw of complete network (i.e., of each edge weight)
    draw_of_network <- as.data.frame(t(edge_samples[draw,])) # probably more efficient way to do this (i.e., full matrix...)
    # if (nrow(draw_of_network)==1) (rownames(draw_of_network) <- colnames(edge_samples)) # for 1991 with 2 photos... can simply delete this year too maybe
    colnames(draw_of_network) <- 'weight_sample_raw'

    # format IDs out of dyad label
    draw_of_network$A <- sub("^(\\d+)_.*", "\\1", rownames(draw_of_network)) # pull out ID A
    draw_of_network$B <- sub("^\\d+_(\\d+)$", "\\1", rownames(draw_of_network)) # pull out ID B

    # create edgelist
    net <- igraph::graph_from_edgelist(as.matrix(draw_of_network[,c('A','B'),]), directed=FALSE)

    # add in edgeweights
    igraph::E(net)$weight <- inv_logit(draw_of_network[,c('weight_sample_raw'),]) # inv-logit here because it's a bernoulli model # confirm makes sense to transform prior to strength calculation

    # for modularity, likely want to incorporate threshold? Or are modularity measures often weighted? At least try both ways
    wtc <- cluster_louvain(net)
    mod <- modularity(net, membership(wtc))

    # add to accumulating dataframe
    metric_samples[i] <- mod

  }

  return(metric_samples)

}
# fit <- tar_read(brms_fit)
# m <- extract_global_trait(fit = tar_read(brms_fit))




# Carve identifications into groups ---------------------------------------
carve_groups <- function(photoData, meta) {

  photoData <- merge(photoData, meta[,c('Title','year','ageClass'),], by=c('Title','year'), all.x=TRUE) ## need to think on whether this is side-specific and so on? maybe other issues...
  #adults_only <- photoData[ageClass=='Adult',,]

  # sort data table
  setorder(photoData, dateTime)

  # Calculate the time difference in minutes
  photoData[, time_diff := c(0, diff(dateTime) / 60)]

  # Create a group identifier based on the threshold
  photoData[, myGroups := cumsum(time_diff > 10) + 1]

  # Summary statistics - group size and duration
  photoData[,groupSize:=as.numeric(length(unique(Title))),by=myGroups]
  photoData[, minTime:=min(dateTime), by=myGroups]
  photoData[, maxTime:=max(dateTime), by=myGroups]
  photoData[, groupDur:=as.numeric(maxTime-minTime, units='mins'), by=myGroups] # in minutes
  # extract unique
  u_groups <- unique(photoData[,c('myGroups','groupSize', 'groupDur')])
  # group size
  hist(u_groups$groupSize)
  range(u_groups$groupSize) # range 1-27
  median(u_groups$groupSize) # median 2
  mean(u_groups$groupSize) # mean 2.4
  # group duration
  hist(u_groups$groupDur)
  range(u_groups$groupDur) # range 0-74 mins
  median(u_groups$groupDur) # median 1 min
  mean(u_groups$groupDur) # mean 4.4 mins

  # Extract median dateTime of group
  photoData[,groupTime:=mean(dateTime), by=myGroups]

  # subset to one row per group per individual
  group_ind_data <- unique(photoData[,c('Title','year', 'groupSize','myGroups', 'groupTime')])

  group_ind_data[,gs:=groupSize,] # group size for each individual

  group_df <- unique(group_ind_data[,c('Title','myGroups','year','gs', 'groupTime')])

  return(group_df)

}
# photoData <- tar_read(side)
# meta <- tar_read(nbw_meta)




# Version for reliable data which returns NA if fewer than 2 individuals in a given year
# Calculate associations based on group observations ----------------------
build_group_associations_2 <- function(g) {

  # convert to data.table for easier processing
  g <- data.table(g)

  # unique individuals
  individuals <- g[,unique(Title),]

  if (length(individuals)<2) (return(NA))

  # generate all possible dyads
  possible_dyads <- data.table(t(combn(individuals, 2)))
  colnames(possible_dyads) <- c('A','B')

  # get unique group IDs
  group_ids <- unique(g$myGroups)

  # clear output
  output <- NULL

  # loop through and build dataframe of associations
  for (i in unique(group_ids)) {

    # individuals present in given group
    seen_individuals <- g[myGroups==i,unique(Title),]

    # possible dyads FOR each group (for computational efficiency, only including dyads where at least one individual is present)
    local_dyads <- possible_dyads[(A %in% seen_individuals) | (B %in% seen_individuals),, ]

    # add group ID
    local_dyads[,group_ID:=i,]

    # check whether A and B are present and calculate associations ('together')
    local_dyads[,A_present:=ifelse(A %in% seen_individuals,1,0),by=.I]
    local_dyads[,B_present:=ifelse(B %in% seen_individuals,1,0),by=.I]
    local_dyads[,together:=ifelse(A_present==1 & B_present==1,1,0),by=.I]

    # add dyad name
    local_dyads[,dyad:=paste(pmin(A,B),pmax(A,B),sep='_'),by=c('A','B')]

    # build up output iteratively
    if (exists('output')) (output <- rbindlist(list(output, local_dyads)))
    if (!(exists('output'))) (output <- local_dyads)

  }

  return(output)

}




# Calculate associations based on group observations ----------------------
build_group_associations <- function(g) {

  # convert to data.table for easier processing
  g <- data.table(g)

  # unique individuals
  individuals <- g[,unique(Title),]

  # generate all possible dyads
  possible_dyads <- data.table(t(combn(individuals, 2)))
  colnames(possible_dyads) <- c('A','B')

  # get unique group IDs
  group_ids <- unique(g$myGroups)

  # clear output
  output <- NULL

  # loop through and build dataframe of associations
  for (i in unique(group_ids)) {

    # individuals present in given group
    seen_individuals <- g[myGroups==i,unique(Title),]

    # possible dyads FOR each group (for computational efficiency, only including dyads where at least one individual is present)
    local_dyads <- possible_dyads[(A %in% seen_individuals) | (B %in% seen_individuals),, ]

    # add group ID
    local_dyads[,group_ID:=i,]

    # check whether A and B are present and calculate associations ('together')
    local_dyads[,A_present:=ifelse(A %in% seen_individuals,1,0),by=.I]
    local_dyads[,B_present:=ifelse(B %in% seen_individuals,1,0),by=.I]
    local_dyads[,together:=ifelse(A_present==1 & B_present==1,1,0),by=.I]

    # add dyad name
    local_dyads[,dyad:=paste(pmin(A,B),pmax(A,B),sep='_'),by=c('A','B')]

    # build up output iteratively
    if (exists('output')) (output <- rbindlist(list(output, local_dyads)))
    if (!(exists('output'))) (output <- local_dyads)

  }

  return(output)

}
# g <- tar_read(group_df_all_old)
# g <- tar_read(group_df_all_grouped_reliable)



# Combine group associations for summarizing ------------------------------
aggregate_group_associations <- function(g) {

  g[, sum_together:=sum(together), by=c('year_group','dyad')]
  g[, sum_opps:=.N, by=c('year_group','dyad')]

  aggregated <- unique(g[, c('A','B','sum_together','sum_opps','dyad', 'dyad_annual','year_group')])
  aggregated[,together:=sum_together,]

  return(aggregated)

}
# g <- tar_read(group_associations_all)



# Create key to link year_groups with years  ------------------------------
year_key <- function(g) {

  key <- data.table(unique(g[,c('tar_group','year'),]))
  colnames(key) <- c('year_group','year')
  key <- key[order(key$year)]

}
# g <- tar_read(group_df_all_grouped)




##################################################
# Network model with partial pooling across years
# [But year-specific dyads are kept separate]
unlist_group_associations <- function(group_associations) {

  # loop through list to extract and combine annual associations
  for (i in 1:length(group_associations)) {

    temp <- group_associations[[i]]
    temp$year_group <- i

    if (exists('output')) output <- rbindlist(list(output, temp))
    if (!exists('output')) output <- temp

  }

  # add year-specific dyad IDs
  output[,dyad_annual:=paste(dyad, year_group, sep='-')]

  return(output)


}
# group_associations <- tar_read(group_associations)




# Extract network traits --------------------------------------------------
extract_global_trait_ma <- function(fit, meta, chosen_dyadic_sexes) {

  # extract all edge samples
  all_edge_samples <- data.frame(coef(fit, summary=FALSE)[[1]])
  colnames(all_edge_samples) <- str_extract(colnames(all_edge_samples), "\\d+_\\d+\\.\\d+")

  # # extract year group and proper dyad names
  d <- data.table(fit$data)
  d[,year_group:=str_extract(dyad_annual, "(?<=-)[0-9]+$"), by=.I]
  d[,dyad:=str_extract(dyad_annual, "^\\d+_\\d+"), by=.I]

  # year groups to loop through
  years <- unique(d$year_group)
  years <- years[!years %in% c(4, 18)]  # exclude two years with just one dyad: 1991, 2009

  # number of draws
  N_draws <- 100

  # randomly select draws to use in advance
  chosen_draws <- sample(1:nrow(all_edge_samples), size = N_draws, replace = FALSE)

  # loop through years
  for (y in years) {

    print(y)

    # restrict to year of interest
    edge_samples <- all_edge_samples[,(str_extract(colnames(all_edge_samples), "(?<=\\.)\\d+")==y)]
    colnames(edge_samples) <- str_remove(colnames(edge_samples), "\\.\\d+")

    # add restrictions based on age or sex (optional)
    dyads <- colnames(edge_samples)
    dyadic_sex_classes <- unlist(lapply(dyads, function(i) {
      generate_dyadic_sex(sub("^(\\d+)_.*", "\\1",i), sub("^\\d+_(\\d+)$", "\\1", i), meta)}))
    edge_samples <- edge_samples[,which(dyadic_sex_classes %in% chosen_dyadic_sexes)]

    # skip if just one relationship
    if (is.null(ncol(edge_samples))) (next)

    # create empty matrix to store metric samples
    metric_samples <- matrix(0, N_draws)

    # loop through draws of network
    for (i in 1:N_draws) {

      # select draw
      draw <- chosen_draws[i]

      # pull draw of complete network (i.e., of each edge weight)
      draw_of_network <- as.data.frame(t(edge_samples[draw,])) # probably more efficient way to do this (i.e., full matrix...)
      # if (nrow(draw_of_network)==1) (rownames(draw_of_network) <- colnames(edge_samples)) # for 1991 with 2 photos... can simply delete this year too maybe
      colnames(draw_of_network) <- 'weight_sample_raw'

      # format IDs out of dyad label
      draw_of_network$A <- sub("^(\\d+)_.*", "\\1", rownames(draw_of_network)) # pull out ID A
      draw_of_network$B <- sub("^\\d+_(\\d+)$", "\\1", rownames(draw_of_network)) # pull out ID B

      # create edgelist
      net <- igraph::graph_from_edgelist(as.matrix(draw_of_network[,c('A','B'),]), directed=FALSE)

      # add in edgeweights
      igraph::E(net)$weight <- inv_logit(draw_of_network[,c('weight_sample_raw'),]) # inv-logit here because it's a bernoulli model # confirm makes sense to transform prior to strength calculation

      # for modularity, likely want to incorporate threshold? Or are modularity measures often weighted? At least try both ways
      wtc <- cluster_louvain(net) # this gives modularity value, but sometimes 2 values? So using modularity function below instead
      mod <- modularity(net, membership(wtc), weights=E(net)$weight)

      # add to accumulating dataframe
      metric_samples[i,] <- mod

    }

    metric_df <- data.table(samples=metric_samples[,1], year_group=y)

    # build up output iteratively
    if (exists('output')) (output <- rbindlist(list(output, metric_df)))
    if (!exists('output')) (output <- metric_df)

  }

  return(output)

}
# fit <- tar_read(brms_fit_group_multiAnnual)
# meta <- tar_read(nbw_meta_raw)
# chosen_dyadic_sexes <- c('Male_Male')
# chosen_dyadic_sexes <- c('Female-Juvenile_Female-Juvenile')




generate_dyadic_sex <- function(A, B, meta) {

  sexA <- meta[Title==A, as.character(unique(sex)),]
  sexB <- meta[Title==B, as.character(unique(sex)),]

  if (is.na(sexA)) (sexA <- 'Unk')
  if (is.na(sexB)) (sexB <- 'Unk')

  dyadicSex <- paste(sort(c(sexA,sexB))[1], sort(c(sexA,sexB))[2], sep="_")

  return(dyadicSex)

}
# A <- '45'
# B <- '54'
# meta <- tar_read(nbw_meta_raw)


# Note: function will skip years where model fit is inadequate for interpretation
annual_S_and_R <- function(associations) {

  output <- NULL

  for (i in 1:length(associations)) {

    # pull out annual associations
    a <- associations[[i]]

    # pull out numerator and denominator of associations
    a[,numerator:=sum(together),by=dyad]
    a[,denominator:=.N,by=dyad]

    # one row per dyad
    a <- unique(a[,c('dyad','numerator','denominator')])

    # setup skip in case of fitting error
    skip_to_next <- FALSE

    # run social differentiation model
    tryCatch(data.frame(social_differentiation(a$numerator, a$denominator)), error = function(e) {skip_to_next <<- TRUE})

    # temporary results
    results <- NULL # clear previous version
    if (skip_to_next==FALSE) (sfit <- data.frame(social_differentiation(a$numerator, a$denominator)))
    if (skip_to_next==FALSE) (results <- data.frame(S = sfit["Social Differentiation","Estimate"],
                                                    S_se = sfit["Social Differentiation","SE"],
                                                    R = sfit["Correlation","Estimate"],
                                                    R_se = sfit["Correlation","SE"],
                                                    year_group = i))

    # save results
    if (exists('output')) (output <- rbindlist(list(output, results)))
    if (!exists('output')) (output <- results)

  }
  
  # Based on visual assessment, additional years with low data and poor fit for S and/or no value for R
  output <- output[!is.na(R),,]
  output <- output[!(S<0.01),,]
  
  
  # removing these to make annual mean more robust

  return(output)

  # If you want to visualize
  # gs <- ggplot(output, aes(x=year_group, y=S)) +
  # 
  #  geom_point(color='steelblue') +
  #  geom_errorbar(aes(ymin=S-S_se, ymax=S+S_se), color='steelblue') +
  #  ylim(0,4)+
  #  theme_classic()
  # 
  # gr <-ggplot(output, aes(x=year_group, y=R)) +
  # 
  # geom_point(color='red') +
  # geom_errorbar(aes(ymin=R-R_se, ymax=R+R_se), color='red') +
  # ylim(0,1.25)+
  # theme_classic()
  # 
  # gs | gr

}
# associations <- tar_read(group_associations)



combine_sex_modularity <- function(mod_mm, mod_ff) {

  mod_mm[,mod_mean:=mean(samples, na.rm=TRUE), by='year_group']
  mod_mm[,mod_sd:=sd(samples, na.rm=TRUE), by='year_group']
  m <- unique(mod_mm[,c('mod_mean','mod_sd', 'year_group')])
  m[,sex:='Male',]

  mod_ff[,mod_mean:=mean(samples, na.rm=TRUE), by='year_group']
  mod_ff[,mod_sd:=sd(samples, na.rm=TRUE), by='year_group']
  f <- unique(mod_ff[,c('mod_mean','mod_sd', 'year_group')])
  f[,sex:='Female',]

  c <- rbindlist(list(m, f))
  return(c)

}
# mod_mm <- tar_read(modularity_mm)
# mod_ff <- tar_read(modularity_ff)



# Pull out edges
edge_list_ma <- function(fit, include_zeros) {

  # extract all edge samples
  all_edge_samples <- data.frame(coef(fit, summary=FALSE)[[1]]) 
  colnames(all_edge_samples) <- str_extract(colnames(all_edge_samples), "\\d+_\\d+\\.\\d+")

  # # extract year group and proper dyad names
  d <- data.table(fit$data)
  d[,year_group:=str_extract(dyad_annual, "(?<=-)[0-9]+$"), by=.I]
  d[,dyad:=str_extract(dyad_annual, "^\\d+_\\d+"), by=.I]

  # year groups to loop through
  years <- unique(d$year_group)
  years <- years[!years %in% c(4, 18)] # exclude two years with just one dyad: 1991, 2009

  # number of draws
  N_draws <- 100

  # randomly select draws to use in advance
  chosen_draws <- sample(1:nrow(all_edge_samples), size = N_draws, replace = FALSE)

  # loop through years
  for (y in years) {

    print(y)

    # identify non-zero dyads
    y_data <- d[year_group==y,,]
    y_data[,nAssociations:=sum(together),by=dyad]
    nz_dyads <- y_data[nAssociations>=1,unique(dyad),]

    # restrict to year of interest
    edge_samples <- all_edge_samples[,(str_extract(colnames(all_edge_samples), "(?<=\\.)\\d+")==y)]
    colnames(edge_samples) <- str_remove(colnames(edge_samples), "\\.\\d+")

    # apply transformation to all samples
    edge_samples <- data.frame(lapply(edge_samples, inv_logit), check.names = FALSE)

    # extract edge list
    edges <- data.table(data.frame(dyad=colnames(edge_samples),
                        edge=colMeans(edge_samples),
                        edge_sd=apply(edge_samples, 2, sd)))

    # add year_group to edge
    edges$year_group <- y

    # exclude non-zero dyads if necessary
    if (!include_zeros) (edges <- edges[dyad %in% nz_dyads,,])

    # build up output iteratively
    if (exists('output')) (output <- rbindlist(list(output, edges)))
    if (!exists('output')) (output <- edges)

  }

  return(output)

}
# fit <- tar_read(brms_fit_group_multiAnnual)
# include_zeros <- FALSE




plot_network <- function(edge_list, threshold, lwd, meta) {

  # Note: partially adapted from BisonR plotting function
  # https://github.com/JHart96/bisonR/blob/main/R/bison_model.R

  # choose colors
  cols <- viridis(2, option=proj_color, begin=0, end=0.5)

  #pull out A and B
  edge_list$A <- sub("^(\\d+)_.*", "\\1", edge_list$dyad) # pull out ID A
  edge_list$B <- sub("^\\d+_(\\d+)$", "\\1", edge_list$dyad) # pull out ID B

  # threshold to not show very weak relationships
  plot_edges <- edge_list[edge>=threshold,,]

  # make net
  net <- igraph::graph_from_edgelist(as.matrix(plot_edges[,c('A','B'),]), directed=FALSE)

  # add node visualshttp://127.0.0.1:39619/graphics/f13feed5-bff0-497b-850d-92c4bc0483a4.png
  meta_sex <- unique(meta[,c('Title','sex'),])
  sex_vector <- setNames(meta_sex$sex, meta_sex$Title)
  node_sex <- sex_vector[names(V(net))]
  V(net)$color <- ifelse(is.na(node_sex), adjustcolor("grey", alpha.f = .5),
                  ifelse(node_sex == "Male", adjustcolor(cols[1], alpha.f = .75),
                         ifelse(node_sex == "Female-Juvenile", adjustcolor(cols[2], alpha.f = .75), "grey")))

  # add weights adjust edge visuals
  E(net)$weight <- plot_edges$edge
  coords <- layout_with_fr(net)
  par(pty = "s")  # Ensures a square plotting region
  par(mar=c(0,0,0,0))
  igraph::plot.igraph(net, edge.width=plot_edges$edge_sd * lwd, layout=coords, vertex.size=12, vertex.label.family='Helvetica',vertex.label.color="white",vertex.label.cex=0.25, edge.color=rgb(0, 0, 0, 0.2), edge.arrow.size=0)
  igraph::plot.igraph(net, edge.width=plot_edges$edge * lwd/5, layout=coords, vertex.size=12, vertex.label.family='Helvetica', vertex.label.color="white", vertex.label.cex=0.25, vertex.label.cex=0.5, edge.color=rgb(0, 0, 0, 0.75), edge.arrow.size=0, add=TRUE)

}
# edge_list <- tar_read(edges_ma)[year_group==26]
# threshold <- 0.02
# lwd <- 100
# meta <- tar_read(nbw_meta_raw)




# compare distance between models based on BIC
compare_BIC_plot <- function(fits, label) {

  # pull out summary
  fit_sum <- data.frame(fits$summary)

  # calculate best fit based on BIC
  min_BIC <- min(fit_sum$BIC)

  # plot BIC across models: similar support for "best" and k=3?
  ggplot(fit_sum, aes(x=K.out, y=BIC)) +
    geom_line(linewidth=1, alpha=0.5) +
    geom_point(aes(color = BIC==min_BIC), size=4) +
    labs(y='Bayesian information criterion', x='Number of relationship categories')+
    scale_color_viridis(discrete=TRUE, option=proj_color, begin = 0, end = 0.5) +
    ggtitle(label)+
    theme_classic()

}
# fits <- tar_read(ff_mixtures)
# label <- 'F-F'



print('Cleared functions')
