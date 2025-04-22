
library(targets)

# Source
tar_source('R') # will do all in 'R' folder

# Seed
tar_option_set(seed = 1234)

# Configuration - reduce uploads to cloud for improved efficiency 
# tar_config_set(seconds_meta_append = 15,
#                seconds_meta_upload = 15,
#                seconds_reporter = 0.5)

# Variables
# suppressMessages(set_cmdstan_path(path='C:/Users/sjfwa/AppData/Local/R/cmdstan-2.33.1')) # cmdstan path for local machine
# cmdstanr::set_cmdstan_path('/home/sjfw/.cmdstan/cmdstan-2.36.0') # path for supercomputer


# ############################
# Parallel computing - Canada Supercomputer (Comment out to run locally)
# options(clustermq.scheduler = 'slurm', clustermq.template='SLURM.tmpl')
# # Set targets options with SLURM resources
# tar_option_set(
#   resources = tar_resources(
#     clustermq = tar_resources_clustermq(
#       template = list(
#         cores = 4,            # Number of cores per task
#         memory = 8192,        # Memory in MB (e.g., 8192 for 8 GB)
#         time = '7-00:00:00'   # Time in D-HH:MM:SS
#       )
#     )
#   )
# )

# Set color theme
proj_color <- 'plasma'
rel_colors <- viridis(3, option=proj_color, begin=1, end=0)


# Targets
list( 
  
  #######################################################################
  ## Steps for public data

  # Comment these lines out if running from raw data

  # tar_target(k_data, read.csv('./Public-Data/k_data.csv')),
  # tar_target(k_data_relatedness, read.csv('./Public-Data/k_data_relatedness.csv')),
  # tar_target(k_data_unks_included, read.csv('./Public-Data/k_data.csv')),
  # 
  # tar_target(k_data_mm, read.csv('./Public-Data/k_data_mm.csv')),
  # tar_target(k_data_ff, read.csv('./Public-Data/k_data_ff.csv')),
  # tar_target(k_data_fm, read.csv('./Public-Data/k_data_fm.csv')),
  
  
  # #######################################################################
  # ## Read in data
  # tar_target(raw_photoData, read_data_excel('./input/LV_SS_Master_1988-2023_excel.xlsx')),
  # 
  # # process photo data
  # tar_target(side, process_photoID(raw_photoData, 'Left', 'Gully')), # only left-sided photos from the Gully
  # 
  # # build metadata for individual bottlenose whales
  # tar_target(nbw_meta_raw, build_nbw_metadata(raw_photoData, 'left')),
  # 
  # 
  # #######################################################################
  # ## Run genetic algorithms
  # tar_target(relatedness, calculate_relatedness()),
  # tar_target(relatedness_df, merge(edges_df_group, relatedness[,c('wang','wang.low','wang.high','dyad'),])),
  # 
  # 
  # #######################################################################
  # ## Define groups
  # 
  # # Defining groups
  # tar_target(groups, carve_groups(side, nbw_meta_raw)),
  # tar_target(group_df_all, merge(groups, nbw_meta_raw, by=c('Title','year'),all.x=TRUE)),
  # tar_target(group_df_all_old, group_df_all[ageClass=='Adult' | minimumAge>=4,,]),
  # 
  # 
  # #######################################################################
  # ## Estimate main social network (pooling all years)
  # 
  # # Build associations across years
  # tar_target(group_associations_no_demo_restriction, build_group_associations(group_df_all_old)),
  # tar_target(group_associations_no_demo_restriction_day, merge(group_associations_no_demo_restriction, unique(group_df_all_old[,.(group_ID=myGroups, day=as.Date(groupTime))]), by='group_ID', all.X=TRUE)),
  # tar_target(group_associations_demo, overlappingIDs(group_associations_no_demo_restriction_day, nbw_meta_raw)),
  # 
  # # run edge weight models for full left-sided dataset
  # tar_target(brms_fit_group, brm(
  #   formula = together ~ (1|dyad),
  #   data = group_associations_demo,
  #   family = bernoulli(),
  #   prior = c(
  #     set_prior("normal(-1.5, 1)", class = "Intercept"),
  #     set_prior("normal(0, 1)", class = "sd", group = "dyad", coef = "Intercept")),
  #   chains=4,
  #   cores=4)),
  # 
  # # extract edges
  # tar_target(edges_group, edge_list(brms_fit_group, include_zeros=TRUE)),
  # tar_target(edges_group_nz, edge_list(brms_fit_group, include_zeros=FALSE)),
  # 
  # # add metadata: sex, age differences, residency combinations
  # tar_target(edges_sex_group, edge_sex(edges_group, nbw_meta_raw)),
  # tar_target(edges_age_group, edge_age_diff(edges_sex_group, nbw_meta_raw)),
  # tar_target(edges_df_group, edge_residency(edges_age_group, nbw_meta_raw)),
  # 
  # # add metadata: sex, age differences, residency combinations
  # tar_target(nz_edges_sex_group, edge_sex(edges_group_nz, nbw_meta_raw)),
  # tar_target(nz_edges_age_group, edge_age_diff(nz_edges_sex_group, nbw_meta_raw)),
  # tar_target(nz_edges_df_group, edge_residency(nz_edges_age_group, nbw_meta_raw)),
  # 
  # # calculate maximum edge weight for each individual
  # tar_target(max_edges, max_edge(edges_df_group)),
  # tar_target(max_edges_df, merge(max_edges, nbw_meta_raw[,c('Title','sex')], by='Title')),
  # 
  # 
  # #######################################################################
  # ## Estimate annual social networks
  # 
  # # Use tar_group to split dataframe by year
  # tar_group_by(group_df_all_grouped, group_df_all_old, year),
  # tar_target(year_key, year_key(group_df_all_grouped)), # track year-group IDs
  # 
  # # Calculate social associations within each year-specific dataframe, then combine the results back together
  # tar_target(group_associations, build_group_associations(group_df_all_grouped), pattern=map(group_df_all_grouped), iteration="list"),
  # tar_target(group_associations_all, unlist_group_associations(group_associations)),
  # tar_target(group_associations_all_aggregated, aggregate_group_associations(group_associations_all)), # aggregated version just useful for summaries
  # 
  # # Run edge weight models using multi-annual network structure
  # tar_target(brms_fit_group_multiAnnual, brm(
  #   formula = together ~ (1|dyad_annual),
  #   data = group_associations_all,
  #   family = bernoulli(),
  #   prior = c(
  #     set_prior("normal(-1.5, 1.5)", class = "Intercept"),
  #     set_prior("normal(0, 1)", class = "sd", group = "dyad_annual", coef = "Intercept")),
  #   chains=4,
  #   cores=4,
  #   control = list(adapt_delta = 0.95))),
  # 
  # # extract edges
  # tar_target(edges_ma, edge_list_ma(brms_fit_group_multiAnnual, include_zeros=TRUE)),
  # tar_target(edges_ma_nz, edge_list_ma(brms_fit_group_multiAnnual, include_zeros=FALSE)),
  # 
  # # add metadata: sex, age differences, residency combinations
  # tar_target(edges_sex_ma, edge_sex(edges_ma, nbw_meta_raw)),
  # tar_target(edges_age_ma, edge_age_diff(edges_sex_ma, nbw_meta_raw)),
  # tar_target(edges_df_ma, edge_residency(edges_age_ma, nbw_meta_raw)),
  # 
  # # add metadata: sex, age differences, residency combinations
  # tar_target(nz_edges_sex_ma, edge_sex(edges_ma_nz, nbw_meta_raw)),
  # tar_target(nz_edges_age_ma, edge_age_diff(nz_edges_sex_ma, nbw_meta_raw)),
  # tar_target(nz_edges_df_ma, edge_residency(nz_edges_age_ma, nbw_meta_raw)),
  # 
  # 
  # 
  # #######################################################################
  # ## Calculate global network properties
  # 
  # # Calculate social differentiation and correlation between true and estimated association indices
  # tar_target(SR, annual_S_and_R(group_associations)),
  # 
  # # Calculate modularity (no indication of biased estimates from different years)
  # tar_target(modularity, extract_global_trait_ma(brms_fit_group_multiAnnual, nbw_meta_raw, c('Female-Juvenile_Female-Juvenile',
  #                                                                                            'Female-Juvenile_Male',
  #                                                                                            'Female-Juvenile_Unk',
  #                                                                                            'Male_Male',
  #                                                                                            'Male_Unk',
  #                                                                                            'Unk_Unk'))),
  # 
  # 
  # #######################################################################
  # ## Fit mixture models
  # 
  # # Fit mixture model with all individuals (including unknown sex)
  # tar_target(all_mixtures, fit_mixture_model_all(group_associations_demo, nbw_meta_raw, 3, 'BIC')),
  # 
  # # Fit mixture models for different dyadic sex compositions (will take a little while to run)
  # tar_target(ff_mixtures, fit_mixture_models(group_associations_demo, 'F-F', nbw_meta_raw, 5, 'BIC')),
  # tar_target(fm_mixtures, fit_mixture_models(group_associations_demo, 'F-M', nbw_meta_raw, 5, 'BIC')),
  # tar_target(mm_mixtures, fit_mixture_models(group_associations_demo, 'M-M', nbw_meta_raw, 5, 'BIC')),
  # 
  # # Extract "best" model based on BIC
  # tar_target(ff_mixtures_best, get_bestmodel(ff_mixtures)),
  # tar_target(fm_mixtures_best, get_bestmodel(fm_mixtures)),
  # tar_target(mm_mixtures_best, get_bestmodel(mm_mixtures)),
  # 
  # # Extract model based on 3 categories
  # tar_target(ff_mixtures_3, ff_mixtures$all.models[[3]]),
  # tar_target(fm_mixtures_3, fm_mixtures$all.models[[3]]),
  # tar_target(mm_mixtures_3, mm_mixtures$all.models[[3]]),
  # 
  # 
  # 
  # #######################################################################
  # ## Dyadic regressions
  # 
  # # Prepare data from categorical classifications for multinomial models
  # 
  # # For model with unknown-sex individuals
  # tar_target(k_data_unks_included, merge(edges_df_group, unique(data.table(by_edge(all_mixtures$all.models[[3]]))), by.x='dyad',by.y='edge.id')),
  # 
  # tar_target(k_data_mm, merge(edges_df_group, unique(data.table(by_edge(mm_mixtures$all.models[[3]]))), by.x='dyad',by.y='edge.id')),
  # tar_target(k_data_fm, merge(edges_df_group, unique(data.table(by_edge(fm_mixtures$all.models[[3]]))), by.x='dyad',by.y='edge.id')),
  # tar_target(k_data_ff, merge(edges_df_group, unique(data.table(by_edge(ff_mixtures$all.models[[3]]))), by.x='dyad',by.y='edge.id')),
  # 
  # tar_target(k_data, rbindlist(list(k_data_mm, k_data_fm, k_data_ff), idcol = TRUE, fill = TRUE)),
  # tar_target(k_data_relatedness, merge(k_data, relatedness[,c('wang','wang.low','wang.high','dyad'),], by='dyad')),
  # tar_target(k_data_relatedness_unks_included, merge(k_data_unks_included, relatedness[,c('wang','wang.low','wang.high','dyad'),], by='dyad')),
  # 
  # 
  # # Relatedness models
  # 
  # tar_target(mk_rel_all, brm(
  #   formula = likely.k ~ wang + (1| mm(A, B)),
  #   data = k_data_relatedness_unks_included[,,],
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  #   control = list(adapt_delta = 0.99))),
  # 
  # tar_target(mk_rel_mm, brm(
  #   formula = likely.k ~ wang + (1| mm(A, B)),
  #   data = k_data_relatedness[.id==1,,],
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  #   control = list(adapt_delta = 0.95))),
  # 
  # tar_target(mk_rel_fm, brm(
  #   formula = likely.k ~ wang + (1| mm(A, B)),
  #   data = k_data_relatedness[.id==2,,],
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  #   control = list(adapt_delta = 0.99))),
  # 
  # tar_target(mk_rel_ff, brm(
  #   formula = likely.k ~ wang + (1| mm(A, B)),
  #   data = k_data_relatedness[.id==3,,],
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  #   control = list(adapt_delta = 0.95))),
  # 
  # 
  # # Age difference models
  # 
  # tar_target(mk_Age_all, brm(
  #   formula = likely.k ~ ageDiff + (1| mm(A, B)),
  #   data = k_data_unks_included,
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  # )),
  # 
  # tar_target(mk_Age_mm, brm(
  #   formula = likely.k ~ ageDiff + (1| mm(A, B)),
  #   data = k_data_mm,
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  # )),
  # 
  # tar_target(mk_Age_fm, brm(
  #   formula = likely.k ~ ageDiff + (1| mm(A, B)),
  #   data = k_data_fm,
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  # )),
  # 
  # tar_target(mk_Age_ff, brm(
  #   formula = likely.k ~ ageDiff + (1| mm(A, B)),
  #   data = k_data_ff,
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  # )),
  # 
  # 
  # # Residency models
  # 
  # tar_target(mk_res_all, brm(
  #   formula = likely.k ~ dyadicResidency + (1| mm(A, B)),
  #   data = k_data_unks_included[dRes %in% c('R-R','R-T','T-T'),,],
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  # )),
  # 
  # tar_target(mk_res_mm, brm(
  #   formula = likely.k ~ dyadicResidency + (1| mm(A, B)),
  #   data = k_data_mm[dRes %in% c('R-R','R-T','T-T'),,],
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  # )),
  # 
  # tar_target(mk_res_fm, brm(
  #   formula = likely.k ~ dyadicResidency + (1| mm(A, B)),
  #   data = k_data_fm[dRes %in% c('R-R','R-T','T-T'),,],
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  # )),
  # 
  # tar_target(mk_res_ff, brm(
  #   formula = likely.k ~ dyadicResidency + (1| mm(A, B)),
  #   data = k_data_ff[dRes %in% c('R-R','R-T','T-T'),,],
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  # )),
  # 
  # 
  # #######################################################################
  # ## SOCPROG
  # 
  # # write data to use in SOCPROG
  # tar_target(socprog_input, write_xlsx(prep_SOCPROG_input(group_df_all_old, reliable_only=FALSE), './SOCPROG/SP_input/input_for_SOCPROG.xlsx')),
  # tar_target(socprog_input_reliable, write_xlsx(prep_SOCPROG_input(group_df_all_old, reliable_only=TRUE), './SOCPROG/SP_input/input_for_SOCPROG_reliable.xlsx')),
  # # secondary metadata for SOCPROG
  # tar_target(socprog_input_secondary, write_xlsx(prep_SOCPROG_supplemental(nbw_meta_raw), './SOCPROG/SP_input/supplemental_input_for_SOCPROG.xlsx')),
  # 
  # 
  # #######################################################################
  # ## Figures
  # 
  # tar_target(Figure1, save_figure('./Manuscript/Figures/Figure1.png',w=4,h=4,
  #                                 plot_network(edges_ma[year_group==27], threshold=0.02, lwd=100, nbw_meta_raw))),
  # 
  # tar_target(Figure2, save_figure('./Manuscript/Figures/Figure2.png',w=10,h=3,
  #                                 (socMod_plot_edit(ff_mixtures,'Female-Female', 3) | socMod_plot_edit(fm_mixtures,'Female-Male', 3) | socMod_plot_edit(mm_mixtures,'Male-Male', 3)))),
  # 
  # tar_target(Figure3, save_figure('./Manuscript/Figures/Figure3.png',w=20,h=4,
  #                                 plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Standard-Adult', 'all_adult', 'All relationships') |
  #                                   plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Standard-Adult', 'mm_adult', 'Male-Male') |
  #                                   plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Standard-Adult', 'ff_adult', 'Female-Female') |
  #                                   plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Standard-Adult', 'mf_adult', 'Male to Female') |
  #                                   plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Standard-Adult', 'fm_adult', 'Female to Male'))),
  # 
  # tar_target(Figure4, save_figure('./Manuscript/Figures/Figure4.png',w=3,h=3,
  #                                      plot_age_effect(list(mk_Age_mm, mk_Age_fm, mk_Age_ff),
  #                                                      var = 'ageDiff',
  #                                                      xlab = 'Difference in minimum age',
  #                                                      chosen_categories = 'Strong'))),
  # 
  # tar_target(Figure5, save_figure('./Manuscript/Figures/Figure5.png',w=15,h=7.5,
  #                                 relationship_sankey_mm(k_data) | relationship_sankey_ff(k_data))),
  # 
  # 
  # 
  # # Supplemental figures
  # 
  # tar_target(FigureS1, save_figure('./Manuscript/Figures/FigureS1.png',w=14,h=4,
  #                                  compare_BIC_plot(ff_mixtures, 'F-F') |
  #                                    compare_BIC_plot(fm_mixtures, 'F-M') |
  #                                    compare_BIC_plot(mm_mixtures, 'M-M'))),
  # 
  # tar_target(Figure_S2, save_figure('./Manuscript/Figures/FigureS2.png',w=10,h=5,
  #                                   simple_relatedness_plot(relatedness_df, k_data_relatedness))),
  # 
  # tar_target(FigureS3, save_figure('./Manuscript/Figures/FigureS3.png',w=20,h=4,
  #                                  plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Reliable-Adult', 'all_adult_reliable', 'All relationships') |
  #                                    plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Reliable-Adult', 'mm_adult_reliable', 'Male-Male') |
  #                                    plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Reliable-Adult', 'ff_adult_reliable', 'Female-Female') |
  #                                    plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Reliable-Adult', 'mf_adult_reliable', 'Male to Female') |
  #                                    plot_socprog_LAR_results_panel('./SOCPROG/SP_results/Reliable-Adult', 'fm_adult_reliable', 'Female to Male'))),
  # 
  # 
  # 
  # #######################################################################
  # ## Supplemental analyses: Distinctively marked individuals only
  # 
  # # Calculate associations
  # tar_target(group_df_all_old_reliable, group_df_all_old[reliability=='Reliable',,]),
  # tar_group_by(group_df_all_grouped_reliable, group_df_all_old_reliable, year),
  # tar_target(year_key_reliable, year_key(group_df_all_grouped_reliable)), # track year-group IDs
  # tar_target(group_associations_no_demo_restriction_reliable, build_group_associations(group_df_all_old_reliable)),
  # tar_target(group_associations_no_demo_restriction_day_reliable, merge(group_associations_no_demo_restriction_reliable, unique(group_df_all_old_reliable[,.(group_ID=myGroups, day=as.Date(groupTime))]), by='group_ID', all.X=TRUE)), ###### No effect on code but can change to 'all.x' in next iteration
  # tar_target(group_associations_demo_reliable, overlappingIDs(group_associations_no_demo_restriction_day_reliable, nbw_meta_raw)),
  # 
  # # for annual stream
  # tar_target(df_exclusions, group_df_all_old_reliable[!(year%in%c('2008','2009')),,]),
  # tar_group_by(df_exclusions_grouped, df_exclusions, year),
  # 
  # tar_target(group_associations_reliable, build_group_associations_2(df_exclusions_grouped), pattern=map(df_exclusions_grouped), iteration="list"),
  # 
  # tar_target(group_associations_all_reliable, unlist_group_associations(group_associations_reliable)), # also adds dyad-year names
  # 
  # # Run edge weight models using multi-annual network structure
  # tar_target(brms_fit_group_multiAnnual_reliable, brm(
  #   formula = together ~ (1|dyad_annual),
  #   data = group_associations_all_reliable,
  #   family = bernoulli(),
  #   prior = c(
  #     set_prior("normal(-1.5, 1.5)", class = "Intercept"),
  #     set_prior("normal(0, 1)", class = "sd", group = "dyad_annual", coef = "Intercept")),
  #   chains=4,
  #   cores=4,
  #   control = list(adapt_delta = 0.95))),
  # 
  # 
  # # Run edge weight models for 'reliable' left-sided dataset
  # tar_target(brms_fit_group_reliable, brm(
  #   formula = together ~ (1|dyad),
  #   data = group_associations_demo_reliable,
  #   family = bernoulli(),
  #   prior = c(
  #     set_prior("normal(-1.5, 1)", class = "Intercept"),
  #     set_prior("normal(0, 1)", class = "sd", group = "dyad", coef = "Intercept")),
  #   chains=4,
  #   cores=4)),
  # 
  # tar_target(SR_reliable, annual_S_and_R(group_associations_reliable)),
  # 
  # # Calculate modularity
  # tar_target(modularity_reliable, extract_global_trait_ma(brms_fit_group_multiAnnual, nbw_meta_raw, c('Female-Juvenile_Female-Juvenile',
  #                                                                                            'Female-Juvenile_Male',
  #                                                                                            'Female-Juvenile_Unk',
  #                                                                                            'Male_Male',
  #                                                                                            'Male_Unk',
  #                                                                                            'Unk_Unk'))),
  # 
  # # Fit mixture model for "reliable" data
  # tar_target(all_mixtures_reliable, fit_mixture_model_all(group_associations_demo_reliable, nbw_meta_raw, 3, 'BIC')),
  # 
  # # Fit mixture models for different dyadic sex compositions (will take a little while to run)
  # tar_target(ff_mixtures_reliable, fit_mixture_models(group_associations_demo_reliable, 'F-F', nbw_meta_raw, 5, 'BIC')),
  # tar_target(fm_mixtures_reliable, fit_mixture_models(group_associations_demo_reliable, 'F-M', nbw_meta_raw, 5, 'BIC')),
  # tar_target(mm_mixtures_reliable, fit_mixture_models(group_associations_demo_reliable, 'M-M', nbw_meta_raw, 5, 'BIC')),
  # 
  # # for model with unknown-sex individuals
  # tar_target(k_data_unks_included_reliable, merge(edges_df_group, unique(data.table(by_edge(all_mixtures_reliable$all.models[[3]]))), by.x='dyad',by.y='edge.id')),
  # tar_target(k_data_relatedness_unks_included_reliable, merge(k_data_unks_included_reliable, relatedness[,c('wang','wang.low','wang.high','dyad'),], by='dyad')),
  # 
  # tar_target(mk_res_all_reliable, brm(
  #   formula = likely.k ~ dyadicResidency + (1| mm(A, B)),
  #   data = k_data_unks_included_reliable[dRes %in% c('R-R','R-T','T-T'),,],
  #   family = categorical(link=logit),
  #   prior = c(
  #     set_prior("normal(0, 1)", class = "b")),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4)),
  # 
  # tar_target(mk_Age_all_reliable, brm(
  #   formula = likely.k ~ ageDiff + (1| mm(A, B)),
  #   data = k_data_unks_included_reliable,
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4)),
  # 
  # tar_target(mk_rel_all_reliable, brm(
  #   formula = likely.k ~ wang + (1| mm(A, B)),
  #   data = k_data_relatedness_unks_included_reliable,
  #   family = categorical(link=logit),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4,
  #   control = list(adapt_delta = 0.99))),
  # 
  # 
  # ## Biopsy robustness check
  # 
  # tar_target(biopsyTitles, biopsySexTitles(raw_photoData)),
  # tar_target(k_data_biopsy, dyadic_biopsy(k_data, biopsyTitles)),
  # 
  # tar_target(mk_biopsyCheck, brm(
  #   formula = likely.k ~ dyadic_biopsy + (1| mm(A, B)),
  #   data = k_data_biopsy[dSex=='F-F',,],
  #   family = categorical(link=logit),
  #   prior = c(
  #     set_prior("normal(0, 1)", class = "b")),
  #   warmup = 2000,
  #   iter = 4000,
  #   chains=4,
  #   cores=4)),

  
  #######################################################################
  ## Write supplement

  tar_quarto(
    supplement,
    file.path('Manuscript','Supplement_BottlenoseRelationships.qmd')),


  #######################################################################
  ## Write manuscript

  tar_quarto(
    paper,
    file.path('Manuscript','MS_BottlenoseRelationships.qmd'))
  
)