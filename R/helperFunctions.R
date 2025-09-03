MPA_create_paths <- function(purpose, report_year) {
  if (!dir.exists('../data')) dir.create('../data')
  if (!dir.exists(paste0('../data/', report_year))) dir.create(paste0('../data/', report_year))
  data_path <<- paste0("../data/", report_year, "/", purpose)
  if (!dir.exists(data_path)) dir.create(data_path)
  primary_data_path <<- paste0("../data/", report_year, "/", purpose, "/primary")
  if (!dir.exists(primary_data_path)) dir.create(primary_data_path)
  processed_data_path <<- paste0("../data/", report_year, "/", purpose, "/processed")
  if (!dir.exists(processed_data_path)) dir.create(processed_data_path)
  modelled_data_path <<- paste0("../data/", report_year, "/", purpose, "/modelled")
  if (!dir.exists(modelled_data_path)) dir.create(modelled_data_path)
  workspace_path <<- paste0("../data/", report_year, "/", purpose, "/workspace")
  if (!dir.exists(workspace_path)) dir.create(workspace_path)

  if (!dir.exists('../outputs')) dir.create('../outputs')
  if (!dir.exists(paste0('../outputs/', report_year))) dir.create(paste0('../outputs/', report_year))
  if (!dir.exists(paste0('../outputs/', report_year, "/", purpose))) dir.create(paste0('../outputs/', report_year, "/", purpose))
  fig_path <<- paste0("../outputs/", report_year, "/", purpose, "/figures")
  if (!dir.exists(fig_path)) dir.create(fig_path)
  tab_path <<- paste0("../outputs/", report_year, "/", purpose, "/figures")
  if (!dir.exists(tab_path)) dir.create(tab_path)
  params_path <<- "../parameters"
}



MPA_getData <- function() {
    ## Fish data
    MPA_getFishData()

    ## Benthos data
    MPA_getBenthosData()
}


MPA_getBenthosData <- function() {
        writeLines("
SELECT V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR,
 V_RM_SAMPLE.REEF_NAME, V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR,
 V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.TRANSECT_NO, BENTHOS_SUMMARY_ZEROS.GROUP_CODE,
Sum(BENTHOS_SUMMARY_ZEROS.COVER) AS SumOfCOVER
FROM V_RM_SAMPLE INNER JOIN BENTHOS_SUMMARY_ZEROS ON V_RM_SAMPLE.SAMPLE_ID = BENTHOS_SUMMARY_ZEROS.SAMPLE_ID
WHERE ((((V_RM_SAMPLE.P_CODE)='RAP' Or (V_RM_SAMPLE.P_CODE)='RMRAP') Or ((V_RM_SAMPLE.P_CODE)='RM' AND (V_RM_SAMPLE.REPORT_YEAR) > 2020))
 AND ((V_RM_SAMPLE.VISIT_NO) Is Not Null))
GROUP BY V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.REEF_NAME,
 V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR, V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.TRANSECT_NO,
 BENTHOS_SUMMARY_ZEROS.GROUP_CODE, V_RM_SAMPLE.VISIT_NO
HAVING (((BENTHOS_SUMMARY_ZEROS.GROUP_CODE)='HC' Or (BENTHOS_SUMMARY_ZEROS.GROUP_CODE) Like 'SC' Or
 (BENTHOS_SUMMARY_ZEROS.GROUP_CODE) Like 'A') AND ((V_RM_SAMPLE.VISIT_NO) Is Not Null))
ORDER BY V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.SITE_NO
",
paste0(primary_data_path, "/benthos.sql"))
    system(paste0("java -jar ../scripts/dbExport.jar ", primary_data_path, "/benthos.sql ", primary_data_path, "/benthos.csv reef reefmon"))

        benthos <- read.csv(paste0(primary_data_path, "/benthos.csv"), strip.white=TRUE)
        benthos <- benthos %>%
          ## filter(TRANSECT_NO!='6',!is.na(RAP_REEF_PAIR)) %>%
          filter(TRANSECT_NO!='6') %>%
            filter(REPORT_YEAR!=2017) # ARLINGTON REEF (OPEN) sampled in 2017, but should not be included in the analyses
        head(benthos)
        save(benthos, file = paste0(primary_data_path, "/benthos.RData"))
}

MPA_getFishData <- function() {
    writeLines("
SELECT V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR,
  V_RM_SAMPLE.REEF_NAME, V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR,
  V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.SAMPLE_DATE, V_RM_SAMPLE.TRANSECT_NO,
  RM_FISH05.FAMILY, RM_FISH05.FISH_CODE, Sum(RM_FISH05.ABUNDANCE) AS SumOfABUNDANCE, RM_FISH05.LENGTH
FROM V_RM_SAMPLE INNER JOIN RM_FISH05 ON V_RM_SAMPLE.SAMPLE_ID = RM_FISH05.SAMPLE_ID
WHERE (
  ((V_RM_SAMPLE.VISIT_NO) Is Not Null)
  AND
    (
      ((V_RM_SAMPLE.P_CODE)='RAP' Or (V_RM_SAMPLE.P_CODE)='RMRAP')
      OR
      ((V_RM_SAMPLE.P_CODE)='RM' AND (V_RM_SAMPLE.REPORT_YEAR) > 2020)
    )
)
GROUP BY V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.REEF_NAME,
 V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR, V_RM_SAMPLE.SITE_NO,
 V_RM_SAMPLE.SAMPLE_DATE, V_RM_SAMPLE.TRANSECT_NO, RM_FISH05.FAMILY, RM_FISH05.FISH_CODE,
 RM_FISH05.LENGTH, V_RM_SAMPLE.P_CODE, V_RM_SAMPLE.VISIT_NO
HAVING (((RM_FISH05.FISH_CODE) Not Like 'APR_VIRE' And
         (RM_FISH05.FISH_CODE) Not Like 'PDA.*' And
         (RM_FISH05.FISH_CODE) Not Like 'CHA_LUNS' And
         (RM_FISH05.FISH_CODE) Not Like 'ACA_SP' And
         (RM_FISH05.FISH_CODE) Not Like 'CHA_OXYC' And
         (RM_FISH05.FISH_CODE) Not Like 'CHA_SEME' And
         (RM_FISH05.FISH_CODE) Not Like 'CHL_LABI' And
         (RM_FISH05.FISH_CODE) Not Like 'CHS_FRON' And
         (RM_FISH05.FISH_CODE) Not Like 'CRO_ALTI' And
         (RM_FISH05.FISH_CODE) Not Like 'CTE_BINO' And
         (RM_FISH05.FISH_CODE) Not Like 'DIP_BIFA' And
         (RM_FISH05.FISH_CODE) Not Like 'GNA_AURO' And
         (RM_FISH05.FISH_CODE) Not Like 'GRA_ALBI' And
         (RM_FISH05.FISH_CODE) Not Like 'GYN_SPP' And
         (RM_FISH05.FISH_CODE) Not Like 'IST_DECO' And
         (RM_FISH05.FISH_CODE) Not Like 'LET_ERUS' And
         (RM_FISH05.FISH_CODE) Not Like 'NAS_ANBR' And
         (RM_FISH05.FISH_CODE) Not Like 'PMS_OLIG' And
         (RM_FISH05.FISH_CODE) Not Like 'POM_PAVO' And
         (RM_FISH05.FISH_CODE) Not Like 'POM_UN' And
         (RM_FISH05.FISH_CODE) Not Like 'PSU_TUKA' And
         (RM_FISH05.FISH_CODE) Not Like 'SAR_RUBR' And
         (RM_FISH05.FISH_CODE) Not Like 'SCA_SP'))
ORDER BY V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.REEF_NAME, V_RM_SAMPLE.REPORT_YEAR,
 V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.SAMPLE_DATE, V_RM_SAMPLE.TRANSECT_NO",
           paste0(primary_data_path, "/fish.sql"))
    system(paste0("java -jar ../scripts/dbExport.jar ", primary_data_path, "/fish.sql ", primary_data_path, "/fish.csv reef reefmon"))
    fish <- read.csv(paste0(primary_data_path, "/fish.csv"), strip.white=TRUE)
    fish = fish %>% filter(TRANSECT_NO!='6') %>%
        filter(REPORT_YEAR!=2017) # ARLINGTON REEF (OPEN) sampled in 2017, but should not be included in the analyses
    head(fish)
    save(fish, file = paste0(primary_data_path, "/fish.RData"))
}

test <- function() {
    writeLines("
SELECT V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR,
  V_RM_SAMPLE.REEF_NAME, V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR,
  V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.SAMPLE_DATE, V_RM_SAMPLE.TRANSECT_NO,
  RM_FISH05.FAMILY, RM_FISH05.FISH_CODE, Sum(RM_FISH05.ABUNDANCE) AS SumOfABUNDANCE, RM_FISH05.LENGTH
FROM V_RM_SAMPLE INNER JOIN RM_FISH05 ON V_RM_SAMPLE.SAMPLE_ID = RM_FISH05.SAMPLE_ID
WHERE ((((V_RM_SAMPLE.P_CODE)='RM' AND (V_RM_SAMPLE.REPORT_YEAR) > 2020))
 AND ((V_RM_SAMPLE.VISIT_NO) Is Not Null) AND ROWNUM < 10)
GROUP BY V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.REEF_NAME,
 V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR, V_RM_SAMPLE.SITE_NO,
 V_RM_SAMPLE.SAMPLE_DATE, V_RM_SAMPLE.TRANSECT_NO, RM_FISH05.FAMILY, RM_FISH05.FISH_CODE,
 RM_FISH05.LENGTH, V_RM_SAMPLE.P_CODE, V_RM_SAMPLE.VISIT_NO
",
'data/test.sql')
    system("java -jar scripts/dbExport.jar data/test.sql data/test.csv reef reefmon")
    test <- read.csv('data/test.csv', strip.white=TRUE)
    test
}

MPA_loadFishData <- function() {
    load(file = paste0(primary_data_path, "/fish.RData"))
    fish
}
MPA_loadBenthosData <- function() {
    load(file = paste0(primary_data_path, "/benthos.RData"))
    benthos
}

MPA_cleanImport <- function(data,purpose) {
    if(!is.null(data$SITE_LAT)) data$LAT <- as.numeric(as.character(data$SITE_LAT))
    if(!is.null(data$SITE_LONG)) data$LONG <- as.numeric(as.character(data$SITE_LONG))

    ## As of 2021, there was a change in the monitoring design in which
    ## some reefs were dropped and others added.
    ## As a result, some of the pairs have been disrupted.
    ##  - some pairs no longer exist
    ##  - some reefs are now part of a new pair
    ##  - some new reefs form new pairs
    ## Unfortunately, the RAP_REEF_PAIR field in the database is now scrambled.
    ## Mike has created a lookup in order to form sensible RAP_REEF_PAIRS for
    ## the post 2020 records
    rap_reef_pair_lookup <- read.csv(paste0(params_path, "/RAP Pair changes_ME.csv"),
                                     strip.white = TRUE)
    data <- data %>%
        left_join(rap_reef_pair_lookup %>%
                  dplyr::select(REEF_NAME, NEW_RAP_PAIR, OPENORCLOSED_AFTER2004)) %>%
        mutate(RAP_REEF_PAIR = ifelse(REPORT_YEAR > 2020,
                                      NEW_RAP_PAIR,
                                      RAP_REEF_PAIR),
               RAP_OPEN_CLOSED = ifelse(REPORT_YEAR > 2020,
                                        OPENORCLOSED_AFTER2004,
                                        RAP_OPEN_CLOSED)
               )
    ## data %>% filter(is.na(RAP_REEF_PAIR)) %>%
    ##     dplyr::select(REEF_NAME, RAP_REEF_PAIR, REPORT_YEAR) %>% distinct() %>% dim
    data <- data %>%
        mutate(
            Year=REPORT_YEAR
           #,dYear=Year
           ,cYear=factor(Year)
           ,Zone=factor(RAP_OPEN_CLOSED, levels=c('C','O'), labels=c('Closed','Open'))
           ,Pair=factor(RAP_REEF_PAIR)
           ,Reef=factor(interaction(RAP_REEF_PAIR,REEF_NAME))
           ,Site=factor(interaction(RAP_REEF_PAIR,REEF_NAME,SITE_NO))
           ,Transect=factor(interaction(RAP_REEF_PAIR,REEF_NAME,SITE_NO,TRANSECT_NO))
        )
    if (purpose == 'Reports') {
      data <- data %>%
        mutate(Sector=dplyr:::recode(A_SECTOR, CA='CAIN', IN='CAIN'),
               Sector=factor(Sector,
                             levels=c('CAIN','TO','PO','SW','CB','CG','PC','WH','CL'),
                             labels=c('Cairns','Townsville','Pompeys','Swains',
                                      'Cap-Bunkers','Cape Grenville',
                                      'Princess Charlotte Bay','Whitsundays',
                                      'Cooktown-Lizard')))
    }
    if (purpose == 'Web') {
      data <- data %>%
        mutate(Sector=factor(A_SECTOR, levels=c('CA','IN','TO','PO','SW','CB',
                                                'CG','PC','WH','CL'),
                             labels=c('Cairns','Innisfail','Townsville','Pompeys',
                                      'Swains','Cap-Bunkers','Cape Grenville',
                                      'Princess Charlotte Bay','Whitsundays',
                                      'Cooktown-Lizard')))
    }
    if(exists('FISH_CODE', where = data)) {
      data <- data %>%
        mutate(Species=gsub('\\.','_',FISH_CODE))
    }
    data
}

MPA_group_2025 <- function(data) {
  ## Add trophic group to the data based on a lookup table
  ## As of May 2025, there is a new lookup.
  ## Mike produced a file (dahsboard_fish_groups for murray.csv) which
  ## I have renamed to dashboard_fish_groups.csv (it is the same one
  ## that is used for the LTMP dashboard).
  trophic_groups <- read_csv(paste0(params_path, "/dashboard_fish_groups.csv"))
  lw_conv <- read.csv(paste0(params_path, "/L-W co-effs.csv"), header=T, sep=",", strip.white=T)
  finer_groups <- read.csv(paste0(params_path, "/Murray_LTMPFish_Mar21.csv")) %>%
    mutate(Species = gsub('\\.','_', FISH_CODE), Group=Groups)

  groups <-
    trophic_groups %>%
    dplyr::filter(`Dashboard species list` == "restricted") |>
    mutate(Trophic = Dashboard_trophic) |>
    mutate(Trophic = ifelse(Trophic == "Carnivore" &
                              !str_detect(FISH_CODE, "^PMS.*|^VAR.*"),
      "Secondary targets", Trophic),
      Trophic = ifelse(Trophic == "Carnivore" &
                         str_detect(FISH_CODE, "^PMS.*|^VAR.*"),
        "Coral Trout", Trophic),
      Trophic = ifelse(Trophic == "Corallivore", "Corallivores", Trophic),
      Trophic = ifelse(Trophic == "Omnivore", "Omnivores", Trophic)
    ) |>
    dplyr::select(FISH_CODE, Trophic) |>
    dplyr::mutate(Group = Trophic)

  richness <- data %>%
    group_by(Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Species) %>%
    summarize(SumOfABUNDANCE=sum(SumOfABUNDANCE,na.rm=TRUE)) %>%
    ungroup() %>%
    MPA_speciesRichness() %>%
    mutate(Group='Species Richness', Value=Richness) %>%
    dplyr::select(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Value)

  data1 <- data  # keep a pre copy so we can process Frederieke's groups

  data <-
    data %>%
    mutate(Value=SumOfABUNDANCE) %>%
    left_join(groups, by=c("FISH_CODE")) %>%
    dplyr::filter(!is.na(Trophic)) %>%
    MPA_calculateDensities() %>%
    dplyr::select(-Area) %>%
    split(.$Group)

  ## For Coral Trout and Secondary targets, create additional LENGTH and BIOMASS items
  data[['Coral Trout Biomass']] <- MPA_calculateBiomass(data[['Coral Trout']],lw_conv) %>%
    mutate(Value=Biomass, Group='Coral Trout Biomass') %>% dplyr::select(-a,-b,-Biomass)
  data[['Coral Trout Length']] <- data[['Coral Trout']] %>%
    mutate(Value=LENGTH,Group='Coral Trout Length')
  data[['Secondary targets Biomass']] <- MPA_calculateBiomass(data[['Secondary targets']],lw_conv) %>%
    mutate(Value=Biomass, Group='Secondary targets Biomass') %>% dplyr::select(-a,-b,-Biomass)
  data[['Secondary targets Length']] <- data[['Secondary targets']] %>%
    mutate(Value=LENGTH,Group='Secondary targets Length')

  ## For finer groups (for Frederieke)
  data1 <- data1 %>% mutate(Value=SumOfABUNDANCE) %>%
    left_join(finer_groups %>%
                dplyr::select(Species, Group)) %>%
    MPA_calculateDensities() %>% dplyr::select(-Area) %>%
    split(.$Group)
  for (i in names(data1)) {
    nms <- paste(i, 'Biomass')
    data1[[nms]] <- MPA_calculateBiomass(data1[[i]],lw_conv) %>%
      mutate(Value=Biomass, Group=nms) %>% dplyr::select(-a,-b,-Biomass)
    nms <- paste(i, 'Length')
    data1[[nms]] <- data1[[i]] %>%
      mutate(Value=LENGTH,Group=nms)
  }
  data1 <- do.call('rbind', data1) %>%
    dplyr::select(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Value)

  do.call('rbind', data) %>%
    dplyr::select(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Value) %>%
    bind_rows(richness) %>%
    bind_rows(data1)
}

## Standardize all so that the response is Value
## It is either:
## - density
## - length (Coral Trout and Secondary targets)
## - biomass per unit area (Coral Trout and Secondary targets)
MPA_group <- function(data) {
    groups <- read.csv(paste0(params_path, "/MPA paper trophic groups.csv"), strip.white=TRUE)
    lw_conv <- read.csv(paste0(params_path, "/L-W co-effs.csv"), header=T, sep=",", strip.white=T)

    finer_groups <- read.csv(paste0(params_path, "/Murray_LTMPFish_Mar21.csv")) %>%
        mutate(Species = gsub('\\.','_', FISH_CODE), Group=Groups)

    groups$Species <- groups$Code
    groups$Group <- groups$Trophic

    richness <- data %>%
        group_by(Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Species) %>%
      summarize(SumOfABUNDANCE=sum(SumOfABUNDANCE,na.rm=TRUE)) %>%
      ungroup() %>%
        MPA_speciesRichness() %>%
        mutate(Group='Species Richness', Value=Richness) %>%
        dplyr::select(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Value)
    data1 <- data  # keep a pre copy so we can process Frederieke's groups
    data <- data %>% mutate(Value=SumOfABUNDANCE) %>%
        left_join(groups) %>%
        MPA_calculateDensities() %>% dplyr::select(-Area) %>%
        split(.$Group)

    ## For Coral Trout and Secondary targets, create additional LENGTH and BIOMASS items
    data[['Coral Trout Biomass']] <- MPA_calculateBiomass(data[['Coral Trout']],lw_conv) %>%
        mutate(Value=Biomass, Group='Coral Trout Biomass') %>% dplyr::select(-a,-b,-Biomass)
    data[['Coral Trout Length']] <- data[['Coral Trout']] %>%
        mutate(Value=LENGTH,Group='Coral Trout Length')
    data[['Secondary targets Biomass']] <- MPA_calculateBiomass(data[['Secondary targets']],lw_conv) %>%
        mutate(Value=Biomass, Group='Secondary targets Biomass') %>% dplyr::select(-a,-b,-Biomass)
    data[['Secondary targets Length']] <- data[['Secondary targets']] %>%
        mutate(Value=LENGTH,Group='Secondary targets Length')

    ## For finer groups (for Frederieke)
    data1 <- data1 %>% mutate(Value=SumOfABUNDANCE) %>%
        left_join(finer_groups %>% dplyr::select(Species, Group)) %>%
        MPA_calculateDensities() %>% dplyr::select(-Area) %>%
        split(.$Group)
    for (i in names(data1)) {
        nms <- paste(i, 'Biomass')
        data1[[nms]] <- MPA_calculateBiomass(data1[[i]],lw_conv) %>%
            mutate(Value=Biomass, Group=nms) %>% dplyr::select(-a,-b,-Biomass)
        nms <- paste(i, 'Length')
        data1[[nms]] <- data1[[i]] %>%
          mutate(Value=LENGTH,Group=nms)
        }
    data1 <- do.call('rbind', data1) %>%
        dplyr::select(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Value)

    do.call('rbind', data) %>%
        dplyr::select(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Value) %>%
        bind_rows(richness) %>%
        bind_rows(data1)
        #filter(!is.na(Value))
}


MPA_speciesRichness <- function(data) {
  d <- data %>%
    dplyr:::select(Sector,SHELF,Pair,Zone,Reef,Site,
                   Transect,Year,cYear,Species,SumOfABUNDANCE) %>%
    mutate(Present = ifelse(is.na(SumOfABUNDANCE) | SumOfABUNDANCE==0,0,1)) %>%
    dplyr:::select(-SumOfABUNDANCE) %>%
            spread(key=Species,value=Present)
  d <- d %>%
    mutate(Richness=d %>%
             rowwise() %>%
             dplyr:::select(matches('^[A-Z]{3,3}_[A-Z]{3,4}$')) %>% rowSums(na.rm=TRUE))
  d
}

MPA_calculateDensities <- function(data) {
  densities <- read.csv(paste0(params_path, "/density_table.csv"), strip.white=TRUE) %>%
    mutate(Taxa=gsub('\\_','.',Taxa)) %>%
    dplyr:::rename(Species=Taxa,Area=Convert)
  data %>%
    left_join(densities) %>%
    mutate(Area = ifelse(is.na(Area), 4, Area),
           Value = Value * Area)
}

MPA_calculateBiomass <- function(data, lw_conv){
  data <- data %>% #gather(key='Species', value='Value', contains('_')) %>%
    left_join(lw_conv) %>%
                                        #mutate(Biomass=SumOfABUNDANCE*a*LENGTH^b)
    mutate(Biomass = Value * a * LENGTH^b)
  data
}

## First breaks the data.frame up into a list with item membership determined by filters
## defined in parameters/webGroups.csv
## Then the list is bound back into a single data.frame.
## The resulting data frame will be longer than the original if any entries (FAMILY OR FISH_CODE)
## are defined in multiple groups.
## Value represents abundance only
MPA_webgroups <- function(data) {
    webgroups <- read.table(paste0(params_path, "/webGroups.csv"), sep=';', header=TRUE)
    data1 <- list()
    for (i in 1:nrow(webgroups)) {
        data1[[as.character(webgroups[i,1])]] =
            data %>% dplyr::filter_(.dots=as.character(webgroups[i,2])) %>%
            mutate(Group=as.character(webgroups[i,1])) %>% droplevels %>%
            group_by(Group,Sector,SHELF,Year,cYear,Zone,Pair,Reef,Site,Transect) %>%
            summarize(Value=sum(SumOfABUNDANCE,na.rm=TRUE)) %>% ungroup
    }
    do.call('rbind', data1)
}

MPA_benthosgroups <- function(data) {
  data %>%
    dplyr::filter(GROUP_CODE %in% c('HC','A','SC')) %>%
    droplevels() %>%
    mutate(Group = GROUP_CODE,
           Value = SumOfCover)
}

MPA_transectAgg <- function(data) {
  data %>%
    group_by(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,cYear,Year) %>%
    summarize(Value = ifelse(unique(Group) %in%
                             c('Coral Trout Length', 'Secondary targets Length'),
                             mean(Value,na.rm=TRUE),
                             sum(Value,na.rm=TRUE)))
}

## The following is not correct once we have expressed everything as a density.
## Rather than be sum it should be mean
## MPA_siteAgg <- function(data) {
##     data %>% group_by(Group,Sector,SHELF,Pair,Reef,Site,Zone,cYear,Year) %>%
##         summarize(Value=ifelse(unique(Group) %in% c('Coral Trout Length', 'Secondary target Length','A','HC','SC'), mean(Value,na.rm=TRUE),
##                                 sum(Value,na.rm=TRUE)))
## }
MPA_siteAgg <- function(data) {
  nms <- unique(data$Group)[grepl('Length',unique(data$Group))]
  data %>%
    group_by(Group,Sector,SHELF,Pair,Reef,Site,Zone,cYear,Year) %>%
    summarize(Value = ifelse(unique(Group) %in% c(nms,'A','HC','SC'),
                             mean(Value,na.rm=TRUE),
                             mean(Value,na.rm=TRUE)))
}


MPA_makeLabels <- function(type='Reports') {
    labels = list()
    if (type=='Reports') {
        labels[['Coral Trout Biomass']] = expression(Trout~biomass~(kg~per~1000~m^2))
        labels[['Coral Trout']] = expression(Trout~(per~1000~m^2))
        labels[['Coral Trout Length']] = expression(Trout~Length~(cm))
        labels[['Secondary targets Biomass']] = expression(Secondary~targets~biomass~(kg~per~1000~m^2))
        labels[['Secondary targets']] = expression(Secondary~targets~(per~1000~m^2))
        labels[['Secondary targets Length']] = expression(Secondary~targets~Length~(cm))
        labels[['Scrapers']] = expression(Scrapers~(per~1000~m^2))
        labels[['Croppers']] = expression(Croppers~(per~1000~m^2))
        labels[['Farmers']] = expression(Farmers~(per~1000~m^2))
        labels[['Planktivores']] = expression(Planktivores~(per~1000~m^2))
        labels[['Detritivores']] = expression(Detritivores~(per~1000~m^2))
        ## labels[['Benthic foragers']] = expression(Benthic~forager~(per~1000~m^2))
        labels[['Browsers']] = expression(Browsers~(per~1000~m^2))
        labels[['Excavators']] = expression(Excavators~(per~1000~m^2))
        ## labels[['Obligate corallivores']] = expression(Obligate~corallivores~(per~1000~m^2))
        labels[['Corallivores']] = expression(Corallivores~(per~1000~m^2))
        ## labels[['Omnivorous Pomacentridae']] = expression(Omnivorous~Pomacentridae~(per~1000~m^2))
        labels[['Omnivores']] = expression(Omnivores~(per~1000~m^2))

        labels[['Species Richness']] = expression(Species~Richness~(per~site))

        labels[['A']] = expression(Algal~cover~('%'))
        labels[['HC']] = expression(Hard~coral~cover~('%'))
        labels[['SC']] = expression(Soft~coral~cover~('%'))

        labels[['Labridae (Wrasses)']] = expression(Wrass~(per~1000~m^2))
        labels[['Labridae (Wrasses) Length']] = expression(Wrass~Length~(cm))
        labels[['Labridae (Wrasses) Biomass']] = expression(Wrass~biomass~(kg~per~1000~m^2))
        labels[['Lethrinidae (Emperors)']] = expression(Emporer~(per~1000~m^2))
        labels[['Lethrinidae (Emperors) Length']] = expression(Emporer~Length~(cm))
        labels[['Lethrinidae (Emperors) Biomass']] = expression(Emporer~biomass~(kg~per~1000~m^2))
        labels[['Lethrinus miniatus and L. nebulosus (Redthroat and Spangled emperors)']] = expression(Redthroat~(per~1000~m^2))
        labels[['Lethrinus miniatus and L. nebulosus (Redthroat and Spangled emperors) Length']] = expression(Redthroat~Length~(cm))
        labels[['Lethrinus miniatus and L. nebulosus (Redthroat and Spangled emperors) Biomass']] = expression(Redthroat~biomass~(kg~per~1000~m^2))
        labels[['Lutjanidae (Tropical Snappers)']] = expression(Snapper~(per~1000~m^2))
        labels[['Lutjanidae (Tropical Snappers) Length']] = expression(Snapper~Length~(cm))
        labels[['Lutjanidae (Tropical Snappers) Biomass']] = expression(Snapper~biomass~(kg~per~1000~m^2))
        labels[['Plectropomus and Variola spp (Coral trout)']] = expression(Coral~trout~(per~1000~m^2))
        labels[['Plectropomus and Variola spp (Coral trout) Length']] = expression(Coral~trout~Length~(cm))
        labels[['Plectropomus and Variola spp (Coral trout) Biomass']] = expression(Coral~trout~biomass~(kg~per~1000~m^2))
        labels[['Serranidae (Rockcods)']] = expression(Rockcod~(per~1000~m^2))
        labels[['Serranidae (Rockcods) Length']] = expression(Rockcod~Length~(cm))
        labels[['Serranidae (Rockcods) Biomass']] = expression(Rockcod~biomass~(kg~per~1000~m^2))
    } else {
        labels[['large']] = expression(Median~abundance~per~Site)
        labels[['small']] = expression(Median~abundance~per~Site)
        labels[['total']] = expression(Median~abundance~per~Site)
        labels[['trout']] = expression(Median~abundance~per~Site)
        labels[['herbivores']] = expression(Median~abundance~per~Site)
        labels[['secondary_targets']] = expression(Median~abundance~per~Site)
        labels[['A']] = expression(Algal~cover~('%'))
        labels[['HC']] = expression(Hard~coral~cover~('%'))
        labels[['SC']] = expression(Soft~coral~cover~('%'))
    }
    labels
}

MPA_makeTitles <- function(type='Reports') {
    titles = list()
    if (type=='Reports') {
        titles[['Coral Trout Biomass']] = ''
        titles[['Coral Trout']] = ''
        titles[['Coral Trout Length']] = ''
        titles[['Secondary targets Biomass']] = ''
        titles[['Secondary targets']] = ''
        titles[['Secondary targets Length']] = ''
        titles[['Scrapers']] = ''
        titles[['Croppers']] = ''
        titles[['Farmers']] = ''
        titles[['Planktivores']] = ''
        titles[['Detritivores']] = ''
        ## titles[['Benthic foragers']] = ''
        titles[['Browsers']] = ''
        titles[['Excavators']] = ''
        ## titles[['Obligate corallivores']] = ''
        titles[['Corallivores']] = ''
        ## titles[['Omnivorous Pomacentridae']] = ''
        titles[['Omnivores']] = ''

        titles[['Species Richness']] = ''

        titles[['A']] = ''
        titles[['HC']] = ''
        titles[['SC']] = ''
        titles[['Labridae (Wrasses) Biomass']] = ''
        titles[['Lethrinidae (Emperors) Biomass']] = ''
        titles[['Lethrinus miniatus and L. nebulosus (Redthroat and Spangled emperors) Biomass']] = ''
        titles[['Lutjanidae (Tropical Snappers) Biomass']] = ''
        titles[['Plectropomus and Variola spp (Coral trout) Biomass']] = ''
        titles[['Serranidae (Rockcods) Biomass']] = ''

        titles[['Labridae (Wrasses) Length']] = ''
        titles[['Lethrinidae (Emperors) Length']] = ''
        titles[['Lethrinus miniatus and L. nebulosus (Redthroat and Spangled emperors) Length']] = ''
        titles[['Lutjanidae (Tropical Snappers) Length']] = ''
        titles[['Plectropomus and Variola spp (Coral trout) Length']] = ''
        titles[['Serranidae (Rockcods) Length']] = ''

        titles[['Labridae (Wrasses)']] = ''
        titles[['Lethrinidae (Emperors) Length']] = ''
        titles[['Lethrinus miniatus and L. nebulosus (Redthroat and Spangled emperors)']] = ''
        titles[['Lutjanidae (Tropical Snappers)']] = ''
        titles[['Plectropomus and Variola spp (Coral trout)']] = ''
        titles[['Serranidae (Rockcods)']] = ''

    } else {
        titles[['large']] = 'Large fish density from fixed site surveys'
        titles[['small']] = 'Small fish density from fixed site surveys'
        titles[['total']] = 'Total fish density from fixed site surveys'
        titles[['trout']] = 'Coral trout density from fixed site surveys'
        titles[['herbivores']] = 'Herbivore density from fixed site surveys'
        titles[['secondary_targets']] = 'Secondary target density from fixed site surveys'
        titles[['A']] = 'Algal cover from fixed site surveys'
        titles[['HC']] = 'Hard coral cover from fixed site surveys'
        titles[['SC']] = 'Soft coral cover from fixed site surveys'
    }
    titles
}


MPA_rawMeans <- function(data) {
    data %>% group_by(Sector,Zone,cYear) %>%
        summarize(Mean=mean(Value, na.rm=TRUE),
                  lower=ci(Value,na.rm=TRUE)[2],
                  upper=ci(Value,na.rm=TRUE)[3]
                  ) %>%
        suppressMessages() %>%
        suppressWarnings()
}

## Still need to put the Report data purpose figures in..
MPA_RAPPlot <- function(dat, ytitle, title, stat='median', purpose='Web') {
    if (purpose=='Web') {
        if(stat=='mean') {
            dat = dat %>% mutate(Value=Mean)
        } else {
            dat = dat %>% mutate(Value=Median)
            #ytitle=substitute(ytitle,list(Mean=Median))
        }
        max.y1=max.y = max(dat$upper, na.rm=TRUE)*1.25
        if(any(all.vars(ytitle) == 'cover')) max.y1 = min(max(pretty(max.y1)),100)

        p <-ggplot(dat, aes(y=Value, x=cYear, fill=Zone,color=Zone)) +
            geom_blank() + #aes(x=1,y=0))+
            geom_line(aes(x=as.numeric(cYear)),position=position_dodge(width=0.1))+
            geom_linerange(aes(ymin=lower, ymax=upper),position=position_dodge(width=0.1), show.legend=FALSE)+
            geom_point(position=position_dodge(width=0.1), size=2)+
                                        #facet_grid(~Sector, switch='x')+
            scale_fill_manual('', breaks=c('Closed','Open'), titles=c('Fishing prohibited','Open to fishing'),values=c('forestgreen','blue'))+
            scale_color_manual('', breaks=c('Closed','Open'), labels=c('Fishing prohibited','Open to fishing'),values=c('forestgreen','blue'))
                                        #scale_y_continuous(expression(paste(Trout~biomass~"(per 1000", m^2, ")", sep="")), labels=comma)+
        if (any(all.vars(ytitle) == 'cover')) p=p+scale_y_continuous(ytitle, breaks=seq(0,max.y1,length=5),labels=seq(0,max.y1,length=5), limits=c(0,max.y))
        if (!any(all.vars(ytitle) == 'cover')) p = p+scale_y_continuous(ytitle, labels=comma, limits=c(0,max.y))
        p=p+scale_x_discrete('Year')+
            theme_classic() +
            ggtitle(title)
        p<-p+theme(legend.position=c(1,1), legend.justification=c(1,1),
                   legend.direction = 'horizontal', legend.background=element_blank(),
                   strip.background=element_blank(),strip.text.x=element_text(size=12),
                   axis.title.y=element_text(vjust=1.5),
                   panel.spacing=unit(1,unit='lines'), axis.text.x=element_text(size=10),
                   axis.line.y=element_line(), axis.line.x=element_line(),strip.placement='outside')
    } else if (purpose=='Reports') {
        if(stat=='mean') {
            p <-ggplot(dat, aes(y=Mean, x=as.numeric(as.character(cYear)), fill=Zone, shape=Zone,linetype=Zone)) +
                geom_blank()+
                geom_line(aes(x=as.numeric(as.character(cYear))))+
                geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1, linetype=1)+geom_point()+
                ## facet_grid(~Sector, switch='x')+
                facet_wrap(~Sector, nrow = 2,scales = 'free_x', strip.position='bottom')+
                ## xlim(range(as.numeric(as.character(dat$cYear)))) +
                scale_fill_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c('black','white'))+
                scale_shape_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(21,21))+
                scale_linetype_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(1,2))+
                                        #scale_y_continuous(expression(paste(Trout~biomass~"(per 1000", m^2, ")", sep="")), labels=comma)+
                scale_y_continuous(ytitle, labels=comma)+
                scale_x_continuous('', breaks=scales::breaks_width(1),
                                   limits=range(as.numeric(as.character(dat$cYear))))+
                theme_classic()
        } else {
            p <-ggplot(dat, aes(y=Median, x=as.numeric(as.character(cYear)), fill=Zone, shape=Zone,linetype=Zone)) +
                geom_blank()+
                geom_line(aes(x=as.numeric(as.character(cYear))))+
                geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1, linetype=1)+geom_point()+
                ## facet_grid(~Sector,switch='x')+
                facet_wrap(~Sector, nrow = 2,scales = 'free_x', strip.position='bottom')+
                scale_fill_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c('black','white'))+
                scale_shape_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(21,21))+
                scale_linetype_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(1,2))+
                                        #scale_y_continuous(expression(paste(Trout~biomass~"(per 1000", m^2, ")", sep="")), labels=comma)+
                scale_y_continuous(ytitle, labels=comma)+
                scale_x_continuous('', breaks=scales::breaks_width(1),
                                   limits=range(as.numeric(as.character(dat$cYear))))+
                                        #scale_x_continuous('', breaks=2006:2014)+
                theme_classic()
        }
        p<-p+theme(legend.position=c(0.01,1), legend.justification=c(0,1), strip.background=element_blank(),strip.text.x=element_text(size=12),
                   axis.title.y=element_text(vjust=1.5), axis.title.x=element_blank(),
                   axis.ticks.length.x = unit(2.75, units = 'points'),
                   panel.spacing=unit(1,unit='lines'),
                   axis.text.x=element_text(size=10, colour = c('black',rep(NA,3))),
                   axis.line.y=element_line(), axis.line.x=element_line(),strip.placement='outside')
    }

    #g <- ggplotGrob(p)
    #print(p)
    #grid.newpage()
                                        #grid.draw(rbind(g[c(1,4:5)], g[1:3],size="first"))
    p
}

MPA_INLA_biomass <- function(data) {
    ndraws <- 1000
    n.1 <- 1:nrow(data) # indices of data
    ## find out which combinations of Sector/Year/Zone were not surveyed
    data.mis <- data %>% tidyr::expand(Sector,cYear,Zone) %>% anti_join(data) %>% mutate(ValueMis = 1)
    newdata <- cbind(Value=NA,
                     expand.grid(Sector=levels(data$Sector),
                                 cYear=levels(data$cYear),
                                 Zone=levels(data$Zone)),
                     Pair=NA,
                     Reef=NA,
                     Site=NA)
    dat <- rbind(data %>%
                 dplyr:::select(Value,Sector,cYear,Zone,Pair,Reef,Site),
                 newdata) %>%
        as.data.frame
    n.2 <- (nrow(data)+1):nrow(dat)
    if (length(unique(data$Sector))>1) {
        ## Fit the hurdle gamma model
        ## Start by preparing the data
        ## 1. bind a version of the data for which Value > 0 to the data
        ## 2. Create a two column matrix for Y
        dat.g <- dat %>% filter(Value > 0)
        dat.c <- rbind(dat, dat.g)
        Y <- rbind(
            cbind(as.numeric(dat$Value > 0), NA),
            cbind(NA, dat.g$Value/1000))
        dat.inla.hg <- inla(Y~Sector*cYear*Zone+
                                f(Pair, model='iid') +
                                f(Reef, model='iid') +
                                f(Site, model='iid'),
                            data=dat.c,
                            family=c('binomial','gamma'),
                            control.predictor = list(compute=TRUE, link=NA),
                            control.compute = list(config = TRUE, dic = TRUE, cpo = TRUE,
                                                   waic = TRUE, return.marginals.predictor = TRUE)
                            )
        ## Get draw
        draws <- inla.posterior.sample(ndraws, dat.inla.hg)
        draws.attr <- attr(draws, ".content")
        wch <- which(draws.attr$tag == "(Intercept)")
        n.i <- draws.attr$start[wch]:draws.attr$start[length(draws.attr$start)]
        newdata.hg <- data %>% dplyr::select(Sector, cYear, Zone) %>% distinct()
        draws = sapply(draws, function(x) x[['latent']])
        xmat <- model.matrix(~Sector*cYear*Zone, data = newdata.hg)
        fit = t(draws[n.i,]) %*% t(xmat)
        ## fortify posteriors
        fit.hg <- newdata.hg %>% cbind(t(fit)) %>%
            pivot_longer(cols = c(-Sector,-cYear,-Zone), values_to = 'Fit') %>%
            group_by(Sector, cYear, Zone) %>%
            mutate(Fit = 1000*exp(Fit),
                   Iter = 1:n()) %>%
            dplyr::select(-name)

        cellmeans <- fit.hg %>%
            full_join(data.mis) %>%
            mutate(Fit = ifelse(is.na(ValueMis), Fit, NA)) %>%
            dplyr::select(-ValueMis)

        ## effects
        xmat.hg <- model.matrix(~Sector*cYear*Zone, data = newdata.hg)
        newdata.hg1 <- newdata.hg %>% cbind(xmat.hg) %>%
            group_by(Zone) %>%
            summarise(across(c(-Sector,-cYear),
                             mean)) %>%
            ungroup() %>%
            arrange(desc(Zone)) %>%
            dplyr::select(-Zone) %>%
            summarise(across(everything(), diff)) %>%
            as.matrix()

        effects = t(draws[n.i,]) %*% t(newdata.hg1)
        effects.95 <- 100*(HDInterval::hdi(exp(effects), credMass = 0.95)-1)[,1] %>%
            setNames(c('lower.1', 'upper.1'))
        effects.90 <- 100*(HDInterval::hdi(exp(effects), credMass = 0.90)-1)[,1] %>%
            setNames(c('lower', 'upper'))
        effects <- data.frame(
            Mean = 100*(mean(exp(effects))-1),
            Median = 100*(median(exp(effects))-1),
            `X2.5.` = effects.95[1],
            `X10.` = effects.90[1],
            `X90.` = effects.90[2],
            `X97.5.` = effects.95[2],
            p.0 = sum(effects >0)/length(effects),
            t(effects.90),
            t(effects.95))


##         ## Start with binomial component
##         dat.inla.b <- inla(Value~Sector*cYear*Zone+
##                              f(Pair, model='iid') +
##                              f(Reef, model='iid') +
##                              f(Site, model='iid'),
##                          data=dat %>% mutate(Value = as.numeric(Value > 0)),
##                          family='binomial',
##                          control.family=list(link='logit'),
##                          control.predictor = list(compute=TRUE, link=NA),
##                          control.compute = list(config = TRUE, dic = TRUE, cpo = TRUE,
##                                                 waic = TRUE, return.marginals.predictor = TRUE)
##                          )
##         ## Get draws
##         draws <- inla.posterior.sample(ndraws, dat.inla.b)
##         draws.attr <- attr(draws, ".content")
##         wch <- which(draws.attr$tag == "(Intercept)")
##         n.i <- draws.attr$start[wch]:draws.attr$start[length(draws.attr$start)]
##         newdata.b <- data %>% dplyr::select(Sector, cYear, Zone) %>% distinct()
##         draws = sapply(draws, function(x) x[['latent']])
##         xmat <- model.matrix(~Sector*cYear*Zone, data = newdata.b)
##         fit = t(draws[n.i,]) %*% t(xmat)
##         ## fortify posteriors
##         fit.b <- newdata.b %>% cbind(t(fit)) %>%
##             pivot_longer(cols = c(-Sector,-cYear,-Zone), values_to = 'Fit.b') %>%
##             group_by(Sector, cYear, Zone) %>%
##             mutate(Fit.b = plogis(Fit.b),
##                    Iter = 1:n()) %>%
##             dplyr::select(-name)
##         ## overall effects
##         xmat.effects <- newdata.b %>% cbind(xmat) %>%
##             group_by(Zone) %>%
##             summarise(across(c(-Sector,-cYear),
##                              mean)) %>%
##             ungroup() %>%
##             arrange(desc(Zone)) %>%
##             dplyr::select(-Zone) %>%
##             summarise(across(everything(), diff)) %>%
##             as.matrix()
##         fit.effects.b = t(draws[n.i,]) %*% t(xmat.effects)

##         ## Now the gamma
##         dat.g <- data %>%
##             dplyr:::select(Value,Sector,cYear,Zone,Pair,Reef,Site) %>%
##             filter(Value != 0) %>%
##             droplevels
##         dat.pred <- rbind(dat.g,newdata) %>%
##             as.data.frame
##         dat.inla.g <- inla(Value~Sector*cYear*Zone+
##                              f(Pair, model='iid') +
##                              f(Reef, model='iid') +
##                              f(Site, model='iid'),
##                          data=dat.pred,
##                          family='gamma',
##                          control.family=list(link='log'),
##                          control.predictor = list(compute=TRUE, link=NA),
##                          control.compute = list(config = TRUE,dic = TRUE, cpo = TRUE,
##                                                 waic = TRUE, return.marginals.predictor = TRUE)
##                          )
##         summary(dat.inla.g)
##         ## Get draws
##         draws <- inla.posterior.sample(ndraws, dat.inla.g)
##         draws.attr <- attr(draws, ".content")
##         wch <- which(draws.attr$tag == "(Intercept)")
##         n.i <- draws.attr$start[wch]:draws.attr$start[length(draws.attr$start)]
##         newdata.g <- data %>% dplyr::select(Sector, cYear, Zone) %>% distinct()
##         draws = sapply(draws, function(x) x[['latent']])
##         xmat <- model.matrix(~Sector*cYear*Zone, data = newdata.g)
##         fit = t(draws[n.i,]) %*% t(xmat)
##         ## fortify posteriors
##         fit.g <- newdata.g %>% cbind(t(fit)) %>%
##             pivot_longer(cols = c(-Sector,-cYear,-Zone), values_to = 'Fit.g') %>%
##             group_by(Sector, cYear, Zone) %>%
##             mutate(Fit.g = exp(Fit.g),
##                    Iter = 1:n()) %>%
##             dplyr::select(-name)
##         ## overall effects
##         xmat.effects <- newdata.g %>% cbind(xmat) %>%
##             group_by(Zone) %>%
##             summarise(across(c(-Sector,-cYear),
##                              mean)) %>%
##             ungroup() %>%
##             arrange(desc(Zone)) %>%
##             dplyr::select(-Zone) %>%
##             summarise(across(everything(), diff)) %>%
##             as.matrix()
##         apply(draws[n.i,],1,mean)[1:7]
##         fit.effects.g = t(draws[n.i,]) %*% t(xmat.effects)
##         apply(draws[n.i,],1,mean)[1:12]
##         fit.effects.g %>% head
##         mean(fit.effects.g)
##         100*(mean(exp(fit.effects.g))-1)
##         100*(HDInterval::hdi(exp(fit.effects.g))-1)

##         mean(plogis(fit.effects.b))
##         100*(HDInterval::hdi(plogis(fit.effects.g))-1)

##         ## Combine
##         newdata.comb <- fit.b %>%
##             full_join(fit.g) %>%
##             mutate(Fit=exp(log(Fit.b) + log(Fit.g)))
##         cellmeans <- newdata.comb %>% dplyr::select(Sector, cYear, Zone, Iter, Fit)
##         ## remove those predictions associated with Sector/Year/Zone not monitored
##         cellmeans <- cellmeans %>%
##             full_join(data.mis) %>%
##             mutate(Fit = ifelse(is.na(ValueMis), Fit, NA)) %>%
##             dplyr::select(-ValueMis)
##     mod.cellmeans <- cellmeans %>%
##         group_by(Sector, cYear, Zone) %>%
##         summarise(Median = median(Fit),
##                   lower = HDInterval::hdi(Fit)[1],
##                   upper = HDInterval::hdi(Fit)[2]) %>%
##         ungroup()
## p=MPA_RAPPlot(mod.cellmeans, ytitle=labels[[i]], title=titles[[i]],purpose=purpose)
## ggsave(filename=paste0('figures/',purpose,'/RAPPlot_',i,'_',sec,'_deltaGamma.pdf'), p,width=15, height=5)

##         ## overall effects
##         combine.effects <- data.frame(Eff.b = plogis(fit.effects.b),
##                                       Eff.g = exp(fit.effects.g)) %>%
##             mutate(Eff = exp(log(Eff.b) + log(Eff.g)))


##         combine.effects %>% head
##         ## cellmeans %>%
##         ##     ungroup() %>%
##         ##     group_by(Sector, cYear, Iter) %>%
##         ##     arrange(desc(Zone)) %>%
##         ##     summarise(
##         ##         D = diff(Fit),
##         ##         Diff = exp(diff(log(Fit)))) %>%
##         ##     summary()
##         ##     ungroup() %>%
##         ##     group_by(Iter) %>%
##         ##     summarise(mean(Diff))

    }
   list(model = dat.inla.hg, newdata = newdata, n.2 = n.2, cellmeans = cellmeans, effects = effects)
}

MPA_INLA_biomass_old <- function(data) {
    draws <- 1000
    n.1 = 1:nrow(data) # indices of data
    dd <- expand.grid(Sector=levels(data$Sector),Year=levels(data$cYear),Zone=levels(data$Zone))
    n<-nrow(dd)
    m <- matrix(0,n/2,n)
    for (i in 1:n/2) m[i,i] <- -1
    for (i in 1:n/2) m[i,i+(n/2)] <- 1
    N <- nrow(data)
    LP <-inla.make.lincombs(Predictor=cbind(matrix(0,n/2,N),m ), '(Intercept)'=rep(0,n/2))
    ## LP <- inla.make.lincomb(Predictor = c(0,rep(0,199)), '(Intercept)' = 1)
    ## find out which combinations of Sector/Year/Zone were not surveyed
    data.mis <- data %>% tidyr::expand(Sector,cYear,Zone) %>% anti_join(data) %>% mutate(ValueMis = 1)

    newdata <- cbind(Value=NA,expand.grid(Sector=levels(data$Sector),cYear=levels(data$cYear),Zone=levels(data$Zone)), Pair=NA, Reef=NA,Site=NA)
    dat <- rbind(data %>% dplyr:::select(Value,Sector,cYear,Zone,Pair,Reef,Site), newdata) %>% as.data.frame
    n.2 = (nrow(data)+1):nrow(dat)
    if (length(unique(data$Sector))>1) {
        ## Start with binomial component
        dat.inla.b <- inla(Value~Sector*cYear*Zone+
                             f(Pair, model='iid') +
                             f(Reef, model='iid') +
                             f(Site, model='iid'),
                         data=dat %>% mutate(Value = as.numeric(Value > 0)),
                         family='binomial',
                         ## lincomb = LP,
                         control.family=list(link='logit'),
                         control.predictor = list(compute=TRUE, link=NA),
                         control.compute = list(config = TRUE, dic = TRUE, cpo = TRUE, waic = TRUE, return.marginals.predictor = TRUE)
                         ## control.inla=list(lincomb.derived.only=TRUE, cmin=0))
                         )
        summary(dat.inla.b)
        a <- inla.posterior.sample(1000, dat.inla.b)
        aa = sapply(a, function(x) x[['latent']])
        b <- newdata %>% dplyr::select(-Value,-Pair,-Reef,-Site) %>%
            bind_cols(plogis(aa[n.2,])) %>%
            pivot_longer(cols = c(-Sector,-cYear,-Zone), values_to = 'Fit.b') %>%
            group_by(Sector, cYear, Zone) %>%
            mutate(Iter = 1:n()) %>%
            dplyr::select(-name)

        ## b <- newdata %>%
        ##     mutate(ID = 1:n() + nrow(data)) %>%
        ##     dplyr::select(-Value) %>%
        ##     group_by(Sector,cYear,Zone) %>%
        ##     nest() %>%
        ##     mutate(Marginals = map(.x = data, .f = function(.x) dat.inla.b$marginals.fitted.values[[.x$ID]])) %>%
        ##     mutate(Marginals1 = map(.x = Marginals, .f = function(.x) {
        ##         .x[is.infinite(.x[,2]),2] <- 10000000
        ##         .x
        ##     })) %>%
        ##     mutate(Fit.b=map(.x= Marginals1, .f=function(.x) inla.rmarginal(draws,.x))) %>%
        ##     dplyr::select(-data,-Marginals,-Marginals1) %>%
        ##     unnest(cols=c(Fit.b)) %>%
        ##     mutate(Iter=1:n(), Fit.b=plogis(Fit.b)) %>%
        ##     ungroup()
        ## Now the gamma
        dat.g <- data %>%
            dplyr:::select(Value,Sector,cYear,Zone,Pair,Reef,Site) %>%
            filter(Value != 0) %>%
            droplevels
        dat.pred <- rbind(dat.g,newdata) %>%
            as.data.frame
        dat.inla.g <- inla(Value~Sector*cYear*Zone+
                             f(Pair, model='iid') +
                             f(Reef, model='iid') +
                             f(Site, model='iid'),
                         data=dat.pred,
                         family='gamma',
                         control.family=list(link='log'),
                         control.predictor = list(compute=TRUE, link=NA),
                         control.compute = list(config = TRUE,dic = TRUE, cpo = TRUE, waic = TRUE, return.marginals.predictor = TRUE)
                         ## control.inla=list(lincomb.derived.only=TRUE, cmin=0))
                         )
        summary(dat.inla.g)
        a <- inla.posterior.sample(1000, dat.inla.g)
        aa = sapply(a, function(x) x[['latent']])
        xmat <- model.matrix(~Sector*cYear*Zone, data = newdata)
        fit = t(aa[1942:2139,]) %*% t(xmat)
        g <- newdata %>% dplyr::select(-Value,-Pair,-Reef,-Site) %>%
            bind_cols(exp(aa[n.2,])) %>%
            pivot_longer(cols = c(-Sector,-cYear,-Zone), values_to = 'Fit.g') %>%
            group_by(Sector, cYear, Zone) %>%
            mutate(Iter = 1:n()) %>%
            dplyr::select(-name)
        ## g <- newdata %>%
        ##     mutate(ID=1:n()+nrow(dat.g)) %>%
        ##     dplyr::select(-Value) %>%
        ##     group_by(Sector,cYear,Zone) %>%
        ##     nest() %>%
        ##     mutate(Fit.g=map(.x=data, .f=function(.x) inla.rmarginal(draws,dat.inla.g$marginals.linear.predictor[[.x$ID]]))) %>%
        ##     dplyr::select(-data) %>%
        ##     unnest(cols=c(Fit.g)) %>%
        ##     mutate(Iter=1:n(), Fit.g=exp(Fit.g)) %>%
        ##     ungroup()
        g
        ## Combine together
        comb <- b %>%
            full_join(g) %>%
            mutate(Fit=exp(log(Fit.b) + log(Fit.g)))
        cellmeans <- comb %>% dplyr::select(Sector, cYear, Zone, Iter, Fit)
        ## remove those predictions associated with Sector/Year/Zone not monitored
        cellmeans <- cellmeans %>%
            full_join(data.mis) %>%
            mutate(Fit = ifelse(is.na(ValueMis), Fit, NA)) %>%
            dplyr::select(-ValueMis)
    }
   cellmeans
}

MPA_INLA_cellmeans <- function(mod.cm) {
    mod.cellmeans <- mod.cm %>%
        group_by(Sector, cYear, Zone) %>%
        summarise(Median = median(Fit),
                  lower = HDInterval::hdi(Fit)[1],
                  upper = HDInterval::hdi(Fit)[2]) %>%
        ungroup()
    mod.cellmeans
}

MPA_INLA_zones <- function(mod.cm) {
    mod.d <- mod.cm %>%
        group_by(Sector,cYear,Iter) %>%
        arrange(desc(Zone)) %>%           # order Open first (since diff subtracts first from second)
        summarise(
            ## Diff = diff(Fit),
            Diff = 100*(Fit[2] - Fit[1])/Fit[1])#,
            ## PDiff = 100*(exp(diff(log(Fit)))-1)) %>%
        ungroup()
    sector.year.d <- mod.d %>%
        group_by(Sector, cYear,Iter) %>%
        summarise(Diff = mean(Diff, na.rm = TRUE),
                  PDiff = mean(PDiff, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(Sector,cYear) %>%
        summarise(across(c(Diff,PDiff),
                         list(median = median,
                              lower = ~ HDInterval::hdi(., credMass = 0.8)[1],
                              lower.1 = ~ HDInterval::hdi(.)[1],
                              upper = ~ HDInterval::hdi(., credMass = 0.8)[2],
                              upper.1 = ~ HDInterval::hdi(.)[2]),
                         ,.names = "{.col}_{.fn}"))
    sector.d <- mod.d %>%
        group_by(Sector, Iter) %>%
        summarise(Diff = mean(Diff, na.rm = TRUE),
                  PDiff = mean(PDiff, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(Sector) %>%
        summarise(across(c(Diff,PDiff),
                         list(median = median,
                              lower = ~ HDInterval::hdi(., credMass = 0.8)[1],
                              lower.1 = ~ HDInterval::hdi(.)[1],
                              upper = ~ HDInterval::hdi(., credMass = 0.8)[2],
                              upper.1 = ~ HDInterval::hdi(.)[2]),
                         ,.names = ".{.col}_{.fn}"))

    year.d <- mod.d %>%
        group_by(cYear, Iter) %>%
        summarise(Diff = mean(Diff, na.rm = TRUE),
                  PDiff = mean(PDiff, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(cYear) %>%
        summarise(across(c(Diff,PDiff),
                         list(median = median,
                              lower = ~ HDInterval::hdi(., credMass = 0.8)[1],
                              lower.1 = ~ HDInterval::hdi(.)[1],
                              upper = ~ HDInterval::hdi(., credMass = 0.8)[2],
                              upper.1 = ~ HDInterval::hdi(.)[2]),
                         ,.names = "{.col}_{.fn}"))
    overall.d <- mod.d %>%
        group_by(Iter) %>%
        summarise(Diff = mean(Diff, na.rm = TRUE),
                  PDiff = mean(PDiff, na.rm = TRUE)) %>%
        ungroup() %>%
        summarise(across(c(Diff,PDiff),
                         list(median = median,
                              lower = ~ HDInterval::hdi(., credMass = 0.8)[1],
                              lower.1 = ~ HDInterval::hdi(.)[1],
                              upper = ~ HDInterval::hdi(., credMass = 0.8)[2],
                              upper.1 = ~ HDInterval::hdi(.)[2]),
                         ,.names = "{.col}_{.fn}"))
    list(
        sector.year.effect = sector.year.d,
        sector.effect = sector.d,
        year.effect = year.d,
        overall.effect = overall.d)
}


MPA_inla <- function(data,   # the full data (used to get prediction levels)
                     data.mod,  # the data used to fit the model
                     Y,         # the response
                     inla.form, # the model formula
                     fam='nbinomial', # the family
                     link='log') {    # the link function

    if (grepl('(beta)',fam)) {
        data.mod = data.mod %>% mutate(Value=Value/100)
        Y = data.mod %>% pull(Value)
    }
    if (grepl('(beta|^gamma)',fam)) {
        data.mod = data.mod %>% mutate(Value=ifelse(Value==0,0.01,Value))
        Y = data.mod %>% pull(Value)
    }
    if (grepl('(beta)',fam)) {
        data.mod = data.mod %>% mutate(Value=ifelse(Value==1,0.99,Value))
        Y = data.mod %>% pull(Value)
    }

    n.1 = 1:nrow(data.mod) # indices of data
    ## Define newdata as the unique levels of the current data
    ## this way, unmonitored combinations are never predicted
    newdata <- data %>%
        dplyr::select(Sector, cYear, Zone) %>%
        distinct() %>%
        mutate(Value = NA, Pair = NA, Reef = NA, Site = NA)
    n.2 = 1:nrow(newdata)+nrow(data.mod)

    data.mod <- rbind(data.mod %>%
                      dplyr:::select(Value,Sector,cYear,Zone,Pair,Reef,Site),
                      newdata) %>%
        as.data.frame
    if (!is.null(dim(Y))) { # hurdle gamma model
        Y <- rbind(Y, matrix(NA, nrow = nrow(newdata), ncol = 2))
    } else {
        Y <- c(Y, rep(NA, nrow(newdata)))
    }
    if (length(unique(data$Sector))==1) {
        inla.form <- inla.form %>%
            update(.~CYear*Zone)
    }
    if (fam == 'hurdlegamma') fam = c('binomial','gamma')
    environment(inla.form) <- environment()
    dat.inla <- inla(inla.form,
                     data=data.mod,
                     family=fam,
                     ## control.family=list(link=link),
                     control.predictor = list(compute=TRUE, link=1),
                     control.compute = list(config = TRUE, return.marginals.predictor = TRUE))
    list(dat.inla = dat.inla, newdata = newdata, n.2 = n.2)
}

MPA_inla_old <- function(data,fam='nbinomial',link='log') {
    if (fam=='beta') data = data %>% mutate(Value=Value/100)
    n.1 = 1:nrow(data) # indices of data
    dd <- expand.grid(Sector=levels(data$Sector),Year=levels(data$cYear),Zone=levels(data$Zone))
    n<-nrow(dd)
    m <- matrix(0,n/2,n)
    for (i in 1:n/2) m[i,i] <- -1
    for (i in 1:n/2) m[i,i+(n/2)] <- 1
    N <- nrow(data)
    LP <-inla.make.lincombs(Predictor=cbind(matrix(0,n/2,N),m ), '(Intercept)'=rep(0,n/2))

    #Xmat = model.matrix(~Sector*Year*Zone, data=dd)[1:2,]
    #LP=inla.make.lincomb(as.data.frame(Xmat))
    newdata <- cbind(Value=NA,expand.grid(Sector=levels(data$Sector),cYear=levels(data$cYear),Zone=levels(data$Zone)), Pair=NA, Reef=NA,Site=NA)
    dat <- rbind(data %>% dplyr:::select(Value,Sector,cYear,Zone,Pair,Reef,Site), newdata) %>% as.data.frame
    n.2 = (nrow(data)+1):nrow(dat)
    ## newdata3 <- cbind(Biomass=NA,expand.grid(Sector=levels(data$Sector),Year=NA,Zone=levels(data$Zone)), Pair=NA, Reef=NA,Site=NA)
    ## n.3 = (nrow(dat)+1):(nrow(dat)+nrow(newdata3))
    ## dat <- rbind(dat, newdata3) %>% as.data.frame
    #INLA:::inla.dynload.workaround()
    if (grepl('(beta|gamma)',fam)) dat = dat %>% mutate(Value=ifelse(Value==0,0.01,Value))
    if (grepl('(beta)',fam)) dat = dat %>% mutate(Value=ifelse(Value==1,0.99,Value))
    if (length(unique(data$Sector))>1) {
        dat.inla <- inla(Value~Sector*cYear*Zone+
                             f(Pair, model='iid') + #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))) +
                             f(Reef, model='iid') + #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))) +
                             f(Site, model='iid'), #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))),
                         data=dat,
                                        #control.fixed=list(expand.factor.strategy="inla",mean=0, prec=0.01, mean.intercept=0, prec.intercept=0.01),
                                        #control.family=list(hyper=list(theta1=list(prior='loggamma', param=c(0.01,0.01)),
                                        #                        theta2=list(prior='gaussian', param=c(0,0.01)))),
                                        #control.family=list(hyper=list(theta=list(prior='loggamma', param=c(0.1,0.1)))),
                         family=fam,#'nbinomial',
                         control.family=list(link=link),
                         control.predictor = list(compute=TRUE, link=1),
                         control.compute = list(config = TRUE, return.marginals.predictor = TRUE))
                         ##control.inla=list(lincomb.derived.only=TRUE, cmin=0))#,
                                        #                     lincomb=LP)
    } else {
           dat.inla <- inla(Value~cYear*Zone+
                             f(Pair, model='iid') + #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))) +
                             f(Reef, model='iid') + #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))) +
                             f(Site, model='iid'), #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))),
                         data=dat,
                                        #control.fixed=list(expand.factor.strategy="inla",mean=0, prec=0.01, mean.intercept=0, prec.intercept=0.01),
                                        #control.family=list(hyper=list(theta1=list(prior='loggamma', param=c(0.01,0.01)),
                                        #                        theta2=list(prior='gaussian', param=c(0,0.01)))),
                                        #control.family=list(hyper=list(theta=list(prior='loggamma', param=c(0.1,0.1)))),
                         family=fam,#'nbinomial',
                         control.family=list(link=link),
                         control.predictor = list(compute=TRUE, link=1),
                         control.compute = list(config = TRUE,return.marginals.predictor = TRUE))
                         #control.inla=list(lincomb.derived.only=TRUE))
    }
    list(Inla=dat.inla,N=N,newdata=newdata,n.1=n.1,n.2=n.2)
}

perc <- function(x) {
    100*(exp(x)-1)
}

MPA_inla.cellmeans <- function(dat,model,regs, mult=1) {
    #n1 = nrow(dat)+1
    #n2 = n1 + nrow(model$newdata)-1
    cellmeans = cbind(model[['newdata']],
        model[[1]]$summary.linear.predictor[model[['n.2']],]
                      )
    #mult=1
    if (length(model[[1]]$all.hyper$family) == 2) {
        inv.link = exp
        mult = 1000
    } else {
        if (model[[1]]$all.hyper$family[[1]]$label=='tweedie') inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta2$from.theta
        if (model[[1]]$all.hyper$family[[1]]$label=='zeroinflatednbinomial1') inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta1$from.theta
        if (model[[1]]$all.hyper$family[[1]]$label=='nbinomial') inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta$from.theta
        if (model[[1]]$all.hyper$family[[1]]$label=='nbinomial2') inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta$from.theta
        if (model[[1]]$all.hyper$family[[1]]$label=='gamma') inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta$from.theta
                                        #if (model[[1]]$all.hyper$family[[1]]$label=='zeroinflatedbinomial1') {inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta$from.theta; mult=100;}
        if (model[[1]]$all.hyper$family[[1]]$label=='binomial') {inv.link=binomial()$linkinv; mult=100;}
        if (model[[1]]$all.hyper$family[[1]]$label=='beta') {inv.link=binomial()$linkinv; mult=100;}
                                        #if (model[[1]]$all.hyper$family[[1]]$label=='betabinomial') {inv.link=binomial()$linkinv; mult=100;}
    }

    cellmeans$mean=inv.link(cellmeans$mean) * mult
    cellmeans$Median=inv.link(cellmeans[,'0.5quant']) * mult
    cellmeans$lower = inv.link(cellmeans[,'0.025quant']) * mult
    cellmeans$upper = inv.link(cellmeans[,'0.975quant']) * mult
    cellmeans = cellmeans %>% left_join(regs %>% mutate(Exclude=1))
    wch=which(cellmeans$Exclude==1)
    cellmeans[wch,8:17]=0
    ## find out which combinations of Sector/Year/Zone were not surveyed
    data.mis <- data.mod %>%
        tidyr::expand(Sector,cYear,Zone) %>%
        anti_join(dat) %>%
        mutate(ValueMis = 1)
    cellmeans <- cellmeans %>%
        full_join(data.mis) %>%
        mutate(across(c(-Sector,-cYear,-Zone,-ValueMis),
                      function(x) ifelse(is.na(ValueMis), x, NA)))
    cellmeans_sector.year.zone <- cellmeans

    draws <- inla.posterior.sample(1000, model[[1]])
    draws.attr <- attr(draws, ".content")
    wch <- which(draws.attr$tag == "(Intercept)")
    n.i <- draws.attr$start[wch]:draws.attr$start[length(draws.attr$start)]
    draws = sapply(draws, function(x) x[['latent']])

    ## Percent Effects (Zone) for each Year/Sector
    newdata <- dat %>% dplyr::select(Sector, cYear, Zone) %>% distinct()
    xmat <- model.matrix(~Sector*cYear*Zone, data = newdata)
    ndat <- newdata %>% cbind(xmat) %>%
        group_by(Sector, cYear, Zone) %>%
        summarise(across(everything(),
                         mean)) %>%
        ungroup() %>%
        group_by(Sector, cYear) %>%
        arrange(desc(Zone)) %>%
        dplyr::select(-Zone) %>%
        summarise(across(everything(), diff)) %>%
        ungroup()
    xmat.e <- ndat %>%
        dplyr::select(-Sector, -cYear) %>%
        as.matrix()
    effects <- t(draws[n.i,]) %*% t(xmat.e) %>%
        perc() %>%
        posterior::summarise_draws(
                       Mean = mean,
                       Median = median,
                       "X2.5." = ~ quantile(.x, p=0.025)[[1]],
                       "X10." = ~ quantile(.x, p=0.05)[[1]],
                       "X90." = ~ quantile(.x, p=0.95)[[1]],
                       "X97.5." = ~ quantile(.x, p=0.975)[[1]],
                       P.0 = ~ sum(.x > 0)/length(.x),
                       lower = ~ HDInterval::hdi(.x, credMass = 0.9)[[1]],
                       upper = ~ HDInterval::hdi(.x, credMass = 0.9)[[2]],
                       lower.1 = ~ HDInterval::hdi(.x, credMass = 0.95)[[1]],
                       upper.1 = ~ HDInterval::hdi(.x, credMass = 0.95)[[2]]) %>%
        bind_cols(ndat %>% dplyr::select(Sector, cYear)) %>%
        dplyr::select(Sector,cYear, everything(), -variable)
    effects_sector.year = effects

    ## Percent Effects (Zone) for each Sector
    newdata <- dat %>% dplyr::select(Sector, cYear, Zone) %>% distinct()
    xmat <- model.matrix(~Sector*cYear*Zone, data = newdata)
    ndat <- newdata %>% cbind(xmat) %>%
        dplyr::select(-cYear) %>%
        group_by(Sector, Zone) %>%
        summarise(across(everything(),
                         mean)) %>%
        ungroup() %>%
        group_by(Sector) %>%
        arrange(desc(Zone)) %>%
        dplyr::select(-Zone) %>%
        summarise(across(everything(), diff)) %>%
        ungroup()
    xmat.e <- ndat %>%
        dplyr::select(-Sector) %>%
        as.matrix()
    effects <- t(draws[n.i,]) %*% t(xmat.e) %>%
        perc() %>%
        posterior::summarise_draws(
                       Mean = mean,
                       Median = median,
                       "X2.5." = ~ quantile(.x, p=0.025)[[1]],
                       "X10." = ~ quantile(.x, p=0.05)[[1]],
                       "X90." = ~ quantile(.x, p=0.95)[[1]],
                       "X97.5." = ~ quantile(.x, p=0.975)[[1]],
                       P.0 = ~ sum(.x > 0)/length(.x),
                       lower = ~ HDInterval::hdi(.x, credMass = 0.9)[[1]],
                       upper = ~ HDInterval::hdi(.x, credMass = 0.9)[[2]],
                       lower.1 = ~ HDInterval::hdi(.x, credMass = 0.95)[[1]],
                       upper.1 = ~ HDInterval::hdi(.x, credMass = 0.95)[[2]]) %>%
        bind_cols(ndat %>% dplyr::select(Sector)) %>%
        dplyr::select(Sector, everything(), -variable)
    effects_sector <- effects

    ## Percent Effects (Zone) for each Year
    newdata <- dat %>% dplyr::select(Sector, cYear, Zone) %>% distinct()
    xmat <- model.matrix(~Sector*cYear*Zone, data = newdata)
    ndat <- newdata %>% cbind(xmat) %>%
        dplyr::select(-Sector) %>%
        group_by(cYear, Zone) %>%
        summarise(across(everything(),
                         mean)) %>%
        ungroup() %>%
        group_by(cYear) %>%
        arrange(desc(Zone)) %>%
        dplyr::select(-Zone) %>%
        summarise(across(everything(), diff)) %>%
        ungroup()
    xmat.e <- ndat %>%
        dplyr::select(-cYear) %>%
        as.matrix()
    effects <- t(draws[n.i,]) %*% t(xmat.e) %>%
        perc() %>%
        posterior::summarise_draws(
                       Mean = mean,
                       Median = median,
                       "X2.5." = ~ quantile(.x, p=0.025)[[1]],
                       "X10." = ~ quantile(.x, p=0.05)[[1]],
                       "X90." = ~ quantile(.x, p=0.95)[[1]],
                       "X97.5." = ~ quantile(.x, p=0.975)[[1]],
                       P.0 = ~ sum(.x > 0)/length(.x),
                       lower = ~ HDInterval::hdi(.x, credMass = 0.9)[[1]],
                       upper = ~ HDInterval::hdi(.x, credMass = 0.9)[[2]],
                       lower.1 = ~ HDInterval::hdi(.x, credMass = 0.95)[[1]],
                       upper.1 = ~ HDInterval::hdi(.x, credMass = 0.95)[[2]]) %>%
        bind_cols(ndat %>% dplyr::select(cYear)) %>%
        dplyr::select(cYear, everything(), -variable)
    effects_year <- effects

    ## Percent Effects (Zone) overall
    newdata <- dat %>% dplyr::select(Sector, cYear, Zone) %>% distinct()
    xmat <- model.matrix(~Sector*cYear*Zone, data = newdata)
    ndat <- newdata %>% cbind(xmat) %>%
        dplyr::select(-Sector,-cYear) %>%
        group_by(Zone) %>%
        summarise(across(everything(),
                         mean)) %>%
        ungroup() %>%
        ## group_by(Sector) %>%
        arrange(desc(Zone)) %>%
        dplyr::select(-Zone) %>%
        summarise(across(everything(), diff)) %>%
        ungroup()
    xmat.e <- ndat %>%
        as.matrix()
    effects <- t(draws[n.i,]) %*% t(xmat.e) %>%
        perc() %>%
        posterior::summarise_draws(
                       Mean = mean,
                       Median = median,
                       "X2.5." = ~ quantile(.x, p=0.025)[[1]],
                       "X10." = ~ quantile(.x, p=0.05)[[1]],
                       "X90." = ~ quantile(.x, p=0.95)[[1]],
                       "X97.5." = ~ quantile(.x, p=0.975)[[1]],
                       P.0 = ~ sum(.x > 0)/length(.x),
                       lower = ~ HDInterval::hdi(.x, credMass = 0.9)[[1]],
                       upper = ~ HDInterval::hdi(.x, credMass = 0.9)[[2]],
                       lower.1 = ~ HDInterval::hdi(.x, credMass = 0.95)[[1]],
                       upper.1 = ~ HDInterval::hdi(.x, credMass = 0.95)[[2]]) %>%
        dplyr::select(everything(), -variable)
    effects_overall <- effects

    ## aa=inv.link(t(plyr:::ldply(model[[1]]$marginals.linear.predictor[model[['n.2']]], function(x) {
    ##     inla.rmarginal(10000,x)
    ## })[,-1]))*mult
    ## ndat.p=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone)) #%>% arrange(Year,Zone)
    ## #nn=expand.grid(Sector=levels(dat$Sector),Year=levels(dat$Year))
    ## ndat.p = as.matrix((dd=(cbind(ndat.p,t(aa)) %>% arrange(Sector,cYear)))[,-1:-3])
    ## s1=ndat.p[seq(1,nrow(ndat.p),by=2),]; s2=ndat.p[seq(2,nrow(ndat.p),by=2),];

    ## #ndat.p = cbind(dd[,1:3],plyr:::adply(100*(s1-s2)/s2, 1, function(x) {
    ## #    data.frame(Mean=mean(x),Median=median(x),t(quantile(x,p=c(0.025,0.975))),'p>0'=length(x[x>0])/length(x))
    ## #}))
    ## ndat.p = cbind(dd[seq(1,nrow(dd),by=2),1:2],plyr:::adply(100*(s1-s2)/s2, 1, function(x) {
    ##     data.frame(Mean=mean(x),Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE)),'p>0'=length(x[x>0])/length(x))
    ## }))
    ## ## ndat = cbind(ndat,t(aa)) %>% group_by(Sector,Year) %>% do({
    ## ##     x=.
    ## ##     xx=as.matrix((x[1,-1:-3]-x[2,-1:-3])/x[2,-1:-3])
    ## ##     data.frame(Mean=mean(xx), Median=median(xx),t(quantile(xx,p=c(0.025,0.975))),
    ## ##                    'p>0'=length(xx[xx>0])/length(x))
    ## ## })  %>% dplyr:::rename(lower=X2.5., upper=X97.5.)
    ## ndat=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    ## if (length(unique(ndat$Sector))>1) Xmat = model.matrix(~-1+Sector:cYear:Zone, data=ndat)
    ## if (length(unique(ndat$Sector))==1) Xmat = model.matrix(~-1+cYear:Zone, data=ndat)
    ## Xmat=as.matrix(cbind(ndat,Xmat) %>% dplyr:::select(-Zone) %>% group_by(cYear,Sector) %>% do({
    ##     x=.
    ##     data.frame(x[1,-1:-2]-x[2,-1:-2])
    ## }) %>% ungroup %>% dplyr:::select(-Sector,-cYear))
    ## ndat=ndat %>% ungroup %>% dplyr:::select(-Zone) %>% distinct %>% bind_cols(
    ##     plyr:::adply(aa %*% t(Xmat),2,function(x) {
    ##         data.frame(Mean=mean(x), Median=median(x, na.rm=TRUE),t(quantile(x,p=c(0.025,0.975), na.rm=TRUE)),
    ##                    'p>0'=length(x[x>0])/length(x))
    ##     })
    ## ) %>% dplyr:::rename(lower=X2.5., upper=X97.5.)

    ## ## Sector cell means
    ## ndat.sector=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    ## if (length(unique(ndat.sector$Sector))>1) Xmat = model.matrix(~-1+Sector:cYear:Zone, data=ndat.sector)
    ## if (length(unique(ndat.sector$Sector))==1) Xmat = model.matrix(~-1+cYear:Zone, data=ndat.sector)
    ## Xmat=cbind(ndat.sector,Xmat) %>% dplyr:::select(-cYear) %>% group_by(Zone,Sector) %>% summarize_all(funs(mean)) %>% ungroup
    ## cellmeans.sector=Xmat %>% dplyr:::select(Sector,Zone)
    ## Xmat=as.matrix(Xmat %>% dplyr:::select(-Sector,-Zone))
    ## cellmeans.sector=cbind(cellmeans.sector,
    ##     plyr:::adply((aa %*% t(Xmat)),2,function(x) {
    ##         data.frame(Mean=mean(x), Median=median(x, na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE)),t(quantile(x,p=c(0.25,0.75), na.rm=TRUE))
    ##                    )
    ##     })
    ## )%>% dplyr:::rename(lower=X2.5., upper=X97.5., lower.1=X25., upper.1=X75.)
    ## ## cellmeans.sector = cbind(model[['newdata3']],
    ## ##     model[[1]]$summary.linear.predictor[model[['n.3']],]
    ## ##                          )

    ## ##Sector effect sizes
    ## aa.sector = aa %*% t(Xmat)
    ## ndat.p.sector=expand.grid(Sector=levels(dat$Sector),Zone=levels(dat$Zone))
    ## ndat.p.sector = as.matrix((dd=(cbind(ndat.p.sector,t(aa.sector)) %>% arrange(Sector)))[,-1:-2])
    ## s1=ndat.p.sector[seq(1,nrow(ndat.p.sector),by=2),]; s2=ndat.p.sector[seq(2,nrow(ndat.p.sector),by=2),];
    ## ndat.p.sector = cbind(dd[,1:2],plyr:::adply(100*(s1-s2)/s2, 1, function(x) {
    ##     data.frame(Mean=mean(x),Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE)),'p>0'=length(x[x>0])/length(x))
    ## }))


    ## ## ndat.sector=expand.grid(Sector=levels(dat$Sector),Zone=levels(dat$Zone))
    ## ## ndat.sector = cbind(ndat.sector,t(aa.sector)) %>% group_by(Sector) %>% do({
    ## ##     x=.
    ## ##     xx=as.matrix((x[1,-1:-2]-x[2,-1:-2])/x[2,-1:-2])
    ## ##     data.frame(Mean=mean(xx), Median=median(xx),t(quantile(xx,p=c(0.025,0.975))),
    ## ##                    'p>0'=length(xx[xx>0])/length(x))
    ## ## })  %>% dplyr:::rename(lower=X2.5., upper=X97.5.)
    ## ndat.sector=expand.grid(Sector=levels(dat$Sector),Zone=levels(dat$Zone))
    ## if (length(unique(ndat.sector$Sector))>1) Xmat = model.matrix(~-1+Sector:Zone, data=ndat.sector)
    ## if (length(unique(ndat.sector$Sector))==1) Xmat = model.matrix(~-1+Zone, data=ndat.sector)
    ## Xmat=as.matrix(cbind(ndat.sector,Xmat) %>% dplyr:::select(-Zone) %>% group_by(Sector) %>% do({
    ##     x=.
    ##     data.frame(x[1,-1]-x[2,-1])
    ## }) %>% ungroup %>% dplyr:::select(-Sector))
    ## ndat.sector=ndat.sector %>% ungroup %>% dplyr:::select(-Zone) %>% distinct %>% bind_cols(
    ##     plyr:::adply(aa.sector %*% t(Xmat),2,function(x) {
    ##         data.frame(Mean=mean(x), Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE)),
    ##                    'p>0'=length(x[x>0])/length(x))
    ##     })
    ## ) %>% dplyr:::rename(lower=X2.5., upper=X97.5.)

    ## #Overall Zone means
    ## ndat.zone=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    ## if (length(unique(ndat.zone$Sector))>1) Xmat = model.matrix(~-1+Sector:cYear:Zone, data=ndat.zone)
    ## if (length(unique(ndat.zone$Sector))==1) Xmat = model.matrix(~-1+cYear:Zone, data=ndat.zone)
    ## Xmat=cbind(ndat.zone,Xmat) %>% dplyr:::select(-cYear,-Sector) %>% group_by(Zone) %>% summarize_all(funs(mean)) %>% ungroup
    ## cellmeans.zone=Xmat %>% dplyr:::select(Zone)
    ## Xmat=as.matrix(Xmat %>% dplyr:::select(-Zone))
    ## cellmeans.zone=cbind(cellmeans.zone,
    ##     plyr:::adply(aa %*% t(Xmat),2,function(x) {
    ##         data.frame(Mean=mean(x), Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE))
    ##                    )
    ##     })
    ## )%>% dplyr:::rename(lower=X2.5., upper=X97.5.)

    ## ##Zone effect sizes
    ## aa.zone = aa %*% t(Xmat)
    ## ndat.p.zone=expand.grid(Zone=levels(dat$Zone))
    ## ndat.p.zone = as.matrix((dd=(cbind(ndat.p.zone,t(aa.zone))))[,-1])
    ## s1=ndat.p.zone[seq(1,nrow(ndat.p.zone),by=2),]; s2=ndat.p.zone[seq(2,nrow(ndat.p.zone),by=2),];
    ## x=100*(s1-s2)/s2
    ## ndat.p.zone = data.frame(Mean=mean(x,na.rm=TRUE),Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.1,0.9,0.975),na.rm=TRUE)),'p>0'=length(x[x>0])/length(x))

    ## ## ndat.zone=expand.grid(Zone=levels(dat$Zone))
    ## ## ndat.zone = cbind(ndat.zone,t(aa.zone)) %>% do({
    ## ##     x=.
    ## ##     xx=as.matrix((x[1,-1]-x[2,-1])/x[2,-1])
    ## ##     data.frame(Mean=mean(xx), Median=median(xx),t(quantile(xx,p=c(0.025,0.975))),
    ## ##                    'p>0'=length(xx[xx>0])/length(x))
    ## ## })  %>% dplyr:::rename(lower=X2.5., upper=X97.5.)
    ## ndat.zone=expand.grid(Zone=levels(dat$Zone))
    ## Xmat = model.matrix(~-1+Zone, data=ndat.zone)
    ## Xmat=as.matrix(cbind(ndat.zone,Xmat) %>% dplyr:::select(-Zone) %>% do({
    ##     x=.
    ##     data.frame(x[1,]-x[2,])
    ## }) %>% ungroup)
    ## ndat.zone=plyr:::adply(aa.zone %*% t(Xmat),2,function(x) {
    ##         data.frame(Mean=mean(x,na.rm=TRUE), Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE)),
    ##                    'p>0'=length(x[x>0])/length(x))
    ##     }) %>% dplyr:::rename(lower=X2.5., upper=X97.5.)

    ## ## Zone by year cellmeans
    ## ndat.zoneyear=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    ## if (length(unique(ndat.zoneyear$Sector))>1) Xmat = model.matrix(~-1+Sector:cYear:Zone, data=ndat.zoneyear)
    ## if (length(unique(ndat.zoneyear$Sector))==1) Xmat = model.matrix(~-1+cYear:Zone, data=ndat.zoneyear)
    ## Xmat=cbind(ndat.zoneyear,Xmat) %>% dplyr:::select(-Sector) %>% group_by(Zone,cYear) %>% summarize_all(funs(mean)) %>% ungroup
    ## cellmeans.zoneyear=Xmat %>% dplyr:::select(cYear,Zone)
    ## Xmat=as.matrix(Xmat %>% dplyr:::select(-cYear,-Zone))
    ## cellmeans.zoneyear=cbind(cellmeans.zoneyear,
    ##     plyr:::adply(aa %*% t(Xmat),2,function(x) {
    ##         data.frame(Mean=mean(x,na.rm=TRUE), Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE))
    ##                    )
    ##     })
    ##     )%>% dplyr:::rename(lower=X2.5., upper=X97.5.)

    ## ## Zone by year effect sizes
    ## aa.zoneyear = aa %*% t(Xmat)
    ## ndat.p.zoneyear=expand.grid(cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    ## ndat.p.zoneyear = as.matrix((dd=(cbind(ndat.p.zoneyear,t(aa.zoneyear)) %>% arrange(cYear)))[,-1:-2])
    ## s1=ndat.p.zoneyear[seq(1,nrow(ndat.p.zoneyear),by=2),]; s2=ndat.p.zoneyear[seq(2,nrow(ndat.p.zoneyear),by=2),];
    ## #s1=ndat.p.zoneyear[1:(nrow(ndat.p.zoneyear)/2),]; s2=ndat.p.zoneyear[(1+nrow(ndat.p.zoneyear)/2):nrow(ndat.p.zoneyear),];
    ## ndat.p.zoneyear = cbind(dd[,1:2] %>% dplyr::select(cYear) %>% distinct,plyr:::adply(100*(s1-s2)/s2, 1, function(x) {
    ##     data.frame(Mean=mean(x,na.rm=TRUE),Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE)),'p>0'=length(x[x>0])/length(x))
    ## }))

    ## ndat.zoneyear=expand.grid(cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    ## if (length(unique(ndat.zoneyear$cYear))>1) Xmat = model.matrix(~-1+cYear:Zone, data=ndat.zoneyear)
    ## if (length(unique(ndat.zoneyear$cYear))==1) Xmat = model.matrix(~-1+Zone, data=ndat.zoneyear)
    ## Xmat=as.matrix(cbind(ndat.zoneyear,Xmat) %>% dplyr:::select(-Zone) %>% group_by(cYear) %>% do({
    ##     x=.
    ##     data.frame(x[1,-1]-x[2,-1])
    ## }) %>% ungroup %>% dplyr:::select(-cYear))
    ## ndat.zoneyear=ndat.zoneyear %>% ungroup %>% dplyr:::select(-Zone) %>% distinct %>% bind_cols(
    ##     plyr:::adply(aa.zoneyear %*% t(Xmat),2,function(x) {
    ##         data.frame(Mean=mean(x,na.rm=TRUE), Median=median(x,na.rm=TRUE),t(quantile(x,p=c(0.025,0.975),na.rm=TRUE)),
    ##                    'p>0'=length(x[x>0])/length(x))
    ##     })
    ## ) %>% dplyr:::rename(lower=X2.5., upper=X97.5.)


    ## list(cellmeans=cellmeans,ndat=ndat,ndat.p=ndat.p,
    ##      cellmeans.sector=cellmeans.sector, ndat.sector=ndat.sector,ndat.p.sector=ndat.p.sector,
    ##      cellmeans.zone=cellmeans.zone,ndat.zone=ndat.zone,ndat.p.zone=ndat.p.zone,
    ##      cellmeans.zoneyear=cellmeans.zoneyear, ndat.zoneyear=ndat.zoneyear, ndat.p.zoneyear = ndat.p.zoneyear)
    list(cellmeans_sector.year.zone = cellmeans_sector.year.zone,
         effects_sector.year = effects_sector.year,
         effects_sector = effects_sector,
         effects_year = effects_year,
         effects_overall = effects_overall
         )
}


MPA_sectorPlot <- function(dat, ytitle) {
    p <-ggplot(dat, aes(y=Median, x=Sector, fill=Zone, shape=Zone)) +
        #geom_line()+
        geom_errorbar(aes(ymin=lower, ymax=upper),width=0.01, linetype=1,position=position_dodge(width=0.1))+
        geom_linerange(aes(ymin=lower.1, ymax=upper.1), size=1, position=position_dodge(width=0.1)) +
            geom_point(position=position_dodge(width=0.1))+
                scale_fill_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c('black','white'))+
                    scale_shape_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(21,21))+
                        scale_linetype_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(1,2))+
                            #scale_y_continuous(expression(paste(Trout~biomass~"(per 1000", m^2, ")", sep="")), labels=comma)+
                                scale_y_continuous(ytitle, labels=comma)+
                                        #scale_x_continuous('', breaks=2006:2014)+
                                theme_classic()
    p<-p+theme(legend.position=c(1,1), legend.justification=c(1,1), strip.background=element_blank(),strip.text.x=element_text(size=12),
               axis.title.y=element_text(vjust=1.5), panel.spacing=unit(1,unit='lines'), axis.text.x=element_text(size=10),
               axis.line.y=element_line(), axis.line.x=element_line())
    print(p)
}

MPA_gam <- function(dat, family, link) {
    library(mgcv)
    if (grepl('*.link=logit.*',family)) {dat1 = dat %>% mutate(Value=Value/100)
    } else if (grepl('.*negbin.*',family)) {dat1 = dat %>% mutate(Value=as.integer(Value/100))
    } else { dat1 =dat %>% mutate(Value=Value/1000)
    }
    lv = levels(dat1$cYear)
    dat1 = dat1 %>% mutate(cYear=factor(cYear, levels=lv[c(2,1,3:length(lv))]))
    if (length(unique(dat1$Sector))>1) form = Value~Sector*cYear*Zone+s(Pair,bs='re') + s(Reef,bs='re') + s(Site,bs='re')
    if (length(unique(dat1$Sector))==1) form = Value~cYear*Zone+s(Pair,bs='re') + s(Reef,bs='re') + s(Site,bs='re')

    mod = gam(form,
              data=dat1,
              family=family,
              method='REML'
              )
    detach(package:mgcv)
    mod
}

MPA_cellmeans_gam <- function(dat.gam) {
    coefs = coef(dat.gam)
    wch=grep('^s\\(.*', names(coefs))
    coefs = coefs[-wch]
    vc = vcov(dat.gam)[-wch,-wch]
    data = dat.gam$model

    ##Cellmeans
    dat <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
    if (length(unique(dat$Sector))>1) dat <- cbind(dat, model.matrix(~Sector*cYear*Zone, data=dat))
    if (length(unique(dat$Sector))==1) dat <- cbind(dat, model.matrix(~cYear*Zone, data=dat))
    #xmat<- as.matrix(plyr::ddply(dat,~Sector*cYear*Zone,plyr::colwise(mean))[,-1:-3])
    xmat = dat %>% group_by(Sector,cYear,Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-cYear,-Zone) %>% as.matrix
    if (dat.gam$family$link=='log') invlink<-exp
    if (dat.gam$family$link=='logit') invlink<-invlogit
    if (dat.gam$family$link=='identity') invlink<-I
    fit = as.vector(coefs %*% t(xmat))
    SE = sqrt(diag(xmat %*% vc %*% t(xmat)))
    Q = qt(0.975, df=dat.gam$df.residual)
    dat.cellmeans = dat %>% mutate(Median=invlink(fit),
                                   lower=invlink(fit-Q*SE),
                                   upper=invlink(fit+Q*SE)
                                   ) %>%
        dplyr::select(Sector, Zone, Year=cYear, Median, lower, upper)
    dat.cellmeans
    dat.cellmeans %>%
        ggplot(aes(y=Median, x=Year, color=Zone)) +
        geom_pointrange(aes(ymin=lower, ymax=upper)) +
        geom_line(aes(linetype=Zone, x=as.numeric(Year))) +
        facet_grid(~Sector)
}


MPA_makePriors <- function(cellmeans, data, link='log') {
    link=eval(parse(text=link))
    priors=list()
                                        #priors$intercept=c(mu=log(cellmeans[1,'Mean']), sd=log(2*cellmeans[1,'upper']-cellmeans[1,'lower']))
    #priors$intercept=c(mu=round(log(median(data$Value)),2), sd=round(log(sd(data$Value)),2))
    priors$intercept=c(round(link(median(data$Value, na.rm=TRUE)),2), abs(round(link(sd(data$Value,na.rm=TRUE)),2)))
    if (any(is.infinite(priors$intercept))) priors$intercept[1] <- 0

    if (length(unique(data$Sector))>1) Xmat = model.matrix(~Sector*cYear*Zone, data=cellmeans)
    if (length(unique(data$Sector))==1) Xmat = model.matrix(~cYear*Zone, data=cellmeans)
    coefs = cellmeans$Mean
    #b<-solve(t(X)%*%X)%*%t(X)%*%y
    #OR
    b<-solve(crossprod(Xmat), crossprod(Xmat,coefs))
    priors$b = c(0, abs(round((sd(b[-1])),2)))
    priors
}


MPA_stan <- function(dat, cellmeans,family='zero_inflated_negbinomial') {

    if (grepl('*.link=logit.*',family)) {dat1 = dat %>% mutate(Value=Value/100)
    }else if (grepl('^Gamma.*',family)) {dat1 = dat %>% mutate(Value=Value)
    }else dat1 = dat %>% mutate(Value=as.integer(Value))

    if (grepl('Beta',family)) dat1 = dat1 %>% mutate(Value=ifelse(Value==0,0.01,Value))

    link=gsub('.*link=(.*)\\)','\\1',family)
    priors = MPA_makePriors(cellmeans,data=dat1, link=link)

    prior =  prior=c(prior_string(paste0("normal(",paste(priors$intercept, collapse=','),")"), class='Intercept'),
                     prior_string(paste0("normal(",paste(priors$b, collapse=','),")"), class='b'),
                     prior(cauchy(0,5), class='sd')
                     )
    if (grepl('(^zero_inflated_negbinomial.*|^negbinomial)',family)) {
        prior = c(prior,
                  prior(gamma(0.01,0.01), class='shape')
                  )
    }
    if (grepl('^zero_inflated_negbinomial.*',family)) {
        prior=c(prior,prior(beta(1,1), class='zi'))
    }
    if (length(unique(dat1$Sector))>1) form = Value~Sector*cYear*Zone+(1|Pair) + (1|Reef) + (1|Site)
    if (length(unique(dat1$Sector))==1) form = Value~cYear*Zone+(1|Pair) + (1|Reef) + (1|Site)
    print(form)
    if (grepl('^Beta.*',family)) {
        dat.stan = brm(form, data=dat1,
                       family=Beta(link='logit'), iter = 2000, warmup = 1000, thin=3, chains=3,
                       prior=prior)
    } else if (grepl('^Gamma.*',family)) {
                dat.stan = brm(form, data=dat1,
                       family=Gamma(link='log'), iter = 2000, warmup = 1000, thin=3, chains=3,
                       prior=prior)
    } else if (grepl('^zero_inflated_negbinomial.*',family)) {
       dat.stan = brm(form, data=dat1,
                       family='zero_inflated_negbinomial', iter = 2000, warmup = 1000, thin=3, chains=3,
                       prior=prior)
    } else {
        dat.stan = brm(form, data=dat1,
                       family='negbinomial', iter = 2000, warmup = 1000, thin=3, chains=3,
                       prior=prior)
    }

    dat.stan
}

MPA_cellmeans_stan <- function(dat.stan) {
    coefs = fixef(dat.stan, summary=FALSE)
    ##Cellmeans
    dat <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
    if (length(unique(dat$Sector))>1) dat <- cbind(dat, model.matrix(~Sector*cYear*Zone, data=dat))
    if (length(unique(dat$Sector))==1) dat <- cbind(dat, model.matrix(~cYear*Zone, data=dat))
    #xmat<- as.matrix(plyr::ddply(dat,~Sector*cYear*Zone,plyr::colwise(mean))[,-1:-3])
    xmat = dat %>% group_by(Sector,cYear,Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-cYear,-Zone) %>% as.matrix
    if (dat.stan$family$link=='log') cellmeans.mcmc<-exp(coefs %*% t(xmat))
    if (dat.stan$family$link=='logit') cellmeans.mcmc<-invlogit(coefs %*% t(xmat))*100
    if (dat.stan$family$link=='identity') cellmeans.mcmc<-coefs %*% t(xmat)

    #if(trans=='exp') cellmeans.mcmc<-exp(coefs %*% t(xmat))
    #if(trans=='logit') cellmeans.mcmc<-invlogit(coefs %*% t(xmat))
    #if(trans=='gaussian') cellmeans.mcmc<-(coefs %*% t(xmat))
    dat.cellmeans <- plyr::adply(cellmeans.mcmc,2,function(x){
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
                   t(ci(x)))
    })

    #aa<-plyr::ddply(dat,~Sector+cYear+Zone, plyr::numcolwise(mean))
    aa = dat %>% group_by(Sector,cYear,Zone) %>% summarize_if(is.numeric, mean) %>% as.data.frame

    dat <- cbind(aa[,1:3],dat.cellmeans)
    if (length(unique(dat$Sector)) > 1) dat$Sector <- factor(dat$Sector, labels=c('Cairns','Townsville','Pompeys','Swains','Cap-Bunkers'))
    dat$dcYear <- as.Date(paste(dat$cYear,'-01-01',sep=''))#as.numeric(as.character(dat$cYear))

    ## Closed higher than open
    co.mcmc <- cellmeans.mcmc[,seq(1,ncol(cellmeans.mcmc),by=2)] - cellmeans.mcmc[,seq(2,ncol(cellmeans.mcmc),by=2)]
    co.dat <- apply(co.mcmc,2,function(x) {
        length(x[x>0])/length(x)
    })
    co.dat <- cbind(expand.grid(cYear=seq(min(year(dat$dcYear)),max(year(dat$dcYear)),by=2),Sector=c("CAIN","TO","PO","SW","CB")), Prob=co.dat)

    ## Effects sizes
    co.effect <- plyr::adply(co.mcmc,2,function(x){data.frame(Mean=mean(x), HPDinterval(as.mcmc(x)),t(quantile(x,p=c(0.025,0.975))))})
    co.effect <- cbind(expand.grid(cYear=seq(min(year(dat$dcYear)),max(year(dat$dcYear)),by=2),Sector=c("CAIN","TO","PO","SW","CB")), co.effect)

    ## Means per Sector
    dat.s <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
    if (length(unique(dat.s$Sector)) > 1) dat.s <- cbind(dat.s, model.matrix(~Sector*cYear*Zone, data=dat.s))
    if (length(unique(dat.s$Sector))== 1) dat.s <- cbind(dat.s, model.matrix(~cYear*Zone, data=dat.s))
    #xmat<- as.matrix(plyr::ddply(dat.s,~Sector*Zone,plyr::colwise(mean))[,-1:-3])
    xmat = dat.s %>% group_by(Sector,Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-Zone,-cYear) %>% as.matrix
    #if(trans=='exp') sector.cellmeans.mcmc<-exp(coefs %*% t(as.matrix(xmat)))
    #if(trans=='logit') sector.cellmeans.mcmc<-invlogit(coefs %*% t(as.matrix(xmat)))
                                        #if(trans=='gaussian') sector.cellmeans.mcmc<-(coefs %*% t(as.matrix(xmat))
    if (dat.stan$family$link=='log') sector.cellmeans.mcmc<-exp(coefs %*% t(as.matrix(xmat)))
    if (dat.stan$family$link=='logit')sector.cellmeans.mcmc<-invlogit(coefs %*% t(as.matrix(xmat)))*100
    if (dat.stan$family$link=='identity')sector.cellmeans.mcmc<-(coefs %*% t(as.matrix(xmat)))
    sector.cellmeans <- plyr::adply(sector.cellmeans.mcmc,2,function(x){
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
                   t(ci(x)))
    })
    facts <- expand.grid(Zone=levels(data$Zone),Sector=levels(data$Sector))
    dat.s <- cbind(facts,sector.cellmeans)

    ## Effects per sector
    if (length(unique(dat.s$Sector))>1) {
        co.mcmc.s <- sector.cellmeans.mcmc[,c(1,3,5,7,9)] - sector.cellmeans.mcmc[,c(2,4,6,8,10)]
        co.effect.s <- plyr::adply(co.mcmc.s,2,function(x){data.frame(Mean=mean(x), HPDinterval(as.mcmc(x)),t(quantile(x,p=c(0.025,0.975))))})
        co.effect.s <- cbind(expand.grid(Sector=c("CAIN","TO","PO","SW","CB")), co.effect.s)

    ## Probabilities Closed higher than open per sector
    co.dat.s <- apply(co.mcmc.s,2,function(x) {
        length(x[x>0])/length(x)
    })
    co.dat.s <- cbind(expand.grid(Sector=c("CAIN","TO","PO","SW","CB")), Prob=co.dat.s)
    } else {
        co.dat.s = NULL
        co.effect.s=NULL
    }

    ## Overall closed higher than open probabilities
    co.mcmc1 <- as.vector(cellmeans.mcmc[,seq(1,ncol(cellmeans.mcmc),by=2)]) - as.vector(cellmeans.mcmc[,seq(2,ncol(cellmeans.mcmc),by=2)])
    co <- length(co.mcmc1[co.mcmc1>0])/length(co.mcmc1)

    ## Overall means
    dat.o <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
    if (length(unique(dat.o$Sector))>1) dat.o <- cbind(dat.o, model.matrix(~Sector*cYear*Zone, data=dat.o))
    if (length(unique(dat.o$Sector))==1) dat.o <- cbind(dat.o, model.matrix(~cYear*Zone, data=dat.o))
    #xmat<- as.matrix(plyr::ddply(dat.o,~Zone,plyr::colwise(mean))[,-1:-3])
    xmat = dat.o %>% group_by(Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-Zone,-cYear) %>% as.matrix
    if(dat.stan$family$link=='log') overall.cellmeans.mcmc<-exp(coefs %*% t(as.matrix(xmat)))
    if(dat.stan$family$link=='logit') overall.cellmeans.mcmc<-invlogit(coefs %*% t(as.matrix(xmat)))*100
    if(dat.stan$family$link=='identity') overall.cellmeans.mcmc<-(coefs %*% t(as.matrix(xmat)))
    ## overall.cellmeans <- adply(overall.cellmeans.mcmc,2,function(x){
    ##     data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
    ##                t(ci(as.mcmc(x))))
    ## })
    overall.cellmeans <- plyr::adply(overall.cellmeans.mcmc,2,function(x){
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
                   t(ci(x)))
    })
    facts.o <- expand.grid(Zone=levels(data$Zone))
    dat.o <- cbind(facts.o,overall.cellmeans)

    ## Overall effects
    MCMCsum <- function(x) {
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.5),t(quantile(x,p=c(0.025,0.975))))
    }
    o.mcmc <- overall.cellmeans.mcmc[,c(1)] - overall.cellmeans.mcmc[,c(2)]
    o.effect <-MCMCsum(o.mcmc)
    o.effect <- rbind(o.effect, MCMCsum(overall.cellmeans.mcmc[,c(1)] - overall.cellmeans.mcmc[,c(2)])/(overall.cellmeans.mcmc[,c(2)]))
    rownames(o.effect) <- c('Difference', 'Percent difference')

    ## Zone by year cellmeans
    dat.zy <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
    if (length(unique(dat.zy$cYear)) > 1) dat.zy <- cbind(dat.zy, model.matrix(~Sector*cYear*Zone, data=dat.zy))
    if (length(unique(dat.zy$cYear))== 1) dat.zy <- cbind(dat.zy, model.matrix(~cYear*Zone, data=dat.zy))
    #xmat<- as.matrix(plyr::ddply(dat.s,~cYear*Zone,plyr::colwise(mean))[,-1:-3])
    xmat = dat.zy %>% group_by(cYear,Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-Zone,-cYear) %>% as.matrix
    #if(trans=='exp') sector.cellmeans.mcmc<-exp(coefs %*% t(as.matrix(xmat)))
    #if(trans=='logit') sector.cellmeans.mcmc<-invlogit(coefs %*% t(as.matrix(xmat)))
                                        #if(trans=='gaussian') sector.cellmeans.mcmc<-(coefs %*% t(as.matrix(xmat))
    if (dat.stan$family$link=='log') zoneyear.cellmeans.mcmc<-exp(coefs %*% t(as.matrix(xmat)))
    if (dat.stan$family$link=='logit') zoneyear.cellmeans.mcmc<-invlogit(coefs %*% t(as.matrix(xmat)))*100
    if (dat.stan$family$link=='identity') zoneyear.cellmeans.mcmc<-(coefs %*% t(as.matrix(xmat)))
    zoneyear.cellmeans <- plyr::adply(zoneyear.cellmeans.mcmc,2,function(x){
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
                   t(ci(x)))
    })
    facts <- expand.grid(Zone=levels(data$Zone),cYear=levels(data$cYear))
    dat.zy <- cbind(facts,zoneyear.cellmeans)

    ## Effects per zoneyear
    co.mcmc.zy <- zoneyear.cellmeans.mcmc[,c(1,3,5,7,9,11,13,15)] - zoneyear.cellmeans.mcmc[,c(2,4,6,8,10,12,14,16)]
    co.effect.zy <- plyr::adply(co.mcmc.zy,2,function(x){data.frame(Mean=mean(x), HPDinterval(as.mcmc(x)),t(quantile(x,p=c(0.025,0.975))))})
    co.effect.zy <- cbind(expand.grid(cYear=levels(data$cYear)), co.effect.zy)

    co.mcmc.p.zy <- 100*(zoneyear.cellmeans.mcmc[,c(1,3,5,7,9,11,13,15)] - zoneyear.cellmeans.mcmc[,c(2,4,6,8,10,12,14,16)])/zoneyear.cellmeans.mcmc[,c(2,4,6,8,10,12,14,16)]
    co.effect.p.zy <- plyr::adply(co.mcmc.p.zy,2,function(x){data.frame(Mean=mean(x, na.rm=TRUE), HPDinterval(as.mcmc(x)),t(quantile(x,p=c(0.025,0.975))))})
    co.effect.p.zy <- plyr::adply(co.mcmc.p.zy,2,function(x){data.frame(Mean=mean(x, na.rm=TRUE))})
    co.effect.p.zy <- cbind(expand.grid(cYear=levels(data$cYear)), co.effect.p.zy)


    ## Probabilities Closed higher than open per year
    #co.dat.zy <- apply(co.mcmc.zy,2,function(x) {
    #    length(x[x>0])/length(x)
    #})
    #co.dat.zy <- cbind(expand.grid(cYear=levels(data$cYear)), Prob=co.dat.zy)
    #} else {
    #    co.dat.zy = NULL
    #    co.effect.zy=NULL
    #}

    list(cellmeans=dat,
         Closed_VS_Open_effect=co.effect,
         Closed_VS_Open_prob=co.dat,
         SectorZoneMeans=dat.s,
         Sector_Closed_VS_Open_effects=co.effect.s,
         Sector_Closed_VS_Open_prob=co.dat.s,
         OverallZoneMeans=dat.o,
         Overall_Closed_VS_Open_effect.effect=o.effect,
         Overall_Closed_VS_Open_prob=co
         )
}

MPA_SAVE_PLOTS <- function(p, filename, PNG = FALSE) {
    ggsave(paste0(filename, ".pdf"), p, width=15, height=10)
    if (PNG) {
        ggsave(paste0(filename, ".png"), p, width=15, height=10, dpi = 300)
        ggsave(paste0(filename, ".jpg"), p, width=15, height=10, dpi = 300)
    }
}

MPA_remove_combinations_without_fish <- function(dat) {
    regs<-
        dat %>% group_by(Sector,cYear,Zone) %>%
        summarise(Sum=sum(Value)) %>%
        as.data.frame() %>%
        filter(Sum==0) %>%
        dplyr::select(-Sum) %>%
        suppressMessages() %>%
        suppressWarnings()
    dat1 = dat %>% left_join(regs %>% mutate(Exclude=1)) %>%
        filter(is.na(Exclude)) %>%
        dplyr::select(-Exclude) %>%
        droplevels() %>%
        suppressMessages() %>%
        suppressWarnings()
    dat1
}

MPA_sector_levels4modelling <- function(dat) {
    dat %>%
        mutate(Sector=factor(Sector,
                             levels=c('Townsville','Cairns','Pompeys','Swains',
                                      'Cap-Bunkers','Cape Grenville', 'Princess Charlotte Bay',
                                      'Whitsundays','Cooktown-Lizard')))
    }

MPA_sector_levels4plotting <- function(dat) {
    dat %>%
        mutate(Sector=factor(Sector,
                             levels=c('Cape Grenville','Princess Charlotte Bay',
                                      'Cooktown-Lizard','Cairns','Townsville',
                                      'Whitsundays','Pompeys','Swains','Cap-Bunkers')))
}
