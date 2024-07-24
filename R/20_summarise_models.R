purpose <- 'Reports' #'Web' #'Reports'
sector <- 'All' #'Cairns' #'All'
PNG <- FALSE
    
library(dplyr)
library(tidyr)
library(reshape)
library(nlme)
library(data.table)
library(gmodels)
library(ggplot2)
library(gridExtra)
library(scales)
library(gtable)
library(MASS)
library(lme4)
library(arm)
library(brms)
library(coda)
library(INLA)
library(grid)
library(purrr)
library(png)
library(stringr)
library(ggtext)
library(magick)
source('helperFunctions.R')

## ---- settings
purpose <- 'Reports' #'Web' #'Reports'
sector <- 'All' #'Cairns' #'All'
PNG <- FALSE
MAX_REPORT_YEAR <- 2024
report_year <- 2024
sec <- "All"
## ----end

## ---- paths
MPA_create_paths(purpose, report_year)
## ----end


load(file = paste0(modelled_data_path, '/cm.all.RData'))
load(file = paste0(modelled_data_path, '/cm.inla.RData'))
load(file = paste0(processed_data_path, "/labels.RData"))

for (i in names(labels)) {  
    cellmeans.inla <- cm.inla[[i]]
    cellmeans.inla[[1]] <- MPA_sector_levels4plotting(cellmeans.inla[[1]])
    cellmeans.inla[[1]] <- cellmeans.inla[[1]] %>% filter(Sector != 'Cooktown-Lizard') %>% droplevels()
    p <- MPA_RAPPlot(cellmeans.inla[[1]], ytitle=labels[[i]], title=titles[[i]],purpose=purpose)
    ggsave(filename=paste0(fig_path, '/RAPPlot_',i,'_',sec,'_inla.pdf'), p,width=10, height=6)
    if (PNG) {
        ggsave(filename=paste0(fig_path, '/RAPPlot_',i,'_',sec,'_inla.pdf'), p, width=10, height=6, dpi = 300)
        ggsave(filename=paste0(fig_path, '/RAPPlot_',i,'_',sec,'_inla.pdf'), p, width=10, height=6, dpi = 300)
    }
}

## Zone effect (Sector/taxa)
### Abundance

cm.abund <- do.call(rbind,
                    sapply(cm.inla[seq(1, length(cm.inla), by = 3)],
                           `[`, "effects_sector")) %>%
    as.data.frame() %>%
    mutate(Group = str_replace(rownames(.), "(.*)\\.effects.*", "\\1")) %>%
    mutate(Name = factor(str_replace(Group, "( Biomass| Length)", ""))) %>%
    mutate(Name = str_wrap(Name, 40)) %>%
    mutate(Name = factor(Name, levels = rev(unique(Name))))
cm.abund <- MPA_sector_levels4plotting(cm.abund)
p <- cm.abund %>%
    ggplot(aes(y = Name, x = Median)) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = lower.1, xmax = upper.1)) +
    geom_linerange(aes(xmin = lower, xmax = upper),size = 1) +
    facet_wrap(~Sector, nrow = 2, scales = 'free_x', strip.position = "top") +
    scale_x_continuous('Percent change (Open vs Closed)') +
    theme_classic() +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 12),
          axis.title.y = element_blank()) 
ggsave(filename=paste0(fig_path, '/RAPPlot_effects_sector_inla.pdf'), p,width=10, height=10)
ggsave(filename=paste0(fig_path, '/RAPPlot_effects_sector_inla.png'), p,width=10, height=10, dpi = 300)
ggsave(filename=paste0(fig_path, '/RAPPlot_effects_sector_inla.jpg'), p,width=10, height=10, dpi = 300)

cm.abund <- do.call(rbind,
                    sapply(cm.inla[seq(1, length(cm.inla), by = 3)],
                           `[`, "effects_sector")) %>%
    as.data.frame() %>%
    mutate(Group = str_replace(rownames(.), "(.*)\\.effects.*", "\\1")) %>%
    mutate(Name = factor(str_replace(Group, "( Biomass| Length)", ""))) %>%
    mutate(Name = str_wrap(Name, 15)) %>%
    mutate(Name = factor(Name, levels = (unique(Name))))
cm.abund <- MPA_sector_levels4plotting(cm.abund)
cm.abund <- cm.abund %>% mutate(Sector = factor(Sector, levels = rev(levels(Sector))))
p <- cm.abund %>%
    ggplot(aes(y = Sector, x = Median)) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = lower.1, xmax = upper.1)) +
    geom_linerange(aes(xmin = lower, xmax = upper),size = 1) +
    facet_wrap(~Name, nrow = 2, scales = 'free_x', strip.position = "top") +
    scale_x_continuous('Percent change (Open vs Closed)') +
    theme_classic() +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 12),
          panel.spacing = unit(10, 'points'),
          axis.title.y = element_blank()) 
ggsave(filename=paste0(fig_path, '/RAPPlot_effects_sector1_inla.pdf'), p,width=15, height=10)
ggsave(filename=paste0(fig_path, '/RAPPlot_effects_sector1_inla.png'), p,width=15, height=10, dpi = 300)
ggsave(filename=paste0(fig_path, '/RAPPlot_effects_sector1_inla.jpg'), p,width=15, height=10, dpi = 300)

## Larger fishes
## ---- larger fishes
cm.larger <- cm.all[20:37]

alternative_group_names = tribble(
    ~Group,                                                                                     ~Name,                                                                        ~Image,
    "Labridae (Wrasses)",                                                                         "Labridae (Wrasses)",                                                       "9_Choerodon_anchorago_bw.png",
    "Lutjanidae (Tropical Snappers)",                                                             "Lutjanidae (Tropical snappers)",                                           "7_Lutjanidae_Lutjanus.gibbus.png",
    "Lethrinidae (Emperors)",                                                                     "Lethrinidae (Emperors))",                                                   "6_Lethrinidae.png",
    "Lethrinus miniatus and L. nebulosus (Redthroat and Spangled emperors)",                      "<i>Lethrinus miniatus + nebulosus</i><br>(Redthroat and Spangled emporers)", "5_Lethrinus_miniatus_vec.png",
    "Serranidae (Rockcods)",                                                                      "Serranidae (Rockcods)",                                                    "4_Serranidae.png",
    "Plectropomus and Variola spp (Coral trout)",                                                 "<i>Plectropomus + Variola spp.</i><br>(Coral trout)",                        "3_plectropomus_leopardus.png"
)

img2 <- img <- vector('list', length(alternative_group_names$Image))
for (i in 1:length(img)) img2[[i]] <- rasterGrob(magick::image_read(paste0('data/',alternative_group_names$Image[i])) %>% magick::image_flop(), interpolate=TRUE)
for (i in 1:length(img)) img[[i]] <- rasterGrob(readPNG(source=paste0('data/',alternative_group_names$Image[i])), interpolate=TRUE)

## cm.abund <- do.call('rbind', cm.all[seq(1,length(cm.all),by=3)]) %>% mutate(Group=rownames(.)) %>%
cm.abund <- do.call('rbind', cm.larger[seq(1,length(cm.larger),by=3)]) %>% mutate(Group=rownames(.)) %>%
    left_join(alternative_group_names) %>%
    mutate(Name=factor(Name, levels=rev(alternative_group_names$Name))) %>%
    arrange(Name) %>%
    mutate(lev=as.numeric(Name))

levs = cm.abund %>% dplyr::select(Name,lev) %>% mutate(Name=factor(Name, levels=rev(alternative_group_names$Name))) %>% distinct
g1 = cm.abund %>% ggplot() +
    geom_hline(yintercept=0, linetype='dashed') + 
    geom_pointrange(aes(y=Median, x=Name, ymin=lower.1, ymax=upper.1)) +
    geom_linerange(aes(y=Median, x=Name, ymin=lower, ymax=upper), size=1) +
    scale_y_continuous('Percent change (Open vs Closed)') +
    scale_x_discrete('', labels = cm.abund$Name) +
    coord_flip() +
    theme_classic() +
    ggtitle('a)') +
    theme(axis.text.y=element_markdown())
g1
gs2 = gs = ggplot() +
    geom_blank(data=cm.abund, aes(y=NULL, x=Name)) +    
    coord_flip() +
    ylim(0,1) +
    scale_x_discrete('', labels=cm.abund$Name) +
    theme_classic()
gs
for (i in 1:nrow(levs)) {
    j = nrow(levs)-i + 1
    gs <- gs +
        annotation_custom(img[[j]],
                          xmin=levs$lev[i]+0.4,
                          xmax=levs$lev[i]-0.4,
                          ymin=0, ymax=0.99) +
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.line=element_blank(),
              axis.ticks=element_blank())
    gs2 <- gs2 +
        annotation_custom(img2[[j]],
                          xmin=levs$lev[i]+0.4,
                          xmax=levs$lev[i]-0.4,
                          ymin=0, ymax=0.99) +
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.line=element_blank(),
              axis.ticks=element_blank())
}
#gt <- ggplot_gtable(ggplot_build(gs))
#gt$layout$clip[gt$layout$name == "panel"] <- "off"
#grid.draw(gt)
g1 + gs + plot_layout(widths=c(0.9,0.1))

ggsave(filename=paste0(fig_path, '/RAPCaterpiller_abundance.pdf'), g1,width=15, height=5)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_abundance.png'), g1,width=15, height=5, dpi=300)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_abundance.jpg'), g1,width=15, height=5, dpi=300)

## cm.length <- do.call('rbind', cm.all[seq(2,length(cm.all),by=3)])%>% mutate(Group=rownames(.)) %>%
cm.length <- do.call('rbind', cm.larger[seq(2,length(cm.larger),by=3)])%>% mutate(Group=rownames(.)) %>%
    mutate(Group=gsub(' Length','',Group)) %>%
    left_join(alternative_group_names) %>%
    mutate(Name=factor(Name, levels=rev(alternative_group_names$Name))) %>%
    arrange(Name) %>%
    mutate(lev=as.numeric(Name))
g2 <- cm.length %>% ggplot() +
    geom_hline(yintercept=0, linetype='dashed') + 
    geom_pointrange(aes(y=Median, x=Name, ymin=lower.1, ymax=upper.1)) +
    geom_linerange(aes(y=Median, x=Name, ymin=lower, ymax=upper), size=1) +
    scale_y_continuous('Percent change (Open vs Closed)') +
    scale_x_discrete('', labels=cm.length$Name) +
    coord_flip() +
    theme_classic() +
    ggtitle('b)') +
    theme(axis.text.y=element_markdown())
g2

ggsave(filename=paste0(fig_path, '/RAPCaterpiller_length.pdf'), g2,width=15, height=5)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_length.png'), g2,width=15, height=5, dpi=300)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_length.jpg'), g2,width=15, height=5, dpi=300)

## cm.biomass <- do.call('rbind', cm.all[seq(3,length(cm.all),by=3)])%>% mutate(Group=rownames(.)) %>%
cm.biomass <- do.call('rbind', cm.larger[seq(3,length(cm.larger),by=3)])%>% mutate(Group=rownames(.)) %>%
    mutate(Group=gsub(' Biomass','',Group)) %>%
    left_join(alternative_group_names) %>%
    mutate(Name=factor(Name, levels=rev(alternative_group_names$Name))) %>%
    arrange(Name) %>%
    mutate(lev=as.numeric(Name))
g3 <- cm.biomass %>% ggplot() +
    geom_hline(yintercept=0, linetype='dashed') + 
    geom_pointrange(aes(y=Median, x=Name, ymin=lower.1, ymax=upper.1)) +
    geom_linerange(aes(y=Median, x=Name, ymin=lower, ymax=upper), size=1) +
    scale_y_continuous('Percent change (Open vs Closed)') +
    scale_x_discrete('', labels=cm.biomass$Name) +
    coord_flip() +
    theme_classic() +
    ggtitle('c)') +
    theme(axis.text.y=element_markdown())
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_biomass.pdf'), g3,width=15, height=5)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_biomass.png'), g3,width=15, height=5, dpi=300)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_biomass.jpg'), g3,width=15, height=5, dpi=300)

##Compilation figure
library(patchwork)
g = (g1 + gs + plot_layout(widths=c(0.9,0.1))) /
    (g2 + gs + plot_layout(widths=c(0.9,0.1))) /
    (g3 + gs + plot_layout(widths=c(0.9,0.1)))
g
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_all.pdf'), g, width=10, height=8)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_all.png'), g, width=10, height=8, dpi=300)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_all.jpg'), g, width=10, height=8, dpi=300)
## without axis labels
g = (gs2+theme(plot.margin=margin(r=-1, unit='cm')) + g1+theme(axis.text.y=element_blank(), plot.margin=margin(l=-5,unit='cm')) + plot_layout(widths=c(0.07,0.93))) /
    (gs2+theme(plot.margin=margin(r=-1, unit='cm')) + g2+theme(axis.text.y=element_blank(), plot.margin=margin(l=-5,unit='cm')) + plot_layout(widths=c(0.07,0.93))) /
    (gs2+theme(plot.margin=margin(r=-1, unit='cm')) + g3+theme(axis.text.y=element_blank(), plot.margin=margin(l=-5,unit='cm')) + plot_layout(widths=c(0.07,0.93)))
g
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_all1.pdf'), g, width=10, height=8)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_all1.png'), g, width=10, height=8, dpi=300)
ggsave(filename=paste0(fig_path, '/RAPCaterpiller_all1.jpg'), g, width=10, height=8, dpi=300)
### end

## ----end
