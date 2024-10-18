library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(ggrepel)
# library(plotly)
library(DT)

# library(PombeKOViewer)

load("./data/sp_ko_lib.rda")
load("./data/sp_orthologs.rda")
load("./data/sp_prot_trans.rda")
load("./data/sp_prot_trans_cor.rda")
load("./data/sp_som1.rda")
load("./data/sp_som2.rda")


# if(!'som2_cluster' %in% names(sp_ko_lib)) {
#   sp_ko_lib %>%
#     left_join(
#       sp_som2 %>%
#         select(som2_cluster=Cluster_number,
#                KOstrain=Systematic_ID,
#                som2_filtered=Filtered,
#                som2_GO_up=GOterms_Upregulated,
#                som2_GO_down=GOterms_Downregulated),
#       by='KOstrain'
#     ) %>%
#     left_join(
#       sp_som1 %>%
#         select(som1_cluster=Cluster_number,
#                KOstrain=Systematic_ID,
#                som1_filtered=Filtered),
#       by='KOstrain'
#     ) ->
#     sp_ko_lib
# }
