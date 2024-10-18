#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

source('global.R')
# options(shiny.trace=TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  observe({
    # ?data=KOLibrary&data2=searchByKOStrain
    # ?data=KOLibrary&data2=searchByProtein&protein=SPBC2D10.18
    # ?data=KOLibrary&data2=searchByKOStrain&KOStrain=SPBC2D10.18
    # ?data=KOLibrary&data2=searchByProtein&protein=SPBC1289.10c
    # ?data=KOLibrary&data2=searchByKOStrain&KOStrain=SPBC1289.10c
    query <- parseQueryString(session$clientData$url_search)
    if('data' %in% names(query)) {
      # print(paste('move to', query[['data']], 'and select row', query[['geneid']]))
      updateNavbarPage(session, inputId="tabs", selected=query[['data']])
    }
    if(all(c('data','data2') %in% names(query) &
           is.null(query[['KOStrain']]) &
           is.null(query[['protein']]))) {
      # print(paste('move to', query[['data']], 'and select row', query[['geneid']]))
      updateNavbarPage(session, inputId="tabs", selected=query[['data']])
      updateNavbarPage(session, inputId="tabs2", selected=query[['data2']])
    }
    # if(all(c('data','data2','KOStrain') %in% names(query)) &
    #    !is.null(query[['KOStrain']])) {
    #   rows <-
    #     grep(query[['KOStrain']],
    #          MainKOStrainData()$`KO strain`)
    #   selected_row_KOstrain(as.numeric(rows))
    #   updateNavbarPage(session, inputId="tabs", selected=query[['data']])
    #   updateNavbarPage(session, inputId="tabs2", selected=query[['data2']])
    # }
    #
    # if(all(c('data','data2','protein') %in% names(query)) &
    #    !is.null(query[['protein']])) {
    #   rows <-
    #     grep(query[['protein']],
    #          MainKOProteinData()$`protein`)
    #   selected_row_protein(as.numeric(rows))
    #   updateNavbarPage(session, inputId="tabs", selected=query[['data']])
    #   updateNavbarPage(session, inputId="tabs2", selected=query[['data2']])
    #   print(rows)
    # }
    if(!is.null(query[['KOStrain']])) {
      rows <-
        head(grep(query[['KOStrain']],
             MainKOStrainData()$`KO strain`,
             ignore.case=TRUE), 1)
      selected_row_KOstrain(as.numeric(rows))
      updateNavbarPage(session, inputId="tabs", selected='KOLibrary')
      updateNavbarPage(session, inputId="tabs2", selected='searchByKOStrain')
    }

    if(!is.null(query[['protein']])) {
      rows <-
        head(grep(query[['protein']],
             MainKOProteinData()$`protein`,
             ignore.case=TRUE), 1)
      selected_row_protein(as.numeric(rows))
      updateNavbarPage(session, inputId="tabs", selected='KOLibrary')
      updateNavbarPage(session, inputId="tabs2", selected='searchByProtein')
    }

    # if (!is.null(query[['geneid']]) & is.null(query[['cgid']])) {
    #   if(query[['data']] == 'wholelifecycle') {
    #     rows <-
    #       grep(query[['geneid']],
    #            Rshinyfly::pg_lifecycle[['protein_groups']][['ensembl_gene_id']])
    #   } else if(query[['data']] == 'embryogenesis') {
    #     rows <-
    #       grep(query[['geneid']],
    #            Rshinyfly::pg_timecourse[['protein_groups']][['ensembl_gene_id']])
    #   }
    #   selected_row(as.numeric(rows))
    # }
    # if (is.null(query[['geneid']]) & !is.null(query[['cgid']])) {
    #   if(query[['data']] == 'wholelifecycle') {
    #     rows <-
    #       grep(query[['cgid']],
    #            Rshinyfly::pg_lifecycle[['protein_groups']][['flybasecgid_gene']])
    #   } else if(query[['data']] == 'embryogenesis') {
    #     rows <-
    #       grep(query[['cgid']],
    #            Rshinyfly::pg_timecourse[['protein_groups']][['flybasecgid_gene']])
    #   }
    #   selected_row(as.numeric(rows))
    # }

  })
  selected_row_KOstrain <- reactiveVal()
  selected_row_protein <- reactiveVal()



  updateSelectizeInput(session, 'KO_strain_protein',
                       choices = sort(unique(sp_ko_lib$protein)),
                       server = TRUE,
                       selected=c('SPBC14F5.03c', 'SPACUNK4.16c'))
  updateSelectizeInput(session, 'KO_strain_gene',
                       choices = sort(unique(sp_ko_lib$`gene name`)),
                       server = TRUE,
                       selected=c('csc4', 'caf16'))
  updateSelectizeInput(session, 'highlight',
                       choices = sort(c(as.character(unique(sp_prot_trans$`gene name`)),
                                        as.character(unique(sp_prot_trans$protein)))),
                       server = TRUE,
                       selected=c('dad3','SPACUNK4.17'))


  KOStrainClusterSelection <- reactive({
    sp_som2 %>%
      select(Cluster_number, Size_of_the_cluster, Filtered) %>%
      distinct() %>%
      mutate(string=sprintf('#%d - %d strains<br />%s',
                            Cluster_number, Size_of_the_cluster, Filtered)) %>%
      select(string) %>%
      unlist() -> jnk
    matrix(jnk, ncol=11, byrow=TRUE)
  })
  output$kostrain_cluster_selection_table = DT::renderDataTable({
    DT::datatable(
      # sp_som2 %>%
      #   select(Cluster_number, Size_of_the_cluster, Filtered),
      KOStrainClusterSelection(),
      options = list(pageLength=15),
      selection = list(mode="single",
                       target='cell'),
      rownames=FALSE,
      escape=FALSE
      # colnames=FALSE
    )
  })


  output$kostrain_celltext <- renderPrint({
    cell <- input$kostrain_cluster_selection_table_cells_selected
    cell <- as.numeric(cell) + matrix(c(0, 1),nrow=1)
    if(length(cell)) {
      text <- KOStrainClusterSelection()[cell]
      cluster_id <- sub('#(\\d+) -.*', '\\1', text)
      filter_status <- sub('.*<br />(.*)', '\\1', text)
      sub_cluster <-
        sp_som2 %>%
        filter(Cluster_number == cluster_id)
      # left_join(sp_ko_lib %>%
      #             select(Systematic_ID=KOstrain, KOgene) %>%
      #             distinct(),
      #           by='Systematic_ID')
      image_name <- sprintf('network_images/Strain_Cluster_%s.jpeg', cluster_id)
      image_string <- ''
      if(filter_status == 'Kept') {
        image_string <- sprintf('<img src="%s" width="100%%" /><br />Scroll down for bigger view.<br /><br />', image_name)
        # image_string <- 'Network images below table!<b />'
      }
      cat(sprintf(
        '%s<strong>Cluster: %s</strong><br />%s<br /><br /></br ><strong>GO terms enriched (adjusted p-value <0.05) in generally Downregulated Proteins:</strong><br />%s<br /><br /><strong>GO terms enriched (adjusted p-value <0.05) in generally Upregulated Proteins:</strong><br />%s',
        # '%sCluster: %s<br />%s<br /><br />',
        image_string, cluster_id,
        paste0(sprintf('<a target="_blank" rel="noopener noreferrer" href="https://www.pombase.org/gene/%s">%s - %s</a>',
                       sub_cluster$Systematic_ID,
                       sub_cluster$Systematic_ID,
                       sub_cluster$`KO gene name`),
               collapse='<br />'),
        paste(unlist(strsplit(unique(sub_cluster$GOterms_Downregulated), ':')), collapse='<br />'),
        paste(unlist(strsplit(unique(sub_cluster$GOterms_Upregulated), ':')), collapse='<br />')
      ))
    } else {
      cat('Please select a cell in the table.')
    }
  })
  output$kostrain_stringDB <- renderPrint({
    cell <- input$kostrain_cluster_selection_table_cells_selected
    cell <- as.numeric(cell) + matrix(c(0, 1),nrow=1)
    if(length(cell)) {
      text <- KOStrainClusterSelection()[cell]
      cluster_id <- sub('#(\\d+) -.*', '\\1', text)
      filter_status <- sub('.*<br />(.*)', '\\1', text)
      sub_cluster <-
        sp_som2 %>%
        filter(Cluster_number == cluster_id)
      clean_IDs <- grep('Wildtype', sub_cluster$Systematic_ID,
                        invert=TRUE, value=TRUE)
      if(length(clean_IDs)) {
        stringDB_link_construct <-
          file(sprintf(
            'https://string-db.org/api/tsv/get_link?identifiers=%s&species=4896&required_score=150',
            paste0(sub_cluster$Systematic_ID, collapse='%0d')
          ))
        stringDB_permalink <-
          readLines(stringDB_link_construct)[[2]]
        cat(sprintf(
          '<strong>Inspect on stringDB for more details: </strong><br /><a target="_blank" rel="noopener noreferrer" href="%s"><img src="stringDB_logo.png", alt="String DB link", width="150px"/></a>',
          stringDB_permalink
        ))
        close(stringDB_link_construct)
      }
    }
  })
  output$kostrain_networkimg <- renderPrint({
    cell <- input$kostrain_cluster_selection_table_cells_selected
    cell <- as.numeric(cell) + matrix(c(0, 1),nrow=1)
    if(length(cell)) {
      text <- KOStrainClusterSelection()[cell]
      cluster_id <- sub('#(\\d+) -.*', '\\1', text)
      filter_status <- sub('.*<br />(.*)', '\\1', text)
      image_name <- sprintf('network_images/Strain_Cluster_%s.jpeg', cluster_id)
      image_string <- ''
      if(filter_status == 'Kept') {
        image_string <- sprintf('<img src="%s" width="100%%" /><br />', image_name)
      }
      cat(image_string)
    } else {
      cat('Please select a cell in the table.')
    }
  })


  ProteinClusterSelection <- reactive({
    sp_som1 %>%
      select(Cluster_number, Size_of_the_cluster, Filtered) %>%
      distinct() %>%
      mutate(string=sprintf('#%d - %d proteins<br />%s',
                            Cluster_number, Size_of_the_cluster, Filtered)) %>%
      select(string) %>%
      unlist() -> jnk
    matrix(c(jnk, rep('', 6)), ncol=10, byrow=TRUE)
  })
  output$protein_cluster_selection_table = DT::renderDataTable({
    DT::datatable(ProteinClusterSelection(),
                  options = list(pageLength=15),
                  selection = list(mode="single",
                                   target='cell'),
                  rownames=FALSE,
                  escape=FALSE
                  # colnames=FALSE
    )
  })
  output$protein_stringDB <- renderPrint({
    cell <- input$protein_cluster_selection_table_cells_selected
    cell <- as.numeric(cell) + matrix(c(0, 1),nrow=1)
    if(length(cell)) {
      text <- ProteinClusterSelection()[cell]
      cluster_id <- sub('#(\\d+) -.*', '\\1', text)
      filter_status <- sub('.*<br />(.*)', '\\1', text)
      sub_cluster <-
        sp_som1 %>%
        filter(Cluster_number == cluster_id)
      clean_IDs <- grep('Wildtype', sub_cluster$Systematic_ID,
                        invert=TRUE, value=TRUE)
      stringDB_link_construct <-
        file(sprintf(
          'https://string-db.org/api/tsv/get_link?identifiers=%s&species=4896&required_score=150',
          paste0(sub_cluster$Systematic_ID, collapse='%0d')
        ))
      if(length(clean_IDs)) {
      stringDB_permalink <-
        readLines(stringDB_link_construct)[[2]]
      cat(sprintf(
        '<strong>Inspect on stringDB for more details: </strong><br /><a target="_blank" rel="noopener noreferrer" href="%s"><img src="stringDB_logo.png", alt="String DB link", width="150px"/></a>',
        stringDB_permalink
      ))
      close(stringDB_link_construct)
      }
    }
  })
  output$protein_celltext <- renderPrint({
    cell <- input$protein_cluster_selection_table_cells_selected
    cell <- as.numeric(cell) + matrix(c(0, 1),nrow=1)
    if(length(cell)) {
      text <- ProteinClusterSelection()[cell]
      cluster_id <- sub('#(\\d+) -.*', '\\1', text)
      filter_status <- sub('.*<br />(.*)', '\\1', text)
      sub_cluster <-
        sp_som1 %>%
        filter(Cluster_number == cluster_id)
      # left_join(sp_ko_lib %>%
      #             select(Systematic_ID=KOstrain, KOgene) %>%
      #             distinct(),
      #           by='Systematic_ID')
      image_name <- sprintf('network_images/Protein_Cluster_%s.jpeg', cluster_id)
      image_string <- ''
      if(filter_status == 'Kept') {
        image_string <- sprintf('<img src="%s" width="100%%" /><br />Scroll down for bigger view.<br /><br />', image_name)
      }
      cat(sprintf(
        '%s<strong>Cluster: %s</strong><br />%s<br /><br />',
        image_string, cluster_id,
        paste0(sprintf('<a target="_blank" rel="noopener noreferrer" href="https://www.pombase.org/gene/%s">%s - %s</a>',
                       sub_cluster$Systematic_ID,
                       sub_cluster$Systematic_ID,
                       sub_cluster$`gene name`),
               collapse='<br />'))
      )
    } else {
      cat('Please select a cell in the table.')
    }
  })
  output$protein_networkimg <- renderPrint({
    cell <- input$protein_cluster_selection_table_cells_selected
    cell <- as.numeric(cell) + matrix(c(0, 1),nrow=1)
    if(length(cell)) {
      text <- ProteinClusterSelection()[cell]
      cluster_id <- sub('#(\\d+) -.*', '\\1', text)
      filter_status <- sub('.*<br />(.*)', '\\1', text)
      image_name <- sprintf('network_images/Protein_Cluster_%s.jpeg', cluster_id)
      image_string <- ''
      if(filter_status == 'Kept') {
        image_string <- sprintf('<img src="%s" width="100%%" /><br />', image_name)
      }
      cat(image_string)
    } else {
      cat('Please select a cell in the table.')
    }
  })

  MainKOStrainData <- reactive({
    sp_ko_lib %>%
      select(`KO strain`, `KO gene name`) %>%
      distinct() %>%
      left_join(sp_orthologs,
                by=c('KO strain'='s.pombe')) %>%
      mutate(sortcol=ifelse(`KO gene name` == '', NA, `KO gene name`)) %>%
      arrange(sortcol) %>%
      select(-sortcol)
  })
  KOStrainSelection <- reactive({
    unlist(MainKOStrainData()[input$main_KO_strain_table_rows_selected,
                              'KO strain'])
  })
  output$main_KO_strain_table = DT::renderDataTable({
    DT::datatable(MainKOStrainData(),
                  options = list(
                    pageLength = 10
                  ),
                  selection = list(mode='single', selected=selected_row_KOstrain()),
                  rownames= FALSE
    )
  })

  MainKOStrainClusterData <- reactive({
    sp_som2 %>%
      select(`KO strain`=Systematic_ID, `KO gene name`, Cluster_number) %>%
      distinct() %>%
      mutate(sortcol=ifelse(`KO gene name` == '', NA, `KO gene name`)) %>%
      arrange(sortcol) %>%
      select(-sortcol)
  })
  MainKOStrainClusterSelection <- reactive({
    unlist(MainKOStrainClusterData()[input$main_KO_strain_cluster_table_rows_selected,
                              'KO strain'])
  })
  output$main_KO_strain_cluster_table = DT::renderDataTable({
    DT::datatable(MainKOStrainClusterData() %>%
                    left_join(sp_orthologs,
                              by=c('KO strain'='s.pombe')),
                  options = list(
                    pageLength = 10
                  ),
                  selection = 'single',
                  rownames= FALSE
    )
  })
  proxy_kostrain_cluster = dataTableProxy('kostrain_cluster_selection_table')
  observe({
    if(length(input$main_KO_strain_cluster_table_rows_selected) > 0) {
      sp_som2 %>%
        filter(Systematic_ID == MainKOStrainClusterSelection()) ->
        df

    proxy_kostrain_cluster %>%
      selectCells(matrix(c(ceiling(df$Cluster_number / 11),
                           (df$Cluster_number - 1) %% 11),
                         ncol=2))
    }
  })

  MainKOProteinData <- reactive({
    sp_ko_lib %>%
      select(protein, `gene name`) %>%
      distinct() %>%
      left_join(sp_orthologs,
                by=c('protein'='s.pombe')) %>%
      mutate(sortcol=ifelse(`gene name` == '', NA, `gene name`)) %>%
      arrange(sortcol) %>%
      select(-sortcol)
  })
  KOProteinSelection <- reactive({
    unlist(MainKOProteinData()[input$main_KO_protein_table_rows_selected,
                              'protein'])
  })
  output$main_KO_protein_table = DT::renderDataTable({
    DT::datatable(MainKOProteinData(),
                  options = list(
                    pageLength = 10
                  ),
                  selection = list(mode='single', selected=selected_row_protein()),
                  rownames= FALSE
    )
  })

  MainProteinClusterData <- reactive({
    sp_som1 %>%
      select(`protein`=Systematic_ID, `gene name`, Cluster_number) %>%
      distinct() %>%
      mutate(sortcol=ifelse(`gene name` == '', NA, `gene name`)) %>%
      arrange(sortcol) %>%
      select(-sortcol)
  })
  MainProteinClusterSelection <- reactive({
    unlist(MainProteinClusterData()[input$main_protein_cluster_table_rows_selected,
                                     'protein'])
  })
  output$main_protein_cluster_table = DT::renderDataTable({
    DT::datatable(MainProteinClusterData() %>%
                    left_join(sp_orthologs,
                              by=c('protein'='s.pombe')),
                  options = list(
                    pageLength = 10
                  ),
                  selection = 'single',
                  rownames= FALSE
    )
  })
  proxy_protein_cluster = dataTableProxy('protein_cluster_selection_table')
  observe({
    if(length(input$main_protein_cluster_table_rows_selected) > 0) {
      sp_som1 %>%
        filter(Systematic_ID == MainProteinClusterSelection()) ->
        df

      proxy_protein_cluster %>%
        selectCells(matrix(c(ceiling(df$Cluster_number / 10),
                             (df$Cluster_number - 1) %% 10),
                           ncol=2))
    }
  })



  proxy = dataTableProxy('main_prottrans_table')
  observeEvent(input$clear1, {
    proxy %>% selectRows(NULL)
  })
  MainProtTransData <- reactive({
    df_cor <- sp_prot_trans %>%
      filter(pvalue.P <= 1,
             pvalue.T <= 1) %>%
      group_by(`KO strain`, `KO gene name`) %>%
      summarise(correlation=cor(
        log2FC_Strain_wt.P, log2FC_Strain_wt.T))
    sp_prot_trans %>%
      select(`KO strain`, `KO gene name`) %>%
      distinct() %>%
      left_join(df_cor, by=c('KO strain', 'KO gene name'))
  })
  ProtTransSelection <- reactive({
    unlist(MainProtTransData()[input$main_prottrans_table_rows_selected,
                               'KO strain'])
  })
  output$main_prottrans_table = DT::renderDataTable({
    DT::datatable(MainProtTransData(),
                  options = list(
                    pageLength = 10
                  ),
                  selection = 'multiple',
                  rownames= FALSE
    ) %>%
      DT::formatRound('correlation', 3)
  })
  output$correlationPlot <- plotly::renderPlotly({
    # ggplot(prot_trans_cor,
    #        aes(RNA_protein_cor, -log10(RNA_protein_cor_pvalue),
    #            text=sprintf('%s - %s<br />%s', Gene_Of_Interest, Gene.names, product))) +
    #   geom_hline(yintercept = -log10(0.05), color="#878787", linetype="dashed") +
    #   geom_segment(aes(x=0, y=0 , xend=0, yend=36), color="#878787") +
    #   geom_point(aes(color=type)) +
    #   guides(color=FALSE) +
    #   theme_classic()+
    #   xlab("Pearson R, mRNA vs. protein") +
    #   ylab("-log10(p-value)") +
    #   theme(axis.text=element_text(size=6),
    #         axis.title=element_text(size=8,face="bold")) +
    #   xlim(-1,1) +
    #   # annotate("text", x = 0, y=40, size=3, label="Total = 1,706", hjust=0.5) +
    #   # annotate("text", x = -.8, y=35, size=3, label="Negative = 90", hjust=0) +
    #   # annotate("text", x = .8, y=35, size=3, label="Positive = 750", hjust=1) +
    #   scale_color_manual(values=c("#2166ac", "gray50", "#b2182b")) -> jnk_plot
    # plotly::ggplotly(jnk_plot) %>%
    #       plotly::layout(showlegend = FALSE)
    df_cor <- sp_prot_trans %>%
      filter(pvalue.P <= 1,
             pvalue.T <= 1) %>%
      mutate(strip_text=sprintf('%s - %s', `KO strain`, `KO gene name`)) %>%
      group_by(`strip_text`) %>%
      summarise(correlation=cor(
        log2FC_Strain_wt.P, log2FC_Strain_wt.T)) %>%
      arrange(correlation) %>%
      mutate(index=1:n())
    ggplot(df_cor, aes(index, correlation, text=strip_text)) +
      geom_hline(yintercept=0) +
      geom_point() +
      ggtitle('Correlation distribution') +
      theme(axis.text.x=element_blank(),
            axis.title.x=element_blank()) -> jnk
    plotly::ggplotly(jnk)
  })
  output$distPlot <- plotly::renderPlotly({
    # cat(dput(ProtTransSelection()))
    if(length(ProtTransSelection())) {
      df <- sp_prot_trans %>%
        filter(pvalue.P < input$prot_pvalue,
               pvalue.T < input$trans_pvalue,
               `KO strain` %in% ProtTransSelection()) %>%
        mutate(highlight=FALSE,
               strip_text=sprintf('%s - %s', `KO strain`, `KO gene name`),
               plotly_labels=sprintf('%s - %s', `protein`, `gene name`))
      df_cor <- sp_prot_trans %>%
        filter(pvalue.P <= 1,
               pvalue.T <= 1,
               `KO strain` %in% ProtTransSelection()) %>%
        mutate(strip_text=sprintf('%s - %s', `KO strain`, `KO gene name`)) %>%
        group_by(`strip_text`) %>%
        summarise(correlation=cor(
          log2FC_Strain_wt.P, log2FC_Strain_wt.T))
      if(length(input$highlight)) {
        df <-
          df %>%
          mutate(highlight=if_else(protein %in% input$highlight |
                                     `gene name` %in% input$highlight,
                                   TRUE,
                                   FALSE))
      }
      ggplot(df, aes(log2FC_Strain_wt.T, log2FC_Strain_wt.P,
                     text=plotly_labels)) +
        geom_hline(yintercept=0, color='grey') +
        geom_vline(xintercept=0, color='grey') +
        geom_point(data=df %>% filter(!highlight), alpha=.3, color='black') +
        geom_point(data=df %>% filter(highlight), alpha=1, color='red') +
        geom_text(data=df_cor,
                  aes(label=sprintf('R: %.2f', correlation), text=NULL),
                  x=min(df$log2FC_Strain_wt.T)*0.9,
                  y=max(df$log2FC_Strain_wt.P)*0.9,
                  hjust=-.5, vjust=1.5) +
        facet_wrap(~ strip_text) +
        scale_x_continuous(limits=c(min(df$log2FC_Strain_wt.T)*1.2, max(df$log2FC_Strain_wt.T))) +
        theme_classic() +
        xlab('Transcriptome - log2 fold change (ko/wt)') +
        ylab('Proteome - log2 fold change (ko/wt)') +
        theme(panel.border=element_rect(fill=NA, color='black')) ->
        dist_plot
      if(sum(df$highlight)) {
        dist_plot +
          geom_text(data=df %>% filter(highlight),
                    aes(label=plotly_labels),
                    size=3) ->
          dist_plot
      }

      plotly::ggplotly(dist_plot)
    }
  })

  # output$detected_protein <- plotly::renderPlotly({
  #   # validate(
  #   #   need(length(input$lifecycle_table_rows_selected) > 0,
  #   #        "Please select a protein/gene in the search tab.")
  #   # )
  #
  #   selected_protein <- input$specific_protein
  #   sp_ko_lib %>%
  #     as_tibble() %>%
  #     filter(pG == selected_protein) %>%
  #     group_by(strain) %>%
  #     arrange(desc(log2_intensity)) %>%
  #     mutate(strain_index=1:n()) %>%
  #     ungroup() %>%
  #     arrange(strain) %>%
  #     mutate(index=1:n()) %>%
  #     ggplot(aes(index, log2_intensity, color=strain, label=KOstrain)) +
  #     geom_point(alpha=.5) ->
  #     selected_protein_plot
  #
  #   plotly::ggplotly(selected_protein_plot) %>%
  #     plotly::layout(showlegend = FALSE)
  # })


# KO per strain section ---------------------------------------------------

  KOStrainData <- reactive({
    selected_strain <- KOStrainSelection()
    selected_protein <- input$KO_strain_protein
    selected_gene <- input$KO_strain_gene
    if(length(selected_strain)) {
      sp_ko_lib %>%
        as_tibble() %>%
        filter(`KO strain` == selected_strain,
               strain != 'wt') %>%
        mutate(highlight=protein %in% selected_protein |
                 `gene name` %in% selected_gene) %>%
        select(-strain, `KO strain`) %>%
        arrange(desc(`log2 intensity`)) ->
        df
    } else {
      tibble()
    }
  })
  output$KO_strain_print <- renderPrint({
    df <- KOStrainData()
    if(length(KOStrainSelection()) == 0) {
      cat('Please search and select KO strain in the top table')
    } else {
      cat(
        sprintf(paste(c(
          'We quantified %d out of %d proteins in %s.<br />',
          '%d proteins are differentially regulated (FDR p-value < 0.05, see orange dotted line in right panel)<br />',
          '%d out of %d proteins of interest where detected.'
        ), collapse=''),
        sum(df$`log2 intensity` > 0),
        length(df$`log2 intensity`),
        KOStrainSelection(),
        sum(df$`FDR p-value` < 0.05, na.rm=TRUE),
        sum(df$highlight & df$`log2 intensity` > 0),
        length(c(input$KO_strain_protein, input$KO_strain_gene))
        )
      )
    }
  })
  output$KO_cluster_download <- downloadHandler(
    filename = function() {
      sprintf('KOstrain_cluster_%s.csv',
              Sys.Date())
    },
    content = function(con) {
      write.table(sp_som2,
                  con, sep='\t', row.names=FALSE, quote=FALSE)
    }
  )
  output$Protein_cluster_download <- downloadHandler(
    filename = function() {
      sprintf('Protein_cluster_%s.csv',
              Sys.Date())
    },
    content = function(con) {
      write.table(sp_som1,
                  con, sep='\t', row.names=FALSE, quote=FALSE)
    }
  )
  output$KO_strain_download <- downloadHandler(
    filename = function() {
      sprintf('PombeKO_perStrain_%s_%s.csv',
              KOStrainSelection(),
              Sys.Date())
    },
    content = function(con) {
      write.table(KOStrainData() %>%
                    left_join(sp_orthologs,
                              by=c('protein'='s.pombe')),
                  con, sep='\t', row.names=FALSE)
    }
  )
  output$KO_strain_table = DT::renderDataTable({
    if(length(KOStrainSelection())) {
      DT::datatable(
        KOStrainData() %>%
          select(-matches('.*_GO_.*'),
                 -`KO strain`, -`KO gene name`, -highlight,
                 -matches('.*_filtered')) %>%
          left_join(sp_orthologs,
                    by=c('protein'='s.pombe')) %>%
          mutate(protein=sprintf('<a target="_blank" rel="noopener noreferrer" href="https://www.pombase.org/gene/%s">%s</a>',
                            protein, protein)),
        options = list(
          order = list(list(1, 'desc')),
          pageLength = 40
        ),
        rownames=FALSE,
        escape=FALSE) %>%
        DT::formatRound('log2 intensity', 3) %>%
        DT::formatRound('FDR p-value', 3) %>%
        DT::formatRound('log2 fold change', 3)
    } else {
      DT::datatable(tibble())
    }
  })
  output$KO_strain_plot <- plotly::renderPlotly({
    # print(length(KOStrainSelection()))
    if(length(KOStrainSelection())) {
      df <- KOStrainData() %>%
        filter(`log2 intensity` > 0) %>%
        mutate(index=1:n()) %>%
        mutate(labels=sprintf('%s - %s',
                              `gene name`,
                              protein))

      ggplot(df, aes(index, `log2 intensity`,
                     color=highlight, label=`KO strain`,
                     text=labels)) +
        geom_point(data=df %>% filter(!highlight)) +
        geom_point(data=df %>% filter(highlight),
                   size=3) +
        scale_color_manual('highlight', values=c(`TRUE`='#fc4e2aFF',
                                                 `FALSE`='#77777733')) +
        guides(color=FALSE) +
        labs(title=sprintf(
          'Ranked protein intensities in\nKO strain %s', KOStrainSelection())) +
        ylab('log2 protein intensities') +
        theme_classic() +
        theme(legend.position='none') ->
        abundance_plot
      if(sum(df$highlight)) {
        abundance_plot +
          geom_text(data=df %>%
                             arrange(desc(`log2 intensity`)) %>%
                             filter(highlight) %>%
                             slice(1:30),
                           aes(label=sprintf('%s - %s',
                                             `gene name`,
                                             protein)),
                           size=2.5, color='black') ->
          abundance_plot
      }
      plotly::ggplotly(abundance_plot)
    }
  })
  output$KO_strain_scatter <- plotly::renderPlotly({
    # print(length(KOStrainSelection()))
    if(length(KOStrainSelection())) {
      df <- KOStrainData() %>%
        filter(`log2 intensity` > 0,
               !is.na(`log2 fold change`)) %>%
        mutate(labels=sprintf('%s - %s',
                                     `gene name`,
                                     protein))


      ggplot(df,
             aes(`log2 fold change`, -log10(`FDR p-value`), color=highlight,
                 text=labels)) +
        geom_hline(yintercept=-log10(0.05), linetype='F2', color='#fc4e2a99', size=2) +
        geom_point(data=df %>% filter(!highlight)) +
        geom_point(data=df %>% filter(highlight),
                   size=3) +
        # geom_label_repel(aes(label=labels),
        #                  size=2.5, color='black',
        #                  fill='#FFFFFF33',
        #                  box.padding=1) +
        # geom_label_repel(aes(label=labels),
        #                  size=2.5, color='black',
        #                  fill='#FFFFFF33', max.overlaps=100,
        #                  box.padding=.1) +
        # geom_text(aes(label=labels)) +
        scale_color_manual('highlight', values=c(`TRUE`='#fc4e2aFF',
                                                 `FALSE`='#77777733')) +
        guides(color=FALSE) +
        labs(title=sprintf(
          'Volcano plot of proteome intensities in\nKO strain %s compared to wt',
             KOStrainSelection())) +
        xlab('log2 fold change (ko/wt)') +
        theme_classic() +
        theme(legend.position='none') ->
        scatter_plot
      if(sum(df$highlight)) {
        scatter_plot +
          geom_text(data=df %>%
                      arrange(desc(`log2 intensity`)) %>%
                      filter(highlight) %>%
                      slice(1:30),
                    aes(label=sprintf('%s - %s',
                                      `gene name`,
                                      protein)),
                    size=2.5, color='black') ->
          scatter_plot
      }
      plotly::ggplotly(scatter_plot)
    }
  })

# KO per protein section --------------------------------------------------

  output$KO_protein_print <- renderPrint({
    if(length(KOProteinSelection()) == 0) {
      cat('Please search and select KO strain in the top table')
    } else {
      selected_protein <- KOProteinSelection()
      sp_ko_lib %>%
        as_tibble() %>%
        filter(protein == selected_protein |
                 `gene name` == selected_protein) ->
        df
      som1 <- sp_som1 %>% filter(Systematic_ID == selected_protein)
      som2 <- sp_som2 %>% filter(Systematic_ID == selected_protein)
      cat(
        sprintf(paste(c(
          '%s was detected in %d knockout strains and %d wildtype measurements.<br />',
          'It was assigned to #%s protein cluster.<br />',
          'It was assigned to #%s KOstrain cluster.'
        ), collapse=''),
        KOProteinSelection(),
        sum(df$strain != 'wt' & df$`log2 intensity` > 0),
        sum(df$strain == 'wt' & df$`log2 intensity` > 0),
        ifelse(length(som1$Cluster_number), som1$Cluster_number, 'NONE'),
        ifelse(length(som2$Cluster_number), som2$Cluster_number, 'NONE')
        )
      )
    }
  })

  KOProteinData <- reactive({
    selected_protein <- KOProteinSelection()
    if(length(selected_protein)) {
      # sp_ko_lib %>%
      #   as_tibble() %>%
      #   filter(pG == selected_protein |
      #            Gene.names == selected_protein,
      #          strain != '461 wt') %>%
      #   mutate(above_th=log2_intensity > input$KO_protein_threshold) %>%
      #   select(-strain, -mysum, -count, -pG) %>%
      #   arrange(desc(log2_intensity)) ->
      #   df
      sp_ko_lib %>%
        as_tibble() %>%
        filter(protein == selected_protein |
                 `gene name` == selected_protein) %>%
        select(-protein) %>%
        arrange(desc(`log2 intensity`)) ->
        df
    } else {
      tibble()
    }
  })
  output$KO_protein_boxplot <- renderPlot({
    if(length(KOProteinSelection())) {
      df <- KOProteinData() %>%
        filter(`log2 intensity` > 0) %>%
        mutate(xaxis=ifelse(strain=='strain',
                            'KO strains',
                            'wt replicates (n=2922)'))

      dot_df <-
        df %>% filter(strain != 'wt') %>%
        arrange(desc(`log2 intensity`)) %>%
        mutate(label=ifelse(row_number() <= input$KO_protein_labels |
                              row_number() > (n()-input$KO_protein_labels),
                            as.character(`KO strain`),
                            ''))

      if(length(df)) {
        jitter_pos <- position_jitter(width=.4, seed = 1)
        # ggplot(df, aes(xaxis, `log2 intensity`,
        #                label=`KO strain`)) +
        ggplot(df, aes(xaxis, `log2 intensity`)) +
          geom_point(data=dot_df,
                     position=jitter_pos) +
          geom_boxplot(alpha=.3, outlier.shape=NA, fill=NA, color='#993404') +
          scale_color_manual('above threshold', values=c(`TRUE`='#fc4e2a99',
                                                         `FALSE`='#77777733')) +
          guides(color=FALSE) +
          geom_label_repel(data=dot_df,
                           aes(label=label),
                           size=2.5, color='black',
                           fill='#FFFFFF33',
                           box.padding=1,
                           position=jitter_pos) +
          labs(title=sprintf('log2 protein intensities of gene %s\nin KO strains and wt',
                             KOProteinSelection())) +
          ylab('log2 protein intensity') +
          theme_classic() +
          theme(axis.title.x=element_blank())
      }
    }
  })
  output$KO_protein_table = DT::renderDataTable({
    if(length(KOProteinSelection())) {
      DT::datatable(
        KOProteinData() %>%
          select(-matches('.*_GO_.*'),
                 -`gene name`, -`gene product`, -strain,
                 -matches('.*_filtered')) %>%
          left_join(sp_orthologs,
                    by=c('KO strain'='s.pombe')) %>%
          mutate(`KO strain`=ifelse(`KO strain` == 'BIO',
                                    'wild-type',
                                    # 'wt (biolocial rep (n=461))',
                                    sprintf('<a target="_blank" rel="noopener noreferrer" href="https://www.pombase.org/gene/%s">%s</a>',
                                     `KO strain`, `KO strain`))),
        options = list(
          order = list(list(0, 'desc')),
          pageLength = 25
        ),
        rownames=FALSE,
        escape=FALSE) %>%
        DT::formatRound('log2 intensity', 3) %>%
        DT::formatRound('FDR p-value', 3) %>%
        DT::formatRound('log2 fold change', 3)
    }
  })

  output$KO_protein_download <- downloadHandler(
    filename = function() {
      sprintf('PombeKO_perProtein_%s_%s.csv',
              KOProteinSelection(),
              Sys.Date())
    },
    content = function(con) {
      write.table(KOProteinData() %>%
                    left_join(sp_orthologs,
                              by=c('KO strain'='s.pombe')), con, sep='\t', row.names=FALSE)
    }
  )




  # KOCluster1Data <- reactive({
  #   if(input$KO_cluster1 %in% sp_ko_lib$KOstrain) {
  #     selected_cluster <-
  #       unique(sp_ko_lib$som1_cluster[
  #         sp_ko_lib$KOstrain == input$KO_cluster1])
  #   } else {
  #     selected_cluster <- input$KO_cluster1
  #   }
  #   sp_ko_lib %>%
  #     as_tibble() %>%
  #     filter(som1_cluster == selected_cluster,
  #            strain != '461 wt',
  #            log2_intensity > 0) %>%
  #     select(-strain, -mysum, -count) %>%
  #     arrange(desc(log2_intensity)) ->
  #     df
  # })
  # output$KO_cluster1_boxplot <- renderPlot({
  #   df <- KOCluster1Data()
  #   df %>%
  #     group_by(KOstrain) %>%
  #     mutate(label=if_else(
  #       rank(-log2_intensity) %in% 1:input$KO_cluster1_labels,
  #       sprintf('%s - %s', pG, Gene.names),
  #       "")
  #     ) ->
  #     df
  #   jitter_pos <- position_jitter(width=.9, seed = 1)
  #   ggplot(df, aes(1, log2_intensity)) +
  #     geom_point(position=jitter_pos,
  #                color='#77777799') +
  #     geom_boxplot(alpha=.3, outlier.shape=NA, fill=NA, color='#993404')+
  #     guides(color=FALSE) +
  #     geom_label_repel(aes(label=label),
  #                      size=2.5, color='black',
  #                      fill='#FFFFFF33',
  #                      position=jitter_pos) +
  #     facet_wrap(~ KOstrain) ->
  #     bplot
  #   bplot
  # })






#   KOCluster2Data <- reactive({
#     if(input$KO_cluster2 %in% sp_ko_lib$KOstrain) {
#       selected_cluster <-
#         unique(sp_ko_lib$som2_cluster[
#           sp_ko_lib$KOstrain == input$KO_cluster2])
#     } else {
#       selected_cluster <- input$KO_cluster2
#     }
#     sp_ko_lib %>%
#       as_tibble() %>%
#       filter(som2_cluster == selected_cluster,
#              strain != '461 wt',
#              log2_intensity > 0) %>%
#       select(-strain, -mysum, -count) %>%
#       arrange(desc(log2_intensity)) ->
#       df
#   })
#   output$KO_cluster2_boxplot <- renderPlot({
#     df <- KOCluster2Data()
#     df %>%
#       group_by(KOstrain) %>%
#       mutate(label=if_else(
#         rank(-log2_intensity) %in% 1:input$KO_cluster2_labels,
#         sprintf('%s - %s', pG, Gene.names),
#         "")
#       ) ->
#       df
#     jitter_pos <- position_jitter(width=.9, seed = 1)
#     ggplot(df, aes(1, log2_intensity)) +
#       geom_point(position=jitter_pos,
#                  color='#77777799') +
#       geom_boxplot(alpha=.3, outlier.shape=NA, fill=NA, color='#993404')+
#       guides(color=FALSE) +
#       geom_label_repel(aes(label=label),
#                        size=2.5, color='black',
#                        fill='#FFFFFF33',
#                        position=jitter_pos) +
#       facet_wrap(~ KOstrain) ->
#       bplot
#     bplot
#   })
})
