#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
shinyUI(fluidPage(tags$head(tags$style(
  HTML("
       .nav-stacked, navbar-brand { font-size: 80%; }
       div#main_KO_strain_table { font-size: 85%;}
       div#main_KO_strain_table td { padding: 2px 8px; }
       div#main_KO_strain_table t4 { padding: 5px 10px; }
       div#KO_strain_table { font-size: 85%;}
       div#KO_strain_table td { padding: 2px 8px; }
       div#KO_strain_table t4 { padding: 5px 10px; }
       div#main_KO_protein_table { font-size: 85%;}
       div#main_KO_protein_table td { padding: 2px 8px; }
       div#main_KO_protein_table t4 { padding: 5px 10px; }
       div#KO_protein_table { font-size: 85%;}
       div#KO_protein_table td { padding: 2px 8px; }
       div#KO_protein_table t4 { padding: 5px 10px; }
       div#kostrain_cluster_selection_table { font-size: 75%;}
       div#kostrain_cluster_selection_table td { padding: 2px 3px; }
       div#kostrain_cluster_selection_table t4 { padding: 2px 4px; }
       div#protein_cluster_selection_table { font-size: 75%;}
       div#protein_cluster_selection_table td { padding: 2px 3px; }
       div#protein_cluster_selection_table t4 { padding: 2px 4px; }
       div#main_prottrans_table { font-size: 75%;}
       div#main_prottrans_table td { padding: 2px 3px; }
       div#main_prottrans_table t4 { padding: 2px 4px; }
       div#main_KO_strain_cluster_table { font-size: 75%;}
       div#main_KO_strain_cluster_table td { padding: 2px 3px; }
       div#main_KO_strain_cluster_table t4 { padding: 2px 4px; }
       div#main_protein_cluster_table { font-size: 75%;}
       div#main_protein_cluster_table td { padding: 2px 3px; }
       div#main_protein_cluster_table t4 { padding: 2px 4px; }
       .datatables .dataTables_wrapper .dataTables_filter { font-size: 140%; float: left; }
       .datatables .dataTables_wrapper .dataTables_length { float: right; }
       "))),

  shiny::navbarPage(
    id = "tabs",
    shiny::HTML('<em>S. pombe</em> KO Viewer'),
    windowTitle='S. pombe KO Viewer',




    shiny::tabPanel(
      "Welcome",
      shiny::fluidPage(column(
        9,
        shiny::titlePanel(
          HTML("<em>S. pombe</em> KO library viewer"),
          windowTitle="<em>S. pombe</em>  KO library viewer"),
        shiny::HTML('<br /><br />'),
        shiny::h4('Proteome-wide effects of single gene perturbations in a eukaryotic cell'),
        shiny::HTML("<p>Merve Öztürk<sup>1</sup>, Anja Freiwald<sup>1</sup>, Jasmin Cartano<sup>1</sup>, Ramona Schmitt<sup>1</sup>, Mario Dejung<sup>1</sup>, Katja Luck<sup>1</sup>, Sigurd Braun<sup>2</sup>, Michal Levin<sup>1</sup> and Falk Butter<sup>1</sup></p>",
                    "<p><sup>1</sup>Institute of Molecular Biology (IMB), 55122 Mainz, Germany<br /><sup>2</sup>Department of Physiological Chemistry, Biomedical Center, Ludwig-Maximilians University of Munich, Planegg-Martinsried, Germany</p>"),
        shiny::HTML('<p>Eukaryotic gene expression is controlled at the transcriptional, translational and protein degradation level. While transcriptional outcomes, which are commonly also used as a proxy for protein abundance, have been investigated on a larger scale, the study of translational output requires large-scale proteomics data. We here determined the individual proteome changes for  3,308 non-essential genes in the yeast S. pombe. By similarity clustering of proteome changes, we infer gene functionality that can be extended to other species such as human or baker’s yeast. We observed that genes with high proteome remodeling are predominantly involved in gene expression regulation, in particular acting as translational regulators. Focusing on the knockout strains with a large number of altered proteins, we performed paired transcriptome/proteome measurements to uncover translational regulators and features of translational regulation. </p>'),
        shiny::HTML('<p>Five data types can be explored on this website:<br />',
                    '<ol>',
                    '<li>The effect of the knockdown of single genes on the protein levels of other genes - <a href="?data=KOLibrary&data2=searchByKOStrain">LINK</a></li>',
                    '<li>The levels of single proteins across different knockout strains - <a href="?data=KOLibrary&data2=searchByProtein">LINK</a></li>',
                    '<li>Clusters of knockout strains with similar effects on protein levels of target genes - <a href="?data=KOLibrary&data2=clusterKOStrain">LINK</a></li>',
                    '<li>Clusters of genes with similar effects upon knockdown of single genes - <a href="?data=KOLibrary&data2=clusterProtein">LINK</a></li>',
                    '<li>Correlations between changes on the mRNA and protein level of target genes upon knockdown of single genes - <a href="?data=comparisonProteomTranscriptom">LINK</a></li>',
                    '</ol>',
                    '</p><br /><br />'),
        shiny::h4('Data acquisition: Proteomics screen with thousands of single gene deletion strains'),
        shiny::HTML('<p>To systematically investigate the effect of individual gene deletions on the proteome, we used the S. pombe haploid knockout collection with 3,308 individually deleted genes <a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3962850/">[Kim et al., Nat Biotechnol. 2010]</a>. We took advantage of the 96-well array format of the deletion library (Bioneer, version 3.0) by combining eight knockout strains with 2 different controls per mass spectrometry (MS) measurement (Figure 1a) and assessed the proteome of the 3,308 knockout strains by quantitative proteomics using 10plex TMT (tandem mass tag) <a href="https://pubmed.ncbi.nlm.nih.gov/12713048/">[Thompson et al., Anal Chem. 2002]</a> amounting to 469 MS runs (panel a). One of the controls served as technical control required for run-to-run normalization and was generated by growing a large batch of S. pombe wild-type cells. The other biological controls were S. pombe wild-type replicates, grown alongside the knockout strains to be able to judge biological protein expression level variation in wild-type cells (see publication for comprehensive information on normalization approach).</p>'),
        shiny::HTML('<p>Overall we quantified 2,921 proteins (56 % of protein-coding genes), with a mean of 1,596 proteins per strain, ranging from 1,369 to 1,996 proteins (panel b). Of the 2,921 quantified proteins, 985 (34%) could be quantified in at least 90% of the knockout strains and 1,548 (53%) in at least 50% of the knockout strains.</p>'),
        shiny::HTML('<p><img src="entrypage01.png" width="600px"/></p>'),
        shiny::HTML('<p><strong>Proteomics screen to study proteome expression in a knockout library.</strong>',
                    '<strong>a</strong>, Schematics of the experimental design for quantifying protein expression levels in the S. pombe knockout collection by quantitative proteomics using 10plex TMT. Each MS run contained a technical (127C label) and a biological control (130N label).',
                    '<strong>b</strong>, Normalized protein expression levels of 3,308 knockout strains and 461 wild-type replicates. The complete dataset contains quantification for 2,921 protein-coding genes of which 513 proteins were quantified across all samples.',
                    '</p>'),
        shiny::HTML('<br /><br /><br />'),
        shiny::HTML('<p><strong>An explanation on how to use our web interface can be downloaded here:
                    <a href="How_tos_spombe_website_12122022.pdf" target="_blank">"How To"</a></strong><br /><br /><br /><br /></p>'),
        shiny::h4('Data access:'),
        shiny::HTML('<p>Proteome data has been uploaded to <a href="http://www.proteomexchange.org" target="_blank">proteomeXchange</a> with the dataset identifiers PXD024332 (genome-wide screen) and PXD024383 (94 gene set). Transcriptomics data has been submitted to <a href="https://www.ncbi.nlm.nih.gov/geo/" target="_blank">GEO</a> with the dataset identifier GSE167543  (94 gene set).<br /><br /></p>')
        ))
    ),

# Whole KO library --------------------------------------------------------


    shiny::tabPanel(
      'KO Library',
      value='KOLibrary',
      shiny::HTML('<h3>Proteome Data</h3>'),
      shiny::tabsetPanel(
        type="tabs",
        id = "tabs2",


        shiny::tabPanel(
          "Search by KO strain",
          value='searchByKOStrain',
          verticalLayout(
            fluidRow(
              column(8,
                     shiny::p(
                       'In this section the effects of the knock-out of individual',
                       'genes can be explored. Enter a gene name or select a gene',
                       'from the list below to see the protein intensities of all',
                       'measured proteins upon knockout (left panel) or the',
                       'differential regulation when compared to wild-type',
                       '(right panel).'),
                     DT::dataTableOutput('main_KO_strain_table')

              ),
              column(4,
                     selectizeInput("KO_strain_protein", "Highlight protein of interest:",
                                    multiple=TRUE,
                                    choices=NULL),
                     selectizeInput("KO_strain_gene", "Highlight gene of interest:",
                                    choices=NULL,
                                 multiple=TRUE),
                     downloadLink('KO_strain_download',
                                  'Download differential regulation statistics table')
              )
            ),
            htmlOutput('KO_strain_print'),
            fluidRow(
              column(4,
                     plotly::plotlyOutput('KO_strain_plot', height='800px'),
                     plotly::plotlyOutput('KO_strain_scatter', height='800px')
              ),
              column(8,
                     DT::dataTableOutput('KO_strain_table')
              )
            )
            )
        ),



        shiny::tabPanel(
          "Search by protein",
          value='searchByProtein',
          verticalLayout(
            fluidRow(
              column(8,
                     shiny::p(
                       'In this section the protein intensities of individual proteins',
                       'across KO strains can be explored. Enter a protein name or',
                       'select one from the list below to see the protein intensities',
                       'of your protein of interest in all knockout strains (KO strains)',
                       'and in all wild-type replicates (wt (n=461))'),
                     DT::dataTableOutput('main_KO_protein_table')
              ),
              column(4,
                     # sliderInput('KO_protein_threshold', 'detected threshold:',
                     #             6.5, 23, 14.5, .1),
                     sliderInput('KO_protein_labels', 'label strains',
                                 1,30,6, step=1),
                     downloadLink('KO_protein_download',
                                  'Download differential regulation statistics table')
              )
            ),
            htmlOutput('KO_protein_print'),
            fluidRow(
              column(6,
                     shiny::plotOutput('KO_protein_boxplot', height='800px')
              ),
              column(6,
                     DT::dataTableOutput('KO_protein_table')
              )
            )
          )
        ),





        shiny::tabPanel(
          "KO strain clusters",
          value='clusterKOStrain',
          shiny::fluidRow(
            shiny::column(8,
                          DT::dataTableOutput('main_KO_strain_cluster_table')
                          ),
            shiny::column(4,
                          shiny::p(
                            'In this section the clusters established by KO strain',
                            'intensities across proteins can be explored. Proteins',
                            'from the same cluster are depicted as black dots (nodes).',
                            'Established String db interactions are depicted with grey',
                            'lines (edges)'),
                          downloadLink('KO_cluster_download',
                                       'Download full KOStrain cluster table'))
            ),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              htmlOutput("kostrain_celltext")
              # htmlOutput("kostrain_stringDB")
            ),
            shiny::mainPanel(
              DT::dataTableOutput('kostrain_cluster_selection_table'),
              htmlOutput("kostrain_networkimg"),
              htmlOutput("kostrain_stringDB")
            )
          )
        ),



        shiny::tabPanel(
          "Protein clusters",
          value='clusterProtein',
          shiny::fluidRow(
            shiny::column(8,
                          DT::dataTableOutput('main_protein_cluster_table')
            ),
            shiny::column(4,
                          shiny::p(
                            'In this section the clusters established by protein',
                            'intensities across KO strains can be explored. Proteins',
                            'from the same cluster are depicted as black dots (nodes).',
                            'Established String db interactions are depicted with grey',
                            'lines (edges)'),
                          downloadLink('Protein_cluster_download',
                                       'Download full Protein cluster table'))
          ),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              htmlOutput("protein_celltext")
              # htmlOutput("protein_stringDB")
            ),
            shiny::mainPanel(
              DT::dataTableOutput('protein_cluster_selection_table'),
              htmlOutput("protein_networkimg"),
              htmlOutput("protein_stringDB")
            )
          )
        )
      )
    ),


# 94 selected KO celllines ------------------------------------------------


shiny::tabPanel(
  'Proteome-Transcriptome Comparisons',
  value='comparisonProteomTranscriptom',
  verticalLayout(
    fluidRow(
      column(8,
             shiny::p(
               'In this section the changes in expression on the transcriptome',
               'and the proteome level upon knock-out of individual genes can be',
               'explored. Enter a gene name or select a gene from the list below',
               'to see the effect of the knock-out of the protein of interest on',
               'the transcription levels of relevant target genes (x-axis) in',
               'comparison to the changes in their protein intensities (y-axis).',
               'The correlation distribution is ploted in the lower right.'),
             DT::dataTableOutput('main_prottrans_table'),
             actionButton('clear1', 'Clear Rows')
      ),
      column(4,
             shinyWidgets::sliderTextInput("trans_pvalue","Transcript p-value:",
                                           choices=c(1, 0.5, 0.2, 0.1, 0.05, 0.01, 0.005, 0.001),
                                           selected=1, grid = T),
             shinyWidgets::sliderTextInput("prot_pvalue","Protein p-value:",
                                           choices=c(1, 0.5, 0.2, 0.1, 0.05, 0.01, 0.005, 0.001),
                                           selected=1, grid = T),
             # selectInput("strain", "Strain:",
             #             unique(sp_prot_trans$KOstrain),
             #             multiple=TRUE,
             #             selected=unique(sp_prot_trans$KOstrain)[1:4]),
             selectizeInput("highlight", "highlight protein:",
                            choices=NULL,
                            multiple=TRUE)
      )
    ),
    fluidRow(
      column(9,
             plotly::plotlyOutput("distPlot")
      ),
      column(3,
             plotly::plotlyOutput("correlationPlot")
      )
    )
  )
),




# Impressum ---------------------------------------------------------------


    shiny::tabPanel(
      "Impressum",
      value='test',
      tags$head(tags$style(
        HTML(".nav-stacked, navbar-brand { font-size: 75%; }"
             )
        )
      ),
      shiny::fluidPage(
        shiny::titlePanel(
          HTML("Impressum/Contact"),
          windowTitle="<em>S. pombe</em>  KO library viewer"),
        shiny::HTML('<p>AG Butter<br />
                                    Ackermannweg 4<br />
                                    55129 Mainz<br />
                                    Germany</p>'),
        shiny::h4('Acknowledgements:'),
        shiny::p('Hosting of this webpage is supported by  Zentrum für Datenverarbeitung (ZDV), University of  Mainz.')
      )
    )


  )
))
