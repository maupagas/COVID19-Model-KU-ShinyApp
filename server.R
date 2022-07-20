# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$header <- renderUI({
    if (as.numeric(input$language) == 1){
      tags$div(tags$img(src='ku_logo.png', height="105px", width="353px", align="middle"),
               tags$h1(tags$b("Model Simulator COVID-19"), style = "display:inline; vertical-align:middle; horizontal-align:middle; margin-left: 200px"),
               tags$br(),
               tags$p("This is a simulator of the COVID-19 outbreak based on possible interventions. Full details of the model can be found in:",
                      tags$br(),"Rodr\u00EDguez J, Pat\u00F3n M, Uratani JM and  Acu\u00F1a JM (2021).
                      Modelling the impact of interventions on the progress of the 
                      COVID-19 outbreak including age segregation. PLoS ONE 16(3): e0248243.", tags$a(href = "https://doi.org/10.1371/journal.pone.0248243", "Link to the paper."), tags$br(),
                      "The model evaluates transition between infection stages for a population group assumed to be well mixed with no geographical distribution.
                      Four interventions can be evaluated: (1) Level of social isolation, (2) Level of use of PPE, (3) Number of ICU available and (4) Level of testing.",
                      tags$br(), "Full details of the model and parameters currently used are available on request at",
                      tags$a(href = "mailto:jorge.rodriguez@ku.ac.ae", "jorge.rodriguez@ku.ac.ae."),
                      "The source code of the model can be also",
                      tags$a(href = "https://github.com/EnvBioProM/COVID_Model", "downloaded from Github.")),
               tags$p(tags$b("IMPORTANT NOTE:", style = "color: #b10a05"), tags$b("This tool is for illustrative purposes only and 
          has not been calibrated. It is not to be used for prediction purposes."))
      )
    } else if (as.numeric(input$language) == 2){tags$div(
      tags$div(tags$img(src='ku_logo.png', height="105px", width="353px", align="middle"),
               tags$h1(tags$b("Modelo de Simulaci\u00F3n del COVID-19"), style = "display:inline; vertical-align:middle; horizontal-align:middle; margin-left: 200px")), 
      
      #Main Text
      tags$p("Este es un simulador del brote epid\u00E9mico del COVID-19 basado en posibles actuaciones. Los detalles completos del modelo se pueden encontrar en:",
             tags$br(),"Rodr\u00EDguez J, Pat\u00F3n M, Uratani JM and  Acu\u00F1a JM (2021). 
            Modelling the impact of interventions on the progress of the 
                      COVID-19 outbreak including age segregation. PLoS ONE 16(3): e0248243.", tags$a(href = "https://doi.org/10.1371/journal.pone.0248243", "Enlace a la publicaci\u00F3n."), tags$br(),
             "El modelo muestra la transici\u00F3n entre etapas de la infecci\u00F3n para un grupo de poblaci\u00F3n que se asume bien mezclado sin distribuci\u00F3n geogr\u00E1fica adicional alguna.
            Se pueden evaluar cuatro posibles interevenciones: (1) Nivel de aislamiento social, (2) Nivel de uso de material personal de protecci\u00F3n, 
            (3) N\u00FAmero disponible de UCIs y (4) el porcentaje de la poblacion testada.",
             tags$br(), "Para obtener los detalles completos del modelo y los par\u00E1metros utilizados contactar a",
             tags$a(href = "mailto:jorge.rodriguez@ku.ac.ae", "jorge.rodriguez@ku.ac.ae."), 
             "El c\u00F3digo completo del modelo tambi\u00E9n puede ser",
             tags$a(href = "https://github.com/EnvBioProM/COVID_Model", "descargado desde Github.")),
      tags$p(tags$b("AVISO IMPORTANTE:", style = "color: #b10a05"), tags$b("Esta herramienta es solo para prop\u00F3sitos ilustrativos y no ha sido calibrada. No est\u00E1 hecha para realizar predicciones."))
    )}
    
  })
  
  
  
  # Get ALL UI Buttons here
  output$headerSidebar<- renderUI({
    #Describe the side panel use
    if(input$language == 1){
      tags$p(tags$b("Fill in a number for each field within the declared range [min-max].",
                    tags$br(),"Select also the second figure to plot."),tags$br())}
    else if(input$language == 2){
      tags$p(tags$b("Rellenar con un n\u00FAmero cada campo dentro del rango establecido [min-max].",
                    tags$br(),"Seleccionar tambi\u00E9n la segunda figura que representar."),tags$br())
    }
  })
  
  # Simulation Time
  output$tFinal <- renderUI({
    if (as.numeric(input$language) == 1)    {simTimeLabel = "Length of simulation in days - (tFinal, [10-1000]):"} 
    else if(as.numeric(input$language) == 2){simTimeLabel = "Tiempo de simulaci\u00F3n en d\u00EDas - (tFinal, [10-1000]):"}
    numericInput(inputId = "tFinal", label = simTimeLabel, min = 10, max = 1000, value = 365)})
  
  
  # Number of critical care beds per million people
  
  output$capICU <- renderUI({
    if (as.numeric(input$language) == 1)    {ICULabel = "How many ICUs are available per million patient? - (capIC, [0-10000])"} 
    else if(as.numeric(input$language) == 2){ICULabel = "Cu\u00E1ntas UCIs est\u00E1n disponibles por millones de pacientes? - (capIC, [0-10000])"}
    numericInput(inputId = "capICU", label = ICULabel, min = 0, max = 10000, value = 261)})
  
  ##############################################################################################
  
  # Input5: Number of daily interindividual interactions of healthy people
  output$ni_h <- renderUI({
    if (as.numeric(input$language) == 1)    {ni_hLabel = "Number of daily interindividual interactions of healthy people: - (ni_h, [0-100])"} 
    else if(as.numeric(input$language) == 2){ni_hLabel = "N\u00FAmero de interacciones individuales de gente sana: - (ni_h, [0-100])"}
    numericInput(inputId = "ni_h", label = ni_hLabel, min = 0, max = 100, value = 2.5)})
  
  output$lpa <- renderUI({
    if (as.numeric(input$language) == 1)    {lpaLabel = "Level of personal protection and awareness of people: - (lpa, [0-1])"} 
    else if(as.numeric(input$language) == 2){lpaLabel = "Nivel de protecci\u00F3n y concienciaci\u00F3n de la poblaci\u00F3n: - (lpa, [0-1])"}
    # Input6: Level of personal protection and awareness of 
    numericInput(inputId = "lpa", label = lpaLabel, min = 0, max = 1, value = 0.75)})
  
  output$pTest <- renderUI({ 
    if (as.numeric(input$language) == 1)    {pTestLabel = "Percentage of population tested: - (pTest, [0-100]):"} 
    else if(as.numeric(input$language) == 2){pTestLabel = "Porcentaje de la poblaci\u00F3n testada: - (pTest, [0-100]):"}
    # Input7: Reduction factor of daily interactions by pre-symptomatic
    numericInput(inputId = "pTest", label = pTestLabel, min = 0, max = 100, value = 0)})
  
  # Plot selectors
  output$numOfPlot <- renderUI({ 
    if (as.numeric(input$language) == 1)    {numOfPlotLabel = "Select variable to represent in the second figure"
    choicesPlot    = list("Number of cases" = 1, "Number of critical cases" = 2, "Number of fatalities" = 3) } 
    else if(as.numeric(input$language) == 2){numOfPlotLabel = "Seleccionar variable a representar en la segunda figura";
    choicesPlot = list("N\u00FAmero de casos" = 1, "N\u00FAmero de casos cr\u00EDticos"  = 2, "N\u00FAmero de fallecimientos" = 3)}
    if (as.numeric(input$language) == 1)    {AgesToPlotLabel = "Select total number or distributed per age for the second figure"
    choicesAges    = list("Number of cases per age" = 1, "Total number of cases" = 2);} 
    else if(as.numeric(input$language) == 2){AgesToPlotLabel = "Seleccionar representaci\u00F3n por edades o total";
    choicesAges = list("N\u00FAmero de casos por edad" = 1, "N\u00FAmero total de casos" = 2)}
    
    div(style="display: inline-block;",
        radioButtons(inputId = "numOfPlot", label = numOfPlotLabel,
                     choices = choicesPlot,
                     selected = 1, width = '400px'),
        
        div(style="display: inline-block;",
            radioButtons(inputId = "AgesToPlot", label = AgesToPlotLabel,
                         choices = choicesAges,
                         selected = 1), width = '400px'))})
  
  
  
  observeEvent(input$ni_h,{
    
  ############### Plot #######################################################
  output$COVID <- renderPlot({
    
    # This function is made to filter automatically all the values to be used later on
    filterVals <- function(x, simTime){
      x_df = as.data.frame(x)
      colnames(x_df) = NULL;
      x_df <- data.frame(simTime, x_df)
      xFilt <- melt(x_df, id = "simTime")
      return(xFilt)
    }
    
    # Add extra lines plot function
    addExtraLines <- function(var, numRes, lineColor){
      ## Code for ggplot2
      varPlot = geom_line(aes(y = var, color = lineColor));   
    }
    
    ###################################################################################################
    # General theme for the plots
    My_Theme = theme(axis.title.x = element_text(size = 16, face="bold"),
                     axis.text.x = element_text(size = 14),
                     axis.title.y = element_text(size = 16, face="bold"),
                     axis.text.y = element_text(size = 14),
                     axis.line = element_line(colour = 'black', size = 1),
                     axis.ticks = element_line(colour = "black", size = 1),
                     panel.background = element_rect(fill = "white",  size = 1, linetype = "solid"), #add colour = black if the rest of the border is desired
                     panel.grid.major = element_line(size = 0.5,  linetype = 'solid', colour = "gray"), 
                     # panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
                     legend.position = "right",
                     legend.title = element_text(size=16, face="bold"),
                     legend.text = element_text(size = 14),
                     legend.text.align = 0)
    
    
    
    colorLines = c(rgb(0,113.985,188.955,      maxColorValue = 255),
                   rgb(216.75,82.875,24.99,    maxColorValue = 255),
                   rgb(236.895,176.97,31.875,  maxColorValue = 255),
                   rgb(125.97,46.92,141.78,    maxColorValue = 255),
                   rgb(118.83,171.87,47.94,    maxColorValue = 255),
                   rgb(76.755,189.975,253.215, maxColorValue = 255),
                   rgb(161.925,19.89,46.92,    maxColorValue = 255),
                   rgb(0,113.985,188.955,      maxColorValue = 255),
                   rgb(216.75,82.875,24.99,    maxColorValue = 255))
    
    axisProperties = scale_colour_manual(values  = colorLines)  
    loadXlsx = 0;
    # #Age groups per name
    nameAgeGroups   = c("0-9                    ",
                        "10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+");  
    if(loadXlsx  == 0){
      # Load for the first time the parameters from excel
      #Name of file to load parameters from
      fileName = "pbmCOVID_v2.3.xlsx"
      
      # 1. Load General Parameters
      # Assign variable names from first column
      GenParam <- openxlsx::read.xlsx(fileName, colNames = F, rowNames = T)
      loadXlsx = 1;    
    }
    
    # Convert the data frame into columns
    auxM = t(GenParam)
    GenP = as.data.frame(auxM)
    simTime = seq(0,input$tFinal,1);
    nAG = length(IntP$ni_h)
    
    ## INITIAL MODEL STATES FROM PARAMETERS
    # POPULATION SIZE AND DISTRIBUTION PER AGE
    # Number of individuals per status and age range (data used for Comunidad de Madrid in 2018)
    # Agegroups 0s  10s 20s 30s 40s 50s 60s 70s 80+ 
    Nt_ini  <- EpiP$Nt;
    Nhn_ini = Nt_ini * EpiP$fhn_t;
    Nh_ini  = Nt_ini - Nhn_ini;
    Nps_ini = (EpiP$pzM * 1e-6) * Nt_ini;

    
    # Initial numbers of population types and per age groups. 
    # All in a single row vector 1x(8xnAG)
    nAG = length(EpiP$Nt);
    stNini = rep(0, 8 * nAG);
    stNini[c(1:nAG)]         = Nhn_ini;
    stNini[seq(nAG+1,nAG*2,1)]   = Nh_ini;
    stNini[seq(2*nAG+1,nAG*3,1)] = Nps_ini;
    
    #Labels
    if(input$language == 1){
      stageLabel = "Stage";
      AgeGroupLabel = 'Age Group';
      yNumCasesLabel = "Number of cases (%)";
      totNumCaseslabel = "Total # of cases";
      yNumCritCasesLabel = "Number of critical cases (%)";
      totNumCritCasesLabel = "Total # of critical cases";
      yNumFatalLabel = "Number of fatalities (%)";
      totNumFatalLabel = "Total # of fatalities";
      populationStage = c("Non-susceptible", "Healthy", "Pre-symptomatic", "Symptomatic", "Hospitalised",   "Critical", "Deceased",        "Recovered");
      timeLabel = "Time (d)";  
      populationStageLabel = " Percentage of population at each stage (%)";
    } else if(input$language == 2){
      #Plot1
      stageLabel           = "Etapa";
      AgeGroupLabel        = 'Grupo de edad';
      yNumCasesLabel       = "N\u00FAmero de casos (%)";
      totNumCaseslabel     = "N\u00FAmero total de casos";
      yNumCritCasesLabel   = "N\u00FAmero de casos cr\u00EDticos (%)";
      totNumCritCasesLabel = "N\u00FAmero total de casos cr\u00EDticos";
      yNumFatalLabel       = "N\u00FAmero de fallecidos (%)";
      totNumFatalLabel     = "N\u00FAmero total de fallecidos";
      populationStage      = c("No susceptible", "Sanos", "Pre-sintom\u00E1ticos", "Sintom\u00E1ticos", "Hospitalizados",   "Cr\u00EDticos", "Fallecidos", "Recuperados");
      timeLabel           = "Tiempo (d\u00EDas)";  
      populationStageLabel = "Porcentaje de poblaci\u00F3n en cada etapa (%)";
    }
    
    
    ##### Here define all the inputs in EpiP, InP or GenP that need to be updated
    # Simulation time
    # tFinal = input$tFinal;

    orig_lpa_h = 0.75;     orig_lpa_ps = orig_lpa_h;        orig_lpa_s = orig_lpa_h;
    orig_pTest = 0;        orig_rfi_ps = 1 - orig_pTest;    orig_rfi_s   = 1 - orig_pTest;
    
    input_rfi_as = 1 - input$pTest/100;
    input_rfi_s  = 1 - input$pTest/100;
    
    # Interaction parameters  
    # language         = input$lang;
    
    IntP$ni_h[1:nAG] = input$ni_h;      
    GenP$capICpM     = input$capICU;
    
    # Adjustment if it goes over maximum
    IntP$lpa_h  = input$lpa * orig_lpa_h;  if (max(IntP$lpa_h)>1)  { IntP$lpa_h  = IntP$lpa_h /max(IntP$lpa_h);   }   
    IntP$lpa_ps = input$lpa * orig_lpa_ps; if (max(IntP$lpa_ps)>1) { IntP$lpa_ps = IntP$lpa_ps /max(IntP$lpa_ps); }  
    IntP$lpa_s  = input$lpa * orig_lpa_s;  if (max(IntP$lpa_s )>1) { IntP$lpa_s  = IntP$lpa_s  /max(IntP$lpa_s ); }  
    
    IntP$rfi_ps = input_rfi_as * orig_rfi_ps;  if (max(IntP$rfi_ps)>1) { IntP$rfi_ps = IntP$rfi_ps /max(IntP$rfi_ps); }   
    IntP$rfi_s  = input_rfi_s  * orig_rfi_s ;  if (max(IntP$rfi_s)>1)  { IntP$rfi_s  = IntP$rfi_s  /max(IntP$rfi_s);  }   
    
    # Group all parameters to execute ode45
    allParameters = list(GenP, IntP, EpiP);
    stNt <- ode(stNini, times = simTime, newModelCOVID, parms = allParameters, method="ode23")
    
    # Obtain results from ode simulation
    
    #Calculate number of total cases
    
    # Naming states and vectors management.
    Nhn_t = stNt[, 1+ seq(1,nAG,1)];             Nh_t  = stNt[,1+ seq(1*nAG +1 ,2*nAG,1)]; 
    Nps_t = stNt[, 1+ seq(2*nAG +1 ,3*nAG,1)];   Ns_t  = stNt[,1+ seq(3*nAG +1 ,4*nAG,1)];
    Nsh_t = stNt[, 1+ seq(4*nAG +1 ,5*nAG,1)];   Nsc_t = stNt[,1+ seq(5*nAG +1 ,6*nAG,1)];   
    Nd_t  = stNt[, 1+ seq(6*nAG +1 ,7*nAG,1)];   Nr_t  = stNt[,1+ seq(7*nAG +1 ,8*nAG,1)]; 
    
    # Prepare data for Results
    
    #Number of state
    # Totals per state (all age groups added).
    NhnT = rowSums(Nhn_t);   NhT  = rowSums(Nh_t);
    NpsT = rowSums(Nps_t);   NsT  = rowSums(Ns_t);
    NshT = rowSums(Nsh_t);   NscT = rowSums(Nsc_t);
    NdT  = rowSums(Nd_t);    NrT  = rowSums(Nr_t);
    
    ### Compile everything in a list ######
    
    St = list(Nhn = Nhn_t, Nh = Nh_t, Nps = Nps_t, Ns = Ns_t, 
              Nsh = Nsh_t, Nsc = Nsc_t, Nd = Nd_t, Nr = Nr_t);
    
    StT = list(NhnT = NhnT, NhT = NhT, NpsT = NpsT, NsT = NsT,
               NshT = NshT, NscT = NscT, NdT = NdT, NrT = NrT)
    
    # Number of active cases in either stage of infection over time for simulation i.
    nCasesT  = StT$NpsT + StT$NsT + StT$NshT + StT$NscT;
    nCasesAG = St$Nps   + St$Ns   + St$Nsh   + St$Nsc;
    
    colnames(nCasesAG) = nameAgeGroups;
    
    # Maximum of total critical cases for simulation i.
    maxSC = max(StT$NscT);
    # Total final number of fatalities for simulation i.
    NdF = tail(St$Nd, 1);
    
    # Storage of ALL of Outputs.
    Out = list(stNt = stNt, St = St, StT = StT, nCasesAG = nCasesAG, nCasesT = nCasesT, maxSC = maxSC, NdF = NdF);
    
    ### Enf Of Other Results ######
    filterAG = input$AgesToPlot; # Select to plot total (0) or per age (1) results.  
    numPlot  = input$numOfPlot;  # Select one of the 3 plots: 1) Number of cases; 2) Number of critical case; 3) Number of fatalities
    
    
    #######################################################################################################################
    
    #1. Total population at each stage
    StT_filtered <- filterVals(StT, simTime)
    colnames(StT_filtered)[2] = "Stage";
    StT_filtered$Stage <- mapvalues(StT_filtered$Stage, from = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"), to = populationStage)
    StT_Plot <- ggplot()  + geom_line(data = StT_filtered, aes(x = simTime, y = value, color = Stage), size = 1) +
      ylab(populationStageLabel) + xlab(timeLabel) +
      My_Theme  + axisProperties  +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,20), expand = expansion(mult = c(0, 0.1))) + 
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      theme(legend.position = "right") + 
      labs(color=stageLabel)
    
    # output$info <- renderText({
    #   paste0("x=", round(input$plot_click$x,0 * input$tFinal), "\ny=", round(input$plot_click$y * 100, 0))
    # })
    # 
    # REST OF PLOTS
    
    if(numPlot == 1){
      
      # Plot 2: Active cases over time
      if (filterAG == 1){
        nCasesFilt     <- filterVals(nCasesAG, simTime)
        colnames(nCasesFilt)[2] = "AgeGroup";
        nCasesFilt$AgeGroup <- mapvalues(nCasesFilt$AgeGroup, from = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9"), to = nameAgeGroups)
        nCasesPlot <- ggplot()  + geom_line(data = nCasesFilt, aes(x = simTime, y = value, color = AgeGroup), size = 1) +
          ylab(yNumCasesLabel) + xlab(timeLabel) +
          My_Theme  + axisProperties  +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
          scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
          scale_fill_manual(labels=nameAgeGroups) + 
          labs(color=AgeGroupLabel)
      } else{
        nCasesFilt   <- as.data.frame(nCasesT)
        nCasesPlot <- ggplot()  + geom_line(data = nCasesFilt, aes(x = simTime, y = nCasesT, color = "steelblue"), size = 1)+
          ylab(yNumCasesLabel) + xlab(timeLabel) +
          My_Theme  + axisProperties  +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
          scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
          scale_colour_discrete(name  ="", label = totNumCaseslabel)
      }
      
      # Unified name of the variable to plot
      plotResults <-  nCasesPlot;
      
    } else if (numPlot == 2){
      
      #Plot 5: Critical cases over time
      if (filterAG == 1){
        ncritCasFilt   <- filterVals(Out$St$Nsc, simTime)
        colnames(ncritCasFilt)[2] = "AgeGroup";
        ncritCasFilt$AgeGroup <- mapvalues(ncritCasFilt$AgeGroup, from = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9"), to = nameAgeGroups)
        plotCritCases <- ggplot()  + geom_line(data = ncritCasFilt, aes(x = simTime, y = value, color = AgeGroup), size = 1) +
          ylab(yNumCritCasesLabel) + xlab(timeLabel) +
          My_Theme + axisProperties  +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
          scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
          labs(color=AgeGroupLabel)
      } else{
        ncritCasFilt <- as.data.frame(Out$StT$Nsc)
        plotCritCases <- ggplot()  + geom_line(data = ncritCasFilt, aes(x = simTime, y = Out$StT$Nsc, fill=Out$StT$Nsc, color = "bluesteel"), size = 1)+
          ylab(yNumCritCasesLabel) + xlab(timeLabel) +
          My_Theme  + axisProperties  +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
          scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
          scale_colour_discrete(name  ="", label = totNumCritCasesLabel)
      }
      plotResults <-  plotCritCases;
      
      
    }else if(numPlot == 3){
      # Plot 6: Total fatalities over time
      if (filterAG == 1){
        nFatalFilt     <- filterVals(Out$St$Nd, simTime)
        colnames(nFatalFilt)[2] = "AgeGroup";
        nFatalFilt$AgeGroup <- mapvalues(nFatalFilt$AgeGroup, from = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9"), to = nameAgeGroups)
        plotFatalCases <- ggplot()  + geom_line(data = nFatalFilt, aes(x = simTime, y = value, color = AgeGroup), size = 1) +
          ylab(yNumFatalLabel) + xlab(timeLabel) +
          My_Theme + axisProperties +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
          scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
          labs(color=AgeGroupLabel)
      } else{
        nFatalFilt <- as.data.frame(Out$StT$Nd)
        plotFatalCases <- ggplot()  + geom_line(data = nFatalFilt, aes(x = simTime, y = Out$StT$Nd, color = 'bluesteel'), size = 1)+
          ylab(yNumFatalLabel) + xlab(timeLabel) +
          My_Theme  + axisProperties  +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
          scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
          scale_colour_discrete(name  ="", label = totNumFatalLabel) 
      }
      plotResults <-  plotFatalCases;
    }
    
    # Plot the to figures aligned 
    plot_grid(StT_Plot, plotResults, align = "v", nrow = 2)
    
  }, height = 620, width = 800)
  

  
  
  
})
  
  
}
 



