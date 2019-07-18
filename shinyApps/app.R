library(shiny)
library(tidyverse)
library(markdown)
library(usmap)
library(maps)

if(!file.exists("Data")){dir.create("Data")}
asthma <- read_csv("./Data/Asthma2017.csv", guess_max = 69000)

health2017_narm <- asthma %>%
  filter(!is.na(AsthmaStatus))

curr_ast_17 <- asthma %>%
  filter(AsthmaStatus == "Current")

asthma2017 <- asthma %>%
  filter(AsthmaStatus == "Current" | AsthmaStatus == "Former")

#For Map
CurrAsthmaState <- curr_ast_17 %>%
  group_by(State) %>%
  summarise(CurrAsthmaCount = n())

SampleState <- health2017_narm %>%
  group_by(State) %>%
  summarize(SampleCount = n())

ByState <- as_tibble(cbind(CurrAsthmaState, "TotalSampled" = SampleState$SampleCount))

StateName = as.list(cdlTools::fips(ByState$State, to = "Name"))

StateName = replace(StateName, StateName == "Deleware", "Delaware")

ByState <- ByState %>%
  mutate(PercentByState = ByState$CurrAsthmaCount/ByState$TotalSampled*100,
         StateName = StateName,
         StateWithPercent = paste(StateName, as.character(PercentByState), "%")) %>%
  select(State, StateName, CurrAsthmaCount, TotalSampled, PercentByState) %>%
  rename(`State Name` = StateName,
         `Respondents with Asthma`= CurrAsthmaCount,
         `Total Respondents`=TotalSampled ,
         `Asthma Percentage`=PercentByState)

ByState$fips = ByState$State

#Conditional Probability
Conditional_Probability <- 
  function(variable, VarProb, InterProp, df1 = health2017_narm, df2 = asthma2017) {
    
    variable <- enquo(variable)
    
    VarProb <- df1 %>%
      group_by(!!variable) %>%
      summarise(VariableProbability = n()/450016*100)
    
    InterProb <- df2 %>%
      group_by(!!variable) %>%
      summarise(IntersectionProbability = n()/450016*100)
    
    CombinedTable <- cbind(VarProb, InterProb[,2])
    
    ConditionalProbability <- CombinedTable %>%
      mutate(AsthmaPrevalence = round(IntersectionProbability/VariableProbability*100, 2)) %>%
      select(!!variable, AsthmaPrevalence) %>%
      arrange(desc(AsthmaPrevalence))
    
    return(ConditionalProbability)
  }

#GLM
asthma_lm <- asthma %>% 
  dplyr::select(AsthmaStatus, AgeGroup, Sex, Race, IncomeGroup, EducationLevel, BMICategory, CurrentSmoker, Exercise, DepressiveDisorder) %>% 
  filter(!is.na(AsthmaStatus), !is.nan(AsthmaStatus)) %>%
  mutate(AsthmaStatus = replace(AsthmaStatus, AsthmaStatus == "Current", 1),
         AsthmaStatus = replace(AsthmaStatus, AsthmaStatus == "Former", 0),
         AsthmaStatus = replace(AsthmaStatus, AsthmaStatus == "Never", 0)) %>%
  mutate(BMICategory = replace(BMICategory, BMICategory == "Underweight", "Underweight (< 18.5)"),
         BMICategory = replace(BMICategory, BMICategory == "Normal", "Normal (18.5 - 24.9)"),
         BMICategory = replace(BMICategory, BMICategory == "Overweight", "Overweight (25 - 29.9)"),
         BMICategory = replace(BMICategory, BMICategory ==  "Obese", "Obese (> 29.9)")) %>%
  as_factor()

asthma_lm$AsthmaStatus <- as.numeric(asthma_lm$AsthmaStatus)

asthma_lm$Race <- relevel(as_factor(asthma_lm$Race), ref="Asian")

asthma_lm$EducationLevel <- relevel(as_factor(asthma_lm$EducationLevel), ref="Did not Attend High School")

asthma_narm <- asthma_lm[complete.cases(asthma_lm),]
asthma_narm <- data.frame(asthma_narm)

ui <- navbarPage("U.S. Asthma Statistical Analysis for 2017",
                 
                 tabPanel("Geographic Distribution",
                          column(width = 2, 
                                 div(class='option_group',
                                     selectInput(inputId = 'stateStat', 
                                                 label = h4("Choose a State"),
                                                 choices = c("None", "All", ByState$`State Name`)),
                                     checkboxInput(inputId = "rank",
                                                   label = "Rank Percentage of Respondents with Asthma by State"),
                                     conditionalPanel(condition = "input.rank == 1",
                                                      selectInput('ascdesc', 'Order', c("Top 10", "Bottom 10"))),
                                     checkboxInput(inputId = 'stateMap', 
                                                   label = "Show Heatmap - Percentage of Respondents Currently having Asthma by State"),
                                     helpText("Let's start by looking at asthma prevalence by state using both tables and a heat map.")
                            )),
                          column(width = 4, mainPanel(
                            h4("Asthma Prevalence by State"),
                            tableOutput("ranking"),
                            tableOutput("bystate")
                            )),
                          column(width = 4, mainPanel(
                            plotOutput("map", width = 900, height = 500)
                            )
                          )
                          ), 
                 
                 tabPanel("Asthma Prevalence",
                          column(width = 2, 
                                 div(class='option_group',
                                     helpText("Research suggests that various Demographic Factors and Health Risk Factors are realted to Asthma and Asthma Prevalence in the U.S. Let's look at them one by one."),
                                     radioButtons(inputId = 'varchoice',
                                                 label = "Asthma Prevalence",
                                                 choices = c("By Demographic Factors", "By Health Risk Factors")),
                                     conditionalPanel("input.varchoice === 'By Demographic Factors'",
                                                      selectInput("Demo", "(Please select a factor)",
                                                                  c("Age", "Sex", "Race", "Income", "Education"))),
                                     conditionalPanel("input.varchoice === 'By Health Risk Factors'",
                                                      selectInput("Risk", "(Please select  factor)",
                                                                  c("Activeness", "Smoking Habit", "Depressive Disorder", "BMI Categories")))
                                 )       
                            ),
                          column(width = 4,
                            mainPanel(h4("Probability of Having Asthma given the Selected Factor"),
                              tableOutput('condprop1'),
                              tableOutput('condprop2')
                            )
                          ),
                          column(width = 6,
                                 mainPanel(h4("Probability Visualization"),
                                   plotOutput('condplot1')
                                 ))

                 ),
                 tabPanel("Asthma Risk",
                 column(width = 3,
                        div(class = 'option_group',
                            selectInput(inputId = "Age",
                                        label = "Select Your Age Group",
                                        choices = c("18 to 24", "25 to 34", "35 to 44", 
                                                    "45 to 54", "55 to 64", "65 and above"))),
                            selectInput(inputId = "Sex",
                                        label = "Select Your Gender (0 = Female, 1 = Male)",
                                        choices = c(0,1)),
                            selectInput(inputId = "Race",
                                        label = "Select Your Race",
                                        choices = c("American Indian/Alaskan Native", "Asian", "Black", "Hispanic", 
                                                    "Multiracial, non-Hispanic", "Native Hawaiian/other Pacific Islander", 
                                                     "White", "Others")),
                            selectInput(inputId = "Income",
                                        label = "Select Your Income Group",
                                        choices = c("less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999", 
                                                    "$20,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", 
                                                    "$50,000 to $74,999", "$75,000 or above")),
                            selectInput(inputId = "Education",
                                        label = "Select Your highest Education Level Achieved",
                                        choices = c("Did not Attend High School", "Graduated High School", 
                                                    "Attended College/Technical School", 
                                                    "Graduated from College/Technical School")),
                            selectInput(inputId = "Exercise",
                                        label = "You Exercised This Month (0 = No, 1 = Yes)",
                                        choices = c(0, 1)),
                            selectInput(inputId = "Depress",
                                        label = "You Have/Had Depression (0 = No, 1 = Yes)",
                                    choices = c(0, 1)),
                            selectInput(inputId = "BMI",
                                        label = "Select Your BMI Category (BMI = kg/m^2)",
                                    choices = c("Underweight (< 18.5)", 
                                                "Normal (18.5 - 24.9)", 
                                                "Overweight (25 - 29.9)", 
                                                "Obese (> 29.9)")),
                            selectInput(inputId = "Smoke",
                                    label = "You are A Current Smoker (0 = No, 1 = Yes)",
                                    choices = c(0, 1)),
                        actionButton('pred', "Show Prediction Result"),
                        checkboxInput("mod", "Show Model and Evaluation")
                        
                        ),
                 column(width = 3,
                        wellPanel(h4("Predict the Likelihood of Having Asthma"),
                                  textOutput("result")),
                        mainPanel(
                                  helpText("Let's predict the likelihood of you having asthma. Select your demographic groups and health factors then hit the 'Show Prediction Result' button to see your calculated probability. Check the 'Show Model and Evaluation' box to see the model. For more information, please check out 'Statistical Methodology' under 'For Data Scientists.' If the probability is higher than 16%, there is a 29% chance that you may have asthma. If the probability is 16% and below, there is a 90% chance that you may not have asthma."),
                                  helpText("Disclaimer: The data and algorithm concerned a sample of the American population. Prediction is likely inaccurate for users not born and raised in America.")
                                  
                          
                        )),
                 column(width = 6,
                        wellPanel(h4("Classification Algorithm and Evaluation"),
                          verbatimTextOutput("call"),
                          verbatimTextOutput("eval"),
                          verbatimTextOutput("summary")
                        ))
                 ),
                 
                 tabPanel("More Information",
                 navlistPanel("For Data Scientists",
                            tabPanel("Project Overview", br(),
                                     fluidRow(
                                       column(8, includeMarkdown("Intro.md")),
                                       column(3, includeMarkdown("Intro2.Rmd")))),
                            
                            tabPanel("The Dataset", br(),
                                     fluidRow(
                                       column(8, includeMarkdown("stat1.md")))),
                            
                            tabPanel("Statistical Methodology", br(),
                                     fluidRow(
                                       column(8, includeMarkdown("Stat.md")))),
                            
                            tabPanel("About the Author", br(),
                                     fluidRow(
                                       column(8, includeMarkdown("About.Rmd")),
                                       column(3, includeMarkdown("Profile.Rmd"))))
                 
                 )
                 )

)

server <- function(input, output) {
  #stateMapInput <- reactive({input$stateMap})
  output$map <- renderPlot({
    if(input$stateMap) {
      plot_usmap (data = ByState, values = "Asthma Percentage", labels = TRUE, line = "white") +
        scale_fill_continuous(name = "Percentage Currently Having Asthma", low = "white", high = "steel blue", label = scales::comma) +
        labs(title = "Visual Presentation of Asthma Prevalence by State", 
             subtitle = "The map did not show Puerto Rico and Guam. From the color scale, 
             West Virginia (WV) had the highest percentage of respondents reporting 
             suffering from asthma in 2017. Meanwhile, Minnesota (MN) had the lowest percentage.") +
        theme(legend.position = "bottom",
              legend.direction = 'horizontal',
              plot.title = element_text(size = rel(2), hjust = 0.5),
              plot.subtitle = element_text(size = rel(1.5), hjust = 0.5))
    }
    else{plot_usmap(labels = TRUE)}
  })

  output$ranking <- renderTable({
    if (input$rank == 1) {
    if (input$ascdesc == "Bottom 10") {
      ByState %>% arrange(`Asthma Percentage`) %>% head(10) %>%
        select(-State, -fips)
    }
    else if (input$ascdesc == "Top 10") {
      ByState %>% arrange(desc(`Asthma Percentage`)) %>% head(10) %>%
        select(-State, -fips)
    }
    }
  })
  
  output$bystate <- renderTable({
    if (input$stateStat != "None" && input$stateStat != "All") {
      ByState %>% 
        filter(StateName == input$stateStat) %>%
        select(-State, -fips)
    }
    else if (input$stateStat == "All") {
      ByState %>%
        select(-State, -fips)
    }
    
  })
  
  output$condprop1 <- renderTable({
    
    if (input$varchoice == "By Demographic Factors" && input$varchoice != "By Health Risk Factors") {
    if (input$Demo == "Age") {
      Conditional_Probability(AgeGroup) %>% filter(!is.na(AgeGroup))
      
    }
    else if (input$Demo == "Sex") {
      Conditional_Probability(Sex) %>% 
        mutate(Sex = replace(Sex, Sex == 0, "Female"),
               Sex = replace(Sex, Sex == 1, "Male")) %>%
        filter(!is.na(Sex)) 
    }
    else if (input$Demo == "Race") {
      Conditional_Probability(Race) %>% filter(!is.na(Race))
    }
    else if (input$Demo == "Income") {
      Conditional_Probability(IncomeGroup) %>% filter(!is.na(IncomeGroup))
    }
    else if (input$Demo == "Education") {
      Conditional_Probability(EducationLevel) %>% filter(!is.na(EducationLevel))
    }
    }
  })
  
  output$condprop2 <- renderTable ({
    if (input$varchoice != "By Demographic Factors" && input$varchoice == "By Health Risk Factors") {
    if (input$Risk == "Activeness") { 
      Conditional_Probability(Exercise) %>%
        mutate(Exercise = replace(Exercise, Exercise == 0, "No"),
               Exercise = replace(Exercise, Exercise == 1, "Yes")) %>%
        filter(!is.na(Exercise))
    }
    else if (input$Risk == "Depressive Disorder") {
      Conditional_Probability(DepressiveDisorder) %>%
        mutate(DepressiveDisorder = replace(DepressiveDisorder, DepressiveDisorder == 0, "No"),
               DepressiveDisorder = replace(DepressiveDisorder, DepressiveDisorder == 1, "Yes")) %>%
        filter(!is.na(DepressiveDisorder))
    }
    else if (input$Risk == "Smoking Habit") {
      Conditional_Probability(CurrentSmoker) %>%
        mutate(CurrentSmoker = replace(CurrentSmoker, CurrentSmoker == 0, "Yes"),
               CurrentSmoker = replace(CurrentSmoker, CurrentSmoker == 1, "No")) %>%
        filter(!is.na(CurrentSmoker))
    }
    else if (input$Risk == "BMI Categories") {
      Conditional_Probability(BMICategory) %>% filter(!is.na(BMICategory))
    }
    }
   
  })
  

  output$condplot1 <- renderPlot({
    if (input$varchoice == "By Demographic Factors" && input$varchoice != "By Health Risk Factors") {
      if (input$Demo == "Age") {
        curr_ast_17 %>%
          ggplot() +
          geom_bar(aes(x = AgeGroup), fill = "darkolivegreen4") +
          coord_flip() +
          labs(x=" ", subtitle = "Asthma Prevalence Decreased with Age.") +
          guides(fill = FALSE)+
          ggtitle("Asthma Prevalence By Age Groups") + theme_classic()
        
      }
      else if (input$Demo == "Sex") {
        Conditional_Probability(Sex) %>% 
          mutate(Sex = replace(Sex, Sex == 0, "Female"),
                 Sex = replace(Sex, Sex == 1, "Male")) %>%
          filter(!is.na(Sex)) %>%   
          ggplot(aes(x = reorder(Sex, -AsthmaPrevalence), y = AsthmaPrevalence, fill = Sex)) + 
          geom_bar(stat = 'identity', position = "dodge", color = "white") + 
          theme(axis.title.y = element_blank()) + 
          labs(x="Sex", y="Probability", subtitle = "Females were more likely to have Asthma than Males.") +
          scale_fill_brewer(palette = "Accent") +
          guides(fill = FALSE) +
          ggtitle("Asthma Prevalence By Sex") + theme_classic()
        
      }
      else if (input$Demo == "Race") {
        Conditional_Probability(Race) %>% filter(!is.na(Race)) %>%
          ggplot(aes(x = reorder(Race, -AsthmaPrevalence), y = AsthmaPrevalence)) +
          geom_bar(stat = 'identity', fill = 'darkseagreen4') +
          coord_flip() +
          labs(x=" ", subtitle = "Asthma is the Most Prevalent among White people, 
               and least among Asian people.") +
          guides(fill = FALSE)+
          ggtitle("Asthma Prevalence By Races") + theme_classic()
      }
      else if (input$Demo == "Income") {
        Conditional_Probability(IncomeGroup) %>% filter(!is.na(IncomeGroup)) %>%
          ggplot(aes(x = reorder(IncomeGroup, -AsthmaPrevalence), y = AsthmaPrevalence)) +
          geom_bar(stat = 'identity',fill = 'cadetblue4') +
          coord_flip() +
          labs(x=" ", subtitle = "Respondents with Higher Income were Less Likely to Have Asthma.") +
          guides(fill = FALSE)+
          ggtitle("Asthma Prevalence By Income Group") + theme_classic()
      }
      else if (input$Demo == "Education") {
        Conditional_Probability(EducationLevel) %>% filter(!is.na(EducationLevel)) %>%
          ggplot(aes(x = reorder(EducationLevel, -AsthmaPrevalence), y=AsthmaPrevalence)) +
          geom_bar(stat = 'identity', fill = 'chartreuse4') +
          coord_flip() +
          labs(x = " ", subtitle = "Higher Education Level Achieved did not 
               Lower Asthma Prevalence.") +
          guides(fill = FALSE) +
          ggtitle("Asthma Prevalence By Education Level") + theme_classic()
      }
    }
  else if (input$varchoice != "By Demographic Factors" && input$varchoice == "By Health Risk Factors") {
      if (input$Risk == "Activeness") { 
        Conditional_Probability(Exercise) %>%
          mutate(Exercise = replace(Exercise, Exercise == 0, "No"),
                 Exercise = replace(Exercise, Exercise == 1, "Yes")) %>%
          filter(!is.na(Exercise)) %>%
          ggplot(aes(x = reorder(Exercise, -AsthmaPrevalence), y = AsthmaPrevalence, fill = Exercise))+
          geom_bar(stat = 'identity', position = "dodge") + 
          theme(axis.title.y = element_blank(),
                axis.title.x = element_blank()) +
          guides (fill = F) +
          labs(x="  ", subtitle = "Asthma is more Prevalent with Inactiveness.") +
          ggtitle("Asthma by Physical Activeness") + theme_classic()
        
      }
      else if (input$Risk == "Depressive Disorder") {
        Conditional_Probability(DepressiveDisorder) %>%
          mutate(DepressiveDisorder = replace(DepressiveDisorder, DepressiveDisorder == 0, "No"),
                 DepressiveDisorder = replace(DepressiveDisorder, DepressiveDisorder == 1, "Yes")) %>%
          filter(!is.na(DepressiveDisorder)) %>%
          ggplot(aes(x = reorder(DepressiveDisorder, -AsthmaPrevalence), y = AsthmaPrevalence, fill = DepressiveDisorder)) + 
          geom_bar(stat = 'identity', position = "dodge", color = "gold") + 
          scale_fill_brewer(palette = "Spectral")  +
          theme(axis.title.y = element_blank(),
                axis.title.x = element_blank()) +
          guides (fill = F) +
          labs(x="  ", subtitle = "Asthma is more Prevalent with Depressive Disorders.") +
          ggtitle("Asthma by Depressive Disorder") + theme_classic()
      }
      else if (input$Risk == "Smoking Habit") {
        Conditional_Probability(CurrentSmoker) %>%
          mutate(CurrentSmoker = replace(CurrentSmoker, CurrentSmoker == 0, "Yes"),
                 CurrentSmoker = replace(CurrentSmoker, CurrentSmoker == 1, "No")) %>%
          filter(!is.na(CurrentSmoker)) %>%
          ggplot(aes(x = reorder(CurrentSmoker, -AsthmaPrevalence), y = AsthmaPrevalence, fill = CurrentSmoker)) + 
          geom_bar(stat = 'identity', position = "dodge", color = "gray") + 
          scale_fill_brewer(palette = "Set1") +
          theme(axis.title.y = element_blank(),
                axis.title.x = element_blank()) +
          guides (fill = F) +
          labs(x="  ", subtitle = "Asthma is more Prevalent among Smokers.") +
          ggtitle("Asthma by Smoker Status") + theme_classic()
      }
      else if (input$Risk == "BMI Categories") {
        Conditional_Probability(BMICategory) %>% 
          filter(!is.na(BMICategory)) %>%
          ggplot(aes(x = reorder(BMICategory, -AsthmaPrevalence), y=AsthmaPrevalence)) +
          geom_bar(stat = 'identity', fill = 'seagreen4') +
          coord_flip() +
          labs(x = " ", subtitle = "Asthma is most Prevalent among Obese respondents,
               and Least among those with Normal Weight.") +
          guides(fill = FALSE) +
          ggtitle("Asthma Prevalence By Weight Groups") + theme_classic()
      }
    }
    
  })
  
  set.seed(12345)
  training <- sample(1:nrow(asthma_narm), 0.6*nrow(asthma_narm))
  asthma.training <- asthma_narm[training,]
  asthma.test <- seq(1:nrow(asthma_narm))[-training]
  asthma.test.results = asthma_narm[-training,1]
  
  
  asthma.lr.ML <- glm(AsthmaStatus ~., family=binomial(link="logit"), data = asthma.training)
  
  newdata <- eventReactive(input$pred, 
                           {data.frame(AgeGroup = input$Age, 
                        Sex = as.double(input$Sex), 
                        IncomeGroup = input$Income, 
                        EducationLevel = input$Education,
                        Race = input$Race,
                        Exercise = as.double(input$Exercise),
                        DepressiveDisorder = as.double(input$Depress),
                        BMICategory = input$BMI,
                        CurrentSmoker = as.double(input$Smoke))})
  
  output$result <- renderPrint({
    if (input$pred) {
    print(paste0("According to this data and algorithm, the probability of you having asthma is ", round(predict(asthma.lr.ML, newdata(), type = "response")*100, 2), "%"))
    }
  })
  
  
  output$call <- renderPrint({
    if(input$mod ==1) {
    summary(asthma.lr.ML)$call
    }
  })
  
  output$eval <- renderPrint({
    if (input$mod == 1){
      asthma.test.probabilities <- predict(asthma.lr.ML, asthma_narm, type = "response")[asthma.test]
      
      asthma.pred.test <- ifelse(asthma.test.probabilities > 0.16, 1, 0)  
      
      conf.mat <- table("Predicted" = asthma.pred.test, "Actual" = asthma.test.results) 
      colnames(conf.mat) = c("No", "Yes")
      rownames(conf.mat) = c("No", "Yes")
      
      TruN <- conf.mat[1,1] 
      TruP <- conf.mat[2,2] 
      FalN <- conf.mat[1,2] 
      FalP <- conf.mat[2,1] 
      TotN <- conf.mat[1,1] + conf.mat[2,1] 
      TotP <- conf.mat[1,2] + conf.mat[2,2] 
      Tot <- TotN+TotP 
      
      Accuracy.Rate <- (TruN + TruP) / Tot
      
      Error.Rate <- (FalN + FalP) / Tot
      
      Sensitivity <- TruP / TotP 
      
      Specificity <- TruN / TotN 
      
      FalseP.Rate <- 1 - Specificity # False positive rate
      
      logit.rates.50 <- c(Accuracy.Rate, Error.Rate, Sensitivity, Specificity, FalseP.Rate)
      
      names(logit.rates.50) <- c("Accuracy Rate", "Error Rate", "True Positives", "True Negatives", "False Positives")
      
      options(scipen="4")
      print(logit.rates.50, digits=2)
    }
  })
  
  output$summary <- renderPrint({
    if (input$mod){
      summary(asthma.lr.ML)$coefficients[, -c(2:3)]
    }
  })
  
  
}


shinyApp(ui = ui, server = server)



