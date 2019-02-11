# Shiny app for Attitudes, Identities, and Individual Differences data exploration
# author: ian hussey (ian.hussey@ugent.be)
# license: GPLv2+

# dependencies ----

library(tidyverse)
library(shiny)
library(patchwork)


# options ----

#options(shiny.deprecation.messages = FALSE)


# get data ----

load("./shiny_app_data_1.RData")
load("./shiny_app_data_2.RData")


# UI ----

ui <- 
  
  navbarPage("Attitudes, Identities, and Individual Differences dataset explorer",
             
             # tab 1 - overview ----
             tabPanel("Overview",  
                      
                      # Main panel for displaying outputs
                      mainPanel(
                        
                        # pupose
                        h3("Overview of the Attitudes, Identities, and Individual Differences study"),
                        
                        # text 
                        p("The 'Attitudes, Identities, and Individual Differences' study was a massive, online, planned missing-data study run between 2005 and 2007 on the", a(href = "https://implicit.harvard.edu/", "Project Implicit"), "website. It's goal was to provide a very large dataset that would allow researchers to examine attitudes as a general construct using both implicit and explicit measures."),
                        p("The full dataset includes >440,000 experimental sessions and spans 95 different attitude domains (category pairs such as Christian/Muslim, Democrat/Republican, and Progress/Tradition). Each session completed both implicit and explicit measures of attitudes towards one randomly-assigned domain. This involved one of two types of IAT (evaluative or identity) and a subset of 96 different self-report attiude scales (e.g., gut vs. actual feelings towards the categories, stability of these feelings, cultural pressures within these domains), followed by one of 20 different well-known individual differences measures (e.g, Big-5 personality, Need for Cognition, and self-esteem). Because of its very large sample size, the dataset has high potential for reuse by social scientists to generate and test hypotheses and models. This app uses a 15% stratified subset of the full dataset."),
                        p("There is currently an ", a(href = "https://docs.google.com/document/d/1zKqFrMzGsQga7XgBOwWGlmLI4aTfGo4T-xe-Hw-jP5A", "open call"), " for Registered Reports to several participating journals based on this dataset. Please contact Charlie Ebersole (cebersole@virginia.edu) to obtain the exploratory dataset. A manuscript describing the curated dataset is in preparation, and a ", a(href = "https://psyarxiv.com/7rbfp/", "preprint"), " is already available which examines the structural validity of the individual differences measures included. The full dataset, including raw and processed data, codebooks, and code, will be made available on the", a(href = "https://osf.io/pcjwf/", "Open Science Framework"), " in the future after the close of call for papers."),
                        
                        # purpose
                        h3("Purpose"),
                        
                        # text 
                        p("This Shiny app provides a way to discover what variables, domains, and sample sizes are available in the full Attitudes, Identities, and Individual Differences dataset in order to encourage its reuse."),
                        
                        # usage
                        h3("Usage"),
                        
                        # text 
                        p("The 'Attitudes between domains' tab allows comparions between domains along different implicit and explicit attitude features. The 'Bivariate relations' tab displays the available sample size for different bivariate comparisons, and has the option to plot bivariate linear regressions. Please note that there are a greater number variables are available in the dataset than in this app. Code for this shiny app ", a(href = "https://github.com/ianhussey/AIID-shiny-app", "here."))
                        
                      )
             ),
             
             # tab 2 - attitudes between domains ----
             tabPanel("Attitudes between domains",
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar panel for inputs
                        sidebarPanel(
                          
                          # input: IV
                          selectInput("metric", "Attitude variable",
                                      c("Evaluation IAT D score"  = "Evaluation IAT",
                                        "Identity IAT D score" = "Identity IAT",
                                        "Preference between the categories" = "prefer",
                                        "Gut feelings [difference]" = "gut_diff",
                                        "Gut feelings [mean]" = "gut_mean",
                                        "Actual feelings [difference]" = "actual_diff",
                                        "Actual feelings [mean]" = "actual_mean",
                                        "Evaluation [difference]" = "evaluation_diff",
                                        "Evaluation [mean]" = "evaluation_mean",
                                        "Positive feelings only [difference]" = "positive_only_diff",
                                        "Positive feelings only [mean]" = "positive_only_mean",
                                        "Negative feelings only [difference]" = "negative_only_diff",
                                        "Negative feelings only [mean]" = "negative_only_mean",
                                        
                                        "Certainty about feelings [difference]" = "certainty_diff",
                                        "Certainty about feelings [mean]" = "certainty_mean",
                                        "Importance of feelings [difference]" = "importance_diff",
                                        "Importance of feelings [mean]" = "importance_mean",
                                        "Implied oppositionality between the categories [difference]" = "polarity_diff",
                                        "Implied oppositionality between the categories [mean]" = "polarity_mean",
                                        "Stability of feelings [difference]" = "stability_diff",
                                        "Stability of feelings [mean]" = "stability_mean",
                                        "Frequency of thinking about feelings [difference]" = "think_diff",
                                        "Frequency of thinking about feelings [mean]" = "think_mean",
                                        
                                        "Categories are part of self-concept [difference]" = "identity_diff",
                                        "Categories are part of self-concept [mean]" = "identity_mean",
                                        "Positive evaluations important to self-concept [difference]" = "self_concept_diff",
                                        "Positive evaluations important to self-concept [mean]" = "self_concept_mean",
                                        "Positive evalutions consistent with personal values [difference]" = "personal_values_diff",
                                        "Positive evalutions consistent with personal values [mean]" = "personal_values_mean",
                                        
                                        "I think other people prefer" = "others_prefer",
                                        "I express evaluations based of others' reactions [difference]" = "others_pressure_diff",
                                        "I express evaluations based of others' reactions [mean]" = "others_pressure_mean",
                                        "Society at large evaluates [difference]" = "cultural_attitudes_diff",
                                        "Society at large evaluates [mean]" = "cultural_attitudes_mean",
                                        "Cultural pressure and motivation to evaluate [difference]" = "cultural_pressure_diff",
                                        "Cultural pressure and motivation to evaluate [mean]" = "cultural_pressure_mean"
                                      )),

                          # input: stat
                          selectInput("statistic", "Summary statistic",
                                      c("Mean" = "M",
                                        "Standard error of the mean" = "SE",
                                        "Standard deviation" = "SD",
                                        "Z score" = "Z",
                                        "n"))
                          
                        ),
                        
                        # Main panel for displaying outputs
                        mainPanel(
                          
                          # Output: Formatted text for caption
                          p(textOutput("statistic")),
                          
                          # Output: N as formatted text 
                          p(textOutput("metric")),
                          
                          # Output: Plot of the requested variable against mpg
                          plotOutput("plot_2")
                          
                        )
                      )
             ),
             
             # tab 3 - bivariate relations ----
             tabPanel("Bivariate relations",
                      
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar panel for inputs
                        sidebarPanel(
                          
                          # input: domain
                          selectInput("specific_domain", "Attitude domain",
                                      c("African Americans - European Americans",
                                        "Artists - Musicians",
                                        "Asians - Whites",
                                        "Astrology - Science",
                                        "Atheism - Religion",
                                        "Athletic People - Intelligent People",
                                        "Avoiding - Approaching",
                                        "Bill Clinton - Hillary Clinton",
                                        "Briefs - Boxers",
                                        "Burger King - McDonald's",
                                        "Canadian - American",
                                        "Capital Punishment - Imprisonment",
                                        "Career - Family",
                                        "Chaos - Order",
                                        "Coffee - Tea",
                                        "Cold - Hot",
                                        "Conservatives - Liberals",
                                        "Corporations - Nonprofits",
                                        "David Letterman - Jay Leno",
                                        "Denzel Washington - Tom Cruise",
                                        "Determinism - Free will",
                                        "Difficult - Simple",
                                        "Dogs - Cats",
                                        "Dramas - Comedies",
                                        "Drinking - Abstaining",
                                        "Effort - Talent",
                                        "Evolution - Creationism",
                                        "Fat People - Thin People",
                                        "Foreign Places - American Places",
                                        "Friends - Family",
                                        "Gay People - Straight People",
                                        "George Bush - John Kerry",
                                        "Gun Control - Gun Rights",
                                        "Helpers - Leaders",
                                        "Hiphop - Classical",
                                        "Innocence - Wisdom",
                                        "Japan - United States",
                                        "Jazz - Teen Pop",
                                        "Jews - Christians",
                                        "Jocks - Nerds",
                                        "Kobe - Shaq",
                                        "Lawyers - Politicians",
                                        "Lord of the Rings - Harry Potter",
                                        "Manufactured - Natural",
                                        "Meat - Vegetables",
                                        "Meg Ryan - Julia Roberts",
                                        "Microsoft - Apple",
                                        "Money - Love",
                                        "Mother Teresa - Princess Diana",
                                        "Mountains - Ocean",
                                        "Muslims - Jews",
                                        "National Defense - Education",
                                        "New York - California",
                                        "Night - Morning",
                                        "Numbers - Letters",
                                        "Old People - Young People",
                                        "Organized Labor - Management",
                                        "Pants - Skirts",
                                        "Past - Future",
                                        "Pepsi - Coke",
                                        "Poor People - Rich People",
                                        "Private - Public",
                                        "Prolife - Prochoice",
                                        "Protein - Carbohydrates",
                                        "Protestants - Catholics",
                                        "Punishment - Forgiveness",
                                        "Realism - Idealism",
                                        "Reason - Emotions",
                                        "Rebellious - Conforming",
                                        "Receiving - Giving",
                                        "Redsox - Yankees",
                                        "Relaxing - Exercising",
                                        "Republicans - Democrats",
                                        "Rich People - Beautiful People",
                                        "Security - Freedom",
                                        "Single - Married",
                                        "Skeptical - Trusting",
                                        "Solitude - Companionship",
                                        "Southerners - Northerners",
                                        "Speed - Accuracy",
                                        "Stable - Flexible",
                                        "State - Church",
                                        "Strong - Sensitive",
                                        "Tall People - Short People",
                                        "Tax Reductions - Social Programs",
                                        "Team - Individual",
                                        "Technology - Nature",
                                        "Television - Books",
                                        "Tradition - Progress",
                                        "Traditional Values - Feminism",
                                        "Urban - Rural",
                                        "West Coast - East Coast",
                                        "Winter - Summer",
                                        "Wrinkles - Plastic Surgery",
                                        "50 Cent - Britney Spears",
                                        "All")),
                          
                          # input: iat type
                          selectInput("iat_type", "IAT type",
                                      c("Implicit evaluation" = "Evaluation",
                                        "Implicit identity" = "Identity",
                                        "Both" = "Both")),
                          
                          # input: iat exclusions
                          selectInput("iat_exclusion", "IAT performance exclusions",
                                      c("Default", 
                                        "Stricter",
                                        "None")),
                          
                          # input: IV
                          selectInput("IV", "Variable 1",
                                      c("IAT D score",
                                        "Self-reported preferences",
                                        "Age",
                                        "Date and time",
                                        "Implicit/explicit task order",
                                        "Age",
                                        "Sex",
                                        "Education",
                                        "English fluency",
                                        "Ethnicity",
                                        "Income",
                                        "Political identity",
                                        "Religiosity",
                                        "IAT block order",
                                        "IAT mean reaction time",
                                        "IAT median reaction time",
                                        "IAT standard deviation of reaction times",
                                        "IAT percent correct",
                                        "IAT G score",
                                        "IAT A score",
                                        "Gut feelings between categories",
                                        "Actual feelings between categories",
                                        "Evaluations between categories",
                                        "Positive evalutions only between categories",
                                        "Negative evaltions only between categories",
                                        "Importance of feelings towards categories between categories",
                                        "Difference in how often you think about the categories",
                                        "Certainty of feelings between categories",
                                        "Stability of feelings between categories",
                                        "Relevance to identity between categories",
                                        "Subjective oppositionality between categories",
                                        "Other people prefer",
                                        "Cultural attitudes between categories",
                                        "Cultural pressure between categories",
                                        "Relevance to self-concept between categories",
                                        "Relevance to personal values between categories",
                                        "Pressure from other people between categories",
                                        "Big 5 - Extroversion",                                               
                                        "Big 5 - Conscientiousness",                                          
                                        "Big 5 - Neuroticism",                                                
                                        "Big 5 - Agreeableness",                                              
                                        "Big 5 - Openness",                                                  
                                        "Balanced Inventory of Desirable Responding - Impression Management", 
                                        "Balanced Inventory of Desirable Responding - Self Deception",        
                                        "Belief in a Just World",                                             
                                        "Bayesian Racism",                                                   
                                        "Humanitarianism-Egalitarianism",                                     
                                        "Intuitions about Controllability and Awareness of Thoughts - Others",
                                        "Intuitions about Controllability and Awareness of Thoughts - Self",  
                                        "Need for Cognition",     
                                        "Need for Cognitive Closure - Predictability",                        
                                        "Need for Cognitive Closure - Decisiveness",                          
                                        "Need for Cognitive Closure - Closed-mindedness",                     
                                        "Need for Cognitive Closure - Order",                                 
                                        "Need for Cognitive Closure - Ambiguity", 
                                        "Protestant Ethic",                                                   
                                        "Personal Need for Structure",                                        
                                        "Rosenberg Self-Esteem",                                              
                                        "Ring-Wing Authoritarianism",                                         
                                        "Social Dominance Orientation",                                       
                                        "Self-Monitoring",                                                    
                                        "Spheres of Control - Interpersonal Control",                       
                                        "Spheres of Control - Personal Efficiacy")),
                          
                          # input: DV
                          selectInput("DV", "Variable 2",
                                      c("Self-reported preferences",
                                        "IAT D score",
                                        "Age",
                                        "Date and time",
                                        "Implicit/explicit task order",
                                        "Age",
                                        "Sex",
                                        "Education",
                                        "English fluency",
                                        "Ethnicity",
                                        "Income",
                                        "Political identity",
                                        "Religiosity",
                                        "IAT block order",
                                        "IAT mean reaction time",
                                        "IAT median reaction time",
                                        "IAT standard deviation of reaction times",
                                        "IAT percent correct",
                                        "IAT G score",
                                        "IAT A score",
                                        "Gut feelings between categories",
                                        "Actual feelings between categories",
                                        "Evaluations between categories",
                                        "Positive evalutions only between categories",
                                        "Negative evaltions only between categories",
                                        "Importance of feelings towards categories between categories",
                                        "Difference in how often you think about the categories",
                                        "Certainty of feelings between categories",
                                        "Stability of feelings between categories",
                                        "Relevance to identity between categories",
                                        "Subjective oppositionality between categories",
                                        "Other people prefer",
                                        "Cultural attitudes between categories",
                                        "Cultural pressure between categories",
                                        "Relevance to self-concept between categories",
                                        "Relevance to personal values between categories",
                                        "Pressure from other people between categories",
                                        "Big 5 - Extroversion",                                               
                                        "Big 5 - Conscientiousness",                                          
                                        "Big 5 - Neuroticism",                                                
                                        "Big 5 - Agreeableness",                                              
                                        "Big 5 - Openness",                                                  
                                        "Balanced Inventory of Desirable Responding - Impression Management", 
                                        "Balanced Inventory of Desirable Responding - Self Deception",        
                                        "Belief in a Just World",                                             
                                        "Bayesian Racism",                                                   
                                        "Humanitarianism-Egalitarianism",                                     
                                        "Intuitions about Controllability and Awareness of Thoughts - Others",
                                        "Intuitions about Controllability and Awareness of Thoughts - Self",  
                                        "Need for Cognition",     
                                        "Need for Cognitive Closure - Predictability",                        
                                        "Need for Cognitive Closure - Decisiveness",                          
                                        "Need for Cognitive Closure - Closed-mindedness",                     
                                        "Need for Cognitive Closure - Order",                                 
                                        "Need for Cognitive Closure - Ambiguity", 
                                        "Protestant Ethic",                                                   
                                        "Personal Need for Structure",                                        
                                        "Rosenberg Self-Esteem",                                              
                                        "Ring-Wing Authoritarianism",                                         
                                        "Social Dominance Orientation",                                       
                                        "Self-Monitoring",                                                    
                                        "Spheres of Control - Interpersonal Control",                       
                                        "Spheres of Control - Personal Efficiacy")),
                          
                          # input: plots
                          selectInput("to_plot", "Plots",
                                      c("Distribution plots + scatter plot with linear regression line" = "dist_plus_scatter",
                                        "Distribution plots only" = "dist_only"))
                          
                        ),
                        
                        # Main panel for displaying outputs
                        mainPanel(
                          
                          # Output: Formatted text for caption
                          h3(textOutput("caption")),
                          
                          # Output: N as formatted text 
                          p(textOutput("available_N")),
                          
                          # Output: Plot of the requested variable against mpg
                          plotOutput("plot")
                          
                          )
                      )
             )
  )



# Server ----

server <- function(input, output) {
  
  # tab 2 - attitudes between domains ----
  
  # plot
  output$plot_2 <- reactivePlot(function(){
    
    data_2 <- shiny_app_data_2 %>%
      filter(metric == input$metric &
               statistic == input$statistic) %>%
      mutate(domain = forcats::fct_reorder(as.factor(domain), score))
    
    p <- ggplot(data_2, aes(x = score, y = domain)) +
      geom_point(alpha = 0.8) +
      theme_minimal() +
      #theme(panel.background = element_rect(fill = NA, color = "black")) +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 13))
    
    p
    
  }, height = 750)
  
  
  # tab 3 - bivariate relations ----
  
  # formula text
  formula_text <- reactive({
    paste(input$DV, " ~ ", input$IV)
  })
  
  # return formula text for printing as a caption
  output$caption <- renderText({
    formula_text()
  })
  
  
  # available N
  available_N_text <- reactive({
    
    data <- shiny_app_data_1 %>%
      mutate(DV = .[[input$DV]],
             IV = .[[input$IV]])
    
    if (input$specific_domain != "All") {
      data <- data %>%
        filter(as.character(domain) == input$specific_domain)
    }
    
    if (input$iat_type != "Both") {
      data <- data %>%
        filter(as.character(iat_type) == input$iat_type)
    }
    
    if (input$iat_exclusion == "Default") {
      data <- data %>%
        filter(as.character(exclude_iat) == FALSE)
    }
    
    if (input$iat_exclusion == "Stricter") {
      data <- data %>%
        filter(as.character(exclude_iat_stricter) == FALSE)
    }
    
    N <- data %>%
      select(IV, DV) %>%
      na.omit() %>%
      count() %>%
      pull(n)
    
    paste("N with pairwise data =", N)
  })
  
  # return N text for printing
  output$available_N <- renderText({
    available_N_text()
  })
  
  
  # plot
  output$plot <- reactivePlot(function(){
    
    data <- shiny_app_data_1 %>%
      mutate(DV = .[[input$DV]],
             IV = .[[input$IV]])
    
    if (input$specific_domain != "All") {
      data <- data %>%
        filter(as.character(domain) == input$specific_domain)
    }
    
    if (input$iat_type != "Both") {
      data <- data %>%
        filter(as.character(iat_type) == input$iat_type)
    }
    
    if (input$iat_exclusion == "Default") {
      data <- data %>%
        filter(as.character(exclude_iat) == FALSE)
    }
    
    if (input$iat_exclusion == "Stricter") {
      data <- data %>%
        filter(as.character(exclude_iat_stricter) == FALSE)
    }
    
    p_dv <- ggplot(data, aes(DV)) + 
      geom_density(alpha = 0.5, fill = "#489893", adjust = 1.5) +
      xlab(input$DV) + 
      theme_minimal()
    
    p_iv <- ggplot(data, aes(IV)) + 
      geom_density(alpha = 0.5, fill = "#489893", adjust = 1.5) +
      xlab(input$IV) + 
      theme_minimal()

    p_scatter <- ggplot(data, aes(IV, DV)) + 
      geom_jitter(alpha = 0.25) +
      geom_smooth(method = "lm", color = "#489893") + 
      xlab(input$IV) +
      ylab(input$DV) + 
      theme_minimal()

    if (input$to_plot == "dist_plus_scatter") {
      ( ( p_dv / p_iv ) | p_scatter ) + plot_layout(ncol = 2)
    } else {
      ( p_dv / p_iv ) + plot_layout(ncol = 1)
    }
    
  })
  
}


# build app ----

shinyApp(ui, server)


