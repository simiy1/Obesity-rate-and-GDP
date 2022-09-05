library("shiny")
library('DT')


# Introduction
intro_panel <- tabPanel(
  "Introduction",
  titlePanel("Introduction"),
  img(src="photo.gif", align = "right", height = '200px', width = '250px'),
  p("Group G1: Jasper Fan-Chiang, Bonnie Du, Siming Yin"),
  br(),
  p("Nowadays, obesity is viewed by authorities as one of the most serious public health problems of the 21st century. Obesity is a disorder involving excessive body fat that increases the risk of health problems."),
  p("In general, obesity means overweight. Obesity is correlated with various diseases and conditions, for example, heart disease, diabetes, certain kinds of cancers, and even mental health issues. "),
  p("According to the data in 2015, there are 600 million adults and 100 million children were obese in 195 countries."),
  br(),
  strong("Obesity is a leading preventable cause of death worldwide"),
  br(),
  br(),
  p("GDP is used to measure the prosperity of a country during a certain period. By examining the GDP value for each country, we can determinate the development and progress of a country."),
  p("This is one of the important measurement or powerful statistical indicator for comparing the national economies development in the international market."),
  br(),
  strong("We want to discover how population obesity prevalence is associated with economic affluence, whether obesity can become a potential negative outcome with economics growth, and whether or not the development of a country will lead to the growth of overweighted people."),
  br(),
  br(),
  p("Our group find out that there might be some interesting relationships between obesity and GDP. Since GDP is related to national economies and obesity has individual, socioeconomic, and environmental causes."),
  p("We will take a closer look to the relationship between GDP and obesity in U.S. especially. Because USA is one of the most wealthy countries in the world, at the same time, it is the 12th highest obesity rate country around the world(in the list of countries by obesity rate)"),
  br(),
  p("Beside discussing with the relationship between GDP and obesity rate, we also show how obesity rate changed over years and why it would become a serious health problem to human being, in order to let people be aware with their own health problem. Through study the GDP and obesity, we will try to find the pattern or relationship between these two, in order to let people realize that obesity is one of the serious worldwide disease and pay more attention to their health."),
  br(),
  p(strong("Data Report")),
  a("https://info201b-wi21.github.io/project-simiy1/index.html"),
  br(),
  br(),
  p(strong("Reference")),
  br(),
  a("https://en.wikipedia.org/wiki/List_of_countries_by_obesity_rate"),
  br(),
  br(),
  p("Data Set 1: Obesity_countries"),
  p("We will use this dataset to discover the change of the obesity rate over the years in worldwise. Also this data is contians the information for the obesity rate for different sex. We can take a closer look to the obesity rate by the sex of different countries over years."),
  p("This dataset is obtained from WHO. Aman Arora collected from WHO Exploratory Data Analysis and Visualizations. Aman creates a clean version for the data. In the proposal, we are providing the whole data, but in order to make our project cleaner, we choose to use the clean version for presenting the obesity rate."),
  a("https://www.kaggle.com/amanarora/obesity-among-adults-by-country-19752016?select=obesity-cleaned.csv"),
  br(),
  br(),
  p("Data Set 2: Obesity_GDP_USA"),
  p("We will use this dataset to analysis the relationship between obesity and GDP in USA. Through analysising the dataset, we want to discover how population obesity prevalence is associated with economic affluence in the United States. We will show a correlation between obesity and GDP in USA. In order to let more people to pay attention to their own health"),
  p("This dataset is obtained from Kaggle. Anne Dunn collected data from DataUSA, regarding adult obesity rates, average age, average income, and poverty rates for each state, and the Federal Reserve as well as the U.S. Bureau of Economic Analysis, regarding Real GDP by state and state region."),
  a("https://www.kaggle.com/annedunn/obesity-and-gdp-rates-from-50-states-in-20142017"),
  br(),
  br(),
  p("Data Set 3: GDP-Countries"),
  p("We will use this dataset to found out the GDP growth between different countries."),
  p("This dataset is obtained from Kaggle. Nitisha collected the data file from the World Bank in order to present the an economic snapshot of a country."),
  a("https://www.kaggle.com/nitishabharathi/gdp-per-capita-all-countries"),
  br(),
  br()
)

#Question 1
relation_control <- selectInput(inputId = "relation", label = h4("Relationship between Obesity Rate and GDP"), 
                                choices = list("2017" = 2017, "2016" = 2016, 
                                               "2015" = 2015, "2014" = 2014),
                                selected = "2017")
                            


relation_display <- sidebarPanel(
  tabsetPanel(type = "tabs",
              tabPanel("Display And Interaction", plotOutput("relation_plot"), textOutput("plot_analysis")),
              tabPanel("Result Analysis", textOutput("result_analysis"))
  ), width = 15
)

relation_panel <- tabPanel(
  "Relationship",
  titlePanel("Is there any interesting relationship between GDP and obesity rate inside US?"),
  p("To see that people will get more weights or less when they become richer, there are 4 different graphs of the relationship between obesity rate and GDP for recent years."),
  p("In this section, you can view different years GDP and obesity of different regions in the U.S."),
  sidebarLayout(
    sidebarPanel(relation_control),
    mainPanel(relation_display)
  )
)


#Question 2
change_control <- sidebarPanel(
  radioButtons(inputId = "change_sex", label = h3("Choose the trend of obesity rate(Sex)"),
               choices = list("All" = "all", "Both Sexes" = "Both sexes", "Male" = "Male", "Female" = "Female"), 
               selected = "all"),
  br(),
  sliderInput(
    inputId = "year_range",           
    label = "Range of the Year:", 
    min = 1975,                  
    max = 2016,                  
    value = c(1975, 2016),
    step = 1
  ), 
  br(),
  textInput(inputId = "text", label = h3("Country"), value = "Afghanistan"),
  helpText("Note: One Country Analysis is only for 'Both sexes', 'Male', and 'Female' option. The graph will not be available for choosing option 'All'.\n
             The graph of One Country Analysis is from 1975 to 2016.")
)

change_display <- sidebarPanel(
  tabsetPanel(type = "tabs",
              tabPanel("Different Sexes and Year", plotOutput("change_plot"), textOutput("change_analysis")),
              tabPanel("One Country Analysis", DT::dataTableOutput("x1"), plotOutput("one_plot"), textOutput("one_plot_text"))
  ), width = 8
)

change_panel <- tabPanel(
  "Change",
  titlePanel("Changes in obesity rate in the whole world over time (1975 - 2016) based on sexes?"),
  p("After seeing the obesity rates in US, we want to find out how the obesity rate has changed from the past to recent years. In that case, we will able to know whether the world is getting fatter or not. Also, we also analysis the obesity rate in different sexes, such as males, females, and both. "),
  p("In this section, you can choose to view different sexs of obesity trend in the world. You can also choose one specific countries to view the change from the past to the recent years"),
  sidebarLayout(
    change_control,
    change_display
  )
)


#Question 3
change_comparison <- sidebarPanel(
  radioButtons(inputId = "change_category", label = h3("Choose different area for comparison"),
               choices = list("US" = "us", "Global" = "global", "Total" = "total"), 
               selected = "total"),
  br(),
  helpText("Note: click the buttons to see the range you want to check specifially.")
)

comparison_panel <- tabPanel(
  "Comparison",
  titlePanel("The comparison of the obesity rate in us by state and the global obesity rate from 2014 to 2016"),
  p("In this section, we plan to compare the obesity rate between the obesity rate counted by states in US and the global obesity rate by the sum of all the countries in the world. 
    Since the year that contain on both graph is only 2014-2016, we specifically analyze the data in these three years and do the comparison."),
  p("We count the mean of the US obesity rate in each states to be the US mean obesity rate, and then count the mean of the obesity rate from all over the world to be the global mean obesity rate.
    Then we graph the the plot to show the trend of the growth between US and the world as a whole."),
  br(),
  p("click the red dots on the graph to see more details"),
  sidebarLayout(
    change_comparison,
    mainPanel(
      plotOutput("comparison_plot", click = "plot_click"),
      verbatimTextOutput("click_info"),
      p(
        "From the plot, we can see that from 2014 to 2016 the total obesity rate in us is all higher than the global obesity rate and both of them get slightly increase from 2014 to 2016. 
        According to this graph, though it shows all of the data we need correctly, but since the changes of obesity rate very small, it is hard to see obvious difference in the graph. 
        It might be better to compare with the increasing rate or increase the amount to data up to 5-10 years. These changes may let the graph be clear."
      )
    )
  )
)

#Question 4

change_prediction <- sidebarPanel(
  radioButtons(inputId = "Mean_Median", label = h3("Prediction"),
               choices = list("Mean", "Median"), 
               selected = "Mean"),
  helpText("Note: click the buttons to check specifially.")
)

prediction_panel <- tabPanel(
  "Prediction",
  titlePanel("The prediction of global GDP in 2014 - 2016"),
  p("1. Two data we use are the obesity rate and GDP of us and the global obesity rate. Using the ratio we get from the us, it would be interesting to see our approximation of the global average GDP."),
  p("2. From the question 1-3 above, we already know the mean GDP and the mean obesity rate in US in 2014 -2016 and the mean global obesity rate. 
    According to all three variable, we are planning to use the mean ratio of GDP/obesity rate in US as the ratio times the global obesity rate to predict the mean global GDP.
    Then we plot these predicted GDP into the graph."),
  textOutput("prediction_sentence"),
  br(),
  strong(textOutput("prediction_summary_2014")),
  strong(textOutput("prediction_summary_2015")),
  strong(textOutput("prediction_summary_2016")),
  br(),
  p("4. From the plot and the exact result we receive from the calculation, 
    we can see that the global average GDP is gradually increasing from 2014 to 2016. 
    It would be more interesting if we can compare to the real global GDP and see the differences between them."),
  br(),
  sidebarLayout(
    change_prediction,
    mainPanel(
      plotOutput("prediction_plot")
    )
  )
)



my_ui <- tagList(
  includeCSS("style.css"),
  navbarPage(
  "GDP and Obesity Rate",
  intro_panel,
  relation_panel,
  change_panel,
  comparison_panel,
  prediction_panel
  )
)
