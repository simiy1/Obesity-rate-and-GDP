library("shiny")
library("ggplot2")
library('dplyr')
library('tidyverse')
library('RColorBrewer')
library('DT')

# read the data file
Obesity_countries <- read.csv("data/obesity-cleaned.csv")
Obesity_GDP_USA <- read.csv("data/Obesity_GDP_USA.csv")
Obesity_countries <- select(Obesity_countries, Country:Sex)
str(Obesity_countries)
Obesity_countries$Obesity.... <- sub('\\s*\\[.*$', '', Obesity_countries$Obesity....)
as.num <- function(x, na.strings = "NA") {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}
Obesity_countries$Obesity.... <- as.num(Obesity_countries$Obesity...., na.strings="No data")


my_server <- function(input, output) {
  
  output$relation_plot <- renderPlot({
    plot_data <- Obesity_GDP_USA %>%
      filter(Year == input$relation) %>%
      select(Region, Adult.Obesity.100, Real.GDP) %>%
      group_by(Region) %>%
      summarize(
        Obesity_rate_by_percent = mean(Adult.Obesity.100),
        GDP_by_10_thousand = mean(Real.GDP/10000)
      )  %>%
      pivot_longer(
        cols = c(Obesity_rate_by_percent, GDP_by_10_thousand),
        names_to = "Types"
      )
    p <- ggplot(data = plot_data, mapping = aes(x = value, y = Region, fill = Types)) +
      geom_col(position = position_dodge2(reverse = TRUE)) +
      labs(
        title = paste("Relationship between Obesity Rate and GDP in", input$relation),
        x = "Value",
        y = "Regions",
        fill = "Types"
      ) + scale_fill_brewer(palette = "Set1", labels = c("GDP (by 10000)", "Obesity Rate (by %)"))
    p
  })
  output$plot_analysis <- renderText({
    data <- Obesity_GDP_USA %>%
      filter(Year == input$relation) %>%
      select(Region, Adult.Obesity.100, Real.GDP) %>%
      group_by(Region) %>%
      summarize(
        Obesity_rate_by_percent = mean(Adult.Obesity.100),
        GDP_by_10_thousand = mean(Real.GDP/10000)
      ) %>%
      mutate(diff = abs(Obesity_rate_by_percent - GDP_by_10_thousand))
    max <- data %>%
      filter(diff == max(diff)) %>%
      pull(Region)
    min <- data %>%
      filter(diff == min(diff)) %>%
      pull(Region)
    text <- paste("In", input$relation, ", the sum of the average of the obesity rate by regions (by the percentage) is", sum(data$Obesity_rate_by_percent, na.rm = TRUE), 
                  "and the sum of the average GDP by regions is", sum(data$GDP_by_10_thousand, na.rm = TRUE),". Among all these regions,", max, 
                  "has the highest difference between GDP and Obesity rate, while", min, "has the lowest difference.", 
                  "The mean difference amongs the region is", mean(data$diff, na.rm = TRUE), ".")
    })
  
  output$space <- renderText({
    text <- paste(" ")
  })
  
  output$result_analysis <- renderText({
    text <- paste("From those plots, we can find out that there is a little connection between obesity rate and GDP. However, it is not that obvious to see, but we can find out that with the regions that have higher GDP are more intended to have the higher obesity rate. At first, we assume that obesity rate and GDP might be positive correlation, but it turns out that there isn't strong connections between two of them. Furthermore, the differences of obesity rate and GDP in each region don't change a lot to really find out the differences in each year. Thus, the reason that causes this phenomenon might because how wealthy the people are won't change too much about what they will eat. ",
                  "Also, in general, for those regions which has the relatively high GDP, they will have a relatively low obesity rate. In the U.S, the GDP is various between regions and the obesity rate is pretty avergae")
  })
  output$change_plot <- renderPlot({
    plot_data <- Obesity_countries %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])%>%
      group_by(Year, Sex) %>%
      summarize(
        Obesity = mean(Obesity....,  na.rm = T, .groups='drop')
      )
    if(input$change_sex == 'all') {
      p <- ggplot(data = plot_data, mapping = aes(x = Year, y = Obesity, color = Sex)) + 
        geom_line(size = 5)+
        labs(
          title = paste("The Changes of Obesity Rate from", input$year_range[1], "-", input$year_range[2],  "(All Sexes)"),
          x = "Year",
          y = "Obesity Rate, %",
          fill = "Sex"
        )
    } else {
      plot_data <- plot_data %>%
        filter(Sex == input$change_sex)
      p <- ggplot(data = plot_data, mapping = aes(x = Year, y = Obesity)) + 
        geom_line(size = 5, color = "#219AC6")+
        labs(
          title = paste("The Changes of Obesity Rate from", input$year_range[1], "-", input$year_range[2], "(", input$change_sex, ")", seq=""),
          x = "Year",
          y = "Obesity Rate, %"
        )
    }
      p
  })
  output$change_analysis <- renderText({
    if(input$change_sex != 'all'){
    data <- Obesity_countries %>%
      replace(is.na(.), 0) %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])%>%
      filter(Sex == input$change_sex) %>%
      group_by(Country) %>%
      summarize(
        Obesity = mean(Obesity....,  na.rm = T, .groups='drop')
      )
    text = paste("As you can see the trend of the obesity rate is increasing from", input$year_range[1], "to", input$year_range[2],
                  ". All the", input$change_sex, "popultion is s getting heavier over these years. More and more people are becoming overweight. The flow of this trend is getting worse and worse since it is a curve growth, which means the population of those overweight will become more and more in the same sort of time. ",
                  "Between these years the minimum obesity rate is", min(data$Obesity),
                  "; and the maximum obesity rate is", max(data$Obesity))
    } else {
      text = paste("As you can see the trend of the obesity rate is increasing from", input$year_range[1], "to", input$year_range[2],
            ". All the", input$change_sex, "popultion is s getting heavier over these years. More and more people are becoming overweight. The flow of this trend is getting worse and worse since it is a curve growth, which means the population of those overweight will become more and more in the same sort of time. "
      )
    }
    text
  })
  output$x1 <- DT::renderDataTable({
    if(input$change_sex != 'all'){
    filter_data <- Obesity_countries %>%
      replace(is.na(.), 0) %>%
      filter(Year >= 1975, Year <= 2016) %>%
      filter(Country == input$text)%>%
      filter(Sex == input$change_sex)%>%
      select(Year, 'Obesity Rate %' = Obesity....)
    DT::datatable(filter_data)}
  })
  output$one_plot <- renderPlot({
    if(input$change_sex != 'all'){
      filter_data <- Obesity_countries %>%
      replace(is.na(.), 0) %>%
      filter(Country == input$text) %>%
      filter(Sex == input$change_sex)%>%
      select(Year, Obesity....)
    
    s = input$x1_rows_selected
    par(mar = c(4, 4, 1, .1))
    plot(filter_data, main = paste("The graph of the tread obesity rate of", input$text, "from 1975 to 2016" ),  y = "Obesity Rate %")
    if (length(s)) points(filter_data[s, , drop = FALSE], pch = 20, cex = 2, col = "purple")
  }
  })
  output$one_plot_text <- renderText({
    if(input$change_sex != 'all'){
      filter_data <- Obesity_countries %>%
        replace(is.na(.), 0) %>%
        filter(Country == input$text) %>%
        filter(Sex == input$change_sex)%>%
        select(Year, Obesity....)
      text <- paste("This graph shows the tread obesity rate of ", input$text, " from the past(1975) to (2016). The obesity is increasing over years. The peak is ", max(filter_data$Obesity....),
                    "and the lowest point is ", min(filter_data$Obesity....), ". And the difference between the higher point and lowest point is ", max(filter_data$Obesity....)-min(filter_data$Obesity....), ".",
                    "When you click table, it will highlight the point on the graph for you!", seq = "")
  
      }else {
        text<- paste("There is no graph and table for one specific country unless you choose one of the three options for sex(Both sexes, Male, Female)")
      }} )
  
  output$comparison_plot <- renderPlot({
    obesity_rate_Total <- filter(Obesity_countries, Year <= "2016") %>%
      filter(Year >= "2014") %>%
      replace(is.na(.), 0) %>%
      filter(Sex == "Both sexes")
    
    obesity_rate_Total <- group_by(obesity_rate_Total, Year) %>%
      summarise(total_obesity_rate = mean(Obesity....))
    obesity_rate_US <- filter(Obesity_GDP_USA, Year <= "2016") %>%
      filter(Year >= "2014") %>%
      replace(is.na(.), 0)
    
    obesity_rate_US <- group_by(obesity_rate_US, Year) %>%
      summarise(us_obesity_rate = mean(Adult.Obesity.100))
    
    Total_rate <- left_join(obesity_rate_Total, obesity_rate_US)
    
    Total_rate <- Total_rate %>%
      pivot_longer(cols = c(total_obesity_rate, us_obesity_rate), names_to = "Rates")
    if(input$change_category == "total"){
      total_vs_us_plot <- ggplot(data = Total_rate) +
        geom_col(mapping = aes(x = Year, y = value, fill = Rates), position = position_dodge()) +
        scale_fill_manual(values = c("black", "yellow"), 
                          label = c(total_obesity_rate = "Global",us_obesity_rate = "US")) +
        geom_point(mapping = aes(x = Year, y = value), color = "red") +
        labs(
          title = "Total obesity rate versus us obersity rate",
          x = "year",
          y = "mean obesity rate"
        )
      return(total_vs_us_plot)
    }else if(input$change_category == "us"){
      Total_rate <- Total_rate %>% filter(Rates == "us_obesity_rate")
      
      total_vs_us_plot <- ggplot(data = Total_rate) +
        geom_col(mapping = aes(x = Year, y = value), color = "black", fill = "yellow", position = position_dodge()) +
        geom_point(mapping = aes(x = Year, y = value), color = "red")+
        labs(
          title = "US obesity rate versus us obersity rate",
          x = "year",
          y = "mean obesity rate"
        )
      return(total_vs_us_plot)
    }else{
      Total_rate <- Total_rate %>% filter(Rates == "total_obesity_rate")
      total_vs_us_plot <- ggplot(data = Total_rate) +
        geom_col(mapping = aes(x = Year, y = value), fill = "black", position = position_dodge()) +
        geom_point(mapping = aes(x = Year, y = value), color = "red") +
        labs(
          title = "Global obesity rate versus us obersity rate",
          x = "year",
          y = "mean obesity rate"
        )
      return(total_vs_us_plot)
    }
  })
  
  output$click_info <- renderPrint({
    obesity_rate_Total <- filter(Obesity_countries, Year <= "2016") %>%
      filter(Year >= "2014") %>%
      replace(is.na(.), 0) %>%
      filter(Sex == "Both sexes")
    
    obesity_rate_Total <- group_by(obesity_rate_Total, Year) %>%
      summarise(total_obesity_rate = mean(Obesity....))
    
    obesity_rate_US <- filter(Obesity_GDP_USA, Year <= "2016") %>%
      filter(Year >= "2014") %>%
      replace(is.na(.), 0)
    
    obesity_rate_US <- group_by(obesity_rate_US, Year) %>%
      summarise(us_obesity_rate = mean(Adult.Obesity.100))
    
    Total_rate <- left_join(obesity_rate_Total, obesity_rate_US)
    
    Total_rate <- Total_rate %>%
      mutate(total = total_obesity_rate, us = us_obesity_rate) %>%
      select(Year, total, us) %>%
      pivot_longer(cols = c(total, us), names_to = "Rates") 
    nearPoints(Total_rate, input$plot_click)
  })
  
  output$prediction_sentence <- renderText({
    summary <- paste("3. The prediction of GDP", input$Mean_Median, "in each year: ")
    return(summary)
  })
  
  output$prediction_summary_2014 <- renderText({
    if(input$Mean_Median == "Mean"){
      obesity_rate_in_us_2014 <- mean(filter(Obesity_GDP_USA, Year == "2014") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2014 <- mean(filter(Obesity_GDP_USA,Year == "2014") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2014 <- GDP_in_us_2014 / obesity_rate_in_us_2014
      obesity_in_countries_2014 <- mean(filter(Obesity_countries, Year == "2014") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      
      predicted_GDP_2014 <- GDP_versus_obesity_2014 * obesity_in_countries_2014
      Mean_2014 <- paste("- Predicted GDP Mean in 2014:", predicted_GDP_2014)
      return(Mean_2014)
    }else {
      obesity_rate_in_us_2014 <- median(filter(Obesity_GDP_USA, Year == "2014") %>%
                                          pull(Adult.Obesity.100))
      GDP_in_us_2014 <- median(filter(Obesity_GDP_USA,Year == "2014") %>%
                                 pull(Real.GDP))
      GDP_versus_obesity_2014 <- GDP_in_us_2014 / obesity_rate_in_us_2014
      obesity_in_countries_2014 <- median(filter(Obesity_countries, Year == "2014") %>%
                                            replace(is.na(.), 0) %>%
                                            filter(Sex == "Both sexes") %>%
                                            pull(Obesity....))
      predicted_GDP_2014 <- GDP_versus_obesity_2014 * obesity_in_countries_2014
      Median_2014 <- paste("- Predicted GDP Median in 2014:", predicted_GDP_2014)
      return(Median_2014)
    }
  })
  
  output$prediction_summary_2015 <-renderText({
    
    if(input$Mean_Median == "Mean"){
      obesity_rate_in_us_2015 <- mean(filter(Obesity_GDP_USA, Year == "2015") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2015 <- mean(filter(Obesity_GDP_USA,Year == "2015") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2015 <- GDP_in_us_2015 / obesity_rate_in_us_2015
      obesity_in_countries_2015 <- mean(filter(Obesity_countries, Year == "2015") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      predicted_GDP_2015 <- GDP_versus_obesity_2015 * obesity_in_countries_2015
      Mean_2015 <- paste("- Predicted GDP Mean in 2015:", predicted_GDP_2015)
      return(Mean_2015)
    }else{
      obesity_rate_in_us_2015 <- median(filter(Obesity_GDP_USA, Year == "2015") %>%
                                         pull(Adult.Obesity.100))
      GDP_in_us_2015 <- median(filter(Obesity_GDP_USA,Year == "2015") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2015 <- GDP_in_us_2015 / obesity_rate_in_us_2015
      obesity_in_countries_2015 <- median(filter(Obesity_countries, Year == "2015") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      predicted_GDP_2015 <- GDP_versus_obesity_2015 * obesity_in_countries_2015
      Median_2015 <- paste("- Predicted GDP Median in 2015:", predicted_GDP_2015)
      return(Median_2015)
    }
  })
  
  output$prediction_summary_2016 <- renderText({
    if(input$Mean_Median == "Mean"){
      obesity_rate_in_us_2016 <- mean(filter(Obesity_GDP_USA, Year == "2016") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2016 <- mean(filter(Obesity_GDP_USA,Year == "2016") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2016 <- GDP_in_us_2016 / obesity_rate_in_us_2016
      obesity_in_countries_2016 <- mean(filter(Obesity_countries, Year == "2016") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      predicted_GDP_2016 <- GDP_versus_obesity_2016 * obesity_in_countries_2016
      Mean_2016 <- paste("- Predicted GDP Mean in 2016:", predicted_GDP_2016)
      return(Mean_2016)
    }else {
      obesity_rate_in_us_2016 <- median(filter(Obesity_GDP_USA, Year == "2016") %>%
                                          pull(Adult.Obesity.100))
      GDP_in_us_2016 <- median(filter(Obesity_GDP_USA,Year == "2016") %>%
                                 pull(Real.GDP))
      GDP_versus_obesity_2016 <- GDP_in_us_2016 / obesity_rate_in_us_2016
      obesity_in_countries_2016 <- median(filter(Obesity_countries, Year == "2016") %>%
                                            replace(is.na(.), 0) %>%
                                            filter(Sex == "Both sexes") %>%
                                            pull(Obesity....))
      predicted_GDP_2016 <- GDP_versus_obesity_2016 * obesity_in_countries_2016
      Median_2016 <- paste("- Predicted GDP Median in 2016:", predicted_GDP_2016)
      return(Median_2016)
    }
  })
  
  output$prediction_plot <- renderPlot({
    
    if(input$Mean_Median == "Mean"){
      obesity_rate_in_us_2014 <- mean(filter(Obesity_GDP_USA, Year == "2014") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2014 <- mean(filter(Obesity_GDP_USA,Year == "2014") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2014 <- GDP_in_us_2014 / obesity_rate_in_us_2014
      obesity_in_countries_2014 <- mean(filter(Obesity_countries, Year == "2014") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      
      predicted_GDP_2014 <- GDP_versus_obesity_2014 * obesity_in_countries_2014
      
      obesity_rate_in_us_2015 <- mean(filter(Obesity_GDP_USA, Year == "2015") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2015 <- mean(filter(Obesity_GDP_USA,Year == "2015") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2015 <- GDP_in_us_2015 / obesity_rate_in_us_2015
      obesity_in_countries_2015 <- mean(filter(Obesity_countries, Year == "2015") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      
      predicted_GDP_2015 <- GDP_versus_obesity_2015 * obesity_in_countries_2015
      
      obesity_rate_in_us_2016 <- mean(filter(Obesity_GDP_USA, Year == "2016") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2016 <- mean(filter(Obesity_GDP_USA,Year == "2016") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2016 <- GDP_in_us_2016 / obesity_rate_in_us_2016
      obesity_in_countries_2016 <- mean(filter(Obesity_countries, Year == "2016") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      
      predicted_GDP_2016 <- GDP_versus_obesity_2016 * obesity_in_countries_2016
      
      predict_GDP_df <- data_frame(Year = c("2014", "2015", "2016"),
                                   predicted_GDP = c(predicted_GDP_2014, predicted_GDP_2015, predicted_GDP_2016)) 
      
      predict_GDP_plot_mean <- ggplot(data = predict_GDP_df) +
        geom_point(mapping = aes(x = Year, y = predicted_GDP, color = Year), size = 10) + 
        labs(
          title = "Predicted average GDP Over Time",
          x = "Year",
          y = "predicted GDP"
        )
      return(predict_GDP_plot_mean)
    }else {
      obesity_rate_in_us_2014 <- median(filter(Obesity_GDP_USA, Year == "2014") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2014 <- median(filter(Obesity_GDP_USA,Year == "2014") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2014 <- GDP_in_us_2014 / obesity_rate_in_us_2014
      obesity_in_countries_2014 <- median(filter(Obesity_countries, Year == "2014") %>%
                                         replace(is.na(.), 0) %>%
                                         filter(Sex == "Both sexes") %>%
                                         pull(Obesity....))
      
      predicted_GDP_2014 <- GDP_versus_obesity_2014 * obesity_in_countries_2014
      
      obesity_rate_in_us_2015 <- median(filter(Obesity_GDP_USA, Year == "2015") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2015 <- median(filter(Obesity_GDP_USA,Year == "2015") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2015 <- GDP_in_us_2015 / obesity_rate_in_us_2015
      obesity_in_countries_2015 <- median(filter(Obesity_countries, Year == "2015") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      
      predicted_GDP_2015 <- GDP_versus_obesity_2015 * obesity_in_countries_2015
      
      obesity_rate_in_us_2016 <- median(filter(Obesity_GDP_USA, Year == "2016") %>%
                                        pull(Adult.Obesity.100))
      GDP_in_us_2016 <- median(filter(Obesity_GDP_USA,Year == "2016") %>%
                               pull(Real.GDP))
      GDP_versus_obesity_2016 <- GDP_in_us_2016 / obesity_rate_in_us_2016
      obesity_in_countries_2016 <- median(filter(Obesity_countries, Year == "2016") %>%
                                          replace(is.na(.), 0) %>%
                                          filter(Sex == "Both sexes") %>%
                                          pull(Obesity....))
      
      predicted_GDP_2016 <- GDP_versus_obesity_2016 * obesity_in_countries_2016
      
      predict_GDP_df <- data_frame(Year = c("2014", "2015", "2016"),
                                   predicted_GDP = c(predicted_GDP_2014, predicted_GDP_2015, predicted_GDP_2016)) 
      
      predict_GDP_plot_median <- ggplot(data = predict_GDP_df) +
        geom_point(mapping = aes(x = Year, y = predicted_GDP, color = Year), size = 10) + 
        labs(
          title = "Predicted average GDP Over Time",
          x = "Year",
          y = "predicted GDP"
        )
      return(predict_GDP_plot_median)
    }
    })
}
