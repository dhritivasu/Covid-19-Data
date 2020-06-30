library(shiny)
library(plotly)
library(DT)
library(plyr)
library(shinythemes)
library(RColorBrewer)

covid19 <- read.csv("https://storage.googleapis.com/kagglesdsdata/datasets%2F494766%2F1289348%2Fcountry_wise_latest.csv?GoogleAccessId=gcp-kaggle-com@kaggle-161607.iam.gserviceaccount.com&Expires=1593750962&Signature=Cb%2BvFpR%2BdVmXry2Qj5JaIrf0pi9U99uQjn3Dh3GNaFNsQe%2FRQ6vy9ruiLCemoVMY%2FLI%2BLfZrSUEmcDScPYb3WqPj44ybawgDuq9g4vYZknERHoeDvZ3SEcsIjyoGkAsbea6PW3mPqwO%2Fl0MwlCHA1QvPmhBGk0MowwYfVhdOCiwDJITCokboVXrMYkrnHkrnIpysRDFauRthYac5Z9ZKHpWrTedQnr%2F4%2F3r6WGF%2FGfyPoJjG4qX%2BlvRggBlPC9AbXCJfOUV2X39IB3nMPsBSK1rKxk7RKg0ACPHX%2BXzuhocAJsEbfcgM6NUKSbvDSRUJEfmie5CG2RyGt8QzFvARbg%3D%3D", sep=",", header=TRUE)
daywise <- read.csv("https://storage.googleapis.com/kagglesdsdata/datasets%2F494766%2F1289348%2Fday_wise.csv?GoogleAccessId=gcp-kaggle-com@kaggle-161607.iam.gserviceaccount.com&Expires=1593750985&Signature=mu4RfJ%2BUjwb%2Fkd363CFVQ0pfd4Xlrsj2OZtuNrPGRaTy7OjBu7oC1iXRmI9LQO5ULVdKwNkL15VG2HIfCcw6sbghPbd8Mu3nXtn0InjqZn4q4HMWzvGh%2F1Ykmc2xiJuJFr9b2yu8%2BTlRdZxjWOvc7t2BcrDNVcPOw3A7lvRNHR59b%2Fn1lPfiDKW3C3b6g241TFFn10GellOAMzkKNbS4zCxqN%2FEElKyroWK3KFmqzL8xKeB3HH0vQNJTBP92qkjyXdPy0zxuDz18QtQ8XG0DCUlyNi2ugveActKQ0aRqj2EGzSYG05vsD%2FG7S%2FCYgHdCXe3hwdzLaF9dXA%2BCse4%2BDg%3D%3D", sep=",", header=TRUE)

colors = c('#264653', '#2a9d8f','#74ac84','#e9c46a','#f4a261', '#e76f51')

plotUI <- function(id, label = "plot ui") {
  n <- NS(id)
  return(radioButtons(n("chosenFactor"), label = NULL, 
                            choices = list("% of Deaths in Each WHO Region" = 1, "Confirmed Cases Rate" = 2, "Number of Confirmed vs Number of Deaths" = 3, "Covid-19 Confirmed vs Deaths" = 4, "% of Active Cases in Each WHO Region" = 5), selected = 1))
}
Graphs <- function(input, output, session){
  reactive(input$chosenFactor, {
    fig1 <- plot_ly(covid19, labels = ~WHO.Region, values = ~Deaths, type = 'pie')
    fig1 <- layout(fig1, title = "Percentage of Deaths in Each WHO Region", colorway = c('#264653', '#2a9d8f','#74ac84','#e9c46a','#f4a261', '#e76f51'))
    fig2 <- plot_ly(daywise, x = ~daywise$Date, y = ~daywise$Confirmed, type = 'scatter', mode = 'lines', line = list(color = '#74ac84'))
    fig2 <- layout(fig2, title = 'Confirmed Cases', yaxis = list(title = 'Confirmed'), xaxis = list(title = 'Date'))
    fig3 <- plot_ly(covid19,x = ~WHO.Region, y = ~Deaths, type = 'bar', name = 'Deaths')
    fig3 <- fig3 %>% add_trace(y = ~Confirmed, name = 'Confirmed')
    fig3 <- fig3 %>% layout(yaxis = list(title = 'Count'), xaxis = list(title = 'WHO Region'), barmode = 'group', title = "Number of Confirmed vs Number of Deaths", colorway = c('#2a9d8f', '#e76f51'))
    fig4 <- plot_ly(data = iris, x = ~covid19$Confirmed, y = ~covid19$Deaths, color = ~covid19$WHO.Region, colors = colors)
    fig4 <- layout(fig4, title = "Covid-19 Confirmed vs Deaths", yaxis = list(title = 'Deaths'), xaxis = list(title = 'Confirmed'))
    fig5 <- plot_ly(covid19, labels = ~WHO.Region, values = ~Active, type = 'pie')
    fig5 <- fig5 %>% layout(title = 'Percentage of Active Cases in Each WHO Region', colorway = c('#264653', '#2a9d8f','#74ac84','#e9c46a','#f4a261', '#e76f51'))
    fig <- plotly_empty()
    if(length(input$chosenFactor) == 1) {
      if(input$chosenFactor == 1) {
        fig <- fig1
      }
      else if(input$chosenFactor == 2) {
        fig <- fig2
      }
      else if(input$chosenFactor == 3) {
        fig <- fig3
      }
      else if(input$chosenFactor == 4) {
        fig <- fig4
      }
      else if(input$chosenFactor == 5) {
        fig <- fig5
      }
      else if(input$chosenFactor == 6) {
        fig <- fig6
      }
    }
  })
  return(fig)
}
shinyUI <- fluidPage(theme = shinytheme("flatly"),
  headerPanel("Covid-19 Data"),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a Graph to Display:"),
      plotUI("chosenFactor"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs", br(), plotlyOutput("graph"))
      ),
      br(),
      tabsetPanel(
        tabPanel("Covid-19 Deaths 5 Number Summary", br(), dataTableOutput("summaryDEATHS"))
      ),
      br(),
      tabsetPanel(
        tabPanel("Covid-19 Recovered 5 Number Summary", br(), dataTableOutput("summaryRECOVERED"))
      )
    )
  )
)

shinyServer <- function(input, output){
  output$graph <- renderPlotly({
    callModule(Graphs, "chosenFactor")
  })
  output$summaryDEATHS <- renderDataTable({
    americas <- c()
    europe <- c()
    easternmediterranean <- c()
    southeastasia <- c()
    africa <- c()
    westernpacific <- c()
    inds <- c(1:nrow(covid19))
    a <- 1
    b <- 1
    c <- 1
    d <- 1
    e <- 1
    f <- 1
    for(i in inds) {
      if (covid19[i, "WHO.Region"] == "Americas") {
        americas[a] <- covid19[i, "Deaths"]
        a <- a+1
        print(covid19[i, "Deaths"])
      }
      else if (covid19[i, "WHO.Region"] == "Europe") {
        europe[b] <- covid19[i, "Deaths"]
        b <- b+1
        print(covid19[i, "Deaths"])
      }
      else if (covid19[i, "WHO.Region"] == "Eastern Mediterranean") {
        easternmediterranean[c] <- covid19[i, "Deaths"]
        c <- c+1
        print(covid19[i, "Deaths"])
      }
      else if (covid19[i, "WHO.Region"] == "South-East Asia") {
        southeastasia[d] <- covid19[i, "Deaths"]
        d <- d+1
        print(covid19[i, "Deaths"])
      }
      else if (covid19[i, "WHO.Region"] == "Africa") {
        africa[e] <- covid19[i, "Deaths"]
        e <- e+1
        print(covid19[i, "Deaths"])
      }
      else if (covid19[i, "WHO.Region"] == "Western Pacific") {
        westernpacific[f] <- covid19[i, "Deaths"]
        f <- f+1
        print(covid19[i, "Deaths"])
      }
    }
    stats <- matrix(nrow = 7, ncol = 6)
    colnames(stats) <- c("Americas","Europe","Eastern Mediterranean", "South-East Asia", "Africa", "Western Pacific")
    rownames(stats) <- c("mean","standard deviation", "min", "Q1","median", "Q3", "max")
    stats["mean", "Americas"] <- round(mean(americas), 2)
    stats["standard deviation", "Americas"] <- round(sd(americas),2)
    stats[ "min", "Americas"] <- min(americas)
    stats["Q1","Americas"] <- quantile(americas, prob=0.25)
    stats["median","Americas"] <- median(americas)
    stats["Q3","Americas"] <- quantile(americas, prob=0.75)
    stats["max","Americas"] <- max(americas)
    stats["mean", "Europe"] <- round(mean(europe), 2)
    stats["standard deviation", "Europe"] <- round(sd(europe),2)
    stats[ "min", "Europe"] <- min(europe)
    stats["Q1","Europe"] <- quantile(europe, prob=0.25)
    stats["median","Europe"] <- median(europe)
    stats["Q3","Europe"] <- quantile(europe, prob=0.75)
    stats["max","Europe"] <- max(europe)
    stats["mean", "Eastern Mediterranean"] <- round(mean(easternmediterranean), 2)
    stats["standard deviation", "Eastern Mediterranean"] <- round(sd(easternmediterranean),2)
    stats[ "min", "Eastern Mediterranean"] <- min(easternmediterranean)
    stats["Q1","Eastern Mediterranean"] <- quantile(easternmediterranean, prob=0.25)
    stats["median","Eastern Mediterranean"] <- median(easternmediterranean)
    stats["Q3","Eastern Mediterranean"] <- quantile(easternmediterranean, prob=0.75)
    stats["max","Eastern Mediterranean"] <- max(easternmediterranean)
    stats["mean", "South-East Asia"] <- round(mean(southeastasia), 2)
    stats["standard deviation", "South-East Asia"] <- round(sd(southeastasia),2)
    stats[ "min", "South-East Asia"] <- min(southeastasia)
    stats["Q1","South-East Asia"] <- quantile(southeastasia, prob=0.25)
    stats["median","South-East Asia"] <- median(southeastasia)
    stats["Q3","South-East Asia"] <- quantile(southeastasia, prob=0.75)
    stats["max","South-East Asia"] <- max(southeastasia)
    stats["mean", "Africa"] <- round(mean(africa), 2)
    stats["standard deviation", "Africa"] <- round(sd(africa),2)
    stats[ "min", "Africa"] <- min(africa)
    stats["Q1","Africa"] <- quantile(africa, prob=0.25)
    stats["median","Africa"] <- median(africa)
    stats["Q3","Africa"] <- quantile(africa, prob=0.75)
    stats["max","Africa"] <- max(africa)
    stats["mean", "Western Pacific"] <- round(mean(westernpacific), 2)
    stats["standard deviation", "Western Pacific"] <- round(sd(westernpacific),2)
    stats[ "min", "Western Pacific"] <- min(westernpacific)
    stats["Q1","Western Pacific"] <- quantile(westernpacific, prob=0.25)
    stats["median","Western Pacific"] <- median(westernpacific)
    stats["Q3","Western Pacific"] <- quantile(westernpacific, prob=0.75)
    stats["max","Western Pacific"] <- max(westernpacific)
    datatable( stats, rownames = TRUE, options = list(dom = 't') )
  })
  output$summaryRECOVERED <- renderDataTable({
    americas <- c()
    europe <- c()
    easternmediterranean <- c()
    southeastasia <- c()
    africa <- c()
    westernpacific <- c()
    inds <- c(1:nrow(covid19))
    aa <- 1
    bb <- 1
    cc <- 1
    dd <- 1
    ee <- 1
    ff <- 1
    for(i in inds) {
      if (covid19[i, "WHO.Region"] == "Americas") {
        americas[aa] <- covid19[i, "Recovered"]
        aa <- aa+1
        print(covid19[i, "Recovered"])
      }
      else if (covid19[i, "WHO.Region"] == "Europe") {
        europe[bb] <- covid19[i, "Recovered"]
        bb <- bb+1
        print(covid19[i, "Recovered"])
      }
      else if (covid19[i, "WHO.Region"] == "Eastern Mediterranean") {
        easternmediterranean[cc] <- covid19[i, "Recovered"]
        cc <- cc+1
        print(covid19[i, "Recovered"])
      }
      else if (covid19[i, "WHO.Region"] == "South-East Asia") {
        southeastasia[dd] <- covid19[i, "Recovered"]
        dd <- dd+1
        print(covid19[i, "Recovered"])
      }
      else if (covid19[i, "WHO.Region"] == "Africa") {
        africa[ee] <- covid19[i, "Recovered"]
        ee <- ee+1
        print(covid19[i, "Recovered"])
      }
      else if (covid19[i, "WHO.Region"] == "Western Pacific") {
        westernpacific[ff] <- covid19[i, "Recovered"]
        ff <- ff+1
        print(covid19[i, "Recovered"])
      }
    }
    stats <- matrix(nrow = 7, ncol = 6)
    colnames(stats) <- c("Americas","Europe","Eastern Mediterranean", "South-East Asia", "Africa", "Western Pacific")
    rownames(stats) <- c("mean","standard deviation", "min", "Q1","median", "Q3", "max")
    stats["mean", "Americas"] <- round(mean(americas), 2)
    stats["standard deviation", "Americas"] <- round(sd(americas),2)
    stats[ "min", "Americas"] <- min(americas)
    stats["Q1","Americas"] <- quantile(americas, prob=0.25)
    stats["median","Americas"] <- median(americas)
    stats["Q3","Americas"] <- quantile(americas, prob=0.75)
    stats["max","Americas"] <- max(americas)
    stats["mean", "Europe"] <- round(mean(europe), 2)
    stats["standard deviation", "Europe"] <- round(sd(europe),2)
    stats[ "min", "Europe"] <- min(europe)
    stats["Q1","Europe"] <- quantile(europe, prob=0.25)
    stats["median","Europe"] <- median(europe)
    stats["Q3","Europe"] <- quantile(europe, prob=0.75)
    stats["max","Europe"] <- max(europe)
    stats["mean", "Eastern Mediterranean"] <- round(mean(easternmediterranean), 2)
    stats["standard deviation", "Eastern Mediterranean"] <- round(sd(easternmediterranean),2)
    stats[ "min", "Eastern Mediterranean"] <- min(easternmediterranean)
    stats["Q1","Eastern Mediterranean"] <- quantile(easternmediterranean, prob=0.25)
    stats["median","Eastern Mediterranean"] <- median(easternmediterranean)
    stats["Q3","Eastern Mediterranean"] <- quantile(easternmediterranean, prob=0.75)
    stats["max","Eastern Mediterranean"] <- max(easternmediterranean)
    stats["mean", "South-East Asia"] <- round(mean(southeastasia), 2)
    stats["standard deviation", "South-East Asia"] <- round(sd(southeastasia),2)
    stats[ "min", "South-East Asia"] <- min(southeastasia)
    stats["Q1","South-East Asia"] <- quantile(southeastasia, prob=0.25)
    stats["median","South-East Asia"] <- median(southeastasia)
    stats["Q3","South-East Asia"] <- quantile(southeastasia, prob=0.75)
    stats["max","South-East Asia"] <- max(southeastasia)
    stats["mean", "Africa"] <- round(mean(africa), 2)
    stats["standard deviation", "Africa"] <- round(sd(africa),2)
    stats[ "min", "Africa"] <- min(africa)
    stats["Q1","Africa"] <- quantile(africa, prob=0.25)
    stats["median","Africa"] <- median(africa)
    stats["Q3","Africa"] <- quantile(africa, prob=0.75)
    stats["max","Africa"] <- max(africa)
    stats["mean", "Western Pacific"] <- round(mean(westernpacific), 2)
    stats["standard deviation", "Western Pacific"] <- round(sd(westernpacific),2)
    stats[ "min", "Western Pacific"] <- min(westernpacific)
    stats["Q1","Western Pacific"] <- quantile(westernpacific, prob=0.25)
    stats["median","Western Pacific"] <- median(westernpacific)
    stats["Q3","Western Pacific"] <- quantile(westernpacific, prob=0.75)
    stats["max","Western Pacific"] <- max(westernpacific)
    datatable( stats, rownames = TRUE, options = list(dom = 't') )
  })
}

shinyApp(shinyUI, shinyServer)