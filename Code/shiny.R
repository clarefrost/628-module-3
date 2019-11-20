library(shiny)
require(shinythemes)
library(leaflet)
library(ggplot2)

data = read.csv("data_sug.csv", header = TRUE)
df=data[,c("name","business_id","city","latitude","longitude","stars_business","sug1","sug2","sug3","sug4_byattribute")]
df=unique(df)

ui<-fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Yelp suggestion for Restaurant"),
  sidebarLayout(position="left",
                sidebarPanel(
                  helpText("Information of a traditional American Restaurant"),
                  textInput("business_id", 
                               label = h3("Please input business_id"), 
                               value = "mKT30qU_MlLj4Ps82JAZ8w"),
                  submitButton("Submit")
                ),
                
                mainPanel(tabsetPanel(
                  tabPanel("Review Star for the Restaurant", h3(textOutput("name")), plotOutput("disPlot")),
                  tabPanel("Recommendation for Restaurant", textOutput("text1"), textOutput("text2"),textOutput("text3"),textOutput("text4"),textOutput("text5"),textOutput("text6")),
                  tabPanel("Restaurant Map", h2(textOutput("map_info")),leafletOutput("map")),
                  tabPanel("business_id for the restaurant name you input",DT::dataTableOutput("table"))
                ),
                h6("if you have any question, feel free to contact me: chao.chang@wisc.edu")
                )
  )
  

)

server<-function(input, output) {
  output$name <- renderText({
    if (dim(df[df["business_id"]==input$business_id,])[1]==0){
      "Your input is wrong"
    }
    else{
    paste("The Restaurant name is :",df[df["business_id"]==input$business_id,"name"] )}
  })
  
  output$disPlot<-renderPlot({
    if (dim(df[df["business_id"]==input$business_id,])[1]==0){
    }
    else{
    p <- ggplot(data = data[data["business_id"]==input$business_id,],aes(x = factor(stars_review)),xlab="review star")
    p <- p  + ggtitle("Review Star Distribution") + geom_histogram(position = 'identity',stat="count",aes(fill = factor(stars_review)))
    print(p)}
    })  
  
  output$text1 <- renderText({
    if(is.null(df[df["business_id"]==input$business_id,'sug1'])|dim(df[df["business_id"]==input$business_id,])[1]==0){
      paste(" ")
    }else{
    paste("Suggestion:",df[df["business_id"]==input$business_id,'sug1'])}
  })
  output$text2 <- renderText({
    if(is.null(df[df["business_id"]==input$business_id,'sug2'])|dim(df[df["business_id"]==input$business_id,])[1]==0){
    }else{
    paste("Suggestion:",df[df["business_id"]==input$business_id,'sug2'])}
  })
  output$text3 <- renderText({
    if(is.null(df[df["business_id"]==input$business_id,'sug3'])|dim(df[df["business_id"]==input$business_id,])[1]==0){
    }else{
    paste("Suggestion:",df[df["business_id"]==input$business_id,'sug3'])}
  })
  output$text4 <- renderText({
    if(is.null(df[df["business_id"]==input$business_id,'sug4_byattribute'])|dim(df[df["business_id"]==input$business_id,])[1]==0){
    }else{
      paste("Suggestions by attributes: ",df[df["business_id"]==input$business_id,'sug4_byattribute'])}
  })
  output$text5 <- renderText({
    if(is.null(df[df["business_id"]==input$business_id,'sug5_byattribute'])|dim(df[df["business_id"]==input$business_id,])[1]==0){
    }else{
      paste("Suggestions by attributes: ",df[df["business_id"]==input$business_id,'sug5_byattribute'])}
  })
  output$text6 <- renderText({
    if(is.null(df[df["business_id"]==input$business_id,'sug6_byattribute'])|dim(df[df["business_id"]==input$business_id,])[1]==0){
    }else{
      paste("Suggestions by attributes: ",df[df["business_id"]==input$business_id,'sug6_byattribute'])}
  })
  output$map_info=renderText({
    if (dim(df[df["business_id"]==input$business_id,])[1]==0){
      "Your input is wrong"
    }
    else{
      paste("The map's center is: ",df[df["business_id"]==input$business_id,"name"])
    }
  })
  
  output$map = renderLeaflet({
    if (dim(df[df["business_id"]==input$business_id,])[1]==0){
    }
    else{
    df_loc_id=df[df["business_id"]==input$business_id,]
    center_lng=df_loc_id$longitude
    center_lat=df_loc_id$latitude
    star=df[df["business_id"]!=input$business_id,]$stars_business
    name=df[df["business_id"]!=input$business_id,]$name
    content <- paste(sep = "<br/>",
                     name,
                     paste("Average Star:", as.character(star)))
    star1=df_loc_id$stars_business
    name1=df_loc_id$name
    content1 <- paste(sep = "<br/>",
                     name1,
                     paste("Average Star:", as.character(star1)))
    leafIcons <- icons(
      iconUrl = ("http://leafletjs.com/examples/custom-icons/leaf-green.png"),
      iconWidth = 38, iconHeight = 95,
      iconAnchorX = 22, iconAnchorY = 94,
      shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
    leaflet(df) %>% setView(lng = center_lng , lat = center_lat, zoom = 14) %>% addTiles() %>% addMarkers(lng=df[df["business_id"]!=input$business_id,]$longitude, lat=df[df["business_id"]!=input$business_id,]$latitude,clusterOptions=markerClusterOptions(), popup=content) %>% addMarkers(lng=df_loc_id$longitude, lat=df_loc_id$latitude, icon=leafIcons, popup=content1)}
    
  })
  output$table <- DT::renderDataTable(DT::datatable({
    df[,c("name","business_id","city","latitude","longitude","stars_business")]
  }, rownames = T))
  
  
}


shinyApp(ui=ui,server=server)