library(shiny)
library(tidyverse)
library(stringr)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(ggplot2)
library(ggpubr)
library(ggrepel)



school_name=function(school_url){
  gsub(pattern='-',replacement=' ',
       strsplit(school_url, "[/]")[[1]][6])%>%
    str_to_title()
}


df=read_csv('final_table.csv')%>%as_tibble()%>%
  mutate(school=unname(sapply(school_link,FUN=school_name)))
df=df[,c(27,4:26,1:3)]

player=read_csv('player_data.csv')%>%as_tibble()%>%
  mutate(school=unname(sapply(school,FUN=school_name)))%>%
  select(-1)

player_features=c('MP','FG%','FGA','2P%','3P%','3PA',
                  'FTA','FT%',colnames(player)[19:25])


shinyApp(
  ui = fluidPage(
    includeCSS(path = "style.css"),
    
    tags$div(
      tags$div(
        class = "shinytitle", 
        tags$h2(strong("2021-2022 NCAA Basketball Visualizer"))
      ),
      fluidPage(
        theme = shinythemes::shinytheme("flatly"),
        sidebarLayout(
          sidebarPanel(
            h3("Select Conferences"),
            selectInput('conference',label='',
                        c(unique(df$conferences))%>%sort()),
            actionButton('sum1','Get Summary',class='btn-danger'),
            tags$div(class = "hr",hr()),
            h3('Compare Two Schools'),
            selectInput(inputId="school1",
                        label="Select School 1:",
                        choices=df$school),
            selectInput(inputId="school2",
                        label="Select School 2:",
                        choices=df$school),
            actionButton('compare','Compare Two Schools',class='btn-danger'),
            actionButton('sum2','Get Summary',class='btn-danger'),
            tags$div(class = "hr",hr()),
            h3('Team Stat Leaders'),
            selectInput(inputId='player_school',label='Select a School',
                        choices=df$school),
            actionButton('apply',"Get Stat Leaders",class='btn-danger'),
            actionButton('sum3','Get Summary',class='btn-danger')
          ),
          mainPanel(
            tags$head(
              tags$style("body {background-color: MintCream; }")
            ),
            tabsetPanel(
              tabPanel("Table", dataTableOutput("table")),
              tabPanel("Summary",uiOutput("summary"))
            )
            
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    output$table=renderDataTable({
      df%>%filter(conferences==input$conference)%>%
        select(1:24)
    })
    
    compdflong<-eventReactive(input$compare,{
      olddatawide<-df[df$school==input$school1|df$school==input$school2,]
      return(gather(olddatawide,stat,value,G:PTS))
    })
    
    observeEvent(input$compare,{
      compModal<-function(missing=FALSE){
        modalDialog(
          tags$div(
            class = "modaltitle", 
            renderUI(tags$div(img(src=df[df$school==input$school1,"image_link"]),
                              img(src='https://png.pngtree.com/png-vector/20220520/ourmid/pngtree-black-and-white-vs-symbol-on-white-background-png-image_4630177.png',width=100,height=100),
                              img(src=df[df$school==input$school2,"image_link"])))
          ),
          tags$div(class = "hr",hr()),
          br(),
          renderPlot(ggplot(compdflong()[compdflong()$stat %in% c("FG%","2P%","3P%","FT%"),],aes(x=stat,y=value,fill=school))+
                       geom_bar(stat="identity",position=position_dodge())+theme_bw()+
                       geom_text(aes(label=value),vjust=-0.2,position=position_dodge(0.9))+
                       labs(title="Team Shooting Efficiency",y="Shooting Percentage (Decimal)",x="")+
                       scale_y_continuous(limits=c(0,1),n.breaks=9)+
                       theme(axis.title.y=element_text(margin=margin(r=10)))),
          tags$div(class = "hr",hr()),
          br(),
          renderPlot(ggplot(compdflong()[compdflong()$stat %in% c("ORB","AST","TOV","PTS"),],aes(x=stat,y=value,fill=school))+
                       geom_bar(stat="identity",position=position_dodge())+theme_bw()+
                       geom_text(aes(label=value),vjust=-0.2,position=position_dodge(0.9))+
                       labs(title="Team Offensive Statistics",y="Count",x="")+
                       scale_y_continuous(limits=c(0,90),n.breaks=8)+
                       theme(axis.title.y=element_text(margin=margin(r=10)))),
          tags$div(class = "hr",hr()),
          br(),
          renderPlot(ggplot(compdflong()[compdflong()$stat %in% c("DRB","STL","BLK","PF"),],aes(x=stat,y=value,fill=school))+
                       geom_bar(stat="identity",position=position_dodge())+theme_bw()+
                       geom_text(aes(label=value),vjust=-0.2,position=position_dodge(0.9))+
                       labs(title="Team Defensive Statistics",y="Count",x="")+
                       scale_y_continuous(limits=c(0,40),n.breaks=8)+
                       theme(axis.title.y=element_text(margin=margin(r=10))))
        )
      }
      showModal(compModal())
    })
    
    observeEvent(input$apply,{
      
      compModal<-function(missing=FALSE){
        modalDialog(
          tags$div(
            class = "modaltitle", 
            renderUI(tags$div(img(src=df[df$school==input$player_school,"image_link"])))
          ),
          tags$div(class = "hr",hr()),
          br(),
          uiOutput('ui_element')
        )
      }
      showModal(compModal())
    })
    
    observe({
      school_options = df[df$conferences==input$conference,]$school
      updateSelectInput(
        session = session,
        inputId = "school1",
        choices = school_options,
        selected = head(school_options,1))
      updateSelectInput(
        session = session,
        inputId = "school2",
        choices = school_options,
        selected = head(school_options,1))
      
      updateSelectInput(
        session = session,
        inputId = "player_school",
        choices = school_options,
        selected = head(school_options,1))
      
    })
    
    
    output$ui_element=renderUI({purrr::map(##
      seq_len(length(player_features)),
      function(i){
        renderText({
          in_school_player=player%>%filter(school==input$player_school)
          names=in_school_player$Player
          values=in_school_player[[player_features[i]]]
          values[is.na(values)] = mean(values,na.rm=TRUE)
          name=names[which.max(values)]
          paste(player_features[i],':',max(values),',',name)
        })}  
    )})
    
    sumdat1<-eventReactive(input$sum1,{
      dat1<-df[df$conferences==input$conference,]
      return(dat1)
    })
    
    observeEvent(input$sum1,{
      output$summary = renderUI({
        fluidPage(
          renderPlot(ggplot(sumdat1(),aes(FG,FT))+geom_point(color = "#CD3333",size = 3)+theme_bw()+
                       geom_text_repel(aes( label = school),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Team point efficiency",y="Field goals attempt",x="Field goals made")),
          renderPlot(ggplot(sumdat1(),aes(`2P`,`3P`))+geom_point(color = "#CD3333",size = 3)+theme_bw()+
                       geom_text_repel(aes(label = school),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Team point structure",y="Three-point field goals made",x="Two-point field goals made")),
          renderPlot(ggplot(sumdat1(),aes(ORB,DRB))+geom_point(color = "#CD3333",size = 3)+theme_bw()+
                       geom_text_repel(aes(label = school),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Team rebound structure",y="Defensive rebounds",x="Offensive rebounds")),
          renderPlot(ggplot(sumdat1(),aes(STL,BLK))+geom_point(color = "#CD3333",size = 3)+theme_bw()+
                       geom_text_repel(aes(label = school),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Team defense structure",y="Blocks",x="Steals")),
          withProgress(message = 'Making plot',value = 0,{
              n = 10
              for(i in 1:n){
                incProgress(1/n)
                Sys.sleep(1)
              }
          })
        )
      }
      )
    }
    )
    pl = na.omit(player)
    sumdat2<-eventReactive(input$sum2,{
      dat2<-pl[pl$school==input$school1|pl$school==input$school2,]
      return(dat2)
    })
    
    observeEvent(input$sum2,{
      output$summary = renderUI({
        fluidPage(
          renderPlot(ggplot(sumdat2(),aes(FG,FT))+geom_point(aes(color = as.factor(sumdat2()$school)),size = 3)+theme_bw()+
                       geom_text_repel(aes( label = Player),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Personal point efficiency",y="Field goals attempt",
                            x="Field goals made",color='school')),
          renderPlot(ggplot(sumdat2(),aes(`2P`,`3P`))+geom_point(aes(color = as.factor(sumdat2()$school)),size = 3)+theme_bw()+
                       geom_text_repel(aes(label = Player),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Personal point structure",y="Three-point field goals made",
                            x="Two-point field goals made",color='school')),
          renderPlot(ggplot(sumdat2(),aes(ORB,DRB))+geom_point(aes(color = as.factor(sumdat2()$school)),size = 3)+theme_bw()+
                       geom_text_repel(aes(label = Player),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Personal rebound structure",y="Defensive rebounds",
                            x="Offensive rebounds",color='school')),
          renderPlot(ggplot(sumdat2(),aes(STL,BLK))+geom_point(aes(color = as.factor(sumdat2()$school)),size = 3)+theme_bw()+
                       geom_text_repel(aes(label = Player),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Personal defense structure",y="Blocks",
                            x="Steals",color='school')),
          withProgress(message = 'Making plot',value = 0,{
            n = 10
            for(i in 1:n){
              incProgress(1/n)
              Sys.sleep(1)
            }
          })
        )
      }
      )
    }
    )
    
    sumdat3<-eventReactive(input$sum3,{
      dat3<-pl[pl$school==input$player_school,]
      return(dat3)
    })
    
    observeEvent(input$sum3,{
      output$summary = renderUI({
        fluidPage(
          renderPlot(ggplot(sumdat3(),aes(FG,FT))+geom_point(color = "#CD3333",size = 3)+theme_bw()+
                       geom_text_repel(aes( label = Player),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Personal point efficiency",y="Field goals attempt",x="Field goals made")),
          renderPlot(ggplot(sumdat3(),aes(`2P`,`3P`))+geom_point(color = "#CD3333",size = 3)+theme_bw()+
                       geom_text_repel(aes(label = Player),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Personal point structure",y="Three-point field goals made",x="Two-point field goals made")),
          renderPlot(ggplot(sumdat3(),aes(ORB,DRB))+geom_point(color = "#CD3333",size = 3)+theme_bw()+
                       geom_text_repel(aes(label = Player),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Personal rebound structure",y="Defensive rebounds",x="Offensive rebounds")),
          renderPlot(ggplot(sumdat3(),aes(STL,BLK))+geom_point(color = "#CD3333",size = 3)+theme_bw()+
                       geom_text_repel(aes(label = Player),size = 3)+
                       geom_smooth(method = 'lm',se = FALSE,formula = y ~ x)+
                       labs(title="Personal defense structure",y="Blocks",x="Steals")),
          withProgress(message = 'Making plot',value = 0,{
            n = 10
            for(i in 1:n){
              incProgress(1/n)
              Sys.sleep(1)
            }
          })
        )
      }
      )
    }
    )
  }
)
##### test code ####
