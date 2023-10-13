
library(shiny)

library(readr)
ANAT201 <- read_csv("ANAT201.csv",  col_types = "cccccccccc")
BIO113 <- read_csv("BIO113.csv",  col_types = "cccccccccc")
CHEM101 <- read_csv("CHEM101.csv",  col_types = "cccccccccccc")
BIO111 <- read_csv("BIO111.csv",  col_types = "cccccccccc")
BIO112 <- read_csv("BIO112.csv",  col_types = "cccccccccc")
CHEM111 <- read_csv("CHEM111.csv",  col_types = "cccccccccc")

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

ANAT201<-na.omit(ANAT201)
BIO113<-na.omit(BIO113)
CHEM101<-na.omit(CHEM101)

ANAT201lec<-ANAT201%>%
    select(-c(LabDays, LabTime))%>%
    mutate(Days = strsplit(LectureDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LectureTime, pattern="^.*?(?=-)"),
           End= str_extract(LectureTime, pattern = "(?<=-).*"))%>%
    select(-c(LectureDays, LectureTime))

ANAT201lab<-ANAT201%>%
    select(-c(LectureDays, LectureTime))%>%
    mutate(Days = strsplit(LabDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LabTime, pattern="^.*?(?=-)"),
           End= str_extract(LabTime, pattern = "(?<=-).*"))%>%
    select(-c(LabDays, LabTime))

ANAT201<-rbind(ANAT201lab, ANAT201lec)%>%
    mutate(Start = format(parse_date_time(Start, '%I:%M %p'), "%H:%M"), 
           End = format(parse_date_time(End, '%I:%M %p'), "%H:%M"))

BIO113lec<-BIO113%>%
    select(-c(LabDays, LabTime))%>%
    mutate(Days = strsplit(LectureDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LectureTime, pattern="^.*?(?=-)"),
           End= str_extract(LectureTime, pattern = "(?<=-).*"))%>%
    select(-c(LectureDays, LectureTime))

BIO113lab<-BIO113%>%
    select(-c(LectureDays, LectureTime))%>%
    mutate(Days = strsplit(LabDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LabTime, pattern="^.*?(?=-)"),
           End= str_extract(LabTime, pattern = "(?<=-).*"))%>%
    select(-c(LabDays, LabTime))

BIO113<-rbind(BIO113lab, BIO113lec)%>%
    mutate(Start = format(parse_date_time(Start, '%I:%M %p'), "%H:%M"), 
           End = format(parse_date_time(End, '%I:%M %p'), "%H:%M"))

CHEM101lec<-CHEM101%>%
    select(-c(LabDay1, LabTime1, LabDay2, LabTime2))%>%
    mutate(Days = strsplit(LectureDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LectureTime, pattern="^.*?(?=-)"),
           End= str_extract(LectureTime, pattern = "(?<=-).*"))%>%
    select(-c(LectureDays, LectureTime))

CHEM101lab1<-CHEM101%>%
    select(-c(LectureDays, LectureTime, LabDay2, LabTime2))%>%
    mutate(Days = strsplit(LabDay1, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LabTime1, pattern="^.*?(?=-)"),
           End= str_extract(LabTime1, pattern = "(?<=-).*"))%>%
    select(-c(LabDay1, LabTime1))

CHEM101lab2<-CHEM101%>%
    select(-c(LectureDays, LectureTime, LabDay1, LabTime1))%>%
    mutate(Days = strsplit(LabDay2, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LabTime2, pattern="^.*?(?=-)"),
           End= str_extract(LabTime2, pattern = "(?<=-).*"))%>%
    select(-c(LabDay2, LabTime2))

CHEM101lab<-rbind(CHEM101lab1, CHEM101lab2)

CHEM101<-rbind(CHEM101lab, CHEM101lec)%>%
    mutate(Start = format(parse_date_time(Start, '%I:%M %p'), "%H:%M"), 
           End = format(parse_date_time(End, '%I:%M %p'), "%H:%M"))

CHEM111lec<-CHEM111%>%
    select(-c(LabDays, LabTime))%>%
    mutate(Days = strsplit(LectureDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LectureTime, pattern="^.*?(?=-)"),
           End= str_extract(LectureTime, pattern = "(?<=-).*"))%>%
    select(-c(LectureDays, LectureTime))

CHEM111lab<-CHEM111%>%
    select(-c(LectureDays, LectureTime))%>%
    mutate(Days = strsplit(LabDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LabTime, pattern="^.*?(?=-)"),
           End= str_extract(LabTime, pattern = "(?<=-).*"))%>%
    select(-c(LabDays, LabTime))

CHEM111<-rbind(CHEM111lab, CHEM111lec)%>%
    mutate(Start = format(parse_date_time(Start, '%I:%M %p'), "%H:%M"), 
           End = format(parse_date_time(End, '%I:%M %p'), "%H:%M"))




BIO111lec<-BIO111%>%
    select(-c(LabDays, LabTime))%>%
    mutate(Days = strsplit(LectureDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LectureTime, pattern="^.*?(?=-)"),
           End= str_extract(LectureTime, pattern = "(?<=-).*"))%>%
    select(-c(LectureDays, LectureTime))

BIO111lab<-BIO111%>%
    select(-c(LectureDays, LectureTime))%>%
    mutate(Days = strsplit(LabDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LabTime, pattern="^.*?(?=-)"),
           End= str_extract(LabTime, pattern = "(?<=-).*"))%>%
    select(-c(LabDays, LabTime))

BIO111<-rbind(BIO111lab, BIO111lec)%>%
    mutate(Start = format(parse_date_time(Start, '%I:%M %p'), "%H:%M"), 
           End = format(parse_date_time(End, '%I:%M %p'), "%H:%M"))



BIO112lec<-BIO112%>%
    select(-c(LabDays, LabTime))%>%
    mutate(Days = strsplit(LectureDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LectureTime, pattern="^.*?(?=-)"),
           End= str_extract(LectureTime, pattern = "(?<=-).*"))%>%
    select(-c(LectureDays, LectureTime))

BIO112lab<-BIO112%>%
    select(-c(LectureDays, LectureTime))%>%
    mutate(Days = strsplit(LabDays, split = ""))%>%
    unnest(Days)%>%
    mutate(Start=str_extract(LabTime, pattern="^.*?(?=-)"),
           End= str_extract(LabTime, pattern = "(?<=-).*"))%>%
    select(-c(LabDays, LabTime))

BIO112<-rbind(BIO112lab, BIO112lec)%>%
    mutate(Start = format(parse_date_time(Start, '%I:%M %p'), "%H:%M"), 
           End = format(parse_date_time(End, '%I:%M %p'), "%H:%M"))

BIO<-rbind(BIO111, BIO112)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    titlePanel("Lecture-Lab combination picker"),
    tabsetPanel(
        tabPanel("Nursing course picker",
            {sidebarLayout(
                sidebarPanel(
                    textInput("AnatCRN",
                              "", 
                              placeholder =  "ANAT 201 CRN"),
                    
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                    tableOutput("C101table"),
                    tableOutput("B113Table")
                )
            )}),
        tabPanel("Biology Course picker", 
                 { sidebarLayout(
                     sidebarPanel(
                         textInput("ChemCRN",
                                   "", 
                                   placeholder =  "CHEM 111 CRN"),
                         
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         tableOutput("B111Table"),
                         tableOutput("B112Table")
                     )
                 )})
    )
    
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    anat<-reactive(input$AnatCRN)
    chem<-reactive(input$ChemCRN)
    
    output$C101table <- renderTable(
        {ANAT201%>%
                filter(CRN==anat())%>%
                left_join(CHEM101, by= "Days")%>%
                mutate(Conflict= ifelse(!( (Start.y<=Start.x) & (Start.x<=End.y) ) & (!( (Start.y<=End.x) & (End.x<=End.y) )) & (!( (Start.x<=Start.y) & (Start.y<=End.x) )) & (!( (Start.x<=End.y) & (End.y<=End.x) ))
                                        , 0, 1) )%>%
                select(Subject = Subj.y, Course = Crse.y, Section=Sec.y, Conflict)%>%
                group_by(Subject, Course, Section)%>%
                summarise(test = mean(Conflict))%>%
                filter(test==0)%>%
                arrange(Course, Section)%>%
                select(-test)}
    )
    output$B113Table <- renderTable(
        {ANAT201%>%
                filter(CRN==anat())%>%
                left_join(BIO113, by= "Days")%>%
                mutate(Conflict= ifelse(!( (Start.y<=Start.x) & (Start.x<=End.y) ) & (!( (Start.y<=End.x) & (End.x<=End.y) )) & (!( (Start.x<=Start.y) & (Start.y<=End.x) )) & (!( (Start.x<=End.y) & (End.y<=End.x) ))
                                        , 0, 1) )%>%
                select(Subject = Subj.y, Course = Crse.y, Section=Sec.y, Conflict)%>%
                group_by(Subject, Course, Section)%>%
                summarise(test = mean(Conflict))%>%
                filter(test==0)%>%
                arrange(Course, Section)%>%
                select(-test)}
    )
    output$B111Table<-renderTable(
        {CHEM111%>%
                filter(CRN==chem())%>%
                left_join(BIO111, by = "Days")%>%
                mutate(Conflict= ifelse(!( (Start.y<=Start.x) & (Start.x<=End.y) ) & (!( (Start.y<=End.x) & (End.x<=End.y) )) & (!( (Start.x<=Start.y) & (Start.y<=End.x) )) & (!( (Start.x<=End.y) & (End.y<=End.x) ))
                                        , 0, 1) )%>%
                select(Subject = Subj.y, Course = Crse.y, Section=Sec.y, Conflict)%>%
                group_by(Subject, Course, Section)%>%
                summarise(test = mean(Conflict))%>%
                filter(test==0)%>%
                arrange(Course, Section)%>%
                select(-test)}
    )
    
    output$B112Table<-renderTable(
        {CHEM111%>%
                filter(CRN==chem())%>%
                left_join(BIO112, by = "Days")%>%
                mutate(Conflict= ifelse(!( (Start.y<=Start.x) & (Start.x<=End.y) ) & (!( (Start.y<=End.x) & (End.x<=End.y) )) & (!( (Start.x<=Start.y) & (Start.y<=End.x) )) & (!( (Start.x<=End.y) & (End.y<=End.x) ))
                                        , 0, 1) )%>%
                select(Subject = Subj.y, Course = Crse.y, Section=Sec.y, Conflict)%>%
                group_by(Subject, Course, Section)%>%
                summarise(test = mean(Conflict))%>%
                filter(test==0)%>%
                arrange(Course, Section)%>%
                select(-test)}
    )
   
}

# Run the application 
shinyApp(ui = ui, server = server)
