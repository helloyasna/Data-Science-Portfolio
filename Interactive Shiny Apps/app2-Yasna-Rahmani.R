library(tidyverse)  # for data manipulation and graphics
library(tidytext)   # for text mining
library(DT)         # to render Data Tables nicely
library(plotly) # for interactive plotting
library(wordcloud) #for wordcloud

# ===============================================
# Import data
# ===============================================
dat <- read_csv(file = "harry_potter_books.csv", col_types = "ccc")

# Assuming your dataset has a 'book' and 'text' column
book_names = unique(dat$book)  # Use 'dat' instead of 'tbl'

# ===============================================
# Tokenization
# ===============================================
tbl_tokens = dat |> 
  unnest_tokens(word, text)

# Keywords for women and men
keywords_women =  "\\b(she|her|Hermione|Ginny|Lily|Molly|Bellatrix|Rita|Tonks|Minerva|Fleur|Cho)\\b"
keywords_men = "\\b(he|him|Harry|Ron|Draco|Fred|George|Neville|Severus|Albus|Remus|Voldemort|Lucius|Arthur)\\b"

female_tokens= dat |>
  filter(str_detect(text, keywords_women)) |>
  unnest_tokens(word, text) |>
  mutate(gender= "female")

male_tokens=  dat |>
  filter(str_detect(text, keywords_men)) |>
  unnest_tokens(word, text) |>
  mutate(gender= "male")

combined_tokens = bind_rows(female_tokens, male_tokens)

# Sentiment lexicon (e.g., "bing", "nrc", or "afinn")
sentiment_lexicon <- get_sentiments("bing")

# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Using Text Analysis to Explore Gender in Harry Potter"),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  fluidRow(
    # widgets of column 1
    column(4,
           p(em("Book for Analysis")),
           selectInput(inputId = "selected_book", 
                       label = "Choose a book", 
                       choices = c(book_names, "All books"),
                       selected = book_names[1])
    ), # closes column 1
    
    # widgets of column 2
    column(3,
           p(em("# of words for word cloud")),
           sliderInput(inputId = "top_n", 
                       label = "Top n words",
                       min = 1,
                       max = 50,
                       value = 10)
    ), # closes column 2es column 2
    
    # widgets of column 3
    column(3,
           p(em("Do you want to include filler words?")),
           radioButtons(inputId = "stopwords", 
                        label = "Stopwords", 
                        choices = c("use all tokens" = "opt1",
                                    "remove stopwords" = "opt2"), 
                        selected = "opt1")
    ), # closes column 3
    
    # widgets of column 4
    column(2,
           p(em("Select a gender for Analysis 1")),
           checkboxInput(inputId = "female_gender",
                         label = strong("Female"),
                         value = FALSE),
           checkboxInput(inputId = "male_gender",
                         label = strong("Male"),
                         value = FALSE)
    ) # closes column 4
  ),  # End of fluidRow
  
  hr(),  # Horizontal line
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("Sentiment Analysis by Gender for Selected Book"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("Word cloud Analysis by gender"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  )  # End of tabsetPanel
  
)  # End of fluidPage

# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  selected_tokens <- reactive({
    tokens <- combined_tokens  
    
    # Gender-based filtering
    if (input$female_gender & !input$male_gender) {
      tokens <- dat %>%
        filter(str_detect(text, regex(keywords_women, ignore_case = TRUE))) %>%
        unnest_tokens(word, text)
    } else if (input$male_gender & !input$female_gender) {
      tokens <- dat %>%
        filter(str_detect(text, regex(keywords_men, ignore_case = TRUE))) %>%
        unnest_tokens(word, text)
    } else {
      tokens <- combined_tokens
    }
    
    # Stopword removal based on the user's choice
    if (input$stopwords == "opt2") {
      tokens <- tokens %>%
        anti_join(stop_words, by = "word")
    }
    
    # Filter by selected book (if not "All books")
    if (input$selected_book != "All books") {
      tokens <- tokens %>%
        filter(book == input$selected_book)
    }
    
    # Join with the sentiment lexicon for analysis
    tokens <- tokens %>%
      inner_join(get_sentiments("bing"), by = "word", relationship= "many-to-many")
    
    tokens
  })
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  output$plot1 <- renderPlot({
    # Get the selected tokens, filtered by the chosen book and gender
    sentiment_data <- selected_tokens() %>%
      mutate(index = row_number() %/% 40) %>%  # Divide into bins of ~80 lines
      count(chapter,index,sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
      mutate(score = positive - negative)
    
    # Plot sentiment score as text progresses
    ggplot(sentiment_data, aes(x = index, y = score)) +
      geom_col(fill= "purple",color= "black", alpha = 0.7) +
      labs(title = paste("Sentiment Score Progression in", input$selected_book),
           x = "Text Progression (Indexed by 40)",
           y = "Sentiment Score") +
      theme_minimal()
  })
  
  output$table1 <- renderDataTable({
    sentiment_data <- selected_tokens() %>%
      mutate(index = row_number() %/% 40) %>%  
      count(chapter,index, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
      mutate(score = positive - negative)
    
  })
  
  # ===============================================
  # Outputs for the second TAB 
  # ===============================================
  output$plot2 <- renderPlot({
    # Filter and count words based on selected book and gender
    word_data <- selected_tokens() %>%
      count(gender, word, sort = TRUE)
  
    top_words <- word_data %>%
      group_by(gender) %>%
      slice_max(order_by = n, n = input$top_n) %>%
      ungroup()

    par(mfrow = c(1, 2))  # Set up a side-by-side layout for male and female
    
    if ("female" %in% top_words$gender) {
      with(subset(top_words, gender == "female"),
           wordcloud(words = word, freq = n, min.freq = 1,
                     max.words = input$top_n, colors = "purple", scale = c(4, 0.8)))
      title("Female Word Cloud")
    }
    
    if ("male" %in% top_words$gender) {
      with(subset(top_words, gender == "male"),
           wordcloud(words = word, freq = n, min.freq = 1,
                     max.words = input$top_n, colors = "blue", scale = c(4, 0.8)))
      title("Male Word Cloud")
    }
    
    par(mfrow = c(1, 1))  
  })
  output$table2 <- renderDataTable({
    word_data <- selected_tokens() %>%
      count(gender, word, sort = TRUE)
    
    # Filter to the top N words 
    top_words <- word_data %>%
      group_by(gender) %>%
      slice_max(order_by = n, n = input$top_n) |>
      ungroup()
    
    top_words %>%
      arrange(gender, desc(n)) %>%
      pivot_wider(names_from = gender, values_from = n, values_fill=0)

  })
}

# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)

