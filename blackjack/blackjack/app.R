#PACKAGES
library(shiny)
library(shinydashboard)
library(dplyr)
library(png)
library(tidyr)
library(shinyjs)
#simple deck data definitions
values = c('2','3','4','5','6','7','8','9','10','jack','king','queen', 'ace')
suits = c('hearts','diamonds','clubs','spades')
app_type = 'local'

if (!exists("app_type")) {
  windowsFonts("Dinot-CondBoldIta" = windowsFont("Dinot-CondBoldIta"))
} else if (app_type == 'prod') {
  dir.create('~/.fonts')
  file.copy("www/DINOT-CondBoldIta.otf", "~/.fonts")
  file.copy("www/DINOT-Black.otf", "~/.fonts")
  file.copy("www/DINOT-Bold.otf", "~/.fonts")
  file.copy("www/DINOT-CondMedium.otf", "~/.fonts")
  file.copy("www/DINOT-CondBlackIta.otf", "~/.fonts")
  system('fc-cache -f ~/.fonts')
} else {
  windowsFonts("Dinot-CondBoldIta" = windowsFont("Dinot-CondBoldIta"))
}

evaluate_hand = function(dealer, player){
  
  if(player > 21){
    text = 'Lost'
  }else if(dealer == player){
    text = 'Push'
  }else if(dealer > 21){
    text = 'Win'
  }else if(dealer > player ){
    text = 'Lost'
  }else if(player > dealer){
    text = 'Win'
  }
  
  return(text)
  
}

#Start UI------------------------------------------------------
ui <- dashboardPage(
  
  dashboardHeader(title = "Blackjack"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Play Game", tabName = "play", icon = icon("diamond"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      
      tabItem(tabName = 'play',
              fluidRow(
                column(width = 6,
                align = 'center',
                uiOutput('dealer_card_one')
                ),
                column(width = 6,
                uiOutput('dealer_card_two')
                )
                
              ),
              fluidRow(
                column(12,
                align = 'center',
                uiOutput('player_card_one'),
                uiOutput('player_card_two')
                )
              ),
              fluidRow(
                column(12,
                       align = 'center',
                actionButton(inputId = 'deal_me', 'Deal!')
                )
              ),
              fluidRow(
                column(12,
                       
                align = 'center',
                uiOutput('player_actions')
                )
              )
              )
      
      
    )
    
  )
)

#Start Server--------------------------------------------------
server <- function(input, output) {
  
  #Dealer Cards
  dealer_cards = reactiveVal(NULL)
  player_cards = reactiveVal(NULL)
  split_cards = reactiveVal(NULL)
  used_cards = reactiveVal(NULL)
  first_hand = reactiveVal(TRUE)
  
  #reactive to keep track of the deck
  deck = reactive({
    n_decks = 5
    numbers = c(1:(52*n_decks))
    
    df = data.frame(
      'in_deck' = numbers
    )
    if(first_hand()){
      first_hand(FALSE)
      return(df)
      
    }else{
      deck_now = df %>% 
        anti_join(used_cards(), by = 'in_deck')
      return(deck_now)
      
    }
  })
  
  
  #when deal is hit
  game_over = reactiveVal(FALSE)
  observeEvent(input$deal_me, {
    
    if(game_over()){
      first_hand(TRUE)
      dealer_cards(NULL)
      player_cards(NULL)
      split_cards(NULL)
      used_cards(NULL)
      player_stand(FALSE)
      game_over(FALSE)
    }
    
    
    #can make this dynamic if more than one player
    cards_needed = 4
    #use sample_n to randomly sample the deck dataframe
    cards_current = sample_n(deck(), cards_needed)
    #if this is the first hand of the shoe then current cards are
    #the only cards that need to be removed
    
    
    
    
    
    if(is.null(used_cards())){
      used_cards(cards_current)
    }else{
      #otherwise bind the used_cards with the current cards
      df = bind_rows(used_cards(), cards_current)
      used_cards(df)
    }
    
    #assign the relevant cards to the dealer/player
    dealer_cards(cards_current %>% slice(1,3))
    player_cards(cards_current %>% slice(2,4))
    
  })
  
  observeEvent(input$hit_button, {
    
    #can make this dynamic if more than one player
    cards_needed = 1
    #use sample_n to randomly sample the deck dataframe
    cards_current = sample_n(deck(), cards_needed)

    #bind the used_cards with the current cards
    df = bind_rows(used_cards(), cards_current)
    used_cards(df)
    
    new_cards = bind_rows(player_cards(), cards_current)
    #assign the relevant cards to the player
    player_cards(new_cards)

    
  })
  
  player_stand = reactiveVal(FALSE)
  observeEvent(input$stand_button, {
    player_stand(TRUE)
  })
  
  observeEvent(input$dd_button, {
    
    #can make this dynamic if more than one player
    cards_needed = 1
    #use sample_n to randomly sample the deck dataframe
    cards_current = sample_n(deck(), cards_needed)
    
    #bind the used_cards with the current cards
    df = bind_rows(used_cards(), cards_current)
    used_cards(df)
    
    new_cards = bind_rows(player_cards(), cards_current)
    #assign the relevant cards to the player
    player_cards(new_cards)
    
    #update it as if player stood to move onto dealer
    player_stand(TRUE)
  })

  observeEvent(input$split_button,{
    
    #remove the second card from original hand
    player_cards(player_cards %>% slice(1))
    
    #make second hand
    split_cards(player_cards %>% slice(1))
    
  })
  
  output$dealer_card_two = renderUI({
    if(is.null(dealer_cards())){
      
    }else{
        card_wanted = dealer_cards() %>% 
          slice(-c(1)) %>% 
          pull(in_deck)
        
        
        content = '<div>'
        
        for(i in 1:length(card_wanted)){
          #use modulo to get relevant suit/card value
          suit = suits[card_wanted[i] %% 4 + 1]
          value = values[card_wanted[i] %% 12 + 1]
          image_name = paste0('cards/',value,'_of_',suit,'.png')
          image_tag = paste0('<img src="',image_name   ,'", height = "80px">')
          content = paste0(content, image_tag)
        }
        content = paste0(content, '<div>')
        return(HTML(content))
        
    
      
    }
  })
  
  output$dealer_card_one = renderUI({
    if(is.null(dealer_cards())){}else{
      if(!hand_status()){
      card_wanted = dealer_cards() %>% 
        slice(1) %>% 
        pull(in_deck)
      #use modulo to get relevant suit/card value
      suit = suits[(card_wanted %% 4) + 1]
      value = values[(card_wanted %% 12) + 1]
      image_name = paste0('cards/',value,'_of_',suit,'.png')
      return(tags$img(src = image_name, height = 80))
      }else{
        return(tags$img(src = 'cards/back_card.png', height = 80))
      }
    }
  })
  
  
  
  
  output$player_card_one = renderUI({
    if(is.null(player_cards())){}else{
      card_wanted = player_cards() %>% 
        slice(1) %>% 
        pull(in_deck)
      #use modulo to get relevant suit/card value
      suit = suits[(card_wanted %% 4) + 1]
      value = values[(card_wanted %% 12) + 1]
      image_name = paste0('cards/',value,'_of_',suit,'.png')
      tags$img(src = image_name, height = 80)
    }
  })
  
  output$player_card_two = renderUI({
    if(is.null(player_cards())){
      
    }else{

      card_wanted = player_cards() %>% 
        slice(-c(1)) %>% 
        pull(in_deck)
   
      
      content = '<div>'
      
      for(i in 1:length(card_wanted)){
          #use modulo to get relevant suit/card value
          suit = suits[card_wanted[i] %% 4 + 1]
          value = values[card_wanted[i] %% 12 + 1]
          image_name = paste0('cards/',value,'_of_',suit,'.png')
          image_tag = paste0('<img src="',image_name   ,'", height = "80px">')
          content = paste0(content, image_tag)
      }
      content = paste0(content, '<div>')
      return(HTML(content))
      
    }
  })
  
  
  output$player_actions = renderUI({
    if(!(first_hand())){
      
      if(hand_status()){
        shinyjs::hide(id = "deal_me")
        
        
        if(!is.null(split_cards())){
          components = fluidRow(
            column(6,
                   actionButton('hit_button', 'HIT'),
                   actionButton('dd_button','DOUBLE DOWN',  class="btn btn-warning"),
                   actionButton('stand_button','STAND',  class="btn btn-primary")
            ),
            column(6,
                   
                   actionButton('hit_button_split', 'HIT'),
                   actionButton('dd_button_split','DOUBLE DOWN',  class="btn btn-warning"),
                   actionButton('stand_button_split','STAND',  class="btn btn-primary")
            )
          )
          
          
          
          
        }else if(nrow(player_cards()) == 2 && ((player_cards()$in_deck[1] %% 12) == (player_cards()$in_deck[2] %% 12))){
            components = fluidRow(
              actionButton('hit_button', 'HIT'),
              actionButton('split_button','SPLIT'),
              actionButton('dd_button','DOUBLE DOWN', class="btn btn-warning"),
              actionButton('stand_button','STAND',  class="btn btn-primary")
            )
            
          
          
        }else{
          components = fluidRow(
            actionButton('hit_button', 'HIT'),
            actionButton('dd_button','DOUBLE DOWN',  class="btn btn-warning"),
            actionButton('stand_button','STAND',  class="btn btn-primary")
          )
        }
        
        
        
        
      }else{
        
        
        components = fluidRow(column(6,
                                     tags$h1(evaluate_hand(dealer_count(), player_count()))
        )
        )
        
        shinyjs::show(id = "deal_me")
        game_over(TRUE)
        
      }
      return(components)
      
    }else{
      return(NULL)
    }
    
  })
  
  
  hand_status = reactive({
    
    if(is.null(player_cards())){
    
    }else{
        if(player_count() >= 21 | player_stand()){
          return(FALSE)
        } 
    }
    return(TRUE)
    
  })
  
 
  observeEvent(hand_status(), {
    if(hand_status()){

    }else{
      if(player_count() > 21){
        
      }else if(player_count() == 21 | player_stand()){
        
        while (dealer_count() < 17) {
          #can make this dynamic if more than one player
          cards_needed = 1
          #use sample_n to randomly sample the deck dataframe
          cards_current = sample_n(deck(), cards_needed)
          
          #bind the used_cards with the current cards
          df = bind_rows(used_cards(), cards_current)
          used_cards(df)
          
          new_cards = bind_rows(dealer_cards(), cards_current)
          #assign the relevant cards to the player
          dealer_cards(new_cards)
          dealer_count()
        }
        
      }
      
    }
  })
  
  
  player_count = reactive({
    
    p_card_values = player_cards()$in_deck %% 12 + 2
    p_card_values = ifelse(p_card_values == 0, 11,p_card_values)
    p_card_values = ifelse(p_card_values > 10, 10,p_card_values)
    
    sum_player = sum(p_card_values)
    if(sum_player > 21){
      p_card_values = ifelse(p_card_values == 11, 1,p_card_values)
      sum_player = sum(p_card_values)
    }
    
    return(sum_player)
    
  })
  
  dealer_count = reactive({
    
    d_card_values = dealer_cards()$in_deck %% 12 + 2
    d_card_values = ifelse(d_card_values == 0, 11,d_card_values)
    d_card_values = ifelse(d_card_values > 10, 10,d_card_values)
    
    sum_player = sum(d_card_values)
    if(sum_player > 21){
      d_card_values = ifelse(d_card_values == 11, 1,d_card_values)
      sum_player = sum(d_card_values)
    }
    
    return(sum_player)
    
  })
  
  
  
}

shinyApp(ui, server)