# Shiny app for tidygraph

#tidygraph <- readRDS("data/tidygraph.RDAT")
library(shiny)
party_cols <- c("chartreuse3", "dodgerblue3", "firebrick2","goldenrod1","grey30")

pullm <- function(df, vars) {
  outlist <- list()
  for(i in seq_along(vars)) {
    outlist[[i]] <- pull(df, vars[[i]])
  }
  outlist
}

parties <- tidygraph %>% activate(nodes) %>% mutate(party = as.character(party)) %>% pull(party) %>% unique()
nodes <- tidygraph %>% activate(nodes) %>% pull(name)

pcoltib <- tibble(party_cols, party_names = c("(CB)", "(Con)", "(Lab)", "(LD)", "(other)"))

ui <- fluidPage(
  selectInput(inputId = "nodeSelect", label = "Select a node to highlight", choices = nodes, selected = "Hayter"),
  checkboxInput(inputId = "labels", label = "Add labels?", value = F),
  radioButtons(inputId = "showAllNodes", label = "Which connections?", choices = c("show all", "show single")),
  radioButtons(inputId = "partySelect", label = "Select a party to highlight", choices = c(parties, "none"), selected = "none"),
  radioButtons(inputId = "partySelect2", label = "Select a second party to highlight", choices = c(parties, "none"), selected = "none"),
  actionButton(inputId = "renew_graph", label = "Replot graph"),
  actionButton(inputId = "original", label = "show original"),
  plotOutput(outputId = "main_graph"),
  tableOutput(outputId = "byparty")
)

server <- function(input, output) {
  coords <- reactive({
    if (input$renew_graph) {
      create_layout(tidygraph, "dh") %>% select(x, y)
    } else {
      create_layout(tidygraph, "dh") %>% select(x, y)
    }
  })
  
  # surrounding_nodes <- reactive({
  # 
  # })
  # 
  output$main_graph <- renderPlot({
    party_selection1 <- input$partySelect
    party_selection2 <- input$partySelect2
    print(party_selection2)
    
    
    party_nodes <- tidygraph %>% activate(nodes) %>% filter(party %in% party_selection1) %>% pull(name)
    party_nodes2 <- tidygraph %>% activate(nodes) %>% filter(party %in% party_selection2) %>% pull(name)
    
    partyMixColours <- c(first = "blue", second = "red", mixed = "grey")
    
    switch1 <- input$labels
    
    selected_tg <- tidygraph %>% activate(edges) %>% filter(value.x == input$nodeSelect | value.y == input$nodeSelect)
    surrounding_nodes <- selected_tg %>% pullm(c("value.x", "value.y")) %>% unlist() %>% unique()
    selected_node = input$nodeSelect
    
    original <- tidygraph %>%
      mutate(select_edge = ifelse(value.x %in% selected_node | value.y %in% selected_node, 1, 0.2)) %>%
      ggraph(layout = "manual", circular = FALSE, node.positions = coords()) + 
      geom_edge_link(aes(alpha = select_edge, width = weights)) + 
      scale_edge_width(range = c(0.5,1.5)) +
      geom_node_point(aes(colour = party)) +
      scale_colour_manual(values = party_cols) +
      {if(switch1)geom_node_label(aes(filter = name %in% surrounding_nodes, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines'))}+
      theme_graph() +
      theme(legend.position="none")

    selected_node <- tidygraph %>%
      activate(edges) %>% filter(value.x == selected_node | value.y == selected_node) %>%
      activate(nodes) %>% filter(name %in% surrounding_nodes) %>%
      ggraph(layout = "lgl") +
      geom_edge_link(aes(width = weights)) +
      scale_edge_width(range = c(0.5,1.5)) +
      geom_node_point(aes(colour = party)) +
      scale_colour_manual(values = party_cols) +
      {if(switch1)geom_node_label(aes(filter = name %in% surrounding_nodes, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines'))}+
      theme_graph() +
      theme(legend.position="none")
    
    n_parties_selected <- sum(c(party_selection1, party_selection2) != "none")
    
    by_party <- tidygraph 
    by_party %<>% mutate(pselect = ifelse(party.x %in% c(party_selection1, party_selection2) | party.y %in% c(party_selection1 , party_selection2), T, F))
    

    by_party %<>% mutate(ptype = ifelse(party.x == party_selection1 & party.y == party_selection1, party_selection1, NA))
    by_party %<>% mutate(ptype = ifelse(party.x == party_selection2 & party.y == party_selection2, party_selection2, ptype))
    if (n_parties_selected == 1){
      by_party %<>% mutate(ptype = ifelse((party.x %in% c(party_selection1, party_selection2) | party.y %in% c(party_selection1,party_selection2)) & is.na(ptype), "mixed", ptype))    
    }
    if (n_parties_selected == 2) {
      by_party %<>% mutate(ptype = ifelse((party.x == party_selection1 & party.y == party_selection2) | (party.x == party_selection2 & party.y == party_selection1), "mixed", ptype))    
    }
    by_party %<>% activate(nodes) %>%
       mutate(in_pselect = party %in% c(party_selection1, party_selection2))

    # by_party %<>% 
    #   mutate(pselect = ifelse(party.x %in% c(party_selection1, party_selection2) | party.y %in% c(party_selection1 , party_selection2), T, F)) %>%
    #   mutate(ptype = ifelse(party.x == party_selection1 & party.y == party_selection1, party.x, 
    #                         ifelse(party.x == party_selection2 & party.y == party_selection2, party.x,
    #                                ifelse(party.x %in% c(party_selection1, party_selection2) | party.y %in% c(party_selection1,party_selection2), "mixed", NA)))) %>%
    #   activate(nodes) %>%
    #   mutate(in_pselect = party %in% c(party_selection1, party_selection2))
    # 
    #print(by_party)
    edgecol1 <- pcoltib$party_cols[pcoltib[2] == party_selection1]
    if(length(edgecol1)==0) {edgecol1 <- "grey"}
    edgecol2 <- pcoltib$party_cols[pcoltib[2] == party_selection2]
    if(length(edgecol2)==0) {edgecol2 <- "grey"}
    print(edgecol1)
    print(edgecol2)
     by_party_graph <- by_party %>%
      activate(edges) %>%
      mutate(ptype = as.factor(ptype)) %>%
      mutate(ptype = fct_relevel(ptype, "mixed", party_selection1)) %>%
      print(.) %>%
      ggraph(layout = "manual", circular = FALSE, node.positions = coords()) +
      scale_edge_alpha_manual(values = c(0.6, 0.6)) +
      geom_edge_link(aes(alpha = pselect, width = weights, colour = ptype)) +
      scale_edge_colour_manual(values = c("grey", edgecol1, edgecol2)) +
      scale_edge_width(range = c(0.5,1.5)) +
      geom_node_point(aes(colour = party)) +
      scale_colour_manual(values = c(party_cols)) +
      {if(switch1)geom_node_label(aes(filter = name %in% surrounding_nodes, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines'))}+
      theme_graph() +
      theme(legend.position="none")

    
    if(input$showAllNodes == "show single") {
      print("a")
      selected_node
    } else if (party_selection1 == "none" & party_selection2 == "none") {
      print("b")
      original 
    } else {
      print("c")
      by_party_graph
    }
  })
  
  output$byparty <- renderTable({
    party_selection1 <- input$partySelect
    party_selection2 <- input$partySelect2
    print(party_selection2)
    
    n_parties_selected <- sum(c(party_selection1, party_selection2) != "none")
    
    party_nodes <- tidygraph %>% activate(nodes) %>% filter(party %in% party_selection1) %>% pull(name)
    party_nodes2 <- tidygraph %>% activate(nodes) %>% filter(party %in% party_selection2) %>% pull(name)
    
    partyMixColours <- c(first = "blue", second = "red", mixed = "grey")
    
    switch1 <- input$labels
    
    selected_tg <- tidygraph %>% activate(edges) %>% filter(value.x == input$nodeSelect | value.y == input$nodeSelect)
    surrounding_nodes <- selected_tg %>% pullm(c("value.x", "value.y")) %>% unlist() %>% unique()
    selected_node = input$nodeSelect
    by_party <- tidygraph 
    by_party %<>% mutate(pselect = ifelse(party.x %in% c(party_selection1, party_selection2) | party.y %in% c(party_selection1 , party_selection2), T, F))
    by_party %<>% mutate(ptype = ifelse(party.x == party_selection1 & party.y == party_selection1, party_selection1, NA))
    by_party %<>% mutate(ptype = ifelse(party.x == party_selection2 & party.y == party_selection2, party_selection2, ptype))
    if (n_parties_selected == 1){
      by_party %<>% mutate(ptype = ifelse((party.x %in% c(party_selection1, party_selection2) | party.y %in% c(party_selection1,party_selection2)) & is.na(ptype), "mixed", ptype))    
    }
    if (n_parties_selected == 2) {
      by_party %<>% mutate(ptype = ifelse((party.x == party_selection1 & party.y == party_selection2) | (party.x == party_selection2 & party.y == party_selection1), "mixed", ptype))    
    }
    by_party %<>% activate(nodes) %>%
      mutate(in_pselect = party %in% c(party_selection1, party_selection2))
    
    # by_party %<>% 
    #   mutate(pselect = ifelse(party.x %in% c(party_selection1, party_selection2) | party.y %in% c(party_selection1 , party_selection2), T, F)) %>%
    #   mutate(ptype = ifelse(party.x == party_selection1 & party.y == party_selection1, party.x, 
    #                         ifelse(party.x == party_selection2 & party.y == party_selection2, party.x,
    #                                ifelse(party.x %in% c(party_selection1, party_selection2) | party.y %in% c(party_selection1,party_selection2), "mixed", NA)))) %>%
    #   activate(nodes) %>%
    #   mutate(in_pselect = party %in% c(party_selection1, party_selection2))
    # 
    #print(by_party)
    edgecol1 <- pcoltib$party_cols[pcoltib[2] == party_selection1]
    if(length(edgecol1)==0) {edgecol1 <- "grey"}
    edgecol2 <- pcoltib$party_cols[pcoltib[2] == party_selection2]
    if(length(edgecol2)==0) {edgecol2 <- "grey"}

    by_party %<>%
      activate(edges) %>%
      mutate(ptype = as.factor(ptype)) %>%
      mutate(party = fct_relevel(ptype, "mixed", party_selection1)) 
  })
}

shinyApp(ui = ui, server = server)