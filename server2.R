#
#
#The goal of this app is to reduce the time to make a picklist from freezerpro. After obtaining a raw list in a CSV from freezerpro, you can upload it here. 
#The app will pick cells to your desired cell count, count how many cells are there in both archive and non archive batch, and will point out the list that need more freezebacks. 

library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(writexl)
library(stringr)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    if (grepl("\\.csv$", input$file$name)) {
      read.csv(input$file$datapath)
    } else if (grepl("\\.(xlsx|xls)$", input$file$name)) {
      read_excel(input$file$datapath)
    } else {
      stop("Invalid file format")
    }
  })
  
  #Store the final data in this df
  sorted_data <- reactive({
    req(data())
    name_counts <- data()
    
    # Group data by Name and count the number of times each name appears
    name_counts <- name_counts %>%
      group_by(Name) %>%
      arrange(Name,UID) %>% mutate(count_archive_included = n()) %>%
      filter(Freezer != "+4C Walk-In (PRISM)")
    
    # View the results
    original_counts <- name_counts
    
    # Searching each Cell Name by Batch, removing the smallest UID (Archive Batch *theoretically?)
    name_counts %>% group_by(Name,UID) %>% summarise() %>% group_by(Name) %>% slice(if (n() > 1) 2:n() else 1)  %>% ungroup() -> unique_ids
    
    # records all the UIDs of non-archive batch
    unique_id_test <- unique(unique_ids$UID)
    
    # Goes into OG dataset and grab all values from the non-archive batch
    name_counts %>% filter(UID %in% unique_id_test) -> non_archive_batch
    
    #specifically looking for vials at not stored at GPP
    original_counts %>% group_by(Name) %>% filter(!any(Freezer == "PRISM Archive at 320 Charles")) %>% summarise() -> NO_320_CHARLES_VIALS
    
    #safety double check for Archive Batch
    non_archive_batch %>%
      filter(!grepl("PRISM Archive at 320 Charles", Freezer)) -> archive_check
    
    # Check if any rows were removed
    if (nrow(archive_check) < nrow(non_archive_batch)) {
      # Get the names of the samples that were removed
      removed_samples <- unique(non_archive_batch$Name[!non_archive_batch$Name %in% archive_check$Name])
      
      # Print a statement saying the samples were removed
      cat("All IDs named", paste0(removed_samples, collapse = ", "), "have been removed completely from the data set.\n")
    }
    
    #From the non-archive batch, check counts of vials and marks for freezebacks or not. The logic is picking fron non-archive sets and picking sets that have less than 7 vials.
    non_archive_batch %>% mutate(count_non_archive = n()) %>% 
      mutate(freezebacks = ifelse(count_non_archive <= 7, "yes", "no")) %>% group_by(Name) -> non_archive_batch_fb
    
    # Make the final picklist, and pick cells to reach our target cell count. 
    target_cells <- input$cells
    non_archive_batch_fb$Cells.In.One.Vial <- as.numeric(non_archive_batch_fb$Cells.In.One.Vial)
    non_archive_batch_fb %>%
      group_by(Name, UID) %>%
      filter(sum(Cells.In.One.Vial) >= target_cells | row_number() == n()) %>%
      ungroup() %>%
      group_by(UID) %>%
      mutate(n_samples = n()) %>%
      group_by(Name) %>%
      arrange(UID,Freezer,Level1,Box,Position) %>%
      slice(seq_len(min(which(cumsum(Cells.In.One.Vial) >= target_cells), n()))) %>%
      ungroup() %>%
      mutate(UID_Batch_Count = n_samples, note = ifelse(n_samples < 4, paste0("There are only " , UID_Batch_Count , " vial(s) for sample ID ", UID), "")) %>%
      select(UID,Name,Cells.In.One.Vial,Vials,Growth.Medium,Freeze.Date,Freezer,BARCODE,Level1,Box,Position,count_archive_included,count_non_archive,freezebacks,UID_Batch_Count,note) %>%
      mutate(Position = as.numeric(Position)) %>%
      arrange(Freezer,Level1,Box,Position)
    
    
  })
  
  #removes all the garabage columns for easy of use in case of repicks
  cleaned_data <- reactive({
    req(data())
    name_counts <- data()
    
    
   cleaned_fp <- name_counts %>% 
    select(UID,Name,Cells.In.One.Vial,Vials,Growth.Medium,Freeze.Date,Freezer,BARCODE,Level1,Box,Position) %>%
    arrange(Name,UID)
   cleaned_fp
    
  })
  
  #Stores all the freezebacks in a df
  freezebacks_yes <- reactive({
    req(sorted_data())
    sorted_data() %>%
      arrange(Name) %>%
      filter(freezebacks == "yes") %>%
      distinct(Name, .keep_all = TRUE)
  })
  
  #store original freezerpro data
  original_fb_sheet <- reactive({
    req(data())
    data() %>%
      arrange(Name,UID)
  })
  
  
  
  
  output$table <- renderDT({
    datatable(sorted_data(), options = list(orderClasses = TRUE))
  })
  
  #Goal is to export an excel sheet that has date, name of choosing, and 4 tabs worth of infomation
  output$download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_",input$pool_name,"_picklist.xlsx")
    },
    content = function(file) {
      write_xlsx(list(
        Picklist = sorted_data(),
        freezeback = freezebacks_yes(),
        Cleaned_Data = cleaned_data(),
        original_sheet = original_fb_sheet()
        
      ), file)
    }
  )
}