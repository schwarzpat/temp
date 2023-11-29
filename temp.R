# R SETTINGS ---
#options(renv.consent = TRUE)
#renv::restore()
Sys.setenv(LC_TIME = "English")
#options(shiny.launch.browser = TRUE)

# LOAD LIBRARIES ---
library(tidyverse)
library(timetk)
library(shiny)
library(finnts)
library(openxlsx)
library(here)
library(feasts)
library(recipes)
library(purrr)
library(tictoc)

# PREPARE LOGGING ---
now <- Sys.time()
date_str <- format(now, "%Y-%m-%d %H.%M.%S")

# MODEL SPECIFICATION ---
num_hyperparameters <- 5
models <- c("arima")
external_regressors <- c("FI",
                         "SE",
                         "NO",
                         "DK",
                         "Weekend",
                         "firstDayOfMonth",
                         "firstDayOfQuarter",
                         "fi_salary",
                         "fi_pension",
                         "no_salary",
                         "no_pension",
                         "se_salary",
                         "se_pension",
                         "dk_salary",
                         "dk_pension",
                         "securitas",
                         "WorkingDayAfterHoliday_FI",
                         "WorkingDayAfterHoliday_SE",
                         "WorkingDayAfterHoliday_NO",
                         "WorkingDayAfterHoliday_DK")

# DEFINE UI ---
ui <- fluidPage(
  titlePanel("Forecasting"),
  helpText("Some text"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = c(".csv")),
      numericInput("horizon", "Forecast horizon", value = 3),
      actionButton("run_forecast", "Run Forecast"),
      downloadButton("download_forecast", "Download Forecast"),
      downloadButton("download_diagnostics", "Download Diagnostics")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Forecast Plot", plotOutput("forecast_plot")),
                  tabPanel("Forecast Table", tableOutput("forecast_table")),
                  tabPanel("Trend Decomposition", plotOutput("decomp"))
      )
    )
  )
)

# DEFINE SERVER ---
server <- function(input, output, session) {
  
# FUNCTIONS ---
  
# FIRST WORKDAY OF QUARTER ---
find_first_weekday_of_quarter <- function(date) {
  while(wday(date) %in% c(6,7)) {
    date <- date + 1
  }

  return(date)
}

# ADJUST FOR WEEKENDS ---
adjust_for_weekends <- function(all_dates, days, direction) {
  selected_days <- lapply(all_days, floor_date(all_days, "months"),
  function(x) {
    target_days <- if("last" %in% days) {
      c(x[day(x) %in% days], last(x[wdays(x) %in% 2:6]))
    } else {
      x[day(x) %in% days]
    }
    for (i in seq_along(target_days)) {
      while (wday(target_days[i]) %in% c(1, 7)) {
        if (direction == "preceding") {
          target_days[i] <- target_days[i] - days(1)
        } else {
          target_days[i] <- target_days[i] + day(2)
        }
      }
    }
    return(target_days)
  })
return(do.call(c, selected_days))
}
        
# FIND PAYMENT DAYS ---
payment_days <- function(year) {
  start_date <- ymd(paste0(year, "-01-01"))
  end_date <- ymd(paste0(year, "-12-31"))
  all_days <- seq(start_date, end_date, by = "days")
  
  dk_days_salary <- adjust_for_weekends(all_days, "last", "preceding")
  dk_days_pension <- adjust_for_weekends(all_days, 1, "following")
  
  se_days_salary <- adjust_for_weekends(all_days, 25, "preceding")
  se_days_pension <- adjust_for_weekends(all_days, c(18,19), "preceding")
  
  no_days_salary <- adjust_for_weekends(all_days, 12, "preceding")
  no_days_pension <- adjust_for_weekends(all_days, 20, "preceding")
  
  fi_days_salary <- adjust_for_weekends(all_days, c(15, "last"), "preceding")
  fi_days_pension <- adjust_for_weekends(all_days, 7, "following")
  
  securitas <- adjust_for_weekends(all_days, c(15, 25), "preceding")
  
  return(list(dk_salary = dk_days_salary,
              dk_pension = dk_days_pension,
              se_salary = se_days_salary,
              se_pension = se_days_pension,
              no_salary = no_days_salary,
              no_pension = no_days_pension,
              fi_salary = fi_days_salary,
              fi_pension = fi_days_pension,
              securitas = securitas))
}

# IN MULTIPLE YEARS ---
payment_days_multiyear <- function(start_year, end_year) {
  all_payment_days <- lapply(start_year:end_year, payment_days)
  
  all_columns <- names(all_payment_days[[1]])
  
  concatenated_days <- lapply(all_columns, function(x) {
    do.call(c, lapply(all_payment_days, function(y) y[[x]]))
  })
  names(concatenated_days) <- all_columns
  
  start_date <- ymd(paste0(start_year, "-01-01"))
  end_date <- ymd(paste0(end_year, "-12-31"))
  all_days <- seq(start_date, end_date, by = "days")
  
  df <- data.frame(date = all_days)
  for (col in all_columns) {
    df[[col]] <- 0
  }
  
  
  df <- df %>%
    mutate(across(all_columns,
                  ~ifelse(date %in% concatenated_days[[cur_column()]], 1, 0),
                  .names = "{.col}"))

  return(df)
}

# MOVE PAYDAYS BACK ---
movePayday <- function(df, holidayColumn, paydayColumn) {
  df$Date <- as.Date(df$Date)
  for (i in 1:nrow(df)) {
    if (df[i, holidayColumn] ==1 && (
        df[i, holidayColumn] != "None" ||
        df[i, "Weekend"] == 1)) {
      j <- i
      while (df[j, paydayColumn] != "None" ||
               df[j, "Weekend"] == 1) {
        j <- j - 1
        if (j > nrow(df)) {
          break
        }
      if (j <= nrow(df)) {
        df[i, paydayColumn] <- 0
        df[j, paydayColumn] <- 1
      } 
    }
    } 
  }
    return(df)
}
  
# MOVE PAYDAYS FORWARD ---
movePaydayForward <- function(df, holidayColumn, paydayColumn) {
  df$Date <- as.Date(df$Date)
  for (i in 1:nrow(df)) {
    if (df[i, holidayColumn] ==1 && (
        df[i, holidayColumn] != "None" ||
        df[i, "Weekend"] == 1)) {
      j <- i
      while (df[j, paydayColumn] != "None" ||
               df[j, "Weekend"] == 1) {
        j <- j + 1
        if (j > nrow(df)) {
          break
        }
      if (j <= nrow(df)) {
        df[i, paydayColumn] <- 0
        df[j, paydayColumn] <- 1
      }
      }
    }
  }
  return(df)
}

is_next_workday <- function(holiday, weekend) {
  length_h <- length(holiday)
  next_workday_after_holiday <- rep(0, length_h)
  
  for (i in 1:(length_h - 1)) {
    if (holiday[i] == 1 && weekend[i] == 0) {
      j <- i + 1
      while (j <= length_h && (weekend[j] == 1 || holiday[j] == 1)) {
        j <- j + 1
      }
      if (j <= length_h) {
        next_workday_after_holiday[j] <- 1
      }
    }
  }
  return(next_workday_after_holiday)
}  

  
# LOAD DATA

data <- reactive({
  req(input$file)
  inFile <- input$file
  if (is.null(inFile)) {
    return(NULL)
  }
  df <- read.csv(inFile$datapath, header = T)
  id_list <-  unique(df$country)
  
  hist_data <- df %>%
    rename(id = country) %>%
    mutate(Date = as.Date(date)) %>%
    filter(Date >= today() - years (3) - months(0)) %>%
    filter(id %in% id_list) %>%
    group_by(id, Date) %>%
    summarise(y = sum(y)) %>%
    mutate(id =  as.character(id))
  
  hist_min <- as.Date(min(hist_data$Date))
  hist_max <- as.Date(max(hist_data$Date)) -1
  hist_dates <- seq(hist_min, hist_max, by = "days")
  future_dates <- hist_max + 1 + 0:(input$horizon - 1)
  
  hist_data <- hist_data %>%
    filter(Date %in% hist_dates)
  
  filler_list <- list()
  future_list <- list()
  
  for (id in id_list) {
    filler <- data.frame(id = id,
                         Date = hist_dates,
                         y = 0)
    future_id <- data.frame(id = id,
                            Date = future_dates)
    filler_list[[id]] <- filler
    future_list[[id]] <- future_id
  }
  
  filler_df <- do.call(rbind, filler_list)
  future_df <- do.call(rbind, future_list)
  
  rm(filler_list, future_list, future_id, filler)
  
  missing <- filler_df %>%
    anti_join(hist_data, by = c("id", "Date"))
  
  hist_filled <- hist_data %>%
    full_join(missing, by = c("id", "Date"), keep = FALSE) %>%
    select(-y.y) %>%
    rename(y = y.x)
  
  rm(missing, filler_df)
  
  hist_filled$y <- hist_filled$y %>%
    replace_na(1)
  
  all_dates <- rbind(hist_filled, future_df) %>%
    arrange(id, Date)
  
  rm(hist_filled, future_df)
  
  generated_holidays <-
    read.csv(here("HOLIDAYS/generated_holidays.csv")) %>%
    filter(id %in% id_list, id != "GB") %>%
    mutate(ds = as.Date(ds)) %>%
    filter(ds >= hist_min & ds <= max(future_dates)) %>%
    filter(holiday != "Sondag")
  
  # all_dates <- all_dates %>%
  #   left_join(generated_holidays, by = c("id" = country, "Date" = "ds")) %>%
  #   select(-year)
  
  pivot <- generated_holidays %>%
    pivot_wider(names_from = country,
                values_from = holiday,
                values_fill = "None") 
  
  all_dates <- all_dates %>%
    left_join(pivot, by = c("Date" = "ds")) %>%
    select(-year) %>%
    map_if(~ .x %in% names(my_df)[nchar(names(my_df)) == 2], ~ replace_na(.x, "None")) %>%
    mutate(DayOfMonth = day(Date),
           Weekend = ifelse(DayOfWeek %in% c("Saturday", "Sunday"), 1, 0)) %>%
    mutate_if(is.character, as.factor) %>%
    row_wise() %>%
    mutate(
      firstDayOfQuarter = as.numeric(Date == find_first_weekday_of_quarter(floor_date(Date, "quarter"))),
      firstDayOfMonth = as.numeric(Date == find_first_weekday_of_quarter(floor_date(Date, "month"))) 
    ) %>%
  ungroup()
  
  all_dates$id <- 1
  
  # all_dates <- all_dates %>%
  #   group_by(id, Date, DK, FI, NO, SE, Weeken
  
  df_payment_days <- payment_days_multiyear(2015, 2030)
  
  all_dates <- all_dates %>%
    left_join(df_payment_days)
  
  # all_dates <- all_dates %>%
  #   movePayday(all_dates, "DK", "dk_pension")%>%
  #   movePayday(all_dates, "FI", "fi_pension") %>%
  #   movePayday(all_dates, "NO", "no_pension") %>%
  #   movePayday(all_dates, "SE", "se_pension") %>%
  #   movePayday(all_dates, "DK", "dk_salary") %>%
  #   movePayday(all_dates, "FI", "fi_salary") %>%
  #   movePayday(all_dates, "NO", "no_salary") %>%
  #   movePayday(all_dates, "SE", "se_salary") %>%
  #   movePayday(all_dates, "FI", "securitas") %>%
  #   movePaydayForward(all_dates, "FI", "firstDayOfQuarter")%>%
  #   movePaydayForward(all_dates, "FI", "firstDayOfMonth")
  
  
  # Define parameters for movePayday and movePaydayForward
  payday_params <- list(
    list("DK", "dk_pension"),
    list("FI", "fi_pension"),
    list("NO", "no_pension"),
    list("SE", "se_pension"),
    list("DK", "dk_salary"),
    list("FI", "fi_salary"),
    list("NO", "no_salary"),
    list("SE", "se_salary"),
    list("FI", "securitas")
  )
  
  forward_params <- list(
    list("FI", "firstDayOfQuarter"),
    list("FI", "firstDayOfMonth")
  )
  
  # all_dates <- all_dates %>%
  #   reduce(payday_params, ~ movePayday(.x, ..1, ..2)) %>%
  #   reduce(forward_params, ~ movePaydayForward(.x, ..1, ..2))
  # 
  # all_dates$tempFI <- ifelse(all_dates$FI != "None", 1, 0)
  # all_dates$tempNO <- ifelse(all_dates$NO != "None", 1, 0)
  # all_dates$tempSE <- ifelse(all_dates$SE != "None", 1, 0)
  # all_dates$tempDK <- ifelse(all_dates$DK != "None", 1, 0)
  # 
  # all_dates$Weekend <- as.numeric(all_dates$Weekend) -1
  # all_dates$WorkdayAfterHoliday_FI <- is_next_workday(all_dates$tempFI, all_dates$Weekend)
  # all_dates$WorkdayAfterHoliday_NO <- is_next_workday(all_dates$tempNO, all_dates$Weekend)
  # all_dates$WorkdayAfterHoliday_SE <- is_next_workday(all_dates$tempSE, all_dates$Weekend)
  # all_dates$WorkdayAfterHoliday_DK <- is_next_workday(all_dates$tempDK, all_dates$Weekend)

  
  # Create temporary columns, calculate WorkdayAfterHoliday, and then remove temp columns
  id_list <- setdiff(id_list, "GB")
  temp_col_names <- paste0("temp", id_list)

  all_dates <- all_dates %>%
    mutate(Weekend = as.numeric(Weekend) - 1) %>%
    mutate(across(all_of(id_list),
                  ~ ifelse(. != "None", 1, 0),
                  .names = "temp{.col}")) %>%
    mutate(across(temp_col_names,
                  ~ is_next_workday(., Weekend),
                  .names = "WorkdayAfterHoliday_{.col}")) %>%
    select(-contains("temp"))
  
  print(all_dates)
  
  all_dates <-
    all_dates %>%
    recipe(y ~ ., data = .) %>%
    embed::step_collapse_cart(all_of(id_list), outcome = vars(y) %>%
    prep() %>%
    bake(new_data = NULL) %>%
    mutate(across(c(where(is.numeric), -y, -id), as.factor)))
  
  #logging
  write.xlsx(all_dates, file.path(here("LOG"), paste0("all_dates_", date_str, ".xlsx")))
  
  return(list(all_dates = all_dates, hist_data = hist_data, hist_max = hist_max))
})

forecast_data <- eventReactive(input$run_forecast, {
   
   # DECOMPOSITION ---
   decomp <- data()$hist_data %>%
     select(Date, y) %>%
     group_by(Date) %>%
     summarise(y = sum(y)) %>%
     as_tsibble(index = Date) %>%
     model(stl = STL(y)) %>%
     components() %>%
     autoplot() +
     theme_bw() 
   
   # FORECAST ---
   tic()
   
   run_info <- set_run_info(experiment_name = "forecast",
                            run_name = paste0("forecast_", date_str))
                            
   prep_data(run_info,
             input_data = data()$all_dates,
             combo_vars = c("id"),
             target_var = "y",
             date_type = "day",
             forecast_horizon = as.numeric(input$horizon),
             recipes_to_run = "R1",
             clean_outliers = FALSE,
             hist_end_date = data()$hist_max,
             external_features = external_regressors,
             #box_cox = TRUE
             target_log_transformation = TRUE)
             
   prep_models(run_info,
               back_test_scenarios = NULL,
               back_test_horizon = NULL,
               models_to_run = models,
               run_ensemble_models = TRUE,
               pca = NULL,
               num_hyperparameters = num_hyperparameters)
 
   prepped_models_tbl <- get_prepped_models(run_info)
   
   train_models(run_info,
                run_ensemble_models = FALSE,
                run_local_models = TRUE,
                global_model_recipes = c("R1"),
                #feature_selection = FALSE,
                negative_forecast = FALSE,
                parallel_processing = "local_machine",
                inner_parallel = FALSE,
                num_cores = NULL)
   
   ensemble_models(run_info,
                   parallel_processing = "local_machine",
                   inner_parallel = FALSE,
                   num_cores = NULL)
   
   final_models(run_info,
                average_models = TRUE,
                max_avg_models = 3,
                weekly_to_daily = FALSE,
                parallel_processing = "local_machine",
                inner_parallel = FALSE,
                num_cores = NULL)
   
   models_tbl <- get_trained_models(run_info)
   print(models_tbl)
 
   fcst <- get_forecast_data(run_info)
   run_data <- get_run_info("forecast")
   
   # Logging ---
   write.xlsc(fcst, file.path(here("LOG"), paste0("fcst_", date_str, ".xlsx")))
   write.xlsc(run_data, file.path(here("LOG"), paste0("run_data_", date_str, ".xlsx")))
   (timer <- as.character(toc()[4]))
   
   log_data <- data.frame(date_str = date_str,
                          timer = timer,
                          run_info = run_data,
                          models_tbl = models_tbl,
                          prepped_models_tbl = prepped_models_tbl)
   
   log_file <- file.path(here("LOG"), "logs.xlsx")
   
   if (file.exists(log_file)) {
     existing_data <- read.xlsx(log_file, sheet = 1)
     updated_data <- rbind(existing_data, log_data)
     write.xlsx(updated_data, log_file, sheet = 1)
   } else {
     write.xlsx(log_data, log_file, sheetName = "Sheet1")
   }
   
   # FORECAST PLOT ---
   fplot <- fcst %>%
     ggplot(aes(x = Date)) +
     geom_line(aes(y = Forecast, group = Model_Name, color = Model_Name, alpha = 0.5)) +
     geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "grey", alpha = 0.2) +
     geom_line(data = subset(fcst, Date <= data()$hist_max),
               aes(y = Target, color = "black", linewidth = 0.5)) +
     scale_x_date(limits = c(data()$hist_max - 15, NA)) +
     scale_y_continuous(limits = c(0, max(data()$all_dates$y) * 1.2)) +
     geom_vline(xintercept = data()$hist_max, linetype = "dashed", color = "blue", linewidth = 0.5) +
     labs(x = "Date", y = "Amount", title = "Forecast") +
     geom_line(data = subset(fcst, Best_Model == "Yes") & Date > data()$hist_max,
               aes(y = Forecast, color = "red", linewidth = 1, alpha = 0.5)) +
     theme_bw() +
     theme(legend.position = "none")
   
   # FORECAST TABLE ---
   ftab <- fcst %>%
     arrange(id, desc(Date)) %>%
     filter(Date > data()$hist_max & Best_Model == "Yes") %>%
     muutate_if(is.Date, ~format(., "%Y-%m-%d")) %>%
     select(-c(Date, Forecast, Lo80, Hi80, Model_Name)) %>%
     mutate_if(is.numeric, round, 0)
   
   return(list(fplot = fplot, ftab = ftab, decomp = decomp))
 })

 # OUTPUTS ---
 output$decomp <- renderPlot({
   forecast_data()$decomp
 
 
 output$forecast_plot <- renderPlot({
   forecast_data()$fplot
 })
 
 output$forecast_table <- renderTable({
   forecast_data()$ftab
 })
 
 output$download_forecast <- downloadHandler(
   filename = function() {
     paste0("forecast_", date_str, ".xlsx")
   },
   content = function(file) {
     write.xlsx(forecast_data()$ftab, file)
   }
 )
 
 output$download_diagnostics <- downloadHandler(
   filename = function() {
     paste0("diagnostics_", date_str, ".xlsx")
   },
   content = function(file) {
     write.xlsx(forecast_data()$fcst, file)
   }
 )

})
}

# RUN ---
shinyApp(ui = ui, server = server)

   
   
   
   
   
   
 

































