
# disclaimer --------------------------------------------------------------

# This program is specifically designed for western-european CSVs,
# (see encodings). The currency formatting is set to Euro.

# The report generation heavily depends on the DKB export format.


# open goals --------------------------------------------------------------

# 1. create better process for the cumulative report
# 2. shift localization responsibility towards user
# 3. separate programs: data import for DKB, general use plotting / analysis
#      with standard data format
# 4. reduce plotting code redundancy, eg. theme_light()

# global setup ------------------------------------------------------------

colors <- c("In" = "#8CD47E",
            "Out" = "#FF6961")

suggested_timeframes <-
  c("1 month", "3 months", "6 months", "12 months")

# use labels in plots?
ENV_LABELS <- T

# data functions ---------------------------------------------------------------

LOAD_PACKAGES <- function() {

  # check and install packages
  packages = c(
    "dplyr",#         general data manipulation
    "lubridate",#     easy date manipulation
    "readr",#         powering read_delim
    "ggplot2",#       plotting
    "RColorBrewer",#  custom scale colors
    "gridExtra",#     for grid plots
    "scales",#
    "tidyr",#
    "ggrepel"#        adding starting and ending labels on cumulative plot
  )

  check_packages <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
}

GET_FILE <- function() {

  # choose file
  dkb_file <- file.choose()

  return(dkb_file)
}

IMPORT <- function(file, skip_n) {
  # read raw file
  dkb_raw <- read_delim(
    file,
    ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ",",
                    encoding = "ISO-8859-1"),
    skip = skip_n
  )
  return(dkb_raw)
}

FORMAT <- function(file) {
  # format raw data
  dkb_trim <- file %>%
    rename(date = 1,
           recipient = 4,
           amount = 8) %>%
    select(1, 4, 8) %>%
    mutate(date = dmy(date)) %>%
    mutate(
      year = floor_date(date, unit = "year"),
      month = floor_date(date, unit = "month"),
      type = ifelse(amount > 0, "In", "Out"),
      day = day(date)
    ) %>%
    arrange(recipient, desc(amount))

  return(dkb_trim)
}

CREATE_MAPTAB <- function(data) {
  maptab <- data %>%
    group_by(recipient) %>%
    select(recipient) %>%
    distinct() %>%
    mutate(
      recipient_clean = NA,
      label1 = NA,
      label2 = NA,
      label3 = NA
    )

  # TODO create backup as well
  if (file.exists("maptab.csv")) {

    # read csv
    disc_maptab <-
      read.csv2("maptab.csv", fileEncoding = "ISO-8859-1")

    # entries only in recipients, not in db
    new_rec <- anti_join(maptab, disc_maptab, by = "recipient")

    # add new recipients to existing mapping table on disc
    maptab <- rbind(disc_maptab, new_rec)
  }

  return(maptab)
}

JOIN_TRIM_MAPTAB <- function(file, data) {

  # read maptab
  maptab <- read.csv2(file, fileEncoding = "ISO-8859-1")

  # join with db
  database <- left_join(data, maptab, by = "recipient")

  # if recipient clean is empty, fill it with recipient
  database$recipient_clean <-
    ifelse(is.na(database$recipient_clean),
           database$recipient,
           database$recipient_clean)

  return(database)

}

DATA_CUMULATIVE <- function(file) {

  # import dkb raw for getting the last date and last balance,
  #  as they are omitted in the IMPORT function
  dkb_raw <- read.csv2(file, header = F)

  lastbalance <- dkb_raw[4, 2] %>%
    gsub(dkb_raw[3, 2], pattern = "[[:alpha:]]|[[:space:]]|\\.", replacement = "")
  lastbalance <- as.numeric(sub(",", ".", lastbalance, fixed = TRUE))

  # read last date
  lastdate <- dmy(dkb_raw[3, 2])

  # import trimmed file and format f
  dkb_reg <- file %>%
    IMPORT(skip_n = 5) %>%
    FORMAT()

  # create 2nd df and sum
  db_cumulative <- dkb_reg %>%
    group_by(date) %>%
    summarise(amount = sum(amount) * -1)

  # create df and rename head
  new_entry <- data.frame(date = lastdate, amount = lastbalance)

  # sort reverse and append latest balance and date
  db_cumulative <-
    db_cumulative[order(as.Date(db_cumulative$date, "%m/%d/%Y"), decreasing = TRUE),]
  db_cumulative <- rbind(new_entry,
                         db_cumulative)

  # add new column with cumulative sum
  db_cumulative <-
    cbind(cumsum = round(cumsum(db_cumulative$amount), 0),
          db_cumulative)

  return(db_cumulative)
}

# plotting functions ------------------------------------------------------

# function for creating bar charts based on input column
dkb_bar <- function(data, column_to_use) {
  data_orderedbar <- data %>%
    select(.data[[column_to_use]], "amount") %>%
    group_by(.data[[column_to_use]]) %>%
    summarise(amount = round(sum(amount)), 0) %>%
    mutate(type = ifelse(amount > 0, "In", "Out")) %>%
    arrange(amount)

  plot_orderedbar <- ggplot(data_orderedbar,
                            aes(
                              x = amount,
                              y = reorder(.data[[column_to_use]], amount),
                              fill = amount,
                              label = dollar(
                                amount,
                                prefix = "",
                                suffix = "€",
                                decimal.mark = ".",
                                big.mark =
                              )
                            )) +
    geom_col() +
    theme_light() +
    labs(
      x = "amount",
      y = "",
      title = paste0(column_to_use, " by amount",
                     "\n", "generated on ", today())
    ) +
    scale_fill_gradient(low = "#FF6961", high = "#8CD47E") +
    scale_x_continuous(labels = dollar_format(prefix = "", suffix = "€")) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank())


  if (ENV_LABELS == T) {
    plot_orderedbar <- plot_orderedbar +
      geom_label(fill = "white", alpha = 0.5)
  }

  return(plot_orderedbar)
}

# function for creating a plot series over time

dkb_grid <- function(data, timeframe, col_to_use) {
  data_grid <- data %>%
    group_by(.data[[timeframe]],
             .data[[col_to_use]]) %>%
    summarise(amount = round(sum(amount)), 0) %>%
    mutate(type = ifelse(amount > 0, "In", "Out"))


  plot_grid <- ggplot(data_grid, aes(
    .data[[timeframe]],
    y = amount,
    fill = type,
    label = dollar(
      amount,
      prefix = "",
      suffix = "€",
      decimal.mark = ",",
      big.mark = "."
    )
  )) +
    geom_col() +
    theme_light() +
    scale_fill_manual(values = colors) +
    facet_wrap(.data[[col_to_use]] ~ .) +
    scale_y_continuous(labels = dollar_format(prefix = "", suffix = "€")) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )


  switch (
    timeframe,
    month = plot_grid <- plot_grid +
      scale_x_date(NULL,
                   date_labels = "%b %y",
                   breaks = "month") +
      labs(
        x = "",
        y = "",
        title = paste0(col_to_use,
                       " by year and month", "\n", "generated on ", today())
      ),
    year = plot_grid <- plot_grid +
      scale_x_date(NULL,
                   date_labels = "%Y",
                   breaks = "year") +
      labs(
        x = "",
        y = "",
        title = paste0(col_to_use,
                       " by year", "\n", "generated on ", today())
      ),
    date = plot_grid <- plot_grid +
      labs(
        x = "",
        y = "",
        title = paste0(col_to_use,
                       " by date", "\n", "generated on ", today())
      )

  )


  if (ENV_LABELS == T) {
    plot_grid <- plot_grid +
      geom_label(fill = "white",
                 size = 3,
                 alpha = 0.5)
  }

  return(plot_grid)

}

# function for creating a running sum plot

dkb_cmlsum <- function(data) {
  # get first and last row for start and end labels
  db_cml_label <- data %>%
    slice(1, nrow(data))

  plot_cml <- ggplot(data, aes(date, cumsum)) +
    geom_area(fill = colors[1]) +
    theme_light() +
    labs(
      x = "",
      y = "",
      title = paste0("cumulative account balance", "\n", "generated on ", today())
    ) +
    scale_x_date(breaks = "month", date_labels = "%b %y") +
    scale_y_continuous(label = dollar_format(
      prefix = "",
      suffix = "€",
      big.mark = ".",
      decimal.mark = ","
    )) +
    geom_point(color = "#7ABD7E",
               alpha = 0.75,
               size = 2) +
    expand_limits(y = 0) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )

  if (ENV_LABELS == T) {
    plot_cml <- plot_cml +
      geom_label_repel(data = db_cml_label,
                       aes(
                         label = dollar(
                           cumsum,
                           prefix = "",
                           suffix = "€",
                           decimal.mark = ",",
                           big.mark = "."
                         ),
                         alpha = 0.5
                       ))
  }

  return(plot_cml)
}


dkb_net <- function(data, timeframe) {
  data_net <- data %>%
    group_by(.data[[timeframe]], type, .groups = T) %>%
    summarise(amount = round(abs(sum(amount))), 0)

  plot_net <- ggplot(data_net,
                     aes(
                       .data[[timeframe]],
                       y = amount,
                       label = dollar(
                         amount,
                         prefix = "",
                         suffix = "€",
                         decimal.mark = ",",
                         big.mark = "."
                       ),
                       fill = type,
                       group = type
                     )) +
    geom_col(position = position_dodge()) +
    theme_light() +
    scale_fill_manual(values = colors) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = paste0("cashflow by ", timeframe, "\n",
                     "generated on ", today())
    ) +
    scale_y_continuous(label = dollar_format(
      prefix = "",
      suffix = "€",
      big.mark = ".",
      decimal.mark = ","
    ))

  switch (
    timeframe,
    month = plot_net <- plot_net +
      scale_x_date(NULL,
                   date_labels = "%b %y",
                   breaks = "month"),
    year = plot_net <- plot_net +
      scale_x_date(NULL,
                   date_labels = "%Y",
                   breaks = "year")

  )


  if (ENV_LABELS == T) {
    plot_net <- plot_net +
      geom_label(fill = "white",
                 size = 3,
                 alpha = 0.75)
  }

  return(plot_net)
}
