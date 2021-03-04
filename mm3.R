
# setup -------------------------------------------------------------------

colors <- c("In" = "#8CD47E",
            "Out" = "#FF6961")

# start parameters --------------------------------------------------------

# controlling programm sections
ENV_LOAD_PACKAGES <- T
ENV_IMPORT_TRIM <- T
ENV_ADD_UPDATE_MAPTAB <- F
ENV_JOIN_TRIM_MAPTAB <- T

# load packages -----------------------------------------------------------
if(ENV_LOAD_PACKAGES==T){
  # check and install packages
  packages = c("dplyr","lubridate","readr", "ggplot2",
               "svDialogs","RColorBrewer","gridExtra",
               "scales","tidyr", "ggrepel")

  check_packages <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

  remove(packages, check_packages)
}

# import and format base --------------------------------------------------
if(ENV_IMPORT_TRIM==T){

# get raw data for extracting initial account balance

  # choose file
dkb_file <- file.choose()

# import
dkb_import <- read_delim(dkb_file,
                      ";", escape_double = FALSE,
                      locale = locale(decimal_mark = ",",
                                      encoding = "ISO-8859-1"),
                      skip = 6)

dkb_trim <- dkb_import %>%
  rename(date = 1,
         recipient = 4,
         amount = 8) %>%
  select(1,4,8) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = floor_date(date, unit="year"),
         month = floor_date(date, unit="month"),
         type = ifelse(amount >0,"In","Out"),
         day = day(date)) %>%
  arrange(recipient,desc(amount))

remove(dkb_import)
}

# mapping table -----------------------------------------------------------
if(ENV_ADD_UPDATE_MAPTAB==T) {
maptab <- dkb_trim %>%
  group_by(recipient) %>%
  select(recipient) %>%
  distinct() %>%
  mutate(recipient_clean = NA,
         label1 = NA,
         label2 = NA,
         label3 = NA)

# TODO also create backup!
if(file.exists("maptab.csv")){

  # read csv
  disc_maptab <- read.csv2("maptab.csv", fileEncoding = "ISO-8859-1")

  # entries only in recipients, not in db
  new_rec <- anti_join(maptab,disc_maptab, by = "recipient")

  # add new recipients to existing mapping table on disc
  maptab <- rbind(disc_maptab, new_rec)
}

# create mapping table
write.csv2(maptab,"maptab.csv", row.names = F, fileEncoding = "ISO-8859-1")

remove(disc_maptab, maptab, new_rec)
}

# database extension ------------------------------------------------------
if(ENV_JOIN_TRIM_MAPTAB==T){
if(file.exists("maptab.csv")){

  #read maptab
  maptab <- read.csv2("maptab.csv", fileEncoding = "ISO-8859-1")

  # join with db
  database <- left_join(dkb_trim, maptab, by = "recipient")

  # fill recipient clean with recipient
  database$recipient_clean <- ifelse(is.na(database$recipient_clean),
                                     database$recipient,
                                     database$recipient_clean)

  # TODO grab this later for generating plots with labels
  ENV_MAPTAB_USE <- T
  remove(maptab)

} else {
  ENV_MAPTAB_USE <- F
  }
}

# 1. ordered bar charts ------------------------------------------------------

# function for creating bar charts based on input column

dkb_report<- function(data, column_to_use) {

    custom_chart <- data %>%
      select(.data[[column_to_use]], "amount") %>%
      group_by(.data[[column_to_use]]) %>%
      summarise(amount = round(sum(amount)),0) %>%
      mutate(type = ifelse(amount>0,"In","Out")) %>%
      arrange(amount)

      dkb_bar <- ggplot(custom_chart,
                             aes(x=amount,
                                 y=reorder(.data[[column_to_use]],amount),
                                 fill=amount,
                                 label=dollar(amount, prefix = "",suffix = "€", decimal.mark = ".",
                                              big.mark = )))+
      geom_col()+
      theme_light()+
      geom_label(fill="white",alpha= 0.5)+
      labs(x ="amount", y ="", title = paste0(column_to_use," by amount",
                                              "\n","generated on ",today()))+
      scale_fill_gradient(low = "#FF6961", high = "#8CD47E")+
      scale_x_continuous(labels = dollar_format(prefix ="", suffix ="€"))+
      theme(legend.position = "none",
            panel.grid.minor = element_blank())

  return(dkb_bar)
}

# 2. dkb grid ----------------------------------------------------------------

# function for creating a plot series over time

dkb_grid <- function(data, timeframe, col_to_use) {
  database2 <- data %>%
    group_by(.data[[timeframe]],
             .data[[col_to_use]]) %>%
    summarise(amount = round(sum(amount)),0) %>%
    mutate(type = ifelse(amount>0,"In","Out"))


  grid_plot <- ggplot(database2, aes(.data[[timeframe]],
                        y=amount,
                        fill = type,
                      label = dollar(amount, prefix="",suffix = "€",
                                            decimal.mark = ",", big.mark = "."))
                      )+
    geom_col()+
    theme_light()+
    geom_label(fill="white", size = 3, alpha = 0.5)+
    scale_fill_manual(values = colors)+
    facet_wrap(.data[[col_to_use]] ~ .)+
    scale_y_continuous(labels = dollar_format(prefix ="", suffix ="€"))+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust= 1),
          panel.grid.minor = element_blank())


  switch (timeframe,
          month = grid_plot <- grid_plot+
            scale_x_date(NULL,
                         date_labels = "%b %y",
                         breaks = "month")+
            labs(x="",y="",
                 title = paste0(col_to_use,
                                " by year and month","\n","generated on ",today())),
          year = grid_plot <- grid_plot+
            scale_x_date(NULL,
                         date_labels = "%Y",
                         breaks = "year")+
            labs(x="",y="",
                 title = paste0(col_to_use,
                                " by year","\n","generated on ",today())),
          date = grid_plot <- grid_plot+
            labs(x="",y="",
                 title = paste0(col_to_use,
                                " by date","\n","generated on ",today()))

  )

  return(grid_plot)

}

# 3. create running sum ------------------------------------------------------

# function for creating a running sum plot

dkb_cmlsum <- function(data) {

# read raw file
dkb_raw <- read_delim(dkb_file,
                      ";", escape_double = FALSE,
                      locale = locale(decimal_mark = ",",
                                      encoding = "ISO-8859-1"))

# read last balance
lastbalance <- gsub(dkb_raw[3,2],pattern = "[[:alpha:]]|[[:space:]]|\\.",replacement = "")
lastbalance <- as.numeric(sub(",", ".",lastbalance, fixed = TRUE))

# read last date
lastdate <- dmy(dkb_raw[2,2])

# create df and rename head
new_entry <- data.frame(date = lastdate, amount = lastbalance)

# create 2nd df and sum
db_cumulative <- data %>%
  group_by(date) %>%
  summarise(amount = sum(amount)*-1)

# sort reverse and append with cumulative sum
db_cumulative <- db_cumulative[order(as.Date(db_cumulative$date, "%m/%d/%Y"), decreasing = TRUE),]
db_cumulative <- rbind(new_entry,
                       db_cumulative)

db_cumulative <- cbind(cumsum = round(cumsum(db_cumulative$amount),0),
                       db_cumulative)

# get first and last row from cumulative db
db_cml_label <- db_cumulative %>%
  slice(1,nrow(db_cumulative))

dkb_cml_plot <- ggplot(db_cumulative, aes(date, cumsum))+
  geom_area(fill=colors[1])+
  theme_light()+
  labs(x="",y="", title = paste0("cumulative account balance","\n","generated on ",today()))+
  scale_x_date(breaks = "month", date_labels = "%b %y")+
  scale_y_continuous(label = dollar_format(prefix = "", suffix = "€",
                                           big.mark = ".", decimal.mark = ","))+
  geom_point(color="#7ABD7E", alpha = 0.75, size =2)+
  geom_label_repel(aes(label = dollar(cumsum, prefix = "",suffix = "€", decimal.mark = ",",big.mark = "."),
                      alpha =0.5), data = db_cml_label)+
  expand_limits(y=0)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust= 1),
        panel.grid.minor = element_blank())

return(dkb_cml_plot)
}


# 4. net ---------------------------------------------------------------------

dkb_net <- function(data,timeframe) {
  db_net <- data %>%
    group_by(.data[[timeframe]],type, .groups =T) %>%
    summarise(amount = round(abs(sum(amount))),0)

    net_plot <- ggplot(db_net, aes(.data[[timeframe]],
                       y = amount,
                       label = dollar(amount, prefix="",suffix = "€",
                                      decimal.mark = ",", big.mark = "."),
                       fill=type,
                       group=type))+
    geom_col(position = position_dodge())+
    theme_light()+
      geom_label(fill="white", size = 3, alpha = 0.75)+
      scale_fill_manual(values = colors)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust= 1),
            panel.grid.minor = element_blank())+
      labs(x="",y="",title = paste0("cashflow by ", timeframe, "\n",
                                    "generated on ",today()))+
      scale_y_continuous(label = dollar_format(prefix = "", suffix = "€",
                                               big.mark = ".", decimal.mark = ","))

  switch (timeframe,
    month = net_plot <- net_plot+
      scale_x_date(NULL,
                   date_labels = "%b %y",
                   breaks = "month"),
    year = net_plot <- net_plot+
      scale_x_date(NULL,
                   date_labels = "%Y",
                   breaks = "year"),

  )


  return(net_plot)
}



# creating report ---------------------------------------------------------






suggested_timeframes <- c("1 month", "3 months", "6 months", "12 months")

ENV_TIMEFRAME <- dlg_list(suggested_timeframes)$res

# TODO find best fit for timeframes
# eg 6 months / semi-year -> year month
switch (ENV_TIMEFRAME,
  "1 month" = report_start <- today()%m-% months(1),
  "3 months" = report_start <- today()%m-% months(3),
  "6 months" = report_start <- today()%m-% months(6),
  "12 months" = report_start <- today()%m-% months(12),
  )

myreport <- database %>%
  filter(date >= report_start)

dkb_grid(myreport, "year", "label1")
dkb_grid(myreport, "month", "label1")
dkb_grid(myreport, "date", "label1")

# dodge amount labels
dkb_net(myreport, "month")
dkb_net(myreport, "year")


dkb_cmlsum(myreport)

# sort NA as well
dkb_report(myreport, "recipient_clean")
dkb_report(myreport, "label1")



# net per year
y_net <- database %>%
  group_by(year) %>%
  summarise(amount = sum(amount)) %>%
  mutate(type = ifelse(amount>0,"In","Out"))

ggplot(y_net,aes(year, amount, fill = type,
                 label=dollar(round(amount), prefix = "",suffix = "€", decimal.mark = ",",
                              big.mark = ".")))+
  geom_bar(stat="identity")+
  theme_light()+
  geom_label(fill="white")+
  scale_x_date(NULL, breaks = "year", date_labels = "%Y")+
  scale_y_continuous(labels = dollar_format(prefix ="", suffix ="€"))+
  scale_fill_manual(values = colors)+
  labs(title = paste0("net amount by year","\n","generated on ",today()),
       x ="", y ="")+
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

# net per year and month
ym_net <- database %>%
  group_by(month) %>%
  summarise(amount = round(sum(amount))) %>%
  mutate(type = ifelse(amount>0,"In","Out"))

ggplot(ym_net, aes(month, amount,fill=type,
                   label=dollar(amount, prefix = "",suffix = "€", decimal.mark = ",",
                                big.mark = ".")))+
  geom_bar(stat="identity")+
  theme_light()+
  geom_label(fill="white")+
  scale_x_date(NULL, breaks = "month", date_labels = "%b %y")+
  scale_y_continuous(labels = dollar_format(prefix ="", suffix ="€"))+
  scale_fill_manual(values = colors)+
  labs(title = paste0("net amount by year and month","\n","generated on ",today()),
       x ="", y ="")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust= 1),
        panel.grid.minor = element_blank())