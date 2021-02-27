# check and install packages
packages = c("dplyr","lubridate","readr", "ggplot2","svDialogs")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

remove(packages, package.check)



setwd("~/code/mm3")

# start parameters --------------------------------------------------------

# controlling programm sections
ENV_IMPORT_TRIM <- T
ENV_ADD_UPDATE_MAPTAB <- F
ENV_JOIN_TRIM_MAPTAB <- T

# import and format base --------------------------------------------------
if(ENV_IMPORT_TRIM==T){

# import
# TODO check if file choose is os-specific
dkb_import <- read_delim(file.choose(),
                       ";", escape_double = FALSE,
                       locale = locale(decimal_mark = ",",
                                       encoding = "ISO-8859-1"),
                       skip = 6)

# trim and format
dkb_trim <- dkb_import %>%
  rename(date = 1,
         recipient = 4,
         amount = 8) %>%
  select(1,4,8) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = as.factor(year(date)),
         month = as.factor(month(date)),
         type = ifelse(amount >0,"Income","Spend"),
         day = day(date)) %>%
  arrange(recipient,desc(amount)) %>%
  mutate(year_month = paste0(year,"_",sprintf("%02d",as.numeric(as.character(month)))))

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

if(file.exists("mapping_table.csv")){

  # read csv
  disc_maptab <- read.csv2("maptab.csv", fileEncoding = "ISO-8859-1")

  # entries only in recipients, not in db
  new_rec <- anti_join(maptab,disc_maptab, by = "recipient")

  # add new recipients to existing mapping table on disc
  maptab <- rbind(disc_maptab, new_rec)
}

# create mapping table
write.csv2(recipients,"maptab.csv", row.names = F, fileEncoding = "ISO-8859-1")

remove(disc_maptab, maptab, new_rec, recipients)
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

# plotting ----------------------------------------------------------------

# net per year
y_net <- database %>%
  group_by(year) %>%
  summarise(amount = sum(amount))

ggplot(y_net,aes(year, amount, label=paste0(round(amount,0),"€")))+
  geom_bar(stat="identity")+
  theme_classic()+
  geom_label()

# net per year and month
ym_net <- database %>%
  group_by(year_month) %>%
  summarise(amount = sum(amount))

ggplot(ym_net, aes(year_month, amount, label=paste0(round(amount,0),"€")))+
  geom_bar(stat="identity")+
  theme_classic()+
  geom_label()


# ym label 1
ym_net <- database %>%
  filter(label1 == "Rent") %>%
  group_by(year_month) %>%
  summarise(amount = sum(amount))

ggplot(ym_net, aes(year_month, amount, label=paste0(round(amount,0),"€")))+
  geom_bar(stat="identity")+
  theme_classic()+
  geom_label()


# ordered bar charts ------------------------------------------------------

dkb_report<- function(data, column_to_use,n_amount, plot) {
  # function for creating (ordered) bar charts

  # TODO add helpers for function and use sensible variable names

  # data: database
  # column_to_use
  # n_amount: 1 -> count, 2 -> amount
  # plot: T / F

  if(n_amount==1){
    custom_chart <- data %>%
      select_(column_to_use) %>%
      group_by_(column_to_use) %>%
      tally() %>%
      arrange(-n)

  } else if (n_amount==2){
    custom_chart <- data %>%
      select_(column_to_use, "amount") %>%
      group_by_(column_to_use) %>%
      summarise(amount = sum(amount)) %>%
      arrange(amount)
  }

  if(plot==T){
    # TODO order column_to_use
    if(n_amount==1){
      custom_chart <- ggplot(custom_chart, aes(x=n, label = n))+
        aes_string(y=column_to_use)+
        geom_col()+
        geom_label()+
        theme_classic()+
        labs(x = "n", y = "", title = paste0(column_to_use, " by count"))

  } else if (n_amount==2){
    custom_chart <- ggplot(custom_chart, aes(x =amount, label=paste0(round(amount,0),"€")))+
      aes_string(y=column_to_use)+
      geom_col()+
      theme_classic()+
      geom_label()+
      labs(x ="amount", y ="", title = paste0(column_to_use," by amount"))

  }}


  return(custom_chart)
}

dkb_report(database,"label1",2,T)


# analyze account ---------------------------------------------------------

account_analysis <- function(column_to_use){
  # function to analyze accounts from input column over time

  # TODO use sensible variable names
  # TODO fill in year_month for every category for better comparison
  # TODO add data variable for more customization

ana_choices <- database %>%
  select_(column_to_use) %>%
  distinct() %>%
  unlist()

sel_choice <- dlg_list(choices = ana_choices)$res

choice_analysis <- database %>%
  filter(database[column_to_use] == sel_choice) %>%
  group_by(year_month) %>%
  summarise(amount = sum(amount))

ana_plot <- ggplot(choice_analysis, aes(year_month,amount, label = paste0(round(amount,0),"€")))+
  geom_col(na.rm = F, position = )+
  theme_classic()+
  geom_label(na.rm = F)+
  labs(x = "", y= "", title = paste0(sel_choice," by year and month"))

return(ana_plot)
}

account_analysis("label1")