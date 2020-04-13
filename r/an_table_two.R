library(data.table)

part <- readRDS('data/clean_participants.rds')
part$weekday <- weekdays(part$date)

age_lab <- "Age of participant (years)"
gender_lab <- "Gender of participant"
hh_size_lab <- "Household size"
dow_lab <- "Day of week"


part[, hh_size := fcase(
  hh_size == 1, "1",
  hh_size == 2, "2",
  hh_size == 3, "3",
  hh_size == 4, "4",
  hh_size > 4, "4+"
)]

## Participant chracteristics
t_total <- data.table(Category = "Overall", Variables = "Overall", N_participants = nrow(part), order = 1)
t_ages <-  part[order(part_age_group) ,  .(N_participants = .N), by = part_age_group]
t_gender <-  part[order(part_gender) ,  .(N_participants = .N), by = part_gender]
t_hh_size <-  part[order(hh_size) ,  .(N_participants = .N), by = hh_size]
t_weekday <-  part[order(weekday) ,  .(N_participants = .N), by = weekday]


t_counts <- rbind(
  t_total, 
  t_ages[, .(Category = age_lab,
             Variables = part_age_group,
             N_participants,
             order = 2)
         ],
  t_gender[, .(Category = gender_lab,
               Variables = part_gender,
               N_participants,
               order = 3)
           ],
  # Probably regroup to 6+ and missing
  t_hh_size[, .(Category = hh_size_lab,
                Variables = hh_size,
                N_participants,
                order = 4)
            ],
  t_weekday[, .(Category = dow_lab,
                Variables = weekday,
                N_participants, 
                order = 5)
            ]
)


## Number of contacts per group.

part[, mean_contacts := n_contacts]

## Calculate the mean and standard devation by the groupings



mean_contacts <- function(df = part, 
                          category = "Overall",
                          var = "mean_contacts", by = NULL, filter = FALSE){
    x <- df[, .(
      Mean_contacts = mean(get(var)),
      SD_contacts = sd(get(var)),
      Median_contacts = median(get(var)),
      l_iqr = quantile(get(var), p = 0.25),
      u_iqr = quantile(get(var), p = 0.75)
    )
  ]
  x <- x[, .(Category = category,
                Variables = "Overall",
                Mean_contacts,
                SD_contacts,
                Median_contacts,
                l_iqr,
                u_iqr
  )
  ]
  if(!is.null(by)){
    x <- df[order(get(by)), .(
      Mean_contacts = mean(get(var)),
      SD_contacts = sd(get(var)),
      Median_contacts = median(get(var)),
      l_iqr = quantile(get(var), p = 0.25),
      u_iqr = quantile(get(var), p = 0.75)
    ),
    by = get(by)
  ]
  
    x <- x[, .(Category = category,
               Variables = get,
               Mean_contacts,
               SD_contacts,
               Median_contacts,
               l_iqr,
               u_iqr
    )
    ]  
    
  }
  x
  
}


t_overall <- mean_contacts(part, category = "Overall")
t_ages_contacts <- mean_contacts(part, by = "part_age_group", category = age_lab)
t_gender_contacts <-  mean_contacts(part, by = "part_gender", category = gender_lab)
t_hh_size_contacts <-  mean_contacts(part, by = "hh_size", category = hh_size_lab)
t_weekday_contacts <-  mean_contacts(part, by = "weekday", category = dow_lab)


# Combine the groups

t_contacts <- rbind(
  t_overall,
  t_ages_contacts,
  t_gender_contacts,
  t_hh_size_contacts,
  t_weekday_contacts
  
)

t_contacts
polymod <- readRDS('data/polymod_participants.rds')
polymod_part_contacts <- readRDS('data/polymod_contacts_part.rds')

polymod_mean_contacts <- polymod_part_contacts[,  .(mean_contacts = mean(.N)), by = part_id]

polymod <- merge(polymod, polymod_mean_contacts, by = "part_id", all.x = TRUE)


polymod[, hh_size := fcase(
  hh_size == 1, "1",
  hh_size == 2, "2",
  hh_size == 3, "3",
  hh_size == 4, "4",
  hh_size > 4, "6+"
)]

polymod <- polymod[part_age_group != "[0,18)"]
polymod <- polymod[!is.na(mean_contacts)]
polymod

tp_overall <- mean_contacts(polymod, category = "Overall")
tp_ages_contacts <- mean_contacts(polymod, by = "part_age_group", category = age_lab)
tp_gender_contacts <-  mean_contacts(polymod, by = "part_gender", category = gender_lab)
tp_hh_size_contacts <-  mean_contacts(polymod, by = "hh_size", category = hh_size_lab)
#tp_weekday_contacts <-  mean_contacts(polymod, by = "weekday", category = dow_lab)

tp_gender_contacts[ ,Variables := fifelse(Variables == "F", "Female", "Male")]

tp_contacts <- rbind(
  tp_overall,
  tp_ages_contacts,
  tp_gender_contacts,
  tp_hh_size_contacts
  
)

tp_contacts
## Contacts compared to reference point.

tp_contacts[, SD_lab := fifelse(is.na(SD_contacts), "-", as.character(round(SD_contacts, 1)))]
tp_contacts[, pm_Contacts := paste0(round(Mean_contacts,1)," (", SD_lab, ")" )]
tp_contacts[, pm_Contacts_med := paste0(round(Mean_contacts,1)," (", l_iqr, ", ", u_iqr, ")")]

tp_contacts <- tp_contacts[, .(Category,
                Variables,
                pm_Contacts,
                pm_Contacts_med)]


tp_contacts
t_contacts


table_two <- merge(t_counts, t_contacts, by = c("Category", "Variables"), all.x = TRUE)
table_two <- merge(table_two, tp_contacts, by = c("Category", "Variables"), all.x = TRUE)

table_two[, SD_lab := fifelse(is.na(SD_contacts), "-", as.character(round(SD_contacts, 1)))]
table_two[, Contacts := paste0(round(Mean_contacts,1)," (", SD_lab, ")" )]
table_two[, Contacts_med := paste0(round(Mean_contacts,1)," (", l_iqr, ", ", u_iqr, ")")]



table_two[, Value := fcase(
  Variables == "[18,20)", "18-19", 
  Variables == "[20,30)", "20-29", 
  Variables == "[30,40)", "30-39",
  Variables == "[40,50)", "40-49",
  Variables == "[50,60)", "50-59",
  Variables == "[60,70)", "60-69",
  Variables == "[70,120)", "70+"
)
]


table_two[, Value := fifelse(
  is.na(Value),
  as.character(Variables),
  Value)
  ]

table_two <- table_two[order(order), .(
  Category,
  Value,
  N_participants,
  Contacts,
  Contacts_med,
  pm_Contacts,
  pm_Contacts_med
)]
setcolorder(table_two, c(
  "Category",
  "Value",
  "N_participants",
  "Contacts",
  "Contacts_med"
)
)

table_two

## Write to csv for output into paper
write.csv(table_two, file = "outputs/table_two.csv", row.names = FALSE)














