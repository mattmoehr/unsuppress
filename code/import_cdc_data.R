## Matt Moehr
## 2018-06-01

## Import csv data downloaded from CDC Wonder.

# install.packages("tidyverse")
library(tidyverse)


state <- read_tsv(file = "data/mortality_state_all_ages.txt",
                  col_names = TRUE
                  )

## the cdc "helpfully" puts some notes and metadata at the end of each downloaded
## file. this subset will keep just the real data.
state <- subset(state, is.na(Notes))

## we also want to throw out the "NS" (not specified) age group.
state <- subset(state, `Age Group Code` != 'NS')

state <- subset(state,
                select = c(State,
                           `State Code`,
                           Year,
                           `Age Group Code`,
                           Deaths,
                           Population,
                           `Crude Rate`
                           )
                 )

## we need a var to hold the age group as an actual number not a text string

## Return the string_val up till the first occurance of split_char.
## The do.call stuff makes sure you get back an array.
## hackish function to split the string and then get the first bit
struntil <- function(string_val, split_char) {
  do.call("rbind", strsplit(string_val, split_char, fixed = TRUE))[,1]
}
# test the function
# struntil("22-test", "-")
# foo <- struntil(state$`Age Group Code`, "-")
# as.integer(struntil(state$`Age Group Code`, '-'))


## IN THEORY i can use mutate and case_when to recode the string variable
## into the integer variable age_group. 
state$age_group <- NaN
state$age_group[state$`Age Group Code` == '1'] <- 0
state$age_group[state$`Age Group Code` == '85+'] <- 85
state$age_first_step <- as.integer(struntil(state$`Age Group Code`, '-'))
state$age_group[is.na(state$age_group)] <- state$age_first_step[is.na(state$age_group)]
state$age_group <- as.integer(state$age_group)

## this will give us the age marginals but we will want to regroup them 
## i think?
age_raw <- state %>%
             filter(State == 'Minnesota' &
                    Year == 2016 
                    ) %>%
             mutate(deaths_num = as.integer(Deaths),
                    population_num = as.integer(Population),
                    crude_rate = as.numeric(`Crude Rate`)
                    )

## just check that this looks ok
age_raw %>% 
  select(age_group, deaths_num)

## hopefully no NAs in the data
age_raw %>%
  filter(is.na(deaths_num))

###############################################################################

county <- read_tsv(file = "data/mortality_all_counties.txt",
                   col_names = TRUE
                   )

county <- subset(county, is.na(Notes))
county <- subset(county,
                 select = c(County,
                            `County Code`,
                            Year,
                            Deaths,
                            Population,
                            `Crude Rate`
                            )
                 )

county_raw <- county %>%
                filter(substr(`County Code`, 1, 2) == '27' &
                       Year == 2016
                       ) %>%
                mutate(deaths_num = as.integer(Deaths),
                       population_num = as.integer(Population),
                       crude_rate = as.numeric(`Crude Rate`)
                       )
      
## just check that this looks ok
county_raw %>% 
  select(County, deaths_num)

## hopefully no NAs in the data
county_raw %>%
  filter(is.na(deaths_num))

###############################################################################

county_age <- read_tsv(file = "data/mortality_mn_county_all_ages.txt",
                       col_names = TRUE
                       )

county_age <- subset(county_age, is.na(Notes))
county_age <- subset(county_age, `Age Group Code` != 'NS')
county_age <- subset(county_age,
                     select = c(County,
                                `County Code`,
                                Year,
                                `Age Group Code`,
                                Deaths,
                                Population,
                                `Crude Rate`
                                )
                     )

county_age$age_group <- NaN
county_age$age_group[county_age$`Age Group Code` == '1'] <- 0
county_age$age_group[county_age$`Age Group Code` == '85+'] <- 85
county_age$age_first_step <- as.integer(struntil(county_age$`Age Group Code`, '-'))
county_age$age_group[is.na(county_age$age_group)] <- county_age$age_first_step[is.na(county_age$age_group)]
county_age$age_group <- as.integer(county_age$age_group)


county_age_raw <- county_age %>%
                    filter(Year == 2016) %>%
                    mutate(deaths_num = as.integer(Deaths),
                           population_num = as.integer(Population),
                           crude_rate = as.numeric(`Crude Rate`)
                           )

## just check that this looks ok
county_age_raw %>% 
  select(County, age_group, deaths_num) %>%
  spread(age_group , deaths_num)
