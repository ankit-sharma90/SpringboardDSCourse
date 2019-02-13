#load data
df <- read.csv('/Volumes/GoogleDrive/My Drive/TouchBistro/Other/Springboard Data Science Course/Exercise Files/titanic_original.csv')

### STEP 1 - replace missing values in Embarked column ###
  df %>% count(embarked)
  df$embarked <- df$embarked %>% na_if("")
  df$embarked <- df$embarked %>% replace_na("S")
  df %>% count(embarked)

### STEP 2 - replace missing values in Age column ###
  df %>% count(age,sort=TRUE)
  meanage <- df$age %>% mean(na.rm=TRUE)
  df$age <- df$age %>% replace_na(meanage)
  df %>% count(age,sort=TRUE)
  
### STEP 3 - replace missing values in boat column ###
  df %>% count(boat,sort=TRUE)
  df$boat <- df$boat %>% na_if("")
  df %>% count(boat,sort=TRUE)
  
### STEP 4 - create dummy variable column "has_cabin_number" ###
  df %>% count(cabin,sort=TRUE)
  df <- df %>% mutate (
            has_cabin_number = 
              case_when (
                df$cabin == "" ~ 0,
                df$cabin != "" ~ 1)
          )
  df %>% count(has_cabin_number,sort=TRUE)
  
### Save output to CSV ###  
  write.csv(df,file = '/Volumes/GoogleDrive/My Drive/TouchBistro/Other/Springboard Data Science Course/Exercise Files/titanic_clean.csv')
  