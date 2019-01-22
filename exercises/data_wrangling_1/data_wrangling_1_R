
#load data
df <- read.csv('/Volumes/GoogleDrive/My Drive/TouchBistro/Other/Springboard Data Science Course/Exercise Files/refine_original.csv')

### STEP 1 - clean brand names ###

  df$company <- gsub(".*l.*ps$","phillips",df$company,ignore.case = TRUE)
  df$company <- gsub("^ak.*z*.","akzo",df$company,ignore.case = TRUE)
  df$company <- gsub("^van.*houten","van houten",df$company,ignore.case = TRUE)
  df$company <- gsub("^unil.*er$","unilever",df$company,ignore.case = TRUE)

### STEP 2 - seperate product code & number ###
  df2 <-separate(df,Product.code...number,c("product_code","product_number"),sep="-")
  
### STEP 3 - create product category column ###
  df2 <- df2 %>% 
    mutate(product_category = 
             case_when (
                product_code == "p" ~ "Smartphone",
                product_code == "v" ~ "TV",
                product_code == "x" ~ "Laptop",
                product_code == "q" ~ "Tablet"
          ))

### STEP 4 - create full address column ###
  df2 <- df2 %>%  mutate(full_address = paste(address,city,country,sep="-"))

### STEP 5 - create dummy variables ###

  ### company dummy variables ###
  df2 <- df2 %>% mutate(company_phillips = ifelse(company == "phillips",1,0))
  df2 <- df2 %>% mutate(company_akzo = ifelse(company == "akzo",1,0))
  df2 <- df2 %>% mutate(company_van_houten = ifelse(company == "van houten",1,0))
  df2 <- df2 %>% mutate(company_unilever = ifelse(company == "unilever",1,0))
  
  ### product dummy variables ###  
  df2 <- df2 %>% mutate(product_smartphone = ifelse(product_category == "Smartphone",1,0))
  df2 <- df2 %>% mutate(product_tv = ifelse(product_category == "TV",1,0))
  df2 <- df2 %>% mutate(product_laptop = ifelse(product_category == "Laptop",1,0))
  df2 <- df2 %>% mutate(product_tablet = ifelse(product_category == "Tablet",1,0))
  
### Save output to CSV ###  
  write.csv(df2,file = "/users/asharma/Desktop/data_wrangling_1.csv")
  
