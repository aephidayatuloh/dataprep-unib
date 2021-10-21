
# Import Data -------------------------------------------------------------

library(readxl)
transaction <- read_xlsx("D:/aephidayatuloh/R/dataprep-unib/data/membership.xlsx", 
                         sheet = "transaction")
transaction

product <- read_xlsx("D:/aephidayatuloh/R/dataprep-unib/data/membership.xlsx", 
                     sheet = "product")
product

subproduct <- read_xlsx("D:/aephidayatuloh/R/dataprep-unib/data/membership.xlsx", 
                        sheet = "subproduct")
subproduct

membership <- read_xlsx("D:/aephidayatuloh/R/dataprep-unib/data/membership.xlsx", 
                        sheet = "membership")
membership


library(tidyverse)
library(lubridate)

membership <- membership %>% 
  mutate(JoinDate = as_date(JoinDate), 
         YoungestDepDOB = as_date(YoungestDepDOB))
membership

trx_tbl <- transaction %>% 
  filter(Qty <= 10) %>% 
  mutate(Monetary = Qty*PricePerUnit) %>% 
  mutate(TransactionDate = as_date(TransactionDatetime),
         TrxMonth = case_when(month(TransactionDate) == 1 ~ "Jan", 
                              month(TransactionDate) == 2 ~ "Feb", 
                              month(TransactionDate) == 3 ~ "Mar", 
                              month(TransactionDate) == 4 ~ "Apr", 
                              month(TransactionDate) == 5 ~ "May", 
                              month(TransactionDate) == 6 ~ "Jun", 
                              month(TransactionDate) == 7 ~ "Jul", 
                              month(TransactionDate) == 8 ~ "Aug", 
                              month(TransactionDate) == 9 ~ "Sep", 
                              month(TransactionDate) == 10 ~ "Oct", 
                              month(TransactionDate) == 11 ~ "Nov", 
                              TRUE ~ "Dec"))

# Tenure ------------------------------------------------------------------

member <- membership %>% 
  mutate(YoungestAge = as.duration(YoungestDepDOB %--% as_date("2019-12-31"))/dyears(1), 
         Tenure = as.duration(JoinDate %--% as_date("2019-12-31"))/dmonths(1))

# Recency -----------------------------------------------------------------

recency <- trx_tbl %>% 
  group_by(FKMemberID) %>% 
  summarise(LastTrx = max(TransactionDatetime)) %>% 
  mutate(LastTrx = as_date(LastTrx), 
         Recency = as.duration(LastTrx %--% as_date("2019-12-31"))/ddays(1))

# Frequency ---------------------------------------------------------------

freq <- trx_tbl %>% 
  distinct(FKMemberID, TransactionDatetime) %>% 
  count(FKMemberID, name = "Frequency")

# Monetary & Consumption --------------------------------------------------

monetary <- trx_tbl %>% 
  left_join(subproduct, by = "SubProductID") %>% 
  group_by(FKMemberID) %>% 
  summarise(Monetary = sum(Qty*PricePerUnit), 
            Consumption = sum(Qty*Weight))


# Product -----------------------------------------------------------------

product_trx <- trx_tbl %>% 
  left_join(product, by = "ProductID") %>% 
  mutate(ProductName = str_remove_all(ProductName, "Product ")) %>% 
  select(FKMemberID, Qty, ProductName) %>% 
  group_by(FKMemberID, ProductName) %>% 
  summarise(TotalQty = sum(Qty)) %>% 
  pivot_wider(id_cols = c(FKMemberID, ProductName), names_from = ProductName, values_from = TotalQty, values_fill = 0)

# Member bertransaksi 6 bulan terakhir ------------------------------------

last6mo_trx <- trx_tbl %>% 
  filter(between(TransactionDate, as_date("2019-07-01"), as_date("2019-12-31"))) %>% 
  distinct(FKMemberID) %>% 
  mutate(Last6Mo = 1)


# Data Akhir --------------------------------------------------------------

final_dt <- member %>% 
  inner_join(recency, by = c("MemberID" = "FKMemberID")) %>% 
  inner_join(freq, by = c("MemberID" = "FKMemberID")) %>% 
  inner_join(monetary, by = c("MemberID" = "FKMemberID")) %>% 
  inner_join(product_trx, by = c("MemberID" = "FKMemberID")) %>% 
  left_join(last6mo_trx, by = c("MemberID" = "FKMemberID")) %>% 
  select(MemberID, Dependant, YoungestAge, Tenure, Recency, Frequency, Consumption, Monetary, Last6Mo, A, B) %>% 
  mutate(Last6Mo = case_when(is.na(Last6Mo) ~ 0, 
                             TRUE ~ 1))

final_dt
