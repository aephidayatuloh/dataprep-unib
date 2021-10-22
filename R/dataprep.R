
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

# Filter ------------------------------------------------------------------

trx_tbl <- filter(transaction, Qty <= 10)

filter(product, str_detect(ProductCategory, "A"))

filter(membership, MemberID == 433907)

filter(trx_tbl, FKMemberID == 433907)

filter(trx_tbl, FKMemberID == 433907 & as_date(TransactionDatetime) > as_date("2019-10-01"))

filter(filter(trx_tbl, FKMemberID == 433907), as_date(TransactionDatetime) > as_date("2019-10-01"))


# Pipe Operator -----------------------------------------------------------

filter(trx_tbl, FKMemberID == 433907) %>% 
  filter(as_date(TransactionDatetime) > as_date("2019-10-01"))

trx_tbl %>% 
  filter(FKMemberID == 433907) %>% 
  filter(as_date(TransactionDatetime) > as_date("2019-10-01"))


# Slice -------------------------------------------------------------------

trx_tbl %>% 
  slice(1:100)

trx_tbl %>% 
  slice_max(TransactionDatetime)


# Arrange -----------------------------------------------------------------

trx_tbl %>% 
  filter(month(TransactionDatetime) == 1) %>% 
  arrange(TransactionDatetime)

trx_tbl %>% 
  filter(month(TransactionDatetime) == 1) %>% 
  arrange(desc(TransactionDatetime))

# Distinct ----------------------------------------------------------------

trx_tbl %>% 
  distinct(FKMemberID)

trx_tbl %>% 
  distinct(FKMemberID, .keep_all = TRUE)

# Select ------------------------------------------------------------------

trx_tbl %>% 
  select(FKMemberID, Qty, PricePerUnit, TransactionDate, TrxMonth)

trx_tbl %>% 
  select(-c(ProductID, SubProductID, TransactionDatetime))

# Mutate ------------------------------------------------------------------

membership <- membership %>% 
  mutate(JoinDate = as_date(JoinDate), 
         YoungestDepDOB = as_date(YoungestDepDOB))
membership

trx_tbl <- trx_tbl %>% 
  mutate(Monetary = Qty*PricePerUnit)

trx_tbl <- trx_tbl %>% 
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
                              month(TransactionDate) == 12 ~ "Dec", 
                              TRUE ~ "Unknown"))

trx_tbl <- trx_tbl %>% 
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

# Transmute ---------------------------------------------------------------

membership %>% 
  transmute(MemberID, 
            Tenure = as.duration(JoinDate %--% as_date("2019-12-31"))/dmonths(1))


# Rename ------------------------------------------------------------------

membership %>% 
  rename(ID = MemberID)


# Count -------------------------------------------------------------------

trx_tbl %>% 
  count()

trx_tbl %>% 
  count(FKMemberID)

trx_tbl %>% 
  count(FKMemberID, sort = TRUE)

trx_tbl %>% 
  count(FKMemberID, sort = TRUE, name = "TrxFreq")

trx_tbl %>% 
  count(ProductID) %>% 
  mutate(Pct = n/sum(n))
  

# Summarise ---------------------------------------------------------------

trx_tbl %>% 
  summarise(TotalMonetary = sum(Monetary))


# Grouping ----------------------------------------------------------------

trx_tbl %>% 
  group_by(FKMemberID) %>% 
  summarise(TotalMonetary = sum(Monetary), 
            TrxFreq = n(), 
            LastTrx = max(TransactionDatetime))


# Inner Join --------------------------------------------------------------

trx_tbl %>% 
  inner_join(subproduct, by = "SubProductID")

band_members
band_instruments

band_members %>% 
  inner_join(band_instruments, by = "name")

# Full (Outer) Join -------------------------------------------------------

band_members %>% 
  full_join(band_instruments, by = "name")

# Left Join ---------------------------------------------------------------

band_members %>% 
  left_join(band_instruments, by = "name")

# Right Join --------------------------------------------------------------

band_members %>% 
  right_join(band_instruments, by = "name")

# Anti Join ---------------------------------------------------------------

band_members %>% 
  anti_join(band_instruments, by = "name")
