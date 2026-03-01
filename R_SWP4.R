
library(dplyr)     # data manipulation
library(tidyr)     # tidying data
library(stringr)   # string helpers
library(AER)       # ivreg for IV estimation
library(sandwich)  # robust vcov
library(lmtest)    # coeftest

dat<-data_clean



# Select 2L bottles (VOL_EQ = 0.3521)
data.select1 <- subset(dat, VOL_EQ == 0.3521)

# Select 12-can packs (VOL_EQ = 0.75 and PACKAGE == "CAN")
data.select2 <- subset(dat, VOL_EQ == 0.75 & PACKAGE == "CAN")

# Combine both product types
data.select <- rbind(data.select1, data.select2)

# Restrict to the 4 main cola brands
brands <- c("COKE CLASSIC", "DIET COKE", "PEPSI", "DIET PEPSI")
data.select <- subset(data.select, L5 %in% brands)

# Sort data for reproducibility (by market, store, week, brand, package)
data.select <- data.select[order(data.select$MARKET,
                                 data.select$IRI_KEY,
                                 data.select$WEEK,
                                 data.select$L5,
                                 data.select$PACKAGE), ]

# Rename for convenience
dat <- data.select
dim(dat)

# ------------------------------------------------------------------------------
# STEP 2: MARKET SHARES AND OUTSIDE-GOOD SHARE
# ------------------------------------------------------------------------------
# Compute product share as in SWP3:
# s_j = (liter_j / total_liter_in_store_week) * (within_trips / total_trips)
dat$share <- (dat$liter / dat$total.liter) * (dat$within.trips / dat$total.trips)

# Compute outside-good share per store-week: s_0 = 1 - sum_j s_j
# Aggregate inside goods' shares by store-week
tmp <- aggregate(share ~ IRI_KEY + WEEK, data = dat, FUN = sum, na.rm = TRUE)

# Outside share = 1 - inside share (guard against negatives)
tmp$share_og <- pmax(0, 1 - tmp$share)

# Keep only store-week and outside share
tmp <- subset(tmp, select = c(IRI_KEY, WEEK, share_og))

# Merge back to main data
dat <- merge(dat, tmp, by = c("IRI_KEY", "WEEK"), all.x = TRUE)

# Guard logs against zeros to avoid -Inf
dat$ln_share    <- log(pmax(dat$share,    1e-10))
dat$ln_share_og <- log(pmax(dat$share_og, 1e-10))

# Check summary statistics
summary(dat$share)
summary(dat$share_og)

# ------------------------------------------------------------------------------
# STEP 3: NESTING STRUCTURE - DIET vs REGULAR (main model)
# ------------------------------------------------------------------------------
# Define nest membership based on whether product is diet or regular
dat$DIET <- ifelse(dat$L5 %in% c("DIET COKE", "DIET PEPSI"), "DIET", "REGULAR")

# Compute total liters per store-week-nest (for within-group shares s_{j|g})
nest_totals <- aggregate(liter ~ IRI_KEY + WEEK + DIET, 
                         data = dat, FUN = sum, na.rm = TRUE)

# Rename to avoid confusion
names(nest_totals)[names(nest_totals) == "liter"] <- "total_liter_nest"

# Merge nest totals back to main data
dat <- merge(dat, nest_totals, by = c("IRI_KEY", "WEEK", "DIET"), all.x = TRUE)
#names(dat)
# Within-group share s_{j|g} = liter_j / total_liter_in_nest
dat$share_g <- dat$liter / dat$total_liter_nest

# Log of within-group share (will be endogenous regressor in nested logit)
dat$ln_share_g <- log(pmax(dat$share_g, 1e-10))

# Group share S_g = sum of product shares in the nest (for elasticity formulas later)
group_share <- aggregate(share ~ IRI_KEY + WEEK + DIET, 
                         data = dat, FUN = sum, na.rm = TRUE)
names(group_share)[names(group_share) == "share"] <- "Sg"

# Merge group shares back
dat <- merge(dat, group_share, by = c("IRI_KEY", "WEEK", "DIET"), all.x = TRUE)

# Check summary statistics
summary(dat$share_g)
summary(dat$ln_share_g)
summary(dat$Sg)

table(dat$DIET, dat$L5) 
# ------------------------------------------------------------------------------
# STEP 4: INSTRUMENTS
# ------------------------------------------------------------------------------

# -------------------------
# 4.1 HAUSMAN-STYLE PRICE INSTRUMENTS
# -------------------------

# Price in OTHER MARKET (same week, brand, size, package)
# Compute mean price by MARKET x WEEK x L5 x VOL_EQ x PACKAGE
tmp <- aggregate(price.per.liter ~ MARKET + WEEK + L5 + VOL_EQ + PACKAGE,
                 data = dat, FUN = mean, na.rm = TRUE)

# Rename the price variable
names(tmp)[names(tmp) == "price.per.liter"] <- "price_other_market"

# Switch markets: EAU CLAIRE <-> PITTSFIELD
tmp$MARKET <- ifelse(tmp$MARKET == "EAU CLAIRE", "PITTSFIELD", "EAU CLAIRE")

# Merge back: each product gets the price from the OTHER market
dat <- merge(dat, tmp, by = c("MARKET", "WEEK", "L5", "VOL_EQ", "PACKAGE"), all.x = TRUE)

# Collapse to store-level average price (for leave-one-store-out computations)
store_week_brand <- aggregate(price.per.liter ~ MARKET + WEEK + L5 + VOL_EQ + PACKAGE + IRI_KEY,
                              data = dat, FUN = mean, na.rm = TRUE)
names(store_week_brand)[names(store_week_brand) == "price.per.liter"] <- "store_price"

# Leave-one-store-out mean and min prices within MARKET x WEEK x L5 x VOL_EQ x PACKAGE
# For each store, compute mean and min of OTHER stores' prices in same market-week-brand
lo_store <- store_week_brand %>%
  group_by(MARKET, WEEK, L5, VOL_EQ, PACKAGE) %>%
  mutate(
    # Average price in other stores (excluding current store)
    avg_price_other_stores_brand =
      if (n() > 1) (sum(store_price, na.rm = TRUE) - store_price) / (n() - 1) else NA_real_,
    
    # Minimum price in other stores (excluding current store)
    min_price_other_stores_brand =
      if (n() > 1) sapply(seq_len(n()), function(i) min(store_price[-i], na.rm = TRUE)) else NA_real_
  ) %>%
  ungroup()

# Merge leave-one-store-out instruments back to product-level data
dat <- dat %>%
  left_join(lo_store %>% select(MARKET, WEEK, L5, VOL_EQ, PACKAGE, IRI_KEY,
                                avg_price_other_stores_brand, min_price_other_stores_brand),
            by = c("MARKET", "WEEK", "L5", "VOL_EQ", "PACKAGE", "IRI_KEY"))

# -------------------------
# 4.2 COST SHIFTERS (sugar, aluminum, gasoline)
# -------------------------

# Read cost shifter data
data.sugar    <- read.csv("week_sugar.csv")        # WEEK, sugar.price
data.aluminum <- read.csv("alumnium_price.csv")    # WEEK, alumnium.price
gasoline      <- read.csv("gasoline.csv")          # date, gas.high, gas.deep

# Convert gasoline data from daily to weekly averages
gasoline$Date <- as.Date(gasoline$date, format = "%m/%d/%Y")
gasoline$W    <- as.Date(cut(gasoline$Date, "week"))

# Aggregate to weekly means
gasoline <- aggregate(cbind(gas.deep, gas.high) ~ W, 
                      data = gasoline, FUN = mean, na.rm = TRUE)

# Create WEEK identifier matching the dat dataset
W <- unique(gasoline$W)
W <- data.frame(W)
W$WEEK <- min(dat$WEEK, na.rm = TRUE) + seq_len(nrow(W)) - 1

# Merge to get WEEK variable in gasoline
gasoline <- merge(gasoline, W, by = "W")
gasoline <- gasoline[, -1]  # drop the W (date) column

# Merge all cost shifters into main data by WEEK
dat <- merge(dat, data.sugar,    by = "WEEK", all.x = TRUE)
dat <- merge(dat, data.aluminum, by = "WEEK", all.x = TRUE)
dat <- merge(dat, gasoline,      by = "WEEK", all.x = TRUE)

# Create interaction terms to strengthen exclusion restrictions
# Aluminum cost primarily affects cans
dat$aluminum_x_can <- dat$alumnium.price * ifelse(dat$PACKAGE == "CAN", 1, 0)

# Sugar cost primarily affects regular (non-diet) products
dat$sugar_x_regular <- dat$sugar.price * ifelse(dat$DIET == "REGULAR", 1, 0)

# -------------------------
# 4.3 BLP-STYLE INSTRUMENTS for ln(within-group share)
# -------------------------

# Build sums of characteristics of OTHER products in the SAME NEST
# (market-store-week-nest level)
blp <- dat %>%
  group_by(MARKET, IRI_KEY, WEEK, DIET) %>%
  summarise(
    num_products_nest     = n(),                                      # number of products in nest
    sum_price_others_nest = sum(price.per.liter, na.rm = TRUE),      # sum of all prices in nest
    sum_display_others    = sum(display.major + display.minor, na.rm = TRUE),
    sum_feature_others    = sum(feature.large + feature.medium, na.rm = TRUE),
    sum_coupon_others     = sum(coupon, na.rm = TRUE),
    .groups = "drop"
  )

# Merge back to product-level data
dat <- dat %>%
  left_join(blp, by = c("MARKET", "IRI_KEY", "WEEK", "DIET")) %>%
  mutate(
    # Exclude own product from the sums
    sum_price_others_nest = sum_price_others_nest - price.per.liter,
    
    # Average price of other products in nest
    avg_price_others_nest = ifelse(num_products_nest > 1,
                                   sum_price_others_nest / (num_products_nest - 1), 
                                   NA_real_),
    
    # Sum of display of other products in nest
    sum_display_others = sum_display_others - (display.major + display.minor),
    
    # Sum of feature of other products in nest
    sum_feature_others = sum_feature_others - (feature.large + feature.medium),
    
    # Sum of coupons of other products in nest
    sum_coupon_others = sum_coupon_others - coupon
  )

# ------------------------------------------------------------------------------
# STEP 5: PROMOTIONS AGGREGATION
# ------------------------------------------------------------------------------
# Aggregate display variables: major counts more than minor (weight = 0.5)
dat$display <- dat$display.major + 0.5 * dat$display.minor

# Aggregate feature variables: large counts more than medium (weight = 0.5)
dat$feature <- dat$feature.large + 0.5 * dat$feature.medium

# ------------------------------------------------------------------------------
# STEP 6: ESTIMATION - OLS vs IV, NON-NESTED vs NESTED
# ------------------------------------------------------------------------------
unique(dat$IRI_KEY)
dat$store01<-ifelse(dat$IRI_KEY=="213290",1,0)
dat$store02<-ifelse(dat$IRI_KEY=="228037",1,0)
dat$store03<-ifelse(dat$IRI_KEY=="233779",1,0)
dat$store04<-ifelse(dat$IRI_KEY=="234140",1,0)
dat$store05<-ifelse(dat$IRI_KEY=="248128",1,0)
dat$store06<-ifelse(dat$IRI_KEY=="257871",1,0)
dat$store07<-ifelse(dat$IRI_KEY=="259111",1,0)
dat$store08<-ifelse(dat$IRI_KEY=="264075",1,0)
dat$store09<-ifelse(dat$IRI_KEY=="266596",1,0)
dat$store10<-ifelse(dat$IRI_KEY=="648764",1,0)
dat$store11<-ifelse(dat$IRI_KEY=="652159",1,0)
dat$store12<-ifelse(dat$IRI_KEY=="653776",1,0)

# Dependent variable for aggregate logit: y = ln(s_j) - ln(s_0)
y <- dat$ln_share - dat$ln_share_og


# -------------------------
# 6.1 OLS AGGREGATE LOGIT (non-nested, for comparison with SWP3)
# -------------------------
ols_agg <- lm(y ~ -1 + PACKAGE:L5              # brand x format fixed effects (no intercept)
              + store01 + store02 + store03 + store04 + store05 + store06 
              + store07 + store08 + store09 + store10 + store11 + store12  # store fixed effects
              + price.per.liter                 # price coefficient (alpha)
              + display + feature + coupon,     # promotion effects
              data = dat)

print("=== OLS Aggregate Logit (non-nested) ===")
summary(ols_agg)
# -------------------------
# 6.2 OLS NESTED LOGIT (biased because ln_share_g is endogenous)
# -------------------------
ols_nl <- lm(y ~ ln_share_g                    # within-group share (sigma) - ENDOGENOUS
             - 1 + PACKAGE:L5                   # brand x format fixed effects
             + store01 + store02 + store03 + store04 + store05 + store06 
             + store07 + store08 + store09 + store10 + store11 + store12  # store fixed effects
             + price.per.liter                  # price coefficient (alpha) - ENDOGENOUS
             + display + feature + coupon,      # promotion effects
             data = dat)

print("=== OLS Nested Logit (biased - no IV) ===")
summary(ols_nl)

# From: ols_agg (line ~245)
ols_agg$coefficients["price.per.liter"]      # Price coef (biased)
summary(ols_agg)$r.squared                   # R-squared

# From: ols_nl (line ~260)
ols_nl$coefficients["ln_share_g"]            # Sigma (biased, ~0.3-0.5 typically)
ols_nl$coefficients["price.per.liter"]       # Price coef (attenuated due to endogeneity)

# -------------------------
# 6.3 IV AGGREGATE LOGIT (instrument price only, no nesting)
# -------------------------
iv_agg <- ivreg(
  # Main equation (same as OLS aggregate)
  y ~ -1 + PACKAGE:L5 
  + store01 + store02 + store03 + store04 + store05 + store06 
  + store07 + store08 + store09 + store10 + store11 + store12 
  + price.per.liter + display + feature + coupon
  
  # Instrument equation: all exogenous variables + excluded instruments for price
  | -1 + PACKAGE:L5 
  + store01 + store02 + store03 + store04 + store05 + store06 
  + store07 + store08 + store09 + store10 + store11 + store12 
  + display + feature + coupon
  #+ price_other_market                 # Hausman: price in other market
  + avg_price_other_stores_brand       # Hausman: LOOC mean price
  #+ min_price_other_stores_brand       # Hausman: LOOC min price
  + alumnium.price + sugar.price,  # Cost shifters
  
  data = dat
)

print("=== IV Aggregate Logit (price instrumented) ===")
# Robust standard errors (HC1)
print(coeftest(iv_agg, vcov = vcovHC(iv_agg, type = "HC1")))

# Diagnostics: weak instruments, endogeneity test, overidentification
print(summary(iv_agg, diagnostics = TRUE))


# -------------------------
# 6.4 IV NESTED LOGIT (instrument BOTH price and ln_share_g) - MAIN MODEL
# -------------------------
iv_nl <- ivreg(
  # Main equation: nested logit with both endogenous variables
  y ~ ln_share_g                         # within-group share - ENDOGENOUS
  - 1 + PACKAGE:L5                     # brand x format fixed effects
  + store01 + store02 + store03 + store04 + store05 + store06 
  + store07 + store08 + store09 + store10 + store11 + store12  # store fixed effects
  + price.per.liter                    # price - ENDOGENOUS
  + display + feature + coupon         # promotion effects
  
  # Instrument equation: all exogenous + excluded instruments for BOTH endogenous vars
  | -1 + PACKAGE:L5 
  + store01 + store02 + store03 + store04 + store05 + store06 
  + store07 + store08 + store09 + store10 + store11 + store12 
  + display + feature + coupon
  # Instruments for price:
  #+ price_other_market                 # Hausman: other market
  + avg_price_other_stores_brand       # Hausman: LOOC mean
  #+ min_price_other_stores_brand       # Hausman: LOOC min
  + alumnium.price + sugar.price   # Cost shifters
  + aluminum_x_can + sugar_x_regular   # Cost shifter interactions
  
  # Instruments for ln_share_g (BLP-style: characteristics of other products in nest):
  + avg_price_others_nest,              # average price of other products in nest
  #+ num_products_nest                  # number of products in nest
  #+ sum_display_others                 # sum of display of others in nest
  #+ sum_feature_others                 # sum of feature of others in nest
  #+ sum_coupon_others,                 # sum of coupons of others in nest
  
  data = dat
)

print("=== IV Nested Logit (price AND ln_share_g instrumented) - MAIN MODEL ===")
# Robust standard errors (HC1)
print(coeftest(iv_nl, vcov = vcovHC(iv_nl, type = "HC1")))

# Diagnostics: weak instruments, endogeneity test (Hausman), overidentification (Sargan)
print(summary(iv_nl, diagnostics = TRUE))

# ------------------------------------------------------------------------------
# STEP 7: ELASTICITIES - NESTED LOGIT OWN AND CROSS ELASTICITIES
# ------------------------------------------------------------------------------

# Note: Your professor's code uses fixed prices for elasticity computation.
# You need to define representative prices for each product first.
# These should be average prices from your data or scenario prices.

# Define representative prices for 8 products:
# Bottles: Coke Classic, Diet Coke, Pepsi, Diet Pepsi
# Cans: Coke Classic, Diet Coke, Pepsi, Diet Pepsi

# Compute average prices by brand and package from the data
price_summary <- dat %>%
  group_by(L5, PACKAGE) %>%
  summarise(avg_price = mean(price.per.liter, na.rm = TRUE), .groups = "drop")

# Extract prices for each product (adjust based on your actual PACKAGE values)
price.cc.b <- price_summary %>% filter(L5 == "COKE CLASSIC", PACKAGE != "CAN") %>% pull(avg_price)
price.dc.b <- price_summary %>% filter(L5 == "DIET COKE", PACKAGE != "CAN") %>% pull(avg_price)
price.p.b  <- price_summary %>% filter(L5 == "PEPSI", PACKAGE != "CAN") %>% pull(avg_price)
price.dp.b <- price_summary %>% filter(L5 == "DIET PEPSI", PACKAGE != "CAN") %>% pull(avg_price)

price.cc.c <- price_summary %>% filter(L5 == "COKE CLASSIC", PACKAGE == "CAN") %>% pull(avg_price)
price.dc.c <- price_summary %>% filter(L5 == "DIET COKE", PACKAGE == "CAN") %>% pull(avg_price)
price.p.c  <- price_summary %>% filter(L5 == "PEPSI", PACKAGE == "CAN") %>% pull(avg_price)
price.dp.c <- price_summary %>% filter(L5 == "DIET PEPSI", PACKAGE == "CAN") %>% pull(avg_price)

# Handle case where bottle products might not exist for all brands
if(length(price.cc.b) == 0) price.cc.b <- mean(dat$price.per.liter[dat$L5 == "COKE CLASSIC"], na.rm = TRUE)
if(length(price.dc.b) == 0) price.dc.b <- mean(dat$price.per.liter[dat$L5 == "DIET COKE"], na.rm = TRUE)
if(length(price.p.b) == 0) price.p.b <- mean(dat$price.per.liter[dat$L5 == "PEPSI"], na.rm = TRUE)
if(length(price.dp.b) == 0) price.dp.b <- mean(dat$price.per.liter[dat$L5 == "DIET PEPSI"], na.rm = TRUE)

print("Representative prices:")
print(paste("Coke Classic Bottle:", round(price.cc.b, 3)))
print(paste("Diet Coke Bottle:", round(price.dc.b, 3)))
print(paste("Pepsi Bottle:", round(price.p.b, 3)))
print(paste("Diet Pepsi Bottle:", round(price.dp.b, 3)))
print(paste("Coke Classic Can:", round(price.cc.c, 3)))
print(paste("Diet Coke Can:", round(price.dc.c, 3)))
print(paste("Pepsi Can:", round(price.p.c, 3)))
print(paste("Diet Pepsi Can:", round(price.dp.c, 3)))

# -------------------------
# 7.1 Elasticities for OLS Nested Logit
# -------------------------

print(names(ols_nl$coefficients))
print(length(ols_nl$coefficients))

print("=== OLS NESTED LOGIT ELASTICITIES ===")

# Extract coefficients
ols_nl$coefficients
sigma <- ols_nl$coefficients[1]
alpha <- ols_nl$coefficients[14]

print(paste("Sigma (OLS):", round(sigma, 4)))
print(paste("Alpha (OLS):", round(alpha, 4)))

# Extract brand x package fixed effects (adjust indices based on your model)
# Coefficient positions depend on formula order:
# ln_share_g [1], store01-12 [2-13], price.per.liter [14], display [15], feature [16], coupon [17]
# PACKAGE:L5 [18-25]: BOTTLE:COKE CLASSIC, BOTTLE:DIET COKE, etc.

# Build delta for each product
delta.cc.b <- ols_nl$coefficients["PACKAGEBOTTLE:L5COKE CLASSIC"] + alpha * price.cc.b
delta.dc.b <- ols_nl$coefficients["PACKAGEBOTTLE:L5DIET COKE"] + alpha * price.dc.b
delta.p.b  <- ols_nl$coefficients["PACKAGEBOTTLE:L5PEPSI"] + alpha * price.p.b
delta.dp.b <- ols_nl$coefficients["PACKAGEBOTTLE:L5DIET PEPSI"] + alpha * price.dp.b

delta.cc.c <- ols_nl$coefficients["PACKAGECAN:L5COKE CLASSIC"] + alpha * price.cc.c
delta.dc.c <- ols_nl$coefficients["PACKAGECAN:L5DIET COKE"] + alpha * price.dc.c
delta.p.c  <- ols_nl$coefficients["PACKAGECAN:L5PEPSI"] + alpha * price.p.c
delta.dp.c <- ols_nl$coefficients["PACKAGECAN:L5DIET PEPSI"] + alpha * price.dp.c

delta.og <- 0

# Rescale by (1-sigma)
d.cc.b <- delta.cc.b / (1 - sigma)
d.dc.b <- delta.dc.b / (1 - sigma)
d.p.b  <- delta.p.b  / (1 - sigma)
d.dp.b <- delta.dp.b / (1 - sigma)

d.cc.c <- delta.cc.c / (1 - sigma)
d.dc.c <- delta.dc.c / (1 - sigma)
d.p.c  <- delta.p.c  / (1 - sigma)
d.dp.c <- delta.dp.c / (1 - sigma)

# Group denominators (REGULAR = bottles for COKE/PEPSI, DIET = bottles for DIET COKE/DIET PEPSI)
# Assuming nest by DIET: group1 = REGULAR (Coke Classic, Pepsi), group2 = DIET (Diet Coke, Diet Pepsi)
D.g1 <- exp(d.cc.b) + exp(d.p.b) + exp(d.cc.c) + exp(d.p.c)     # REGULAR nest
D.g2 <- exp(d.dc.b) + exp(d.dp.b) + exp(d.dc.c) + exp(d.dp.c)   # DIET nest

# Within-group shares
s.cc.b.g <- exp(d.cc.b) / D.g1
s.p.b.g  <- exp(d.p.b)  / D.g1
s.cc.c.g <- exp(d.cc.c) / D.g1
s.p.c.g  <- exp(d.p.c)  / D.g1

s.dc.b.g <- exp(d.dc.b) / D.g2
s.dp.b.g <- exp(d.dp.b) / D.g2
s.dc.c.g <- exp(d.dc.c) / D.g2
s.dp.c.g <- exp(d.dp.c) / D.g2

# Group shares
t1 <- D.g1^(1 - sigma)
t2 <- D.g2^(1 - sigma)
s.g1 <- t1 / (1 + t1 + t2)
s.g2 <- t2 / (1 + t1 + t2)

# Product shares
s.cc.b <- s.cc.b.g * s.g1
s.p.b  <- s.p.b.g  * s.g1
s.cc.c <- s.cc.c.g * s.g1
s.p.c  <- s.p.c.g  * s.g1

s.dc.b <- s.dc.b.g * s.g2
s.dp.b <- s.dp.b.g * s.g2
s.dc.c <- s.dc.c.g * s.g2
s.dp.c <- s.dp.c.g * s.g2

s.0 <- 1 - s.cc.b - s.dc.b - s.p.b - s.dp.b - s.cc.c - s.dc.c - s.p.c - s.dp.c

print("Product shares (OLS):")
print(rbind(s.cc.b, s.dc.b, s.p.b, s.dp.b, s.cc.c, s.dc.c, s.p.c, s.dp.c, s.0))

# Own-price elasticities
el.cc.b <- alpha * price.cc.b * (1/(1-sigma) - (sigma/(1-sigma)) * s.cc.b.g - s.cc.b)
el.dc.b <- alpha * price.dc.b * (1/(1-sigma) - (sigma/(1-sigma)) * s.dc.b.g - s.dc.b)
el.p.b  <- alpha * price.p.b  * (1/(1-sigma) - (sigma/(1-sigma)) * s.p.b.g  - s.p.b)
el.dp.b <- alpha * price.dp.b * (1/(1-sigma) - (sigma/(1-sigma)) * s.dp.b.g - s.dp.b)

el.cc.c <- alpha * price.cc.c * (1/(1-sigma) - (sigma/(1-sigma)) * s.cc.c.g - s.cc.c)
el.dc.c <- alpha * price.dc.c * (1/(1-sigma) - (sigma/(1-sigma)) * s.dc.c.g - s.dc.c)
el.p.c  <- alpha * price.p.c  * (1/(1-sigma) - (sigma/(1-sigma)) * s.p.c.g  - s.p.c)
el.dp.c <- alpha * price.dp.c * (1/(1-sigma) - (sigma/(1-sigma)) * s.dp.c.g - s.dp.c)

el.nl <- rbind(el.cc.b, el.dc.b, el.p.b, el.dp.b, el.cc.c, el.dc.c, el.p.c, el.dp.c)
print("Own-price elasticities (OLS):")
print(el.nl)
# Cross-price elasticities within group
elc.cc.b <- -alpha * price.cc.b * ((sigma/(1-sigma)) * s.cc.b.g + s.cc.b)
elc.dc.b <- -alpha * price.dc.b * ((sigma/(1-sigma)) * s.dc.b.g + s.dc.b)
elc.p.b  <- -alpha * price.p.b  * ((sigma/(1-sigma)) * s.p.b.g  + s.p.b)
elc.dp.b <- -alpha * price.dp.b * ((sigma/(1-sigma)) * s.dp.b.g + s.dp.b)

elc.cc.c <- -alpha * price.cc.c * ((sigma/(1-sigma)) * s.cc.c.g + s.cc.c)
elc.dc.c <- -alpha * price.dc.c * ((sigma/(1-sigma)) * s.dc.c.g + s.dc.c)
elc.p.c  <- -alpha * price.p.c  * ((sigma/(1-sigma)) * s.p.c.g  + s.p.c)
elc.dp.c <- -alpha * price.dp.c * ((sigma/(1-sigma)) * s.dp.c.g + s.dp.c)

elc.nl <- rbind(elc.cc.b, elc.dc.b, elc.p.b, elc.dp.b, elc.cc.c, elc.dc.c, elc.p.c, elc.dp.c)
print("Cross-price elasticities within group (OLS):")
print(elc.nl)
# Cross-price elasticities across groups
elcc.cc.b <- -alpha * price.cc.b * s.cc.b
elcc.dc.b <- -alpha * price.dc.b * s.dc.b
elcc.p.b  <- -alpha * price.p.b  * s.p.b
elcc.dp.b <- -alpha * price.dp.b * s.dp.b

elcc.cc.c <- -alpha * price.cc.c * s.cc.c
elcc.dc.c <- -alpha * price.dc.c * s.dc.c
elcc.p.c  <- -alpha * price.p.c  * s.p.c
elcc.dp.c <- -alpha * price.dp.c * s.dp.c

elcc.nl <- rbind(elcc.cc.b, elcc.dc.b, elcc.p.b, elcc.dp.b, elcc.cc.c, elcc.dc.c, elcc.p.c, elcc.dp.c)
print("Cross-price elasticities across groups (OLS):")
print(elcc.nl)
# Combined elasticity table
elast.nl <- cbind(el.nl, elc.nl, elcc.nl)
print("OLS Nested Logit Elasticities:")
print(elast.nl)



# -------------------------
# 7.2 IV Nested Logit Elasticities (same structure)
# -------------------------
print("=== IV NESTED LOGIT ELASTICITIES ===")

# CORRECT extraction by name

sigma <- unname(iv_nl$coefficients["ln_share_g"])
alpha <- unname(iv_nl$coefficients["price.per.liter"])

print(paste("Sigma (IV):", round(sigma, 4)))
print(paste("Alpha (IV):", round(alpha, 4)))

# Build delta for each product
delta.cc.b <- iv_nl$coefficients["PACKAGEBOTTLE:L5COKE CLASSIC"] + alpha * price.cc.b
delta.dc.b <- iv_nl$coefficients["PACKAGEBOTTLE:L5DIET COKE"] + alpha * price.dc.b
delta.p.b  <- iv_nl$coefficients["PACKAGEBOTTLE:L5PEPSI"] + alpha * price.p.b
delta.dp.b <- iv_nl$coefficients["PACKAGEBOTTLE:L5DIET PEPSI"] + alpha * price.dp.b

delta.cc.c <- iv_nl$coefficients["PACKAGECAN:L5COKE CLASSIC"] + alpha * price.cc.c
delta.dc.c <- iv_nl$coefficients["PACKAGECAN:L5DIET COKE"] + alpha * price.dc.c
delta.p.c  <- iv_nl$coefficients["PACKAGECAN:L5PEPSI"] + alpha * price.p.c
delta.dp.c <- iv_nl$coefficients["PACKAGECAN:L5DIET PEPSI"] + alpha * price.dp.c

delta.og <- 0

# Rescale by (1-sigma)
d.cc.b <- delta.cc.b / (1 - sigma)
d.dc.b <- delta.dc.b / (1 - sigma)
d.p.b  <- delta.p.b  / (1 - sigma)
d.dp.b <- delta.dp.b / (1 - sigma)

d.cc.c <- delta.cc.c / (1 - sigma)
d.dc.c <- delta.dc.c / (1 - sigma)
d.p.c  <- delta.p.c  / (1 - sigma)
d.dp.c <- delta.dp.c / (1 - sigma)

# Group denominators (REGULAR = bottles for COKE/PEPSI, DIET = bottles for DIET COKE/DIET PEPSI)
# Assuming nest by DIET: group1 = REGULAR (Coke Classic, Pepsi), group2 = DIET (Diet Coke, Diet Pepsi)
D.g1 <- exp(d.cc.b) + exp(d.p.b) + exp(d.cc.c) + exp(d.p.c)     # REGULAR nest
D.g2 <- exp(d.dc.b) + exp(d.dp.b) + exp(d.dc.c) + exp(d.dp.c)   # DIET nest

# Within-group shares
s.cc.b.g <- exp(d.cc.b) / D.g1
s.p.b.g  <- exp(d.p.b)  / D.g1
s.cc.c.g <- exp(d.cc.c) / D.g1
s.p.c.g  <- exp(d.p.c)  / D.g1

s.dc.b.g <- exp(d.dc.b) / D.g2
s.dp.b.g <- exp(d.dp.b) / D.g2
s.dc.c.g <- exp(d.dc.c) / D.g2
s.dp.c.g <- exp(d.dp.c) / D.g2

print("Within-group shares (IV):")
print(rbind(s.cc.b.g, s.dc.b.g, s.p.b.g, s.dp.b.g, s.cc.c.g, s.dc.c.g, s.p.c.g, s.dp.c.g))

# Group shares
t1 <- D.g1^(1 - sigma)
t2 <- D.g2^(1 - sigma)
s.g1 <- t1 / (1 + t1 + t2)
s.g2 <- t2 / (1 + t1 + t2)

print("Group shares (IV):")
print(paste("Group 1 (REGULAR):", round(s.g1, 4)))
print(paste("Group 2 (DIET):", round(s.g2, 4)))

# Product shares
s.cc.b <- s.cc.b.g * s.g1
s.p.b  <- s.p.b.g  * s.g1
s.cc.c <- s.cc.c.g * s.g1
s.p.c  <- s.p.c.g  * s.g1

s.dc.b <- s.dc.b.g * s.g2
s.dp.b <- s.dp.b.g * s.g2
s.dc.c <- s.dc.c.g * s.g2
s.dp.c <- s.dp.c.g * s.g2

s.0 <- 1 - s.cc.b - s.dc.b - s.p.b - s.dp.b - s.cc.c - s.dc.c - s.p.c - s.dp.c

print("Product shares (IV):")
print(rbind(s.cc.b, s.dc.b, s.p.b, s.dp.b, s.cc.c, s.dc.c, s.p.c, s.dp.c, s.0))

# Own-price elasticities
el.cc.b <- alpha * price.cc.b * (1/(1-sigma) - (sigma/(1-sigma)) * s.cc.b.g - s.cc.b)
el.dc.b <- alpha * price.dc.b * (1/(1-sigma) - (sigma/(1-sigma)) * s.dc.b.g - s.dc.b)
el.p.b  <- alpha * price.p.b  * (1/(1-sigma) - (sigma/(1-sigma)) * s.p.b.g  - s.p.b)
el.dp.b <- alpha * price.dp.b * (1/(1-sigma) - (sigma/(1-sigma)) * s.dp.b.g - s.dp.b)

el.cc.c <- alpha * price.cc.c * (1/(1-sigma) - (sigma/(1-sigma)) * s.cc.c.g - s.cc.c)
el.dc.c <- alpha * price.dc.c * (1/(1-sigma) - (sigma/(1-sigma)) * s.dc.c.g - s.dc.c)
el.p.c  <- alpha * price.p.c  * (1/(1-sigma) - (sigma/(1-sigma)) * s.p.c.g  - s.p.c)
el.dp.c <- alpha * price.dp.c * (1/(1-sigma) - (sigma/(1-sigma)) * s.dp.c.g - s.dp.c)

el.nl <- rbind(el.cc.b, el.dc.b, el.p.b, el.dp.b, el.cc.c, el.dc.c, el.p.c, el.dp.c)

print("Own-price elasticities (IV):")
print(el.nl)

# Cross-price elasticities within group
elc.cc.b <- -alpha * price.cc.b * ((sigma/(1-sigma)) * s.cc.b.g + s.cc.b)
elc.dc.b <- -alpha * price.dc.b * ((sigma/(1-sigma)) * s.dc.b.g + s.dc.b)
elc.p.b  <- -alpha * price.p.b  * ((sigma/(1-sigma)) * s.p.b.g  + s.p.b)
elc.dp.b <- -alpha * price.dp.b * ((sigma/(1-sigma)) * s.dp.b.g + s.dp.b)

elc.cc.c <- -alpha * price.cc.c * ((sigma/(1-sigma)) * s.cc.c.g + s.cc.c)
elc.dc.c <- -alpha * price.dc.c * ((sigma/(1-sigma)) * s.dc.c.g + s.dc.c)
elc.p.c  <- -alpha * price.p.c  * ((sigma/(1-sigma)) * s.p.c.g  + s.p.c)
elc.dp.c <- -alpha * price.dp.c * ((sigma/(1-sigma)) * s.dp.c.g + s.dp.c)

elc.nl <- rbind(elc.cc.b, elc.dc.b, elc.p.b, elc.dp.b, elc.cc.c, elc.dc.c, elc.p.c, elc.dp.c)

print("Cross-price elasticities within group (IV):")
print(elc.nl)

# Cross-price elasticities across groups
elcc.cc.b <- -alpha * price.cc.b * s.cc.b
elcc.dc.b <- -alpha * price.dc.b * s.dc.b
elcc.p.b  <- -alpha * price.p.b  * s.p.b
elcc.dp.b <- -alpha * price.dp.b * s.dp.b

elcc.cc.c <- -alpha * price.cc.c * s.cc.c
elcc.dc.c <- -alpha * price.dc.c * s.dc.c
elcc.p.c  <- -alpha * price.p.c  * s.p.c
elcc.dp.c <- -alpha * price.dp.c * s.dp.c

elcc.nl <- rbind(elcc.cc.b, elcc.dc.b, elcc.p.b, elcc.dp.b, elcc.cc.c, elcc.dc.c, elcc.p.c, elcc.dp.c)

print("Cross-price elasticities across groups (IV):")
print(elcc.nl)

# Combined elasticity table
elast.nl <- cbind(el.nl, elc.nl, elcc.nl)
print("IV Nested Logit Elasticities:")
print(elast.nl)

# SWP4 values:
alpha_iv <- iv_nl$coefficients["price.per.liter"]
sigma_iv <- iv_nl$coefficients["ln_share_g"]
mean_elasticity <- mean(c(el.cc.b, el.cc.c, el.dc.b, el.dc.c, 
                          el.p.b, el.p.c, el.dp.b, el.dp.c))

# Ratio of within/across nest cross-elasticities:
ratio <- (sigma/(1-sigma))
print(paste("Alpha (IV):", round(alpha_iv, 4)))
print(paste("Sigma (IV):", round(sigma_iv, 4)))
print(paste("Mean own-price elasticity (IV):", round(mean_elasticity, 4)))
print(paste("Ratio of within/across nest cross-elasticities (sigma/(1-sigma)):", round(ratio, 4)))




# Summary statistics for conclusion:
cat("Nesting parameter sigma:", round(sigma_iv, 3))
cat("Implied within-nest correlation rho:", round(1 - sigma_iv^2, 3))
cat("Mean own-price elasticity:", round(mean_elasticity, 2))
cat("Elasticity range:", round(min(c(el.cc.b, el.cc.c, el.dc.b, el.dc.c, 
                                     el.p.b, el.p.c, el.dp.b, el.dp.c)), 2), "to",
    round(max(c(el.cc.b, el.cc.c, el.dc.b, el.dc.c, 
                el.p.b, el.p.c, el.dp.b, el.dp.c)), 2))
# ------------------------------------------------------------------------------
# NOTES FOR 10-PAGE WRITE-UP
# ------------------------------------------------------------------------------
# 1. Introduction & Research Question (0.5 page)
#    - Combine SWP3 non-nested models into nested logit
#    - Research question: How does nesting affect substitution patterns?
#
# 2. Data & Market Definition (1-1.5 pages)
#    - IRI scanner data, 2004-2006, Eau Claire & Pittsfield DMAs
#    - Store-week markets, 4 brands x 2 formats = 8 inside goods
#    - Outside good = residual trips, market size = total trips
#
# 3. Model: Utility Specification (1.5-2 pages)
#    - Nested logit utility: u_ijt = δ_j + ε_{ig} + (1-σ)ε_ij
#    - δ_j = brand×format FE + α*price + β_display + β_feature + β_coupon + store FE
#    - Nesting: DIET vs REGULAR (based on SWP3 IIA violations)
#    - Share formulas: s_j, s_{j|g}, s_0
#
# 4. Identification & Endogeneity (2 pages)
#    - Why price is endogenous: unobserved demand shocks
#    - Why ln(s_{j|g}) is endogenous: cannibalization within nests
#    - Instruments:
#      a) Hausman: other market price, LOOC mean/min
#      b) Cost shifters: sugar, aluminum, gasoline + interactions
#      c) BLP: avg price, count, promotions of other products in nest
#    - Exclusion restrictions and relevance
#
# 5. Estimation Results (2 pages)
#    - Table: OLS vs IV for aggregate and nested models
#    - Coefficient interpretation: α, σ, promotion effects
#    - First-stage diagnostics: F-stats, weak IV tests
#    - Endogeneity tests (Hausman), overidentification (Sargan)
#
# 6. Elasticities & IIA Discussion (1.5-2 pages)
#    - Present 8x8 elasticity matrix
#    - Compare own vs cross elasticities
#    - Within-nest vs across-nest substitution
#    - How nested logit relaxes IIA vs SWP3 non-nested
#    - Stockpiling in 12-can packs (higher |ε|)
#
# 7. Robustness (1 page)
#    - Alternative nest by PACKAGE
#    - Clustered SE by store
#    - DMA subsamples
#    - Time trends
#
# 8. Conclusions (0.5-1 page)
#    - Key findings: σ ∈ (0,1), asymmetric substitution
#    - Managerial implications: pricing, promotions, product line
#
# 9. Appendix
#    - Full regression tables
#    - Instrument correlation matrix
#    - Additional elasticity matrices for different markets
#
# ------------------------------------------------------------------------------

