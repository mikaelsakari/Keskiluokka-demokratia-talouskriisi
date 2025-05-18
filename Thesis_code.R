OTSIKKO: Keskiluokka, demokratia ja talouskriisi. 
Globaalin finanssikriisin vaikutus entiseen Neuvostoliittoon kuuluneiden autoritaaristen valtioiden keskiluokan demokratiatukeen ja poliittiseen mobilisaatioon


### LADATAAN DATA
lit_1 <- read_dta("lITS 2006 data.dta") %>%
  filter(!is.na(weight_2))

lit_2 <- read_dta("lits_ii.dta")

wave_1 <- data.frame(matrix(ncol = 0, nrow = nrow(lit_1)))
wave_2 <- data.frame(matrix(ncol = 0, nrow = nrow(lit_2)))



#### LITS 1

### PERUSTIEDOT JA TUNNISTEET
wave_1$countryname <- lit_1$countryname
wave_1$id <- lit_1$id
wave_1$psu <- lit_1$tabled
wave_1$fedweight <- lit_1$weight_2

### IKÄ JA SUKUPUOLI
wave_1$age <- lit_1$ageB / 10
wave_1$agesq <- (lit_1$ageB)^2

wave_1$gender <- lit_1$genderB
wave_1$male <- ifelse(lit_1$genderB == 1, 1, 0)  # Mies = 1, Nainen = 0

### TYYTYVÄISYYS TALOUDEN TILAAN
wave_1$econ_satisfaction <- rep(NA, nrow(wave_1))
wave_1$econ_satisfaction[lit_1$q301_9 == 5] <- 2
wave_1$econ_satisfaction[lit_1$q301_9 == 4] <- 1
wave_1$econ_satisfaction[lit_1$q301_9 == 3] <- 0
wave_1$econ_satisfaction[lit_1$q301_9 == 2] <- -1
wave_1$econ_satisfaction[lit_1$q301_9 == 1] <- -2

wave_1$econ_satisfaction <- factor(wave_1$econ_satisfaction, levels = c(-2, -1, 0, 1, 2))
wave_1$econ_satisfaction <- relevel(wave_1$econ_satisfaction, ref = "0")

### UUDELLEENJAKAMISEN KANNATUS
wave_1$redistribution <- rep(NA, nrow(wave_1))
wave_1$redistribution[lit_1$q301_10 == 5] <- 2
wave_1$redistribution[lit_1$q301_10 == 4] <- 1
wave_1$redistribution[lit_1$q301_10 == 3] <- 0
wave_1$redistribution[lit_1$q301_10 == 2] <- -1
wave_1$redistribution[lit_1$q301_10 == 1] <- -2

wave_1$redistribution <- factor(wave_1$redistribution, levels = c(-2, -1, 0, 1, 2))
wave_1$redistribution <- relevel(wave_1$redistribution, ref = "0")

### MARKKINATALOUDEN KANNATUS
wave_1$makertpref <- rep(NA, nrow(wave_1))
wave_1$marketpref[lit_1$q310 == 1] <- 1     # Markkinatalous parempi
wave_1$marketpref[lit_1$q310 == 3] <- 0     # Ei väliä
wave_1$marketpref[lit_1$q310 == 2] <- -1    # Suunnitelmatalous parempi

wave_1$marketpref <- factor(wave_1$marketpref, levels = c(-1, 0, 1))
wave_1$marketpref <- relevel(wave_1$marketpref, ref = "0")

### TERVEYS
wave_1$health <- rep(NA, nrow(wave_1))
wave_1$health[lit_1$q705 == 1] <- 2     # Very good
wave_1$health[lit_1$q705 == 2] <- 1     # Good
wave_1$health[lit_1$q705 == 3] <- 0     # Medium
wave_1$health[lit_1$q705 == 4] <- -1    # Bad
wave_1$health[lit_1$q705 == 5] <- -2    # Very bad

wave_1$health <- factor(wave_1$health, levels = c(-2, -1, 0, 1, 2))
wave_1$health <- relevel(wave_1$health, ref = "0")



#### KESKILUOKAN OPERATIONALISOINTI 

### KOULUTUSTASO 
wave_1$edu <- rep(NA, nrow(wave_1))
wave_1$edu[lit_1$q501 == 1] <- 1  
wave_1$edu[lit_1$q501 == 2] <- 2  
wave_1$edu[lit_1$q501 == 3] <- 3  
wave_1$edu[lit_1$q501 == 4] <- 4  
wave_1$edu[lit_1$q501 == 5] <- 5  # Higher professional degree
wave_1$edu[lit_1$q501 == 6] <- 6  # Postgraduate degree


### AMMATTILUOKITUS
wave_1$occupation <- rep(NA, nrow(wave_1))

for (i in 1:10) {
  wave_1$occupation[rowSums(lit_1[, paste0("q602b1", letters[1:7])] == i, na.rm = TRUE) > 0] <- i
}

### KESKILUOKKA AMMATIN PERUSTEELLA (ISCO-LUOKAT 1-3)
wave_1$mc_occ <- ifelse(wave_1$occupation %in% c(1, 2, 3), 1, 0)

### OPISKELIJASTATUS
wave_1$reason_not_looking_job <- ifelse(lit_1$q512 %in% 1:9, lit_1$q512, NA)
wave_1$reason_not_looking_job <- factor(wave_1$reason_not_looking_job, 
                                        levels = 1:9, 
                                        labels = c("looking after the family/house",
                                                   "temporary sick/injured",
                                                   "long term sick/disabled",
                                                   "no suitable jobs available",
                                                   "waiting for an answer",
                                                   "student",
                                                   "retired and not working",
                                                   "no need/want",
                                                   "other"))
wave_1$student <- ifelse(wave_1$reason_not_looking_job == "student", 1, 0)

### KESKILUOKKA KOULUTUKSEN PERUSTEELLA
wave_1$mc_edu <- ifelse(wave_1$edu %in% c(4, 5, 6), 1, 0)

### KESKILUOKKA KOKONAISUUTENA
# Perustuu koulutukseen ja ammattiin TAI opiskelijastatukseen
wave_1$middle_class <- ifelse((wave_1$mc_edu == 1 & wave_1$mc_occ == 1) | wave_1$student == 1, 1, 0)


#### DEMOKRATIATUKI
wave_1$democracy <- rep(NA, nrow(wave_1))
wave_1$democracy[lit_1$q311 == 1] <- 1 # 1 = Demokratia parempi
wave_1$democracy[lit_1$q311 == 2] <- -1 # 2 = Autoritaarinen järjestelmä parempi
wave_1$democracy[lit_1$q311 == 3] <- 0 # 3 = Ei väliä

wave_1$democracy <- factor(wave_1$democracy, levels = c(-1, 0, 1))
wave_1$democracy <- relevel(wave_1$democracy, ref = "0")

# dummy-muuttuja demokratian kannatukselle
wave_1$democracy_support <- ifelse(wave_1$democracy == 1, 1, 0)


#### POLIITTINEN MOBILISAATIO
## Osallistuminen  mielenosoitukseen
wave_1$lawful_demonstration <- rep(NA, nrow(wave_1))
wave_1$lawful_demonstration[lit_1$q704_1 == 1] <- 2  # Have done
wave_1$lawful_demonstration[lit_1$q704_1 == 2] <- 1  # Might do
wave_1$lawful_demonstration[lit_1$q704_1 == 3] <- 0  # Would never do

wave_1$lawful_demonstration <- factor(wave_1$lawful_demonstration, levels = c(0, 1, 2))
wave_1$lawful_demonstration <- relevel(wave_1$lawful_demonstration, ref = "0")

## Osallistuminen lakkoon
wave_1$strike <- rep(NA, nrow(wave_1))
wave_1$strike[lit_1$q704_2 == 1] <- 2  # Have done
wave_1$strike[lit_1$q704_2 == 2] <- 1  # Might do
wave_1$strike[lit_1$q704_2 == 3] <- 0  # Would never do

wave_1$strike <- factor(wave_1$strike, levels = c(0, 1, 2))
wave_1$strike <- relevel(wave_1$strike, ref = "0")

## Vetoomuksen allekirjoittaminen
wave_1$sign_petition <- rep(NA, nrow(wave_1))
wave_1$sign_petition[lit_1$q704_4 == 1] <- 2  # Have done
wave_1$sign_petition[lit_1$q704_4 == 2] <- 1  # Might do
wave_1$sign_petition[lit_1$q704_4 == 3] <- 0  # Would never do

wave_1$sign_petition <- factor(wave_1$sign_petition, levels = c(0, 1, 2))
wave_1$sign_petition <- relevel(wave_1$sign_petition, ref = "0")



#### LITS 2

### PERUSTIEDOT
wave_2$psu <- lit_2$psu
wave_2$countryname <- tolower(lit_2$countryname)
wave_2$id <- lit_2$SerialID
wave_2$fedweight <- lit_2$XCweight

### IKÄ JA IKÄNELIÖ
wave_2$age <- lit_2$respondentage / 10
wave_2$agesq <- (lit_2$respondentage)^2

### SUKUPUOLI
wave_2$gender <- rep(NA, nrow(wave_2))
wave_2$gender[lit_2$respondentgender == 1] <- 1  # Mies
wave_2$gender[lit_2$respondentgender == 2] <- 0  # Nainen

# MIES
wave_2$male <- ifelse(wave_2$gender == 1, 1, 0)
wave_2$male <- factor(wave_2$male)

### TYYTYVÄISYYS TALOUTEEN
wave_2$econ_satisfaction <- rep(NA, nrow(wave_2))
wave_2$econ_satisfaction[lit_2$q301g == 5] <- 2
wave_2$econ_satisfaction[lit_2$q301g == 4] <- 1
wave_2$econ_satisfaction[lit_2$q301g == 3] <- 0
wave_2$econ_satisfaction[lit_2$q301g == 2] <- -1
wave_2$econ_satisfaction[lit_2$q301g == 1] <- -2

wave_2$econ_satisfaction <- factor(wave_2$econ_satisfaction, levels = c(-2, -1, 0, 1, 2))
wave_2$econ_satisfaction <- relevel(wave_2$econ_satisfaction, ref = "0")

### UUDELLEENJAKAMISEN KANNATUS
wave_2$redistribution <- rep(NA, nrow(wave_2))
wave_2$redistribution[lit_2$q301h == 5] <- 2
wave_2$redistribution[lit_2$q301h == 4] <- 1
wave_2$redistribution[lit_2$q301h == 3] <- 0
wave_2$redistribution[lit_2$q301h == 2] <- -1
wave_2$redistribution[lit_2$q301h == 1] <- -2

wave_2$redistribution <- factor(wave_2$redistribution, levels = c(-2, -1, 0, 1, 2))
wave_2$redistribution <- relevel(wave_2$redistribution, ref = "0")

### MARKKINATALOUDEN KANNATUS
wave_2$marketpref <- rep(NA, nrow(wave_2))
wave_2$marketpref[lit_2$q310 == 1] <- 1     # Markkinatalous parempi
wave_2$marketpref[lit_2$q310 == 2] <- -1    # Suunnitelmatalous parempi
wave_2$marketpref[lit_2$q310 == 3] <- 0     # Ei väliä

wave_2$marketpref <- factor(wave_2$marketpref, levels = c(-1, 0, 1))
wave_2$marketpref <- relevel((wave_2$marketpref), ref = "0")

### TERVEYS
wave_2$health <- rep(NA, nrow(wave_2))
wave_2$health[lit_2$q704 == 1] <- 2     # Very good
wave_2$health[lit_2$q704 == 2] <- 1     # Good
wave_2$health[lit_2$q704 == 3] <- 0     # Medium
wave_2$health[lit_2$q704 == 4] <- -1    # Bad
wave_2$health[lit_2$q704 == 5] <- -2    # Very bad

wave_2$health <- factor(wave_2$health, levels = c(-2, -1, 0, 1, 2))
wave_2$health <- relevel(wave_2$health, ref = "0")

### DEMOKRATIATUKI
wave_2$democracy <- rep(NA, nrow(wave_2))
wave_2$democracy[lit_2$q311 == 1] <- 1 # 1 = Demokratia parempi
wave_2$democracy[lit_2$q311 == 2] <- -1 # 2 = Autoritaarinen järjestelmä parempi
wave_2$democracy[lit_2$q311 == 3] <- 0 # 3 = Ei väliä

wave_2$democracy <- factor(wave_2$democracy, levels = c(-1, 0, 1))
wave_2$democracy <- relevel(wave_2$democracy, ref = "0")

## Dummy: Demokratian kannatus
wave_2$democracy_support <- ifelse(wave_2$democracy == 1, 1, 0)


#### KESKILUOKKA 

## KOULUTUSTASO (EDUCATION)
wave_2$edu <- rep(NA, nrow(wave_2))
wave_2$edu[lit_2$q515 == 1] <- 1  
wave_2$edu[lit_2$q515 == 2] <- 2  
wave_2$edu[lit_2$q515 == 3] <- 3  
wave_2$edu[lit_2$q515 == 4] <- 4  
wave_2$edu[lit_2$q515 == 5] <- 5  
wave_2$edu[lit_2$q515 == 6] <- 6  # Higher education
wave_2$edu[lit_2$q515 == 7] <- 7  # Postgraduate

wave_2$edu <- factor(wave_2$edu)

## AMMATILLINEN ASEMA (OCCUPATION)

wave_2$mc_occ <- ifelse(
  apply(lit_2[, c("q505_1", "q505_2", "q505_3", "q505_4", "q505_5")], 1, function(x) any(x %in% 1:21)),
  1, 0
)

## OPISKELIJASTATUS
wave_2$reason_not_looking_job <- ifelse(lit_2$q524 %in% 1:11, lit_2$q524, NA)

wave_2$reason_not_looking_job <- factor(wave_2$reason_not_looking_job, 
                                        levels = 1:11, 
                                        labels = c("looking after the family/house",
                                                   "temporary sick/injured",
                                                   "long term sick/disabled",
                                                   "no suitable jobs available",
                                                   "waiting for an answer",
                                                   "student",
                                                   "retired and not working",
                                                   "satisfied with current job",
                                                   "no need",
                                                   "doesn't want to work",
                                                   "other"))

wave_2$student <- ifelse(wave_2$reason_not_looking_job == "student", 1, 0)

## KESKILUOKKA KOULUTUKSEN PERUSTEELLA
wave_2$mc_edu <- ifelse(wave_2$edu %in% c(5, 6, 7), 1, 0)

## KESKILUOKKA KOKONAISUUTENA
wave_2$middle_class <- ifelse(
  (wave_2$mc_edu == 1 & wave_2$mc_occ == 1) | wave_2$student == 1,
  1, 0
)

wave_2$middle_class <- factor(wave_2$middle_class)


#### POLIITTINEN MOBILISAATIO

## Osallistuminen lailliseen mielenosoitukseen
wave_2$lawful_demonstration <- rep(NA, nrow(wave_2))
wave_2$lawful_demonstration[lit_2$q715_1 == 1] <- 2  # Have done
wave_2$lawful_demonstration[lit_2$q715_1 == 2] <- 1  # Might do
wave_2$lawful_demonstration[lit_2$q715_1 == 3] <- 0  # Would never do
wave_2$lawful_demonstration[wave_2$lawful_demonstration %in% c(-98, -1)] <- NA

wave_2$lawful_demonstration <- factor(wave_2$lawful_demonstration, levels = c(0, 1, 2))
wave_2$lawful_demonstration <- relevel(wave_2$lawful_demonstration, ref = "0")

## Osallistuminen lakkoon
wave_2$strike <- rep(NA, nrow(wave_2))
wave_2$strike[lit_2$q715_2 == 1] <- 2  # Have done
wave_2$strike[lit_2$q715_2 == 2] <- 1  # Might do
wave_2$strike[lit_2$q715_2 == 3] <- 0  # Would never do
wave_2$strike[wave_2$strike %in% c(-98, -1)] <- NA

wave_2$strike <- factor(wave_2$strike, levels = c(0, 1, 2))
wave_2$strike <- relevel(wave_2$strike, ref = "0")

## Vetoomuksen allekirjoittaminen
wave_2$sign_petition <- rep(NA, nrow(wave_2))
wave_2$sign_petition[lit_2$q715_4 == 1] <- 2  # Have done
wave_2$sign_petition[lit_2$q715_4 == 2] <- 1  # Might do
wave_2$sign_petition[lit_2$q715_4 == 3] <- 0  # Would never do
wave_2$sign_petition[wave_2$sign_petition %in% c(-98, -1)] <- NA

wave_2$sign_petition <- factor(wave_2$sign_petition, levels = c(0, 1, 2))
wave_2$sign_petition <- relevel(wave_2$sign_petition, ref = "0")


#### FINANSSIKRIISIN VAIKUTUS

# KRIISIN VAIKUTUS
wave_2$crisis_impact <- NA_integer_
wave_2$crisis_impact[lit_2$q801 == 1] <- 3
wave_2$crisis_impact[lit_2$q801 == 2] <- 2
wave_2$crisis_impact[lit_2$q801 == 3] <- 1
wave_2$crisis_impact[lit_2$q801 == 4] <- 0

wave_2$crisis_touched <- ifelse(wave_2$crisis_impact >= 1, 1, 0)

# Dummy: KRIISI VAIKUTTI JA LISÄKSI JOLLAKIN VALITULLA TAVALLA
wave_2$crisis_touched <- ifelse(
  wave_2$crisis_impact %in% 1:3 & lit_2$q802b %in% 1:11,
  1, 0
)


### YHDISTETÄÄN AALLOT YHTEEN DATAFRAMEEN

# MÄÄRITELLÄÄN NUMEERISET MUUTTUJAT
numeric_vars <- c(
  "id", "age", "agesq", "fedweight"
)

### VARMISTETAAN ETTÄ MUUNNETAAN MUUTTUJAT FAKTORIKSI (MUUT KUIN NUMERIISET)
wave_1 <- wave_1 %>%
  mutate(across(.cols = !all_of(numeric_vars), .fns = as.factor))

wave_2 <- wave_2 %>%
  mutate(across(.cols = !all_of(numeric_vars), .fns = as.factor))


### YHDISTETÄÄN AALLOT JA LISÄTÄÄN post_crisis-MUUTTUJA, JOKA JAKAA KYSELYT KAHTEEN (2006, 2010)
both_waves <- bind_rows(
  wave_1 %>% mutate(post_crisis = factor(0)),
  wave_2 %>% mutate(post_crisis = factor(1))
)


### MAIDEN NIMET NORMALISOIDAAN
both_waves <- both_waves %>%
  mutate(countryname = case_when(
    countryname == "bih" ~ "bosnia",
    countryname == "czech" ~ "czechrep",
    countryname == "great britain" ~ "uk",
    countryname == "macedonia" ~ "fyrom",
    TRUE ~ countryname
  ))

### VALITAAN VAIN HALUTUT MAAT
both_waves <- both_waves %>%
  filter(countryname %in% c(
    "armenia", "azerbaijan", "belarus", 
    "kazakhstan", "kyrgyzstan", "russia", 
    "tajikistan", "uzbekistan", "estonia", "lithuania", "latvia", 
    "albania", "bulgaria", "georgia", "moldova", 
    "romania", "ukraine", "bosnia", "croatia", 
    "fyrom", "montenegro", "serbia", "slovenia", 
    "slovakia", "czechrep", "hungary", "poland"
  ))


#### OTANTASUUNNITELMA

### MAARYHMÄT

# Autoritaariset maat
authoritarian_countries <- c(
  "armenia", "azerbaijan", "belarus", 
  "kazakhstan", "kyrgyzstan", "russia", 
  "tajikistan", "uzbekistan"
)

# EU-maat
democratic_eu <- c(
  "estonia", "lithuania", "latvia", 
  "czechrep", "romania", "hungary", 
  "poland", "slovenia", "slovakia", "bulgaria"
)

# Demokraattiset EU:n ulkopuoliset maat
democratic_non_eu <- c(
  "albania", "bosnia", "georgia", 
  "moldova", "ukraine", "croatia", 
  "fyrom", "montenegro", "serbia"
)

### OTANTASUUNNITELMARAKENNE

# Autoritaariset maat
authoritarian_design <- svydesign(
  ids = ~id,
  strata = ~psu,
  weights = ~fedweight,
  data = both_waves %>% filter(countryname %in% authoritarian_countries),
  nest = TRUE
)

# EU-maat
eu_design <- svydesign(
  ids = ~id,
  strata = ~psu,
  weights = ~fedweight,
  data = both_waves %>% filter(countryname %in% democratic_eu),
  nest = TRUE
)

# Demokraattiset EU:n ulkopuoliset maat
non_eu_design <- svydesign(
  ids = ~id,
  strata = ~psu,
  weights = ~fedweight,
  data = both_waves %>% filter(countryname %in% democratic_non_eu),
  nest = TRUE
)


### LOGISTINEN REGRESSIO

## AALTOJEN VÄLINEN MUUTOS

# 1. PERUSMALLI:
m1 <- svyglm(
  democracy_support ~ middle_class + post_crisis,
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m1)

m2 <- svyglm(
  democracy_support ~ middle_class * post_crisis,
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m2)

m3 <- svyglm(
  democracy_support ~ middle_class * post_crisis + redistribution + marketpref,
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m3)

m4 <- svyglm(
  democracy_support ~ middle_class * post_crisis + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health,
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m4)

# TÄYSI MALLI
m5 <- svyglm(
  democracy_support ~ middle_class * post_crisis + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health +
    relevel(factor(countryname), ref = "russia"),
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m5)


## FINANSSIKRIISIN VAIKUTUS

# PERSUMALLI
m6 <- svyglm(
  democracy_support ~ middle_class + crisis_touched,
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m6)


m7 <- svyglm(
  democracy_support ~ middle_class * crisis_touched,
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m7)


m8 <- svyglm(
  democracy_support ~ middle_class * crisis_touched + redistribution + marketpref,
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m8)


m9 <- svyglm(
  democracy_support ~ middle_class * crisis_touched + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health,
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m9)


m10 <- svyglm(
  democracy_support ~ middle_class * crisis_touched  + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health +
    relevel(factor(countryname), ref = "russia"),
  design = authoritarian_design,
  family = quasibinomial(link = "logit")
)
summary(m10)

### TEHDÄÄN SAMA LOGISTINEN REGRESSIO MYÖS NIIN EU-MAILLE KUIN EU:N ULKOPUOLISILLE MAILLE...


### MULTINOMIAALINEN REGRESSIO (Tehdään kaikille maaryhmille)

## FINANSSIKRIISIN VAIKUTUS POLITTISEEN MOBILISAATIOON

## MIELENOSOITUS
multi_a5 <- multinom(
  lawful_demonstration ~ middle_class * post_crisis + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health +
    relevel(factor(countryname), ref = "russia"),
  data = both_waves %>% filter(countryname %in% authoritarian_countries)
)

tab_model(multi_a5)

multi_a10 <- multinom(
  lawful_demonstration ~ middle_class * crisis_touched + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health +
    relevel(factor(countryname), ref = "russia"),
  data = both_waves %>% filter(countryname %in% authoritarian_countries)
)

tab_model(multi_a10)


## LAKKO 
multi_a5 <- multinom(
  strike ~ middle_class * post_crisis + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health +
    relevel(factor(countryname), ref = "russia"),
  data = both_waves %>% filter(countryname %in% authoritarian_countries)
)

tab_model(multi_a5)

multi_a10 <- multinom(
  strike ~ middle_class * crisis_touched + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health +
    relevel(factor(countryname), ref = "russia"),
  data = both_waves %>% filter(countryname %in% authoritarian_countries)
)

tab_model(multi_a10)

## VETOOMUSTEN ALLEKIRJOITTAMINEN
multi_a5 <- multinom(
  sign_petition ~ middle_class * post_crisis + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health +
    relevel(factor(countryname), ref = "russia"),
  data = both_waves %>% filter(countryname %in% authoritarian_countries)
)

tab_model(multi_a5)

multi_a10 <- multinom(
  sign_petition ~ middle_class * crisis_touched + redistribution + marketpref +
    age + agesq + male + econ_satisfaction + health +
    relevel(factor(countryname), ref = "russia"),
  data = both_waves %>% filter(countryname %in% authoritarian_countries)
)

tab_model(multi_a10)


#### TESTIT

## MULTIKOLLINEAARIASUUS
vif(m5)
vif(m10)

## ROC-ALUE

# m5
predicted_probs <- predict(m5, type = "response")
values <- m5$y

# AUC:n laskeminen
roc_curve <- roc(values, predicted_probs)
auc_value <- auc(roc_curve)

# AUC
print(auc_value)

# m10

predicted_probs <- predict(m10, type = "response")
values <- m10$y

# AUC:n laskeminen
roc_curve <- roc(values, predicted_probs)
auc_value <- auc(roc_curve)

# AUC
print(auc_value)

# PIIRRÄ KUVA
plot(roc_curve, col = "black", main = "ROC-käyrä")

## AIC (kaikille malleille)

# Esimerkiksi
AIC(m10)

## TJUR R2 (kaikille malleille)

# Esimerkiksi
r2_tjur(m10)
