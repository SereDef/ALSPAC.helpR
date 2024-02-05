## code to prepare `alspac_metadata` dataset (add ages)

library(ALSPAC.helpR)

# Read in the file
data <- load_alspac(
  lower.case = TRUE,
  keep.value.labels = TRUE,
  load.metadata = FALSE)


comp_age <- function(name, df) {

  sums <- sapply(df, function(v) {
    if (grepl('/',v)) { v = stringr::str_split(v, '/')[[1]][1] }
    if (v %in% names(data)){
      droplevels(data[,v])
      age <- as.numeric(levels(data[,v]))[data[,v]]

      # Weeks or years
      if (v %in% c('a902','b924','c991','d990','e699',
                   'pa900','pb900','pc992',
                   'ka497',
                   'td1150','te8030','ypj7500','ypk9510','ccu9991'
                   )) { div = 1 } else { div = 12 }

      s = round(summary(age/div),2)
    } else {
      # print(v)
      s = rep(NA, 7)
    }
  })

  nages <- rbind(df, sums)

  message(name)
  print(t(nages['Median', ]))

  cats <- stringr::str_split(name, '_')[[1]]

  nages <- rbind(nages, rep(cats[1], ncol(nages)), rep(cats[2], ncol(nages)))

  row.names(nages) <- c("age_var","min","quant1","median","mean","quant3","max","NAs","cat1","cat2")

  return(nages)
}

age_alspac <- cbind(
  comp_age('Quest_Mother', data.frame(
  # QUEST MOTHER
  'a'='a902', # Gestation on completion quesitonnaire
   # a901 # Mother's age on completion of questionnaire [ Enrolment ]
  'b'='b924', # Gestation on completion
   # b925 # Age at completion [ 18 weeks gestation ]
  'c'='c991', # Gestation at completion
   # c994 # DV: Maternal age in years at completion of 'Your Pregnancy' questionnaire [ 32 weeks gestation ]
  'd'='d990/d991', # Gestation on completion of questionnaire / Baby's age(wks) - questionnaire done postnatally
   # d994 DV: F1 F2: Maternal age in years at completion of  'About Yourself' questionnaire [ pregnancy or after birth ]
  'e'='e699',  # Weeks since delivery
   # e694 DV: H1 H2: Maternal age in years at completion of 'Me and My Baby' questionnaire [ 8 weeks ; 2m ]
  'f'='f993',  # Age of child at completion in months # CAREFUL f7,8,9
  'g'='g990',  # Age of child at completion (months)
  'h'='h991a', # DV: Age of child at completion (months)
  'k'='k9991a', # DV: Age of study child at completion (months)
  'l'='l9991a', # DV: Age of study child at completion of questionnaire (months)
  'm'='m9991a', # DV: Age of study child at completion (months)
  'n'='n9991a', # DV: Age of study child at completion (months)
  'p'='p9991a', # DV: Age of study child at completion (months)
  'q'='q9991a', # DV: Age of study child at completion (months)
  'r'='r9991a', # DV: Age of study child at completion (months)
  's'='s9991a', # DV: Age of study child at completion (months)
  't'='t9991a'  # DV: Age of study child at completion (months)
  )),
  comp_age('Quest_Partner', data.frame(
  # QUEST PARTNER
  'pa'='pa900/pa901', # Gestation on completion of questionnaire / Baby's age(wks) - ?aire done postnatally
   # pa910 # Age of partner at completion of questionnaire [ Enrolment ]
  'pb'='pb900/pb901', # Gestation on completion of questionnaire /  Baby's age(wks) - ?aire done postnatally
   # pb910 # Age of PTNR at completion of questionnaire [ 18 weeks gestation ]
  'pc'='pc992', # Baby's age (wks) on completion of questionnaire
   # pc993 DV: Age of partner person at completion (years) [ 32 weeks gestation ]
  'pd'='pd992',   # Age of Child at Completion of Questionnaire (in months)
  'pe'='pe990',   # DV: Age in months of study toddler at completion of 'Toddler in the Home'
  'pf'='pf9991a', # DV: Age of study child at completion (months)
  'pg'='pg9991a', # DV: Age of study child at completion (months)
  'ph'='ph9991a', # DV: Age of study child at completion (months)
  'pj'='pj9991a', # DV: Age of study child at completion of questionnaire (months)
  'pk'='pk9991a', # DV: Age of study child at completion (months)
  'pl'='pl9991a', # DV: Age of study child at completion (months)
  'pm'='pm9991a', # DV: Age of study child at completion (months)
  'pp'='pp9991a', # DV: Age of study child at completion (months)
  'pq'='pq9991a'  # DV: Age of study child at completion (months)
  )),
  comp_age('Quest_Child Based', data.frame(
  # QUEST CHILD BASED
  'ka'='ka497',   # Age (weeks) questionnaire completed
  'kb'='kb879a',  # Age of child at completion (months)
  'kc'='kc997',   # Age in months at completion
  'kd'='kd990',   # Age at completion (months)
  'ke'='ke995',   # Age in months at completion
  'kf'='kf999',   # Age of CH (mths) at completion of questionnaire
  'kg'='kg998a',  # DV: Age of child at completion (months)
  'kj'='kj999a',  # Age at completion (in months)
  'kk'='kk998a',  # DV: Age of child at completion (months)
  'kl'='kl991a',  # DV: Age of study child at completion (months)
  'km'='km9991a', # DV: Age of study child at completion (months)
  'kn'='kn9991a', # DV: Age of study child at completion (months)
  'kp'='kp9991a', # DV: Age of study child at completion (months)
  'kq'='kq998a',  # DV: Age of child at completion (months)
  'kr'='kr991a',  # DV: Age of child at completion (months)
  'ks'='ks9991a', # DV: Age of study child at completion (months)
  'kt'='kt9991a', # DV: Age of study child at completion (months)
  'ku'='ku991a',  # DV: Age of study child at completion (months)
  'kv'='kv9991a', # DV: Age of study child at completion (months)
  'kw'='kw9991a', # DV: Age of study child at completion (months)
  'ta'='ta9991a', # DV: Age of study child at completion (months)
  'tb'='tb9991a', # DV: Age of study child at completion (months)
  'tc'='tc9991a', # DV: Age of study teenager at completion (months)
  'td'='td1150', # YP's age in years at time questionnaire was completed
  'te'='te8030/te8040', # YP's age in years when questionnaire completed / months
 'txa'='txa991a'  # DV: Age of study child at completion (months)
 )),
 comp_age('Quest_Child Completed', data.frame(
    # QUEST CHILD COMPLETED
   'ccaa'='ccaa991a', # DV: Age of study child at Q receipt (months), CCAA file
   'ccab'='ccab991a', # DV: Age of study child at Q receipt (months), ccab file
   'ccac'='ccac991a', # DV: Age of study child at Q receipt (months), ccac file
   'ccad'='ccad991a', # DV: Age of study child at Q receipt (months), CCAD file
   'ccae'='ccae991a', # DV: Age of study child at Q receipt (months), CCAE file
   'ccaf'='ccaf991a', # DV: Age of study child at Q receipt (months), CCAF file
    'ccb'='ccb999a',  # DV: Age of study child at receipt of questionnaire (months)
    'ccc'='ccc011a',  # DV: Age of study child at receipt of questionnaire (months)
    'ccd'='ccd011a',  # DV: Age of study child at receipt of questionnaire (months)
    'cce'='cce011a',  # DV: Age of study child at receipt of questionnaire (months)
    'ccf'='ccf991a',  # DV: Age of study child at completion (months)
    'ccg'='ccg011a',  # DV: Age of study child at receipt of questionnaire (months)
    'cch'='cch011a',  # DV: Age of study child at receipt of questionnaire (months)
    'ccj'='ccj991a',  # DV: Age of study child at completion (months)
    'cck'='cck991a',  # DV: Age of study child at completion (months)
    'ccl'='ccl991a',  # DV: Age of study child at completion (months)
    'ccm'='ccm991a',  # DV: Age of study child at completion (months)
    'ccn'='ccn991a',  # DV: Age of study child at completion (months)
    'ccp'='ccp991a',  # DV: Age of study child at completion (months)
    'ccq'='ccq991a',  # DV: Age of respondent at completion (months)
    'ccr'='ccr991a',  # DV: Age of study child at completion (months)
    'ccs'='ccs9991a', # DV: Age of study child at completion (months)
    'cct'='cct9991a', # DV: Age of study child at completion (months)
   'ccxa'='ccxa991a', # DV: Age of study child at completion (months)
   'ccxb'='ccxb991a', # DV: Age of study child at completion (months)
   'ccxd'='ccxd006/ccxd005', # DV: Age of study child at completion (months) / years
   'ccxc'='ccxc004', # DV: Age in months of YP at completion of questionnaire
   'ccxf'='ccxf9992', # DV: Age of respondent at completion (months)
   'ccu'='ccu9991', # DV: Age of respondent at completion (years)
   'ypa'='ypa9020',   # DV: Age at completion (in months)
   'ypb'='ypb9992',   # DV: Respondent age at completion (months)
   'ypc'='ypc2650',   # DV: Age of study young person at completion (months)
   'ypd'='ypd9650',   # DV: Age of study young person at completion (months)
   'ype'='ype9660/ype9650', # DV: Age of study young person at completion (months) / years
   'ypf'='ypf9520/ype9510', # DV: Age of study young person at completion (months) / years
   'ypg'='ypg8000',   # DV: Age of study young person at completion (months)
   'yph'='yph9520/yph9510', # DV: Age of study young person at completion (months) / years
   'ypj'='ypj7500',   # DV: Age of study young person at completion (years)
   'ypk'='ypk9510',   # DV: Participant's age in years when questionnaire completed
   'fjga'='fjga005'  # DV: Age in months of YP at completion: F17 gambling

   )),
 comp_age('Quest_Puberty', data.frame(
  # QUEST PUBERTY
  'pub1'='pub195',  # Age of child at completion (months)
  'pub2'='pub295',  # Age of child at completion (months)
  'pub3'='pub397a', # Age of child at completion (months)
  'pub4'='pub497a', # Age of child at completion (months)
  'pub5'='pub597a', # Age of child at completion (months)
  'pub6'='pub697a', # Age of child at completion (months)
  'pub7'='pub797a', # Age of child at completion (months)
  'pub8'='pub897a', # Age of child at completion (months)
  'pub9'='pub997a'  # Age of child at completion (months)
  )),
 comp_age('Clinic_Child', data.frame(
  # CLINIC CHILD
  'f7'='f7003c',  # Age (months) at Focus @ 7 visit
  'f8'='f8003c/f8006c',  # Age (months) at Focus @ 8 visit / Age (months) at Focus @ 8 revisit
  'f9'='f9003c/f9006c',  # Age (months) at F9 visit / Age (months) at F9 revisit
  'fd'='fd003c',  # Age (months) at visit: F10
  'fe'='fe003c',  # Age (months) at F11+ visit
  'ff'='ff0011a', # DV: Age of study child at attendance (months) [ TF1 ]
  'fg'='fg0011a', # DV: Age of study child at attendance (months): TF2
  'fh'='fh0011a/fh5307', # DV: Age of study child at attendance (months): TF3 / Age in months of YP at clinic visit: TF3
  'fj'='fj003a',  # Age in months at clinic visit: TF4
  'fk'='fkar0010' # Age at clinic visit (in months): F@24
  ))
)
# Order by age
# age_alspac <- age_alspac[,order(as.numeric(age_alspac['median',]))]

alspac_metadata[,c('median_age','age_range','age_variable')] <- ''

for (v in names(age_alspac)) {

  message(v)

  subset <- c(grepl(paste0('^',v), alspac_metadata$name, ignore.case=TRUE)
  & grepl(age_alspac['cat1',v], alspac_metadata$cat1)
  & grepl(age_alspac['cat2',v], alspac_metadata$cat2))

  cat(sum(subset), '\n')

  if (age_alspac['age_var',v] %in% c('a902','b924','c991','d990',
                                     'pa900','pb900')) {
    scale = 'weeks gestation'
  } else if (age_alspac['age_var',v] %in% c('e699','pc992','ka497')) {
    scale = 'weeks'
  } else {
    scale = 'years'
  }

  alspac_metadata[subset, 'median_age'] <- ifelse(!is.na(age_alspac['median',v]),
                                                  paste(age_alspac['median',v], scale), NA)
  alspac_metadata[subset, 'age_range'] <- ifelse(!is.na(age_alspac['min',v]),
                                                 paste0('[',age_alspac['min',v], ' - ', age_alspac['max',v],']'), NA)
  alspac_metadata[subset, 'age_variable'] <- age_alspac['age_var',v]


}

usethis::use_data(alspac_metadata, overwrite = TRUE)


# -------- Inspect leftovers ----------------------------
# left <- alspac_metadata[is.na(alspac_metadata$age_var),]
# summary(as.factor(paste(left$cat1, left$cat2)))
#
# check <- left[left$cat2=='Child', ]
# check <- left[left$cat2=='Puberty', ]
#
# summary(as.factor(alspac_metadata$median_age))

# Add ages early clinics visits
# cv <- data.frame(
#   'cf010'='4 mth',  # Age (wks) at 4 mth # cf010a = Age (days) at 4 mth\
#   'cf011'='8 mth',  # Age (wks) at 8 mth
#   'cf012'='12 mth', # Age (wks) at 12 mth
#   'cf013'='18 mth', # Age (wks) at 18 mth
#   'cf014'='25 mth', # Age (wks) at 25 mth
#   'cf015'='31 mth', # Age (wks) at 31 mth
#   'cf016'='37 mth', # Age (wks) at 37 mth
#   'cf017'='43 mth', # Age (wks) at 43 mth
#   'cf018'='49 mth', # Age (wks) at 49 mth
#   'cf019'='61 mth'  # Age (wks) at 61 mth
# )
#
# for (a in names(cv)) {
#   print(median(as.numeric(data[,a]),na.rm=TRUE))
# }


