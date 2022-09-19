## Vertigo Manuscript Statistics

library('caret')
library('epiR')

data_tab <- as.table(rbind(c(534),
                           c(5),
                           c(23),
                           c(5),
                           c(176),
                           c(4),
                           c(9),
                           c(8),
                           c(0)))

dimnames(data_tab) <- list(Outcome = c("Normal",
                                       "Infarct and VAD",
                                       "Infarct",
                                       "VAD",
                                       "Incidental",
                                       "False Positive Infarct",
                                       "False Positive VAD",
                                       "False Negative Infarct",
                                       "False Negative VAD"),
                           "Number of Studies")

infarct <- as.table(rbind(c(data_tab["Infarct and VAD",1] + data_tab["Infarct",1], c(data_tab["False Positive Infarct",1])),
                          c(data_tab["False Negative Infarct",1], data_tab["Normal",1] + 
                                                                  data_tab["Incidental",1] + 
                                                                  data_tab["VAD",1] + 
                                                                  data_tab["False Negative VAD",1] +
                                                                  data_tab["False Positive VAD",1])))
dimnames(infarct) <- list(Predicted = c("Infarct", "No Infarct"),
                          Actual = c("Infarct", "No Infarct"))

VAD <- as.table(rbind(c(data_tab["Infarct and VAD",1] + data_tab["VAD",1], c(data_tab["False Positive VAD",1])),
                      c(data_tab["False Negative VAD",1], data_tab["Normal",1] + 
                                                          data_tab["Incidental",1] + 
                                                          data_tab["Infarct",1] + 
                                                          data_tab["False Negative Infarct",1] +
                                                          data_tab["False Positive Infarct",1])))
dimnames(VAD) <- list(Predicted = c("VAD", "No VAD"),
                      Actual = c("VAD", "No VAD"))

negPredValue(infarct, negative = rownames(infarct)[2], prevalence = NULL)
posPredValue(infarct, positive = rownames(infarct)[1], prevalence = NULL)

negPredValue(VAD, negative = rownames(VAD)[2], prevalence = NULL)
posPredValue(VAD, positive = rownames(VAD)[1], prevalence = NULL)

## Calculate Sensitivity, Specificity, PPV, NPV for Infarction 95%CI
rval_infarct <- epi.tests(infarct, conf.level = 0.95)
print("****INFARCT****")
print(rval_infarct)

## Calculate Sensitivity, Specificity, PPV, NPV for VAD 95%CI
rval_VAD <- epi.tests(VAD, conf.level = 0.95)
print("****VAD****")
print(rval_VAD)

## Calculate OR for the presence of dysdiadochokinesia, dysarthria, and dysmetria
## in patients with infarction and/or VAD

sx_table <- as.table(rbind(c(2, 6),
                           c(3, 19),
                           c(1, 10)))
dimnames(sx_table) <- list(Symptoms = c("Dysdiadochokinesia", "Dysarthria", "Dysmetria"),
                           Frequency = c("VAD/Infact", "No VAD/Infarct"))

dysdiadochokinesia <- as.table(rbind(c(sx_table["Dysdiadochokinesia",1], sx_table["Dysdiadochokinesia",2]),
                                     c(data_tab["Infarct",1] +
                                         data_tab["Infarct and VAD",1] +
                                         data_tab["VAD",1] +
                                         data_tab["False Negative VAD",1] +
                                         data_tab["False Negative Infarct",1] - sx_table["Dysdiadochokinesia",1],
                                       data_tab["Normal",1] +
                                         data_tab["Incidental",1] +
                                         data_tab["False Positive VAD",1] +
                                         data_tab["False Positive Infarct",1] - sx_table["Dysdiadochokinesia",2])))
dimnames(dysdiadochokinesia) <- list(Dysdiadochokinesia = c("Present", "Absent"),
                                     InfarctVAD = c("Present", "Absent"))
epi.2by2(dat = dysdiadochokinesia, method = "cohort.count", digits = 2, conf.level = 0.95,
         units = 100, interpret = FALSE, outcome = "as.columns")

dysarthria <- as.table(rbind(c(sx_table["Dysarthria",1], sx_table["Dysarthria",2]),
                                     c(data_tab["Infarct",1] +
                                         data_tab["Infarct and VAD",1] +
                                         data_tab["VAD",1] +
                                         data_tab["False Negative VAD",1] +
                                         data_tab["False Negative Infarct",1] - sx_table["Dysarthria",1],
                                       data_tab["Normal",1] +
                                         data_tab["Incidental",1] +
                                         data_tab["False Positive VAD",1] +
                                         data_tab["False Positive Infarct",1] - sx_table["Dysarthria",2])))
dimnames(dysarthria) <- list(Dysarthria = c("Present", "Absent"),
                             InfarctVAD = c("Present", "Absent"))
epi.2by2(dat = dysarthria, method = "cohort.count", digits = 2, conf.level = 0.95,
         units = 100, interpret = FALSE, outcome = "as.columns")

dysmetria <- as.table(rbind(c(sx_table["Dysmetria",1], sx_table["Dysmetria",2]),
                             c(data_tab["Infarct",1] +
                                 data_tab["Infarct and VAD",1] +
                                 data_tab["VAD",1] +
                                 data_tab["False Negative VAD",1] +
                                 data_tab["False Negative Infarct",1] - sx_table["Dysmetria",1],
                               data_tab["Normal",1] +
                                 data_tab["Incidental",1] +
                                 data_tab["False Positive VAD",1] +
                                 data_tab["False Positive Infarct",1] - sx_table["Dysmetria",2])))
dimnames(dysmetria) <- list(Dysmetria = c("Present", "Absent"),
                            InfarctVAD = c("Present", "Absent"))
epi.2by2(dat = dysmetria, method = "cohort.count", digits = 2, conf.level = 0.95,
         units = 100, interpret = FALSE, outcome = "as.columns")

