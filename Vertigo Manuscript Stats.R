## Vertigo Manuscript Sensitivity, Specificity, PPV, NPV 


library('caret')

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

library('epiR')
rval_infarct <- epi.tests(infarct, conf.level = 0.95)
print("****INFARCT****")
print(rval_infarct)


rval_VAD <- epi.tests(VAD, conf.level = 0.95)
print("****VAD****")
print(rval_VAD)

