m1SummaryREF <- c(
  "Call:",
  "mixed.sdf(formula = mmat ~ itsex + (1 | idclass), data = usa8dat, ",
  "    weightVars = c(\"w1\", \"w2\"), weightTransformation = FALSE, ",
  "    verbose = FALSE)",
  "",
  "Formula: mmat ~ itsex + (1 | idclass)",
  "",
  "Plausible Values: 5",
  "Number of Groups:",
  " Level   Group n size mean wgt sum wgt",
  "     2 idclass    457      162   73870",
  "     1     Obs   8912      125 1112932",
  "",
  "Variance terms:",
  " Level    Group        Name Variance Std. Error Std.Dev.",
  "     2  idclass (Intercept)     3525      447.3     59.4",
  "     1 Residual                 2657       85.3     51.5",
  "",
  "Fixed Effects:",
  "            Estimate Std. Error t value",
  "(Intercept)   504.15       5.33   94.59",
  "itsexBOY        6.26       2.03    3.09",
  "",
  "Intraclass Correlation= 0.570"
)

m2SummaryRef <- c(
  "Call:",
  "mixed.sdf(formula = mmat ~ itsex + bsbgfbrn + (1 + bsbgfbrn | ",
  "    idclass), data = usa8dat, weightVars = c(\"w1\", \"w2\"), weightTransformation = FALSE, ",
  "    verbose = FALSE)",
  "",
  "Formula: mmat ~ itsex + bsbgfbrn + (1 + bsbgfbrn | idclass)",
  "",
  "Plausible Values: 5",
  "Number of Groups:",
  " Level   Group n size mean wgt sum wgt",
  "     2 idclass    456      160   73036",
  "     1     Obs   8702      126 1094457",
  "",
  "Variance terms:",
  " Level    Group        Name Variance Std. Error Std.Dev. Corr1",
  "     2  idclass (Intercept)     3441      420.2     58.7      ",
  "     2  idclass  bsbgfbrnNO      249      138.5     15.8 -0.25",
  "     1 Residual                 2621       81.5     51.2      ",
  "",
  "Fixed Effects:",
  "            Estimate Std. Error t value",
  "(Intercept)   506.16       5.44   93.07",
  "itsexBOY        6.53       2.01    3.25",
  "bsbgfbrnNO     -8.86       4.63   -1.91",
  "",
  "Intraclass Correlation= 0.585"
)
