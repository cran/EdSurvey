\name{NEWS}
\title{NEWS for EdSurvey}

\section{Changes in version 2.5.0}{
  \subsection{NEW DATA}{
    \itemize{
      \item Added support for the 2007-2008 Beginning Teacher Longitudinal Study (see \code{readBTLS}) for Waves 1 to 5.
    }
  }
  \subsection{DATA UPDATES}{
    \itemize{
      \item \code{readPISA} uses less memory when reading in PISA 2015 data. This is because it now uses the \code{haven} package's \code{read_sav} new \code{skip} and \code{n_max} arguments to read in portions of files. This should save time on read in.
      \item Other PISA files were changed to use base R read functions, this results in a different ordering of columns in 2012 and 2015.
      \item Some PISA 2003 variables were read in as an integer and are now in as a real values, mostly hours variables.
      \item After the OECD files were updated, \code{downloadPIAAC} and \code{readPIAAC} now properly use the 2012 USA file for Cycle 1.
    }
  }
  \subsection{TECHNICAL}{
    \itemize{
      \item To allow both 4.0 and 3.6 compatibility, an \code{edsurvey.data.frame.list} now stores \code{covs} as characters in preference to factors.
      \item \code{EdSurvey} is no longer imports \code{readr}, \code{rvest}, \code{stringi}, \code{stringr}, or \code{xml2}. In addition, \code{RColorBrewer} was imported but is now suggested.
      \item \code{ContourPlot} now uses \code{heat.colors} by default.
      \item Several data files were stored in text format in functions and are now stored in \code{inst/extdata} as \code{rds} files.
      \item The news for 2.4.0 was edited to note that the \code{gap} linking error in NAEP is only for mathematics and reading.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{lm.sdf} now rounds the printed coefficients matrix in the standard way.
      \item Fixed a bug in \code{edsurveyTable} that caused it to return incorrectly labeled output if multiple covariates were tabbed with weights.
      \item A broken link in the vignette was fixed.
    }
  }
}

\section{Changes in version 2.4.0}{
  \subsection{NEW DATA}{
    \itemize{
      \item Added support for the 2009 High School Longitudinal Study (see \code{downloadHSLS} and \code{readHSLS}). These do not support Restricted Use Data (RUD).
      \item Added support for the 2002 Education Longitudinal Study (see \code{downloadELS} and \code{readELS}).
      \item Added the ECLS Kindergarten Class of 1998-1999 Study. Thes datasets can be downloaded with \code{downloadECLS_K} and read in with \code{readECLS_K1998}. This was added in 2.3.0 but first added to the NEWS for 2.4.0.
      \item Added support for ePIRLS (see \code{read_ePIRLS} and \code{download_ePIRLS}). This was added in 2.3.0 but first added to the NEWS for 2.4.0.
   }
  }
  \subsection{DATA UPDATES}{
    \itemize{
      \item Added support for 2018 for the existing \code{readICILS} function. Thanks to Jeppe Bundsgaard of Aarhus University, Danish School of Education, for contributing the code for this.
      \item OECD moved the PIAAC data to a new location, and the \code{downloadPIAAC} function now uses the new URL.
      \item The PISA 2015 data cache could have been formed incorrectly; that is fixed. When PISA 2015 datasets are first used with 2.4 it will take time to re-cache the data. The process now also uses far less memory.
      \item The PISA data had incorrect PSU and stratum variables for most years. They are all fixed except for 2000, which we do not believe has a PSU variable on the file. Several strata have only one PSU, so the design still needs to be edited by the user to get correct Taylor series sampling variance estimates.
    }
  }
  \subsection{UPDATES}{
    \itemize{
      \item The \code{mixed.sdf} function now correctly aggregates results and has its methodology documented. It no longer supports binomial models and has several arguments deprecated as a result.
      \item The \code{gap} function argument \code{varMethod} is deprecated. The function uses only jackknife variance estimation.
      \item The \code{gap} function now accounts for linking error between NAEP paper and digitally based assessments for mathematics and reading.
      \item The \code{subset} function used to fail when a global variable shared a name with a column on the data; it now works.
      \item The \code{percentile} function has been updated to a formula that generates survey percentiles that are robust to transformation. For example, if the values are multiplied by a constant, then the percentiles move by that same constant.
      \item The \code{lm.sdf} function header now prints the number of plausible values used (when they are used) as well as the number of plausible values used in the sampling variance (\code{jrrIMax}).
      \item The \code{levelsSDF} function now makes more informative warnings when passed a \code{light.edsurvey.data.frame}.
      \item The \code{print} function for \code{cor.sdf} now prints a final new line.
      \item When \code{searchSDF} was called with more than one search string and the \code{levels} argument was set to \code{TRUE}, a useless warning was issued. The warning was removed.
      \item More \code{EdSurvey} functions wrap to the width of the console.
    }
  }
}

\section{Changes in version 2.3.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{rq.sdf} function adds quantile regression to the package. See the \code{rq.sdf} documentation for more information.
      \item The functions \code{getStratumVar} and \code{getPSUVar} were added to give users stratum and PSU variables for surveys and Taylor series analyses.
      \item The \code{summary2} function now accepts a vector of variables.
      \item The \code{searchSDF} function now accepts a vector \code{string} input so that results can be filtered.
      \item The formula for degrees of freedom when Taylor series variance estimation is used has been updated. The new formula is derived in the statistics vignette. See \url{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}.
    }
  }
  \subsection{NEW DATA}{
    \itemize{
      \item PISA 2015 is now supported.
    }
  }
}

\section{Changes in version 2.2.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{waldTest} function allows the user to test composite hypotheses--hypotheses with multiple coefficients involved--even when the data include plausible values.  Because there is no likelihood test for plausible values nor residuals, the Wald test fills the role of the likelihood ratio test, ANOVA, and F-test.
      \item The \code{mvrlm} function adds multivariate regression (a regression with multiple outcomes) to the package. See the \code{mvrlm} documentation for more information.
      \item Survey weighted mixed models can now be fit with the \code{mixed.sdf} function. Both linear and logistic models can be fit. These models are limited to 2 levels (one level with random effects).
      \item Regressions can now output standardized regression coefficients using \code{summary(myLm, src=TRUE)}. When the \code{lm.sdf} call includes \code{standardizeWithSamplingVar=TRUE} the standardized error of the standardized regression coefficient account for the sampling error and measurement error (when applicable). Otherwise, the standard deviations are assumed to be measured without error.
      \item Added \code{summary2} function to produce unweighted and weighted descriptive statistics of a variable in \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}
      \item Added $ variable access to edsurvey.data.frame. e.g. sdf$dsex
      \item Updated covariance matrix estimation in the \code{vcov} function for \code{lm.sdf} and \code{glm.sdf} to work when \code{varEstInputs} was not returned
      \item Added covariance matrix estimation for \code{lm.sdf} when Taylor series variance estimation was used.
      \item Added \code{rebindAttributes} function to make dplyr interaction more smooth. See the \code{rebindAttributes} documentation for an example.
      \item When printing an \code{edsurvey.data.frame} it now says the survey, year, subject, and country at the top. The dimensions are moved down.
      \item In \code{gap}, achievement levels can be specified with partial matches. This helps out when achievement levels have long names.
    }
  }
  \subsection{NEW DATA}{
    \itemize{
      \item Added the ECLS Kindergarten Class of 2010-2011 Study. Longitudinal datasets can be downloaded with \code{downloadECLS_K} and read in with \code{readECLS_K2011}.
      \item PIRLS 2016 is now supported.
    }
  }
  \subsection{NEW STATISTICS}{
    \itemize{
      \item Added \code{returnNumberOfPSU} in \code{achievenemtLevels}, \code{percentile}, \code{lm.sdf}, and \code{gap} to report the number of primary sampling units (PSUs) used to calculate a statistic.
      \item Added \code{oddsRatio} helper function for logit results to show odds ratios.
      \item When running a Pearson correlation on a discrete variable, \code{cor.sdf} function by default condenses the occupied response codes to be consecutive integers. This can now be turned off by setting the \code{condenseLevels} argument to \code{FALSE} so that the code book levels are used instead.
    }
  }
  \subsection{UPDATES}{
    \itemize{
      \item The \code{glm.sdf} function now uses the \code{glm2} package to fit models. This package converges on a broader class of models.
      \item The \code{EdSurvey} package nolonger sets the number of threads used by the required \code{data.table} package to one when EdSurvey is being attached. The issue is now fixed in \code{data.table}.
      \item Across the download functions, when a file is missing the warning text was homogenized. Additionally, all download functions now support a \code{verbose} argument that can be used to make downloads silent.
      \item The \code{lm.sdf} and \code{glm.sdf} functions now accept formulas that use the \code{I()} function or other unevaluated expressions to the left of the tilda. Previously only a single variable could be named.
      \item \code{recode.sdf} now checks that each recode has only a \code{to} and \code{from} in it.
      \item The \code{edsurveyTable} function now works without RHS variable, allowing the formula \code{y ~ 1} to return overall means.
      \item The \code{percentile} function used to produce output even if asked to produce a percentile outside of the valid range (0 to 100). Now it prints out message if users input at least percentile outside of the valid range, and stops if all percentiles are invalid.
      \item The \code{getData} function now removes rows with omitted levels after being recoded.
      \item All SPSS (.sav) file reads using the \code{haven} package set the \code{user_na = TRUE} flag to ensure no defined missing/omitted values are automatically converted to \code{NA} values prematurely.
      \item For consistency with other download functions, \code{downloadPISA} now uses a \code{years} argument instead of a \code{year} argument. 
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Running an \code{edsurveyTable} on an \code{edsurvey.data.frame.list} used to have the potential to create invalid (unprintable) output if the factor levels did not agree on every element of the \code{edsurvey.data.frame.list}. They now return printable output.
      \item An \code{edsurveyTable} could produce a standard error when there was data from only one stratum. It now produces an \code{NA} standard error.
    }
  }
}

\section{Changes in version 2.0.0}{
  \subsection{NEW DATA}{
    \itemize{
      \item Works with the Trends in International Mathematics and Science Study (TIMSS), TIMSS Advanced, the Progress in International Reading Literacy Study (PIRLS), 
      and the International Computer and Information Literacy Study (ICILS), International Civic and Citizenship Education Study (ICCS), the Civic Education Study (CivEd), 
      the Program for International Student Assessment (PISA), the Program for the International Assessment of Adult Competencies (PIAAC), and the Teaching and Learning International Survey (TALIS).
      \item International datasets can be downloaded with \code{downloadTIMSS}, \code{downloadTIMSSAdv}, 
            \code{downloadPIRLS}, \code{downloadICILS}, \code{downloadICCS}, 
	    \code{downloadCivEDICCS}, \code{downloadPISA}, \code{downloadPIAAC}, \code{downloadTALIS}.
      \item International datasets can be loaded with \code{readTIMSS}, \code{readTIMSSAdv}, 
            \code{readPIRLS}, \code{readICILS}, \code{readICCS}, 
	    \code{readCivEDICCS}, \code{readPISA}, \code{readPIAAC}, \code{readTALIS}.
    }
  }
  \subsection{NEW STATISTICS}{
    \itemize{
      \item Added \code{logit.sdf} and \code{probit.sdf} functions with support
            for survey item responses.
      \item Added \code{gap} code{gap} that compares the average, percentile, achievement level, or percentage of survey responses between two groups that potentially share members.
      \item Added \code{percentile} that calculates the percentiles of a numeric variable.
      \item Added \code{showCodebook} that retrieves variable names, variable labels, and value labels for an \code{edsurvey.data.frame}, \code{light.edsurvey.data.frame}, or \code{edsurvey.data.frame.list}.
    }
  }
  \subsection{UPDATES}{
    \itemize{
      \item Redesigned \code{achievementLevels},  \code{edsurveyTable} function for
      significant faster computation with much smaller memory footprint. We also
      made error messages and outputs more informative.
      \item More informative error message and output for \code{cor.sdf},
      \code{levelsSDF}, \code{getPlausibleValue}, \code{print.edsurvey.data.frame},
      \code{searchSDF}, \code{showPlausibleValues}, \code{showWeights}, and \code{getData}.
      \item \code{lm.sdf} and \code{glm.sdf} are now S3 method extended from
      \code{stats::lm} so users can call the function using \code{lm} and \code{glm}.
      \item Added the \code{contourPlot} for regressions diagnostic.
      \item Added the \code{recode.sdf} function for recoding levels within variables.
      \item Added the \code{rename.sdf} function for modifying variable names.
      \item Added the \code{append.edsurvey.data.frame.list} function to return
      a list of sdfs from either an edsurvey.data.frame.list or a single edsurvey.data.frame.
    }
  }
  \subsection{DOCUMENTATION}{
    \itemize{
      \item Manual documentation was refreshed.
      \item Moved vignettes to AIR webiste at https://www.air.org/project/nces-data-r-project-edsurvey or see links in the vignette inlcuded in this package.
      \item Added a new vignette on ``Exploratory Data Analysis on NCES Data'' provides examples of conducting exploratory data analysis on NAEP data.
      \item Added new vignette on ``Calculating Adjusted p-Values From EdSurvey Results'' to the AIR website describing the basics of adjusting p-Values to account for multiple comparisons.
      \item Added new vignette on ``Using EdSurvey to Analyze TIMSS Data'' to the AIR website describing; an introduction to the methods used in analysis of large-scale educational assessment programs such as Trends in International Mathematics and Science Study (TIMSS) using the EdSurvey package. The vignette covers topics such as preparing the R environment for processing, creating summary tables, running linear regression models, and correlating variables.
      \item Added new vignette on ``Using EdSurvey for Trend Analysis'' to the AIR website describing the methods used in the EdSurvey package to conduct analyses of statistics that change over time in large-scale educational studies.
      \item Added new vignette on ``Producing LaTeX Tables From edsurveyTable Results With edsurveyTable2pdf'' to the AIR website detailing the creation of pdf summary tables from summary results using the edsurveyTable2pdf function.
      \item Added new methodology documentation on ``Methods Used for Gap Analysis in EdSurvey'' to the AIR website convering the methods comparing the gap analysis results of the EdSurvey package to the NAEP Data Explorer.
      \item Added new methodology documentation on ``Methods Used for Estimating Percentiles in EdSurvey'' to the AIR website describing the methods used to estimate percentiles.
      \item Added new methodology documentation on ``Weighted and Unweighted Correlation Methods for Large-Scale Educational Assessment: wCorr Formulas'' to the AIR website detailing the methodology used by the wCorr R package for computing the Pearson, Spearman, polyserial, and polychoric correlations, with and without weights applied. See \url{https://www.air.org/resource/weighted-and-unweighted-correlation-methods-large-scale-educational-assessment-wcorr}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed connection issue associated with closing connections to an \code{LaF}.
    }
  }
}

\section{Changes in version 1.0.6}{
  \itemize{
    \item readNAEP function now works on a case sensitive file system.
    \item Vignettes now should appear in numerical order on CRAN.
    \item Examples now name most arguments.
    \item Vignettes now name most argument.
    \item some print functions had number of plausible values added.
  }
}
