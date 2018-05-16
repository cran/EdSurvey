\name{NEWS}
\title{NEWS for EdSurvey}

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
