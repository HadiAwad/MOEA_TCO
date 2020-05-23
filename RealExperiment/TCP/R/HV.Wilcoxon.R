write("", "RealExperiment/TCP/R/HV.Wilcoxon.tex",append=FALSE)
resultDirectory<-"RealExperiment/TCP/data"
latexHeader <- function() {
  write("\\documentclass{article}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\title{StandardStudy}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\usepackage{amssymb}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\author{A.J.Nebro}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\begin{document}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\maketitle", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\section{Tables}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
}

latexTableHeader <- function(problem, tabularString, latexTableFirstLine) {
  write("\\begin{table}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\caption{", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write(problem, "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write(".HV.}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)

  write("\\label{Table:", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write(problem, "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write(".HV.}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)

  write("\\centering", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\begin{scriptsize}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\begin{tabular}{", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write(tabularString, "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write(latexTableFirstLine, "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\hline ", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
}

printTableLine <- function(indicator, algorithm1, algorithm2, i, j, problem) { 
  file1<-paste(resultDirectory, algorithm1, sep="/")
  file1<-paste(file1, problem, sep="/")
  file1<-paste(file1, indicator, sep="/")
  data1<-scan(file1)
  file2<-paste(resultDirectory, algorithm2, sep="/")
  file2<-paste(file2, problem, sep="/")
  file2<-paste(file2, indicator, sep="/")
  data2<-scan(file2)
  if (i == j) {
    write("--", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  }
  else if (i < j) {
    if (is.finite(wilcox.test(data1, data2)$p.value) & wilcox.test(data1, data2)$p.value <= 0.05) {
      if (median(data1) >= median(data2)) {
        write("$\\blacktriangle$", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
}
      else {
        write("$\\triangledown$", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
}
}
    else {
      write("$-$", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
}
  }
  else {
    write(" ", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  }
}

latexTableTail <- function() { 
  write("\\hline", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\end{tabular}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\end{scriptsize}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
  write("\\end{table}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
}

latexTail <- function() { 
  write("\\end{document}", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
}

### START OF SCRIPT 
# Constants
problemList <-c("bash", "flex", "grep", "printtokens", "printtokens2", "sed") 
algorithmList <-c("NSGAII", "MOCell", "IBEA") 
tabularString <-c("lcc") 
latexTableFirstLine <-c("\\hline  & MOCell & IBEA\\\\ ") 
indicator<-"HV"

 # Step 1.  Writes the latex header
latexHeader()
tabularString <-c("| l | p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm } | p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm } | ") 

latexTableFirstLine <-c("\\hline \\multicolumn{1}{|c|}{} & \\multicolumn{6}{c|}{MOCell} & \\multicolumn{6}{c|}{IBEA} \\\\") 

# Step 3. Problem loop 
latexTableHeader("bash flex grep printtokens printtokens2 sed ", tabularString, latexTableFirstLine)

indx = 0
for (i in algorithmList) {
  if (i != "IBEA"){
    write(i , "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
    write(" & ", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)

    jndx = 0
    for (j in algorithmList) {
      for (problem in problemList) {
        if (jndx != 0) {
          if (i != j) {
            printTableLine(indicator, i, j, indx, jndx, problem)
          }
          else {
            write("  ", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
          } 
          if (problem == "sed"){
            if (j == "IBEA"){
              write(" \\\\ ", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
            } 
            else {
              write(" & ", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
            }
          }
     else {
    write("&", "RealExperiment/TCP/R/HV.Wilcoxon.tex", append=TRUE)
     }
        }
      }
      jndx = jndx + 1
}
    indx = indx + 1
  }
} # for algorithm

  latexTableTail()

#Step 3. Writes the end of latex file 
latexTail()

