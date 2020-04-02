#'@description Get Latex code for tableaus
#'@title table2tex
#'@export
table2tex <- function(x,title=F){
  if (title==T)
  {
    cat("\\documentclass[UTF8]{article}\n")
    cat("\\usepackage{CTEX}\n")
    cat("\\usepackage[fleqn]{amsmath}\n")
    cat("\\usepackage{amssymb}\n")
    cat("\\usepackage[a4paper,left=10mm,right=10mm,top=15mm,bottom=15mm]{geometry}\n")
    cat("\\begin{document}\n")
  }
  if(class(x)=="list"){
    for(i in 1:length(x)){
      if(i>1){cat("\\\\")}
      cat("Table ")
      cat(i)
      cat("\n")
      t2t(x[[i]])
    }
  }
  else{
    t2t(x)
  }
  if(title==T)
  {
    cat("\n\n\n\\end{document}\n")
  }
}
