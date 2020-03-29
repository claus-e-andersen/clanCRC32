#' @title CRC32 validation of given file
#' @description
#' Each line is terminated with a CRC32 code. This function
#' eturns if all lines in the file can be validated.
#' @usage
#' require(digest)
#'
#' CRC32.validation("C:\\data\\projects\\R\\work\\exp100830-cobalt-calibrations\\exp100914.txt")
#'
#' @name CRC32.validation
#' @author Claus E. Andersen
#' @return a list with  ok status (TRUE/FALSE), N.ok.lines  (number of lines in file that are validated) and
#' N.not.ok.lines (number of lines that are not ok).
#' @param file.name e.g. "c:\\data\\ME30-data\\catest20006a.txt"
#' @param delimiter  e.g. ";"
#' @param  stop.if.fault (TRUE or FALSE)
#' @export CRC32.validation
CRC32.validation <- function(file.name="c:\\data\\ME30-data\\catest20006a.txt",
                             delimiter=";", stop.if.fault=!TRUE){
  # Name: Claus E. Andersen
  # Created: January 13, 2015
  # Revised: January 13, 2015
  # Returns if all lines in the file can be validated.

  print("CRC32 validation.")

  txt <- readLines(file.name)

  NN <- length(txt)
  ok <- rep(FALSE,NN)
  for(i in 1:NN){
    sss <- txt[i]
    #Find the character that ends the line and that is the begining
    # of a sequence of ; and some digits (o to 9). We assume this is
    # a CRC32 code.
    ii <- regexpr(";[0-9]*$",sss)
    x.stuff <- substring(sss,1,ii-1)
    x.code <- substring(sss,ii+1,99999999)
    dd <- digest(x.stuff,algo='crc32',ascii=FALSE,serialize=FALSE)
    d.code <- convert.hex(dd)
    if(!is.na(x.code) & !is.na(d.code)){
      if(!(x.code==d.code)){
        print("")
        print("There is a problem in this line:")
        print(paste("Line no = ",i))
        print(sss)
        print(paste("file.code (from the software that created the file e.g. MELab or Labview) =",x.code,"and digest.code (R) =",d.code))
        print("CRC32 codes do not match. Has anything changed in this line?")
        if(stop.if.fault){stop("I'll quit!")}
      } else
      {ok[i] <- TRUE}
    }
  }

  print(paste("Lines that are ok = ",sum(ok)))
  print(paste("Lines that are NOT ok = ",sum(!ok)))
  print(paste('File that has been analyzed:',file.name))
  return(list(ok=sum(!ok)==0,N.ok.lines=sum(ok),N.not.ok.lines=sum(!ok)))
} # End CRC32.validation



