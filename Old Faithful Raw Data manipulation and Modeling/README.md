# This is Old Faithful data manipulation project
> regex expression /rmd report/ rmd latex
### 1. A collaboration report
### 2. Code in .rmd (can be used to generate the report pdf)
### 3. A html version report
Made use of regex expression to conduct data cleaning,
Data was collected from [geyserstudy](http://www.geyserstudy.org/), code for collect the original data as follows

```r
if (FALSE) {
  # Only run this if you want to get the files from the web.
  of <- ""
  for (year in c(1970:1973, 1975:1979, 1981, 1992:2012)) { # JAH was 2010
    
    file <- paste0("http://www.geyserstudy.org/ofvclogs/log", year, ".txt")
    
    # Read data in string form, one element in string vector
    # for each row of file
    
    thisof <- scan(file, what="", sep="\n")
    of <- c(of, thisof)
  }
  writeLines(of, "RawStevens_1970_2012.txt")
}
```
This project is inspired by the work of Prof.J.A.Hartigan from Department of Statistics and Data Science at Yale University [old faithful](http://www.stat.yale.edu/~jah49/Pictures_in_R/Fickle_Old_Faithful/OldFaithful.pdf) 
