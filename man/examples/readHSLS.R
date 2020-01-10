\dontrun{
# use function default values at working directory
hsls <- readHSLS("C:/HSLS/2009")

# specify parameters with verbose output
hsls <- readHSLS(path="C:/HSLS/2009", 
                 filename = "hsls_16_student_v1_0.sav", 
                 forceReread = FALSE, 
                 verbose = TRUE)

# specify parameters silent output
hsls <- readHSLS(path="C:/HSLS/2009", 
                 filename = "hsls_16_student_v1_0.sav", 
                 forceReread = FALSE, 
                 verbose = FALSE)
}
