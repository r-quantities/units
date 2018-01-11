# Build against mingw-w64 build of udunits
if(!file.exists("../windows/udunits-2.2.20/include/udunits.h")){
  if(getRversion() < "3.3.0") setInternet2()
  download.file("https://github.com/rwinlib/udunits/archive/v2.2.20.zip", "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
