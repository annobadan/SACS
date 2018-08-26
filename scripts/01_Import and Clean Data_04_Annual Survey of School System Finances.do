*****************************************************
***
*** Import Excel files and save as .dta files
***
*** 20180825 JoonHo Lee
***
***

*****************************************************
***
*** (1) Individual Unit Tables
***
***

*** Set working directory
cd "D:\Data\LCFF\Financial\Annual Survey of School System Finances\Individual Unit Tables"


*** Import excel files and save as .dta files
clear
import excel "elsec00t.xls", firstrow
saveold "elsec00t.dta", replace version(12)

clear
import excel "elsec01t.xls", firstrow
saveold "elsec01t.dta", replace version(12)

clear
import excel "elsec02t.xls", firstrow
saveold "elsec02t.dta", replace version(12)

clear
import excel "elsec03t.xls", firstrow
saveold "elsec03t.dta", replace version(12)

clear
import excel "elsec04t.xls", firstrow
saveold "elsec04t.dta", replace version(12)

clear
import excel "elsec05t.xls", firstrow
saveold "elsec05t.dta", replace version(12)

clear
import excel "elsec06t.xls", firstrow
saveold "elsec06t.dta", replace version(12)

clear
import excel "elsec07t.xls", firstrow
saveold "elsec07t.dta", replace version(12)

clear
import excel "elsec08t.xls", firstrow
saveold "elsec08t.dta", replace version(12)

clear
import excel "elsec09t.xls", firstrow
saveold "elsec09t.dta", replace version(12)

clear
import excel "elsec10t.xls", firstrow
saveold "elsec10t.dta", replace version(12)

clear
import excel "elsec11t.xls", firstrow
saveold "elsec11t.dta", replace version(12)

clear
import excel "elsec12t.xls", firstrow
saveold "elsec12t.dta", replace version(12)

clear
import excel "elsec13t.xls", firstrow
saveold "elsec13t.dta", replace version(12)

clear
import excel "elsec14t.xls", firstrow
saveold "elsec14t.dta", replace version(12)

clear
import excel "elsec15t.xls", firstrow
saveold "elsec15t.dta", replace version(12)

clear
import excel "elsec16t.xls", firstrow
saveold "elsec16t.dta", replace version(12)



*****************************************************
***
*** (2) All data items
***
***

*** Set working directory
cd "D:\Data\LCFF\Financial\Annual Survey of School System Finances\All Data Items"


*** Import excel files and save as .dta files
clear
import excel "elsec00.xls", firstrow
saveold "elsec00.dta", replace version(12)

clear
import excel "elsec01.xls", firstrow
saveold "elsec01.dta", replace version(12)

clear
import excel "elsec02.xls", firstrow
saveold "elsec02.dta", replace version(12)

clear
import excel "elsec03.xls", firstrow
saveold "elsec03.dta", replace version(12)

clear
import excel "elsec04.xls", firstrow
saveold "elsec04.dta", replace version(12)

clear
import excel "elsec05.xls", firstrow
saveold "elsec05.dta", replace version(12)

clear
import excel "elsec06.xls", firstrow
saveold "elsec06.dta", replace version(12)

clear
import excel "elsec07.xls", firstrow
saveold "elsec07.dta", replace version(12)

clear
import excel "elsec08.xls", firstrow
saveold "elsec08.dta", replace version(12)

clear
import excel "elsec09.xls", firstrow
saveold "elsec09.dta", replace version(12)

clear
import excel "elsec10.xls", firstrow
saveold "elsec10.dta", replace version(12)

clear
import excel "elsec11.xls", firstrow
saveold "elsec11.dta", replace version(12)

clear
import excel "elsec12.xls", firstrow
saveold "elsec12.dta", replace version(12)

clear
import excel "elsec13.xls", firstrow
saveold "elsec13.dta", replace version(12)

clear
import excel "elsec14.xls", firstrow
saveold "elsec14.dta", replace version(12)

clear
import excel "elsec15.xls", firstrow
saveold "elsec15.dta", replace version(12)

clear
import excel "elsec16.xls", firstrow
saveold "elsec16.dta", replace version(12)
