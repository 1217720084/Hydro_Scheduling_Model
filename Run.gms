*=====================================================================================
* Name:                 runSM.gms
* Function:             This file is invoked to control the entire operation of SM.
* Developed by:         Electricity Authority, New Zealand
* Source:               https://github.com/ElectricityAuthority/SM/
*                       http://www.emi.ea.govt.nz/Tools/SM
* Contact:              Forum: http://www.emi.ea.govt.nz/forum/
*                       Email: tuong.nguyen@ea.govt.nz
* Last modified on:     01 Jan 2100
*=====================================================================================


$call cls
$onecho > con
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*+++++++++++++++++++++ EXECUTING vSPD v3.0.0 +++++++++++++++++++++
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$offecho

Files
temp       "A temporary, recyclable batch file"

*$call gdxxrw Input.xlsx o=sm.gdx index=myindex!a1
*$stop

$call gams Model.gms s=Model


* Solve the model for the current input file
put_utility temp 'exec' / 'gams Solve.gms r=Model lo=3 Errmsg = 1' ;


*$ontext
*execute 'if exist *.lst   erase /q *.lst '
execute 'if exist *.~gm   erase /q *.~gm '
execute 'if exist *.lxi   erase /q *.lxi '
execute 'if exist *.log   erase /q *.log '
execute 'if exist *.put   erase /q *.put '
*execute 'if exist *.txt   erase /q *.txt '
*execute 'if exist *.gdx   erase /q *.gdx '
execute 'if exist temp.*  erase /q temp.*'
*$offtext

