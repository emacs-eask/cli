@echo off

call eask --version
call eask --version

call eask info
call eask archives -g
call eask clean-all

pause
