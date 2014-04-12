@echo off

set SCRIPT_DIR=%~dp0

set jar="%SCRIPT_DIR%..\altjvm\target\neater.jar"

java -Xms1024m -Xmx1024m -cp %jar% scripts.Stamp %*
