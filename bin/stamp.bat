@echo off

set SCRIPT_DIR=%~dp0

set jar="%SCRIPT_DIR%..\altjvm\target\andysum.jar"

java -Xms1024m -Xmx1024m -cp %jar% scripts.Stamp %*
