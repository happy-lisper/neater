#!/bin/bash

script_dir="$( cd "$(dirname "$0")" && pwd )"

cd $script_dir/../altjvm

#for Java7 echo 1 | java -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Xmx512M -Xss2M -jar  $script_dir/sbt-launch.jar assembly
echo 1 | java  -Xmx512M -Xss2M -jar  $script_dir/sbt-launch.jar assembly
