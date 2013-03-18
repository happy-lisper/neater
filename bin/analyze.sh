#!/bin/bash

script_dir="$( cd "$(dirname "$0")" && pwd )"

jar=$script_dir/../altjvm/target/andysum.jar 
java -cp $jar scripts.Analyze $@
