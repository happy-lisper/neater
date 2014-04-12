#!/bin/bash

script_dir="$( cd "$(dirname "$0")" && pwd )"

jar=$script_dir/../altjvm/target/neater.jar 
java -cp $jar scripts.Stamp $@
