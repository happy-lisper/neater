#!/bin/bash

script_dir="$( cd "$(dirname "$0")" && pwd )"

jar=$script_dir/../altjvm/target/scala-2.12/neater.jar
java -cp $jar scripts.Diff $@
