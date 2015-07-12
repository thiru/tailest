# Tailest

## Overview

Shows the last *n* lines of the last modified file in a directory.

## Arguments

* --debug | -d
  * Show debug information

* --num-lines <n> | -n <n>
  * Specifies the number of lines to show

* --help | -h
  * Shows the help

* --version | -v
  * Show just the current version

## Examples

* The easiest way to run the app is to just call it without any arguments
* This will show the last 30 lines of the last modified file in the current
  directory

    tailest

* The following is probably the next most common usage
  * showing 23 lines of the last modified file in the current directory

    tailest -n 23
