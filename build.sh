#!/bin/sh
function build_container {
    cd "$1"
    stack docker pull
    stack build
    stack image container
    cd ".."
}
build_container "./Auth"
build_container "./Dir"
build_container "./Transaction"
build_container "./FileSystem/"
