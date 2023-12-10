#!/bin/sh

cat input | sed -e 's/F/┌/g' -e 's/L/└/g' -e 's/J/┘/g' -e 's/7/┐/g' -e 's/|/│/g' -e 's/-/─/g' -e 's/S/\\\\e[1;32mS\\\\e[0m/' | xargs -n1 echo -e