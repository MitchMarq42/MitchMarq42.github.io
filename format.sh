#!/bin/sh

# Convert vimwiki syntax to org syntax...

sed 's/^(\=*)(.*)\1$/\1\s\2/' |
    sed 's/^?=?=/\*/g'
