#!/bin/bash

set -e -o pipefail

cstubs=$(cat ../../../external/zarith/src/z.ml \
              | grep -Eo '^external[^=]*= *\"[^\"]*' \
              | sed 's/.*\"ml_z_//' \
              | sort -u)

tested=$(grep -E 'module Ml_z_[^ ]+' -o *.ml \
             | cut -f 2 -d ' ' \
             | sed 's/Ml_z_//' \
             | sort)

implemented=$(cat ../src/runtime.js \
                  | grep -o '//Provides: ml_z[^ ]*' \
                  | sed 's/\/\/Provides: ml_z_//' \
                  | grep -v normalize \
                  | sort)

diff <(echo $cstubs) <(echo $implemented) || (echo "Some javascript stubs are missing"; exit 1)
diff <(echo $implemented) <(echo $tested) || (echo "Some javascript stubs are not tested"; exit 1)
