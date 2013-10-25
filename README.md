hConfigGenerator
================

generate files from stringtemplates and json variables in haskell

## Compile

to compile run

    cabal configure
    cabal build
    
## Run

run the executable with

    dist/build/configGenerator/configGenerator -t templates/test.st -j templates/test.json
    dist/build/configGenerator/configGenerator -t templates/test_angle.st -j templates/test.json -a
