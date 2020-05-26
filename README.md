
# naturL
![Build](https://github.com/TheNaturLFoundation/naturL/workflows/Build/badge.svg?branch=master&event=push)
## What is naturL

naturL is a pseudo-code to Python script transpiler. The pseudo-code is written in French formalized pseudo-code "naturL" to a Python script. This language eases the learning of programming with idiomatic languages structures, close to French logic terms.

## Install naturL

### Unix

Execute this command in a unix terminal 

```
git clone https://github.com/TheNaturLFoundation/naturL.git /usr/bin/ ; bash /usr/bin/naturL/install.sh
```

Or download the zip file [here](https://github.com/TheNaturLFoundation/naturL/archive/master.zip).
and run install.sh
```
make
```
This command will install the latest OCaml system and its dependencies if it finds no compatible OCaml system and start the compilation process. 
You can now use the program with the command 
```
naturL
``` 
### Windows

Run the installer that you can find here, the command is now binded to cmd and PowerShell

## Run naturL

naturL is run via command line interface using the folowing options.
```
"--input"  input_name, "The file that should be read. Default is stdin"

"--output"  output_name :"The file where the output should be printed. Default is stdout"

"--warning" warning_severity, "The minimum severity of the warnings. Default is 0 (all warnings)"

"--debug" : "Display debug infos"

"--language" ["french"; "english"], " This option determines the language of the error messages."

"--import-mode" ["write-nothing"; "moderated"; "overwrite"] : " Specify when imported .py files need to be generated."
```
## Use naturL with idL
naturL can be installed on Windows using our own IDE : [idL](https://github.com/TheNaturLFoundation/idL)
 
If you like our programs :

<a href="https://www.buymeacoffee.com/naturL" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" style="height: 41px !important;width: 174px !important;box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;-webkit-box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;" ></a>
