# ijbconv

This tool handles SAVE data in the files directory of "IchigoJam BASIC RPi" SD card.
[Document](https://mitsuji.github.io/ijbconv/index-ja.htm)


## build

$ stack build


## install

$ stack install


## execute

### show code text of binary file
$ ~/.local/bin/ijbconv-exe bt < code.bin

### save code text of binary file
$ ~/.local/bin/ijbconv-exe bt < code.bin > code.txt

### save code binary of text file
$ ~/.local/bin/ijbconv-exe tb < code.txt > code.bin

