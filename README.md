# ASnake
Optimizing Python transpiler for the ASnake programming language.

A proper readme is yet to come. If you are super interested, we have a lot more info (including some draft documentation) on this Discord:
https://discord.gg/ySDFchT

To install, clone this repository or download a release. Open a terminal in the directory and run:
```console
pip install -e .
```
After that, ASnake and all it's compiler flags can be callable via:
```console
python -m ASnake
```
Here are some examples:
```console
python -m ASnake -e "'Hello world!'"
python -m ASnake --help
python -m ASnake --compile --run test.asnake
python -m ASnake -e "loop 12 do 'the code goes inside the strings dummy, don\'t you know bash?'"
```

You can also checkout the website https://asnake.org !
