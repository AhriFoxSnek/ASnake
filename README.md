# ASnake
Optimizing Python transpiler for the ASnake programming language.

Documentation of the language is available at: https://asnake.org/docs/docs.html

You can also get support or hang out in the ASnake discord: https://discord.gg/ySDFchT

To install run:
```console
python -m pip install ASnake
```
You may have to use `py` or `python3` or similar depending on what you have installed for a Python interpreter.

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
