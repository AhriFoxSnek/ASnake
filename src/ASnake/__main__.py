try:
    from .ASnake import build, ASnakeVersion, latestPythonVersionSupported
except ImportError:
    from ASnake import build, ASnakeVersion, latestPythonVersionSupported

# dependencies
from autopep8 import fix_code
from ruff_api import format_string, errors

# standard library
from subprocess import check_output, CalledProcessError, STDOUT
from os import path, remove, listdir
from time import monotonic
from re import sub as REsub
from re import findall as REfindall
from platform import python_version, python_version_tuple, python_implementation

ignoreCodes=('E114', 'E115', 'E116', 'E261', 'E262', 'E265', 'E266', 'E301', 'E302', 'E305', 'E4', 'E701',
                       'E702', 'E704', 'E722', 'E731', 'W3', 'W5', 'W6')

def fetchErrorLine(error_message, theCode):
    try:
        theLine = int(REfindall(r'Traceback \(most recent call last\):\n  File "(?:.+?)", line (\d+)', error_message)[-1])
        return (theLine, theCode.split('\n')[theLine - 1].strip())
    except IndexError:
        return False

def formatCode(code, version, compileTo='Python'):
    for _ in range(3):
        code = REsub(
            r"""\n\n+?(?=[^'"]+?|#.*?\n+?|(?:(?:[^"'\\]*?(?:\\.|'(?:[^'\\]*\\.)*?[^'\\]*?'|"(?:[^"\\]*\\.)*?[^"\\]*?"))*?[^"'\\]*?$))""",
            '\n', code)
    cpv = python_version_tuple()
    cpv = cpv[0] + '.' + cpv[1]
    if (float(version) < 3.12 <= float(cpv)) or compileTo == 'Cython':
        # ^ breaks behaviour on fstrings when targeting a lower version and compiler's version is higher
        code = fix_code(code, options={'ignore': ignoreCodes + ('E501',)})
    else:
        try:
            code = format_string('tmpFormat', code)
        except errors.ParseError:
            # fall back to autopep8
            code = fix_code(code, options={'ignore': ignoreCodes})
    return code

def execPy(code, fancy=True, pep=True, run=True, execTime=False, headless=False, runtime='Python', windows=False, runCommand=None, version=latestPythonVersionSupported, sourceCode=''):
    if pep:
        if execTime:
            print('# format time: ', end='', flush=True)
            s = monotonic()
        code = formatCode(code, version, runtime)

        if execTime:
            print(round(monotonic() - s, 4))
    if fancy:
        print(code)
    if run:
        from subprocess import run as spRun
        from subprocess import Popen
        from subprocess import PIPE
        if runCommand:
            pyCall = runCommand
        else:
            if not windows:  # linux
                if runtime == 'Pyston':
                    pyCall = 'pyston'
                elif runtime == 'PyPy':
                    pyCall = 'pypy3'
                elif runtime == 'MicroPython':
                    pyCall = 'micropython'
                else:
                    if len(check_output(['which', 'python3']).decode()) > 0:
                        pyCall = 'python3'
                    else:
                        pyCall = 'python'
            else:  # windows
                if runtime == 'PyPy':
                    try:
                        pyCall = check_output(['WHERE', 'pypy'], stderr=STDOUT).decode().split('/')[-1].strip()
                    except CalledProcessError:
                        print("Warning: PATH to pypy3 not found. Defaulting to Python.")
                        pyCall = 'py'
                        runtime = 'Python'
                else:
                    pyCall = 'py'

        err=False
        if fancy:
            print(f'\t____________\n\t~ {runtime} Eval\n')
        if headless:
            from os import getcwd
            with open('ASnakeExec.py', 'w') as f:
                f.write(code)
            if execTime:
                s = monotonic()
            child = Popen(f'{pyCall} ASnakeExec.py', stdout=PIPE, cwd=getcwd(), shell=True)
            child.communicate()
        else:
            if execTime:
                s = monotonic()
            #err = Popen((pyCall, '-c', code), stderr=PIPE).communicate()[1]
            err = False # above will give accurate Python error, but will also capture input('text here: ') which is more important
            # i hope one day i find a workaround, having the exact error line pointed out was nice
            spRun((pyCall, '-c', code))


        if fancy:
            print('\t____________')
        if execTime:
            print('exec time:', monotonic() - s)
        if err:
            err = str(err, 'utf-8')
            if sourceCode or code:
                errorLine = fetchErrorLine(err, sourceCode if sourceCode else code)
                print(('\t~~~~~~~~~~~~' if fancy else '') + '\n\t!!! ' + runtime + " Error\n\n")
                if errorLine:
                    print("Offending compiled line #%s:\n\t%s\n\n" % (errorLine))
                print(err)
            else:
                print(err)
    if headless:
        try:
            remove('ASnakeExec.py')
        except:
            pass


if __name__ == '__main__':
    from argparse import ArgumentParser, FileType, ArgumentError
    from sys import stdin
    from os import rename, chdir, listdir
    def makeParser(error=False):
        try:
            argParser=ArgumentParser(exit_on_error=error)
        except TypeError:
            argParser = ArgumentParser()
        argParser.add_argument('-r', '--run', action='store_true', help="Compiles file in memory then runs it.")
        argParser.add_argument('-e', '--eval', action='store', help="Compiles ASnake in a string to Python and runs it.")
        argParser.add_argument('-v', '--version', action='store', help="Specify which Python version to compile to.")
        argParser.add_argument('-c', '--compile', action='store_true', help="Compiles file to .py and writes it on disk. On Cython will attempt to compile to .so file.")
        argParser.add_argument('-o', '--optimize', action='store_true', help="Toggles optimization on and off. On by default.")
        argParser.add_argument('-fm', '--format', action='store_true', help="Turns off code formatting if on, and turns it off if on.")
        argParser.add_argument('-f', '--fast', action='store_true', help="Turns off code formatting, and turns off optimization. Useful for fast compile times.")
        argParser.add_argument('-pc', '--python-compatibility', action='store_true', help="Disables ASnake syntax to be compatible with most Python scripts. Useful for optimizing Python scripts written without ASnake in mind.")
        argParser.add_argument('-nc', '--no-comment', action='store_true', help="Turns off comments in the compiled file.")
        argParser.add_argument('-p', '--path', action='store', help="Custom path for compiled file.")
        argParser.add_argument('-np', '--no-print', action='store_true', help="Doesn't print the compiled file on console.")
        argParser.add_argument('-jr', '--just-run', action='store_true', help="Will run compiled version of file if it exists, otherwise will compile and run it.")
        argParser.add_argument('-cy', '--cython', '--Cython', action='store_true', help="Compiles the code to Cython and .pyx")
        argParser.add_argument('-pp', '--pypy', '--PyPy', action='store_true', help="Compiles to be compatible with PyPy3 Runtime.")
        argParser.add_argument('-ps', '--pyston', '--Pyston', action='store_true', help="Compiles to be compatible with Pyston runtime.")
        argParser.add_argument('-mp', '--micropython', '--MicroPython', action='store_true',help="Compiles to be compatible with MicroPython runtime.")
        argParser.add_argument('-rc', '--run-command', action='store', help="Specifies the command to call Python when using --run. Useful for when there are multiple Python aliases, and you want a specific one.")
        argParser.add_argument('-a', '--annotate', action='store_true',help="When compiling to Cython, will compile a html file showing Python to C conversions.")
        argParser.add_argument('-d', '--debug', action='store_true', help="Debug info for compiler developers.")
        argParser.add_argument('-t', '--test', action='store_true', help="Headless run debug for compiler developers.")
        argParser.add_argument('-as','--asnake-script', action='store',help="Sets path to ASnake's data folder, so you can run ASnake's collection of scripts included with the compiler. Running bare will list files in the data directory.")
        argParser.add_argument("file", type=FileType("r", encoding='utf-8'), nargs='?', const='notGiven', help="Your ASnake file to compile.")
        return argParser

    enforceTyping=compileAStoPy=runCode=headless=debug=justRun=False
    comment=optimize=pep=fancy=True

    try:
        args = makeParser().parse_args()
    except ArgumentError as e:
        if 'No such file' in str(e):
            from sys import argv
            tmpHandle='' ; tmpArgs = ' '.join(argv[1:])
            if '.asnake' in tmpArgs: tmpHandle='asnake'
            elif '.py'   in tmpArgs: tmpHandle='py'
            if tmpHandle:
                print('File not found. Perhaps try one of these files:')
                print('\t'+('\n\t'.join([_ for _ in listdir() if _.endswith('.'+tmpHandle)])),end='')
                print()
            from platform import python_version
            tmpPyVer=python_version().split('.')
            if tmpPyVer[0] == '3' and int(tmpPyVer[1]) >= 9: # exit_on_error added to argparse in 3.9
                makeParser(True).parse_args()
        else:
            print(e)
        exit()

    data: str = ''
    if not stdin.isatty():
        data = stdin.read()
        ASFile = False
    elif args.file == None or not path.isfile(args.file.name):
        if args.eval:
            data = args.eval
            ASFile = False
            runCode = True
        elif args.asnake_script:
            tmpASnakeScriptPath=__file__.replace('__main__.py','')
            if '\\' in tmpASnakeScriptPath: tmpASnakeScriptPath=tmpASnakeScriptPath+'data\\'
            else: tmpASnakeScriptPath=tmpASnakeScriptPath+'data/'
            if not args.asnake_script.endswith('.asnake'): args.asnake_script+='.asnake'
            if path.isfile(tmpASnakeScriptPath+args.asnake_script):
                ASFile = args.asnake_script
                args.path = tmpASnakeScriptPath
                with open(tmpASnakeScriptPath+args.asnake_script, 'r', encoding='utf-8') as f:
                    data=f.read()
            else:
                print(f'{args.asnake_script} not found. Here is the ASnake script directory:')
                print('\t'+('\n\t'.join([_ for _ in listdir(tmpASnakeScriptPath) if _.endswith('.asnake')])),end='')
                exit()

        else:
            tmp=[i for i in listdir() if i.endswith('.asnake')]
            if not tmp:
                tmp='myScript.asnake'
            else: tmp=tmp[0]
            from sys import argv
            print(f'ASnake Compile Error:\n\tCouldn\'t open file. Make sure to provide a path for a file, and that the path is correct.\nSuggestion:\n\t{argv[0]} -r {tmp}')
            exit()

    else:
        ASFile = args.file.name
        data=args.file.read()


    del args.file
    if args.run and not args.eval: runCode=True
    if args.test: headless=True
    if args.fast: pep=False ; optimize=False
    if args.optimize:
        if optimize: optimize=False
        else: optimize=True
    if args.format:
        if not pep: pep = True
        else: pep = False
    if args.debug: debug=True
    if args.no_print: fancy=False
    if args.compile:
        compileAStoPy=True
        if args.eval: ASFile='cmdEval.asnake'
    if args.no_comment: comment=False
    if args.just_run:
        justRun=True ; fancy=False ; runCode=True ; pep=False
    if args.version:
        try: pythonVersion=float(args.version)
        except: pythonVersion = latestPythonVersionSupported
    if args.python_compatibility:
        data = "$ pythonCompatibility\n" + data
    if args.cython:
        compileTo='Cython' ; enforceTyping = True
        if compileAStoPy and not args.annotate:
            pep = False
    elif args.pyston: compileTo='Pyston'
    elif args.pypy: compileTo='PyPy3'
    elif args.micropython: compileTo = 'MicroPython'
    else: compileTo='Python'

    if not fancy and not compileAStoPy: pep = False

    if not args.version:
        # v if the compile target is same as the interpreter we are using to compile with, use that version
        tmpSafe=False
        if args.pyston:
            import sys
            if hasattr(sys, "pyston_version_info"):
                tmpSafe=True
            del sys
        elif args.pypy and python_implementation() == 'PyPy':
            tmpSafe=True
        elif python_implementation() == 'CPython':
            tmpSafe = True
        if tmpSafe:
            pv = python_version_tuple()
            pythonVersion = pv[0] + '.' + pv[1]
        else:
            if args.pyston:
                pythonVersion = '3.08' # its probably gonna stay here forever




    WINDOWS=False
    if compileAStoPy or runCode:
        from platform import system as OSName
        if 'windows' in OSName().lower():
            WINDOWS = True

    if not debug and (fancy or compileAStoPy): print('# build time: ', end='', flush=True)
    s=monotonic()
    if (compileTo == 'Cython' and justRun) == False:
        code=build(data,comment=comment,optimize=optimize,debug=debug,compileTo=compileTo,pythonVersion=pythonVersion,enforceTyping=enforceTyping)
    else: code=''
    if fancy or compileAStoPy:
        s=round(monotonic()-s,4)
        if debug: print('# build time:', s)
        else: print(s)
    if compileAStoPy:
        if args.path:
            if WINDOWS:
                from pathlib import PureWindowsPath
                args.path = PureWindowsPath(args.path).as_posix()
            if path.isdir(args.path):
                args.path += '/'
            if args.path.endswith('/'):
                args.path+="".join(x for x in '.'.join(ASFile.rsplit('.')[:-1]).split('/')[-1] if x.isalnum())
            if not args.path.endswith('.py'):
                args.path+='.py'
            ASFile,tmpPath=args.path,ASFile
        filePath='/'.join(ASFile.split('/')[:-1])+'/'
        ASFileExt=ASFile.rsplit('.')[-1]
        ASFile='.'.join(ASFile.rsplit('.')[:-1])
        ASFile = "".join(x for x in ASFile.split('/')[-1] if x.isalnum())
        fileName=f'{ASFile}.py{"x" if compileTo=="Cython" else ""}'
        if pep or headless:
            print('# format time: ', end='', flush=True)
            s = monotonic()
            code = formatCode(code, pythonVersion, compileTo)
            print(round(monotonic() - s, 4))
        if filePath=='/': filePath=''
        if ASFileExt == 'py' and path.isfile(filePath+fileName):
            fileName="AS_"+fileName
        if code.startswith(f'# ASnake {ASnakeVersion} ERROR'):
            execPy(code, run=True, execTime=False, pep=False, headless=False, fancy=False, windows=WINDOWS,runCommand=args.run_command,version=pythonVersion)
            exit()
        with open(filePath+fileName,'w',encoding='utf-8') as f:
            f.write(code)
        if compileTo == 'Cython':
            if "'" in fileName:
                fileName=fileName.replace("'","\\'")

            if args.run_command:
                p3Command=args.run_command
            elif WINDOWS:
                if args.pypy:
                    try:
                        py3Command = '"' + check_output(['WHERE', 'pypy'], stderr=STDOUT).decode().split('/')[-1].strip() + '"'
                    except CalledProcessError:
                        print("Warning: PATH to pypy3 not found. Defaulting to Python.")
                        py3Command = '"' + (check_output(['WHERE', 'python']).decode().split('\n')[0]).replace('\r','') + '"'
                else:
                    py3Command='"'+(check_output(['WHERE', 'python']).decode().split('\n')[0]).replace('\r','')+'"'
            else: # linux
                if args.pyston and len(check_output(['which', 'pyston']).decode()) > 0:
                    py3Command='pyston'
                elif args.pypy and len(check_output(['which', 'pypy3']).decode()) > 0:
                    py3Command = 'pypy3'
                elif len(check_output(['which', 'python3']).decode()) > 0:
                    py3Command='python3'
                else: py3Command='python'
            includeNumpy= True if 'import numpy' in code or 'from numpy' in code else False
            with open('ASsetup.py', 'w') as f:
                f.write(f"""
{'import numpy' if includeNumpy else ''}
from setuptools import setup
try:
    from Cython.Build import cythonize
except ModuleNotFoundError:
    print('Cython is not installed, ASnake is unable to compile to .so file.\\nThe .pyx file still compiled.\\nDo something like:\\n\\t{"python" if "windows" in OSName().lower() else py3Command} -m pip install cython')
    raise Exception
setup(ext_modules = cythonize('{filePath + fileName}',annotate={True if args.annotate else False}),
{'include_dirs=[numpy.get_include(),"."]' if includeNumpy else 'include_dirs=["."]'})""")
            try:
                print('# C compile time: ', end='', flush=True)
                s = monotonic()
                cythonCompileText = check_output(f'{py3Command} ASsetup.py build_ext --inplace', shell=True).decode()
                error=False
                print(round(monotonic()-s,2))
            except CalledProcessError as e:
                cythonCompileText = e.output.decode()
                error=True
            remove('ASsetup.py')
            if fancy or error:
                print(cythonCompileText)
                if error and py3Command == 'pypy3':
                    if 'ModuleNotFoundError' in cythonCompileText:
                        print('Suggestion:\n\tpypy3 -m ensurepip\nTo make sure you have pip, then:\n\tpypy3 -m pip install --upgrade pip setuptools\nTo make sure you have the latest setuptools.')
                    elif not WINDOWS and 'fatal error: Python.h: No such file or directory' in cythonCompileText:
                        print("Suggestion:\n\tsudo apt-get install pypy3-dev\n(or your distro's equivalent) For headers.")
            cythonsoFile = ''
            if not error:
                cythonsoFile=cythonCompileText.split('/')[-1][:-5]
                if filePath:
                    rename(cythonsoFile,filePath+cythonsoFile)

                if runCode:
                    if filePath:
                        chdir(filePath)
                    execPy(code,run=False,execTime=False,pep=pep,headless=headless,fancy=False,windows=WINDOWS,version=pythonVersion)
                    if '/' in ASFile: tmp=f"import sys\nsys.path.append('{ASFile.split('/')[-1]}')\nimport {ASFile.split('/')[-1]}"
                    else: tmp=f'import {ASFile}'
                    if args.pyston: runtime = 'Pyston'
                    elif args.pypy: runtime = 'PyPy'
                    else: runtime = 'Cython'
                    execPy(tmp,run=runCode,execTime=True,pep=False,headless=headless,fancy=fancy,runtime=runtime,windows=WINDOWS,runCommand=args.run_command,version=pythonVersion,sourceCode=code)


        if fancy:
            if ASFileExt == 'py' and not args.python_compatibility:
                print('# Warning: Consider using -pc or --python-compatibility flag on Python files to ignore ASnake syntax.')

            ASFileExt='.'+ASFileExt
            if args.path:
                ASFile, tmpPath = tmpPath, ASFile
                fileName=args.path
                ASFileExt=''
            if compileTo == 'Cython':
                if error:
                    print(f'!!! {ASFile}{ASFileExt} Cython compilation failed. See above.')
                else:
                    print(f'{ASFile}{ASFileExt} compiled to {fileName} and {cythonsoFile}')
            else:
                print(f'{ASFile}{ASFileExt} compiled to {fileName}')
    else:
        if ASFile:
            tmp='/'.join(ASFile.split('/')[:-1])+'/'
            if tmp != '/': chdir(tmp)

        if args.pyston: runtime = 'Pyston'
        elif args.pypy: runtime = 'PyPy'
        elif args.micropython: runtime='MicroPython'
        else: runtime = 'Python'

        if compileTo == 'Cython':
            ASFile='.'.join(ASFile.rsplit('.')[:-1])
            execPy(code,run=False,execTime=False,pep=pep,headless=headless,fancy=fancy,windows=WINDOWS,runCommand=args.run_command,version=pythonVersion,runtime=compileTo)
            if '/' in ASFile:
                tmpASFile=ASFile.split('/')[-1].replace("'","\\'")
                ASFile=ASFile.replace("'","").replace("_",'')
                tmp=f"import sys\nsys.path.append('{tmpASFile}');import {ASFile.split('/')[-1]}"
            else: tmp=f'import {ASFile}'
            tmpSrc=''
            if justRun and ASFile+'.pyx' in listdir():
                with open(ASFile+'.pyx', 'r') as f:
                    tmpSrc=f.read()
            execPy(tmp,run=runCode,execTime=True,pep=False,headless=False,fancy=False,runtime=runtime,windows=WINDOWS,runCommand=args.run_command,version=pythonVersion,sourceCode=tmpSrc)
        else:
            execPy(code,run=runCode,execTime=fancy,pep=pep,headless=headless,fancy=fancy,runtime=runtime,windows=WINDOWS,runCommand=args.run_command,version=pythonVersion)


