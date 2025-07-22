# -*- coding: utf-8 -*-
# dependencies
from sly.lex import LexError
from sly import Lexer

# standard library
from copy import copy
from re import compile as REcompile
from re import search as REsearch
from re import findall as REfindall
from re import MULTILINE as REMULTILINE
from re import sub as REsub
from keyword import iskeyword
from unicodedata import category as unicodeCategory

ASnakeVersion = 'v0.13.43'
__version__ = ASnakeVersion[1:]

def AS_SyntaxError(text=None,suggestion=None,lineNumber=0,code='',errorType='Syntax error'):
    showError=[]
    if text != None:
        showError.append(f'{errorType}:\n\t{text}')
    else:
        showError.append(f'{errorType}.')
    if suggestion != None:
        showError.append(f'Suggestion, try something like:\n\t'+suggestion)
    if lineNumber != None:
        try: showError.append(f'Offending line {lineNumber}:\n'+str(code.split('\n')[lineNumber-1]))
        except IndexError:
            try: showError.append('Sorry, line number error. Info for lang-dev:\n\t'+str(lineNumber)+' '+str(text.count('\n')))#,len([i for i in lex if i.type in typeNewline]))
            except Exception as e: print(e)
    showError='\n'.join(showError)
    #print(showError)

    return f'# ASnake {ASnakeVersion} ERROR\nprint("""\n{showError}\n""")'

defaultTypes=set('bool|int|float|complex|str|list|tuple|set|dict|bytearray|bytes|enumerate|filter|frozenset|map|memoryview|object|property|range|reversed|slice|staticmethod|super|type|zip'.split('|'))
pureStdPythonModules = "abc,array,base64,binascii,bisect,calendar,cmath,collections,colorsys,contextvars,copy,dataclasses,decimal,enum,fractions,graphlib,heapq,itertools,keyword,math,numbers,operator,pprint,re,reprlib,statistics,string,struct,textwrap,token,types,typing,unicodedata".split(',')

import ast
import operator
_OP_MAP = {
    ast.Add: operator.add,
    ast.Sub: operator.sub,
    ast.Mult: operator.mul,
    ast.Div: operator.truediv,
    ast.FloorDiv: operator.floordiv,
    ast.USub: operator.neg,
    ast.Pow: operator.pow,
    ast.RShift: operator.rshift,
    ast.LShift: operator.lshift,
    ast.BitOr: operator.or_,
    ast.BitXor: operator.xor,
    ast.BitAnd: operator.and_ ,
    ast.Mod: operator.mod,
}
class Calc(ast.NodeVisitor):
    def visit_BinOp(self, node):
        return _OP_MAP[type(node.op)](self.visit(node.left), self.visit(node.right))
    def visit_Constant(self, node):
        return node.n
    def visit_Expr(self, node):
        return self.visit(node.value)
    def visit_UnaryOp(self, node):
        return _OP_MAP[type(node.op)](node.operand.value)
    @classmethod
    def evaluate(cls, expression):
        return str(cls().visit(ast.parse(expression).body[0]))

showWarning = True
class Lexer(Lexer):

    def error(self, t):
        if showWarning:
            print(f"# ASnake {ASnakeVersion} Warning: Illegal character in:\n'''\n{t.value}'''")
        self.index += 1

    # Set of token names.   This is always required
    tokens = { ID, NUMBER, PLUS, MINUS, TIMES,
               DIVIDE, RDIVIDE, ASSIGN, LPAREN, RPAREN, STRING, NEWLINE,
               GREATER, GREATEQ, LESS, LESSEQ, EQUAL, ELIF, IF, ELSE, THEN,
               LISTCOMP, WHILE, IGNORENL, NOTHING, FUNCTION, NOTEQ,
               LIST, BUILTINF, INS, IMPORT, TAB, MATCH, OF, PIPE, INDEX,
               PYPASS, AND, OR, DEFFUNCT, FROM, RETURN, TYPE, FOR,
               END, ARE, ANYOF, BOOL, COMMA, CONSTANT, TRY, LOOP, STRLIT,
               STRRAW, SET, DICT, MODULO, ASYNC, NRANGE, EXPONENT, PYDEF,
               META, SCOPE, BITWISE, DIVMOD, TYPEWRAP, SHEBANG, COMMENT,
               FUNCMOD, WITHAS, LISTEND, ENDIF, INC, BREAK, LBRACKET, RBRACKET,
               LAMBDA, PYCLASS, ELLIPSIS, BINARY, PIPEGO, DQUOTE, SQUOTE,
               HEXDEC, SCINOTAT, NOTIN, FSTRFRMT, METALBKT, METARBKT
             }

    # String containing ignored characters between tokens
    ignore  = ' \t '
    #ignore_comment = r'(?=(\t| ))*?#.+?(#|(?=\n|$))'

    # Regular expression rules for tokens
    # THIS IS RELATIVELY COMMON FOR LEXERS DUMMIES, REGEX != BAD
    # simplifying expressions tends to be good though
    # order is important, top takes most precedence, bottom least
    SHEBANG = r'#(!| *cython:) *.*'
    COMMENT = r'[\t ]*?(?:#|💭|💬).*?(!#|(?=\n|$))'
    TAB     = r'\n(>>>|\.\.\.)?[\t ]+'
    NEWLINE = r'\n'
    PYPASS  = r"p!(.|\n)+?!p"
    METALBKT= r'[ \t]*\${[ \t]*\n?'
    METARBKT= r'[ \t]*\$}[ \t]*\n?'
    META    = r'\$ *?((\w+(?![^+\-/*^~<>&\n()]*=)(?=[\n \]\)\$\[+:};(]))|(([^+\-/*^~<>&\n()[\],=]*|(\={2}))((=.*)|(?=\n)|$|(?=[,.]))))'
    FUNCMOD = r'@.*'
    PYDEF   = r'''(c|cp)?def +([\w.\[\d:,\] ]* )?\w+ *\(([^()]|\((?:[^)]|'.*[()]+.*'|".*[()]+.*")*\))*\)*( *((-> *[\w\[\], {}]+)? *):?)(?!return)'''
    PYCLASS = r'class ([a-z]|[A-Z])\w*(\(.*\))?:?'
    STRLIT  = r'[rf]?\"\"\"[\w\W]+?\"\"\"|[rf]?\'\'\'[\w\W]+?\'\'\''
    INDEX   = r'''(?:([^\u0000-\u007F\s]|[a-zA-Z_])([^\u0000-\u007F\s]|[a-zA-Z0-9_])*?\.)?([^\u0000-\u007F\s]|[a-zA-Z_])([^\u0000-\u007F\s]|[a-zA-Z0-9_])*?(?:\[[^\[\]]*(?:\[[^\[\]]*\])?[^\[\]]*\])+'''
    LIST    = r'\['
    LISTEND = r'\]'
    IF      = r'if(?=[\W\d\n(])'
    ELIF    = r'(, )?elif(?= |\t|\()'
    ELSE    = r'''(, *)?else(?= |\n|\t|:|\(|'|")'''
    FUNCTION= r'\w+\('
    NRANGE  = r'(-?(\d+|\w+(\(.\))?)\.\.\.?(-?\d+|\w+(\(.\))?))|-?\d+ ?to ?-?\d+'
    BUILTINF= r"""(([a-zA-Z_]+\d*|[^\s\d='";()+\-*[\],{}]*|(f|u)?\"[^\"]*\"|(f|u)?\'[^\"\']*\')\.([^\u0000-\u007F\s]|[a-zA-Z_])+([^\u0000-\u007F\s]|[a-zA-Z0-9_])*)+"""
    TRY     = r'(((try)|(except +([A-Z]\w+|\w+\.\w+)( +as +\w*)?)|(except)|(finally))(( *:?)|( +(do|then))))'
    TYPEWRAP= fr'({"|".join(defaultTypes)})( ?\[\w*\])? *: *(#.*)?(?=\n)'
    TYPE    = '\\s%s\\s'%f'({"|".join(defaultTypes)})'
    LAMBDA  = r'lambda ?(\w* *,?)*:'
    FSTRFRMT= r':,? *(?:\=?[*=.]?[><^|%.+])?(?:(?:\d+(?:\.\d+)?[dfxsn%]?)| *[dbxXogGeEncs](?![^}])|(?: *%[YmdHMS][:-]? *)+)'  # for formatting at end of fstrings brackets
    LISTCOMP= r'''\-?\w*: ([^\u0000-\u007F\s]|[a-zA-Z_])([^\u0000-\u007F\s]|[a-zA-Z0-9_])*(?!"|')'''
    STRING  = r"([fubFUB]?\"\"\"(?:[^\"\\]|\\.|\"(?!\"\"))*\"\"\")|([fubFUB]?'''(?:[^'\\]|\\.|'(?!''))*''')|([fubFUB]?\"(?:\\.|[^\"\\])*\")|([fubFUB]?'(?:\\.|[^'\\])*')"
    LBRACKET= r'{'
    RBRACKET= r'}'
    STRRAW  = r"""[fFbB]?[rR](([fF]?\"{3}(\\"|[^"])+?\"{3})|([fF]?\'{3}(\\'|[^'])+?\'{3})|([fF]?\"(\\"|[^"\n])+?\")|([fF]?\'(\\'|[^'\n])+?\'))"""
    IMPORT  = r"""(^|(?! )|from +[^'"\n ]*) ?c?import(?:(?: [^\n;]*)| *\*)"""
    EQUAL   = r'==|equals?(?= |\t)'
    NOTIN   = r"isn'?t +in( |(?=\n))"
    NOTEQ   = r'!=|isnt|isn\'t|not +equal|unequal'
    LESSEQ  = r'<=|=<|≤'
    GREATEQ = r'>=|=>|≥'
    OR      = r'(\|\||or)(?=[ \t])'
    ASSIGN  = r'''=|is( |(?=("|'|{|\[|\()))|(\+|-|\*\*?|\/\/?|:|%|>>|<<|\^|~|\||&)='''
    BITWISE = r'[\^~|&]|<<|>>(?!=)'
    LESS    = r'<|((is )?less (than )?)|(lesser (than )?)'
    GREATER = r'>|((is )?greater (than )?)'
    ENDIF   = r': *'
    DEFFUNCT= r'(c|cp)?does(?=[ \n\t])'
    SCOPE   = r'(global|local|nonlocal) (\w* *,?)*'
    THEN    = r'((then|do|then +do|, +then|, +do|, +then +do)(?=\s))|;|(:(?=\n)+)'
    WITHAS  = r'(with|(?![^\w\d])as) '
    WHILE   = r'while '
    NOTHING = r'(pass|nothing,?)(?= |$|\n)'
    MATCH   = r'match +'
    EXPONENT= r'\*\*|power(?:\sof(?= |\t))?(?= |\t)|exponent(?=[ \t])'
    OF      = r'(case|of)([ \t]|(?=[\W]))'
    END     = r'end(?=\s|;)'
    PIPE    = r'(into|to)(?!\S)'
    PIPEGO  = r'pipe(?=[ \t])'
    AND     = r'and(?=[ \t])'
    FROM    = r'from(?=[ \t])'
    RETURN  = r'(return|yield +from|yield|del|raise|assert)(?=[\W\d\n]|$)'
    BREAK   = r'(break|continue)(?=[ \n\t])'
    FOR     = r'for(?= |\t)'
    LOOP    = r'loop(?= |\n)'
    ASYNC   = r'(async|await)(?=[ \t])'
    CONSTANT= r'const(?:ant)?[ \t]+'
    ANYOF   = r'(any|all|each) +(of )?'
    INS     = r'(not|in)( |(?=\n))'
    ARE     = r"(arent|aren\'t|are)(?=[ \n\t])"
    BOOL    = r'True|False|None'
    MODULO  = r'%|modulo(?= |\n|\t)|remainder(?=[ \n\t])'
    INC     = r'((\+{2}|\-{2})[^\[\]\(\)\+\-\/\*\d\s,=][^\s\+\-\/\*,\(\)=><]*(\[.*\])?)|([^\[\]\(\)\+\-\/\*\d\s,=][^\s\+\-\/\*,=]*(\[.*\])?(\+{2}|\-{2}))'
    HEXDEC  = r'(?<!\w)0[xXob][0-9a-fA-F]+(?!\w)'
    SCINOTAT= r'(?:0|[1-9]\d*)(?:\.\d+)?[eE]\d+'
    NUMBER  = r'(((( \-\d|\d)\d*\.?\d*)|(\-?\.))(e(-|\+)\d+)?\.?_*\d*j?)'
    BINARY  = r'0[oOxXbB]\d+'
    MINUS   = r'-|minus(?=[ \t])'
    PLUS    = r'\+|plus(?=[ \t])'
    TIMES   = r'\*|times(?=[ \t])'
    DIVMOD  = r'///'
    RDIVIDE = r'//|r(ound ?)?divide( +by)?(?=[ \t])'
    DIVIDE  = r'/|divide( +by)?(?=[ \t])'
    ID      = r'(?:[a-zA-Z_][\w]*|[^\u0000-\u007F\s](?:[^\u0000-\u007F\s]|[\w])*)'
    ELLIPSIS= r'\.\.\.'
    DQUOTE  = r'"'
    SQUOTE  = r"'"
    COMMA   = r','
    IGNORENL= r'\\'
    LPAREN  = r'\(|\['
    RPAREN  = r'\)|]'

    # Tokens not defined here:
    # DEFEXP   = perform default expression wrap
    # DONTDEXP = do not perform default expression wrap
    # IGNORE   = ignore the token, preferably deleting it later.
    # COLON    = the : character
    # FWRAP    = a function meant to wrap the line (to the right)

latestPythonVersionSupported='3.13'

def build(data,optimize=True,comment=True,debug=False,compileTo='Python',pythonVersion=latestPythonVersionSupported,enforceTyping=False,variableInformation={},outputInternals=False,metaInformation=False):
    global showWarning # for disabling syntax warnings for miniLex, can produce warnings that are not the users fault
    # data is the string version of code for parsing
    # optimize when True will enable optimization phase and optimizations, False will disable any optimizations.
    # comment when True will output comments in the final file, False will attempt to have minimal comments.
    # debug when True will print out information to aid in debugging, False should minimize prints.
    # compileTo string will be the compile target.
    # pythonVersion sets the Python version to compile to.
    # enforceTyping will make the compiler complain more about variable types.
    # variableInformation will override storedVarsHistory, allowing prior information to be gained that wasn't in the string code.
    # outputInternals changes its output from the compiled code string, to a tuple with (code, lex, storedVarsHistory)
    # metaInformation will override various meta variables, allowing metas to be overridden before code is ran.

    miniLex = Lexer().tokenize
    codeDict={'RDIVIDE':'//','DIVIDE':'/','PLUS':'+','MINUS':'-','TIMES':'*','LPAREN':'(','RPAREN':')',
    'ELIF':'elif','ELSE':'else','IF':'if','WHILE':'while','GREATEQ':'>=','GREATER':'>','LESS':'<',
    'LESSEQ':'<=','EQUAL':'==','ASSIGN':'=','NOTHING':'pass','NOTEQ':'!=','BUILTINF':'.','OF':'elif',
    'AND':'and','OR':'or','RETURN':'return','FOR':'for','MODULO':'%','EXPONENT':'**','COMMA':',',
    'LISTEND':']','ELLIPSIS':'...','constLPAREN':'(','COLON':':','LINDEX':'[','RINDEX':']',
    "DQUOTE":'"',"SQUOTE":"'", 'LBRACKET':'{','RBRACKET':'}','PYIS':' is '}

    convertType={'int':'NUMBER','float':'NUMBER','Py_ssize_t':'NUMBER','bool':'BOOL','bint':'BOOL','str':'STRING','list':'LIST','dict':'DICT','type':'TYPE','tuple':'TUPLE','set':'SET','bytes':'STRING','object':'ID','range':'FUNCTION','complex':'NUMBER','frozenset':'FUNCTION','bytearray':'STRING','memoryview':'FUNCTION'}
    cythonConvertType = {'int': 'long long int', 'bool': 'bint', 'float': 'double'}
    for t in cythonConvertType: convertType[cythonConvertType[t]]=convertType[t]
    typeTypes=tuple([t for t in convertType])

    # useful types of sets of tokens or other things
    typeAssignables=('STRRAW','STRING','NUMBER','ID','LIST','LISTEND','DICT','BINARY','LBRACKET','BOOL')
    typeOperators=('PLUS','MINUS','TIMES','DIVIDE','RDIVIDE','EXPONENT','BITWISE','MODULO')
    typeCheckers=('LESS','LESSEQ','GREATEQ','GREATER', 'EQUAL', 'PYIS','NOTEQ')
    typePrintable=typeAssignables+typeOperators+typeCheckers+('LINDEX','RINDEX','INDEX','LPAREN','RPAREN','MODULO','IGNORE','INC','INS','DIVMOD','COMMA','BITWISE')
    mopConv={'TIMES':'*=','PLUS':'+=','DIVIDE':'/=','RDIVIDE':'//=','MINUS':'-='}
    typeMops=tuple(i for i in mopConv)
    typeConditionals=('IF','ELIF','ELSE','OF','WHILE')
    typeNewline=('NEWLINE','TAB','THEN','END')
    typeLoop=('WHILE','LOOP','FOOR')
    # useful sets of strings
    listMods=('.pop','.append','.extend','.insert','.remove','.reverse','.sort','.copy','.clear')
    setUpdateMethods=('.add','.clear','.discard','.difference_update','.intersection_update','.pop','.remove','.symmetric_difference_update','.update')
    pyBuiltinFunctions=('abs', 'delattr', 'hash', 'memoryview', 'set', 'all', 'dict', 'help', 'min', 'setattr', 'any', 'dir', 'hex', 'next', 'slice', 'ascii', 'divmod', 'id', 'object', 'sorted', 'bin', 'enumerate', 'input', 'oct', 'staticmethod', 'bool', 'int', 'open', 'str', 'breakpoint', 'isinstance', 'ord', 'sum', 'bytearray', 'filter', 'issubclass', 'pow', 'super', 'bytes', 'float', 'iter', 'print', 'tuple', 'callable', 'format', 'len', 'property', 'type', 'chr', 'frozenset', 'list', 'range', 'vars', 'classmethod', 'getattr', 'locals', 'repr', 'zip', 'compile', 'globals', 'map', 'reversed', 'complex', 'hasattr', 'max', 'round', 'exec', 'eval', '__import__', 'exit')
    pyReservedKeywords=('False', 'None', 'True', 'and', 'as', 'assert', 'async', 'await', 'break', 'class', 'continue', 'def', 'del', 'elif', 'else', 'except', 'finally', 'for', 'from', 'global', 'if', 'import', 'in', 'is', 'lambda', 'nonlocal', 'not', 'or', 'pass', 'raise', 'return', 'try', 'while', 'with', 'yield', 'case')
    ASnakeKeywords=('nothing', 'minus', 'plus', 'times', 'greater', 'end', 'of', 'until', 'then', 'do', 'does', 'less', 'than', 'equals', 'power', 'remainder', 'loop', 'case', 'match', 'pipe', 'all', 'any', 'each', 'divide', 'modulo')
    # ^ match, case, all, any
    # ^ are not ASnake keywords, however can be reassigned in Python, so better to leave it in ASnakeKeywords
    metaPyCompat = {'pythonCompatibility','pycompat','pyCompatibility','pyCompat','pythonCompat'}
    metaPyVersion = {'version','pythonVersion','pyver','PythonVersion','pyVersion'}
    metaIgnoreIndent = {'ignoreIndentation','ignoreIndent','noindent','noIndent','noIndentation'}
    metaPyIs = {'pyis','pythonIs','pyIs','isPython','pythonis','isIdentity'}
    metaConditionalVersion = {'ifVersion','isVersion','ifVersionIs','isVersionIs','ifVersionGreaterThanOrEqualTo','ifver'}
    metaElseVersion = {'elseVersion','elseIfVersion','elsever'}
    metaFunctionLineWrap = {'funcWrap','functionLineWrap','functionWrap','fwrap'}
    metaPyFunc = {'funcPass','funcpass','passFunction','functionPass','pyfunc','pyFunc'}
    metaDefExp = {'defexp','defaultExpression','defaultPrint','expPrint','defprint'}
    metaIgnoreDefExpFunc = {'noDefExpOnFunc', 'defExpIgnoreFunction', 'defaultExpressionIgnoreFunction', 'ignoreDefExpFunction'}

    if compileTo == 'PyPy3' and (pythonVersion == latestPythonVersionSupported or (not isinstance(pythonVersion, str) and pythonVersion > 3.8)):
                                pythonVersion='3.10'
    elif compileTo == 'Cython': pythonVersion='3.6'
    elif compileTo == 'Pyston': pythonVersion='3.8'
    elif compileTo == 'MicroPython': pythonVersion='3.8'
    elif compileTo == 'Codon':  pythonVersion='3.10'

    implementation = compileTo if compileTo != 'Python' else 'CPython'
    code = [f"# Python{pythonVersion} for {implementation} compiled by ASnake "+ASnakeVersion]


    def fixVersionNumber(version):
        tmp = str(version).split('.')
        if len(tmp[-1]) == 1:
            # convert sub .10 versions to have a 0
            # like 3.7 -> 3.07
            version=float(tmp[0]+'.0'+tmp[1])
        return version
    pythonVersion=fixVersionNumber(pythonVersion)

    if isinstance(pythonVersion, str):
        pythonVersion=float(pythonVersion)

    if pythonVersion >= 3.10:
        codeDict['OF']='case'


    # meta
    if metaInformation:
        expPrint = metaInformation[1]
        ignoreIndentation = metaInformation[2]
        functionPassing = metaInformation[3]
        pyIs = metaInformation[4]
        autoEnumerate = metaInformation[5]
        intVsStrDoLen = metaInformation[6]
        metaDefaultExpressionWithFunction = metaInformation[7]
        functionLineWrap = metaInformation[8]
    else:
        expPrint = [0, 'print']
        ignoreIndentation = False
        functionPassing = False
        pyIs = False
        autoEnumerate = True
        intVsStrDoLen = True
        metaDefaultExpressionWithFunction = True
        functionLineWrap = True
        metaInformation = [{}, expPrint, ignoreIndentation, functionPassing, pyIs, autoEnumerate,
                           intVsStrDoLen, metaDefaultExpressionWithFunction, functionLineWrap]
        # ^ by having metaInformation store starting state, we can remember and reset it on each phase


    def makeToken(clone,value=None,type=None,lineno=None):
        tmptok=copy(clone)
        if value != None: tmptok.value=value
        if type != None: tmptok.type=type
        if lineno != None: tmptok.lineno=lineno
        return tmptok

    def convertEmojiToAscii(tok):
        if any(True for c in tok.value if unicodeCategory(c) in {'Zs', 'Cf', 'Cc'}):
            return AS_SyntaxError(f'Whitespace character in {tok.value} is not permitted.', None, tok.lineno, data)
        tmp = ''
        if compileTo == 'Cython':
            cutoff = 256  # cython likes ascii only
        else:
            cutoff = 9000  # is first emoji
        for c in tok.value:
            if ord(c) > cutoff or unicodeCategory(c) not in {'Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl', 'Other_ID_Start', 'Other_ID_Continue'}:
                if c == '.':
                    tmp += '.'
                elif tmp:
                    tmp += str(ord(c)) + 'e'
                else:
                    tmp += 'e' + str(ord(c)) + 'e'
            else:
                tmp += c
        return tmp

    def createFString(tok,token=-1):
        nonlocal lex, lexIndex
        # handle making FSTRING tokens for parsing inside of fstrings
        # turned out optStrFormatToFString needed this so its a function now
        tmpf = [] ; parts = []
        found = checkForEscape = isDict = False
        insideIndex = 0 ; bracketScope = 0

        for x in tok.value:
            # get quote type
            if x == '"':
                quote = x; break
            elif x == "'":
                quote = x; break
        for part in range(0, len(tok.value)):
            #print(tok.value[part],part,found,checkForEscape,parts,bracketScope)
            if found:
                if checkForEscape:
                    if tok.value[part] == ':' and insideIndex <= 0 and parts != []:
                        isDict = True
                    elif tok.value[part] == '[':
                        insideIndex += 1
                    elif tok.value[part] == ']':
                        insideIndex -= 1
                if tok.value[part] == '}':
                    if isDict:
                        parts[-1] = (parts[-1][0], parts[-1][1] - 1)
                        parts.append((False, part + 1))
                        isDict = found = False
                    elif bracketScope > 0: bracketScope-=1
                    else:
                        parts.append((False, part))
                        found = False
                elif tok.value[part] == '{':
                    bracketScope += 1

            elif not found and tok.value[part] == '{':
                tmp = 0
                while part + tmp < len(tok.value) - 1 and tok.value[part + tmp] == '{':
                    tmp += 1
                    checkForEscape = True
                found = True
                parts.append((True, part + tmp))
            elif part == len(tok.value) - 1:
                parts.append((True, part + 1))
        if parts != []:
            for part in range(0, len(parts)):
                if part == 0:
                    tmpf.append(makeToken(tok, tok.value[:parts[0][1]], "FSTR"))
                elif parts[part][0] == True:
                    tmpf.append(makeToken(tok, tok.value[parts[part - 1][1]:parts[part][1]], "FSTR"))
                elif parts[part][0] == False:
                    tmpf.append(makeToken(tok, tok.value[parts[part - 1][1]:parts[part][1]], "FPARSE"))
            # FSTR is bare string, FPARSE is code inside string to be parsed
        tok.type = 'IGNORE'
        adjust=1
        for thing in tmpf:
            if thing.type == 'FSTR':
                if token == -1:
                    lex.append(thing)
                else:
                    lex.insert(token+adjust, thing); adjust+=1
                lexIndex += 1
            else:
                pruneDict = tmpSkip = False
                if ':=' in thing.value:
                    lex.append(makeToken(tok, '{', 'LBRACKET'))
                    pruneDict=True
                tmp=thing.value[1:-1] if pruneDict else thing.value
                for tmptok in miniLex(tmp + ' '):
                    if tmptok.type in {'STRRAW', 'STRLIT'}: tmptok.type = 'STRING'
                    if tmptok.type == 'STRING':
                        if tmptok.value[0] == '"' and quote == '"':
                            tmptok.value = tmptok.value.replace('"', "'", 1)
                            tmptok.value = tmptok.value[:tmptok.value.rfind('"')] + "'"
                        elif tmptok.value[0] == "'" and quote == "'":
                            tmptok.value = tmptok.value.replace("'", '"', 1)
                            tmptok.value = tmptok.value[:tmptok.value.rfind("'")] + '"'
                    elif tmptok.type == 'ID' and tmptok.value.isascii() == False:
                        tmptok.value = convertEmojiToAscii(tmptok.value)
                    elif tmptok.type == 'INDEX' and lex[-1].type != 'TYPE' and '[' in tmptok.value:
                        if token == -1:
                            indexTokenSplitter(tmptok, True)
                        else:
                            adjust+=indexTokenSplitter(tmptok,True,token+adjust)
                        tmpSkip = True
                    elif tmptok.type == 'SCINOTAT':
                        tmptok.type='NUMBER'
                    elif tmptok.value.strip() in reservedIsNowVar:
                        tmptok.type = 'ID'
                    if tmpSkip: tmpSkip = False
                    elif token == -1:
                        lex.append(tmptok)
                    else: lex.insert(token+adjust,tmptok) ; adjust+=1
                    lexIndex += 1
                if pruneDict:
                    lex.append(makeToken(tok, '}', 'RBRACKET'))
                    lexIndex += 1
        #print(' '.join([ii.value for ii in lex if ii.type != 'IGNORE']))
        return tok

    def indexTokenSplitter(tok,fstringMode=False,token=0):
        nonlocal lex, lexIndex, parenScope
        tmpf = tok.value[tok.value.index('['):].split('[', 1)
        if tok.value.index('['):
            # tmp is if the index is preceded by an ID
            tmp = list(miniLex(tok.value[:tok.value.index('[')] + ' '))[0]
        else: tmp=False
        tmpscope = elementsAdded = 0
        def insertOrAppend(token,thing):
            nonlocal elementsAdded
            elementsAdded += 1
            if debug: print(elementsAdded,'---', thing)
            if token:
                lex.insert(token,thing)
                token-=1
            else:
                lex.append(thing)

        if tmp:
            insertOrAppend(token,tmp)
        tmpLI=token if token else -1
        tmpIter=list(miniLex(''.join(tmpf[1:]) + ' '))
        tmpIter.insert(0,makeToken(tok, '[', 'LINDEX'))
        if token: tmpIter=reversed(tmpIter)
        #print(''.join([i.value for i in tmpIter]))
        for i in tmpIter:
            i.lineno = lineNumber
            if i.type not in typeNewline:
                if i.type == 'LISTCOMP':
                    tmpPreColon=i.value.split(':')[0]
                    if tmpPreColon:
                        if tmpPreColon.isdigit():
                            insertOrAppend(token, makeToken(tok, tmpPreColon, 'NUMBER'))
                        else:
                            insertOrAppend(token,makeToken(tok, tmpPreColon, 'ID'))
                    insertOrAppend(token,makeToken(tok, ':', 'COLON'))
                    lex[tmpLI].lineno = lineNumber
                    insertOrAppend(token,[ii for ii in miniLex(i.value.split(':')[-1])][0])
                    lex[tmpLI].lineno = lineNumber
                elif i.type == 'INDEX':
                    for ii in miniLex(i.value[:-1]):
                        if ii.type == 'LISTCOMP':
                            insertOrAppend(token,[iii for iii in miniLex(ii.value.split(':')[0])][0])
                            lex[tmpLI].lineno = lineNumber
                            insertOrAppend(token,makeToken(tok, ':', 'COLON'))
                            lex[tmpLI].lineno = lineNumber
                            insertOrAppend(token,[iii for iii in miniLex(ii.value.split(':')[-1])][0])
                            lex[tmpLI].lineno = lineNumber
                        elif ii.type == 'INDEX':
                            for iii in miniLex(ii.value[:-1]):
                                if iii.type == 'ENDIF': iii.type = 'COLON'
                                elif iii.type == 'LIST': iii.type = 'LINDEX'
                                elif iii.type in {'LPAREN', 'FUNCTION'}: parenScope += 1
                                elif iii.type == 'RPAREN': parenScope -= 1
                                insertOrAppend(token, iii)
                                lex[tmpLI].lineno = lineNumber
                            insertOrAppend(token, makeToken(i, ']', 'RINDEX'))
                        else:
                            if   ii.type == 'ENDIF': ii.type = 'COLON'
                            elif ii.type == 'LIST' : ii.type = 'LINDEX'
                            elif ii.type in {'LPAREN','FUNCTION'}: parenScope += 1
                            elif ii.type == 'RPAREN': parenScope -= 1
                            insertOrAppend(token,ii)
                            lex[tmpLI].lineno = lineNumber
                    insertOrAppend(token,makeToken(i,']','RINDEX'))
                    lex[tmpLI].lineno = lineNumber
                else:
                    if i.type == 'ENDIF':
                        i.type = 'COLON'
                    elif i.type == 'LIST':
                        if tmpscope == 0:
                            i.type = 'LINDEX'
                        else:
                            tmpscope += 1
                    elif i.type == 'LISTEND':
                        if tmpscope == 0:
                            i.type = 'RINDEX'
                        else:
                            tmpscope -= 1
                    elif fstringMode and i.type == 'RBRACKET':
                        return
                    elif i.type in {'LPAREN','FUNCTION'}:
                        parenScope += 1
                    elif i.type == 'RPAREN':
                        parenScope -= 1
                    insertOrAppend(token,i)
                    lex[tmpLI].lineno = lineNumber
        if lex[tmpLI].type == 'LISTEND': lex[tmpLI].type = 'RINDEX'
        if not token: lexIndex+=elementsAdded-1
        return elementsAdded

    def stripStringQuotes(string: str):
        newString = ''
        quoteType = None
        stringStart = False
        escapeChar = False
        for char in string:
            if not stringStart:
                if quoteType == None and char in {'"', "'"}:
                    quoteType = char
                elif quoteType and char != quoteType:
                    stringStart = True ; newString += char
            else:
                if escapeChar:
                    escapeChar = False
                    newString += char
                else:
                    if char == quoteType:
                        return newString
                    elif char == '\\':
                        escapeChar = True
                    else:
                        newString += char

    def isANegativeNumberTokens(suspectedMinusIndex):
        # arg should be suspected minus
        if lex[suspectedMinusIndex].type == 'MINUS' and lex[suspectedMinusIndex+1].type == 'NUMBER' \
        and lex[suspectedMinusIndex-1].type not in {'ID','RPAREN','BUILTINF','NUMBER','STRING','RINDEX','LISTEND','RBRACKET'}:
            return True
        elif lex[suspectedMinusIndex].type == 'NUMBER' and lex[suspectedMinusIndex].value[0] == '-': return True
        return False


    def insertAtTopOfCodeIfItIsNotThere(line):
        nonlocal code
        if any(i for i in code if line in i):
            pass
        else:
            code.insert(1, line)

    def findEndOfExpression(lexIndex):
        tmpParenScope = 0
        for tmpi in range(lexIndex,0,-1):
            if lex[tmpi].type in typeNewline+typeConditionals:
                return tmpi+1
            elif lex[tmpi].type == 'LPAREN' or (lex[tmpi].type == 'FUNCTION' and lex[tmpi].value.endswith('(')):
                tmpParenScope += 1
            elif lex[tmpi].type == 'RPAREN':
                tmpParenScope -= 1
            #elif lex[tmpi].type in typeCheckers+typeMops and tmpParen == 0:
            #    return tmpi+1

    def endOfLineChores(tok):
        nonlocal constWrap, line, rParen, incWrap, bigWrap, inLoop, parenScope, indent, startOfLine
        if constWrap:
            if len([i for i in line if ',' in i]) > 1:
                line.append(')')
            elif compileTo == 'MicroPython' and len([i for i in line if 'const(' in i]) > 0:
                line.append(')')
            else:
                line.append(',)')
            if comment: line.append(' # constant')
            constWrap = False

        tmp = ''  # for big functions but parsing ASnake inside
        while rParen > 0:
            tmp += ')' ; rParen -= 1;
            parenScope = parenScope - 1 if parenScope > 0 else 0
        if tmp != '':
            if not line: pass
            elif line[-1].endswith('\n'): line.insert(-1, tmp)
            else: line.append(tmp)

        # for LOOP syntax i guess
        if inLoop[0] == None:
            if tok.type not in typeNewline: line.append(':\n')
            indent += prettyIndent
            startOfLine = True
            inLoop[0] = True
            if len(inLoop) > 2:
                line.append(decideIfIndentLine(indent, inLoop[2]))
                del inLoop[2]

        for iw in range(0,len(incWrap)):
            if incWrap[iw][0] != '':
                if isinstance(line, str): line = [line]
                while incWrap[iw][1] > 0:
                    line.append('\n') ; startOfLine = True
                    line.append(decideIfIndentLine(indent, incWrap[iw][0]))
                    incWrap[iw][1] -= 1
        incWrap=[]

        bigWrap = False

    def addParenUntilDone():
        nonlocal wrapParenEndOfLine, lex, lexIndex, parenScope
        # adds ) until wrapParenEndOfLine is 0, intended for right before a line ends
        if lexIndex < 2: return
        if wrapParenEndOfLine < 0: wrapParenEndOfLine=0 ; return
        if lex[lexIndex].type == 'ENDIF': position=lexIndex
        else: position=lexIndex+1
        while wrapParenEndOfLine:
            lex.insert(position,makeToken(lex[-1], ')', 'RPAREN'))
            lexIndex += 1
            wrapParenEndOfLine -= 1
    def cullOutOfScopeFunctions():
        nonlocal definedFunctions , currentTab
        for f in set(definedFunctions):
            # if a defined function is no longer in scope, remove it from tracking
            if currentTab < definedFunctions[f]:
                del definedFunctions[f]

    def metaHandling(metaText, theMeta):
        tmp = metaText.split('=')
        returnValue = theMeta
        if len(tmp) > 1:
            tmp = tmp[1].lower().strip()
            if tmp in {'true', 'yes', 'on'}:
                returnValue = True
            elif tmp in {'no', 'false', 'off'}:
                returnValue = False
            else:
                returnValue = True
        else:
            returnValue = not returnValue
        return returnValue

    def debugPrintWholeScript():
        [print(_.value, end=' ') for _ in lex if _.type != 'IGNORE']



    prettyIndent = 4
    # v converts leading tabs to space v
    leadingTabs = REcompile(r"""(?<=\n)\t+(?![\w\s]*('|"){3})""")
    data = leadingTabs.sub(lambda m: ' ' * (len(m.group()) * prettyIndent), data)

    #meta
    if metaInformation:
        inlineReplace=metaInformation[0]

    metaConditionalVersionTriggered = False

    comments=[]
    keepAtTop=[] # for lines that should be kept up top

    lexer = Lexer()
    lex=[]
    lexIndex=0
    currentTab=0
    lineNumber=0 # for source code
    bracketScope=0
    parenScope=0
    pipeWrap=0 # keeps track of amount of PIPEGO
    lastIndent=[0]
    tabBackup=[currentTab,lastIndent[:]]
    inFrom=False
    crunch=False # for smooshing values into the latest lexIndex
    willPipe=False # activates piping on next token
    reservedIsNowVar=[]
    deleteUntilIndentLevel = (False,0)
    pyCompatibility=False
    definedFunctions={}
    wrapParenEndOfLine=0
    definedVars={}


    preLex=list(lexer.tokenize('\n'+data+' \n')) ; preLexIndex=0
    # ^^ needs newline at the start
    # wow a prephase for the prephase, brilliant design. sarcasm.
    token=0
    while token <= len(preLex)-1:
        if preLex[token].type in {'METALBKT', 'METARBKT'}:
            if ignoreIndentation:
                if preLex[token].type == 'METALBKT':
                    preLex[token].type = 'THEN' ; preLex[token].value = 'do'
                else:
                    preLex[token].type = 'END' ; preLex[token].value  = 'end'
                    preLex.insert(token+1,makeToken(preLex[token],';','THEN'))
                if preLex[token-1].type in typeNewline: preLex[token-1].type='IGNORE'
            else:
                return AS_SyntaxError(f'Meta brackets can only be called when ignoreIndent meta is on.', '$ignoreIndent', None, data)
        elif preLex[token].type == 'META':
            metaCall = preLex[token].value.replace('$', '').replace(' ', '')
            if metaCall.startswith(tuple(metaIgnoreIndent)):
                ignoreIndentation = metaHandling(preLex[token].value, ignoreIndentation)
        token+=1
    del token
    preLex=[t for t in preLex if t.type != 'IGNORE']
    ignoreIndentation = metaInformation[2]

    showWarning=False
    # start of prephase
    # warning to self, when checking previous token, do not do lexIndex-1, lexIndex is the previous, as current token hasn't been added yet
    for tok in preLex:
        preLexIndex+=1
        if deleteUntilIndentLevel[0]:
            if currentTab > deleteUntilIndentLevel[1] and tok.type not in typeNewline:
                continue
            elif tok.type == 'NEWLINE' and lex[lexIndex].type != 'TAB': deleteUntilIndentLevel = (False,0)
        if crunch:
            if tok.type in typeNewline:
                crunch = False
            else:
                lex[lexIndex].value += tok.value
                continue
        if willPipe:
            if tok.type in {'ID','BUILTINF','TYPE'}:
                if tok.type == 'BUILTINF' and tok.value[-1] == '(':
                    return AS_SyntaxError(
                        f"Don't pipe to function that calls parenthesis.",
                        '12 to str', lineNumber, data)
                elif tok.type in {'ID','BUILTINF'} and not tok.value.isascii():
                    tok.value=convertEmojiToAscii(tok)
                if lex[lexIndex-3].type == 'LOOP' and lex[lexIndex-1].type == 'ID' and lex[lexIndex-1].value not in definedVars:
                    # loop 3 c to chr to print --> loop 3 c do c to chr to print
                    # keeps old behaviour while still converting to func in prephase
                    tmp=lex[lexIndex-1].value
                    lex.insert(lexIndex-1, makeToken(lex[lexIndex], 'do', 'THEN'))
                    lex.insert(lexIndex-1, makeToken(lex[lexIndex], tmp , 'ID'  ))
                    lexIndex+=2
                tmpValue = f'{tok.value}('
                if lex[lexIndex].value.strip() == 'to':
                    if pipeWrap:
                        tok.type = 'RPAREN' ; tok.value = ')'
                        del lex[lexIndex] ; lexIndex-=1
                        for tmpi in range(lexIndex-1,0,-1):
                            if lex[tmpi].type == 'PIPEGO':
                                lex[tmpi].type = 'FUNCTION'
                                lex[tmpi].value = tmpValue
                                pipeWrap-=1
                                break
                    elif lex[lexIndex-1].type in {'NUMBER','STRING','ID','BUILTINF','BOOL'} and lex[lexIndex-2].type != 'LOOP':
                        if lex[lexIndex-1].type == 'NUMBER' and lex[lexIndex-2].type == 'MINUS' and lex[lexIndex-3].type in typeNewline+typeConditionals+('COMMA',):
                            # include minus
                            insertAt = lexIndex-2
                        elif lex[lexIndex-1].type == 'BUILTINF' and lex[lexIndex-1].value[0] == '.':
                            # for .thing
                            insertAt = lexIndex-2
                            while lex[insertAt].type != 'BUILTINF' or lex[insertAt].value[0]=='.':
                                insertAt -= 1
                        else:
                            insertAt = lexIndex-1
                        tok.type = 'IGNORE'
                        lex[lexIndex].type = 'RPAREN' ; lex[lexIndex].value = ')'
                        lex.insert(insertAt,makeToken(tok,tmpValue,'FUNCTION'))
                        lexIndex+=1
                    elif lex[lexIndex-1].type == 'RPAREN':
                        tok.type = lex[lexIndex].type = 'IGNORE'
                        tmpParenScope=-1
                        check=True # check if inherit the tuple
                        for tmpi in range(lexIndex-2,0,-1):
                            if lex[tmpi].type == 'RPAREN':
                                tmpParenScope-=1
                            elif lex[tmpi].type in {'LPAREN','FUNCTION'}:
                                tmpParenScope+=1
                                if tmpParenScope >= 0 and lex[tmpi-1].type != 'BUILTINF':
                                    if check and lex[tmpi].type == 'LPAREN':
                                        lex[tmpi].type='FUNCTION'
                                        lex[tmpi].value=tmpValue
                                    elif not check and lex[tmpi].type == 'LPAREN':
                                        # inherit the tuple
                                        lex[tmpi].type = 'FUNCTION' ; lex[tmpi].value = tmpValue
                                    else: # function
                                        lex[lexIndex].type = 'RPAREN' ; lex[lexIndex].value = ')'
                                        lex.insert(tmpi,makeToken(tok,tmpValue,'FUNCTION'))
                                        lexIndex+=1
                                    break
                                elif lex[tmpi-1].type == 'BUILTINF': check=False
                            elif lex[tmpi].type == 'COMMA':
                                check=False
                            elif lex[tmpi].type in typeNewline:
                                if not check:
                                    lex[lexIndex].type = 'RPAREN' ; lex[lexIndex].value = ')'
                                    lex.insert(tmpi+1, makeToken(tok, tmpValue, 'FUNCTION')) ; lexIndex+=1
                                break
                elif lex[lexIndex].value.strip() == 'into':
                    tok.type = 'IGNORE' ; tmpAddBy=None
                    for tmpi in range(lexIndex-1, -1, -1):
                        if lex[tmpi].type in typeNewline+('ASSIGN','PYDEF','DEFFUNCT')+typeConditionals:
                            tmp=True
                            if lex[tmpi].type == 'ASSIGN':
                                if ':' in lex[tmpi].value: tmp=False
                                elif 'is' in lex[tmpi].value and lex[tmpi-1].type in typeAssignables and lex[tmpi-1].type not in {'ID','BUILTINF','RINDEX'}:
                                    tmp=False
                                elif '=' == lex[tmpi].value:
                                    for tt in range(tmpi-1,-1,-1):
                                        if lex[tt].type in {'LPAREN','FUNCTION'}:
                                            tmp=False
                                        elif lex[tt].type in typeNewline+('PYDEF','DEFFUNCT'):
                                            if lex[tt].type in {'PYDEF','DEFFUNCT'}: tmpAddBy=tt
                                            if tmp: break
                                            else: tmpAddBy=tt ; tmp=True ; break
                            if tmp:
                                lex[lexIndex].type = 'RPAREN' ; lex[lexIndex].value = ')'
                                lex.insert(tmpAddBy+1 if tmpAddBy!=None else tmpi+1,makeToken(tok,tmpValue,'FUNCTION'))
                                lexIndex+=1 ; break


            else:
                return AS_SyntaxError(
                    f"Line {lineNumber} pipes to invalid syntax, '{tok.value}'. Can only pipe to functions.",
                    '12 to str', lineNumber, data)
            willPipe=False

        if not lex:
            tmptok=copy(tok)
            tok.type='IGNORE'
            lex.append(tok)
            tok.type=tmptok.type
            del tmptok
        elif debug and len(lex) > lexIndex: print(f'lex={lexIndex} ln={lineNumber} ps={parenScope} lexType={lex[lexIndex].type}\ttype={tok.type}, value={tok.value}')

        if tok.type in {'COMMENT', 'IGNORENL', 'INDEX', 'NEWLINE', 'PYDEF', 'PYPASS', 'STRING', 'STRLIT', 'STRRAW', 'TAB', 'THEN', 'TYPEWRAP'}:
            # ^ every type that can have a newline should be included
            lineNumber = lineNumber+tok.value.count('\n')
        tok.lineno = lineNumber

        if tok.type == 'NUMBER':
            if pythonVersion < 3.06 and '_' in tok.value: tok.value = tok.value.replace('_', '')
            lex.append(tok)
        elif tok.type in {'HEXDEC','SCINOTAT'}:
            tok.type = 'NUMBER' ; lex.append(tok)
        elif tok.type == 'IMPORT':
            if tok.value.strip() == 'import this': # The ASnake Rebellion
                code.append(''.join(("import zlib as ASnake\n","myDress = ",str(b'x\x9c\x95UK\x8f\xda0\x10\xbe\xf3+\xac\xed\x05\xa4he;\xce\xc3U\xf7\xd4\xd3J\x95\xfa:"\x0e\x04\xc2n*6a\t\xa9\xd4\x7f_?\xc7\xe3$@\x8bD\x94xf\xbe\xf9\xe6\xe9E\xf5\xb4.EB\x185\x0f\x96\x90\x94\'$\xcf\x12R\xa6\xea\x80\xd1\x84\xc8BK\n$.\xb9\xfb\x90\xa5{a\xb4\x84G\xa6\xed\x983\x16\xc2\x9a\x18M\x8e\xe0\xadC&\x9c\x85>.\xa8\xb7\xe4\xfa\xcd\xfd\x8b\xd4J\x19\x93\x00\\\x803\xea\x85\xc2\x11e9\x0e\xc6\x9c\x83\x7f\xcf\xdf\xd8\x1am\xce\x11\xad`k\x91\xe8\xc8\xd4\x9e\x06\'&\n\x8e\xf9\x88\xdcr\xce\x0b\xaf-"\xb7\x1c"p\xb4\x8co\x14\x94\xd1gY\x9cU>\xe21\xc2\t\xf8\xf6h\x9a\x00\x83\xe8\xb9\x95\x05\x14\xc9k\xce\xe4\xc4*P@\xc9<\n\xa8{\x04\xf4\x95y\x1e\x19\xa4\xc7\xd4\x8cC(\x0c\xf1\x80\xae\xb3\x14CaSL\xeb:\x96:\x932\x8aE\'\xd3\x13\xf4m\xa3U\xa2r:0\xdf\xdd3y\x94\xd0h3\xf4]o\xc6>o\xa7\xf6\xff\xf1\xee\xc7\xe0\x99\xdb/9\x89\xc3S*r(a\x8e\xb5!\xd6\x12[\xc1\xa8\xc7\xe3\x80:L\xe0v\x0f\xbd!\xa2r\xb1+\x03\x95\xcf\x02\x8f\x92?368|4\xad\xfe\x98{\x16\x01<0\x80md\x08\xb3r\xe4\xca*\x85\xd0`\xa9\x85a\x93(\x9b\xa65\xf9\xec\xec\x86\xba\xc00Hhg\x08\xc8\x16\x07\x86(\x8d\xca\x18/\x1a\x81\n^xWx\xc3\x8c{\n\xad\xa6h\xf2\xd1\x1c\x8d&-^\xa1Yl\x19oL9\xe6\x8e\x86g\xbe\xae7W\xd2\xb4\x82q\xf1\xd1J\xa4\x8e\xe4\xbd\x056\xe3\xdd\xae\x92\xb0\xa7s\xcc\n\xeah{\xc4\xdfi\xeeAq\xa83\xc8W\xb6[H\x94KP\x1e\xddK\xd1\xdd\x18O2^\xb4Z(1\xe10\x02\xf9\xa4\xc6\x02\x94T[xIX\x19\x05.\xed?\\\xee\xf6\x9e\xb5\xf1\xf9I@\xb7W\xd4\xc8\xa3}\x92n\x16\xc7\xa6\xbf|=\xfc\xa8wu{\xf9v\xae\xfbf\xaf^\xfa\xe7\xef?w\x9d\xfazZo\x16\x87\xeeL\xaa\x8a4-9o\xdb\x97zI\x93c\xdd.\xab\xd5\xea#\xf9\xf0L\x8e\xc3o2(\xf9\x82\xa8\xdfm\xb0\xc7\xed\xe9T\xb7\xfbe\xb5\xae\xaa\xcd\xca\x18Tu\x7f\xf9Ro_\x86\xfa\xf3\xeb\xf6\xed\xd4t\xad\xf6\xa8%\xdak\xf3>\xf6z\xdb\x81\xa2dl\xf5\xaf9\xdca\xb3n\xde7\xe4\x13a)Uq\xfc\xe9\x06\xd2\xf4d?\xbcU\x1d@\xcc\x13\xf4Q\xec^\xcfw\xe8h\x0f\xab\xd5\xe2tn\xda\xcb\xf2\xe1\xe1\xf1W\xd7\xa8\xbcM\x00\x95\xca_\x02|\xfa\xf9'),"\n# who wears myDress the best?\nexec(ASnake.decompress(myDress)) ; del ASnake")))
                lexIndex-=1
            elif '__future__' in tok.value:
                keepAtTop.append(tok) ; lexIndex-=1
            elif optimize and ',' in tok.value and 'from ' not in tok.value:
                # splits up multi imports on one line to multiple lines so that optFromFunc can utilize it better
                tmp=tok.value.split(',')
                tok.value=tmp[0]
                lex.append(tok)
                tmp=tmp[1:]
                for t in tmp:
                    lex.append(makeToken(tok, 'then', 'THEN'))
                    if debug: print('--',lex[-1])
                    lex.append(makeToken(tok, f'import {t.strip()}', 'IMPORT'))
                    if debug: print('--',lex[-1])
                    lexIndex += 2
            else:
                lex.append(tok)
        elif tok.type == 'ID':
            if tok.value in defaultTypes and tok.value not in reservedIsNowVar:
                tok.type='TYPE' # dumb fix for it not detecting types
            elif tok.value.isascii() == False:
                if any(True for c in tok.value if unicodeCategory(c) in {'Zs', 'Cf', 'Cc'}):
                    return AS_SyntaxError(f'Whitespace character in {tok.value} is not permitted.', None, tok.lineno, data)
                tok.value=convertEmojiToAscii(tok)
            elif functionLineWrap and not functionPassing and not pyCompatibility and tok.value in definedFunctions \
            and lex[lexIndex].type not in {'PIPE','PIPEGO'} and not (lex[lexIndex].type == 'RETURN' and lex[lexIndex].value.strip() == 'del'):
                wrapParenEndOfLine += 1 ; tok.value+='(' ; tok.type = 'FWRAP'
            lex.append(tok)
        elif tok.type == 'TYPEWRAP':
            tok.value=tok.value.replace(':','').replace(' ','')
            if '#' in tok.value: # remove comment from typewrap
                comments.append(['#'+tok.value.split('#',1)[-1],lexIndex,tok.lineno])
                tok.value=tok.value.split('#',1)[0]
            if lex[lexIndex].type == 'DEFFUNCT': tok.type='TYPE'
            if tok.value in reservedIsNowVar:
                tok.type='ID' ; lex.append(tok)
                lex.append(makeToken(tok,'do','THEN')) ; lexIndex+=1
            else: lex.append(tok)
        elif tok.type in {'TAB','THEN'}:
            if lex[lexIndex].type == 'IGNORENL':
                lex[lexIndex].type='IGNORE'
                tok.type='IGNORE'
                lex.pop()
                lexIndex-=2
            else:
                if not pyCompatibility and lex[lexIndex].type == 'NUMBER' and lexIndex > 1 and lex[lexIndex-1].type == 'INS' and lex[lexIndex-1].value.strip() == 'in':
                    # ASnake syntax:  in 12 --> range(12)
                    tmp = False
                    for t in range(lexIndex-1, 0, -1):
                        if lex[t].type in typeNewline:
                            break
                        elif lex[t].type == 'FOR':
                            tmp = True
                    if tmp:
                        lex.insert(lexIndex , makeToken(tok, 'range(', 'FUNCTION'))
                        lex.append(makeToken(tok, ')', 'RPAREN'))
                        lexIndex += 2
                elif lex[lexIndex].type != 'LISTEND':
                    # makes it so tabs inside of lists dont affect tabbing
                    tmp=-1
                    for tmpi in range(lexIndex,0,-1):
                        if tmp >= 0: break
                        elif lex[tmpi].type in typeNewline: break
                        elif lex[tmpi].type != 'STRING':
                            tmp += lex[tmpi].value.count('[')
                            tmp -= lex[tmpi].value.count(']')
                    if tmp >= 0: tok.type='IGNORE'
                if bracketScope > 0 or (parenScope > 0 and tok.type != 'THEN'): tok.type = 'IGNORE'
                if tok.type=='IGNORE': lex.append(tok)
                else:
                    if lex[lexIndex].type == 'TAB':
                        currentTab=tabBackup[0]
                        lastIndent=tabBackup[1][:]
                        lexIndex-=1
                        lex.pop()
                    elif lex[lexIndex].type == 'NEWLINE':
                        while lexIndex > 0 and lex[lexIndex].type == 'NEWLINE':
                            lexIndex -= 1
                            lex.pop()
                        currentTab = tabBackup[0]
                        lastIndent = tabBackup[1]
                    elif lex[lexIndex].type == 'THEN':
                        while lexIndex > 0 and lex[lexIndex].type == 'THEN':
                            lexIndex -= 1
                            lex.pop()
                    elif ignoreIndentation and lex[lexIndex].type == 'END':
                        lex.pop() ; lexIndex-=1
                        if lex[lexIndex].type == 'TAB':
                            lex.pop() ; lexIndex-=1
                    if ignoreIndentation and tok.type == 'THEN':
                        tok.type='TAB'

                    # ---- this section converts 2 spaces to 4 (prettyIndent). very important
                    if tok.type == 'TAB':
                        if ignoreIndentation:
                            currentTab = (len(lastIndent) - 1) * prettyIndent
                            if len(lastIndent) == 1: currentTab = 0
                        else:
                            if '>>> ' in tok.value: tok.value=tok.value.replace('>>> ','')
                            elif '... ' in tok.value: tok.value=tok.value.replace('... ','')
                            tabBackup=[currentTab,lastIndent[:]]
                            tabCount=tok.value.replace('\t',' ').count(' ')
                            if tabCount > lastIndent[-1]:
                               currentTab+=prettyIndent
                               lastIndent.append(tabCount)
                            elif tabCount < lastIndent[-1]:
                               if tabCount not in lastIndent:
                                    while tabCount not in lastIndent:
                                        tabCount+=1
                               while tabCount != lastIndent[-1]:
                                    lastIndent.pop()
                                    if lastIndent == [0]: break
                               currentTab=(len(lastIndent)-1)*prettyIndent
                               if len(lastIndent)==1: currentTab=0
                            if debug: print('currentTab:',tabCount,'finalIndent:',currentTab,'lastIndent:',lastIndent)
                    tok.value='\n'+' '*currentTab
                    # ---- end

                    cullOutOfScopeFunctions()
                    addParenUntilDone()
                    definedVars = {var:definedVars[var] for var in definedVars if definedVars[var]['indent'] <= currentTab}

                    lex.append(tok)
            inFrom=False
        elif ignoreIndentation and tok.type == 'END':
            if lex[lexIndex].type == 'TAB':
                lastIndent = tabBackup[1][:]
                lexIndex -= 1
                lex.pop()
            if len(lastIndent) > 1:
                lastIndent.pop()
                currentTab = (len(lastIndent) - 1) * prettyIndent
                if len(lastIndent) == 1: currentTab = 0
            else:
                currentTab = 0
            tabBackup = [currentTab, lastIndent[:]]
            lex.append(tok)
        elif tok.type == 'INDEX': # grouping [0][1] indexes
            if tok.value.startswith(')'):
                tok.value=tok.value[1:]
                while tok.value[0] in (' ','\t'): tok.value=tok.value[1:]
                tmptok=copy(tok) ; tmptok.type='RPAREN' ; tmptok.value=')'
                lex.append(tmptok) ; lexIndex+=1 ; parenScope-=1
            if tok.value == '[]':
                tmptok=copy(tok)
                tmptok.type='LIST' ; tmptok.value='['
                lex.append(tmptok) ; del tmptok
                tok.type='LISTEND' ; tok.value=']'
                lexIndex+=1
            elif lex[lexIndex].type=='INDEX' and tok.value.startswith('['):
                tok.value=f'{lex[lexIndex].value}{tok.value}'
                lex[lexIndex].type='IGNORE'
            elif tok.value.split('[')[0] in {'list','tuple','dict','set'} and tok.value.split('[')[1].split(']')[0] in defaultTypes:
                # typing inside elements like dict[int, str]
                tok.type='TYPE'
            if tok.type != 'TYPE' and len(REfindall('''(?!('|").*)for (?!.*('|"))''',tok.value)) > 0:
                # the dumb INDEX regex will match list comps.
                # this will detect 'for' inside of it (if not surrounded by quotes)
                # it will parse the inside as ASnake, meanwhile surrounded by [ ]
                tmpval=tok.value.lstrip('[').rsplit(']',1)[0]
                tmpCount=0
                while tok.value[tmpCount] == '[':
                    tmpCount+=1
                for t in range(tmpCount):
                    tmptok=copy(tok) ; tmptok.value='[' ; tmptok.type='LIST'
                    lex.append(tmptok) ; del tmptok ; lexIndex+=1
                for i in miniLex(tmpval+' '):
                    if i.type not in typeNewline: # genius, you can go crazy with whitespace in listcomps
                        if i.type in {'LPAREN','FUNCTION'}: parenScope+=1
                        elif i.type == 'RPAREN': parenScope-=1
                        i.lineno = lineNumber
                        lex.append(i)
                        lexIndex+=1
                        if debug: print('--',i)
                tmptok=copy(tok) ; tmptok.value=']' ; tmptok.type='LISTEND'
                lex.append(tmptok) ; del tmptok
            else:
                if tok.type != 'TYPE' and '[' in tok.value:
                    indexTokenSplitter(tok)
                else:
                    lex.append(tok)
        elif tok.type == 'FROM':
            check = False
            for tmpi in range(lexIndex, 0, -1):
                if lex[tmpi].type == 'RETURN' and 'raise' in lex[tmpi].value:
                    check = True
                elif lex[tmpi].type in typeNewline:
                    break
            if check:
                # from can be used on raise, so do not take it as ASnake from
                tok.type = 'PYPASS'
                tok.value = f"p!{tok.value} !p"
            else:
                inFrom=True
            lex.append(tok)
        elif tok.type in {'STRAW','STRLIT','STRING'}:
            if tok.type in {'STRRAW','STRLIT'}: tok.type='STRING'
            if tok.value[0] in 'fF':
                if len([i for i in ('{','}') if i not in tok.value])>0:
                    tok.value=tok.value[1:] # optimization if f-string doesnt use {} then convert to regular string, better performance
                    # produces better behaviour for the compiler, so do it even if optimization isn't on
                else:
                    tok=copy(createFString(tok))
            lex.append(tok)
        elif tok.type == 'META':
            if tok.value.replace('$', '').replace(' ', '').startswith('def'):
                if tok.value == '$def':
                    return AS_SyntaxError(f'Meta $def must be given a name and = to assign.',f'$ def thing = 2 + 2', tok.lineno, data)
                # hacky thing to make def an alias of inline. do dont make it elif
                if 'def' in tok.value and (tok.value[4]==' ' or tok.value[tok.value.index('def')+3]==' '):
                    tok.value='$ inline '+tok.value.split('def',1)[-1]
            metaCall = tok.value.replace('$', '').replace(' ', '')
            if metaCall.startswith('inline'):
                if '=' not in tok.value:
                    return AS_SyntaxError(f'Meta $inline must be given a name and = to assign.', f'$ def thing = 2 + 2',tok.lineno, data)
                name=''.join(tok.value.split('=')[0].split('inline')[1]).replace(' ','')
                value='='.join(tok.value.split('=')[1:])
                inlineReplace[name] = ''.join(value)
                lexIndex-=1
            elif metaCall.startswith(tuple(metaPyVersion)):
                try:
                    tmp=float(fixVersionNumber(tok.value.split('=')[-1].strip()))
                    if tmp != pythonVersion:
                        code.append(f'# Compile target changed to {compileTo}{tmp}')
                    pythonVersion=tmp
                except ValueError:
                    return AS_SyntaxError(f'Meta {tok.value.split("=")[0].strip()[1:]} must be given a float/decimal.',f'$ {tok.value.split("=")[0].strip()[1:]} = 3.8', None, data)
                lexIndex -= 1
            elif metaCall.startswith(tuple(metaIgnoreIndent)):
                ignoreIndentation = metaHandling(tok.value, ignoreIndentation)
                lexIndex -= 1
            elif metaCall.startswith(tuple(metaConditionalVersion)):
                error=False
                tmp = tok.value.split('=')
                if len(tmp) > 1:
                    tmp = fixVersionNumber(tmp[1])
                    try:
                        tmp = float(tmp)
                    except ValueError:
                        error = True
                    if not error:
                        if pythonVersion >= tmp:
                            lex.append(makeToken(tok, 'if', 'IF'))
                            lex.append(makeToken(tok, 'True', 'BOOL'))
                            lex.append(makeToken(tok, 'do', 'THEN'))
                            lexIndex+=2
                        else:
                            deleteUntilIndentLevel = (True, 0 if lex[lexIndex].type == 'NEWLINE' else currentTab)
                            lexIndex-=1
                        metaConditionalVersionTriggered = tmp
                else:
                    error=True
                if error:
                    tmp = tuple(metaConditionalVersion)[0]
                    for i in tuple(metaConditionalVersion):
                        if metaCall.startswith(i):
                            tmp = i ; break
                    return AS_SyntaxError(f'Meta {tmp} must be given a float/decimal which represents a Python version.',f'$ {tmp} = 3.8', None, data)
            elif metaCall.startswith(tuple(metaElseVersion)):
                if '=' in metaCall:
                    return AS_SyntaxError(
                        f'Meta ${metaCall} must not have a =. It triggers when it is lower than previous {tuple(metaConditionalVersion)[0]} meta.',
                        f'$ {metaCall.split("=")[0]}', None, data)
                if metaConditionalVersionTriggered:
                    if pythonVersion >= metaConditionalVersionTriggered:
                        deleteUntilIndentLevel = (True, 0 if lex[lexIndex].type == 'NEWLINE' else currentTab)
                        lexIndex -= 1
                    else:
                        lex.append(makeToken(tok, 'if', 'IF'))
                        lex.append(makeToken(tok, 'True', 'BOOL'))
                        lex.append(makeToken(tok, 'do', 'THEN'))
                        lexIndex += 2
                    metaConditionalVersionTriggered = False
                else:
                    return AS_SyntaxError(
                        f'Meta ${metaCall} must follow after a {tuple(metaConditionalVersion)[0]} meta.',
                        f'$ {tuple(metaConditionalVersion)[0]} = 3.8\n\t\tprint 3.8\n\t${metaCall}\n\t\tprint "elsed!"', None, data)
            elif metaCall.startswith(tuple(metaPyCompat)):
                pyCompatibility = metaHandling(tok.value, pyCompatibility)
                lex.append(tok)
            elif metaCall.startswith(tuple(metaPyFunc)):
                functionPassing = metaHandling(tok.value, functionPassing)
                lex.append(tok)

            elif tok.value.split('=')[0].replace(' ','').replace('$','') in inlineReplace or any(i for i in inlineReplace if f'${i}' in tok.value):
                if '=' in tok.value and tok.value.replace('$','').split('=')[0].strip() in metaDefExp:
                    lex.append(tok)
                else:
                    if debug: print('inlineReplace',inlineReplace)
                    while '$ ' in tok.value: tok.value = tok.value.replace('$ ', '$')
                    tmpAddThisInline=[]
                    def inlineReplaceFunc(tok):
                        nonlocal lexIndex, tmpAddThisInline
                        while tok.value!='' and len([i for i in inlineReplace if f'${i}' in tok.value]) > 0:
                            tmp=tok.value.split()[0]
                            if ',' in tmp: tmp=tmp.split(',')[0]
                            tmp=tmp.strip()
                            tmp=[i for i in inlineReplace if f'${i}' == tmp]
                            if debug: print(tok.value)
                            if tmp:
                                tmp=tmp[0]
                                for t in miniLex(inlineReplace[tmp]+' '):
                                    if t.type == 'ID' and t.value in defaultTypes and t.value not in reservedIsNowVar:
                                        if preLex[preLexIndex].type == 'PIPE': t.type = 'FUNCTION'
                                        else: t.type = 'TYPE'
                                    if t.type == 'META':
                                        inlineReplaceFunc(t)
                                    else:
                                        t.lineno = lineNumber
                                        tmpAddThisInline.append(t)
                                        if debug: print('--',t)
                                #print(tok.value.lstrip('$'+tmp)) ; exit()
                                while tok.value[0]==' ': tok.value=tok.value[1:]
                                tok.value=tok.value.lstrip('$'+tmp)
                            for t in miniLex(tok.value.rsplit('$')[0]+' '):
                                tok.value=tok.value.replace(t.value,'')
                                t.lineno = lineNumber
                                tmpAddThisInline.append(t)
                                if debug: print('---',t)
                    lexIndex-=1 # cuz the meta counts as a token i think
                    inlineReplaceFunc(tok)
                    for t in reversed(tmpAddThisInline): preLex.insert(preLexIndex,t)
            elif metaCall.startswith(tuple(metaFunctionLineWrap)):
                functionLineWrap = metaHandling(tok.value, functionLineWrap)
                lex.append(tok)
            else: lex.append(tok)
        elif tok.type == 'LISTCOMP':
            check=False # checks for    if thing > 4: doStuff
            for tmpindex in range(len(lex)-1,0,-1):
                if lex[tmpindex].type in typeNewline:
                    break
                elif lex[tmpindex].type in typeConditionals or lex[tmpindex].type in {'WHILE','FOR'}:
                    check=True ; break

            if tok.value.startswith('else'):
                # else: gets detected as LISTCOMP , so convert to ELSE
                tok.type='ELSE' ; lex.append(tok)
                # then split the string and detect & insert the next token
                tmptok=copy(tok)
                tmptok.value=''.join(tok.value.split(':')[1:])
                tmptok.type=[i for i in miniLex(tmptok.value+' ')][0].type
                lex.append(tmptok) ; lexIndex+=1
            elif lex[lexIndex].type == 'IF' or check or bracketScope>0:
                tmptok=copy(tok)
                for t in miniLex(''.join(tmptok.value.split(':')[:1])):
                    t.lineno = lineNumber
                    lex.append(t) ; lexIndex += 1
                    if debug: print('--', t)


                if bracketScope<=0:
                    tmptok = copy(tok)
                    tmptok.type = 'THEN' ; tmptok.value = 'then'
                    lex.append(tmptok) ; lexIndex += 1
                else:
                    lex.append(makeToken(tok, ':', 'COLON'))
                    lexIndex+=1


                tmptok=copy(tok)
                tmptok.value=''.join(tok.value.split(':')[1:]).strip()
                if tmptok.value not in ('',' '):
                    tmptok.type=[i for i in miniLex(tmptok.value+' ')][0].type
                    tmptok.lineno=lineNumber
                    lex.append(tmptok)
            else: lex.append(tok)
        elif tok.type in {'NEWLINE','COMMENT'}:
            if lexIndex>1 and lex[lexIndex].type == 'TAB':
                lex.pop() ; lexIndex-=1
                if len(lastIndent)>1:
                    # if last was tab, revert to not mess up indentation scope
                    currentTab=tabBackup[0]
                    lastIndent=tabBackup[1][:]
            if tok.type == 'COMMENT':
                if tok.value[0] in '💭💬': tok.value='#'+tok.value[1:]
                if len([_ for _ in keepAtTop if _.type == 'SHEBANG']) < 2 and REsearch(r'coding[=:]\s*([-\w.]+)',tok.value):
                    tok.type='SHEBANG'
                    keepAtTop.append(tok) ; lexIndex-=1
                else:
                    skip=False ; tmp=0
                    if tok.value[-1]=='#':
                        for tmpi in range(lexIndex,0,-1):
                            if lex[tmpi].type in typeNewline:
                                tmp=tmpi-1
                                if len(comments) > 0 and comments[-1][1] == tmp:
                                    comments[-1][0]=comments[-1][0]+'\n'+tok.value
                                    skip=True ; lexIndex-=1
                                break
                    else: tmp=lexIndex
                    if not skip: comments.append([tok.value,tmp,tok.lineno]) ; lexIndex-=1
            else:
                addParenUntilDone()
                if ignoreIndentation and lastIndent[-1] != 0:
                    # convert to TAB when ignoreIndentation is active and higher indent than 0
                    tok.type = 'TAB' ; tok.value = f"\n{' '*((len(lastIndent)-1)*prettyIndent)}"
                    lex.append(tok)
                else:
                    if lexIndex>0 and lex[lexIndex].type == 'IGNORENL': lexIndex-=1 # newline is not real if IGNORENL \
                    elif parenScope > 0: lexIndex-=1 # inside a parenthesis
                    elif lex:
                        if lex[lexIndex].type == 'THEN': lex.pop() ; lexIndex-=1
                        if lex[lexIndex].type != 'NEWLINE':
                            # so repeated NEWLINE doesn't override last backup
                            tabBackup = [currentTab, lastIndent[:]]
                            lastIndent = [0] ; currentTab = 0 ; inFrom = False
                            lex.append(tok)  # newline
                            cullOutOfScopeFunctions()
                        else: lexIndex -= 1 # if it is repeated NEWLINE then don't append
        elif tok.type == 'ASSIGN':
            if lex[lexIndex].type == 'LISTCOMP':
                tmp=lex[lexIndex].value.split(':') ; tmp[-1]=tmp[-1].replace(' ','')
                if tmp[-1] in convertType:
                    lex[lexIndex].type='TYPE' ; lex[lexIndex].value=tmp[-1]
                    tmptok=copy(lex[lexIndex])
                    tmptok.type='ID' ; tmptok.value=tmp[0]
                    lex.append(tmptok) ; lexIndex+=1 ; del tmptok
                lex.append(tok)
            elif (any(True for x in ASnakeKeywords if x == lex[lexIndex].value.strip()) \
            or any(True for x in [i for i in convertType]+list(defaultTypes) if x == lex[lexIndex].value)):
                # !! allows reassignment of reserved keywords !!
                lex[lexIndex].type = 'ID' ; lex.append(tok) ; reservedIsNowVar.append(lex[lexIndex].value.strip())
            elif lex[lexIndex].type == 'FWRAP':
                # redefining function as variable
                lex[lexIndex].value = lex[lexIndex].value[:-1] ; lex[lexIndex].type = 'ID' ;  wrapParenEndOfLine -= 1
                del definedFunctions[lex[lexIndex].value]
                lex.append(tok)
            else: lex.append(tok) ; definedVars[lex[lexIndex].value]={'indent':currentTab}
        elif tok.type in {'LOOP','WHILE'} and lex[lexIndex].type not in typeNewline:
            # loop/while can act as new expression indicators
            lex.append(makeToken(tok,'then','THEN')) ; lex.append(tok) ; lexIndex+=1
        elif tok.type == 'FUNCTION' and lex[lexIndex].type == 'ID' and lex[lexIndex].value.strip() == 'def' and 'def' not in reservedIsNowVar:
            lex[lexIndex].type='IGNORE'
            for t in miniLex(lex[lexIndex].value + ' ' + tok.value):
                lex.append(t)
                lex[-1].lineno = lineNumber
                lexIndex+=1
            lexIndex -= 1
            crunch = True
        elif tok.type == 'FUNCTION' and tok.value[-1]=='(' and tok.value[:-1].strip() in pyReservedKeywords and tok.value[:-1].strip() not in reservedIsNowVar:
            for t in miniLex(tok.value.replace('(',' (')+' '):
                t.lineno = lineNumber
                if debug: print('---',t)
                lex.append(t)
                lexIndex+=1
            lexIndex -= 1
        elif tok.type == 'LBRACKET':
            bracketScope+=1 ; lex.append(tok)
        elif tok.type == 'RBRACKET':
            bracketScope-=1 ; lex.append(tok)
        elif tok.type in {'FUNCTION','LPAREN'}:
            if tok.type == 'LPAREN':
                if lex[lexIndex].type == 'ID':
                    lex[lexIndex].value+='(' ; lex[lexIndex].type='FUNCTION' ; lexIndex -= 1
                elif tok.type == 'LPAREN' and lex[lexIndex].type == 'FWRAP' and lex[lexIndex].value[:-1] in definedFunctions:
                    lex[lexIndex].type = 'FUNCTION' ; wrapParenEndOfLine -= 1 ; lexIndex -= 1
                else: lex.append(tok)
            else: lex.append(tok)
            parenScope+=1
        elif tok.type == 'RPAREN':
            parenScope-=1   ; lex.append(tok)
        elif tok.type == 'ENDIF' and bracketScope > 0: tok.type='COLON' ; lex.append(tok)
        elif tok.type == 'SHEBANG':
            keepAtTop.append(tok) ; lexIndex-=1
        elif tok.type == 'IF' and lex[lexIndex].type in {"OR","AND"}:
            lexIndex-=1
        elif tok.type == 'ELSE' and wrapParenEndOfLine:
            addParenUntilDone() ; lex.append(tok)
        elif tok.type == 'PIPEGO' and tok.value not in reservedIsNowVar:
            pipeWrap+=1
            lex.append(tok)
        elif tok.type == 'PIPE'   and tok.value not in reservedIsNowVar:
            willPipe=True
            lex.append(tok)
        elif tok.type == 'IGNORE':
            lexIndex-=1
        elif tok.type == 'NOTIN':
            lex.append(makeToken(tok, 'not ', 'INS'))
            lex.append(makeToken(tok, 'in ', 'INS'))
            lexIndex += 1
        elif tok.type == 'DEFFUNCT':
            if lex[lexIndex].type == 'FWRAP' and lex[lexIndex].value[:-1] in definedFunctions:
                # already defined function is being redefined, thus not a line wrap, thus deconvert it to ID
                lex[lexIndex].value = lex[lexIndex].value[:-1] ; lex[lexIndex].type = 'ID' ;  wrapParenEndOfLine -= 1
            if lex[lexIndex].type != 'ID':
                if lex[lexIndex].value.strip() in pyReservedKeywords:
                    return AS_SyntaxError(f'{lex[lexIndex].value} is a reserved keyword. Use a different name.','myFunction does', lineNumber, data)
                return AS_SyntaxError(rf'{lex[lexIndex].value} is not a valid function name.\n\tFunction names should start with a letter or underscore.\n\tAvoid character literals like ()\!=<>/\\\'"*^%#@:&$.' + '{}','myFunction does', lineNumber, data)
            elif lexIndex-1 > 0 and lex[lexIndex-1].type not in typeNewline:
                return AS_SyntaxError(f'{lex[lexIndex-1].value+lex[lexIndex].value} is not a valid function name.\n\tFunction names should start with a letter or underscore.\n\tThere should be some sort of newline or newline indicator before the function name.',lex[lexIndex-1].value+'\n\t'+lex[lexIndex].value+' does', lineNumber, data)

            if lex[lexIndex].value not in definedFunctions: definedFunctions[lex[lexIndex].value] = currentTab
            # ^ store function name and current indent
            lex.append(tok)
        elif tok.type == 'PYDEF':
            tmpFuncArgs = REsearch(r'\((.*)\)(?=:|\n|$)', tok.value)
            if tmpFuncArgs:
                # extracts out function argument variables and types
                tmpFuncArgs = tmpFuncArgs.group()[1:-1]
                tmpREChecks = (r'[^,]+\[.*\](?:,|$)', r'[^,]+?\((?:.*?,?)*?\)(?: *,|$)', r'[^,]+\{.*\}(?:,|$)',r'[^,]+(?:,|,?.*?$)')
                tmpf = []
                for REcheck in tmpREChecks:
                    tmp = REfindall(REcheck, tmpFuncArgs)
                    if tmp:
                        for t in tmp:
                            if t:  tmpFuncArgs = tmpFuncArgs.replace(t, '') ; tmpf.append(t)
                tmpFuncArgs = {}
                for t in tmpf:
                    if ':' in t:
                        tmp = t.split(':')
                        tmp2 = REsearch(r' *\w+,?(?=\=|$)', tmp[1])
                        if tmp2: tmpFuncArgs[tmp[0].strip()] = tmp2.group().replace(',', '').strip()
                    else: tmpFuncArgs[t.split('=')[0].strip()] = None
                for arg in tmpFuncArgs:
                    if arg in ASnakeKeywords: reservedIsNowVar.append(arg)

            funcName = tok.value.split('def')[1].split('(')[0].replace(' ', '')  # get function name
            if funcName not in definedFunctions: definedFunctions[funcName] = currentTab
            tmp=tok.value.split() ; tmpFound = False
            for t in range(0,len(tok.value.split())):
                # def thing() do  splitting
                if tmp[t] in {'do','then'}:
                    tmpFound = True
                    tok.value = ' '.join(tmp[:t])
                    lex.append(tok)
                    for tt in miniLex(' '.join(tmp[t+1:])+' '):
                        if debug: print('--',tt)
                        lex.append(tt) ; lexIndex+=1
            if not tmpFound: lex.append(tok)
        elif tok.type == 'FSTRFRMT':
            if bracketScope > 0:
                # don't want dicts to be mistaken as fstr formatting
                lex.append(makeToken(tok,':','COLON')) ; lexIndex+=1
                for tt in miniLex(''.join(tok.value.split(':')[-1]) + ' '):
                    if debug: print('--', tt)
                    lex.append(tt) ; lexIndex+=1
                lexIndex -= 1
            else:
                lex.append(tok)
        elif tok.type == 'ANYOF' and 'each' in tok.value: tok.value = 'all' ; lex.append(tok)
        else:
            if reservedIsNowVar and tok.value in reservedIsNowVar: tok.type='ID'
            elif lex[lexIndex].type == 'FOR' and tok.value.strip() in pyReservedKeywords: tok.type='ID' ; reservedIsNowVar.append(tok.value.strip())
            elif tok.type in typeOperators+typeCheckers and tok.type in codeDict:
                if ((lex[lexIndex].type in typeNewline) or (lex[lexIndex].type in {'CONSTANT','TYPE','COMMA'})): pass
                else: tok.value = codeDict[tok.type]
            lex.append(tok)
        # checks
        if lex and lex[lexIndex].type == 'TYPE' and tok.type != 'ID' and lex[lexIndex-1].type not in ('PIPE','COMMA','FROM','CONSTANT','DEFFUNCT')+typeNewline and tok.type != 'CONSTANT':
            lex[lexIndex].type='ID' ; reservedIsNowVar.append(lex[lexIndex].value.strip())
        elif tok.type == 'CONSTANT' and lex[lexIndex].type == 'TYPE':
            tok.type='TYPE' ; tok.value = lex[lexIndex].value
            lex[lexIndex].value = 'const' ; lex[lexIndex].type = 'CONSTANT'
        elif lexIndex>1 and lex[lexIndex-1].type == 'FWRAP' and lex[lexIndex].type in typeMops+('COMMA',) and lex[lexIndex-1].value[:-1] in definedFunctions:
            # prevents line wrap when doing something like:  function + 2
            lex.insert(lexIndex,makeToken(lex[lexIndex],')','RPAREN')) ; lexIndex += 1 ; wrapParenEndOfLine -= 1
        lexIndex+=1

        if ignoreIndentation:
            if tok.type in typeConditionals+('LOOP','FOR','PYDEF','DEFFUNCT'):
                # when using ignoreIndentation META
                # if tok.type causes an indentSoon then manually add the indent to next TAB
                lastIndent.append(lastIndent[-1]+prettyIndent)
                tabBackup=[tabBackup[0],lastIndent[:]]

    # end of phase cleanup
    lex=[l for l in lex if l.type not in {'IGNORE','IGNORENL'}]
    for l in range(0,len(lex)-1):
        if lex[l].type == 'FWRAP':
            if lex[l].value[-1] == '(':
                lex[l].type = 'FUNCTION'
            else:
                lex[l].type = 'ID'

    if lex: lex.append(makeToken(lex[-1],'\n','NEWLINE'))
    # ^ need newline at the end , some stuff relies on that



    # optimize section
    if optimize == True:

        for tok in lex: # a prephase for optimization phase
            if tok.type in {'TIMES', 'PLUS', 'DIVIDE', 'MINUS'}: # removes wording for the compilerNumberEval function
                tok.value = codeDict[tok.type]

        def compilerNumberEval(toks):
            if isinstance(toks[0],str) == False:
                toks=[f.value for f in toks]
            if len(toks)==1: return toks[0]
            return Calc.evaluate(''.join([f for f in toks]))

        def determineIfAssignOrEqual(lexIndex):
            # determines if a ASSIGN `is` operates as a EQUAL `==` not a ASSIGN `=`
            # token should be ASSIGN or EQUAL, and lexIndex should be ASSIGN's index
            # True means it is Equal , False means Assign
            if lex[lexIndex].type == 'EQUAL': return True
            if lex[lexIndex].type != 'ASSIGN':
                print('Compiler-error\ndetermineIfAssignOrEqual error: not a ASSIGN','\nType is: '+lex[lexIndex].type) ; exit()
            elif ':' in lex[lexIndex].value:
                return False
            for tt in range(lexIndex - 1, 0, -1):
                if lex[tt].type in typeConditionals and lex[tt].type != 'ELSE' and lex[tt - 1].type in typeNewline:
                    return True
                elif lex[tt].type in typeNewline:
                    return False
            return False

        def getNumberOfElementsInList(lexIndex,backwards=False):
            # determine number of elements in list/tuple/set via lex tokens accurately
            # returns tuple (int numberOfElements , int lexIndexOfCloser)
            # return None is failure/invalid, ie list comp
            if (not backwards and lex[lexIndex].type not in {'LPAREN','LIST','LINDEX','LBRACKET'}) \
            or (backwards and lex[lexIndex].type not in {'RPAREN', 'LISTEND', 'RINDEX', 'RBRACKET'}):
                print('Compiler-error\ngetNumberOfElementsInList error: not a collection') ; exit()

            if backwards:
                collectionTypeEnd = {'RPAREN': 'LPAREN', 'LISTEND': 'LIST', 'RINDEX': 'LINDEX', 'RBRACKET': 'LBRACKET'}[lex[lexIndex].type]
            else:
                collectionTypeEnd={'LPAREN':'RPAREN','LIST':'LISTEND','LINDEX':'RINDEX','LBRACKET':'RBRACKET'}[lex[lexIndex].type]
            tmpListScope=tmpParenScope=tmpBracketScope=0
            elementCount=0
            if backwards:
                theRange=range(lexIndex-1,0,-1)
            else:
                theRange=range(lexIndex+1,len(lex)-1)
            for t in theRange:
                tmpListScope    += lex[t].value.count('[')
                tmpListScope    -= lex[t].value.count(']')
                tmpParenScope   += lex[t].value.count('(')
                tmpParenScope   -= lex[t].value.count(')')
                tmpBracketScope += lex[t].value.count('{')
                tmpBracketScope -= lex[t].value.count('}')
                #print(collectionTypeEnd,lex[t].type,elementCount,f"{tmpListScope}[ {tmpParenScope}( {tmpBracketScope}{{")
                if (not backwards and tmpListScope <= 0 and tmpParenScope <= 0 and tmpBracketScope <= 0) or (backwards and tmpListScope >= 0 and tmpParenScope >= 0 and tmpBracketScope >= 0):
                    if lex[t].type == collectionTypeEnd:
                        if (not backwards and lexIndex+1 == t) or (backwards and lexIndex-1 == t): elementCount=-1
                        return elementCount+1, t
                    elif lex[t].type in typeNewline:
                        return None # missing close
                    elif lex[t].type == 'FOR':
                        return None # list comp
                    elif lex[t].type == 'COMMA':
                        elementCount+=1
            return elementCount, t

        def checkIfInsideFSTR(lexIndex) -> bool:
            for t in range(lexIndex,0,-1):
                if lex[t].type in typeNewline:
                    if lex[t].type == 'THEN': pass
                    else: return False
                elif lex[t].type == 'FSTR':
                    return True

        def autoMakeTokens(tokenString, tokenPosition):
            nonlocal lex
            # manually writing tokens is boring and error prone.
            # this function takes a string and turns everything into tokens for you.
            for tt in reversed([_ for _ in miniLex(tokenString + ' ')]):
                lex.insert(tokenPosition + 1, tt)

        def getIndexVar(lexIndex, backwards=True):
            # given lexIndex should start with last RINDEX
            indexScope = 0
            insideIndex = []
            if backwards: looper = range(lexIndex,0,-1)
            else:         looper = range(lexIndex,len(lex)-1)
            for li in looper:
                if   lex[li].type == 'RINDEX':
                    indexScope+=1
                    if not backwards and indexScope == 0: return lex[lexIndex-1], insideIndex+[lex[li]], li
                elif lex[li].type == 'LINDEX': indexScope-=1
                elif lex[li].type == 'ID' and backwards and lex[li+1].type == 'LINDEX' and indexScope == 0:
                    return lex[li], insideIndex[::-1] if backwards else insideIndex, li
                elif lex[li].type in typeNewline:
                    return False
                insideIndex.append(copy(lex[li]))
            return False

        # idOPTARGS
        # vv you can choose to disable specific optimizations
        optFromFunc=True
        optFuncTricks=True
        optFuncTricksDict={ 'randint':True,
                            'listString':True,
                            'TupleSetUnpack':True,
                            'optCythonTypeFromConversionFunction':True,
                            'dictlistFunctionToEmpty':True,
                            'boolTonotnot':True,
                            'microPythonConst':True,
                            'sqrtMath': True,
                            'popToDel': True, # main phase
                            'roundFast': True,
                            'insertMathConstants': True,
                            'insertStringConstants': True,
                            'ExponentToTimes': True,
                            'inTo__contains__': False, # main phase, only good for Pyston
                            'intToFloat': True, # main phase, not good in PyPy
                            'max2compare': True,
                            'optCythonConvertTo_libc': True,
                            'startsWithToIndex': True,
                            'idToIs': True,
                            }
        optConstantPropagation=True
        optMathEqual=True
        optListToTuple=True
        optInSet=True
        optWalrus=False # seems to be only faster in pypy
        optLoopAttr=True
        optStrFormatToFString=True
        optCompilerEval=True # pyfuzz indicates there is likely breaking behaviour
        optCompilerEvalDict = {
            'evalTokens': True, # generic math and string type evals
            'evalLen': True,
            'evalThingInList': True,
            'evalStrFunc': True,
            'evalIntToFloat': True,
            'evalNotBoolInversion': True,  # Only provides performance to pypy, but its easy enough to leave it default
            'evalChrFunc': True,
            'evalIntFunc': True,
            'evalStrCenter':True,
            'evalStrInStr':True,
        }
        optPow=True
        optDeadVariableElimination=True
        optNestedLoopItertoolsProduct=True
        optSplitMultiAssign=True
        optUnModAssignment=True
        optCompressPrint=True
        optDeadConditionalElimination=True
        optConvertMultipleOrToIn=True
        # v these are done in main phase v
        optIfTrue=True # hybrid
        optSortedToSort=True
        optListPlusListToExtend=True
        optLoopToMap=True
        optFuncCache=True


        if compileTo == 'Cython':
            # v incompatible optimizations v
            optWalrus=optStrFormatToFString=False
            # v compatible but slower v
            optNestedLoopItertoolsProduct=optFuncCache=False
            # v prevents Cython-ization v
            optLoopToMap=optLoopAttr=optFuncTricksDict['max2compare']=False

        elif compileTo == 'PyPy3':
            # v seems to be slower for some reason on PyPy but faster on Python v
            optNestedLoopItertoolsProduct=optFuncCache=optLoopToMap=optListPlusListToExtend \
           =optFuncTricksDict['intToFloat']=optFuncTricksDict['startsWithToIndex']\
           =optConvertMultipleOrToIn=False
            # v faster in pypy v
            optWalrus=True
        elif compileTo == 'Pyston':
            # v slower v
            optFuncTricksDict['boolTonotnot']=False
            # v faster in pyston v
            optFuncTricksDict['inTo__contains__']=True
        elif compileTo == 'MicroPython':
            optListToTuple=optListPlusListToExtend=False # slower
            optFromFunc=optLoopAttr=False # not slower, but takes up too much memory
            optNestedLoopItertoolsProduct=optFuncCache=False # incompatible

        # vv incompatible optimizations vv
        if pythonVersion < 3.06:
            optStrFormatToFString = False
        if pythonVersion < 3.08:
            optWalrus = False
        # vv slower on certain versions vv
        if pythonVersion < 3.10:
            optListPlusListToExtend=False


        # meta
        pyCompatibility=False
        ogPyIs=pyIs

        orderOfOps = {'RPAREN': 6, 'LPAREN': 5, 'EXPONENT': 4,  'MODULO': 3, 'TIMES': 3, 'DIVIDE': 3,
                      'RDIVIDE': 3, 'PLUS': 2, 'MINUS': 2, 'BITWISE': 1} # Python operator precedence

        if optMathEqual:
            optMathEqualSignal = []
        if optConstantPropagation:
            functionsWithGlobals = {} # keeps track of vars being global in functions
            # example: { 'functionName' : ['var1','var2'] }
            for token in range(0,len(lex)):
                if lex[token].type == 'SCOPE' and lex[token].value.startswith('global'):
                    tmpVars = lex[token].value.split('global')[1].split(',')
                    tmpVars = [i.strip() for i in tmpVars]
                    tmpf = False
                    for tmpi in range(token - 1, 0, -1):
                        if lex[tmpi].type == 'PYDEF':
                            tmpf = lex[tmpi].value.split('(')[0][3:].strip();
                            break
                        elif lex[tmpi].type == 'DEFFUNCT':
                            tmpf = lex[tmpi - 1].value;
                            break
                    if tmpf:
                        functionsWithGlobals[tmpf] = tmpVars[:]

        definedFuncs=set()
        wasImported={} ; trackingImported={}
        # was imported is for optFromFunc , trackingImported is unrelated tracking
        doNotModThisToken=[]
        newOptimization=True
        optimizeRound=0
        varWasFolded=[] # for optConstantPropagation + optDeadVariableElimination


        while newOptimization: # continue to optimize until there is nothing left
            if debug:
                print('\t- optimization round =',optimizeRound,'-')
            optimizeRound+=1
            newOptimization=False
            preAllocated: set[tuple[indent: int , name: str]] = set()  # list of allocated attributes
            token=0
            for blah in range(0,(len(lex)-1)*2):
                if token <= len(lex)-1:
                    if not optimize:
                        if lex[token].type!='META': continue
                        elif lex[token].type=='META':
                            metaCall = lex[token].value.replace('$', '').replace(' ', '').lower()
                            if metaCall.split('=')[0].lower() in {'optimize', 'optimization', 'optimizing'}:
                                if '=' in metaCall:
                                    if metaCall.split('=')[-1].lower() == 'true':
                                        optimize = True
                                    elif metaCall.split('=')[-1].lower() == 'false':
                                        optimize = True
                                else:
                                    optimize = not optimize
                    #if debug: print('!',blah,token,lex[token])
                    if optCompressPrint and ((lex[token].type in {'ID','FUNCTION'} and (lex[token].value.startswith('print') or lex[token].value.startswith('ASprint')))
                    or (not pyCompatibility and lex[token].type == 'STRING' and lex[token-1].type in typeNewline+('DEFEXP',) and lex[token+1].type in typeNewline)):
                        # combines current print with the print on the prior line, to reduce function calls
                        # TODO handle defexp (when its print)
                        # also fstrings (done when prior is, not current)
                        tmpFound = False ; tmp2ndEndWith=tmpEndWith='\\n'
                        tmpf=[] # the print's args
                        if (lex[token].type == 'ID' or (lex[token].type == 'FUNCTION' and lex[token].value[-1] != '(')) and lex[token+1].type == 'LPAREN':
                              tmpStart=2
                        elif lex[token].type == 'STRING': tmpStart=0
                        else: tmpStart=1
                        #for tmpi in range(token+tmpStart,len(lex)):
                        #    if lex[tmpi].type in typeNewline: break
                        #    else: tmpf.append(copy(lex[tmpi]))
                        safe = False
                        if tmpStart > 0:
                            if lex[token+tmpStart].type == 'MINUS' and lex[token+tmpStart+1].type == 'NUMBER':
                                tmpStart+=1
                                lex[token+tmpStart-1].type='IGNORE'
                                lex[token+tmpStart].value = '-'+lex[token+tmpStart].value
                            if lex[token+tmpStart].type in {'STRING','NUMBER'}:
                                tmpf = copy(lex[token+tmpStart])
                                if lex[token+tmpStart+1].type in ('RPAREN',)+typeNewline: safe=True
                                elif token+tmpStart+4 < len(lex)-1 and lex[token+tmpStart+1].type == 'COMMA' and lex[token+tmpStart+2].type == 'ID' and lex[token+tmpStart+2].value == 'end' \
                                and lex[token+tmpStart+3].type == 'ASSIGN' and lex[token+tmpStart+4].type == 'STRING':
                                    safe=True ; tmp2ndEndWith=lex[token+tmpStart+4].value
                                if lex[token+tmpStart].type == 'STRING' and lex[token+tmpStart].value[0] not in {'"',"'"}: safe=False
                        else:
                            safe=True ; tmpf = copy(lex[token])
                        if lex[token+tmpStart-1].type in typeNewline and (lex[token+tmpStart].value.startswith('"""') or lex[token+tmpStart].value.startswith('"""')): safe=False
                        if safe:
                            # check backwards for print
                            tmpSafeFunctions=set(pyBuiltinFunctions)-{'map', 'open', 'input', 'print'}
                            safe = False ; tmpPrintIndent=tmpCurrentIndent=None ; tmpFound=-1 ; tmpOutOfFirstLine=False
                            for tmpi in range(token-1, 0, -1):
                                if lex[tmpi].type in typeNewline:
                                    if lex[tmpi].type == 'NEWLINE':
                                        tmpCurrentIndent = 0
                                        if tmpPrintIndent == None: tmpPrintIndent=tmpCurrentIndent
                                    elif lex[tmpi].type == 'TAB':
                                        tmpCurrentIndent = lex[tmpi].value.count(' ')
                                        if tmpPrintIndent == None: tmpPrintIndent=tmpCurrentIndent
                                    if tmpCurrentIndent != tmpPrintIndent: break
                                    tmpOutOfFirstLine=True
                                elif lex[tmpi].type == 'DEFEXP' and tmpPrintIndent == None: pass
                                elif (lex[tmpi].type in {'ID','FUNCTION'} and (lex[tmpi].value.startswith('print') or lex[tmpi].value.startswith('ASprint')) and lex[tmpi-1].type in typeNewline)\
                                or (not pyCompatibility and lex[tmpi].type == 'DEFEXP' and lex[tmpi+1].type == 'STRING' and lex[tmpi-1].type in typeNewline):
                                    # v indent safety checks
                                    if lex[tmpi-1].type == 'NEWLINE' and tmpPrintIndent == 0: pass
                                    elif lex[tmpi-1].type == 'TAB' and tmpPrintIndent == lex[tmpi-1].value.replace('\t',' ').count(' '): pass
                                    else: break

                                    for tmpii in range(tmpi + 1, len(lex)):
                                        if lex[tmpii].type in typeNewline: break
                                        elif lex[tmpii].type in {'FSTR','STRING','NUMBER'} and lex[tmpii-1].type not in {'ASSIGN','TIMES','FUNCTION'} \
                                        and ((lex[tmpii+1].type in typeNewline or (lex[tmpii+1].type == 'RPAREN' and lex[tmpii+2].type in typeNewline)) \
                                        or (lex[tmpii+1].type == 'COMMA' and lex[tmpii+2].type == 'ID' and lex[tmpii+2].value == 'end' and lex[tmpii+3].type == 'ASSIGN')):
                                            safe=True ; tmpFound=tmpii
                                        elif lex[tmpii].type in {'FSTR','STRING'} and lex[tmpii-1].type == 'ASSIGN' and lex[tmpii-2].type == 'ID' and lex[tmpii-2].value == 'end' and lex[tmpii-3].type == 'COMMA':
                                            tmpEndWith=lex[tmpii].value
                                            if tmpEndWith.endswith("'''") or tmpEndWith.endswith('"""'):
                                                tmpEndWith = tmpEndWith[3:-3]
                                            else:
                                                tmpEndWith = tmpEndWith[1:-1]
                                    break
                                elif lex[tmpi].type in typeConditionals and not tmpOutOfFirstLine: break
                                elif lex[tmpi].type == 'FUNCTION' and lex[tmpi].value.replace('(','') not in tmpSafeFunctions:
                                    safe=False ; break # unknown potentially unpure functions can break behaviour
                        if tmpFound and tmpf and (lex[tmpFound].value.endswith("'''") or lex[tmpFound].value.endswith('"""'))\
                        and (tmpf.value.endswith("'''") or tmpf.value.endswith('"""')):
                            safe=False # TODO: COULD be safe, I just don't feel like handling it rn
                        if safe:
                            if tmpStart == 0: lex[token-1].type='IGNORE'
                            # delete self
                            for tmpi in range(token, len(lex)):
                                if lex[tmpi].type in typeNewline: break
                                else: lex[tmpi].type='IGNORE'
                            # cull quotes
                            tmpQuoteType=False
                            if tmpf.type == 'STRING':
                                if tmpf.value.endswith("'''") or tmpf.value.endswith('"""'):
                                      tmpQuoteType = tmpf.value[-1]*3
                                      tmpf.value = tmpf.value[3:-3]
                                else: tmpf.value = tmpf.value[1:-1]
                            # add to other print
                            tmpStrType = ''
                            if lex[tmpFound].type in {'FSTR','STRING'}:
                                if lex[tmpFound].value.endswith("'''") or lex[tmpFound].value.endswith('"""'):
                                    tmp=1
                                    for _ in lex[tmpFound].value:
                                        if _ in {'"',"'"}:
                                            tmpQuoteType = _
                                            break
                                        tmp+=1 ; tmpStrType+=_
                                    tmpFoundCutAmount=(tmp,3)
                                elif lex[tmpFound].type == 'FSTR':
                                    tmpFoundCutAmount = (0, 1) ; tmpQuoteType=lex[tmpFound].value[-1]
                                else:
                                    if not tmpQuoteType: tmpQuoteType="'" if lex[tmpFound].value[-1] == "'" else '"'
                                    tmp=1
                                    for _ in lex[tmpFound].value:
                                        if _ in {'"', "'"}:
                                            break
                                        tmp += 1; tmpStrType += _
                                    tmpFoundCutAmount = (tmp, 1)
                                if lex[tmpFound].type == 'STRING':
                                    lex[tmpFound].value=f"{tmpStrType}{tmpQuoteType}{lex[tmpFound].value[tmpFoundCutAmount[0]:-tmpFoundCutAmount[1]]}{tmpEndWith}{tmpf.value}{tmpQuoteType*tmpFoundCutAmount[1]}"
                                else:
                                    lex[tmpFound].value = f"{tmpStrType}{lex[tmpFound].value[tmpFoundCutAmount[0]:-tmpFoundCutAmount[1]]}{tmpEndWith}{tmpf.value}{tmpQuoteType * tmpFoundCutAmount[1]}"
                            elif lex[tmpFound].type == 'NUMBER':
                                if not tmpQuoteType: tmpQuoteType="'"
                                lex[tmpFound].value = f"{tmpStrType}{tmpQuoteType}{lex[tmpFound].value}{tmpEndWith}{tmpf.value}{tmpQuoteType}"
                                lex[tmpFound].type = 'STRING'
                            if lex[tmpFound+1].type == 'COMMA' and lex[tmpFound+2].type == 'ID' and lex[tmpFound+2].value == 'end' \
                            and lex[tmpFound+3].type == 'ASSIGN' and lex[tmpFound+4].type == 'STRING':
                                if tmp2ndEndWith == '\\n':
                                    lex[tmpFound+1].type=lex[tmpFound+2].type=lex[tmpFound+3].type=lex[tmpFound+4].type='IGNORE'
                                else:
                                    lex[tmpFound+4].value=tmp2ndEndWith
                            newOptimization=True
                            if debug: print(f'! combined print: {lex[tmpFound].value}')

                    elif lex[token].type == 'ID':

                        if optConstantPropagation: # the one, the only, THE GOAT
                            tmpi=None
                            if lex[token-1].type not in typeConditionals+('OR','AND','LOOP'):
                                if lex[token+1].type in typeAssignables+('FSTR',) and lex[token+1].type != 'LISTEND' and lex[token+2].type not in {'PIPE','LISTCOMP'}:
                                    if lex[token+1].type == 'BUILTINF' and lex[token+1].value[0] == '.':
                                        tmpi=None
                                    else:
                                        tmpi=1
                                elif lex[token+1].type == 'ASSIGN' and lex[token+1].value.strip() in {'is','='} and lex[token+2].type in typeAssignables+('LPAREN','LBRACKET','FUNCTION','MINUS','INS','LINDEX','FSTR') and lex[token+2].type != 'LISTEND' and lex[token+3].type != 'LISTCOMP':
                                    tmpi=2
                            if tmpi and (lex[token].value == 'print' or lex[token-1].type == 'COMMA'): tmpi=None
                            if tmpi and optMathEqual and token+3 < len(lex) and lex[token+2].value == lex[token].value and lex[token+3].type in typeOperators: tmpi=None
                            # ^ optMathEqual comes online after constant folding, which messes stuff up. so if we detect it, then dont perform folding
                            # v if in from for asnake function def, inside conditional, or constant then ignore
                            if tmpi != None:
                                for t in range(token, 0, -1):
                                    # print('---',lex[t].type)
                                    if lex[t].type in typeConditionals + ('LPAREN', 'LOOP', 'FUNCTION', 'FROM', 'FSTR', 'CONSTANT'):
                                        tmpi = None ; break
                                    elif lex[t].type in typeNewline: break


                            if tmpi != None and lex[token + tmpi + 1].type == 'LINDEX':  tmpi=tmpf=None
                            if tmpi != None and lex[token+tmpi].type in {'LIST','LPAREN','LINDEX'} \
                            and lex[token+tmpi+1].type in {'LISTEND','RPAREN','RINDEX'}: tmpi=tmpf=None
                            if tmpi != None and token+tmpi < len(lex):
                                tmpf=[] # get expression
                                vartype=lex[token+tmpi].type
                                listScope=0 ; tmpBracketScope=0 ; tmpParenScope = 0
                                valueStop=None
                                if vartype in {'LIST','LPAREN','LINDEX'}:
                                    for t in range(token+tmpi,len(lex)-1):
                                        if lex[t].type in typeNewline and listScope==0: valueStop=t ; break
                                        elif lex[t].type in typeNewline: pass
                                        elif lex[t].type == 'ID' and (lex[t].value in definedFuncs or lex[t].value == lex[token].value):
                                            tmpf=None ; break
                                        else:
                                            if lex[t].type == vartype:
                                                listScope+=1
                                            elif (vartype=='LIST' and lex[t].type == 'LISTEND') \
                                            or (vartype=='LPAREN' and lex[t].type == 'RPAREN')  \
                                            or (vartype=='LINDEX' and lex[t].type in {'RINDEX','LISTEND'}): listScope-=1
                                            elif vartype in {'LINDEX','LIST'} and lex[t].type == 'INDEX':
                                                listScope += lex[t].value.count('[')
                                                listScope -= lex[t].value.count(']')
                                            tmpf.append(copy(lex[t]))
                                else:
                                    tmpNoEqualsAssign=True
                                    tmpFstrOn=True if lex[token+tmpi].type == 'FSTR' else False
                                    for t in range(token+tmpi,len(lex)-1):
                                        #print(lex[token].value,tmpNoEqualsAssign,tmpParenScope,lex[t].type,lex[t].value,[tt.value for tt in tmpf])
                                        if tmpNoEqualsAssign:
                                            # fixes  x y 12 ; x ; y
                                            # it captures y 12 for x, it shouldn't
                                            if lex[t].type not in ('ID','ASSIGN') or lex[t+1].type in typeOperators+('PIPE','RPAREN','IF') \
                                            or (t == token+tmpi and lex[t-1].type == 'ASSIGN' and lex[t+1].type not in typeAssignables+('ASSIGN',)):
                                                tmpNoEqualsAssign=False
                                        if not tmpNoEqualsAssign:
                                            if lex[t].type in typeNewline and listScope==0 and tmpBracketScope==0 and tmpParenScope==0:
                                                valueStop=t ; break
                                            elif lex[t].type == 'ID':
                                                if lex[t].value in definedFuncs or lex[t].value == lex[token].value: tmpf=None ; break
                                                else: tmpf.append(copy(lex[t]))
                                            elif lex[t].value == lex[token].value and lex[t+1].type=='LINDEX': tmpf=None ; break
                                            elif lex[t].type == 'LISTCOMP': tmpf=None ; break # remove this when we rework listcomps
                                            elif lex[t].type in (vartype,'IGNORE','LPAREN','RPAREN','INS','DEFEXP')+typeOperators:
                                                if lex[t].type == 'LIST':
                                                    listScope+=1
                                                elif lex[t].type == 'LISTEND': listScope-=1
                                                elif lex[t].type in {'FUNCTION','PIPE'}:
                                                    # be scared of folding functions
                                                    if lex[t].type == 'FUNCTION':
                                                        tmpParenScope += 1
                                                    tmpIsPipe = False if lex[t].type == 'FUNCTION' else True
                                                    tmpFuncName = lex[t + tmpIsPipe].value.replace('(', '')
                                                    if tmpFuncName in pyBuiltinFunctions:
                                                        pass # safe
                                                    else:
                                                        tmpSafe=False
                                                        for module in wasImported:
                                                            if tmpFuncName in wasImported[module] and module[:-1] in pureStdPythonModules:
                                                                tmpSafe = True
                                                        if not tmpSafe: tmpf=[] ; break
                                                elif lex[t].type == 'LPAREN':
                                                    tmpParenScope += 1
                                                elif lex[t].type == 'RPAREN':
                                                    tmpParenScope -= 1
                                                if lex[t].type!='IGNORE': tmpf.append(copy(lex[t]))
                                            elif lex[t].type == 'FUNCTION':
                                                tmpParenScope+=1
                                                tmpf.append(copy(lex[t]))
                                            elif vartype == 'FSTR':
                                                if lex[t].type == 'FSTR': tmpFstrOn = not tmpFstrOn
                                                if tmpFstrOn: tmpf.append(copy(lex[t]))
                                            else: vartype=None ; tmpf.append(copy(lex[t]))
                                            if lex[t].type == 'LBRACKET': tmpBracketScope+=1
                                            elif lex[t].type == 'RBRACKET': tmpBracketScope-=1
                                if vartype=='FSTR': vartype='STRING'
                                if tmpf == []: tmpf=None
                                if tmpf!=None and len(tmpf)>2:
                                    if tmpf[0].type in {'ID','BUILTINF'} and tmpf[1].type == 'LINDEX' and (tmpf[-1].type == 'RINDEX' or tmpf[-2].type == 'RINDEX'):
                                        tmpf=None # thing[index] folding is slower than using var reference from var=thing[index]
                                    elif optListPlusListToExtend and tmpf[0].type == 'FUNCTION' and tmpf[0].value.startswith('list') and tmpf[1].type == 'STRING' and tmpf[2].type == 'RPAREN':
                                        tmpf=None # in cases where optListPlusListToExtend is viable, constant propagation is slower
                                if valueStop==None: valueStop=len(lex)-1
                                if tmpf != None: # we got a expression now
                                    tmpListOfVarsInside=()
                                    if vartype == 'NUMBER' and optCompilerEval and optCompilerEvalDict['evalTokens']:
                                        try:
                                            tmpf=compilerNumberEval(tmpf)
                                        except (TypeError, SyntaxError, ZeroDivisionError):
                                            # fails sometimes with bitwise operators when negative inversion is involved
                                            tmptok=copy(lex[token]) ; tmptok.type='LPAREN' ; tmptok.value='(' ; tmpf.insert(0,tmptok) ; del tmptok
                                            tmptok=copy(lex[token]) ; tmptok.type='RPAREN' ; tmptok.value=')' ; tmpf.append(tmptok) ; del tmptok
                                            tmpf=tmpf[::-1]
                                    elif isinstance(tmpf[0],str)==False:
                                        # for lists/tuples
                                        tmpf=tmpf[::-1]
                                        if len(tmpf) > 1 and tmpf[-1].type!='LIST' and any(True for i in tmpf if i.type == 'LISTCOMP' )==False:
                                            tmptok=copy(lex[token]) ; tmptok.type='LPAREN' ; tmptok.value='(' ; tmpf.append(tmptok) ; del tmptok
                                            tmptok=copy(lex[token]) ; tmptok.type='RPAREN' ; tmptok.value=')' ; tmpf.insert(0,tmptok) ; del tmptok
                                        tmpListOfVarsInside=tuple([_.value for _ in tmpf if _.type == 'ID']) # for keeping track of vars inside that may change
                                    else: tmpf=[l for l in tmpf if tmpf.type != 'IGNORE']

                                    linkType = True if (enforceTyping or compileTo == 'Cython') else False

                                    tmpindent = 0  # keeping track if constant is in indented block. this is THE CONSTANTS indent
                                    tmpFoundIndent = tmpFoundThen = tmpInTypeWrap = tmpCheckForDef = tmpOutOfBlock = False
                                    # tmpCheckForDef when not False, checks to see if inside function before canceling optimization
                                    tmptmpIndent = 0  # keeping track of backwards current indent
                                    for tmpi in range(token - 1, 0, -1):
                                        # checks backwards to make sure its valid, also gets indent
                                        if lex[tmpi].type == 'TAB':
                                            if not tmpFoundIndent:
                                                tmptmpIndent = tmpindent = lex[tmpi].value.replace('\t', ' ').count(' ') // prettyIndent
                                                if lex[tmpi + 1].type in typeConditionals:
                                                    tmpindent += 1
                                                elif lex[tmpi + 1].type == 'TYPEWRAP':
                                                    tmpIndent -= 1
                                                tmpFoundIndent = True
                                            tmptmpIndent = lex[tmpi].value.replace('\t', ' ').count(' ') // prettyIndent
                                            if tmpFoundIndent and tmptmpIndent == tmpindent and lex[tmpi + 1].type == 'TYPEWRAP':
                                                tmpInTypeWrap = False
                                        elif lex[tmpi].type == 'NEWLINE':
                                            if not tmpFoundIndent:
                                                if lex[tmpi + 1].type == 'TYPEWRAP':
                                                    tmpindent -= 1
                                                elif lex[tmpi + 1].type in typeConditionals:
                                                    tmpindent += 1
                                                else: tmpindent = 0
                                                tmpFoundIndent = True
                                            tmptmpIndent = 0
                                            if tmpCheckForDef: search = False; break
                                        # don't check THENs for indent
                                        elif lex[tmpi].type == 'THEN' and lex[tmpi - 1].type not in {'TAB', 'NEWLINE'}:
                                            tmpFoundThen = True
                                        elif lex[tmpi].type == 'TYPEWRAP' and not tmpOutOfBlock:
                                            tmpInTypeWrap = True ; tmpOutOfBlock = True
                                        elif tmpindent > tmptmpIndent and lex[tmpi].type == 'ID' and lex[tmpi].value == lex[token].value:
                                            tmpCheckForDef = True  # previous version of self found on previous indent, bail!
                                        elif tmpCheckForDef and lex[tmpi].type in {'PYDEF', 'DEFFUNCT'}:
                                            tmpCheckForDef = False ; tmpOutOfBlock = True
                                        elif lex[tmpi].type == 'PYCLASS':
                                            linkType=False ; tmpOutOfBlock = True
                                        elif lex[tmpi].type in {'ELIF','OF','WHILE','LOOP'}: tmpOutOfBlock = True
                                        elif lex[tmpi].type in {'ELSE','IF'} and lex[tmpi-1].type in typeNewline: tmpOutOfBlock = True
                                        #print(lex[token].value, lex[tmpi].type, tmptmpIndent, tmpindent)
                                    if tmpInTypeWrap: tmpindent-=1
                                    tmpindent *= prettyIndent

                                    def handleIgnoreOnNL():
                                        nonlocal tmpAddToIgnoresWhenNL, ignores, tmpi
                                        if tmpAddToIgnoresWhenNL > 0:
                                            ignores.append(tmpAddToIgnoresWhenNL)
                                            tmpAddToIgnoresWhenNL = 0
                                        elif tmpAddToIgnoresWhenNL < 0:
                                            # when tmpAddToIgnoresWhenNL is negative, use postive version as start and current tmpi as end
                                            ignores.append((-tmpAddToIgnoresWhenNL,tmpi))
                                            tmpAddToIgnoresWhenNL = 0

                                    search=True ; ignores=[] ; inDef=wasInDefs=inFrom=inCase=tmpInConditionalStatement=tmpInWith=False
                                    # wasInDefs is for determining if a later define could break behaviour inside of functions
                                    # inDef i think is for determining if its the name of a function??
                                    tmpIDshow=0 ; tmpAddToIgnoresWhenNL = tttIndent = 0
                                    #print('-----')
                                    for tmpi in range(valueStop,len(lex)): # check if we can determine its a constant
                                        #print(lex[token].value,search,lex[tmpi].type,lex[tmpi].value,ignores,tmpAddToIgnoresWhenNL,tmpi,tmpIDshow)
                                        if not search and (enforceTyping and not linkType): break
                                        if lex[tmpi].type=='INC' or (tmpi+1 < len(lex) and lex[tmpi+1].type=='LINDEX' and lex[tmpi].value in (lex[token].value,)+tmpListOfVarsInside) \
                                        or ((lex[tmpi].type in {'ID','INC'} and lex[tmpi].value.replace('++','').replace('--','') in (lex[token].value,)+tmpListOfVarsInside and (lex[tmpi-1].type not in ('ELIF','OF','IF','OR','AND','FSTR','LIST')+typeCheckers+typeOperators)  ) and lex[tmpi+1].type in typeAssignables+('ASSIGN',) ):
                                            if (lex[tmpi+1].type == 'ASSIGN' and lex[tmpi+2].type == vartype) or lex[tmpi+1].type == vartype or (vartype=='NUMBER' and lex[tmpi].type=='INC'):
                                                pass
                                            elif lex[tmpi+1].type == 'ASSIGN' and vartype == 'STRING' and lex[tmpi+1].value.strip() not in {'=','is',':='}: pass # if fold is string then only hard assigns arent safe
                                            else: linkType=False
                                            if lex[tmpi+1].type=='LINDEX':
                                                if vartype=='STRING': pass
                                                else:
                                                    # if assigning to index
                                                    for tt in range(tmpi, len(lex) - 1):
                                                        if lex[tt].type in typeNewline:
                                                            break
                                                        elif lex[tt].type == 'ASSIGN':
                                                            if 'is' in lex[tt].value and determineIfAssignOrEqual(tt):
                                                                break
                                                            else:
                                                                tmpAddToIgnoresWhenNL = tt ; break
                                            else:
                                                tmpAddToIgnoresWhenNL = tmpi
                                                # if we reach a point where we can determine its no longer constant, then ignore that point onward so the previous code still gets folded
                                                if lex[tmpi].type == 'ID' and lex[tmpi].value in (lex[token].value,)+tmpListOfVarsInside:
                                                    if lex[tmpi+1].type == 'ASSIGN' and lex[tmpi+1].value.strip() == ':=':
                                                        search=False
                                                    elif tttIndent == tmpindent:
                                                        # set ignore point at end of line, so that it may still fold onto expression
                                                        for ii in range(tmpi,len(lex)-1):
                                                            if lex[ii].type in typeNewline:
                                                                tmpAddToIgnoresWhenNL = ii
                                                                break
                                                    else: search=False

                                            if not wasInDefs and lex[tmpi+1].type in typeAssignables+('ASSIGN',) and not tmpAddToIgnoresWhenNL:
                                                # wasInDefs used to not have the `not`, I think saying "if we are not in (another) function" makes sense.
                                                # but because the prior intent is not clear, just be aware of this as a suspect if something breaks later.
                                                tmpSafe=False
                                                if lex[tmpi+1].type == 'ASSIGN' and 'is' in lex[tmpi+1].value and determineIfAssignOrEqual(tmpi+1):
                                                    tmpSafe=True ; tmpAddToIgnoresWhenNL=0
                                                if not tmpSafe:
                                                    tmpAddToIgnoresWhenNL = tmpi
                                            if lex[token].value == lex[tmpi].value and lex[tmpi+1].type not in typeAssignables+('ASSIGN',): tmpIDshow+=1

                                        elif (inFrom or tmpInWith) and lex[tmpi].type == 'ID' and lex[token].value == lex[tmpi].value:
                                            if inFrom: search = False
                                            elif tmpInWith and lex[tmpi-1].type == 'WITHAS' and lex[tmpi-1].value.strip() == 'as': tmpAddToIgnoresWhenNL = tmpi-1
                                        elif lex[tmpi].type == 'ID' and lex[token].value == lex[tmpi].value and lex[tmpi+1].type == 'COMMA' and lex[tmpi-1].type in typeNewline+('COMMA','ENDIF'):
                                            tmptmpParenScope = 0
                                            for tt in range(tmpi,len(lex)-1):
                                                if lex[tt].type == 'ASSIGN' and tmptmpParenScope <= 0:
                                                    ignores.append(tmpi-1)
                                                    break
                                                elif lex[tt].type in typeNewline: break
                                                elif lex[tt].type == 'FUNCTION' and '(' in lex[tt].value: tmptmpParenScope+=1
                                                elif lex[tt].type == 'LPAREN': tmptmpParenScope+=1
                                                elif lex[tt].type == 'RPAREN': tmptmpParenScope-=1
                                        elif lex[tmpi].type == 'FUNCTION' and lex[tmpi].value in {'locals(','globals('} and lex[tmpi+1].type == 'RPAREN' and lex[tmpi+2].type == 'LINDEX' and lex[tmpi+3].type == 'STRING' and lex[tmpi+3].value.replace('"','').replace("'","") == lex[token].value:
                                            tmpAddToIgnoresWhenNL = tmpi
                                        elif lex[tmpi].type == 'FUNCTION' and lex[tmpi].value.replace('(','').replace(')','').strip() in functionsWithGlobals:
                                            tmp=False
                                            tmpVarList = functionsWithGlobals[lex[tmpi].value.replace('(','').replace(')','').strip()]
                                            if not isinstance(tmpf[0],str):
                                                for v in tmpf:
                                                    if v.type in {'ID','FUNCTION'} and v.value.replace('(','').replace(')','').strip() in tmpVarList:
                                                        tmp=True ; break
                                            elif lex[token].value in tmpVarList:
                                                tmp=True
                                            if tmp:
                                                ignores.append(tmpi-1) ; break
                                        elif lex[tmpi].type in {'FUNCTION','ID'} and lex[tmpi].value in {'ASenumerate','enumerate(','enumerate'} and lex[tmpi-1].type == 'INS':
                                            for tt in range(tmpi,0,-1):
                                                if lex[tt].type in typeNewline: break
                                                elif (lex[tt].type == 'ID' and lex[tt].value == lex[token].value):
                                                    ignores.append(tt) ; break
                                        elif lex[tmpi].type == 'BUILTINF' and lex[tmpi].value.split('.')[0] == lex[token].value and len(lex[tmpi].value.split('.')) > 1 and '.'+lex[tmpi].value.split('.')[1] in listMods+setUpdateMethods:
                                            search=False ; linkType=False ; break # discards list mods like .append()
                                        elif lex[tmpi].type == 'SCOPE' and lex[token].value in lex[tmpi].value:
                                            search=False ; break # no global var pls
                                        elif lex[tmpi].type == 'META' and lex[token].value in '='.join(lex[tmpi].value.split('=')[1:]):
                                            search=False
                                        elif lex[tmpi].type == 'DEFFUNCT':
                                            handleIgnoreOnNL()
                                            wasInDefs = True
                                            if lex[tmpi-1].value == lex[token].value:
                                                inDef=True ; search=False
                                                ignores.append([tmpi-1])
                                        elif lex[tmpi].type == 'PYDEF':
                                            handleIgnoreOnNL()
                                            wasInDefs = True
                                            if lex[token].value.strip()+'(' in lex[tmpi].value.strip():
                                                inDef = True ; search = False
                                                ignores.append([tmpi])
                                        elif lex[tmpi].type in typeLoop:
                                            if lex[tmpi-1].type == 'TAB':
                                                tmptmpIndent = lex[tmpi-1].value.count(' ')
                                            elif lex[tmpi-1].type == 'NEWLINE':
                                                tmptmpIndent = 0
                                            elif lex[tmpi-1].type == 'THEN':
                                                tmptmpIndent = 0
                                                for tmpii in range(tmpi, 0, -1):
                                                    if lex[tmpii - 1].type == 'TAB':
                                                        tmptmpIndent = lex[tmpii - 1].value.count(' ')
                                                        break
                                                    elif lex[tmpii - 1].type == 'NEWLINE':
                                                        tmptmpIndent = 0
                                                        break
                                            for tmpii in range(tmpi,len(lex)):
                                                if lex[tmpii].type in typeNewline and lex[tmpii].type != 'THEN':
                                                    if lex[tmpii].type == 'TAB':
                                                        if lex[tmpii].value.count(' ') <= tmptmpIndent:
                                                          break
                                                    else: break
                                                elif lex[tmpii].type == 'ID' and lex[tmpii].value == lex[token].value and lex[tmpii+1] in typeAssignables+('ASSIGN',):
                                                    ignores.append(tmpi) ; break
                                                elif lex[tmpii].type == 'INC' and lex[tmpii].value.replace('--','').replace('++','') == lex[token].value:
                                                    ignores.append(tmpi) ; break
                                        elif lex[tmpi].type == 'FROM' and inDef:
                                            search=inFrom=True ; inDef=False
                                            ignores[-1].append(tmpi)
                                        elif lex[tmpi-1].type == 'INS' and lex[tmpi].type == 'ID' and lex[tmpi].value == lex[token].value and vartype == 'DICT':
                                            search=False # dicts don't seem to play well with stuff like enumerate
                                        elif (not inFrom and not inCase) and lex[tmpi].type == 'ID' and lex[tmpi].value == lex[token].value \
                                        and (lex[tmpi+1].type != 'ASSIGN' or tmpInConditionalStatement):
                                            tmpIDshow+=1
                                        elif lex[tmpi].type in typeNewline:
                                            handleIgnoreOnNL()
                                            if inFrom: inFrom=False
                                            inCase=tmpInWith=False
                                            if lex[tmpi].type in {'NEWLINE','TAB'} or (lex[tmpi].type == 'THEN' and tmpInConditionalStatement):
                                                if lex[tmpi].type == 'THEN' and tmpInConditionalStatement:
                                                    tttIndent = tmpindent+1
                                                else:
                                                    tttIndent = 0 if lex[tmpi].type == 'NEWLINE' else lex[tmpi].value.count(' ')
                                                #print(tmpindent, tttIndent, ignores)
                                                if tmpindent > tttIndent: ignores.append(tmpi) ; break
                                            tmpInConditionalStatement = False
                                        elif lex[tmpi].type == 'IF':
                                            inCase=False ; tmpInConditionalStatement=True
                                        elif lex[tmpi].type == 'OF' and 'case' in lex[tmpi].value:
                                            inCase=True  ; tmpInConditionalStatement=True
                                        elif lex[tmpi].type == 'FOR' and lex[tmpi-1].type not in {'LINDEX','LIST'} and lex[tmpi+1].value == lex[token].value:
                                            search=False
                                        elif lex[tmpi].type == 'LAMBDA' and lex[tmpi].value[7:-1] == lex[token].value:
                                            tmpAddToIgnoresWhenNL=-tmpi
                                        elif lex[tmpi].type == 'WITHAS': tmpInWith=True
                                    if search:
                                        tmptmpSafe = True
                                        if not isinstance(tmpf[0], str) and len(tmpf) > 1:
                                            for t in tmpf:
                                                # a single token is considered safe, but multiple tokens can be safe, particularly if its a tuple of literal values
                                                if t.type not in {'STRING', 'COMMA', 'NUMBER', 'RPAREN', 'LPAREN'}:
                                                    tmptmpSafe = False; break
                                        if len([True for tmpi in range(token,0,-1) if lex[tmpi].type == 'SCOPE' and lex[token].value in lex[tmpi].value])>0: search=False
                                        # ^^ no global var pls
                                        elif len([True for tmpi in range(token,0,-1) if lex[tmpi].type == 'TRY' and 'try' in lex[tmpi].value])>len([True for tmpi in range(token,0,-1) if lex[tmpi].type == 'TRY' and 'except' in lex[tmpi].value]): search=False
                                        # ^^ fixes it in cases where the constant is defined in the except
                                        elif tmpIDshow > 1 and not tmptmpSafe and (len(tmpf)-2 > 1 and not(len(tmpf)-2==2 and tmpf[2].type=='MINUS' and tmpf[1].type=='NUMBER') ) and vartype!="LIST": search=False
                                        # ^^ when a constant involves operations, its better to compute it once and share the value rather than compute the value in many places.
                                        #if not isinstance(tmpf[0],str): print(search,lex[token].value,[_.value for _ in tmpf],len(tmpf),tmpIDshow)
                                    if search or linkType:
                                        inFrom=False # dont replace constants inside of FROM (function args)
                                        ignore=False # ignoring function blocks
                                        inLoop=[False,0]
                                        inCase=False # similar to inFrom ; do not replace constants in OF case statements



                                        if len([True for tmpi in range(token,0,-1) if lex[tmpi].type == 'TYPEWRAP' and (lex[tmpi-1].type=='NEWLINE' or (lex[tmpi-1].type in typeNewline and lex[tmpi-1].value.count(' ')<tmpindent))])>0:
                                            linkType=False # if there is a typewrap defining the types, then we shouldnt mess with it


                                        if debug and search:
                                            print('constant-propagation:',lex[token].value)
                                            if isinstance(tmpf[0],str) == False:
                                                print('tokens',lex[token].value,'=',''.join([f.value for f in tmpf[::-1]]))
                                            else: print('str',lex[token].value,'=',''.join([f for f in tmpf]))

                                        tmpLastIndent = 0 ; tmpFirstIndent=True
                                        for tmpi in range(valueStop,len(lex)):
                                            if inFrom or inCase:
                                                if lex[tmpi].type in typeNewline: inFrom=inCase=False
                                                elif inCase and lex[tmpi].type == 'IF': inCase=False # its fine on a conditional guard
                                            elif not search and not linkType: break # if linktype and search are false, why are we here? leave
                                            else:
                                                if search and ignores!=[]:
                                                    if isinstance(ignores[0], int) and ignores[0] == tmpi:
                                                        search=False
                                                    elif tmpi in ignores: ignore=search=False

                                                    elif (isinstance(ignores[0], list) or isinstance(ignores[0], tuple)) and tmpi == ignores[0][0]:
                                                        ignore=True
                                                    if ignore and tmpi == ignores[0][1]:
                                                        ignore=False ; del ignores[0]
                                                    #if debug: print(tmpi,lex[tmpi].type,f'ignore={ignore}',f'skip/end={ignores}')

                                                if search and ignore == False:
                                                    #print('~',lex[token].value,lex[tmpi].type,lex[tmpi].value,search,linkType,ignores,tmpi, tmpLastIndent,tmpindent)
                                                    if lex[tmpi].type == 'ID' and lex[tmpi].value==lex[token].value and (lex[tmpi+1].type not in typeAssignables+('ASSIGN',) or (lex[tmpi-1].type in typeConditionals+('OR','AND','INS') and lex[tmpi-1].type!='ELSE') or (lex[tmpi+1].type == 'ASSIGN' and 'is' in lex[tmpi+1].value and determineIfAssignOrEqual(tmpi+1)) or (lex[tmpi+1].type == 'LIST' and lex[tmpi-1].type not in typeNewline+('TYPE','CONSTANT','ELSE')+typeAssignables)) and lex[tmpi-1].type not in {'FOR','LOOP','ENDIF'}:
                                                        if lex[tmpi-1].type in typeConditionals and lex[tmpi+1].type == 'ASSIGN' and ':' in lex[tmpi+1].value: continue
                                                        tmpsafe=True ; tmpAppendDEFEXP=False
                                                        if lex[tmpi-1].type in {'RBRACKET','RPAREN','LISTEND'} or lex[tmpi-2].type == 'COMMA':
                                                            for tmpii in range(tmpi,0,-1):
                                                                if lex[tmpii].type == 'LOOP': tmpsafe=False ; break
                                                                elif lex[tmpii].type in typeNewline: break
                                                        if vartype !='STRING' and lex[tmpi+1].type=='LINDEX':
                                                            if (tmpIDshow == 1 and vartype in {'DICT','LIST'}): pass # dict/list ok if only once
                                                            else: tmpsafe=False # so it doesn't replace the var in var[index]s
                                                        if vartype in {'LIST','ID'} and (tmpIDshow > 1 and lex[tmpi-1].type not in typeCheckers+('INS','EQUAL','LPAREN','BITWISE','ANYOF')+typeMops or (lex[tmpi-1].type == 'LPAREN' and lex[tmpi-2].type in {'BUILTINF','FUNCTION'})) and (lex[tmpi-1].value.replace('(','') not in pyBuiltinFunctions or tmpIDshow > 1):
                                                            tmpsafe=False # functions can modify lists in place, therefore replacing it with the list can break behaviour
                                                            for tmpii in range(tmpi,0,-1):
                                                                if lex[tmpii].type in typeNewline+('BUILTINF','FUNCTION','LPAREN'): break
                                                                elif lex[tmpii].type == 'LAMBDA':
                                                                    tmpsafe=True
                                                                    if optListToTuple and isinstance(tmpf[0],str) == False:
                                                                        tmpf[-1].value='(' ; tmpf[-1].type='LPAREN'
                                                                        tmpf[0].value = ')' ; tmpf[0].type = 'RPAREN'
                                                                    break
                                                        if vartype == 'LIST' and lex[tmpi+1].type == 'LISTCOMP': tmpsafe=False # have this until LISTCOMP is smart enough to take in a whole list as an argument
                                                        if lex[tmpi-2].type == 'LOOP' and isinstance(tmpf[0],str) == False and tmpf[0].type == 'RPAREN': tmpsafe=False
                                                        if vartype == 'STRING' and (lex[tmpi-1].type == 'FSTR' or lex[tmpi+1].type == 'FSTR'):
                                                            if isinstance(tmpf[0],str) == False and '\\\\' in tmpf[0].value: tmpsafe=False
                                                            elif '\\\\' in tmpf: tmpsafe=False
                                                        if tmpsafe and pythonVersion < 3.12 and (lex[tmpi-1].type == 'FSTR' or lex[tmpi+1].type == 'FSTR'):
                                                            for _ in tmpf:
                                                                if not isinstance(tmpf[0],str) and _.type == 'STRING' and '\\' in _.value:
                                                                    tmpsafe=False ; break
                                                        if tmpsafe and lex[tmpi-2].type == 'BUILTINF' and '.join' in lex[tmpi-2].value and isinstance(tmpf[0],str) == False and any(True for _ in tmpf if _.type in {'FSTR','STRING'} and ('"""' in _.value or "'''" in _.value)):
                                                            tmpsafe = False # dumb pattern fix
                                                        if tmpsafe and inLoop[0] and tmpindent >= inLoop[1] and isinstance(tmpf[0],str) == False and 'FOR' in [t.type for t in tmpf]:
                                                            tmpsafe=False
                                                        if tmpsafe and isinstance(tmpf[0],str) == False and 'FOR' in [l.type for l in tmpf]:
                                                            # if the fold has a for loop (list comp),
                                                            # and the current expression is also a list comp
                                                            # then cancel optimization as it is slower.
                                                            if lex[tmpi+1].type == 'FOR' and lex[tmpi-1].type in {'LINDEX', 'LIST'}: tmpsafe=False
                                                            else:
                                                                tmpListScope=0
                                                                for tt in range(tmpi,len(lex)-1):
                                                                    if   lex[tt].type in {'LINDEX', 'LIST'}:    tmpListScope+=1
                                                                    elif lex[tt].type in {'RINDEX', 'LISTEND'}: tmpListScope-=1
                                                                    elif lex[tt].type in typeNewline: break
                                                                if tmpListScope != 0:
                                                                    tmpThereWasAFor=False
                                                                    for tt in range(tmpi,0,-1):
                                                                        if lex[tt].type == 'FOR': tmpThereWasAFor=True
                                                                        elif lex[tt].type in {'LINDEX', 'LIST'}:    tmpListScope += 1
                                                                        elif lex[tt].type in {'RINDEX', 'LISTEND'}: tmpListScope -= 1
                                                                        elif lex[tt].type in typeNewline: break
                                                                    if tmpListScope == 0 and tmpThereWasAFor: tmpsafe=False

                                                        if tmpsafe and lex[tmpi-1].type not in typeNewline:
                                                            # sometimes folding will invalidate the print-on-default-expression feature
                                                            # so on instances where we know it doesn't break behaviour, we make a
                                                            # DEFEXP token to signify it is safe.
                                                            tmpStartOfline=0 ; tmpDefExp=True
                                                            for tt in range(tmpi,0,-1):
                                                                if lex[tt].type in typeNewline: tmpStartOfline=tt ; break
                                                            for tt in range(tmpStartOfline+1,len(lex)-1):
                                                                if lex[tt].type in typeNewline: break
                                                                elif lex[tt].type == 'DEFEXP': tmpDefExp=False ; break
                                                                elif lex[tt].type == 'ID' and lex[tt+1].type in typeAssignables: tmpDefExp=False
                                                                elif lex[tt].type not in typePrintable: tmpDefExp=False
                                                            if tmpDefExp and tmpsafe:
                                                                # tmpAppendDEFEXP = True
                                                                # ^ this is 'safer' BUT im worried that the tmpStartOfline is needed somehow, idk how to test for it
                                                                lex.insert(tmpStartOfline+1,makeToken(lex[0],'defExp','DEFEXP'))
                                                                tmpi+=1

                                                        if tmpsafe and not isinstance(tmpf[0],str) and len([True for _ in tmpf if _.type == 'DICT']) > 0:
                                                            # dict not allowed in fstring
                                                            for tt in range(tmpi,0,-1):
                                                                if lex[tt].type in typeNewline: break
                                                                elif lex[tt].type == 'FSTR': tmpsafe = False ; break

                                                        if tmpsafe and isinstance(tmpf[0],str) and not pyIs and (lex[tmpi-1].type == 'PYIS' or (lex[tmpi-2].type == 'ASSIGN' and lex[tmpi-1].type == 'INS') or (lex[tmpi-1].type == 'ASSIGN' and determineIfAssignOrEqual(tmpi-1))):
                                                            tmpsafe=False # using Python is on literal will make it bring up syntax warning


                                                        if tmpsafe:
                                                            if debug: print(f'! replacing lex #{tmpi} (lastType:{lex[tmpi-1].type})')
                                                            if isinstance(tmpf[0],str) == False: # if tmpf[0] is not string, then its a lex
                                                                if not pyCompatibility: # These fix it not printing in ASnake mode. If in Python mode, don't fix it.
                                                                    tmpCheck = False
                                                                    if len(tmpf) == 1 and tmpf[0].type == 'STRING' and (tmpf[0].value.startswith('"""') or tmpf[0].value.startswith("'''")) and lex[tmpi-1].type in typeNewline:
                                                                        # multiline strings dont print due to doc-strings, thus when constant folding onto a asnake bare ID,
                                                                        # it wouldnt print even though it should. this adds print token to fix that
                                                                        tmpCheck = True
                                                                    elif lex[tmpi-1].type in typeNewline and lex[tmpi+1].type in typeNewline and lex[tmpi].type == 'ID' and lex[tmpi].value not in definedFuncs:
                                                                        # when it folds onto a bare ID, it should still print
                                                                        tmpCheck = True
                                                                    elif lex[tmpi+1].type == 'COMMA' and (lex[tmpi-1].type in typeNewline or (lex[tmpi-1].type == 'ID' and lex[tmpi-1].value == 'print')) and all(True if l.type in ('COMMA','NUMBER','STRING','LIST','LISTEND','LINDEX','RINDEX','LPAREN','RPAREN') else False for l in tmpf ) and not checkIfInsideFSTR(tmpi):
                                                                        # if comma group of simple literals
                                                                        if lex[tmpi-1].type in typeNewline:
                                                                            tmpCheck = True
                                                                        else:
                                                                            tmpf.append(makeToken(tmpf[0], '(', 'LPAREN'))
                                                                            tmpf.insert(0,makeToken(tmpf[0], ')', 'RPAREN'))
                                                                    if tmpCheck:
                                                                        tmpAppendDEFEXP=True
                                                                if len(tmpf)>=2 and tmpf[1].type == 'FSTR' == lex[tmpi-1].type:
                                                                    # when folding an fstr onto an fstr, make sure quote types dont collide
                                                                    tmpFSTRcheck=False
                                                                    tmpOtherFSTRQuote = tmpf[1].value[0] if tmpf[1].value[0] in {'"',"'"} else tmpf[1].value[1]
                                                                    if lex[tmpi-1].value[0] in 'fF' and lex[tmpi-1].value[1] == tmpOtherFSTRQuote:
                                                                        tmpFSTRcheck=tmpi-1
                                                                        if lex[tmpi - 1].value[1] == "'": tmpFSTRq = '"'
                                                                        else: tmpFSTRq = "'"
                                                                    if not tmpFSTRcheck:
                                                                        for t in range(tmpi,0,-1):
                                                                            if lex[t].type == 'FSTR' and lex[t].value[1] in {'"',"'"}:
                                                                                tmpFSTRcheck = t
                                                                                if   lex[t].value[1] == "'": tmpFSTRq = '"'
                                                                                elif lex[t].value[1] == '"': tmpFSTRq = "'"
                                                                                break
                                                                    if tmpFSTRcheck:
                                                                        for t in tmpf:
                                                                            if t.type == 'FSTR':
                                                                                if   t.value[-1] == lex[tmpFSTRcheck].value[1]: t.value = t.value[:-1]+tmpFSTRq
                                                                                elif t.value[ 1] == lex[tmpFSTRcheck].value[1]: t.value = 'f'+tmpFSTRq+t.value[2:]

                                                                if len(tmpf)>=2 and tmpf[1].type == 'FSTR' and lex[tmpi+1].type == 'FSTR':
                                                                    # a ID and FSTR with nothing inbetween implies comparison,
                                                                    # whereas FSTR + FSTR implies addition
                                                                    # so we insert comparison to not break behaviour
                                                                    tmpInFString = False
                                                                    for t in range(tmpi,0,-1): # check to make sure we are not in fstring first
                                                                        if lex[t].type == 'FSTR' and lex[t].value.endswith('{'):
                                                                            tmpInFString = True ; break
                                                                        elif lex[t].type in {'TAB','NEWLINE'}: break
                                                                    if not tmpInFString: lex.insert(tmpi+1,makeToken(lex[token],'==','EQUAL'))
                                                                if lex[tmpi+1].type == 'PIPE' and tmpf[0].type == 'RPAREN' and tmpf[-1].type == 'LPAREN':
                                                                    # when folding tuple onto pipe, add another paren as to not inherit the tuple.
                                                                    tmpf.append(makeToken(tmpf[0], '(', 'LPAREN'))
                                                                    tmpf.insert(0, makeToken(tmpf[0], ')', 'RPAREN'))
                                                                lex[tmpi].type='IGNORE'
                                                                tmp=[makeToken(tmpf[0], 'defExp', 'DEFEXP')] if tmpAppendDEFEXP else []
                                                                for t in tmpf+tmp:
                                                                    lex.insert(tmpi,copy(t))
                                                                #ignores.append(tmpi+len(tmpf))
                                                            else:
                                                                tmpf=''.join(tmpf)
                                                                lex[tmpi].value=tmpf ; lex[tmpi].type=vartype
                                                            newOptimization=True
                                                            if lex[token] not in varWasFolded: varWasFolded.append(lex[token])
                                                    elif lex[tmpi].type == 'FROM': inFrom=True
                                                    elif lex[tmpi].type == 'OF' and 'case' in lex[tmpi].value: inCase=True

                                                    elif lex[tmpi].type in {'LOOP','WHILE'} or lex[tmpi].type == 'FOR' and lex[tmpi-1].type in typeNewline:
                                                        if inLoop[0] == False and not isinstance(tmpf[0],str) and len(tmpf) > 1:
                                                            # if variable isnt a literal (or list) and is folding into a loop, dont bother, inefficient
                                                            if compileTo == 'PyPy3' and vartype == 'LIST': pass # faster in pypy for some reason
                                                            elif vartype == 'LIST' and lex[tmpi].type == 'FOR' and lex[tmpi+2].type == 'INS':
                                                                ignores.append(tmpi+4)
                                                            else:
                                                                ignores.append(tmpi) ; search = False ; continue
                                                        inLoop=[True,tmpindent]
                                                        # lookahead for loops in case it changes inside it
                                                        for tmpii in range(tmpi,len(lex)):
                                                            if lex[tmpi].type=='INC' or \
                                                            (((lex[tmpii].type in {'ID','INC'} and lex[tmpii].value.replace('++','').replace('--','').strip()==lex[token].value \
                                                            and ((lex[tmpii-1].type not in typeConditionals+('OR','AND') or lex[tmpii-1].type == 'ELSE') \
                                                            and lex[tmpi-1].type!='ELSE') ) or (lex[tmpii].value.startswith('locals[') \
                                                            and lex[token].value in ''.join(lex[tmpii].value.split('locals[')[1:])))):
                                                                if lex[tmpii].type == 'INC' or (lex[tmpii].type=='ID' and lex[tmpii+1].type in typeAssignables+('ASSIGN',)):
                                                                    tmpSafe=False
                                                                    if lex[tmpii].type == 'ID' and lex[tmpii+1].type == 'ASSIGN' and 'is' in lex[tmpii+1].value and determineIfAssignOrEqual(tmpii+1):
                                                                        tmpSafe=True
                                                                    if not tmpSafe:
                                                                        ignores.append(tmpi+1) ; search=False ; break
                                                                elif lex[tmpii+1].type == 'LINDEX':
                                                                    # assigning to index
                                                                    for tt in range(tmpii+2, len(lex) - 1):
                                                                        if lex[tt].type in typeNewline:
                                                                            break
                                                                        elif lex[tt].type == 'ASSIGN':
                                                                            if 'is' in lex[tt].value and determineIfAssignOrEqual(tt):
                                                                                break
                                                                            else:
                                                                                ignores.append(tmpi+1) ; search=False ; break
                                                            elif (lex[tmpii].type=='TAB' and lex[tmpii].value.replace('\t',' ').count(' ') < inLoop[1]) \
                                                            or (lex[tmpii].type == 'NEWLINE' and lex[tmpii].type!='TAB' and inLoop[1]>0) or (lex[tmpii].type=='THEN' and '\n' in lex[tmpii].value and lex[tmpii].value.replace('\t',' ').count(' ') < inLoop[1]):
                                                                inLoop=[False,0] ; break
                                                        if lex[tmpi].type == 'LOOP' and ((lex[tmpi+1].type=='ID' and lex[tmpi+1].value==lex[token].value) or (tmpi+2 < len(lex)-1 and lex[tmpi+2].type=='ID' and lex[tmpi+2].value==lex[token].value)) \
                                                        and not isinstance(tmpf[0],str) and tmpf[0].type != 'RPAREN':
                                                            if (lex[tmpi+1].type=='ID' and lex[tmpi+1].value==lex[token].value): tmp=1
                                                            else: tmp=2
                                                            # with loop syntax, fold onto iterable
                                                            if debug: print(f'! replacing lex #{tmpi} (lastType:{lex[tmpi-tmp].type})')
                                                            if not isinstance(tmpf[0],str):
                                                                lex[tmpi+tmp].type='IGNORE'
                                                                for t in tmpf:
                                                                    lex.insert(tmpi+tmp,t)
                                                            else:
                                                                tmpf=''.join(tmpf)
                                                                lex[tmpi+tmp].value=tmpf ; lex[tmpi+tmp].type=vartype
                                                    elif lex[tmpi].type == 'LISTCOMP' and lex[tmpi].value.split(':')[0] == lex[token].value:
                                                        if isinstance(tmpf[0], str):
                                                            lex[tmpi].value=tmpf+':'+''.join(lex[tmpi].value.split(':')[1:])
                                                            if debug: print(f'! replacing lex #{tmpi} (lastType:{lex[tmpi-1].type})')
                                                            newOptimization = True
                                                    elif lex[tmpi].type == 'NRANGE' and lex[token].value in lex[tmpi].value and len(tmpf)==1 and ((isinstance(tmpf[0], str) and tmpf.isdigit()) or (tmpf[0].type == 'NUMBER')):
                                                        tmp = False ; tmpt='...'
                                                        if '...' in lex[tmpi].value:
                                                            tmp = lex[tmpi].value.split('...')
                                                        elif '..' in lex[tmpi].value:
                                                            tmp = lex[tmpi].value.split('..')
                                                            tmpt='..'
                                                        elif 'to' in lex[tmpi].value:
                                                            tmp = lex[tmpi].value.split('to', 1)
                                                            tmpt=' to '
                                                        if tmp:
                                                            tmp = [i.strip() for i in tmp]
                                                            if lex[token].value in tmp:
                                                                for t in range(len(tmp)):
                                                                    if tmp[t] == lex[token].value:
                                                                        if isinstance(tmpf[0], str) == True:
                                                                            tmp[t]=tmpf
                                                                        else:
                                                                            tmp[t]=tmpf[0].value
                                                                        newOptimization=True
                                                            if debug and lex[tmpi].value != f"{tmp[0]}{tmpt}{tmp[1]}":
                                                                print('! NRANGE replace',lex[tmpi].value,' --> ',f"{tmp[0]}{tmpt}{tmp[1]}")
                                                            lex[tmpi].value = f"{tmp[0]}{tmpt}{tmp[1]}"
                                                    elif lex[tmpi].type in typeNewline:
                                                        if lex[tmpi].type == 'NEWLINE':
                                                            tmpLastIndent = 0 ; tmpFirstIndent = True
                                                        elif lex[tmpi].type == 'TAB':
                                                            tmpLastIndent = lex[tmpi].value.count(' ') ; tmpFirstIndent = True
                                                        elif lex[tmpi].type == 'END':
                                                            # END decreases the indent, which can confuse propagation. Thus we must keep track of it.
                                                            tmpLastIndent -= prettyIndent
                                                            if tmpLastIndent < 0: tmpLastIndent = 0
                                                        if tmpindent > tmpLastIndent and not tmpFirstIndent: search = False
                                                    elif lex[tmpi].type in typeConditionals and lex[tmpi-1].type in typeNewline:
                                                        inLoop[1]+=prettyIndent
                                                    if (lex[tmpi].type=='TAB' and lex[tmpi].value.replace('\t',' ').count(' ') < tmpindent) \
                                                    or (lex[tmpi].type == 'NEWLINE' and tmpindent>0):
                                                        if tmpInTypeWrap: tmpInTypeWrap=False # it's okay, its a syntax sugar indent that doesn't control flow
                                                        else:
                                                            break # if inside a indented block, try and stay local to that, do not escape it

                                                if tmpi >= len(lex)-1 and linkType and (enforceTyping or compileTo == 'Cython') and lex[token-1].type != 'TYPE'\
                                                and lex[token-2].type != 'LOOP' and lex[token-1].type != 'ID':
                                                    # auto assign type if its known it never changes
                                                    tmptok=copy(lex[token])
                                                    tmptok.type='TYPE'
                                                    if vartype == 'NUMBER' and '.' in tmpf:
                                                        tmptok.value='float'
                                                        lex.insert(token, tmptok)
                                                    elif vartype == 'BOOL':
                                                        if tmpf[0].value == 'None':
                                                            linkType=False ; token-=1
                                                    else:
                                                        tmp=[i for i in convertType if convertType[i] == vartype]
                                                        if tmp!=[]:
                                                            tmptok.value=tmp[0]
                                                            lex.insert(token,tmptok)
                                                    del tmptok ; token+=1 ; break

                        if optMathEqual and lex[token] not in optMathEqualSignal:
                            # checking for: a = a + 1 -> a += 1
                            if token+3 <= len(lex)-1 \
                            and lex[token+1].type == 'ASSIGN' and lex[token+1].value.strip() in {'=','is'} \
                            and lex[token+2].value == lex[token].value and lex[token-1].type in typeNewline \
                            and lex[token+3].type in typeOperators:
                                tmpcheck=True
                                tmpbit=tmptype=False
                                for t in range(token+3,len(lex)-1):
                                    if lex[t].type in typeAssignables:
                                        pass
                                    elif lex[t].type in typeOperators :
                                        if tmptype == False:
                                            tmptype=lex[t].type
                                            if tmptype == 'BITWISE':
                                                tmpbit=lex[t].value+'='
                                            elif tmptype in orderOfOps:
                                                for tmpi in range(t+1,len(lex)):
                                                    if lex[tmpi].type in orderOfOps and 'PAREN' not in lex[tmpi].type:
                                                        if orderOfOps[lex[tmpi].type] < orderOfOps[lex[t].type]:
                                                            # operator precedence. if next operator has less value, do not preform optimization.
                                                            tmpcheck = False
                                                        break
                                                    elif lex[tmpi].type in typeNewline: break
                                            break
                                    elif lex[t].type in typeNewline:
                                        break
                                    else:
                                        tmpcheck=False
                                        break
                                if tmpcheck and tmptype:
                                    optMathEqualSignal.append(lex[token])
                                    # cheap way to skip a second iteration in the case of:  a = a + a * 2
                                    # which otherwise would (shouldn't) end up as:  a *= 2

                                    if tmpbit:
                                        lex[token+1].value=tmpbit ; del tmpbit
                                    else: lex[token+1].value=mopConv[tmptype]
                                    lex.pop(token+2) ; lex.pop(token+2)
                                    newOptimization=True


                        #print(lex[token].value, optWalrus , lex[token+1].type in typeAssignables+('ASSIGN',) , lex[token-1].type in typeNewline+('TYPE',) , lex[token+2].type!='ID')
                        if optWalrus and lex[token+1].type in typeAssignables+('ASSIGN',) \
                        and lex[token-1].type in typeNewline+('TYPE',) and lex[token+2].type!='ID':
                            safe=True
                            if lex[token+1].type == 'ASSIGN':
                                if any(True for i in {'+', '-', '/', '*', ':'} if i in lex[token+1].value):
                                    safe=False
                                tmpi=2
                            else: tmpi=1
                            if tmpi == 2 and lex[token+tmpi].type == 'IF':
                                tmpfstr=False
                                for _ in range(token+tmpi,len(lex)-1):
                                    if not tmpfstr and lex[_].type == 'THEN': tmpTenary = _ ; break
                                    elif lex[_].type == 'FSTR': tmpfstr=not tmpfstr
                            else: tmpTenary = False

                            if safe:
                                # get forward conditionals indent
                                tmpIndent=0 ; tmpInConditional=False ; tmpConditionalIndex=0
                                for tmpii in range(token+1,len(lex)-1):
                                    if lex[tmpii].type == 'IF' and lex[tmpii-1].type in typeNewline:
                                        tmpInConditional=True ; tmpConditionalIndex=tmpii
                                    elif lex[tmpii].type == 'THEN' and lex[tmpii+1].type not in {'TAB','NEWLINE'}:
                                        tmpIndent=-1 # negative 1 should signify that it is on same indent
                                        break
                                    elif lex[tmpii].type == 'TAB':
                                        if lex[tmpii + 1].type == 'IF': tmpConditionalIndex = tmpii + 1
                                        tmpIndent=lex[tmpii].value.count(' ') ; break
                                    elif lex[tmpii].type == 'NEWLINE':
                                        if lex[tmpii+1].type == 'IF': tmpConditionalIndex=tmpii+1
                                        tmpIndent=0 ; break
                                    elif lex[tmpii].type == 'ID' and lex[tmpii].value == lex[token].value and not tmpInConditional:
                                        safe=False ; break
                                    elif lex[tmpii].type in {'WHILE','LOOP'}:
                                        safe=False ; break

                                # check behind
                                if safe:
                                    tmpFirstTNewline=False
                                    for tmpii in range(token-1,0,-1):
                                        if lex[tmpii].type in typeNewline:
                                            if tmpIndent != -1 and lex[tmpii].type == 'TAB' and lex[tmpii].value.count(' ') > tmpIndent or ( lex[tmpii].value.count(' ') == tmpIndent and lex[tmpii+1].type in typeConditionals):
                                               safe = False ; break
                                            elif tmpFirstTNewline:
                                                break
                                            else:
                                                tmpFirstTNewline = True
                                        elif lex[tmpii].type == 'OF': safe = False
                                        elif lex[tmpii].type in {'WHILE','LOOP'}: safe = False ; break

                            if safe:
                                tmpIndent = 0 ; tmp = 1
                                if lex[token - 1].type == 'TYPE': tmp = 2
                                if lex[token - tmp].type == 'TAB': tmpIndent = lex[token - tmp].value.replace('\t',' ').count(' ')

                                tmpf=[] ; tmpval=lex[token].value ; tmpAddTotmpf=True
                                for t in range(token+tmpi,len(lex)-1):
                                    if lex[t].type in typeNewline:
                                        if t == tmpTenary:
                                            if tmpAddTotmpf: tmpf.append([lex[t].value,lex[t].type])
                                            continue
                                        tmpi=t+1
                                        if (lex[t].type == 'TAB' and lex[t].value.replace('\t',' ').count(' ') < tmpIndent) \
                                        or (lex[t].type == 'NEWLINE' and tmpIndent > 0):
                                            tmpf=[] # if the assignment is on a higher indent, cancel this optimization
                                        break
                                    elif lex[t].type == 'ASSIGN' and any(True for i in {'+', '-', '/', '*', ':'} if i in lex[t].value): tmpf=[] ; break
                                    elif lex[t].type == 'LISTCOMP': tmpf=[] ; break
                                    elif lex[t].type == 'INC': tmpf=[] ; break
                                    elif lex[t].type == 'NEWLINE': tmpAddTotmpf=False
                                    elif tmpAddTotmpf: tmpf.append([lex[t].value,lex[t].type])

                                if tmpf != [] and tmpConditionalIndex:
                                    search=False
                                    for t in range(tmpConditionalIndex,len(lex)-1):
                                        if lex[t].type=='IF' and lex[t+1].type=='ID' \
                                        and lex[t+1].value == lex[token].value and ((lex[t+2].type != 'ASSIGN' and ':' not in lex[t+2].value) or lex[t+2].value.strip() == 'is'):
                                            search=True
                                            lex[t+1].type='IGNORE'
                                            tmp=[[')','RPAREN']]+tmpf[::-1]+[[':=','ASSIGN'],[tmpval,'ID'],['(','LPAREN']]
                                            if lex[t+2].type in typeAssignables+('BOOL',):
                                                tmp.insert(0,['==','EQUAL'])
                                            for tt in tmp:
                                                tmptok=copy(lex[t])
                                                tmptok.value=tt[0] ; tmptok.type=tt[1]
                                                lex.insert(t+1,tmptok)
                                        if lex[t].type in typeNewline:
                                            break

                                    if search:
                                        if debug: print(f'! walrus-ing: {lex[token].value}')
                                        for t in range(token,tmpi):
                                            #print(lex[t].type,lex[t].value)
                                            lex[t].type='IGNORE'
                                        if token-1>0 and lex[token-1].type in {'CONSTANT','TYPE'}:
                                            lex[token-1].type='IGNORE'
                                        if token-3>0 and lex[token-3].type == 'CONSTANT':
                                            lex[token-3].type='IGNORE'
                                        newOptimization=True

                        if optPow:
                            # x ** e % y turns into pow(x , e , y)
                            # this optimization should only occur if one of the values is not known,
                            # as compiler eval should take care of it otherwise.
                            # this optimization only becomes faster at higher values,
                            # so at least one value should be known
                            if lex[token+1].type == 'EXPONENT' and lex[token+3].type == 'MODULO' and lex[token+4].type in ('ID','NUMBER') and lex[token-1].type not in typeNewline:
                                # is x , checking e
                                check=False
                                for tmpi in range(token-1,0,-1):
                                    if lex[tmpi].type == 'ASSIGN':
                                        check = True
                                    elif lex[tmpi].type in typeNewline: break
                                if check and lex[token+2].type == 'NUMBER' and len(lex[token+2].value) >= 3:
                                    lex[token+1].type=lex[token+3].type='COMMA'
                                    tmptok=copy(lex[token]) ; tmptok.type='FUNCTION' ; tmptok.value='pow('
                                    lex.insert(token,tmptok) ; del tmptok
                                    tmptok=copy(lex[token]) ; tmptok.type='RPAREN' ; tmptok.value=')'
                                    lex.insert(token+6,tmptok) ; del tmptok
                                    newOptimization=True
                            elif lex[token-1].type == 'EXPONENT' and lex[token+1].type == 'MODULO' and lex[token+2].type in ('ID','NUMBER') and lex[token-3].type not in typeNewline:
                                # is e , checking x
                                check=False
                                for tmpi in range(token-3,0,-1):
                                    if lex[tmpi].type == 'ASSIGN':
                                        check = True
                                    elif lex[tmpi].type in typeNewline: break
                                if check and lex[token-2].type == 'NUMBER' and len(lex[token-2].value) >= 31:
                                    lex[token-1].type='COMMA';lex[token+1].type='COMMA'
                                    tmptok=copy(lex[token]) ; tmptok.type='FUNCTION' ; tmptok.value='pow('
                                    lex.insert(token-2,tmptok) ; del tmptok
                                    tmptok=copy(lex[token]) ; tmptok.type='RPAREN' ; tmptok.value=')'
                                    lex.insert(token+6-2,tmptok) ; del tmptok
                                    newOptimization=True

                        if lex[token-1].type == 'PIPE':
                            if optCompilerEval and optCompilerEvalDict['evalLen'] and 'len' not in reservedIsNowVar:
                                if lex[token].value == 'ASlen' or lex[token].value == 'len' and lex[token-2].type=='STRING' :
                                    # eval len of string
                                    if lex[token - 2].value.startswith('"""') or lex[token - 2].value.startswith("'''"):
                                        tmp=6
                                    else: tmp=2
                                    lex[token].value = str(len(lex[token - 2].value) - tmp)
                                    lex[token].type='NUMBER'
                                    del lex[token - 1] ; del lex[token - 2]
                                    newOptimization=True
                                elif lex[token].value in {'ASlen', 'len'} and lex[token - 2].type in {'RPAREN', 'LISTEND', 'RINDEX', 'RBRACKET'}:
                                    # eval len of list/tuple/set
                                    tmpf = getNumberOfElementsInList(token - 2, backwards=True)
                                    if tmpf != None and tmpf[0] > 1:
                                        if debug: print(f"! compilerEval: len of {lex[token-5].value}{lex[token-4].value}{lex[token-3].value}{lex[token-2].value} -->  {tmpf[0]}")
                                        lex[token].type = 'NUMBER' ; lex[token].value = str(tmpf[0])
                                        for t in range(token-1, tmpf[1]-1, -1):
                                            lex[t].type = 'IGNORE'
                                        newOptimization = True

                        if optLoopAttr and preAllocated and lex[token+1].type in typeAssignables+('ASSIGN',) \
                        and True in (True if p[1].startswith('AS'+lex[token].value) else False for p in preAllocated):
                            # if ID assigns to something new, remove it from preAllocated to prevent breaking behaviour.
                            for p in tuple(_ for _ in preAllocated):
                                if p[1].startswith('AS' + lex[token].value):
                                    preAllocated.remove(p)

                        if optSplitMultiAssign and lex[token+1].type == 'COMMA' and lex[token+2].type == 'ID' and lex[token+3].type in {'COMMA','ASSIGN'} and lex[token-1].type in typeNewline+('CONSTANT','TYPE'):
                            # it is faster to not use multi-assigns
                            # a,b,c = 1,2,3  -->  a=1;b=2;b=3
                            # should only be safe when not an unpack
                            safe=True ; tmpVars=[] ; tmpStartOfCommagrp=0
                            # gather all vars
                            if safe:
                                for tmpi in range(token,len(lex)-1):
                                    if lex[tmpi].type == 'ID': tmpVars.append([copy(lex[tmpi]),[]])
                                    elif lex[tmpi].type == 'ASSIGN':
                                        if tmpi+2 < len(lex) and lex[tmpi+1].type == 'ID' and lex[tmpi+2].type == 'ASSIGN' and not determineIfAssignOrEqual(tmpi+2):
                                            safe = False ; break # rare case so i think its not worth to salvage
                                        tmpStartOfCommagrp=tmpi+1 ; break
                                    elif lex[tmpi].type in typeNewline+('TIMES',): safe=False ; break

                            if safe and lex[tmpStartOfCommagrp].type == 'STRING' and lex[tmpStartOfCommagrp+1].type in typeNewline:
                                # multi-assign a string
                                # a,b,c = 'abc'  -->  a,b,c='a','b','c'  -->  a='a';b='b';c='c'
                                tmp=lex[tmpStartOfCommagrp].value
                                # USE list()
                                tmpSQuote = tmp[0]
                                tmp=tmp[1:-1]
                                tmp=list(tmp)
                                for t in range(0,len(tmp)-1):
                                    if tmp[t] == '\\' and tmp[t+1] == 'u':
                                        # unicode code, merge next 4 characters
                                        tmpf = ''.join(tmp[t:t+6])
                                        tmp =  tmp[:t] + [tmpf] + tmp[t+6:]
                                    if t >= len(tmp)-1: break

                                if len(tmp) == len(tmpVars):
                                    # if same amount of vars as values, perform split
                                    lex[tmpStartOfCommagrp].type='IGNORE'
                                    for letter in reversed(tmp):
                                        lex.insert(tmpStartOfCommagrp, makeToken(lex[token],tmpSQuote+letter+tmpSQuote,'STRING'))
                                        lex.insert(tmpStartOfCommagrp, makeToken(lex[token], ',', 'COMMA'))
                                    lex[tmpStartOfCommagrp].type='IGNORE'

                            if safe:
                                tmpIndex=0 ; tmpEndOfStatement=0 ; tmpScopes={'list':0,'tuple':0,'set':0}
                                tmpVarNames = [i[0].value for i in tmpVars if i[0].type == 'ID']
                                # regular comma group
                                # gather values of vars
                                for tmpi in range(tmpStartOfCommagrp, len(lex) - 1):
                                    #print(tmpIndex,lex[tmpi].type,tmpScopes)
                                    if lex[tmpi].type in typeNewline:
                                        tmpEndOfStatement = tmpi
                                        break
                                    elif lex[tmpi].type == 'COMMA' and not any(True for i in tmpScopes if tmpScopes[i] > 0):
                                        tmpIndex += 1
                                    elif lex[tmpi].type == 'IGNORE': pass
                                    else:

                                        if lex[tmpi].type in {'LIST','LINDEX'}: tmpScopes['list']+=1
                                        elif lex[tmpi].type in {'LISTEND','RINDEX'}: tmpScopes['list']-=1
                                        elif lex[tmpi].type == 'LPAREN' or (lex[tmpi].type == 'FUNCTION' and '(' in lex[tmpi].value):
                                            tmpScopes['tuple'] += 1
                                        elif lex[tmpi].type == 'RPAREN':
                                            tmpScopes['tuple'] -= 1
                                        elif lex[tmpi].type == 'LBRACKET':
                                            tmpScopes['set'] += 1
                                        elif lex[tmpi].type == 'RBRACKET':
                                            tmpScopes['set'] -= 1

                                        if tmpIndex <= len(tmpVars)-1 \
                                        and not (lex[tmpi].type == 'ID' and lex[tmpi].value in tmpVarNames): # cancel optimization if reassigning one of the vars to another
                                            tmpVars[tmpIndex][1].append(copy(lex[tmpi]))
                                        else: safe = False ; break

                                if safe and len([True for i in tmpVars if i[1]]) == len(tmpVars):
                                    # if there is a value for every var
                                    for tmpi in range(token,tmpEndOfStatement):
                                        # remove line
                                        lex[tmpi].type='IGNORE'
                                    # add new statements
                                    for var in tmpVars:
                                        lex.insert(token, makeToken(lex[token], ';', 'THEN'))
                                        for t in reversed(var[1]):
                                            lex.insert(token, t)
                                        lex.insert(token, makeToken(lex[token], '=', 'ASSIGN'))
                                        lex.insert(token,var[0])
                                    newOptimization = True
                                    if debug: print(f"! split multi-assign: {', '.join([i[0].value for i in tmpVars])}")


                        if optFuncTricks and optFuncTricksDict['sqrtMath'] and lex[token+1].type == 'EXPONENT' and lex[token+2].type == 'NUMBER' and lex[token+2].value in {'0.5','.5'}:
                            lex[token + 1] = copy(lex[token])
                            lex[token + 2] = makeToken(lex[token], ')', 'RPAREN')
                            lex[token].type = 'FUNCTION' ; lex[token].value = 'sqrt('
                            if lex[token - 1].type in {'TAB', 'NEWLINE'}:
                                lex.insert(token,makeToken(lex[token], 'defExp','DEFEXP'))
                            newOptimization=True
                            if 'math.' in wasImported and 'sqrt' not in wasImported['math.']:
                                wasImported['math.'].append('sqrt')
                            else:
                                insertAtTopOfCodeIfItIsNotThere("from math import sqrt")

                        elif optFuncTricks and optFuncTricksDict['ExponentToTimes'] and lex[token].type == 'ID' and lex[token + 1].type == 'EXPONENT' and lex[token + 2].type == 'NUMBER':
                            # x*x is faster than x**2
                            # so create many x*x based on the exponent number
                            tmpSafe = False
                            if (compileTo == 'Python' and int(lex[token + 2].value) < 6) \
                            or (compileTo == 'Pyston' and int(lex[token + 2].value) < 11) \
                            or (compileTo == 'PyPy3' and int(lex[token + 2].value) > 2):
                                # x**6 is when x*x becomes slower for cpython
                                # x**11 for pyston
                                # x ** 2 is faster in pypy, anything more is not, ever
                                tmpSafe = int(lex[token + 2].value)
                            if tmpSafe:
                                lex[token + 1].type = lex[token + 2].type = 'IGNORE'
                                for ii in range(tmpSafe-1, 0, -1):
                                    lex.insert(token + 1, copy(lex[token]))
                                    lex.insert(token + 1, makeToken(lex[token], '*', 'TIMES'))
                                lex.insert(token, makeToken(lex[token], '(', 'LPAREN'))
                                lex.insert(token + (tmpSafe*2), makeToken(lex[token], ')', 'RPAREN'))
                            if debug: print(f'! ExpToTimes: {lex[token+1].value}**{tmpSafe}  -->  {(lex[token+1].value+"*")*(tmpSafe-1)}{lex[token+1].value}')



                        elif optFuncTricks and optFuncTricksDict['insertMathConstants'] and lex[token].type in {'BUILTINF','ID'} and \
                            (  ((lex[token].value == 'pi' or lex[token].value == 'math.pi') and 'math.' in trackingImported and 'pi' in trackingImported['math.'])
                            or ((lex[token].value == 'e'  or lex[token].value == 'math.e' ) and 'math.' in trackingImported and 'e'  in trackingImported['math.'])
                                ):
                                # math.pi --> 3.141592653589793
                                tmp='3.141592653589793' if lex[token].value.endswith('pi') else '2.718281828459045' # pi or e
                                if debug: print(f'! insertMathConstants: {lex[token].value} --> {tmp}')
                                lex[token].type = 'NUMBER' ; lex[token].value = tmp
                                #wasImported['math.'].remove('pi')
                                # ^ in event of dead import elimination being implemented, uncomment
                                newOptimization = True

                        elif optFuncTricks and optFuncTricksDict['insertStringConstants'] and lex[token].type in {'BUILTINF','ID'} and 'string.' in trackingImported and \
                            lex[token].value in {'ascii_letters','ascii_lowercase','ascii_uppercase','digits','hexdigits','octdigits','punctuation','printable','whitespace'} \
                            and any(_ for _ in ('ascii_letters','ascii_lowercase','ascii_uppercase','digits','hexdigits','octdigits','punctuation','printable','whitespace') if _ in trackingImported['string.']):
                                tmpPrintable = """0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~     

"""
                                tmp = lex[token].value
                                if tmp == 'hexdigits': tmpf = '0123456789abcdefABCDEF'
                                elif tmp == 'printable': tmpf = tmpPrintable
                                else:
                                    tmpPrintIndex = {'ascii_letters': (10, 62), 'ascii_lowercase': (10, 36),
                                                     'ascii_uppercase': (36, 62), 'digits': (0, 10),
                                                     'octdigits': (0, 8), 'punctuation': (62, 94),
                                                     'whitespace': (94, 0)
                                                     }
                                    tmpf = tmpPrintable[tmpPrintIndex[tmp][0]:tmpPrintIndex[tmp][1]]
                                    del tmpPrintIndex
                                if debug: print(f'! insertMathConstants: {lex[token].value} --> {tmpf}')
                                if tmp in {'printable','punctuation'}:
                                    lex[token].value = f'"""{tmpf}"""'
                                else:
                                    lex[token].value = f'"{tmpf}"'
                                lex[token].type = 'STRING'
                                del tmpPrintable
                                newOptimization = True


                    elif lex[token].type == 'META':
                        metaCall=lex[token].value.replace('$','').replace(' ','')
                        metaCallSplit = metaCall.split('=')[0].strip()
                        if metaCall == 'cython':
                            compileTo='Cython'
                        elif metaCall == 'python':
                            compileTo='Python'
                        elif metaCallSplit in {'optimize','optimization','optimizing'}:
                            optimize = metaHandling(metaCall, optimize)
                        elif metaCallSplit in metaPyCompat:
                            pyCompatibility = metaHandling(metaCall, pyCompatibility)
                            if pyCompatibility: pyIs = True
                            else: pyIs = False
                        elif metaCallSplit in metaPyIs:
                            pyIs = metaHandling(metaCall, pyIs)


                    elif optCompilerEval and optCompilerEvalDict['evalNotBoolInversion'] and lex[token].type == 'INS' and lex[token].value.strip() == 'not' and lex[token+1].type == 'BOOL':
                        if lex[token+1].value.strip() == 'True':
                            lex[token+1].value = 'False' ; lex[token].type = 'IGNORE'
                        elif lex[token+1].value.strip() == 'False':
                            lex[token+1].value = 'True'  ; lex[token].type = 'IGNORE'

                    elif lex[token].type in {'LPAREN','LIST','LBRACKET'} and lex[token-1].type == 'INS' and lex[token-1].value.strip() == 'in':
                        if optInSet:
                            tmpscope=1 ; tmp=0 ; tmpf=[] ; tmpLeftScope = 1 ; inForLoop=hasComma=False
                            for tmpi in range(token+1,len(lex)):
                                tmpLastIndex = tmpi+1
                                if lex[tmpi].type == lex[token].type: tmpscope+=1 ; tmpLeftScope+=1
                                elif lex[token].type == 'LPAREN' and lex[tmpi].type == 'RPAREN': tmpscope-=1
                                elif lex[token].type == 'LIST' and lex[tmpi].type == 'LISTEND': tmpscope -= 1
                                elif lex[token].type == 'LBRACKET' and lex[tmpi].type == 'RBRACKET': tmpscope-=1
                                elif lex[tmpi].type == 'FOR': tmp=0 ; break
                                if tmpscope == 0: tmp=tmpi ; break
                                else:
                                    if not hasComma and lex[tmpi].type == 'COMMA': hasComma=True
                                    tmpf.append(copy(lex[tmpi]))
                            for tmpi in range(token-1,0,-1):
                                if lex[tmpi].type == 'FOR': inForLoop=True ; break
                                elif lex[tmpi].type in typeNewline: break
                            # if tmp is 0 then syntax error, but i don't think optimization stage is where to show it
                            if 0 < tmp and tmpLeftScope == 1 \
                            and lex[tmp+1].type != 'PLUS' and hasComma:
                                tmpf=[l.value for l in tmpf if l.type != 'COMMA']
                                seen=set()
                                tmpsafe=True
                                if lex[token].type == 'LBRACKET': tmpsafe=False # support sets for evalThingInList
                                if not inForLoop:
                                    for t in tmpf:
                                        if t in seen or '[' in t or ']' in t:
                                            tmpsafe = False ; break
                                        else: seen.add(t)
                                if optCompilerEval and optCompilerEvalDict['evalThingInList'] and not inForLoop and lex[token - 2].type in {'STRING', 'NUMBER'}:
                                    # if 2 in [1,2,3]  -->  if True
                                    tmpsafe = False
                                    if debug: tmpOG = lex[token - 2].value
                                    if lex[token - 2].value in seen:
                                          tmp=True
                                    else: tmp=False
                                    lex[token-2].type = 'BOOL'
                                    lex[token-2].value = str(tmp)
                                    for ii in range(token-1,tmpLastIndex):
                                        lex[ii].type = 'IGNORE'
                                    if debug: print(f"! evalThingIn: {tmpOG} in {seen} --> {tmp}") ; del tmpOG
                                    newOptimization = True
                                if tmpsafe and not inForLoop:
                                    # if all are unique, and doesn't contain list
                                    lex[token].type='LBRACKET' ; lex[token].value='{'
                                    lex[tmp].type = 'RBRACKET' ; lex[tmp].value = '}'
                                    if debug: print(f"! converted to set: {{{', '.join(tmpf)}}}")
                                    newOptimization = True
                                elif optListToTuple and lex[token].type == 'LIST':
                                    # might as well convert it to tuple
                                    lex[token].type='LPAREN' ; lex[token].value='('
                                    lex[tmp].type = 'RPAREN' ; lex[tmp].value = ')'
                                    if debug: print(f"! converted to tuple: ({', '.join(tmpf)})")
                                    newOptimization = True

                        if optListToTuple and lex[token].type == 'LIST':
                            if token-2 >= 0 and lex[token].type=='LIST' and lex[token].value!='(' and lex[token-1].value != 'print':
                                if (lex[token-1].type == 'ID' \
                                    or lex[token-2].type == 'ID') and \
                                    (lex[token-3].type != 'TYPE' and lex[token-2].type != 'TYPE'):
                                        if lex[token-1].type == 'ID': tokID=lex[token-1].value
                                        else: tokID=lex[token-2].value
                                        tmpcheck=True
                                        listEnd=False
                                        scope=1 # scope is 1 since lex[token] is a LIST
                                        noListcomp=[]
                                        listisParen=False
                                        if lex[token].value=='(': listisParen=True

                                        for ttoken in range(token-1,0,-1): # checking backwards
                                            if lex[ttoken].type in typeNewline: break
                                            elif lex[ttoken].type == 'BUILTINF' and lex[ttoken].value in listMods: tmpcheck=False ; break
                                            elif optListPlusListToExtend and lex[ttoken].type == 'ASSIGN' and ('+' in lex[ttoken].value or (lex[ttoken+1].value == lex[ttoken-1].value and lex[ttoken+2].type == 'PLUS')): tmpcheck=False ; break


                                        for ttoken in range(token+1,len(lex)-1):
                                            #print(scope,lex[ttoken].type,lex[ttoken].value,noListcomp)
                                            if listEnd == False and lex[ttoken].type == 'LISTEND':
                                                scope-=1
                                                if scope<0: scope=0
                                                if scope == 0:
                                                    listEnd=ttoken
                                                    if lex[listEnd+1].type=='PLUS' and lex[listEnd+2].type=='LIST': tmpcheck=False
                                            else:
                                                if lex[ttoken].type == 'BUILTINF' and tokID in lex[ttoken].value \
                                                and any(i for i in listMods if i in lex[ttoken].value):
                                                    tmpcheck=False
                                                elif lex[ttoken].type == 'INDEX' and tokID in lex[ttoken].value:
                                                    tmpcheck=False
                                                elif lex[ttoken].type == 'ID':
                                                    if lex[ttoken-2].type in ('FUNCTION','BUILTINF') and any(i for i in listMods if i in lex[ttoken-2].value):
                                                        tmpcheck = False
                                                    elif lex[ttoken+1].type == 'ASSIGN' and lex[ttoken+2].value == lex[ttoken].value and (lex[ttoken+3].type == 'PLUS' or '+' in lex[ttoken+1].value):
                                                        tmpcheck=False
                                                    elif optListPlusListToExtend and lex[ttoken].value == tokID and lex[ttoken+1].type=='LINDEX':
                                                        tmpcheck=False
                                                    elif lex[ttoken-1].type == 'PIPE' \
                                                    and any(i for i in listMods if i.replace('.','') in lex[ttoken].value):
                                                        tmpcheck=False
                                                elif lex[ttoken].type == 'ID' and lex[ttoken+1].type == 'ASSIGN' \
                                                and lex[ttoken+1].value=='+=' and lex[ttoken].value == tokID:
                                                    tmpcheck=False
                                                elif lex[ttoken-1].type == 'LISTEND' and lex[ttoken].type == 'PIPE':
                                                    tmpcheck=False # ['my','list'] to func
                                                elif lex[ttoken].type=='LIST':
                                                    scope+=1
                                                elif lex[ttoken].type == 'BUILTINF':
                                                    if ']' in lex[ttoken].value:
                                                        scope-=lex[ttoken].value.count(']')
                                                    if '[' in lex[ttoken].value:
                                                        scope+=lex[ttoken].value.count('[')
                                                # list comp detection
                                                elif lex[ttoken].type == 'FOR' and scope==1:
                                                    noListcomp.append('FOR')
                                                elif lex[ttoken].type == 'INS' and scope==1:
                                                    noListcomp.append('INS')
                                                elif 'FOR' in noListcomp and 'INS' in noListcomp:
                                                    tmpcheck=False
                                                    scope+=lex[ttoken].value.count('[')
                                                    scope-=lex[ttoken].value.count(']')
                                                elif listisParen and lex[ttoken].type == 'RPAREN': listisParen=False ; scope-=1
                                                else:
                                                    scope+=lex[ttoken].value.count('[')
                                                    scope-=lex[ttoken].value.count(']')

                                            if tmpcheck==False and listEnd: break

                                        if tmpcheck and listEnd \
                                        and lexIndex+1<len(lex) and lex[listEnd+1].type != 'INDEX': # tuples are not subscriptable
                                            lex[token].value='(' # dont make its type a LPAREN
                                            lex[token].type = 'LPAREN' # ^ im a rebel
                                            lex[listEnd].value=')' ; lex[listEnd].type='RPAREN'
                                            if debug: print(f'replacing lex #{token} list with tuple: element(s) =',lex[token+1].value)
                                            newOptimization=True
                                        if listEnd == False and lex[token].type != 'INDEX': # INDEX is just [] ?
                                            pass
                                        else:
                                            lex[token].type='optLIST'
                                            if lex[token - 1].type in ('RINDEX', 'ID', 'BUILTINF'):
                                                # the reason up there i said dont make it LPAREN was because it broke
                                                # auto assignments to lists. this is easily fixed by detecting it
                                                # and just adding the ASSIGN ourself
                                                lex.insert(token, makeToken(lex[token], '=', 'ASSIGN'))

                        if optInSet and (token-1>0 and lex[token-1].type=='INS' and lex[token-1].value.strip() == 'in') and lex[token+1].type not in typeOperators:
                            # this is just for tokens with all of it packed into one value, multiple tokens are handled elsewhere
                            if lex[token].value[0] in ('(','[') and lex[token].value[-1] in (')',']'):
                                lex[token].value=lex[token].value[1:-1] ; lex[token].value='{%s}'%lex[token].value
                    elif lex[token].type == 'IMPORT':
                        tmp = lex[token].value.split()
                        if optFromFunc and not any(True for _ in tmp if _ == 'from'): # no from allowed
                            if len(tmp) > 2 and tmp[2] == 'as':
                                importName = tmp[3]+'.'
                            else:
                                importName = tmp[1]+'.'
                            if importName not in wasImported:
                                wasImported[importName]=[]
                                if len(tmp) > 2 and tmp[2] == 'as': # indicate the original import name so it may switch to it later
                                    wasImported[importName]=[f"AS!{tmp[1]+'.'}"]
                        elif '(' not in lex[token].value and tmp[0].strip() == 'from': # allow tracking unrelated to optFromFunc
                            trackingImported[tmp[1].strip()+'.']=[]
                            tmptmp = ''.join(tmp[3:]).split(',')
                            for t in tmptmp:
                                trackingImported[tmp[1].strip()+'.']+=[t.strip()]

                    elif lex[token].type in {'BUILTINF','FUNCTION'} or (lex[token].type == 'TYPE' and lex[token+1].type=='LPAREN'):
                        if optFromFunc:
                            tmpf = [i for i in wasImported if i in lex[token].value]
                            if len(tmpf) > 0:
                                # tmpf is moduleName. like random.
                                # func is all of the things after it, like .randint
                                restr=r'((?: |,)'+tmpf[0].replace('.','\\.')+r"""\w*\b(?=([^"'\\]*(\\.|("|')([^"'\\]*\\.)*[^"'\\]*("|')))*))"""
                                # thing\.\w*\b is all thats needed, rest is for excluding it if its in quotes
                                func=REfindall(restr,' '+lex[token].value,REMULTILINE)
                                if len(func) > 0: func = [f[0] for f in func]

                                if len(func) == 0 or any(i for i in func if i.count('.')>1): pass # must not be multi import like:  ctypes.c_int.from_address
                                elif lex[token] not in doNotModThisToken:
                                    if func[0].split('.')[-1] + '.' in tmpf: # dont not importname.importname.thing()
                                        doNotModThisToken.append(lex[token])

                                    lex[token].value=lex[token].value[len(tmpf[0]):] # replaces module.thing to thing

                                    if lex[token+1].type == 'LPAREN' and lex[token].value[-1]!='(':
                                        lex[token].type='FUNCTION'
                                        lex[token].value+='('
                                        del lex[token+1]
                                    for f in func:
                                        if f != '':
                                            if '.' in f:
                                                f=f[f.index('.')+1:]
                                            #print(len([True for t in range(token,len(lex)) if f == lex[t].value]));exit()
                                            if len([True for t in range(token,0,-1) if lex[t].type == 'ID' and f == lex[t].value]) > 0:
                                                # if its later defined. like randint = random.randint
                                                tmp=f'from {tmpf[0][:-1]} import {f} as AS{f}'
                                                if tmp not in code:
                                                    code.append(tmp)
                                                    for t in range(token,len(lex)):
                                                        if lex[t].type == 'BUILTINF' and lex[t].value == f'{tmpf[0]}{f}': lex[t].value=f'AS{f}'
                                                        elif lex[t].type == 'FUNCTION' and lex[t].value == f: lex[t].value=f'AS{f}'
                                            elif f not in wasImported[tmpf[0]]:
                                                # check if its a conflicting name
                                                for imported in wasImported:
                                                    if imported == tmpf[0]: # skip if self
                                                        continue
                                                    for importsFunction in wasImported[imported]:
                                                        if importsFunction == f: # if there is a match
                                                            ff=f
                                                            #f = f"{tmpf[0][:-1]}_{f}"
                                                            lex[token].value = f"{tmpf[0][:-1]}_{f}"
                                                            f='AS!'+f
                                                            if lex[token].type == 'FUNCTION': lex[token].value+='('
                                                            break
                                                if f not in wasImported[tmpf[0]]:
                                                    wasImported[tmpf[0]].append(f)
                                                    if tmpf[0] not in trackingImported: trackingImported[tmpf[0]]=[]
                                                    trackingImported[tmpf[0]].append(f)
                                    newOptimization=True

                        if optFuncTricks:
                            if optFuncTricksDict['randint']:
                                # tricks to speed up random.randint
                                check=randintOptimize=tmpGetRandBits=False
                                if compileTo == 'Cython': randTooBig=2147483647
                                else: randTooBig=18014398509481983 # above this it becomes non-random and is only even
                                if 'randint(' == lex[token].value and lex[token].type == 'FUNCTION' \
                                and token+5 < len(lex) and lex[token+2].value in ('0','-0') and lex[token+4].value != '1' and lex[token+4].value.startswith('0')==False and lex[token+5].type == 'RPAREN':
                                    if lex[token+4].value.isdigit()== False or int(lex[token+4].value) < randTooBig:
                                        randintOptimize=True
                                        lex[token].value='int(random()*%s'%(int(lex[token+4].value)+1 if lex[token+4].value.isdigit() else f"({lex[token+4].value}+1)")
                                        lex[token+1].type=lex[token+2].type=lex[token+3].type=lex[token+4].type='IGNORE'
                                        check=True
                                elif (lex[token].type == 'FUNCTION' and 'randint(' == lex[token].value                                                                                                                   and ((lex[token+1].type in ('NUMBER','ID') and lex[token+2].type=='COMMA' and lex[token+3].type in ('NUMBER','ID') and lex[token+4].type=='RPAREN' ) ) ) \
                                or (((lex[token].type == 'BUILTINF' and 'randint.randint' == lex[token].value and lex[token+1].type == 'LPAREN') or (lex[token].type == 'FUNCTION' and 'randint(' == lex[token].value) ) and ((lex[token+2].type in ('NUMBER','ID') and lex[token+3].type=='COMMA' and lex[token+4].type in ('NUMBER','ID') and lex[token+5].type=='RPAREN' ) ) ):
                                    # x+int((random()*((y+1)-x)))
                                    # x+int((random()*(y-x)))
                                    # when randint minimum is above 1
                                    randintOptimize=True
                                    tmpIsFunc=True if lex[token+2].type=='COMMA' else False
                                    if tmpIsFunc:
                                        tmpmax = lex[token + 3].value
                                        tmpmin = lex[token + 1].value
                                    else:
                                        tmpmax = lex[token + 4].value
                                        tmpmin = lex[token + 2].value
                                    tmpmax = tmpmax.strip() ; tmpmin = tmpmin.strip()
                                    tmpcheck = True
                                    if (tmpmax.isdigit() and (int(tmpmax) < 10 or int(tmpmax) > randTooBig)) or (tmpmin.isdigit() and tmpmin in {'0','-0','0.0','-0.0'}):
                                        check = tmpcheck = False
                                    if tmpcheck:
                                        if compileTo == 'Cython':
                                            insertAtTopOfCodeIfItIsNotThere("from libc.stdlib cimport rand, RAND_MAX, srand\nfrom libc.time cimport time as Ctime\nsrand(Ctime(NULL))\nrand()")
                                            lex[token].value=f"((rand() % ({tmpmax} - {tmpmin} + 1)) + {tmpmin})"
                                            lex[token].type=lex[token+1].type=lex[token+2].type=lex[token+3].type=lex[token+4].type='IGNORE'
                                            if not tmpIsFunc: lex[token+5].type = 'IGNORE'
                                            check=True
                                        else:
                                            lex[token].type = lex[token+1].type = lex[token+2].type = lex[token+3].type = 'IGNORE'
                                            if not tmpIsFunc: lex[token+4].type='IGNORE'
                                            lex[token].value=f"({tmpmin}+int((random()*({int(tmpmax)+1 if tmpmax.isdigit() else '('+tmpmax+' + 1)'} - {tmpmin})))"
                                            check=True
                                    elif tmpcheck == False and tmpmin in {'0','-0','0.0','-0.0'}:
                                        if tmpmax == '1':
                                            tmpGetRandBits = True
                                            lex[token].value = 'getrandbits(1'
                                            lex[token+1].type = lex[token+2].type = lex[token + 3].type = 'IGNORE'
                                            if not tmpIsFunc: lex[token + 4].type = 'IGNORE'
                                            check=True
                                        elif not tmpmax.isdigit() or int(tmpmax) < randTooBig:
                                            lex[token].value = f'random()*({tmpmax}+1)'
                                            if compileTo == 'Cython': lex[token].value='p!<int>!p('+lex[token].value
                                            else: lex[token].value='int('+lex[token].value
                                            if lex[token + 2].type == 'COMMA': lex[token + 3].type = 'IGNORE'
                                            lex[token + 1].type = lex[token + 2].type = 'IGNORE'
                                            check = True


                                if check:
                                    # turn token into many tokens for further parsing/optimizing
                                    lex[token].type = 'IGNORE'
                                    autoMakeTokens(lex[token].value,token)
                                else:
                                    randintOptimize = False

                                if randintOptimize:
                                    #if lex[token-1].type in {'TAB','NEWLINE'}: lex.insert(token, makeToken(lex[token],'defExp','DEFEXP'))
                                    # ^ https://github.com/AhriFoxSnek/ASnake/commit/f589f96e0cc4c11e209e6c96665f8618ac8bb285
                                    # ^ this commit says "+ optFuncTricksDict randint should trigger default expression"
                                    # ^ but that breaks expected behaviour of functions not trigger defexp unless operated upon
                                    newOptimization=True
                                    if 'random.' in wasImported:
                                        if 'random' in wasImported['random.']:
                                            pass
                                        else:
                                            wasImported['random.'].append('random')
                                    else:
                                        wasImported['random.']=['random']
                                if tmpGetRandBits:
                                    if 'random.' in wasImported:
                                        if 'getrandbits' in wasImported['random.']:
                                            pass
                                        else:
                                            wasImported['random.'].append('getrandbits')
                                    else:
                                        wasImported = {'random.': ['getrandbits']}
                                    newOptimization = True

                                if compileTo == 'Cython' and 'random(' == lex[token].value and lex[token+1].type == 'RPAREN':
                                    lex[token].type=lex[token+1].type = 'IGNORE'
                                    autoMakeTokens("rand() / (RAND_MAX + 1.0)", token)
                                    insertAtTopOfCodeIfItIsNotThere("from libc.stdlib cimport rand, RAND_MAX, srand\nfrom libc.time cimport time as Ctime\nsrand(Ctime(NULL))\nrand()")
                                    newOptimization=True

                            # ^^ randint -- vv list
                            if optFuncTricksDict['listString']:
                                if lex[token].value=='list(' and lex[token].type == 'FUNCTION' and lex[token+1].type == 'STRING' and lex[token-1].type in ('ID','ASSIGN'):
                                    check = True
                                    if lex[token-1].type == 'ASSIGN' and lex[token-3].type == 'COMMA':
                                        # var1 , var2 = list('blah'), 0
                                        # ^ we could handle that but for now nah
                                        check=False
                                    if check:
                                        # var is list('string')
                                        # to
                                        # var is [] then var += 'string'
                                        lex[token].type = 'INDEX' ; lex[token].value = '[]'
                                        tmptok=copy(lex[token])
                                        tmptok.type = 'THEN' ; tmptok.value = 'then'
                                        lex.insert(token+1,tmptok) ; del tmptok
                                        if lex[token-1].type == 'ID': tmp = 1
                                        else: tmp = 2
                                        tmptok=copy(lex[token-tmp])
                                        lex.insert(token+2,tmptok) ; del tmptok
                                        tmptok=copy(lex[token])
                                        tmptok.type = 'ASSIGN' ; tmptok.value = '+='
                                        lex.insert(token+3,tmptok) ; del tmptok
                                        for tmpi in range(token+2,len(lex)):
                                            if lex[tmpi].type == 'RPAREN': lex[tmpi].type='IGNORE' ; break
                                        newOptimization=True

                            # ^^ list -- vv tuple/set of string
                            if optFuncTricksDict['TupleSetUnpack']:
                                if (lex[token].value.strip() == 'tuple(' or lex[token].value.strip() == 'set(') \
                                and lex[token].type == 'FUNCTION' and lex[token+1].type == 'STRING' and lex[token+2].type == 'RPAREN':
                                    if lex[token].value.startswith('tuple'):
                                        lex[token].value='(' ; lex[token].type = 'LPAREN'
                                        lex.insert(token + 1, makeToken(lex[token], '*', 'TIMES'))
                                        lex.insert(token + 3, makeToken(lex[token], ',', 'COMMA'))
                                    else: # set
                                        lex[token].type=lex[token+1].type=lex[token+2].type='IGNORE'
                                        autoMakeTokens("{*"+lex[token+1].value+"}",token)
                                    newOptimization=True

                            if optCompilerEval and optCompilerEvalDict['evalLen'] and 'len' not in reservedIsNowVar:
                                if lex[token].value == 'ASlen' and lex[token+1].type=='LPAREN' and lex[token+2].type=='STRING' and lex[token+3].type=='RPAREN' \
                                or lex[token].value == 'len(' and lex[token+1].type=='STRING' and lex[token+2].type=='RPAREN':
                                    # eval len of string
                                    if lex[token].value == 'len(':
                                        if lex[token + 1].value.startswith('"""') or lex[token + 1].value.startswith("'''"):
                                            tmp = 6
                                        else: tmp = 2
                                        lex[token].value = str(len(lex[token + 1].value) - tmp)
                                    else:
                                        if lex[token + 2].value.startswith('"""') or lex[token + 2].value.startswith("'''"):
                                            tmp = 6
                                        else: tmp = 2
                                        lex[token].value = str(len(lex[token + 2].value) - tmp)
                                        lex[token + 3].type = 'IGNORE'
                                    lex[token].type='NUMBER'
                                    lex[token + 1].type=lex[token + 2].type='IGNORE'
                                    del lex[token + 1] ; del lex[token + 1]
                                    newOptimization=True
                                elif ((lex[token].value in {'ASlen','len'} and lex[token+1].type=='LPAREN' and lex[token+2].type in {'LPAREN','LIST','LINDEX','LBRACKET'}) \
                                or (lex[token].type == 'FUNCTION' and lex[token].value in {'ASlen(','len('} and lex[token+1].type in {'LPAREN','LIST','LINDEX','LBRACKET'} )):
                                    # eval len of list/tuple/set
                                    if lex[token].type == 'FUNCTION': tmp=1
                                    else: tmp=2
                                    tmpf = getNumberOfElementsInList(token+tmp)
                                    if tmpf != None and tmpf[0] > 1:
                                        if debug: print(f"! compilerEval: len of {lex[token+tmp].value}{lex[token+tmp+1].value}{lex[token+tmp+2].value}{lex[token+tmp+3].value} -->  {tmpf[0]}")
                                        lex[token].type = 'NUMBER' ; lex[token].value = str(tmpf[0])
                                        for t in range(token+1,tmpf[1]+2):
                                            lex[t].type='IGNORE'
                                        newOptimization = True

                            if optCompilerEval and optCompilerEvalDict['evalIntToFloat'] and lex[token].type == 'FUNCTION' and lex[token].value == 'float(' and lex[token + 1].type == 'NUMBER' and '.' not in lex[token + 1].value and lex[token + 2].type == 'RPAREN':
                                lex[token + 1].value += '.0'; lex[token].type = lex[token + 2].type = 'IGNORE'


                            if optFuncTricksDict['optCythonTypeFromConversionFunction'] and compileTo=='Cython' and any(True for x in convertType if x == lex[token].value.replace('(','')):
                                # x = str(12)  into -->   str x = str(12)
                                tmpi = None
                                if ((lex[token-1].type=='ID' and lex[token-1].value!='print' and lex[token-2].type in typeNewline+('CONST',)) or (lex[token-1].type=='ASSIGN' and lex[token-2].type=='ID' and lex[token-3].type in typeNewline+('CONST',))):
                                    tmpf = [x for x in convertType if x == lex[token].value.replace('(', '')][0]
                                    if tmpf not in definedFuncs:
                                        if lex[token-1].type=='ASSIGN': tmpi=token-2
                                        else: tmpi=token-1
                                if tmpi!=None:
                                    for ttt in range(token, 0, -1):  # checking backwards for typing. if its typed, don't type it again!
                                        if tmpi == None: break
                                        elif lex[ttt].type == 'ID' and lex[ttt].value == lex[tmpi].value:
                                            for tt in range(ttt, 0, -1):
                                                if lex[tt].type == 'TYPE':
                                                    tmpi=None
                                                    break
                                                elif lex[tt].type in typeNewline:
                                                    break
                                if tmpi!=None:
                                    lex.insert(tmpi, makeToken(lex[token], tmpf, 'TYPE'))
                                    newOptimization = True ;  token+=1

                            if optFuncTricksDict['boolTonotnot'] and (lex[token].type in ('FUNCTION','TYPE') and ((lex[token].value.strip() == 'bool' and lex[token+1].type == 'LPAREN') or lex[token].value == 'bool(')):
                                # x = bool(var)  -->  x = not not var
                                if '(' not in lex[token].value and lex[token+1].type == 'LPAREN':
                                    lex[token].type = lex[token + 1].type = 'INS'
                                    lex[token].value = lex[token + 1].value = 'not '
                                else:
                                    lex[token].type = 'IGNORE'
                                    lex.insert(token, makeToken(lex[token], 'not ', 'INS'))
                                    lex.insert(token, makeToken(lex[token], 'not ', 'INS'))
                                lex.insert(token, makeToken(lex[token], '(', 'LPAREN'))

                            if optFuncTricksDict['dictlistFunctionToEmpty'] and (lex[token].value == 'list(' or lex[token].value == 'dict(') and lex[token+1].type=='RPAREN':
                                # x = list()  -->  x = []
                                # x = dict()  -->  x = {}
                                if lex[token].value == 'list(': tmpType='list'
                                else: tmpType='dict'
                                if tmpType == 'list':
                                    lex[token].type='LIST' ; lex[token].value='['
                                    lex[token+1].type='LISTEND' ; lex[token+1].value=']'
                                else:
                                    lex[token].type = 'LBRACKET' ; lex[token].value = '{'
                                    lex[token + 1].type = 'RBRACKET' ; lex[token + 1].value = '}'

                            if optFuncTricksDict['roundFast'] and (lex[token].value in {'round','round(','ASround(','ASround'} and lex[token+1].type in {'ID','NUMBER'} and lex[token+2].type == 'COMMA' and lex[token+3].type == 'NUMBER' and lex[token+4].type == 'RPAREN') \
                            or (lex[token].value in {'round','round(','ASround(','ASround'} and lex[token+1].type == 'LPAREN'  and lex[token+2].type in {'ID','NUMBER'} and lex[token+3].type == 'COMMA' and lex[token+4].type == 'NUMBER' and lex[token+5].type == 'RPAREN'):
                                # fast rounding, majorly accurate
                                # forumula: int(some_float * (10 ** TOLERANCE) - (.5 if some_float < 0 else -.5)) / (10 ** TOLERANCE)

                                if lex[token+1].type == 'LPAREN':
                                      tmpAdjust=1 ; lex[token+4].type='IGNORE'
                                else: tmpAdjust=0


                                if optLoopAttr and preAllocated and 'ASint' in (p[1] for p in preAllocated): lex[token].value='ASint('
                                elif compileTo == 'Cython': lex[token].value = '<int>('
                                else: lex[token].value='int('
                                if optCompilerEval and optCompilerEvalDict['evalTokens'] and lex[token+1+tmpAdjust].type == 'NUMBER' == lex[token+3+tmpAdjust].type:
                                    try: int(lex[token+3+tmpAdjust].value)
                                    except ValueError:
                                        tmpE=lex[token+3+tmpAdjust].value
                                        return AS_SyntaxError(
                                            f'You are rounding to {tmpE} instead of a integer.',
                                            f'round({lex[token+1+tmpAdjust].value}, {tmpE.split(".")[0]})',
                                            lex[token].lineno, data, 'Function Argument Error:')
                                    tmpTolerance=lex[token+3+tmpAdjust].value ; tmpFloat = lex[token+1+tmpAdjust].value
                                    tmp=f"{round(float(lex[token+1+tmpAdjust].value),int(lex[token+3+tmpAdjust].value))}"
                                    lex[token].type = 'LPAREN' ; lex[token].value = '('
                                else:
                                    tmpFloat = lex[token+1+tmpAdjust].value
                                    tmpTolerance = 10**int(lex[token+3+tmpAdjust].value)
                                    tmp=f"{tmpFloat} * ({tmpTolerance}) - (.5 if {tmpFloat} < 0 else -.5)) / ({tmpTolerance}"
                                if debug: print(f'! fast-round: round({tmpFloat},{lex[token+3+tmpAdjust].value})  -->  {tmp}')
                                lex[token + 1].type = lex[token + 2].type = lex[token + 3].type = 'IGNORE'
                                autoMakeTokens(tmp, token)
                                newOptimization=True
                                lex.insert(token,makeToken(lex[token],'DONTDEXP','DONTDEXP'))


                            if optCompilerEval and optCompilerEvalDict['evalStrFunc'] and lex[token].type == 'FUNCTION' and lex[token].value.replace('(','') == 'str' and lex[token+1].type in {'NUMBER','STRING'} and lex[token+2].type == 'RPAREN':
                                # str(12) --> "12"
                                lex[token].type = lex[token+2].type = 'IGNORE'
                                if  lex[token+1].type == 'NUMBER':
                                    lex[token+1].type = 'STRING' ; lex[token+1].value = f'"{lex[token+1].value}"'
                                if lex[token-1].type in typeNewline and lex[token+3].type in typeNewline:
                                    lex[token].type = 'DONTDEXP'
                                if debug and lex[token+1].type == 'NUMBER': print(f'! evalStr: str({lex[token + 1].value}) --> "{lex[token + 1].value}"')
                                elif debug: print(f'! evalStr: str({lex[token + 1].value}) --> {lex[token + 1].value}')
                                newOptimization = True

                            if optFuncTricksDict['max2compare'] and optCompilerEval and (('max' not in reservedIsNowVar and lex[token].type == 'FUNCTION' and lex[token].value == 'max(') or ('min' not in reservedIsNowVar and lex[token].type == 'FUNCTION' and lex[token].value == 'min(')) \
                            and lex[token+1].type in {'ID','BUILTINF','NUMBER'} and lex[token+2].type == 'COMMA' and lex[token+3].type in {'ID','BUILTINF','NUMBER'} and lex[token+4].type == 'RPAREN':
                                # max(x,y) --> (x if x > y else y)  can only be done if there are two elements
                                # for pypy it's faster to do branchless bit stuff: x ^ ((x ^ y) & -(x < y))
                                tmpIsMax = True if lex[token].value == 'max(' else False
                                tmp1st=lex[token+1].value ; tmp2nd=lex[token+3].value
                                lex[token].type=lex[token+1].type=lex[token+2].type=lex[token+3].type=lex[token+4].type='IGNORE'
                                if lex[token-1].type in typeNewline and lex[token+5].type in typeNewline: lex[token].type = 'DONTDEXP'
                                elif lex[token-1].type in typeNewline and lex[token+5].type in typePrintable: lex[token].type = 'DEFEXP'
                                elif lex[token-1].type in typePrintable:
                                    tmp=token ; tmpSafe=True
                                    for t in range(token,0,-1):
                                        if lex[t].type in typeNewline:
                                            if lex[t+1].type in {'FUNCTION','BUILTINF','CONSTANT','TYPE'} \
                                            or (lex[t+1].type == 'ID' and lex[t+2].type in ('ASSIGN','FUNCTION')+typeAssignables)\
                                            or (lex[t+1].type == 'ID' and lex[t+1].value.strip() in ('print',)+tuple(definedFunctions)):
                                                tmpSafe=False
                                            tmp=t+1 ; break
                                    if tmpSafe:
                                        lex.insert(tmp,makeToken(lex[token],'','DEFEXP'))

                                if compileTo == 'PyPy3':
                                    tmp = tmp1st if tmpIsMax else tmp2nd
                                    autoMakeTokens(f"({tmp} ^ (({tmp1st} ^ {tmp2nd}) & -({tmp1st} < {tmp2nd})))", token)
                                else:
                                    tmp = '>' if tmpIsMax else '<'   
                                    autoMakeTokens(f"({tmp1st} if {tmp1st} {tmp} {tmp2nd} else {tmp2nd})",token)
                                if debug: print(f"! max2compare: {'max' if tmpIsMax else 'min'}({tmp1st},{tmp2nd}) --> ({tmp1st} if {tmp1st} {tmp} {tmp2nd} else {tmp2nd})")

                            if compileTo == 'Cython' and optFuncTricksDict['optCythonConvertTo_libc'] \
                            and lex[token].type == 'FUNCTION' and lex[token].value in {'log(', 'abs(','floor(','remainder(','cos(','tan(','acos('}:
                                # these Cythonization functions may break behaviour, so remove when necessary
                                insertAtTopOfCodeIfItIsNotThere(f"from libc.math cimport {lex[token].value[:-1]} as C{lex[token].value[:-1]}")
                                lex[token].value = 'C' + lex[token].value

                            if optFuncTricksDict['startsWithToIndex'] and lex[token].type == 'BUILTINF' and lex[token].value.endswith('.startswith') \
                            and lex[token+1].type == 'LPAREN' and lex[token+2].type == 'STRING' and lex[token+3].type == 'RPAREN':
                                tmpf = lex[token+2].value
                                tmpQuote = ''
                                for i in tmpf:
                                    if i == '"':   tmpQuote = i; break
                                    elif i == "'": tmpQuote = i; break
                                while tmpf[0] != tmpQuote:
                                    tmpf=tmpf[1:]
                                while tmpf[0] == tmpQuote:
                                    tmpf=tmpf[1:]
                                while tmpf[-1] == tmpQuote:
                                    tmpf=tmpf[:-1]
                                if len(tmpf) == 1:
                                    lex[token].type=lex[token+1].type=lex[token+2].type=lex[token+3].type='IGNORE'
                                    tmpVar = '.'.join(lex[token].value.split('.')[:-1])
                                    tmp=f"({tmpVar}[0] == '{tmpf}') if {tmpVar} else {tmpVar}"
                                    if debug: print(f"! startswith to index {lex[token].value}({lex[token + 2].value})  -->  {tmp}")
                                    autoMakeTokens(tmp,token-1)
                                    newOptimization = True

                            elif optFuncTricksDict['idToIs'] and lex[token].type == 'FUNCTION' and lex[token].value.startswith('id') and lex[token+1].type == 'ID' and lex[token+2].type == 'RPAREN' and lex[token+3].type == 'EQUAL' \
                            and lex[token+4].type == 'FUNCTION' and lex[token+4].value.startswith('id') and lex[token+5].type == 'ID' and lex[token+6].type == 'RPAREN':
                                tmp=(lex[token+1].value,lex[token+5].value)
                                lex[token].type=lex[token+1].type=lex[token+2].type=lex[token+3].type=lex[token+4].type=lex[token+5].type=lex[token+6].type="IGNORE"
                                if pyIs:
                                    autoMakeTokens(f"{tmp[0]} is {tmp[1]}", token-1)
                                else:
                                    lex.insert(token, makeToken(lex[token], tmp[1], 'ID'))
                                    lex.insert(token, makeToken(lex[token], 'is', 'PYIS'))
                                    lex.insert(token, makeToken(lex[token], tmp[0], 'ID'))
                                if debug:
                                    print(f"!idToIs: id({tmp[0]}) == id({tmp[1]})  -->  {tmp[0]} is {tmp[1]}")



                        if optCompilerEval:
                            if optCompilerEvalDict['evalChrFunc'] and lex[token].value == 'chr(' and lex[token+1].type == 'NUMBER' and lex[token+2].type == 'RPAREN':
                                safe=True
                                if pythonVersion < 3.12 and lex[token+1].value in {'92','39'}:
                                    for t in range(token, 0, -1):  # check to make sure we are not in fstring first
                                        if lex[t].type == 'FSTR' and lex[t].value.endswith('{'): safe = False ; break
                                        elif lex[t].type in {'TAB', 'NEWLINE'}: break
                                if safe:
                                    try:
                                        if   lex[token+1].value == '92':
                                            lex[token].value = "'\\\\'"
                                        elif lex[token+1].value == '39':
                                            lex[token].value = "'\\\''"
                                        else:
                                            lex[token].value = f"'{chr(int(lex[token + 1].value))}'"
                                        lex[token+1].type=lex[token+2].type='IGNORE'
                                        lex[token].type = 'STRING'
                                        if debug: print(f"! evalChrFunc: chr({lex[token + 1].value}) --> {lex[token].value}")
                                    except (TypeError, ValueError): pass
                            elif optCompilerEvalDict['evalIntFunc'] and lex[token].value == 'int(' and lex[token+1].type in {'STRRAW','STRING','NUMBER'} and lex[token+2].type == 'RPAREN':
                                # int('12') --> 12
                                safe=True
                                if lex[token + 1].type == 'STRING' and stripStringQuotes(lex[token + 1].value).startswith('0x'):
                                    # int("0x99") --> 153   string would normally cause ValueError.
                                    # this is an error correction, meaning it does break behaviour, but in a way that fixes a bug
                                    try:
                                        lex[token].value = f"{int(stripStringQuotes(lex[token + 1].value), 0)}"
                                    except ValueError: safe=False
                                else: # all other cases
                                    tmpOG = lex[token + 1].value ; tmpIsHex = False
                                    if lex[token+1].type in {'STRRAW','STRING'}: lex[token+1].value = f"{stripStringQuotes(lex[token+1].value)}"
                                    elif lex[token+1].type == 'NUMBER' and lex[token+1].value.startswith('0x'): tmpIsHex=True
                                    try:
                                        if tmpIsHex:
                                            lex[token].value = f"{int(lex[token+1].value, 0)}"
                                        else:
                                            lex[token].value = f"{int(lex[token+1].value)}"
                                    except ValueError:
                                        if lex[token + 1].type in {'STRRAW', 'STRING'}:
                                            return AS_SyntaxError(f'Inside of int({tmpOG}) a ValueError occurs.\n\tString needs to be an integer or float.','int("12")', lex[token].lineno, data)
                                        safe = False ; lex[token+1].value = tmpOG
                                    del tmpOG
                                if safe:
                                    lex[token].type = 'NUMBER'
                                    lex[token + 1].type = lex[token + 2].type = 'IGNORE'
                                    if lex[token-1].type in typeNewline and lex[token+3].type in typeNewline: lex.insert(token,makeToken(lex[token],'','DONTDEXP'))
                            elif optCompilerEvalDict['evalStrCenter'] and lex[token].type == 'BUILTINF' and lex[token].value[0] in {'"',"'"} and lex[token].value.endswith('.center') and lex[token+1].type == 'LPAREN' and lex[token+2].type == 'NUMBER' and '.' not in lex[token+2].value and ((lex[token+3].type == 'COMMA' and lex[token+4].type == 'STRING' and len(stripStringQuotes(lex[token+4].value)) == 1 and lex[token+5].type == 'RPAREN') or (lex[token+3].type == 'RPAREN')):
                                tmpQuote = lex[token].value[0]
                                if lex[token+3].type == 'COMMA':
                                    lex[token+4].type=lex[token+5].type='IGNORE'
                                    tmpFill = stripStringQuotes(lex[token+4].value)
                                else:
                                    tmpFill = ' '
                                lex[token].value = tmpQuote + stripStringQuotes(lex[token].value.split('.')[0]).center(int(lex[token+2].value),tmpFill) + tmpQuote
                                lex[token+1].type=lex[token+2].type=lex[token+3].type='IGNORE'
                                lex[token].type = 'STRING'



                        if optLoopAttr and preAllocated and lex[token].value.startswith('AS') == False and 'AS'+lex[token].value.replace('.','_').replace('(','') in (p[1] for p in preAllocated) \
                        and lex[token-1].type not in {'ID','ASSIGN'}:
                            # if in preAllocated, replace it
                            if debug: print(f"! attrs: {lex[token].value} --> {'AS' + lex[token].value.replace('.', '_')} {lex[token+1].value}")
                            lex[token].value = 'AS' + lex[token].value.replace('.', '_')
                            if lex[token].type == 'BUILTINF':
                                lex[token].type  = 'ID'
                            newOptimization=True


                    elif lex[token].type == 'PIPE':
                        if optFuncTricks:
                            if optFuncTricksDict['optCythonTypeFromConversionFunction'] and compileTo=='Cython' and any(True for x in convertType if x == lex[token+1].value.replace('(','')) and lex[token+2].type != "PIPE":
                                # x = 12 to str  into -->   str x = 12 to str
                                tmpi = None
                                for i in range(token - 1, 0, -1):
                                    if lex[i].type in typeNewline: break
                                    elif lex[i].type == 'ASSIGN' and lex[i - 1].type == 'ID' and lex[i-2].type != 'TYPE':
                                        tmpi = i - 1 ; break
                                    elif lex[i].type == 'ID' and lex[i - 1].type in typeNewline + ('CONST',):
                                        if lex[i+1].type == 'ID': tmpi = i
                                        else: tmpi=None
                                        break
                                    elif lex[i].type == 'TYPE': tmpi=None ; break
                                if tmpi!=None:
                                    tmpf = [x for x in convertType if x == lex[token + 1].value.replace('(', '')][0]
                                    lex.insert(tmpi, makeToken(lex[token], tmpf, 'TYPE'))
                                    token+=1
                                    newOptimization = True

                            if optFuncTricksDict['boolTonotnot'] and lex[token+1].value == 'bool':
                                # x = var to bool  -->  x = not not var
                                if 'in' in lex[token].value: # into
                                    tmpf=True
                                else: # to
                                    tmpf=False
                                if not tmpf or lex[token-2].type != 'PIPE':
                                    lex[token + 1].type = 'IGNORE'
                                    lex[token].type = 'RPAREN' ; lex[token].value = ')'
                                    tmpIndex=False
                                    for tmpi in range(token-1,0,-1):
                                        if lex[tmpi].type in typeNewline:
                                            if not tmpIndex: tmpIndex = tmpi + 1
                                            break
                                        elif lex[tmpi].type == 'PIPEGO':
                                            tmpIndex = tmpi ; break
                                        elif lex[tmpi].type == 'ASSIGN':
                                            tmpIndex = tmpi+1 ; break
                                    lex.insert(tmpIndex, makeToken(lex[token], 'not ', 'INS'))
                                    lex.insert(tmpIndex, makeToken(lex[token], 'not ', 'INS'))
                                    lex.insert(tmpIndex, makeToken(lex[token], '( ', 'LPAREN'))

                    elif lex[token].type in {'LOOP','WHILE','FOR'} and lex[token-1].type in typeNewline+('DEFFUNCT',):
                        # 2nd conditional is to make sure its not in a list comp or other odd situation

                        # check to make sure we're not in tabbed list comp sorta deal
                        tmpscope = 0
                        for tmpi in range(token - 2, 0, -1):
                            if lex[tmpi].type == 'RPAREN':
                                tmpscope -= 1
                            elif lex[tmpi].type in ('LPAREN', 'FUNCTION'):
                                tmpscope += 1
                            elif lex[tmpi].type in typeNewline:
                                break
                        if tmpscope <= 0:  # if paren scope negative (complete) or zero (nonexistent)
                            if optLoopAttr: # the real start of optLoopAttr
                                if lex[token-1].type == 'TAB':
                                    tmpindent=lex[token-1].value.count(' ')
                                else: tmpindent=0
                                tmpf=[]
                                importcheck=[i for i in wasImported]
                                for i in wasImported:
                                    for ii in wasImported[i]:
                                        importcheck.append(ii)
                                assignCheck=[] # a flag that says 'hey, check if this name gets assigned to later'
                                ignoreVars = []
                                firstIndent=False # wait until first indent when doing the optimization on for loops: for x in thing.split()  would not benefit since it runs only once
                                if lex[token].type != 'FOR': firstIndent=True # should only apply to for loops, see above ^
                                else:
                                    # don't optimize the iterator variables in for loop
                                    for ii in range(token+1,len(lex)):
                                        if lex[ii].type == 'INS': break
                                        elif lex[ii].type == 'COMMA': pass
                                        else: ignoreVars.append(lex[ii].value)

                                for tmpi in range(token+1,len(lex)-1):
                                    if lex[tmpi].type == 'TAB':
                                        if lex[tmpi].value.count(' ') <= tmpindent: break
                                        else: firstIndent=True
                                    elif lex[tmpi].type == 'NEWLINE': break
                                    elif lex[tmpi].type=='BUILTINF' and '.' in lex[tmpi].value and lex[tmpi].value[0] not in ('"',"'",'.') and not lex[tmpi].value.startswith('self.') and not lex[tmpi].value.startswith('cls.') and not lex[tmpi].value.startswith(lex[token+1].value+'.') and lex[tmpi].value.split('.')[0] not in ignoreVars and firstIndent and not (optFuncTricks and optFuncTricksDict['popToDel'] and lex[tmpi].value.split('.')[-1] == 'pop' and ((lex[tmpi+2].type in {'NUMBER','ID'} and lex[tmpi+4].type in typeNewline) or (lex[tmpi+2].type=='MINUS' and lex[tmpi+3].value != '1' and lex[tmpi+5].type in typeNewline )) ):
                                        check=False ; minindent=None
                                        tmpImportName=lex[tmpi].value.split('.')[0]
                                        if tmpImportName+'.' in importcheck: break
                                        elif tmpImportName in importcheck: check=True
                                        if check == False:
                                            # search backwards for evidence if its safe to use the variable
                                            for ii in range(token-2,0,-1): # BEHIND LOOP
                                                #if debug: print(lex[tmpi].value,check,lex[ii].type,lex[ii].value,'|',lex[tmpi].value)
                                                if lex[ii].type in ('ID','BUILTINF') and lex[ii].value.startswith(lex[tmpi].value.split('.')[0])\
                                                and lex[ii+1].type in typeAssignables+('ASSIGN',):
                                                    if (lex[ii-1].type == 'TAB' and lex[ii-1].value.count(' ') >= tmpindent) or (lex[ii-1].type in ('NEWLINE','THEN') or (lex[ii-1].type=='TYPE' and lex[ii-2].type in ('NEWLINE','THEN')) or (lex[ii-1].type=='CONSTANT' and lex[ii-2].type in ('NEWLINE','THEN')) or (lex[ii-1].type=='TYPE' and lex[ii-2].type=='CONSTANT' and lex[ii-3] in ('NEWLINE','THEN')) ):
                                                        check=True ; break
                                                    elif lex[ii-1].type == 'TYPE' and ((lex[ii-2].type=='TAB' and lex[ii-2].value.count(' ') >= tmpindent) or lex[ii-2].type in ('NEWLINE','THEN')): check=True;break
                                                elif lex[ii].type == 'PYDEF':
                                                    tmp=REsearch(r"[\w_\d]+\(.*(?=\))",lex[ii].value)
                                                    if tmp!=None and lex[tmpi].value.split('.')[0] in tmp[0].lstrip('('):
                                                        check=True ; break
                                                elif lex[ii].type == 'TAB' and lex[ii].value.count(' ') >= tmpindent and minindent!=None and minindent < lex[ii].value.count(' '):
                                                    break # if it lowers scope then increases scope while backwards, we cant be too sure about this
                                                elif lex[ii].type == 'TAB' and (minindent==None or lex[ii].value.count(' ') < minindent):
                                                    minindent=lex[ii].value.count(' ')
                                            for ii in range(tmpi,0,-1): # search backwards within expression
                                                tmpIndex = ii
                                                if lex[ii].type == 'ID' and lex[ii].value.startswith(lex[tmpi].value.split('.')[0]) and lex[ii + 1].type == 'INS' and lex[ii - 1].type == 'FOR':
                                                    check = False # dont want to optimize listcomps as it could call builtin of something that doesn't exist yet: sum(1 for c in word if c.isupper())
                                                    break
                                                elif lex[ii].type in typeNewline: break
                                            for ii in range(tmpIndex,token,-1): # search behind within the loop
                                                if lex[ii].type in ('ID','BUILTINF') and lex[ii].value.startswith(lex[tmpi].value.split('.')[0]) and lex[ii-1].type in {'WITHAS','FOR'}:
                                                    # WITHAS / FOR makes a new variable, which we cannot preallocate properly
                                                    check = False ; break
                                        if check:
                                            assignCheck.append(lex[tmpi].value.split('.')[0])
                                            minicheck=True
                                            for ii in range(tmpi,token,-1):
                                                if lex[ii].type == 'ID' and lex[ii].value == assignCheck[-1] and lex[ii+1].type == 'LINDEX':
                                                    minicheck=False ; break
                                            if minicheck:
                                                tmpf.append([lex[tmpi].value.rsplit('(')[0],tmpi])
                                    elif lex[tmpi].type == 'ID':
                                        if lex[tmpi].value in assignCheck and lex[tmpi+1].type in ('ASSIGN',)+typeAssignables:
                                            # if the thing in thing.attr is assigned in the loop, don't make this optimization
                                            tmpf=[i for i in tmpf if lex[tmpi].value != i[0].split('.')[0]]
                                        elif lex[tmpi+1].type in ('ASSIGN',)+typeAssignables:
                                            ignoreVars.append(lex[tmpi].value)
                                    elif firstIndent and lex[tmpi].type == 'FUNCTION' and lex[tmpi].value[:-1] in pyBuiltinFunctions and lex[tmpi].value!='range(':
                                        # built-in functions are faster when pre-assigned
                                        tmpf.append([lex[tmpi].value[:-1],tmpi])
                                        if lex[tmpi].value[-1]=='(': lex[tmpi].value=lex[tmpi].value[:-1] # bleh fix
                                        lex.insert(tmpi+1,makeToken(tok,'(','LPAREN'))
                                del importcheck ; del assignCheck


                                if len(tmpf) > 0:
                                    lexAdd=0
                                    tmp=[]
                                    tmpInsertHere = token # all prealloc in the same spot
                                    for t in tmpf:
                                        tmpval=t[0].split('.')
                                        check=False
                                        if tmpval[-1] in listMods or '.'+tmpval[-1] in listMods:
                                            for ii in range(token-1,0,-1):
                                                if lex[ii].type == 'ID' and lex[ii].value == tmpval[0]:
                                                    check=True ; break
                                        elif tmpval[0]+'.' in wasImported: pass
                                        else: check=True
                                        if check == False : break

                                        if '.' in t[0]: tmpname='_'.join(t[0].split('.'))
                                        else: tmpname=t[0]
                                        subBy=1
                                        tmpASname = f'AS{tmpname}'
                                        if t[0] in pyBuiltinFunctions:
                                            # check backward for better assign point, incase already used.
                                            # sometimes can make use of the function before the loop
                                            tmptmpIndent=tmpCurrentIndent=None ; tmpFound=False
                                            tmpChangeTheseIfFound=[]
                                            #print('~~~')
                                            for ii in range(token - 1, 0, -1):
                                                #print(tmpASname,lex[ii].type,lex[ii].value,tmpFound,tmpCurrentIndent,tmptmpIndent)
                                                if lex[ii].type == 'TAB':
                                                    if tmptmpIndent == None: tmptmpIndent=lex[ii].value.count(' ')
                                                    tmpCurrentIndent = lex[ii].value.count(' ')
                                                    if tmpCurrentIndent > tmptmpIndent and tmpFound: tmpFound=False ; break
                                                    if tmpFound and lex[ii+1].type not in {'ELIF','OF','ELSE'}:
                                                        tmpindent=tmpCurrentIndent
                                                        if ii+1 < tmpInsertHere:
                                                            tmpInsertHere = ii + 1
                                                    elif tmpCurrentIndent < tmptmpIndent: break
                                                elif lex[ii].type == 'NEWLINE':
                                                    if tmptmpIndent == None: tmptmpIndent = 0
                                                    tmpCurrentIndent = 0
                                                    if tmpCurrentIndent < tmptmpIndent: break
                                                    if tmpCurrentIndent > tmptmpIndent and tmpFound: tmpFound = False; break
                                                    if tmpFound and lex[ii+1].type not in {'ELIF','OF','ELSE'}: tmpInsertHere = ii + 1; break
                                                elif lex[ii].type == 'FUNCTION' and lex[ii].value.strip() == t[0]+'(' and tmpCurrentIndent == tmptmpIndent:
                                                    tmpFound=True ; tmpChangeTheseIfFound.append(ii)
                                            if tmpFound:
                                                for f in tmpChangeTheseIfFound: lex[f].value = tmpASname+'('


                                        if tmpASname not in tmp and (tmpindent,tmpASname) not in preAllocated \
                                        and (not preAllocated or all(True if pa[1] != tmpASname or pa[0] > tmpindent else False for pa in preAllocated) ):
                                            #print(tmpASname,tmpInsertHere,lex[tmpInsertHere].value,lex[tmpInsertHere+1].value,)
                                            preAllocated.add((tmpindent,tmpASname))
                                            if lex[tmpInsertHere - 1].type == 'DEFFUNCT': subBy = 0
                                            #
                                            tmptok=copy(lex[tmpInsertHere])
                                            tmptok.value=t[0]
                                            tmptok.type='BUILTINF'
                                            lex.insert(tmpInsertHere-subBy,tmptok)
                                            #
                                            tmptok=copy(lex[tmpInsertHere])
                                            tmptok.value='='
                                            tmptok.type='ASSIGN'
                                            lex.insert(tmpInsertHere-subBy,tmptok)
                                            #
                                            tmptok=copy(lex[tmpInsertHere])
                                            tmptok.value=tmpASname
                                            tmptok.type='ID'
                                            lex.insert(tmpInsertHere-subBy,tmptok)
                                            #
                                            if subBy > 0: # not in def
                                                tmptok=copy(lex[tmpInsertHere])
                                                tmptok.value=f'\n{" "*tmpindent}' if tmpindent > 0 else '\n'
                                                tmptok.type='TAB' if tmpindent > 0 else 'NEWLINE'
                                                tmptok.lineno=0 # to not mess up lineNumber for errors
                                                lex.insert(tmpInsertHere-subBy,tmptok)
                                            else: # in def
                                                lex.insert(tmpInsertHere+3,makeToken(tok,'then','THEN'))
                                            #
                                            lexIndex+=4
                                            lexAdd+=4
                                            tmp.append(tmpASname)
                                            if debug: print(f'! attrs: {t[0]} --> {tmp[-1]}')
                                            newOptimization = True
                                        lex[t[1]+lexAdd].value=tmpASname
                                    #[print(l.value,end=' ') for l in lex]
                                    #print()

                                if optNestedLoopItertoolsProduct and lex[token].type in {'LOOP','FOR'}:
                                    # converts nested loop to use itertool's product function.
                                    # ? only safe in cases where loop is directly after the other ? (could be safe, but for now we're cautious)
                                    # for y in 12 do for x in 24 do for z in '48'  -->  for y, x in ASproduct(range(12),range(24),'48'):
                                    # loop 12 y loop 24 x loop '48' z --> same as above
                                    # auto convert to range for known int. otherwise just pass everything after in.

                                    tmpIterables=[]
                                    # list of lists. the lists first element should contain the iterator variable.
                                    # the rest of the elements is the lex of the iterable.

                                    # get indent
                                    tmpIndent = 0
                                    for tmpi in range(token,0,-1):
                                        if lex[tmpi].type == 'NEWLINE':
                                            tmpIndent=0 ; break
                                        elif lex[tmpi].type == 'TAB':
                                            tmpIndent=lex[tmpi].value.count(' ') ; break

                                    # gather loop info
                                    tmpEndPoint=token
                                    safe=True ; tmpCurrent=None ; tmpAfterIn=False ; tmpSawNL=True
                                    # tmpCurrent is the type of loop
                                    # tmpAfterIn is when iterable token gathering begins
                                    # tmpSawNL ensures that the loops are on seperate lines
                                    tmpListScope=0
                                    for tmpi in range(token,len(lex)-1):
                                        if lex[tmpi].type in {'FOR','LOOP'}:
                                            tmpCurrent = lex[tmpi].type
                                            tmpIterables.append([])
                                            tmpAfterIn = False
                                            if not tmpSawNL: safe=False ; break
                                            else: tmpSawNL=False
                                        elif lex[tmpi].type in {'LIST','LINDEX'}: tmpListScope+=1
                                        elif lex[tmpi].type in {'LISTEND', 'RINDEX'}: tmpListScope -= 1
                                        elif lex[tmpi].type in typeNewline: tmpSawNL=True

                                        if tmpCurrent == 'LOOP':
                                            if lex[tmpi].type == 'ID' and lex[tmpi+1].type in typeNewline+('LOOP',) and tmpListScope == 0:
                                                tmpIterables[-1]=[lex[tmpi].value]+tmpIterables[-1]
                                            else:
                                                if lex[tmpi].type in typeNewline+('LOOP',):
                                                    if lex[tmpi].type == 'NEWLINE':
                                                        safe = False ; break
                                                    elif lex[tmpi].type == 'TAB' and lex[tmpi].value.count(' ') <= tmpIndent:
                                                        safe = False ; break
                                                    elif lex[tmpi].type!='LOOP' and lex[tmpi + 1].type not in {'FOR', 'LOOP'}:
                                                        tmpEndPoint = tmpi
                                                        break
                                                    elif tmpAfterIn and lex[tmpi].type == 'LOOP' and tmpIterables[-1] and isinstance(tmpIterables[-1][0], str) == False:
                                                        safe = False ; break
                                                    elif not tmpAfterIn and lex[tmpi].type == 'LOOP':
                                                        tmpAfterIn = True
                                                else:
                                                    if lex[tmpi].type == 'NUMBER':
                                                        tmpIterables[-1].append(makeToken(lex[tmpi], 'range(', 'FUNCTION'))
                                                        tmpIterables[-1].append(copy(lex[tmpi]))
                                                        tmpIterables[-1].append(makeToken(lex[tmpi], ')', 'RPAREN'))
                                                    elif tmpListScope == 0:
                                                        tmpIterables[-1].append(copy(lex[tmpi]))
                                        elif tmpCurrent == 'FOR':
                                            if not tmpAfterIn:
                                                if lex[tmpi-1].type == 'FOR' and lex[tmpi].type == 'ID' and lex[tmpi+1].type == 'INS' and tmpListScope == 0:
                                                    tmpIterables[-1].append(lex[tmpi].value)
                                                elif lex[tmpi].type == 'INS' and 'in' in lex[tmpi].value:
                                                    tmpAfterIn = True
                                            else:
                                                if lex[tmpi].type in typeNewline:
                                                    if lex[tmpi].type == 'NEWLINE':
                                                        safe = False ; break
                                                    elif lex[tmpi].type == 'TAB' and lex[tmpi].value.count(' ') <= tmpIndent:
                                                        safe = False ; break
                                                    elif lex[tmpi+1].type not in {'FOR','LOOP'}:
                                                        tmpEndPoint = tmpi
                                                        break
                                                else:
                                                    if lex[tmpi].type == 'NUMBER' and lex[tmpi+1].type in typeNewline+('FOR','LOOP','ENDIF') and not tmpIterables[-1]:
                                                        tmpIterables[-1].append(makeToken(lex[tmpi],'range(','FUNCTION'))
                                                        tmpIterables[-1].append(copy(lex[tmpi]))
                                                        tmpIterables[-1].append(makeToken(lex[tmpi], ')', 'RPAREN'))
                                                    elif lex[tmpi].type == 'ENDIF':
                                                        pass
                                                    elif tmpListScope == 0:
                                                        tmpIterables[-1].append(copy(lex[tmpi]))

                                    # safety checks
                                    tmpIterables = [_ for _ in tmpIterables if _]
                                    if len(tmpIterables) <= 2: safe=False # is only always faster when there are more than two nested loops

                                    if safe:
                                        tmpIndents = [tmpIndent+(prettyIndent*i) for i in range(len(tmpIterables))]
                                        #print('~~~~_____________________________________________')
                                        for tmpi in range(tmpEndPoint+1,len(lex)-1):
                                            # if there is a expression one indent above baseline after the loop(s), it is not safe.
                                            if lex[tmpi].type == 'NEWLINE' and lex[tmpi+1].type != 'TAB': break
                                            elif lex[tmpi].type == 'TAB':
                                                tmp = lex[tmpi].value.count(' ')
                                                if tmp <= tmpIndent: break
                                                if tmp < tmpIndents[-1]:
                                                    tmpIndents.pop()
                                                if tmp == tmpIndents[-1]:
                                                    #print(lex[tmpi-3].value,lex[tmpi-2].value,lex[tmpi-1].value)
                                                    safe = False ; break
                                                #print(tmp,tmpIndents)
                                            #else:
                                                #print('\t',lex[tmpi].value)


                                    if safe:
                                        for i in tmpIterables:
                                            if isinstance(i[0], str) == False:
                                                safe = False ; break
                                        tmpIterVars = [x[0] for x in tmpIterables]
                                        for i in tmpIterables:
                                            del i[0]

                                        # it is unsafe if one of the iterables calls a itervar
                                        for iterVar in tmpIterVars:
                                            for iterable in tmpIterables:
                                                for t in iterable:
                                                    if t.type == 'ID' and t.value == iterVar:
                                                        safe = False ; break
                                                    elif t.type == 'BUILTINF' and t.value.replace('.','') == iterVar:
                                                        safe = False ; break

                                    if safe: # perform the optimization
                                        tmpf = 'ASoptProduct'
                                        if 'itertools.' in wasImported and 'product' in wasImported['itertools.']:
                                            tmpf = 'product'
                                        else:
                                            keepAtTop.append(makeToken(lex[tmpi],'from itertools import product as ASoptProduct','IMPORT'))
                                        for tmpi in range(token,tmpEndPoint):
                                            lex[tmpi].type = 'IGNORE'
                                        tmptok=lex[tmpi]
                                        lex.insert(token,makeToken(tmptok,'for','FOR'))
                                        lex.insert(token+1,makeToken(tmptok,', '.join(tmpIterVars),'COMMAGRP'))
                                        lex.insert(token+2, makeToken(tmptok, 'in', 'INS'))
                                        lex.insert(token+3, makeToken(tmptok, tmpf+'(', 'FUNCTION'))
                                        tt=token+4
                                        for iterable in tmpIterables:
                                            for t in iterable:
                                                lex.insert(tt, t)
                                                tt += 1
                                            lex.insert(tt, makeToken(tmptok, ',', 'COMMA'))
                                            tt += 1
                                        lex[tt-1].type = 'RPAREN' ; lex[tt-1].value = ')'
                                        lex.insert(tt, makeToken(tmptok, 'do', 'THEN'))
                                        newOptimization=True
                                        if debug: print(f'! nested loop with vars {tmpIterVars} replaced with itertools product')
                                        #[print(l.value,end=' ') for l in lex if l.type != 'IGNORE'] ; exit()

                    elif lex[token].type in {'IF','ELIF'} and lex[token-1]:
                        if optDeadConditionalElimination and lex[token+1].type == 'BOOL':
                            tmpBaseIndent = None
                            for t in range(token,-1,-1):
                                if lex[t].type == 'NEWLINE':
                                    tmpBaseIndent = 0 ; break
                                elif lex[t].type == 'TAB':
                                    tmpBaseIndent = lex[t].value.replace('\t',' ').count(' ')
                                    break

                            if tmpBaseIndent != None:
                                tmpDelete=False if lex[token+1].value.strip() == 'True' else True
                                if tmpDelete: # if False
                                    safe = True
                                    for t in range(token, len(lex)//2): # delete IGNORES, someones gotta do it
                                        # yes there was a case this was needed, "Multiline conditionals" in megaTest
                                        if lex[t].type == 'IGNORE':
                                            del lex[t]
                                        elif lex[t].type == 'NEWLINE':
                                            break
                                    tmpNoTabs=True ; tmpTenary=False
                                    for t in range(token, len(lex)): # check if safe
                                        if lex[t].type == 'TAB':
                                            tmpNoTabs=False
                                            if lex[t].value.replace('\t', ' ').count(' ') <= tmpBaseIndent:
                                                if lex[t+1].type == 'OR' and lex[t+2].type == 'BOOL' and lex[t+2].value.strip() == 'True':
                                                    safe = False
                                                break
                                        elif lex[t].type == 'OR' and not (lex[t+1].type == 'BOOL' and lex[t+1].value.strip() == 'False'):
                                            for tt in range(token+1, t+1):
                                                lex[tt].type = 'IGNORE'
                                            safe = False ; break
                                        elif lex[t].type in {'NEWLINE','THEN','ENDIF'}: break
                                        elif lex[t].type == 'ELSE' and lex[t - 1].type not in typeNewline and lex[t + 1].type not in typeNewline:
                                            tmpTenary = True
                                    if safe:

                                        if tmpTenary: # tenary
                                            # find starting point of tenary
                                            tmpParenScope=0 ; tmpStartingPoint=0
                                            for t in range(token,0,-1):
                                                if lex[t].type in typeNewline: break
                                                elif lex[t].type == 'LPAREN': tmpParenScope += 1
                                                elif lex[t].type == 'RPAREN': tmpParenScope -= 1
                                                if tmpParenScope > 0: tmpStartingPoint=t+1 ; break
                                            # delete!
                                            for t in range(tmpStartingPoint,len(lex)-1):
                                                if lex[t].type == 'ELSE': lex[t].type = 'IGNORE' ; break
                                                lex[t].type = 'IGNORE'
                                            del tmpParenScope ; tmpStartingPoint

                                        else: # if block
                                            tmpLastT = 0 ; tmpOutOfConditional = False
                                            lex[token-1].type = 'IGNORE'
                                            for t in range(token,len(lex)-1):
                                                tmpLastT = t
                                                if not tmpOutOfConditional:
                                                    if lex[t].type in typeNewline+('ENDIF',) and t+1 < len(lex) and lex[t+1].type not in {'AND','OR'}: tmpOutOfConditional=True
                                                    elif lex[t].type in {'IF','ELSE'} and lex[t-1].type in typeNewline: tmpOutOfConditional=True
                                                    elif lex[t].type == 'ELIF': tmpOutOfConditional=True
                                                elif tmpOutOfConditional:
                                                    if lex[t].type == 'NEWLINE': break
                                                    elif tmpOutOfConditional and lex[t].type == 'TAB' and lex[t].value.replace('\t',' ').count(' ') <= tmpBaseIndent: break
                                                    elif lex[t].type == 'TAB' and lex[t+1].type == 'ELSE': break
                                                    elif tmpNoTabs and ((lex[t].type == 'THEN' and  lex[t+1].type == 'ELSE') or (lex[t].type == 'ELSE')):
                                                        lex[t].type = 'THEN' ; lex[t].value = ';' ; break
                                                    elif lex[t].type == 'END' and lex[t+1].type == 'NEWLINE': break
                                                lex[t].type = 'IGNORE'
                                            if tmpOutOfConditional and tmpLastT+1 < len(lex) and lex[tmpLastT+1].type in {'ELIF','ELSE'}:
                                                if lex[tmpLastT+1].type == 'ELSE':
                                                    lex.insert(tmpLastT+2, makeToken(lex[token], 'True', 'BOOL'))
                                                    if   lex[tmpLastT+3].type in {'COLON','ENDIF'}: lex[tmpLastT+3].type='IGNORE'
                                                    elif lex[tmpLastT+3].type not in typeNewline:
                                                        lex.insert(tmpLastT+3, makeToken(lex[token], ';', 'THEN'))
                                                lex[tmpLastT+1].type = 'IF'
                                                lex[tmpLastT+1].value = 'if'
                                            else:
                                                lex.insert(tmpLastT,  makeToken(lex[token],';'   ,'THEN'))
                                                lex.insert(tmpLastT+1,makeToken(lex[token],'pass','NOTHING'))
                                            del tmpLastT, tmpOutOfConditional
                                        del tmpNoTabs, tmpTenary
                                        newOptimization = True
                                        if debug: print("! removed: if False")
                                else: # if True
                                    pass # yet to be implemented


                            del tmpBaseIndent

                    elif lex[token].type == 'STRING':
                        if optStrFormatToFString and '%s' in lex[token].value and lex[token].value[0] not in 'fF' and all(True if i not in lex[token].value else False for i in {'%i','%d','%g','%G','%c','%r','%-','%x','%u','%o','%X','%E','%e','%f','%F','%+', '%0','%1','%2','%3','%4','%5','%6','%7','%8','%9'} ):
                            #print('~~~')
                            tmpModuloCheck = tmpParenUsed = False
                            tmpf=[]
                            tmpRparen=0 # paren after the string
                            preRparen=0 # paren before the string
                            for t in range(token+1,0,-1):
                                # looking back to set tmpRparen
                                if lex[t].type in typeNewline+('ENDIF',): break
                                elif lex[t].type == 'LPAREN': preRparen+=1 ; tmpRparen+=1
                                elif lex[t].type == 'FUNCTION' and lex[t].value[-1] == '(':
                                    preRparen += 1 ; tmpRparen += 1

                            t = token+1
                            tmpSkip = 0
                            #print('-----')
                            while t < len(lex)-1:
                                #print(tmpModuloCheck,tmpParenUsed,lex[t].type,lex[t].value,preRparen,tmpRparen,len(tmpf))
                                if lex[t].type in typeNewline:
                                    break
                                elif tmpSkip > 0:
                                    tmpSkip-=1
                                elif lex[t].type == 'MODULO':
                                    tmpModuloCheck=True ; lex[t].type='IGNOREtmp'+lex[t].type
                                elif lex[t].type == 'LPAREN' and tmpModuloCheck:
                                    if tmpParenUsed:
                                        tmpf.append([copy(lex[t])])
                                    lex[t].type='IGNOREtmp'+lex[t].type ; tmpRparen+=1 ; tmpParenUsed=True
                                elif lex[t].type == 'RPAREN' and tmpModuloCheck:
                                    tmpRparen-=1
                                    if preRparen <= 0:
                                        lex[t].type = 'IGNOREtmp' + lex[t].type
                                    preRparen -= 1
                                    if tmpRparen == 0: break
                                elif lex[t].type == 'COMMA' and (tmpRparen <= 0 or not tmpParenUsed): break
                                elif lex[t].type not in typeAssignables+('COMMA','FUNCTION','LINDEX','RINDEX','BUILTINF')+typeOperators+typeCheckers: break
                                else:
                                    if tmpModuloCheck:
                                        if lex[t-1].type not in ('MODULO','IGNOREtmpMODULO'):
                                            tt=-1
                                        else: tt=0
                                        tmp=[] ; tmpIgnoreFirstParen = True
                                        if tmpf and len(tmpf)==1 and tmpf[0][0].type == 'LPAREN':
                                            tmp = tmpf[0][:]
                                            tmpf=[]
                                            tmpRparen -= len([i for i in tmpf if i.type == 'LPAREN'])
                                        #print(' '.join([ii.value for ii in lex]))
                                        while True:
                                            #print('-',lex[t + tt].type,preRparen,tmpRparen)
                                            if lex[t+tt].type not in ('COMMA',)+typeNewline:
                                                if tmpModuloCheck and lex[t + tt].type in {'LPAREN','RPAREN','FUNCTION'}:
                                                    if lex[t+tt].type in {'LPAREN','FUNCTION'}:
                                                        tmpRparen += 1
                                                    elif lex[t+tt].type == 'RPAREN' and tmpModuloCheck:
                                                        tmpRparen -= 1
                                                    if tmpRparen == 0 or tmpRparen == preRparen:
                                                        # not sure if tmpRparen == preRparen is the correct solution but it seems to work
                                                        if tmpRparen == preRparen:
                                                            lex[t + tt].type = 'IGNOREtmp' + lex[t + tt].type
                                                        break
                                                if tmpModuloCheck and tmpIgnoreFirstParen and lex[t+tt].type == 'IGNOREtmpLPAREN':
                                                    tmpIgnoreFirstParen = False
                                                else:
                                                    tmptok=copy(lex[t+tt])
                                                    lex[t+tt].type='IGNOREtmp' + lex[t + tt].type
                                                    tmp.append(tmptok)
                                                tt+=1
                                            elif lex[t+tt].type in 'COMMA':
                                                if not tmpParenUsed:
                                                    t=len(lex)+4 ; tmpModuloCheck=False
                                                if tmpRparen == 0 or tmpRparen == preRparen:
                                                    break
                                                lex[t + tt].type = 'IGNORE'
                                                tmpf.append(tmp)
                                                tmp = [] ; tt+=1
                                            else: break
                                        tmpf.append(tmp)
                                        tmpSkip+=tt

                                t+=1

                            for i in tmpf:
                                for ii in i:
                                    if ii.type == 'STRING' and '\\' in ii.value:
                                        # cancel optimization if there is backslash in a string
                                        tmpf=[] ; break

                            if tmpf:
                                lex[token].value='f'+lex[token].value
                                for t in range(token,len(lex)-1):
                                    if lex[t].type.startswith('IGNOREtmp'): lex[t].type='IGNORE'
                            else:
                                for t in range(token,len(lex)-1): # restoring types if we cant do the optimization
                                    if lex[t].type.startswith('IGNOREtmp'): lex[t].type=lex[t].type.split('tmp')[-1]

                            #if debug and tmpf: print('optStrFormatToFString',[[f.type for f in toks] for toks in tmpf])
                            safe = False
                            while tmpf:
                                if len(tmpf[0]) == 1 and tmpf[0][0].type == 'STRING' and tmpf[0][0].value[0] not in 'fFrR' and '{' not in tmpf[0][0].value[0]:
                                    tmp=tmpf[0][0].value
                                    if tmp.startswith('"""') or tmp.startswith("'''"):
                                        tmp=tmp[3:-3]
                                    else:
                                        tmp=tmp[1:-1]
                                    lex[token].value=lex[token].value.replace('%s',tmp,1)
                                elif len(tmpf[0]) == 1 and tmpf[0][0].type == 'NUMBER':
                                    if debug: tmp = lex[token].value
                                    lex[token].value=lex[token].value.replace('%s',tmpf[0][0].value,1)
                                    if '{' not in lex[token].value and '}' not in lex[token].value: lex[token].value=lex[token].value[1:]
                                    if debug: print('! converted to fstring:', tmp, '-->', lex[token].value)
                                    newOptimization = True
                                else:
                                    if debug: tmp = lex[token].value
                                    lex[token].value=lex[token].value.replace('%s','{%s}'%''.join([l.value+' ' for l in tmpf[0]]),1)
                                    if debug: print('! converted to fstring:',tmp,'-->',lex[token].value)
                                    newOptimization=True ; safe = True
                                tmpf.pop(0)
                            if tmpf and '{' not in lex[token].value and '}' not in lex[token].value:
                                lex[token].value=lex[token].value[1:]
                            if safe:
                                lex[token] = copy(createFString(lex[token],token))

                        if optCompilerEval and optCompilerEvalDict['evalStrInStr'] and lex[token + 1].type == 'INS' and lex[token + 1].value.strip() == 'in' and lex[token + 2].type == 'STRING':
                            lex[token].type = lex[token+1].type = lex[token+2].type = 'IGNORE'
                            if stripStringQuotes(lex[token].value) in stripStringQuotes(lex[token+2].value):
                                lex.insert(token, makeToken(lex[token], 'True', 'BOOL'))
                            else:
                                lex.insert(token, makeToken(lex[token], 'False', 'BOOL'))

                    elif lex[token].type == 'DEFFUNCT':
                        definedFuncs.add(lex[token-1].value)
                    elif lex[token].type == 'PYDEF':
                        definedFuncs.add(lex[token].value.split('(')[0][3:].strip())
                    elif optLoopAttr and preAllocated and lex[token].type in {'TAB','NEWLINE'}:
                        if lex[token].type == 'NEWLINE' and token+1 < len(lex)-1 and lex[token+1].type!='TAB':
                            for p in tuple(_ for _ in preAllocated):
                                if p[0] > 0:
                                    preAllocated.remove(p)
                        else:
                            preAllocated={_ for _ in preAllocated if _[0] <= lex[token].value.count(' ')}


                    elif optIfTrue and lex[token].type in {'EQUAL','NOTEQ'} and lex[token + 1].type == 'BOOL' and lex[token + 1].value != 'None':
                        if (lex[token+1].value == 'False' and lex[token].type == 'EQUAL') or (lex[token+1].value == 'True' and lex[token].type == 'NOTEQ'):
                            tmp = findEndOfExpression(token-1)
                            if lex[tmp].type != 'INS':
                                lex[token].type = lex[token + 1].type = 'IGNORE'
                                lex.insert(tmp,makeToken(lex[token],'not ','INS'))
                        else: # True
                            lex[token].type = lex[token + 1].type = 'IGNORE'

                    elif optConvertMultipleOrToIn and lex[token].type == 'OR' and ((lex[token-1].type in {'STRING','NUMBER'} and lex[token-2].type in {'EQUAL','ASSIGN'} and determineIfAssignOrEqual(token-2) and (lex[token-3].type in {'ID','INC'} or (lex[token-3].type == 'RINDEX' and getIndexVar(token-3))) \
                    and ((lex[token+1].type in {'ID','INC'} and lex[token+2].type in {'EQUAL','ASSIGN'} and determineIfAssignOrEqual(token+2) and lex[token+3].type in {'STRING','NUMBER'} and lex[token+1].value == lex[token-3].value)\
                    or   (lex[token+1].type == 'ID' and lex[token+2].type == 'LINDEX' and getIndexVar(token+2,False) and lex[getIndexVar(token+2,False)[2]+1].type in {'EQUAL','ASSIGN'} and determineIfAssignOrEqual(getIndexVar(token+2,False)[2]+1) and lex[getIndexVar(token+2,False)[2]+2].type in {'STRING','NUMBER'}) and (lex[token+1].value.strip().replace('-','').replace('+','') == lex[token-3].value.strip().replace('-','').replace('+','') or (getIndexVar(token-3) and getIndexVar(token-3)[0].value == lex[token+1].value))))
                        or (lex[token-1].type == 'ID' and lex[token-2].type in {'EQUAL','ASSIGN'} and lex[token-3].type in {'STRING','NUMBER'} and lex[token+1].type in {'STRING','NUMBER'} and lex[token+2].type in {'EQUAL','ASSIGN'} and lex[token+3].type in {'ID','INC'} and lex[token+3].value.replace('-','').replace('+','') == lex[token-1].value)):
                        # ID1 EQUAL [STRING|NUMBER] OR ID1 EQUAL [STRING|NUMBER]
                        # the index pattern matching is kinda cursed
                        # oh god and we gotta match reverse i hate this so much

                        # TODO: x == 'a' or x == 'b'  -->  x in 'ab'
                        # this is faster in PyPy and Pyston, but not CPython
                        # however, we need to know that x is str, otherwise typeError

                        tmpInsertINC = []
                        if lex[token-3].type == 'RINDEX':
                            tmpIndex = getIndexVar(token-3)
                            tmpStart = tmpIndex[2]
                        else: tmpStart=token-3 ; tmpIndex=False
                        if lex[tmpStart].type in {'ID','INC'}:
                            tmpTheVar = lex[tmpStart].value
                        else:
                            tmpTheVar = lex[token-1].value
                        if lex[tmpStart].type == 'INC': tmpTheVar = tmpTheVar.strip().replace('-', '').replace('+', '')
                        tmpHasOR=True ; tmpInIndex=False
                        tmpf=[] ; tmpi=token
                        if (pyIs or pyCompatibility) and any(True for _ in (token - 2, token + 2) if lex[_].type == 'ASSIGN' and lex[_].value.strip() == 'is'):
                            tmpStart=len(lex) # cancel optimization

                        for i in range(tmpStart,len(lex)-1):
                            #print(i,lex[i].type,tmpInIndex,tmpHasOR)
                            if lex[i].type == 'OR': tmpHasOR=True ; tmpi=i
                            elif tmpHasOR:
                                if tmpInIndex and any(True for _ in tmpIndex[1] if _.value == lex[i].value and _.type == lex[i].type):
                                    if lex[i] == tmpIndex[1][-1]: tmpInIndex=False
                                    pass
                                elif lex[i].type in {'STRING','NUMBER'}:
                                    tmpf.append(copy(lex[i]))
                                    if lex[tmpStart].type in {'ID','INC'}: tmpHasOR=False
                                elif lex[i].type == 'ID' and lex[i].value == tmpTheVar:
                                    if tmpIndex and tmpTheVar == tmpIndex[0].value: tmpInIndex=True
                                    pass
                                elif lex[i].type == 'INC' and lex[i].value.strip().replace('-', '').replace('+', '') == tmpTheVar:
                                    tmpInsertINC.append(copy(lex[i]))
                                elif lex[i].type in {'EQUAL','ASSIGN'} and determineIfAssignOrEqual(i) and lex[i-1].type in {'ID','RINDEX','INC'}: tmpInIndex=False
                                elif lex[i].type in {'EQUAL','ASSIGN'} and lex[i-1].type in {'STRING','NUMBER'}: pass
                                else: tmpi=i ; break
                            elif lex[i].type == 'AND':
                                if tmpf: del tmpf[-1]
                                break
                            else: tmpi=i ; break
                        #print('---',bool(tmpf), tmpStart, tmpi)
                        if tmpf:
                            for i in range(tmpStart, tmpi):
                                lex[i].type = 'IGNORE'
                            lex.insert(tmpStart,makeToken(lex[token],'}','RBRACKET'))
                            for t in tmpf:
                                lex.insert(tmpStart, t)
                                lex.insert(tmpStart, makeToken(lex[token], ',', 'COMMA'))
                            del lex[tmpStart]
                            lex.insert(tmpStart, makeToken(lex[token], '{', 'LBRACKET'))
                            lex.insert(tmpStart, makeToken(lex[token], 'in', 'INS'))
                            if tmpIndex:
                                for t in reversed(tmpIndex[1]):
                                    lex.insert(tmpStart, t)
                            lex.insert(tmpStart, makeToken(lex[token], tmpTheVar, 'ID'))
                            if tmpInsertINC:
                                tmpStartSpot=tmpStart ; tmpWaitForIF=False
                                for i in range(tmpStart,0,-1):
                                    if lex[i].type in {'IF','MATCH'} and lex[i-1].type in typeNewline: tmpStartSpot=i-1 ; break
                                    elif lex[i].type in {'ELIF','OF'}: tmpWaitForIF=True
                                    elif lex[i].type in typeNewline and not tmpWaitForIF: tmpStartSpot=i ; break
                                for t in tmpInsertINC:
                                    lex.insert(tmpStartSpot,makeToken(lex[token],';','THEN'))
                                    lex.insert(tmpStartSpot,t)
                                lex.insert(tmpStartSpot, makeToken(lex[token], ';', 'THEN'))
                            newOptimization=True
                            if debug:
                                print('! convert multi or to in: '+' or '.join([_.value for _ in tmpf]),'-->','in {'+','.join([_.value for _ in tmpf])+'}')

                    elif optCompilerEval and optCompilerEvalDict['evalTokens'] and ((lex[token].type in typeCheckers and lex[token].type != 'PYIS') or (lex[token].type == 'ASSIGN' and 'is' in lex[token].value and determineIfAssignOrEqual(token))) \
                    and ( (lex[token-1].type in {'STRING','NUMBER','BOOL'} and (lex[token+1].type in {'STRING','NUMBER','BOOL'} or isANegativeNumberTokens(token+1)) and (lex[token-2].type in typeConditionals+typeNewline+('AND','OR','LPAREN','ID') )) \
                    or (isANegativeNumberTokens(token-2) and (lex[token+1].type in {'STRING','NUMBER'} or isANegativeNumberTokens(token+1)) and lex[token-3].type in typeConditionals+typeNewline+('AND','OR','LPAREN') ) ):
                        # eval bool conditionals when STRING and/or NUMBER

                        if lex[token].type == 'ASSIGN' and not pyCompatibility: lex[token].type = 'EQUAL'
                        tmpEndResult = None
                        if lex[token+1].type == 'MINUS':
                              tmp2=2
                        else: tmp2=1
                        tmpType1 = lex[token-1].type ; tmpType2 = lex[token+tmp2].type
                        if tmpType1 == 'STRING':
                            tmpQuoteAmount = 2
                            if lex[token-1].value.startswith("'''") or lex[token-1].value.startswith('"""'):
                                tmpQuoteAmount = 6
                            tmpValue1 = len(lex[token-1].value)-tmpQuoteAmount
                        elif tmpType1 == 'NUMBER':
                            try: tmpValue1 = float(lex[token-1].value)
                            except ValueError:
                                if lex[token-1].value.isdigit():
                                    tmpValue1 = int(lex[token-1].value)
                            if lex[token-2].type == 'MINUS':
                                tmpValue1 = -tmpValue1
                        elif tmpType1 == 'BOOL':
                            if lex[token-1].value == 'True': tmpValue1 = 1
                            else: tmpValue1 = 0
                        if tmpType2 == 'STRING':
                            if lex[token+tmp2].value.startswith("'''") or lex[token+tmp2].value.startswith('"""'):
                                tmpQuoteAmount = 6
                            else: tmpQuoteAmount = 2
                            tmpValue2 = len(lex[token + tmp2].value)-tmpQuoteAmount
                        elif tmpType2 == 'NUMBER':
                            try:
                                tmpValue2 = float(lex[token + tmp2].value)
                            except ValueError:
                                if lex[token + 1].value.isdigit():
                                    tmpValue2 = int(lex[token + tmp2].value)
                            if tmp2 == 2:
                                tmpValue2 = -tmpValue2
                        elif tmpType2 == 'BOOL':
                            if lex[token + tmp2].value == 'True': tmpValue2 = 1
                            else: tmpValue2 = 0

                        # check if True/False
                        safe = False
                        if not pyCompatibility and ((tmpType1 == 'STRING' and tmpType2 in {'STRING', 'NUMBER'}) or (tmpType1 == 'NUMBER' and tmpType2 == 'STRING')):
                            safe = True
                        elif tmpType1 in {'BOOL','NUMBER'} and tmpType2 in {'BOOL','NUMBER'}:
                            safe = True


                        if lex[token].type == 'LESS':
                            if safe:
                                if tmpValue1 < tmpValue2:
                                      tmpEndResult=True
                                else: tmpEndResult=False
                        elif lex[token].type == 'LESSEQ':
                            if safe:
                                if tmpValue1 <= tmpValue2:
                                      tmpEndResult=True
                                else: tmpEndResult=False
                        elif lex[token].type == 'GREATEQ':
                            if safe:
                                if tmpValue1 >= tmpValue2:
                                      tmpEndResult=True
                                else: tmpEndResult=False
                        elif lex[token].type == 'GREATER':
                            if safe:
                                if tmpValue1 > tmpValue2:
                                      tmpEndResult=True
                                else: tmpEndResult=False
                        elif lex[token].type == 'EQUAL':
                            if tmpType1 == 'STRING' and tmpType2 == 'STRING':
                                if stripStringQuotes(lex[token-1].value) == stripStringQuotes(lex[token+tmp2].value):
                                      tmpEndResult=True
                                else: tmpEndResult=False
                            elif safe:
                                if tmpValue1 == tmpValue2:
                                      tmpEndResult=True
                                else: tmpEndResult=False
                        elif lex[token].type == 'NOTEQ':
                            if tmpType1 == 'STRING' and tmpType2 == 'STRING':
                                if stripStringQuotes(lex[token - 1].value) != stripStringQuotes(lex[token + tmp2].value):
                                    tmpEndResult = True
                                else:
                                    tmpEndResult = False
                            elif safe:
                                if tmpValue1 != tmpValue2:
                                    tmpEndResult = True
                                else:
                                    tmpEndResult = False


                        if tmpEndResult != None:
                            # replace tokens with end result
                            if debug: print(f'! compileTimeEval {lex[token-1].value} {lex[token].value} {lex[token+1].value} --> {tmpEndResult}')
                            lex[token-1].type = lex[token+1].type = 'IGNORE'
                            if tmp2 == 2:
                                lex[token+2].type = 'IGNORE'
                            if lex[token-2].type == 'MINUS':
                                lex[token-2].type = 'IGNORE'
                            lex[token].type = 'BOOL' ; lex[token].value = str(tmpEndResult)
                            newOptimization = True

                    if optUnModAssignment and (lex[token].type == 'ID' and lex[token+1].type == 'ASSIGN' and any(True for i in {'+', '-', '/', '*'} if i in lex[token+1].value) or (lex[token].type == 'INC' and lex[token+1].type in typeNewline and lex[token-1].type in typeNewline)):
                        # x = 2 ; x += 2 --> x = 2 + 2
                        # undoing mod assignments is an optimization
                        tmpIndent = None ; tmpFirstIndent = False ; safe = False ; tmpInsertAtEnd = 0
                        if lex[token].type == 'ID':
                          tmpSign = lex[token+1].value[0]
                          if lex[token+1].value[1] == '/': tmpSign+='/'
                          if   tmpSign == '+' : tmp = 'PLUS'
                          elif tmpSign == '-' : tmp = 'MINUS'
                          elif tmpSign == '/' : tmp = 'DIVIDE'
                          elif tmpSign == '*' : tmp = 'TIMES'
                          elif tmpSign == '//': tmp = 'RDIVIDE'
                          tmpIDValue = lex[token].value
                          tmpf = [makeToken(lex[token], tmpSign, tmp)]  # stores line
                        elif lex[token].type == 'INC':
                          if   lex[token].value[0]  == '+': tmp = 'PLUS' ; tmpSign=lex[token].value[0]
                          elif lex[token].value[0]  == '-': tmp = 'MINUS'; tmpSign=lex[token].value[0]
                          elif lex[token].value[-1] == '+': tmp = 'PLUS' ; tmpSign=lex[token].value[-1]
                          elif lex[token].value[-1] == '-': tmp = 'MINUS'; tmpSign=lex[token].value[-1]
                          else: continue
                          tmpIDValue = lex[token].value.replace('++','').replace('--','')
                          tmpf = [makeToken(lex[token], tmpSign, tmp),makeToken(lex[token], '1', 'NUMBER')]  # stores line
                        else: continue

                        if lex[token].type != 'INC':
                            for tmpi in range(token+2,len(lex)-1):
                              if lex[tmpi].type in typeNewline+typeConditionals:
                                  break
                              tmpf.append(copy(lex[tmpi]))
                        tmpListOfIDs=tuple([_.value for _ in tmpf if _.type == 'ID'])
                        # search for previous line
                        for tmpi in range(token-1,0,-1):
                          if lex[tmpi].type in typeNewline:
                              if lex[tmpi].type == 'TAB':
                                  if tmpFirstIndent and lex[tmpi].value.count(' ') != tmpIndent: break
                                  tmpIndent=lex[tmpi].value.count(' ') ; tmpFirstIndent=True
                                  if not tmpInsertAtEnd: tmpInsertAtEnd=tmpi
                              elif lex[tmpi].type == 'NEWLINE':
                                  if tmpFirstIndent and tmpIndent > 0: safe=False ; break
                                  tmpIndent=0 ; tmpFirstIndent=True
                                  if not tmpInsertAtEnd: tmpInsertAtEnd=tmpi
                              elif lex[tmpi].type == 'THEN':
                                  tmpInsertAtEnd = tmpi
                          elif lex[tmpi].type in typeConditionals+('LOOP','FOR') \
                          or (lex[tmpi].type == 'ID' and lex[tmpi].value == tmpIDValue) or (lex[tmpi].type=='ID' and lex[tmpi].value in tmpListOfIDs):
                              safe = False ; break
                          elif lex[tmpi].type == 'ASSIGN' and lex[tmpi-1].type == 'ID' and lex[tmpi-1].value == tmpIDValue \
                          and lex[tmpi+1].value != tmpIDValue and lex[tmpi+1].type != 'INDEX' and lex[tmpi-2].type in typeNewline:
                              if lex[tmpi-2].type == 'NEWLINE' or (lex[tmpi-2].type == 'TAB' and lex[tmpi-2].value.count(' ') == tmpIndent) or (lex[token-1].type == 'THEN' and not tmpFirstIndent):
                                  # success, insert at end
                                  for t in reversed(tmpf):
                                      lex.insert(tmpInsertAtEnd, t) ; token+=1
                                  if debug: print('! undoing mod assignment',' '.join([lex[t].value for t in range(tmpi-1,tmpInsertAtEnd+len(tmpf))]))
                                  newOptimization = True
                                  safe = True ; break

                        if safe:
                          # if success, delete line
                          for tmpi in range(token, len(lex) - 1):
                              if lex[tmpi].type in typeNewline:
                                  break
                              else: lex[tmpi].type = 'IGNORE'


                    if token > len(lex)-1: break
                    if lex[token].type == 'IGNORE':
                        del lex[token] ; token-=2

                    if optCompilerEval and optCompilerEvalDict['evalTokens'] and lex[token].type in {'STRING','NUMBER'}: # jumpy
                        # math or string evaluation
                        bitwiseOrderOps = {'~': 5, '<<': 4, '>>': 4, '&': 3, '^': 2, '|': 1}
                        tmpTypeSafe = typeNewline + ('RPAREN', 'ASSIGN', 'FUNCTION', 'LPAREN', 'ELSE', 'DEFEXP', 'LINDEX')
                        check=False
                        # these blocks help follow the order of operations for more accuracy
                        if                                        (token+3 < len(lex)-1 and lex[token + 3].type == 'RPAREN' and lex[token - 3].type in {'LPAREN','FUNCTION'} and lex[token - 2].type == lex[token + 2].type == 'NUMBER' and lex[token - 1].type in orderOfOps and  lex[token + 1].type in orderOfOps and orderOfOps[lex[token - 1].type] == orderOfOps[lex[token + 1].type]) \
                        or (isANegativeNumberTokens(token - 1) and token+4 < len(lex)-1 and lex[token + 3].type == 'RPAREN' and lex[token - 4].type in {'LPAREN','FUNCTION'} and lex[token - 3].type == lex[token + 2].type == 'NUMBER' and lex[token - 2].type in orderOfOps and  lex[token + 1].type in orderOfOps and orderOfOps[lex[token - 2].type] == orderOfOps[lex[token + 1].type]):
                            # if order of ops is equal, preference goes towards left
                            #print('#0',lex[token].value)
                            if isANegativeNumberTokens(token - 1) and lex[token-4].type in {'LPAREN',}:
                                lex.insert(token + 1, makeToken(lex[token], ')', 'RPAREN'))
                                lex.insert(token - 4, makeToken(lex[token], '(', 'LPAREN'))
                            else:
                                lex.insert(token + 1, makeToken(lex[token], ')', 'RPAREN'))
                                lex.insert(token - 3, makeToken(lex[token], '(', 'LPAREN'))
                            newOptimization=True # needs another iteration
                        elif token+3 < len(lex)-1 and ((lex[token+3].type == 'PIPE' and lex[token+3].value == 'to') or lex[token+1].type in typeNewline or lex[token+2].type in typeNewline): pass
                        elif lex[token-1].type in orderOfOps and lex[token+1].type in orderOfOps and not (lex[token-1].type == 'MINUS' and lex[token-2].type in tuple(orderOfOps)+tmpTypeSafe):
                            #print('#-1', lex[token].value)
                            if (lex[token-1].type in {'LPAREN','FUNCTION'} or (orderOfOps[lex[token-1].type] < orderOfOps[lex[token+1].type]) or (lex[token-1].type=='MINUS' and lex[token-2].type in orderOfOps and (lex[token-2].type=='LPAREN' or orderOfOps[lex[token-2].type] < orderOfOps[lex[token+1].type])) ) \
                            and token+4 < len(lex)-1 and ((lex[token+3].type in typeNewline+('RPAREN',) or (isANegativeNumberTokens(token+2) and lex[token+4].type in typeNewline+('RPAREN',))) or not ((lex[token+3].type in orderOfOps and (orderOfOps[lex[token+3].type] >= orderOfOps[lex[token+1].type])) or (isANegativeNumberTokens(token+2) and lex[token+4].type in orderOfOps and orderOfOps[lex[token+4].type] >= orderOfOps[lex[token+1].type]))) \
                            and not (token+3 < len(lex) and lex[token+1].type == 'EXPONENT' and lex[token+3].type == 'EXPONENT') and not (token+4 < len(lex) and lex[token+1].type == 'EXPONENT' and isANegativeNumberTokens(token+2) and lex[token+4].type == 'EXPONENT'):
                                # 'normal'
                                #print('#1', lex[token].value)
                                tmppre=-1
                                if lex[token-1].type == 'MINUS' and lex[token-2].type in orderOfOps and lex[token-2].type != 'LPAREN':
                                    tmppre = -2
                                if lex[tmppre].type in orderOfOps:
                                    if orderOfOps[lex[tmppre].type]==orderOfOps[lex[token+1].type]:
                                        if lex[tmppre].type == 'MINUS' and lex[token+1] == 'PLUS':
                                            check = True
                                        elif lex[tmppre].type == 'DIVIDE' and lex[token+1] == 'TIMES':
                                            check = True
                                        elif lex[tmppre].type == 'MODULO' and lex[token+1] == 'DIVIDE':
                                            check = True
                                        elif lex[tmppre].type == 'BITWISE' and lex[token+1].type == 'BITWISE':
                                            if bitwiseOrderOps[lex[token + 1].value] < bitwiseOrderOps[lex[tmppre].value]:
                                                check = True
                                        #print('#1.1', lex[token].value)
                                    elif (orderOfOps[lex[token-1].type] < orderOfOps[lex[token+1].type]) or (lex[token-1].type=='MINUS' and lex[token-2].type in orderOfOps and (lex[token-2].type=='LPAREN' or orderOfOps[lex[token-2].type] < orderOfOps[lex[token+1].type])):
                                        if isANegativeNumberTokens(token+2) and lex[token+4].type in orderOfOps and orderOfOps[lex[token+1].type] >= orderOfOps[lex[token+4].type]:
                                            check = True
                                        elif lex[token+3].type in orderOfOps and orderOfOps[lex[token+1].type] >= orderOfOps[lex[token+3].type]:
                                            check = True
                                        else:
                                            check = True
                                        #print('#1.2', lex[token].value)

                                else:
                                    check=True
                                    #print('#1.3',lex[token].value)
                            elif (lex[token].type == 'STRING' and ((lex[token+1].type == 'TIMES' and lex[token+2].type == 'NUMBER') or (lex[token+1].type == 'PLUS' and lex[token+2].type == 'STRING' and lex[token-1].type != 'TIMES'))) \
                            or ((lex[token-1].type in {'LPAREN','FUNCTION'} or (lex[token-1].type in orderOfOps and orderOfOps[lex[token-1].type] <= 2)) and lex[token].type == 'NUMBER' and lex[token+1].type == 'TIMES' and lex[token+2].type == 'STRING'):
                                # simple string eval
                                #print("1.4", lex[token].value)
                                check=True
                        elif (lex[token-1].type in tmpTypeSafe+typeCheckers or (lex[token-1].type=='MINUS' and lex[token-2].type in tmpTypeSafe)) and lex[token+1].type in orderOfOps:
                            # when last token isnt operator, checks ahead and also handles bitwise
                            #print('#2', lex[token].value)
                            if isANegativeNumberTokens(token+2) and lex[token+4].type in orderOfOps:
                                tmp=token+4
                            elif lex[token+2].type == 'NUMBER' and lex[token+3].type in orderOfOps:
                                tmp=token+3
                            else:
                                tmp=0
                            if tmp:
                                if lex[tmp].type == 'EXPONENT' and lex[token+1].type == 'EXPONENT':
                                    pass
                                elif lex[tmp].type == 'BITWISE' and lex[token+1].type == 'BITWISE':
                                    if bitwiseOrderOps[lex[token+1].value] > bitwiseOrderOps[lex[tmp].value]:
                                        check = True
                                        #print('#2.1', lex[token].value)
                                elif lex[tmp].type in typeNewline+('LPAREN',) or orderOfOps[lex[token+1].type] > orderOfOps[lex[tmp].type]:
                                    check=True
                                    if lex[tmp+2].type in orderOfOps:
                                        # check ahead to make sure not breaking order of operations way down the line
                                        for ttmp in range(tmp,len(lex)-1):
                                            if lex[ttmp].type in typeNewline+('LPAREN',) \
                                            or lex[ttmp].type in orderOfOps and lex[ttmp-2].type in orderOfOps and orderOfOps[lex[ttmp-2].type] <= orderOfOps[lex[ttmp].type]:
                                                check = False
                                    #print('#2.2', lex[token].value)
                                elif lex[tmp].type == 'RPAREN' and lex[token-1].type in {'FUNCTION','LPAREN'}:
                                    # the case of ( 2 + 2 ) or print( 2 + 2 )
                                    check = True
                                    #print('#2.5', lex[token].value)
                            else:
                                # simple three token eval
                                check = True
                                #print('#2.4',lex[token].value)
                        if token-2 > 0 and lex[token-1].type == 'MINUS' and lex[token-2].type in typeOperators and lex[token-2].type in orderOfOps and lex[token+1].type in orderOfOps and orderOfOps[lex[token+1].type] < orderOfOps[lex[token-1].type]:
                            check = False
                        if check and lex[token-1].type == 'BITWISE' and lex[token+1].type == 'BITWISE':
                            if bitwiseOrderOps[lex[token-1].value] > bitwiseOrderOps[lex[token+1].value]:
                                check=False

                        if check:
                            if lex[token].type == 'NUMBER' and lex[token+1].type in typeOperators and (lex[token+2].type == 'NUMBER' or (lex[token+2].type == 'LPAREN' and lex[token+3].type == 'NUMBER') or isANegativeNumberTokens(token+2)):
                                tmpf=[] ; tmpscope=0 ; tmpLastOperator=fail=False
                                for tmpi in range(token,len(lex)-1):
                                    if lex[tmpi].type in typeOperators+('NUMBER','LPAREN','RPAREN'):
                                        if   lex[tmpi].type == 'LPAREN': tmpscope-=1 ; tmpLastOperator=False
                                        elif lex[tmpi].type == 'RPAREN': tmpscope+=1 ; tmpLastOperator=False
                                        elif lex[tmpi].type in typeOperators:
                                            if tmpLastOperator and lex[tmpi].type == 'MINUS' and lex[tmpi+1].type == 'NUMBER':
                                                tmpf.append(lex[tmpi]) ; continue
                                            elif tmpLastOperator and orderOfOps[lex[tmpi].type] >= orderOfOps[tmpLastOperator]:
                                                break
                                            else:
                                                tmpLastOperator = lex[tmpi].type
                                        tmpf.append(lex[tmpi])
                                    else:
                                        if tmpf[-1].type in typeOperators: del tmpf[-1]
                                        break
                                while tmpscope > 0:
                                    if not tmpf: break
                                    elif tmpf[-1].type == 'RPAREN': tmpscope-=1
                                    tmpf.pop()
                                if tmpscope != 0: fail=True
                                #print(tmpscope, '--',[l.value for l in tmpf])

                                if len(tmpf) <= 1 : fail = True
                                if not fail:
                                    tmpLeftHasMinus=False
                                    if lex[token-1].type == 'MINUS' and (lex[token-2].type in tmpTypeSafe and lex[token-2].type != 'RPAREN'):
                                        tmpf.insert(0,lex[token - 1])
                                        lentmpf = len(tmpf)-1
                                        tmpLeftHasMinus = True
                                    else:
                                        lentmpf = len(tmpf)
                                    try:
                                        tmp=compilerNumberEval(tmpf)
                                        if debug: print(f"! compile-time-eval: {' '.join([lex[die].value for die in range(token, token + len(tmpf))])} --> {tmp}")
                                        lex[token].value=tmp
                                        for die in range(token+1,token+lentmpf):
                                            #print(lex[die].type)
                                            lex[die].type='IGNORE'
                                        if lex[token-1].type == 'MINUS' and lex[token-2].type == 'LPAREN' and lex[token+1].type == 'RPAREN':
                                            lex[token-2].type = lex[token+1].type = 'IGNORE'
                                        if tmpLeftHasMinus: lex[token-1].type = 'IGNORE'
                                        newOptimization=True
                                    except (TypeError, ZeroDivisionError, SyntaxError): pass # SyntaxError means the expression sent to compilerNumberEval made no sense
                            while lex[token].type == 'STRING' and lex[token+1].type == 'PLUS' and lex[token+2].type == 'STRING' \
                            and lex[token].value.startswith("'''")==False and lex[token].value.startswith('"""')==False and lex[token+2].value.startswith("'''")==False and lex[token+2].value.startswith('"""')==False:
                                if lex[token+3].type in typeOperators and orderOfOps[lex[token+3].type] > orderOfOps[lex[token+1].type]: break
                                quotes=None
                                for c in range(len(lex[token].value)):
                                    if lex[token].value[c] in ('"',"'"):
                                        quotes=lex[token].value[c] ; break
                                for c in range(len(lex[token+2].value)):
                                    if lex[token+2].value[c] in ('"',"'"):
                                        lex[token].value=lex[token].value[:-1]+lex[token+2].value[c+1:]
                                        lex[token].value=lex[token].value[:-1]+quotes
                                        break
                                if debug: print(f'! string merging into {lex[token].value}')
                                newOptimization=True
                                lex[token+1].type=lex[token+2].type='IGNORE'
                            if lex[token+1].type == 'TIMES' and ((lex[token].type == 'STRING' and lex[token+2].type == 'NUMBER') or (lex[token].type == 'NUMBER' and lex[token+2].type == 'STRING')):
                                tmpSafe = True
                                if lex[token].type == 'STRING':
                                      tmpS = token   ; tmpN = token+2
                                else: tmpS = token+2 ; tmpN = token
                                # tmpS = string token index , tmpN = number token index
                                if lex[tmpN].value == '0' or '.' in lex[tmpN].value: tmpSafe=False
                                elif lex[tmpS].value.startswith("'''") or lex[tmpS].value.startswith('"""'): tmpSafe=False

                                if tmpSafe:
                                    quotes=None
                                    for c in range(len(lex[token].value)):
                                        if lex[tmpS].value[c] in ('"',"'"):
                                            quotes=lex[tmpS].value[c] ; break
                                    for c in range(len(lex[token].value)):
                                        if lex[tmpS].value[c] == quotes:
                                            lex[tmpS].value=quotes+(lex[tmpS].value[c+1:-1]*int(lex[tmpN].value))+quotes
                                            break
                                    lex[token+1].type=lex[tmpN].type='IGNORE'
                                    newOptimization=True


                    if optCompilerEval and optCompilerEvalDict['evalTokens'] and not pyCompatibility and lex[token].type in {'STRING','STRRAW'} and lex[token+1].type in {'STRING','STRRAW'} and lex[token+2].type not in typeOperators and lex[token-1].type not in typeOperators:
                        # evaluate string comparisons when they have no compare, inside conditional
                        # if 'string' 'string'  -->  if 'string' == 'string'  -->  if True
                        safe = False
                        for ii in range(token,0,-1):
                            if lex[ii].type in typeNewline: break
                            elif lex[ii].type in typeConditionals: safe=True ; break
                        if safe:
                            if debug: print(f'! compileTimeEval {lex[token].value} == {lex[token+1].value} --> {stripStringQuotes(lex[token].value) == stripStringQuotes(lex[token+1].value)}')
                            if stripStringQuotes(lex[token].value) == stripStringQuotes(lex[token+1].value):
                                lex[token].value = 'True'
                            else:
                                lex[token].value = 'False'
                            lex[token].type = 'BOOL'
                            lex[token+1].type = 'IGNORE'
                            newOptimization = True
                        else:
                            # regular combine string. "12" '34' --> "1234"
                            tmpQuote=False
                            if   lex[token].value.startswith('"""'): tmpQuote='"""' ; safe=True
                            elif lex[token].value.startswith('"""'): tmpQuote="'''" ; safe=True
                            else:
                                for char in lex[token].value:
                                    if char in {'"',"'"}: tmpQuote = char ; break
                                if   '\n' in lex[token + 1].value and len(tmpQuote) == 1: tmpQuote = tmpQuote * 3 ; safe = True
                                elif '\n' in lex[token    ].value and len(tmpQuote) == 1: tmpQuote = tmpQuote * 3 ; safe = True
                                else:
                                    if lex[token+1].value[-1] == tmpQuote: safe = True
                                    elif tmpQuote in lex[token+1].value:
                                        if '\\'+tmpQuote in lex[token+1].value:
                                            safe = True
                                            for cc in range(1,len(lex[token+1].value)-1):
                                                if lex[token+1].value[cc] == tmpQuote and lex[token+1].value[cc-1] != '\\': safe=False ; break
                                    else: safe=True
                                if tmpQuote in stripStringQuotes(lex[token + 1].value): safe = False
                            if lex[token+1].value[0].lower() == lex[token].value[0].lower(): safe = True
                            elif lex[token+1].value[0] not in {'"', "'"}: safe = False

                            if safe:
                                tmpFound = False ; tmpStart = ''
                                for char in lex[token].value:
                                    if char not in {'"', "'"}:
                                        if not tmpFound: tmpStart += char
                                        else: break
                                    elif char in {'"', "'"}: tmpFound = True

                                if debug: tmpOG = lex[token+0].value,lex[token+1].value
                                lex[token].value = tmpStart+tmpQuote+stripStringQuotes(lex[token].value)+stripStringQuotes(lex[token+1].value)+tmpQuote
                                lex[token+1].type = 'IGNORE'
                                newOptimization=True
                                if debug: print(f"! compileTimeEval  {tmpOG[0]} {tmpOG[1]} --> {lex[token].value}")
                                del tmpFound, tmpStart

                    if optCompilerEval \
                    and ((lex[token-1].type == 'LPAREN' and lex[token+1].type == 'RPAREN' and (lex[token-2].type in ('LPAREN','ASSIGN','DEFEXP')+typeNewline+typeOperators or (lex[token-2].type == 'FUNCTION' and lex[token-2].value[-1] == '(')) and lex[token].type != 'FUNCTION') \
                    or (lex[token-1].type == 'MINUS' and lex[token-2].type == 'LPAREN' and lex[token+1].type == 'RPAREN' and (lex[token-3].type in ('LPAREN','ASSIGN','DEFEXP')+typeNewline+typeOperators or (lex[token-3].type == 'FUNCTION' and lex[token-3].value[-1] == '(')) and lex[token].type != 'FUNCTION')):
                        # remove useless paren like:  (-12)
                        #print(lex[token-2].type,lex[token-1].type,lex[token].type,lex[token].value,lex[token+1].type,888888888888)
                        if lex[token-1].type == 'MINUS':
                            lex[token-2].type='IGNORE'
                        else:
                            lex[token-1].type='IGNORE'
                        lex[token+1].type='IGNORE'
                        del lex[token+1]
                        token-=2
                    elif optCompilerEval and lex[token].type == 'NUMBER' and (lex[token-1].type == 'FSTR' or (lex[token-1].type in orderOfOps and lex[token-2].type == 'ID' and lex[token-3].type == 'FSTR')) and lex[token+1].type in orderOfOps and lex[token+2].type == 'NUMBER' and lex[token+3].type == 'FSTR':
                        # add paren inside fstrings so compiler eval can understand it easier
                        if lex[token-1].type in orderOfOps:
                            lex.insert(token-2,makeToken(tok, '(', 'LPAREN'))
                        else:
                            lex.insert(token  ,makeToken(tok, '(', 'LPAREN'))
                        lex.insert(token+4,makeToken(tok, ')', 'RPAREN'))
                    token+=1
                else:
                    break # above lex len


        # last optimizations, run at the end (until no more optimization)
        newOptimization = True ; optRounds=0
        while newOptimization:
            if debug: print(f'\t- end optimization round = {optRounds} -')
            newOptimization = False
            for token in range(0,len(lex)-1):
                if token > len(lex)-1: break
                if optFromFunc and optRounds == 0 and lex[token].type == 'IMPORT':
                    # optFromFunc should only run once
                    # this is its import name (ie random.): wasImported,lex[token].value.replace('import ','')+'.'
                    tmpImportNameSplit = lex[token].value.split()
                    if len(tmpImportNameSplit) > 2 and tmpImportNameSplit[2] == 'as':
                        tmpImportedName = tmpImportNameSplit[3]+'.'
                        if tmpImportedName in wasImported:
                            for impv in wasImported[tmpImportedName]:
                                if 'AS!' in impv: importedName = impv[3:] ; break
                            del wasImported[tmpImportedName][wasImported[tmpImportedName].index('AS!'+importedName)]
                            wasImported[importedName] = wasImported[tmpImportedName]
                            del wasImported[tmpImportedName] ; del tmpImportedName
                    else:
                        importedName = tmpImportNameSplit[1]+'.'
                    if debug: print('import: ',lex[token].value.split()[1], wasImported[importedName] if importedName in wasImported else '[]')
                    if 'from' == tmpImportNameSplit[0]:
                        if len(wasImported) > 0 and importedName in wasImported:
                            for fromImport in wasImported[importedName]:
                                if fromImport not in tmpImportNameSplit[3:]:
                                    lex[token].value=lex[token].value+', %s'%fromImport
                            del wasImported[importedName]
                    elif importedName in wasImported:
                        if len(wasImported[importedName]) > 0:
                            tmpf=[i for i in wasImported if i == importedName][0]
                            if len(wasImported[tmpf]) > 1 or wasImported[tmpf][0] != '':
                                tmpImportThese=wasImported[tmpf]
                                tmpConflictingNames=[]
                                for tfunc in tmpImportThese:
                                    for imported in wasImported:
                                        if imported == tmpf:
                                            continue
                                        for importsFunction in wasImported[imported]:
                                            if importsFunction == tfunc:
                                                tmpConflictingNames.append(tfunc)
                                                break
                                for tfunc in range(0,len(wasImported[importedName])):
                                    if wasImported[importedName][tfunc].startswith('AS!'):
                                        tmpAlreadyConflicting=wasImported[importedName][tfunc][3:]
                                        tmpConflictingNames.append(tmpAlreadyConflicting)
                                        wasImported[importedName][tfunc]=tmpAlreadyConflicting

                                if tmpConflictingNames:
                                    tmpImportThese = [_ for _ in wasImported[tmpf] if _ not in tmpConflictingNames]
                                    for conflict in tmpConflictingNames:
                                        lex.insert(token+1, makeToken(tok, f'from {importedName[:-1]} import {conflict} as {importedName[:-1]}_{conflict}','IMPORT'))
                                        lex.insert(token+1, makeToken(tok, ';', 'THEN'))
                                lex[token].value='from %s import %s'%(importedName[:-1], ', '.join([tmpImportThese[i] for i in range(0,len(tmpImportThese))]))
                            else: lex[token].value=f'from {importedName} import *'
                            del wasImported[importedName]

                elif optDeadVariableElimination and lex[token].type == 'ID' and lex[token].value != 'print':
                    if ((lex[token].value not in definedFuncs and lex[token + 1].type in typeAssignables and lex[token+1].type!='LIST') or (lex[token + 1].type=='ASSIGN' and lex[token + 1].value in ('=','is','is '))) and lex[token - 1].type in typeNewline + ('CONSTANT', 'TYPE'):
                        delPoint = tmpIndent = None ; check = True ; tmpReplaceWithPass = inCase = isConstant = outOfBlock = False
                        tmpCurrentIndent = 0 ; tmpParenScope=0
                        # tmpIndent is var's indent, tmpCurrentIndent is iterations indent
                        # outOfBlock signifies that if we are in a function or class, we are safe from previous classes
                        tmpSkipCheck = lex[token] in varWasFolded # bypasses checking if the var is dead, assumes it is dead
                        tmpInsideFunction = None # determine if inside a function. outOfBlock seems to be pretty much the same thing.
                        tmpVarIsDefinedInFunction = None # if the var was defined in a function or on outer scope. for use when checking ahead
                        tmpLowestIndent=(None,0) # track lowest indent 1st: in func 2nd: lowest indent
                        for tmpi in range(token-1, -1, -1):
                            #print(lex[token].value, '-!', lex[tmpi].value, lex[tmpi].type, tmpIndent,check,tmpInsideFunction)
                            if tmpSkipCheck and delPoint != None and (outOfBlock or tmpCurrentIndent == 0): check=True ; break
                            if lex[tmpi].type == 'TAB':
                                if tmpIndent==None: tmpIndent = lex[tmpi].value.replace('\t', ' ').count(' ')
                                if delPoint==None:
                                    delPoint=tmpi
                                    if lex[tmpi-1].type in typeConditionals+('PYDEF', 'DEFFUNCT'): tmpReplaceWithPass = True
                                tmpCurrentIndent = lex[tmpi].value.replace('\t', ' ').count(' ')
                                inCase=False
                                if tmpCurrentIndent < tmpLowestIndent[1]: tmpLowestIndent = (tmpLowestIndent[0], tmpCurrentIndent)
                            elif lex[tmpi].type == 'NEWLINE':
                                if tmpIndent==None:
                                    if isConstant: check = False ; break # constants on global scope should not be eliminated
                                    tmpIndent = 0
                                if delPoint==None:
                                    delPoint = tmpi
                                    if lex[tmpi-1].type in typeConditionals+('PYDEF','DEFFUNCT'): tmpReplaceWithPass = True
                                tmpCurrentIndent = 0
                                inCase=False
                                if tmpLowestIndent[0] in {True,False}: tmpLowestIndent = (tmpLowestIndent[0], 0)
                                else: tmpLowestIndent = (False, 0)
                            elif lex[tmpi].type == 'THEN':
                                if delPoint==None: delPoint=tmpi
                                inCase=False
                            elif lex[tmpi].type == 'SCOPE' and lex[token].value in lex[tmpi].value :
                                check=False ; break
                            elif lex[tmpi].type == 'FROM' and not delPoint: break
                            elif not inCase and lex[tmpi].type in {'ID', 'INC', 'BUILTINF','FUNCTION'} and (lex[tmpi].value.replace('(','').replace('+','').replace('-', '') == lex[token].value or lex[token].value+'.' in lex[tmpi].value):
                                if tmpInsideFunction == None:
                                    tmptmpIndent=tmpCurrentIndent
                                    for tmptmp in range(tmpi-1,0,-1):
                                        if lex[tmptmp].type == 'NEWLINE':
                                            if tmpVarIsDefinedInFunction == None: tmpVarIsDefinedInFunction = False
                                            tmpInsideFunction=False ; break
                                        elif lex[tmptmp].type == 'TAB': tmptmpIndent=lex[tmptmp].value.count(' ')
                                        elif lex[tmptmp].type in {'PYDEF','DEFFUNCT'} \
                                        and ( (lex[tmptmp].type == 'PYDEF'    and (lex[tmptmp-1].type == 'NEWLINE' or (lex[tmptmp-1].type == 'TAB' and lex[tmptmp-1].value.count(' ') < tmptmpIndent))) \
                                        or    (lex[tmptmp].type == 'DEFFUNCT' and (lex[tmptmp-2].type == 'NEWLINE' or (lex[tmptmp-1].type == 'TAB' and lex[tmptmp-2].value.count(' ') < tmptmpIndent)))):
                                            if tmpVarIsDefinedInFunction == None: tmpVarIsDefinedInFunction = True
                                            check=False ; tmpInsideFunction=True ; break
                                    if not check: break
                                else:
                                    check = False ; break
                            elif lex[tmpi].type in {'PYDEF','DEFFUNCT'} and not outOfBlock and tmpIndent:
                                if lex[tmpi].type == 'PYDEF':
                                    if lex[tmpi-1].type == 'NEWLINE' or (lex[tmpi-1].type in {'ASYNC','FUNCMOD'} and lex[tmpi-2].type == 'NEWLINE') or (lex[tmpi-1].type == 'ASYNC' and lex[tmpi-2].type == 'FUNCMOD' and lex[tmpi-3].type == 'NEWLINE'):
                                        tmpInsideFunction = outOfBlock = True
                                    elif lex[tmpi-1].type == 'TAB' and lex[tmpi-1].value.count(' ') < tmpIndent:
                                        tmpInsideFunction = outOfBlock = True
                                if tmpLowestIndent[0] == None:
                                    tmpFuncNLPlacement=1 if lex[tmpi].type == 'PYDEF' else 2
                                    if   lex[tmpi - tmpFuncNLPlacement].type == 'NEWLINE' and tmpLowestIndent[1] > 0:
                                        tmpInsideFunction = True
                                    elif lex[tmpi - tmpFuncNLPlacement].type == 'TAB' and lex[tmpi - 1].value.count(' ') < tmpLowestIndent[1]:
                                        tmpInsideFunction = True
                                    if tmpInsideFunction:
                                        if tmpVarIsDefinedInFunction == None: tmpVarIsDefinedInFunction = True
                                        tmpLowestIndent = (True, tmpLowestIndent[1])
                            elif (lex[tmpi].type in typeConditionals or (lex[tmpi].type == 'TRY' and 'try' in lex[tmpi].value)) and delPoint:
                                # prevents dead variables defined in conditionals from breaking syntax
                                tmpReplaceWithPass = True
                                if lex[tmpi].type == 'IF':
                                    inCase = False
                            elif lex[tmpi].type == 'PYCLASS' and delPoint and tmpIndent:
                                if not outOfBlock and ((lex[tmpi-1].type == 'NEWLINE' and tmpIndent > 0) \
                                or (lex[tmpi-1].type == 'TAB' and lex[tmpi-1].value.count(' ') < tmpIndent)):
                                    check=False ; break
                            elif lex[tmpi].type == 'TYPEWRAP' and not outOfBlock:
                                if lex[tmpi-1].type == 'TAB':
                                    tmpIndent = lex[tmpi-1].value.replace('\t', ' ').count(' ')
                                elif lex[tmpi-1].type == 'NEWLINE': tmpIndent=0
                                else: tmpIndent=None
                            elif lex[tmpi].type == 'OF' and 'case' in lex[tmpi].value:
                                inCase=True
                            elif lex[tmpi].type == 'LPAREN': tmpParenScope+=1
                            elif lex[tmpi].type == 'FUNCTION' and '(' in lex[tmpi].value: tmpParenScope+=1
                            elif lex[tmpi].type == 'RPAREN': tmpParenScope-=1
                            elif lex[tmpi].type == 'CONSTANT' and not delPoint: isConstant = True
                        if tmpParenScope > 0: check=False
                        if delPoint == None or tmpIndent == None: check=False
                        if (token-1<=0): check=True ; delPoint = tmpIndent = 0
                        if check:
                            breakOnNextNL = ttenary = inCase = tmpCallsFunction = tmpOutOfAssign = tmpInConditional = tmpOutOfStartingBlock = tmpInOtherFunction = False
                            tmpCurrentIndent = tmpIndent  # tmpIndent is indent of OG var, tmpCurrentIndent is current
                            for tmpi in range(token + 1, len(lex) - 1):
                                #print(lex[token].value,'+!',lex[tmpi].value,lex[tmpi].type,ttenary,inCase,tmpCallsFunction,tmpInOtherFunction,tmpCurrentIndent,tmpIndent)
                                if not inCase and lex[tmpi].type in {'ID', 'INC', 'BUILTINF', 'FUNCTION'} and (lex[tmpi].value.replace('(', '').replace('+', '').replace('-', '') == lex[token].value or lex[token].value + '.' in lex[tmpi].value) and (not tmpInOtherFunction or not tmpVarIsDefinedInFunction):
                                    if lex[tmpi + 1].type != 'ASSIGN' or (lex[tmpi + 1].value.strip() not in {'=', 'is'} or determineIfAssignOrEqual(tmpi + 1)):
                                        check = False
                                    elif pyIs and lex[tmpi + 1].type == 'ASSIGN' and '=' not in lex[tmpi + 1].value:
                                        check = False
                                    elif not tmpOutOfStartingBlock and tmpCurrentIndent == tmpIndent and not tmpCallsFunction:
                                        breakOnNextNL = True
                                elif not inCase and lex[tmpi].type == 'NRANGE':
                                    tmp = False
                                    if '...' in lex[tmpi].value:
                                        tmp = lex[tmpi].value.split('...')
                                    elif '..' in lex[tmpi].value:
                                        tmp = lex[tmpi].value.split('..')
                                    elif 'to' in lex[tmpi].value:
                                        tmp = lex[tmpi].value.split('to', 1)
                                    if tmp:
                                        tmp = [i.strip() for i in tmp]
                                        if lex[token].value in tmp:
                                            check = False
                                elif lex[tmpi].type == 'INDEX' and (lex[tmpi].value.startswith(lex[token].value) or REsearch(fr'.+\[.*{lex[token].value}.*\]', lex[tmpi].value)):
                                    check = False
                                elif lex[tmpi].type == 'LISTCOMP' and lex[tmpi].value.split(':')[0] == lex[token].value:
                                    check = False
                                elif lex[tmpi].type == 'FROM':
                                    for tmpii in range(tmpi, 0, -1):
                                        if lex[tmpii].type == 'TAB':
                                            tmp = lex[tmpii].value.replace('\t', ' ').count(' ')
                                            break
                                        elif lex[tmpii].type == 'NEWLINE':
                                            tmp = 0 ; break
                                    if tmp < tmpIndent and tmpVarIsDefinedInFunction:
                                        breakOnNextNL = True
                                elif lex[tmpi].type == 'ASSIGN' and ':' not in lex[tmpi].value and lex[tmpi + 1].type == 'IF':
                                    ttenary = True
                                elif lex[tmpi].type == 'ELSE' and ttenary:
                                    ttenary = False
                                elif lex[tmpi].type == 'ELSE' and not ttenary and lex[tmpi-1].type in typeNewline:
                                    tmpOutOfStartingBlock=True
                                elif breakOnNextNL and not ttenary and lex[tmpi].type in typeNewline:
                                    break
                                elif lex[tmpi].type == 'INDEX' and f" {lex[token].value} " in lex[tmpi].value:
                                    check = False
                                elif lex[tmpi].type == 'OF' and 'case' in lex[tmpi].value:
                                    inCase = True
                                elif lex[tmpi].type == 'IF':
                                    inCase = False
                                    tmpInConditional = True
                                elif lex[tmpi].type in {'ELIF', 'OF'}:
                                    tmpInConditional = True
                                elif lex[tmpi].type == 'PYDEF' :
                                    if REfindall(f'=.*?{lex[token].value}',lex[tmpi].value):
                                        # if var is in function default argument like:
                                        # def thing(arg=var)
                                        check = False
                                    elif tmpOutOfStartingBlock and tmpCurrentIndent<tmpIndent: tmpInOtherFunction = True
                                elif lex[tmpi].type in typeNewline:
                                    if not ttenary: tmpOutOfAssign = True
                                    if lex[tmpi].type == 'NEWLINE':
                                        tmpCurrentIndent = 0
                                    elif lex[tmpi].type == 'TAB':
                                        tmpCurrentIndent = lex[tmpi].value.count(' ')
                                    elif lex[tmpi].type == 'THEN' and tmpInConditional:
                                        tmpCurrentIndent += prettyIndent
                                    if tmpCurrentIndent < tmpIndent:
                                        tmpOutOfStartingBlock = True
                                        if tmpi + 2 < len(lex) and (lex[tmpi + 1].type == 'PYDEF' or lex[tmpi + 2].type == 'DEFFUNCT'):
                                            tmpInOtherFunction = True
                                    tmpInConditional = False
                                elif not tmpSkipCheck and not tmpOutOfAssign and lex[tmpi].type == 'FUNCTION':
                                    tmpCallsFunction = True
                                elif lex[tmpi].type == 'SCOPE' and lex[token].value in ''.join(lex[tmpi].value.split(' ')[1:]).split(','):
                                    check = False
                                if not check: break

                                #tmpDEBUG = 'testVar'
                                #if debug and lex[token].value == tmpDEBUG and lex[tmpi].type == 'ID' and lex[tmpi].value == tmpDEBUG: print(inCase,tmpInOtherFunction,tmpInsideFunction);exit()
                            if tmpSkipCheck:
                                varWasFolded.remove(lex[token])

                        if check:  # remove the var
                            #print('-------', lex[token].value)
                            ttenary = tmpPass = False ; tmpEnd = tmpParenScope = tmpListScope = 0
                            for tmpi in range(delPoint+1, len(lex)*2):
                                #print(lex[tmpi].type,lex[tmpi].value,f"({tmpParenScope}",f"[{tmpListScope}")
                                if tmpi >= len(lex)-1:
                                    tmpEnd = tmpi-1 ; break
                                if lex[tmpi].type == 'ASSIGN' and lex[tmpi+1].type == 'IF': ttenary=True
                                elif lex[tmpi].type == 'ASSIGN' and tmpCallsFunction:
                                    lex[tmpi].type = 'DONTDEXP'
                                    if lex[delPoint+1].type == 'NOTHING':
                                        tmpPass = False ; lex[delPoint+1].type = 'IGNORE'
                                    tmpEnd=tmpi ; break
                                elif ttenary and lex[tmpi].type == 'ELSE': ttenary=False

                                if not ttenary and lex[tmpi].type in typeNewline and tmpListScope <= 0 and tmpParenScope <= 0:
                                    tmpEnd=tmpi ; break
                                else:
                                    if tmpReplaceWithPass:
                                        lex[tmpi].type = 'NOTHING'
                                        tmpReplaceWithPass=False
                                    elif lex[tmpi].type == 'INC' and lex[tmpi].value.replace('+','').replace('-','').strip() != lex[token].value:
                                        lex.insert(tmpi+1,makeToken(lex[tmpi],'then','tmpPass')) ; tmpPass=True
                                    elif lex[tmpi].type == 'tmpPass': pass
                                    else:
                                        if lex[tmpi].type in {'LPAREN','FUNCTION'}:
                                            if lex[tmpi].type == 'FUNCTION':
                                                if '(' in lex[tmpi].value:
                                                    tmpParenScope += lex[tmpi].value.count('(')
                                                    tmpParenScope -= lex[tmpi].value.count(')')
                                            else:
                                                tmpParenScope += 1
                                        elif lex[tmpi].type == 'RPAREN':
                                            tmpParenScope -= 1
                                        elif lex[tmpi].type in {'LIST','LINDEX'}: tmpListScope+=1
                                        elif lex[tmpi].type in {'LISTEND','RINDEX'}: tmpListScope-=1
                                        elif lex[tmpi].type == 'INDEX':
                                            tmpListScope += lex[tmpi].value.count('[')
                                            tmpListScope -= lex[tmpi].value.count(']')
                                        elif lex[tmpi].type == 'optLIST':
                                            if lex[tmpi].value == '[': tmpListScope+=1
                                            else: tmpParenScope += 1
                                        lex[tmpi].type = 'IGNORE'
                            if tmpPass:
                                # for removing the INCs from the expression while still keeping their effects by splitting them into new lines
                                # tmpPass is so the previous section doesnt delete the THEN inserts
                                for tmpi in range(delPoint + 1, tmpEnd*3):
                                    if tmpi >= len(lex) - 1: break
                                    if lex[tmpi].type == 'tmpPass':
                                        lex[tmpi].type='THEN'
                            if debug: print('eliminated variable:', lex[token].value)
                            newOptimization=True
            optRounds+=1

            #print(' '.join([t.value for t in lex]))
        # clean up vv
        l = 0
        while l < len(lex):
            if optListToTuple and lex[l].type=='optLIST':
                if lex[l].value=='(': lex[l].type='LPAREN'
                else: lex[l].type='LIST'
            elif lex[l].type == 'IGNORE': del lex[l] ; l-=1
            elif lex[l].type == 'FUNCTION' and lex[l].value[-1]!='(' and lex[l+1].type == 'LPAREN':
                lex[l].value+='(' ; del lex[l+1]
            l+=1
        del wasImported, varWasFolded


    # python transpiling ---------------------------

    def decideIfIndentLine(indent,txt,modifyStartOfLineVar=True):
        nonlocal startOfLine
        if startOfLine and indent>0:
            if modifyStartOfLineVar: startOfLine=False
            return f"{' '*indent}{txt}"
        if indent==0 and startOfLine and modifyStartOfLineVar: startOfLine=False
        return txt

    def clearFunctionVars():
        nonlocal storedVarsHistory, insideDef, notInDef
        #return
        if notInDef and any(True for i in storedVarsHistory if 'insideFunction' in storedVarsHistory[i]):
            # clears out variables from functions
            tmpCopy= {}
            for var in storedVarsHistory:
                if 'insideFunction' in storedVarsHistory[var]:
                    pass
                else:
                    tmpCopy[var]=storedVarsHistory[var]
            storedVarsHistory=copy(tmpCopy)
            insideDef = ''

    def storeVar(var1,var2,var3,staticType=None,position=None):
        nonlocal storedVarsHistory, lex, lexIndex, insideDef
        var1=copy(var1) ; var2=copy(var2) ; var3=copy(var3)
        # var1 = ID , var2 = newValue , var3 = check if theres more going on
        var1.value=var1.value.strip()
        if ':' in var1.value:
            var1.value=var1.value.split(':')[0].replace(' ','')
        if var1.type == 'ID' and '*' in var1.value: var1.value=var1.value.replace('*','')

        if ',' in var1.value:
            # if ID has multiple variables, recurse over them
            tmp=copy(var1) ; tmp.value=tmp.value.split(',')[1]
            storeVar(tmp,var2,var3)
            var1.value=var1.value.split(',')[0]

        if var3.type in typeAssignables+typeNewline+('LISTCOMP','RPAREN','COMMAGRP','COMMA','LINDEX','LPAREN') and var2 != None:
            if var2.type == 'ID' and var2.value in storedVarsHistory and 'value' in storedVarsHistory[var2.value]:
                if enforceTyping and var1.value in storedVarsHistory and 'staticType' in storedVarsHistory[var1.value]\
                and storedVarsHistory[var2.value]['type'] != storedVarsHistory[var1.value]['staticType']:
                    return AS_SyntaxError(f'{var1.value} is of type {storedVarsHistory[var1.value]["type"]}, can\'t inherit {var2.value}\'s type {storedVarsHistory[var2.value]["type"]}.',f'{var1.value} should only be of type {storedVarsHistory[var1.value]["type"]}',lineNumber,data,'Enforced Static Type Error:')
                else:
                    storedVarsHistory[var1.value]={'value':storedVarsHistory[var2.value]['value'],'type':storedVarsHistory[var2.value]['type']}
            elif var2.type == 'LBRACKET' and position and lex[position+2].type in {'COLON','COMMA'}:
                if lex[position+2].type == 'COLON': tmpTheType='DICT'
                else: tmpTheType='SET'
                if var1.value not in storedVarsHistory:
                    storedVarsHistory[var1.value]={'value':var2.value,'type':tmpTheType}
                else:
                    storedVarsHistory[var1.value]['type'] = tmpTheType
            else:
                if var2.value[0]=='(' and var2.value[-1]==')': var2.value=var2.value[1:-1]
                if var1.value not in storedVarsHistory:
                    storedVarsHistory[var1.value]={'value':var2.value,'type':var2.type}
                else:
                    storedVarsHistory[var1.value]['value'] = var2.value
                    storedVarsHistory[var1.value]['type'] = var2.type
            if notInDef: insideDef = ''
            if insideDef != '': storedVarsHistory[var1.value]['insideFunction'] = insideDef
        if staticType != None:
            if var1.value not in storedVarsHistory:
                storedVarsHistory[var1.value]={}
            storedVarsHistory[var1.value]['type']=convertType[staticType.value]
            storedVarsHistory[var1.value]['staticType']=staticType.value

        if var2 != None and var2.type == 'ID' and var2.value in storedVarsHistory \
        and storedVarsHistory[var2.value]['type'] in ('LIST','LISTCOMP') and var3.type in typeNewline and var1.type != 'RINDEX':
            # copy list on assignment, screw references
            if lex[lexIndex].value==var2.value: lex[lexIndex].value+='[:]'
            elif lex[lexIndex+1].value==var2.value: lex[lexIndex+1].value+='[:]'

        if position != None and var1.value in storedVarsHistory:
            tmp=[]
            for tmpi in range(position,len(lex)):
                if lex[tmpi].type in typeNewline: break
                else: tmp.append(lex[tmpi])
            storedVarsHistory[var1.value]['line']=tmp
    def checkVarType(name,theType):
        if name in storedVarsHistory and 'type' in storedVarsHistory[name]:
            if isinstance(theType, tuple) or isinstance(theType, set):
                if storedVarsHistory[name]['type'] in theType: return True
                else: return False
            elif storedVarsHistory[name]['type'] == theType: return True
        else: return False



    def findEndOfFunction(lexIndex,goBackwards=False,needsToBeFunction=False):
        if goBackwards:
            scope=0
            for i in range(lexIndex,0,-1):
                if lex[i].type == 'RPAREN':
                    scope+=1
                elif lex[i].type in ('LPAREN','FUNCTION','BUILTINF'):
                    scope-=1
                    if scope<0: scope=0
                    if scope==0:
                        if needsToBeFunction and lex[i].type=='LPAREN': pass
                        else:
                            if i-1 > 0 and lex[i-1].type == 'BUILTINF' and lex[i].value[0]=='.':
                                return i-1
                            return i
                elif lex[i].type in typeNewline+('LISTCOMP',): return False
        else:
            scope=1
            for i in range(lexIndex,len(lex)-1):
                if lex[i].type == 'RPAREN':
                    scope-=1
                    if scope<0: scope=0
                    if scope==0: return i
                elif lex[i].type in ('LPAREN','FUNCTION','BUILTINF'):
                    scope+=1
                elif lex[i].type in typeNewline+('LISTCOMP',): return False
        return False

    def cythonPrint(tok):
        # an optimization to convert a print statement to printf for cython
        # this was originally meant for expression print detect, but now its
        # in function form so print without parenthesis can use it, and print.
        # currently for singular values only
        nonlocal storedVarsHistory, line
        check=False # for checking if printf needs to be imported
        tmpval=tok.value ; tmptype=tok.type
        if tok.value in storedVarsHistory and 'staticType' in storedVarsHistory[tok.value]:
            if storedVarsHistory[tok.value]['staticType'] == 'int':
                line.append(f'{" "*(indent)}printf("%llu\\n",{tmpval})\n')
                check=True
            else: line.append(f'{" "*(indent)}print({tmpval})')
        elif tmptype in {'STRING','STRLIT','STRRAW'}:
            if any(i for i in ('r','f','R','F') if i == tmpval[0]):
                line.append(f'{" "*(indent)}printf({tmpval}.encode())\n')
                line.append(f'{" "*(indent)}printf("\\n")')
            elif tmpval.startswith("'''") or tmpval.startswith('"""'):
                tmpval=list(tmpval) ; tmpval.insert(-3,'\n')
                tmpval=''.join(tmpval)
                line.append(f'{" "*(indent)}printf({tmpval})\n')
            else: line.append(f'{" "*(indent)}printf("{tmpval[1:-1]}\\n")\n')
            check=True
        elif tmptype == 'NUMBER':
            if '.' in tmpval: line.append(f'{" "*(indent)}printf("%.14f",{tmpval})\n')
            elif int(tmpval) <= 2147483647: line.append(f'{" "*(indent)}printf("%d\\n",{tmpval})\n')
            else: line.append(f'{" "*(indent)}printf("{tmpval}\\n")\n')
            check=True
        else:
            if tmpval in storedCustomFunctions:
                line.append(f'{" " * (indent)}print({tmpval}())')
            else:
                line.append(f'{" "*(indent)}print({tmpval})')

        if check: insertAtTopOfCodeIfItIsNotThere('from libc.stdio cimport printf\n')

    def optLoopToMap_Function(lexIndex, forLoop=True):
        # optimization which turns loops appending with only a function into a map
        # needed this code for both FOR and LOOP tokens, so made it into a function
        nonlocal lex, code, parenScope
        tmpFirstIndent = True
        isAppending = recordIter = tmpFound = False
        tmpIter = [] ; tmpPreTokens=[] ; tmpVal = iterVar = '' ; expressionStart = None
        tmpindent=0
        if not forLoop:
            loopSyntaxCount = 0
        for tmpi in range(lexIndex, len(lex)):
            if not forLoop and loopSyntaxCount <= 3 and not expressionStart:
                if lex[tmpi].type in typeNewline+('FUNCTION',):
                    if lex[tmpi].type == 'FUNCTION': expressionStart = tmpi-1
                    else: expressionStart = tmpi ; tmpFirstIndent = False
                    loopSyntaxCount=3
                elif lex[tmpi].type not in typeMops:
                    loopSyntaxCount+=1
                if loopSyntaxCount > 2:
                    if lex[lexIndex+1].value in storedVarsHistory: return False # for when it isn't a iteration variable
                    recordIter = False
                    if lex[tmpi].type in {'ID','IGNORE'}:
                        iterVar = lex[tmpi].value
                    tmpIter.append(copy(lex[tmpi-1]))
                    if expressionStart == None: expressionStart = tmpi
                    if lex[tmpi].type == 'TAB': tmpindent = lex[tmpi].value.replace('\t', ' ').count(' ')
                    elif lex[tmpi + 1].type == 'PIPE' and lex[tmpi].type == 'ID' and lex[tmpi].value not in storedVarsHistory:
                        expressionStart -= 1
                continue

            if lex[tmpi].type in typeNewline:
                if tmpFirstIndent:
                    recordIter = tmpFirstIndent = False
                    if forLoop: expressionStart = tmpi
                    if lex[tmpi].type == 'TAB': tmpindent = lex[tmpi].value.replace('\t',' ').count(' ')
                else:
                    if tmpindent and lex[tmpi].type == 'TAB' and lex[tmpi].value.replace('\t',' ').count(' ') == tmpindent:
                        return False
                    else:
                        break
            elif tmpFirstIndent and recordIter and lex[tmpi].type != 'ENDIF':
                if forLoop and (lex[tmpi].type == 'NUMBER' or (lex[tmpi].value in storedVarsHistory and 'type' in storedVarsHistory[
                lex[tmpi].value] and storedVarsHistory[lex[tmpi].value]['type'] == 'NUMBER')) and tmpIter == [] and lex[tmpi+1].type in typeNewline+('ENDIF',):
                    tmpIter.append(makeToken(lex[tmpi],'range(','FUNCTION'))
                    tmpIter.append(copy(lex[tmpi]))
                    tmpIter.append(makeToken(lex[tmpi], ')', 'RPAREN'))
                else: tmpIter.append(copy(lex[tmpi]))
            elif tmpFirstIndent and forLoop and lex[tmpi].type == 'INS' and lex[tmpi].value.strip() == 'in':
                recordIter = True ; iterVar = lex[tmpi-1].value
            elif lex[tmpi].type in typeNewline+('FOR','LOOP') and not tmpFirstIndent:
                return False
            elif  (lex[tmpi].type == 'BUILTINF' and 'append' in lex[tmpi].value and ((tmpi+3 < len(lex)-1 and lex[tmpi + 2].type == 'FUNCTION' and lex[tmpi+3].value == iterVar) or (
                    tmpi+4 < len(lex)-1 and  lex[tmpi + 2].type == 'ID' and lex[tmpi + 3].type == 'PIPE' and lex[tmpi + 4].type in ('ID','BUILTINF'))) or (lex[tmpi-1].type == 'PIPE' and lex[tmpi-3].type == 'PIPE')):
                isAppending=True
                if tmpi+2 < len(lex) and lex[tmpi + 2].type == 'FUNCTION':
                    func = lex[tmpi + 2].value.replace('(', '')
                    if lex[tmpi + 4].type != 'RPAREN':
                        break
                elif lex[tmpi-1].type == 'PIPE':
                    func=lex[tmpi-2].value
                else:
                    func = lex[tmpi + 4].value  # pipe

                if '.' in lex[tmpi].value and lex[tmpi].value.split('.')[0] in storedVarsHistory:
                    tmpVal = lex[tmpi].value.split('.')[0]
                    break
                elif '.' in lex[tmpi].value and lex[tmpi].value.split('.')[0] == '' and lex[tmpi-1].type == 'ID' and lex[tmpi-1].value in storedVarsHistory:
                    tmpVal = lex[tmpi-1].value
                elif optLoopAttr and '_append' in lex[tmpi].value and lex[tmpi].value.startswith('AS'):
                    tmpVal = lex[tmpi].value[2:-7]
                   # break

        if not forLoop:
            if not iterVar: return False
            if lex[lexIndex + 1].type == 'NUMBER' or (
                    lex[lexIndex + 1].value in storedVarsHistory and 'type' in storedVarsHistory[
                    lex[lexIndex + 1].value] and storedVarsHistory[lex[lexIndex + 1].value]['type'] == 'NUMBER'):
                tmpIter=[]
                tmpIter.append(makeToken(lex[lexIndex], 'range(', 'FUNCTION'))
                tmpIter.append(copy(lex[lexIndex + 1]))
                tmpIter.append(makeToken(lex[lexIndex], ')', 'RPAREN'))
            else:
                tmpIter = [copy(lex[lexIndex + 1])]

        if isAppending and tmpVal and tmpVal in storedVarsHistory and 'type' in storedVarsHistory[tmpVal] \
        and storedVarsHistory[tmpVal]['type'] in {'LIST', 'LISTCOMP'} and 'line' in storedVarsHistory[tmpVal]:
            for ii in range(tmpi,len(lex)-1):
                if lex[ii].type in typeNewline:
                    tmpPos=ii-1 ; break
            if storedVarsHistory[tmpVal]['type'] == 'LIST' and len(storedVarsHistory[tmpVal]['line']) <= 2:
                # empty list
                tmpPreTokens.append(makeToken(lex[lexIndex], tmpVal, 'ID'))
                tmpPreTokens.append(makeToken(lex[lexIndex], 'list(', 'FUNCTION'))
                tmpPreTokens.append(makeToken(lex[lexIndex], 'map(', 'FUNCTION'))
                tmpPreTokens.append(makeToken(lex[lexIndex], func, 'FUNCTION'))
                tmpPreTokens.append(makeToken(lex[lexIndex], ',', 'COMMA'))
            else:  # non empty list
                line.append(decideIfIndentLine(indent, f'{tmpVal}.extend(map({func},'))
                parenScope+=2
            tmpParen = 2
            tmpFound = True
        elif not isAppending and expressionStart != None and expressionStart+4 < len(lex):
            tmpFound = False ; tmpList=''
            # list( is needed for globals in functions. in instances where its known function is pure, we should get rid of it
            if lex[expressionStart+1].type == 'FUNCTION' and lex[expressionStart+2].type != 'COMMAGRP' and lex[expressionStart+3].type == 'RPAREN' and lex[expressionStart+4].type in typeNewline:
                if expressionStart + 3 + 2 < len(lex)-1 and lex[expressionStart + 3 + 2].type == 'LOOP': return False
                elif forLoop and not (lex[expressionStart+2].type == 'ID' and lex[expressionStart+2].value == iterVar):
                    return False
                lex[expressionStart+1].type = lex[expressionStart+2].type = 'IGNORE'
                tmpf = lex[expressionStart + 1].value.replace("(","")
                tmpList = 'list(' ; tmpParen = 2
                tmpFound = True
                tmpPos = expressionStart + 3
                line.append(decideIfIndentLine(indent, f'{tmpList}map({tmpf}, '))
            elif expressionStart+4 < len(lex)-1 and lex[expressionStart+2].type == 'PIPE'  and lex[expressionStart+1].type != 'COMMAGRP' and lex[expressionStart+3].type == 'ID' and lex[expressionStart+4].type in typeNewline:
                if lex[expressionStart + 1 + 4].type == 'LOOP': return False
                for i in range(4):
                    lex[expressionStart + i].type = 'IGNORE'
                tmpf=lex[expressionStart+3].value
                tmpList='list(' ; tmpParen = 2
                tmpFound = True
                tmpPos = expressionStart + 1
                line.append(decideIfIndentLine(indent, f'{tmpList}map({tmpf}, '))
            if tmpFound:
                parenScope += 2 if tmpList else 1

        if tmpFound:
            for t in range(lexIndex, tmpPos+1):
                lex[t].type = 'IGNORE'
            for t in range(tmpParen):
                lex.insert(tmpPos + 1, makeToken(lex[lexIndex], ')', 'RPAREN'))
            tmpIter = tmpIter[::-1]
            for t in tmpIter:
                lex.insert(tmpPos, t)
            if tmpPreTokens:
                for t in reversed(tmpPreTokens):
                    lex.insert(tmpPos, t)
            if isAppending and optLoopAttr and f"AS{tmpVal}_append = {tmpVal}.append" in code[-1]:
                code = code[:-1]

        return tmpFound

    def checkIfImpureFunction(lexIndex,pyDef,functionArgNames):
        if any(True for var in functionArgNames if functionArgNames[var] in ('list',None)):
            return True # all function arguments must be typed, and not a list
        unsafeFunctions = {'map', 'open', 'input', 'print'}
        if optimize and optLoopAttr:
            # accounts for Python builtins renamed by the optimizer.
            unsafeFunctions.update({f"AS{uf}" for uf in unsafeFunctions})


        # TODO: detect default expressions. the function can be impure, however we can check if the function in expPrint is marked as safe.

        impure = False
        tmp = 1
        tmpIndent=0
        withoutFromSafe=False # for asnake func, declares it is safe even if no from found
        for tmpi in range(lexIndex,0,-1):
            if lex[tmpi].type in typeNewline:
                if lex[tmpi].type == 'TAB':
                    tmpIndent=lex[tmpi].value.replace('\t',' ').count(' ')
                if lex[tmpi].type != 'THEN': break
        while lexIndex + tmp < len(lex) - 1:
            if lex[lexIndex + tmp].type == 'FUNCTION' and (lex[lexIndex + tmp].value.replace('(', '') in unsafeFunctions \
            or (lex[lexIndex + tmp].value.replace('(', '') not in tuple([i for i in storedCustomFunctions if 'pure' in storedCustomFunctions[i] and storedCustomFunctions[i]['pure']]) + pyBuiltinFunctions)):
                if lex[lexIndex + tmp].value.replace('(', '') == lex[lexIndex].value:
                    pass
                else: impure = True ; break
            elif lex[lexIndex + tmp].type == 'BUILTINF' and any(i for i in listMods + ('.open', lex[lexIndex].value + '.', '.join') if i in lex[lexIndex + tmp].value):
                impure = True ; break
            elif (lex[lexIndex + tmp].type == 'ID' and lex[lexIndex + tmp].value in storedVarsHistory and storedVarsHistory[lex[lexIndex + tmp].value]['type'] not in ('LIST','LISTCOMP') ) \
            or (lex[lexIndex + tmp].value in storedCustomFunctions and 'pure' in storedCustomFunctions[lex[lexIndex + tmp].value] and storedCustomFunctions[lex[lexIndex + tmp].value]['pure'] == False):
                if lex[lexIndex + tmp].value not in functionArgNames:
                    impure = True ; break  # ^ TODO check if ID is const, if it is then its not impure. sorta does this when constants put a [0] on the var name
            elif lex[lexIndex + tmp].type == 'ID' and lex[lexIndex + tmp].value.strip() == 'print':
                impure = True ; break
            elif not pyDef and lex[lexIndex + tmp].type == 'FROM':
                withoutFromSafe = False ; break
            elif lex[lexIndex + tmp].type == 'TAB' and lex[lexIndex + tmp].value.count(' ') < tmpIndent:
                if pyDef: break
                else: withoutFromSafe = True
            elif lex[lexIndex + tmp].type == 'NEWLINE' and tmpIndent==0:
                if pyDef: break
                else: withoutFromSafe=True
            elif lex[lexIndex + tmp].type == 'SCOPE': impure=True ; break
            elif lex[lexIndex + tmp].type == 'INS' and lex[lexIndex + tmp].value.strip() == 'in' and lex[lexIndex + tmp+1].type != 'STRING':
                # lists are unhashable. in can imply list. we can't be sure its not a list unless its a string.
                impure=True ; break
            elif lex[lexIndex + tmp].type in {'INDEX','LINDEX','RINDEX'}:
                # lists are unhashable. thus indexes cannot be trusted, as they can be list.
                impure = True ; break
            tmp += 1
        if not pyDef and impure and withoutFromSafe:
            impure = False
        return impure
    def optAddCache():
        nonlocal code, line, lastType, pythonVersion, startOfLine
        tmpCache = 'cache' if pythonVersion >= 3.09 else 'lru_cache'
        if any(i for i in code if f'from functools import {tmpCache}\n' in i) == False:
            code.insert(1, f'from functools import {tmpCache}\n')
        startOfLine = True
        if lastType == 'ASYNC':
            pass  # seems to mess up ASYNC functions
        else:
            line.append(decideIfIndentLine(indent, f'@{tmpCache}{"(maxsize=128)" if tmpCache == "lru_cache" else ""}\n', False))


    inlineReplace = metaInformation[0]
    expPrint = metaInformation[1]
    ignoreIndentation = metaInformation[2]
    functionPassing = metaInformation[3]
    pyIs = metaInformation[4]
    autoEnumerate = metaInformation[5]
    intVsStrDoLen = metaInformation[6]
    metaDefaultExpressionWithFunction = metaInformation[7]
    functionLineWrap = metaInformation[8]
    pyCompatibility = False
    # ^ resetting METAs


    line=[]
    anyCheck='none' # for any of syntax
    fstrQuote='' # keeping track of last quote-type for f-strings
    ignoreNewline=False
    indent=0
    parenScope=0 # for keeping ( track of paren scope ) so tabs inside of them are meaningless
    bracketScope=0 # ^^ same but for { }
    listScope=0 # ^^ same but for [ ]
    indentSoon=False
    startOfLine=True
    tenary=False
    inIf=False
    inReturn=False
    lastType=None
    lastValue=None
    hasPiped=False
    inFuncArg=False
    notInDef=True
    insideDef=''
    combineLines=False
    inLoop=[False,0] # 2nd index is indent. to prevent Cython definitions from being inside them

    # wraps
    bigWrap=False
    rParen=0
    constWrap=False
    incWrap=[['',0]]

    listcomp={}
    lexIndex=-1
    lastIndent=[0,0,[],[],0] # last counted string indent, last indent, last if indent , function indents, with indent
    oldIndent=0
    lineNumber=0
    storedIndents=[0]
    storedVars={}
    storedCustomFunctions={}
    if variableInformation:
        storedVarsHistory=variableInformation
    else:
        storedVarsHistory={} # {'ASVarExample':{'type':'STRING','value':'AS is cool! sometimes'}}
    switchCase={'case':False} ; switchCases=[]
    # idMAIN
    for tok in lex:
        lineNumber=tok.lineno
        if lexIndex+1 <= len(lex)-1:
            lexIndex+=1
        if hasPiped and lastType != 'PIPE': hasPiped=None
        elif hasPiped==None: hasPiped=False
        # ^ delays hasPiped settings to false by one instruction, for multiple pipes per line. hasPiped is to prevent pipe overrides
        if switchCase['case'] and 'indent' in switchCase and indent<=switchCase['indent'] and tok.type not in {'OF','TAB'} and lastType != 'MATCH':
            if lex[lexIndex].type == 'ELSE': pass  # ELSE is an exception where switch-case doesn't end yet
            else:
                switchCase={'case':False}
                if switchCases: switchCase = switchCases.pop() ; indent=switchCase['indent']
        # ^ resets switch-case if it detects indentation is smaller or equal, as this means the switch is over
        if expPrint[0] > indent:
            expPrint[0]=indent
            if len(expPrint) > 2:
                expPrint.pop()
        if debug: print(lineNumber,lexIndex,indent,f'({parenScope}','{'+str(bracketScope),f'[{listScope}','%s'%('T' if startOfLine == True else 'F'),'%s'%('T' if indentSoon == True else 'F'),'%s'%('T' if inIf == True else 'F'),'%s'%('T' if (hasPiped or hasPiped==None) else 'F'),'%s'%('T' if notInDef == True else 'F'),tok.type,tok.value.replace('\n','\\n').replace('\t','\\t'))
        if not inFuncArg or tok.type in typeNewline:
            if tok.type in typeAssignables: # idVALUES
                if tok.type == 'LBRACKET': bracketScope+=1
                elif tok.type == 'RBRACKET':
                    if bracketScope>0: bracketScope-=1
                elif tok.type == 'LIST': listScope+=1
                elif tok.type == 'LISTEND':
                    if listScope>0: listScope-=1
                if lexIndex+1 < len(lex) and lex[lexIndex+1].type in typeAssignables+('ASSIGN',) \
                and tok.value in storedCustomFunctions:
                    del storedCustomFunctions[tok.value] ; tok.value=tok.value.replace('()','')
                if tok.type == 'ID' and lexIndex+1 <= len(lex)-1 \
                and lex[lexIndex+1].type == 'DEFFUNCT': # this is where functions are made
                    # check to make sure function isnt empty
                    tmp=0
                    for tmpi in range(lexIndex+2,len(lex)):
                        if lex[tmpi].type == 'NEWLINE': tmp+=1
                        elif lex[tmpi].type in ('TAB','THEN'): break
                        else: break
                        if tmp >= 2 or tmpi == len(lex)-1:
                            return AS_SyntaxError(f'function {tok.value} is empty.',f'{tok.value} does "something"', lineNumber, data)

                    lastIndent[3].append(indent)
                    # search for func arguments
                    tmp=1 ; tmpf=[] ; search=False ; impure=False
                    scopey=[]
                    if compileTo == 'Cython': # @ modifers
                        cyWrapAround=None
                        tmpIndexScope=0
                        tmpSafeVars=[]
                    while lexIndex+tmp < len(lex)-1:
                        if lex[lexIndex+tmp].type == 'FROM':
                            search=True
                        elif search == True and lex[lexIndex+tmp].type in typeNewline: break
                        elif search == True:
                            tmpf.append(lex[lexIndex+tmp])
                        elif lex[lexIndex+tmp].type == 'SCOPE' and not scopey:
                            scopey=' '.join(lex[lexIndex+tmp].value.split()[1:]).split(',')
                            impure = True
                        elif lex[lexIndex+tmp].type == 'DEFFUNCT' and lex[lexIndex+tmp]!=lex[lexIndex+1]: tmp=len(lex)+1
                        if compileTo=='Cython' and optimize and cyWrapAround in {True,None} and lexIndex+tmp < len(lex)-1:
                            if lex[lexIndex+tmp].type == 'LINDEX': tmpIndexScope+=1
                            elif lex[lexIndex+tmp].type == 'RINDEX': tmpIndexScope-=1
                            elif lex[lexIndex+tmp].type == 'FOR' and ( lex[lexIndex+tmp+3].type in {'NUMBER','STRING','LIST','LBRACKET'} \
                            or (lex[lexIndex+tmp+3].type == 'FUNCTION' and lex[lexIndex+tmp+3].value == 'range(' and lex[lexIndex+tmp+4].type == 'NUMBER' and (lex[lexIndex+tmp+5].type=='RPAREN' or (lex[lexIndex+tmp+6].type == 'NUMBER' and lex[lexIndex+tmp+7].type=='RPAREN'))) \
                            or (lex[lexIndex+tmp+3].type == 'NUMBER' and lex[lexIndex+tmp+4].type=='PIPE' and lex[lexIndex+tmp+5].type == 'ID' and lex[lexIndex+tmp+5].value == 'range') ) :
                                # check if for loop iterator var is safe
                                tmpSafeVars.append(lex[lexIndex+tmp+1].value.strip())
                            elif tmpIndexScope == 1:
                                if (lex[lexIndex+tmp].type == 'ID' and lex[lexIndex+tmp].value in tmpSafeVars) or lex[lexIndex+tmp].type == 'NUMBER':
                                    if cyWrapAround == None: cyWrapAround = True
                                elif lex[lexIndex+tmp].type in {'TIMES','PLUS'}:
                                    pass
                                else: cyWrapAround = False # only safe-vars,numbers,plus,times are obviously safe
                            elif tmpIndexScope > 1: cyWrapAround == False
                        tmp+=1
                    if debug and optimize and compileTo == 'Cython': print('cyWrapAround =',cyWrapAround)
                    functionArgNames = []
                    functionArgTypes = {}
                    if all(True if i.type == 'COMMAGRP' else False for i in tmpf):
                        tmpf=' '.join([i.value for i in tmpf])
                        # TODO: make it like below? maybe not
                    else:
                        if debug: print('defunct arg parsing')
                        tmptype=None
                        assign=False
                        newtmp=[]
                        kwargs=None
                        noKwargs=True if len([i for i in tmpf if '*' in i.value])==0 else False # cython no likely * kwargs
                        tmpLastToken=makeToken(tmpf[0],'pass','TMP')
                        for i in tmpf:
                            if len(i.value) > 0 and i.type!='COMMA':
                                if i.value[-1]==',': i.value=i.value[:-1]
                                if len(i.value)>0:
                                    if i.value[0]==',':
                                        i.value=i.value[1:]
                                else: continue
                                if kwargs!=None:
                                    if kwargs=='TIMES':
                                        i.value='*'+i.value
                                    elif kwargs=='MINUS':
                                        i.value='-'+i.value
                                    else: i.value='**'+i.value
                                    kwargs=None

                                if not assign and i.type in {'ID','TYPE'} and (tmpLastToken.type not in {'COMMA','TMP','TYPE'} and tmpLastToken.value not in convertType):
                                    tmp=' '.join([ii.value for ii in tmpf])
                                    return AS_SyntaxError(f'{tmp}\n\tfrom syntax must have commas to separate arguments','from thingy, str name, int number = 12,', lineNumber, data)

                                elif i.value in convertType:
                                    tmptype=i
                                elif i.type == 'ASSIGN': assign=True
                                elif i.type in {'TIMES','EXPONENT','MINUS'}: kwargs=i.type ; continue
                                elif assign:
                                    newtmp[-1]+=f' = {i.value}'
                                    assign=False
                                elif tmptype!=None:
                                    storeVar(i,None,lex[0],tmptype)
                                    functionArgTypes[i.value]=tmptype.value
                                    if compileTo == 'Cython' and noKwargs:
                                        if tmptype.value == 'bool': tmptype.value = 'bint'
                                        newtmp.append(f'{tmptype.value} {i.value}')
                                    else:
                                        newtmp.append(f'{i.value}: {tmptype.value}')
                                    tmptype=None
                                elif i.type == 'PYPASS': newtmp.append(i.value[2:-2])
                                else: newtmp.append(i.value) ; functionArgNames.append(i.value) ; functionArgTypes[i.value]=None
                            elif i.type == 'COMMA' and kwargs!=None:
                                newtmp.append('*')
                                kwargs=None
                            tmpLastToken=i
                        del tmpLastToken
                        newtmp=[i for i in newtmp if i != ' ']
                        tmpf=', '.join(newtmp)

                    # v impure function detection
                    if impure == False: impure=checkIfImpureFunction(lexIndex,False,functionArgTypes)

                    # storing function metadata
                    storedCustomFunctions[tok.value]={}
                    if lex[lexIndex+2].type == 'TYPE':
                        storedCustomFunctions[tok.value]['type']=convertType[lex[lexIndex+2].value]
                    else:
                        storedCustomFunctions[tok.value]['type']=None
                    storedCustomFunctions[tok.value]['pure']=not impure
                    storedCustomFunctions[tok.value]['args']=functionArgTypes
                    insideDef=tok.value

                    if scopey:
                        storedCustomFunctions[tok.value]['scopes']=scopey
                    if optimize and compileTo != 'MicroPython' and optFuncCache and not impure and 'list' not in tmpf:
                        optAddCache()
                    if compileTo == 'Cython' and optimize and cyWrapAround: # append cy function mods
                        insertAtTopOfCodeIfItIsNotThere('cimport cython\n')
                        line.append(decideIfIndentLine(indent,'@cython.wraparound(False)\n',False))


                    if debug: print(f"{impure =}") #; print(storedCustomFunctions)
                    tmpc=''
                    if compileTo == 'Cython':
                        if lex[lexIndex+1].value.startswith('cdoes'):
                            tmpc = 'c'
                        elif lex[lexIndex+1].value.startswith('cpdoes'):
                            tmpc = 'cp'
                    line.append(decideIfIndentLine(indent,f'{tmpc}def {tok.value}({tmpf}):\n'))
                    startOfLine=True ; indent+=prettyIndent ; indentSoon=True
                    if lexIndex+tmp+3 < len(lex) and (lex[lexIndex+tmp+2].type not in typeNewline or lex[lexIndex+tmp+3].type not in typeNewline): indentSoon=False
                    else:
                        if lex[lexIndex+2].type not in typeNewline:
                            if lex[lexIndex+2].type == 'TYPE' and lex[lexIndex+3].type != 'ASSIGN':
                                    lex.insert(lexIndex+3, makeToken(tok, value=f'\n{" "*indent}', type='TAB'))
                            else: lex.insert(lexIndex+2, makeToken(tok, value=f'\n{" "*indent}', type='TAB'))
                    # ^^ fixes a no indentation function from putting a : on a line that clearly doesnt need it
                    if search: notInDef=False # if there is a from, activate it, else default for normal indentation
                    code.append(''.join(line)) ; line=[] ; continue
                elif inFuncArg: pass # dont write to line if function from argument
                elif not startOfLine and ((tok.value in storedCustomFunctions) or (tok.value in pyBuiltinFunctions and tok.value not in storedVarsHistory and tok.value not in typeTypes and lex[lexIndex+1].type in typeNewline+typeOperators)) \
                and '(' not in tok.value and lastType!='FOR' and functionPassing==False and not (lastType == 'RETURN' and lastValue.strip() == 'del'):
                    #vvv adding () to function name if it doesn't have one
                    tok.value=f'{tok.value}()' ; tok.type='FUNCTION'
                    if lexIndex+1 < len(lex) and lex[lexIndex+1].type == 'PIPE' and 'into' not in lex[lexIndex+1].value:
                        pass
                    else:
                        line.append(decideIfIndentLine(indent,tok.value))
                elif tok.type == 'ID' and (lastType in typeNewline+('COLON','ENDIF','LISTCOMP','DEFFUNCT','LPAREN') or (lex[lexIndex-3].type == 'LOOP' or lex[lexIndex-2].type == 'LOOP')) \
                and tok.value.strip() == 'print' and lex[lexIndex+1].type != 'LPAREN':
                    if lastType == 'LISTCOMP':
                        tok.type='FUNCTION' ; tok.value+='('
                        line.append(decideIfIndentLine(indent,tok.value))
                        parenScope+=1
                        tmptok=copy(tok)
                        tmptok.value=')' ; tmptok.type = 'RPAREN'
                        tmpfquote=''
                        for tmpi in range(lexIndex+1,len(lex)):
                            if lex[tmpi].type == 'FSTR':
                                if tmpfquote == '':
                                    for i in lex[tmpi].value:
                                        if i in ('"',"'"): tmpfquote = i ; break
                                else:
                                    if lex[tmpi].value[-1] == tmpfquote:
                                        tmpfquote=''
                            if lex[tmpi].type in typeNewline+('FROM',):
                                if tmpfquote != '' and lex[tmpi].type == 'THEN':
                                    pass
                                else:
                                    lex.insert(tmpi,tmptok)
                                    del tmptok ; break
                        # ^ for using bare print inside of list comps
                    else:
                        if lex[lexIndex + 1].type == 'ASSIGN': # assigning print to value
                            line.append(decideIfIndentLine(indent,tok.value))
                        elif compileTo == 'Cython' and (lexIndex+2 > len(lex)-1 or lex[lexIndex+2].type in typeAssignables+typeNewline and (lexIndex+3 > len(lex)-1 or lex[lexIndex+3].type in typeNewline)):
                            tok.type='IGNORE'
                            if lex[lexIndex+1].type != 'LPAREN':
                                cythonPrint(lex[lexIndex+1]) ; lex[lexIndex+1].type='IGNORE'
                        else:
                            tok.type='FUNCTION' ; tok.value+='(' ; bigWrap=True ; rParen+=1
                            line.append(decideIfIndentLine(indent,tok.value))
                elif lexIndex-2 > 0 and lex[lexIndex-1].type in ('ID','INTOED') and lex[lexIndex-2].type == 'PIPE' and (lexIndex-4 < 0 or lex[lexIndex-4].type != 'LOOP'):
                    return AS_SyntaxError(f'{tok.value} after function pipe makes no sense. Pipes start with a datatype and then chain onto functions.',f'{tok.value} to str to print', lineNumber, data)
                elif (tok.type == 'NUMBER' or (tok.value in storedVarsHistory and 'type' in storedVarsHistory[tok.value] and storedVarsHistory[tok.value]['type']=='NUMBER')) and lastType == 'INS' and lexIndex-3>0 and lex[lexIndex-2].type=='ID' and lex[lexIndex-3].type=='FOR' \
                and (lexIndex+2 < len(lex) and (lex[lexIndex+1].type not in {'PIPE','LINDEX','INDEX'} and lex[lexIndex+2].value != 'range')):
                    line.append(f'range({tok.value})') # converts bare numbers into ranges when in for loop
                elif (tok.type == 'NUMBER' or checkVarType(tok.value,'NUMBER')) and fstrQuote!='': line.append(tok.value) # to not mess up FSTR formatting
                elif tok.type == 'DICT' and lex[lexIndex+1].type == 'FSTR' and fstrQuote!='':
                    line.append(tok.value)
                elif tok.type == 'BOOL' and lexIndex>0 and lex[lexIndex-1].type == 'ID':
                    # var True
                    if inIf:
                        if optimize and optIfTrue and tok.type == 'BOOL' and tok.value=='True' and ((lex[lexIndex-1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-1].value]['type'] == 'BOOL') or lex[lexIndex-1].type == 'OF'):
                            tok.value='' ; tok.type='IGNORE'
                        else:
                            line.append(f'== {tok.value}')
                    else:
                        storeVar(lex[lexIndex-1],lex[lexIndex],lex[lexIndex+1],position=lexIndex)
                        line.append(f'= {tok.value}')
                elif inIf and tok.type == 'BOOL' and optimize and optIfTrue and tok.value=='True' and lastType in ('EQUAL','ASSIGN') and ':' not in lastValue and lex[lexIndex-2].type=='ID' and lex[lexIndex-2].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-2].value]['type'] == 'BOOL':
                    # var is True
                    tok.type='IGNORE'
                    lex[lexIndex-1].type='IGNORE'
                    line=line[:-1]
                else:
                    if tok.type == 'LIST' and lastType == 'ID' and lex[lexIndex+1].type != 'LISTEND' and lastValue in storedVarsHistory:
                        # convert LIST into LINDEX ... RINDEX if there are no COMMA
                        # so that an index is not wrongly assigned
                        tmpScope = 0;
                        tmp = lexIndex
                        for tmpi in range(lexIndex, len(lex) - 1):
                            if lex[tmpi].type in typeNewline:
                                break
                            elif lex[tmpi].type == 'LIST':
                                tmpScope += 1
                                if tmp == None: tmp = lexIndex
                            elif lex[tmpi].type == 'LISTEND':
                                tmpScope -= 1
                            elif lex[tmpi].type == 'COMMA' and tmpScope == 1:
                                break
                            if tmpScope == 0:
                                if tmp != None:
                                    lex[tmpi].type = 'RINDEX'
                                    lex[tmp].type = 'LINDEX'
                                    tmp = None
                                if lex[tmpi].type in typeOperators:
                                    break
                    if (lastType in typeAssignables+('BUILTINF',) or (lastType == 'ELSE' and startOfLine)) and not inFuncArg and tok.type!='LISTEND' and lastType!='LIST' \
                    and tok.type not in ('RBRACKET','LINDEX') and lex[lexIndex-1].type not in ('LBRACKET','RBRACKET') \
                    and not (lastType == 'ID' and lastValue == 'lambda' and 'lambda' not in reservedIsNowVar):
                            if (inIf or lex[lexIndex-2].type=='LPAREN' or fstrQuote!='') and not (tok.type=='LIST' and lastType in {'STRING','ID'}):
                                # if im 'lazy' | if im == 'lazy'
                                line.append('== ')
                            elif (lastType in {'ID','OF'} or (lastType == 'BUILTINF' and tok.type!= 'LPAREN' and '(' not in lastValue)) and lex[lexIndex-1].value.strip() not in ('print','print(') and lex[lexIndex-2].value.strip() not in ('print','print('):
                                # im 'lazy' | im = 'lazy'
                                check=False
                                if compileTo == 'Cython':
                                    # for cython, checks if there is a type before grouping vars into a,b,c
                                    for t in range(lexIndex,0,-1):
                                        if lex[t].type == 'TYPE': check=True ; break
                                        elif lex[t].type in typeNewline: break
                                if compileTo == 'Cython' and check and tok.type == 'ID':
                                    lex[lexIndex-1].type='IGNORE'
                                    tok.value+=','+lex[lexIndex-1].value
                                    if lex[lexIndex-1].value in line[-1]: line.pop()
                                    if len(lex) > lexIndex+2 and lex[lexIndex+1].type == 'TYPE' and lex[lexIndex+2].type == 'ID':
                                        lex[lexIndex+2].value+=','+tok.value
                                        tok.type='IGNORE' ; tok.value=''
                                        continue
                                    if lex[lexIndex+1].type != 'ID':
                                        # reverses order
                                        check=None
                                        tmp=tok.value.split(',')[::-1]
                                        if tmp[-1] in storedVarsHistory:
                                            # if last value is defined, then assume its assigning to that value
                                            check=tmp[-1] ; tmp=tmp[:-1]
                                        tok.value=', '.join(tmp)
                                        if check != None:
                                            tok.value+='= '+check
                                else:
                                    if lexIndex-2 > 0 and lex[lexIndex-2].type == 'CONSTANT' and lex[lexIndex+1].type == 'LISTCOMP':
                                        line.append('=(')
                                    else:
                                        tmpcheck=True
                                        if lex[lexIndex+1].type not in ('ID','ASSIGN'): # ignore if this: var1 var2 = 12
                                            for tmpi in range(lexIndex,len(lex)):
                                                if lex[tmpi].type in typeNewline: break
                                                elif lex[tmpi].type == 'ASSIGN' and ':' not in lex[tmpi].value:
                                                    # if there is an assign in the expression, then clearly user is not trying to hint assign
                                                    tmpcheck=False
                                        if tok.type == 'LIST' and lastType in {'BUILTINF','ID'} and (parenScope > 0 or not startOfLine):
                                            if lex[lexIndex-2].type in typeNewline or (lex[lexIndex-2].type in {'TYPE','CONSTANT'} and lex[lexIndex-3].type in typeNewline) or (lex[lexIndex-2].type in {'TYPE','CONSTANT'} and lex[lexIndex-3].type in {'TYPE','CONSTANT'} and lex[lexIndex-4].type in typeNewline): pass
                                            else: tmpcheck=False
                                        if tmpcheck:
                                            line.append('= ') ; storeVar(lex[lexIndex-1],tok,lex[lexIndex+1],position=lexIndex)
                            elif lex[lexIndex-1].value=='print':
                                line.append('(') ; rParen+=1 ; bigWrap=True
                            if lexIndex+1 <= len(lex) and parenScope == 0:
                                if lex[lexIndex+1].type == 'LISTCOMP' and lexIndex+2 <= len(lex):
                                    storeVar(lex[lexIndex-1],lex[lexIndex+1],lex[lexIndex+2],position=lexIndex+1)
                                else: storeVar(lex[lexIndex-1],tok,lex[lexIndex+1],position=lexIndex)

                    # ^^ ID-a moved up here from the else down there so it works with piping

                    if (lexIndex+1 <= len(lex) and lex[lexIndex+1].type == 'PIPE' and 'into' not in lex[lexIndex+1].value) \
                    or (lexIndex-1 >= 0 and lex[lexIndex-1].type == 'PIPE'):
                        pass
                    else:
                        if tok.type=='ID' and lexIndex+1 <= len(lex) and lex[lexIndex+1].type=='NUMBER' \
                        and lex[lexIndex+1].value[0]=='-' and startOfLine == False:
                            lex[lexIndex+1].value=lex[lexIndex+1].value[1:]
                            tmptok=copy(tok) ; tmptok.type='MINUS' ; tmptok.value='-'
                            lex.insert(lexIndex+1,tmptok) ; del tmptok
                            # ^ if ID, and negative number in next token, and its not the start of line,
                            # then assume its subtracting the number rather than assigning to a negative number

                        if fstrQuote != '' and tok.type == 'STRING' and tok.value[0]==fstrQuote:
                            # if string is in fstring, make sure quotes don't match/conflict
                            tmp = '"' if fstrQuote == "'" else "'"
                            tok.value = tok.value.replace(fstrQuote, tmp, 1)
                            tok.value = tok.value[:tok.value.rfind(fstrQuote)] + tmp

                        if lexIndex-1 > 0 and lexIndex+1 <= len(lex) \
                            and lex[lexIndex-1].type == 'TYPE':
                            if lexIndex+3 <= len(lex) and lex[lexIndex+2].type in typeAssignables and lex[lexIndex+1].type == 'ASSIGN':
                                storeVar(tok,lex[lexIndex+2],lex[lexIndex+3],lex[lexIndex-1],position=lexIndex+2)
                            elif lex[lexIndex+1].type in typeAssignables:
                                storeVar(tok,lex[lexIndex+1],lex[lexIndex+2],lex[lexIndex-1],position=lexIndex+1)
                            else:
                                storeVar(tok,None,lex[lexIndex+1],lex[lexIndex-1])

                        if lexIndex+1 <= len(lex) and lex[lexIndex+1].type == 'LISTCOMP' and bracketScope<1:
                                # myList : x
                                # is setting myList as the iterable
                                tval=None # tval seems to be for range()
                                if tok.type == 'ID' \
                                and tok.value in storedVars \
                                and storedVars[tok.value].type == 'NUMBER':
                                    tval=storedVars[tok.value].value
                                elif tok.type == 'ID' and tok.value in storedVarsHistory \
                                and storedVarsHistory[tok.value]['type'] == 'NUMBER':
                                    if 'value' in storedVarsHistory[tok.value]:
                                        tval=storedVarsHistory[tok.value]['value']
                                    else: tval=tok.value
                                elif tok.type == 'NUMBER':
                                    tval=tok.value
                                else:
                                    listcomp['list']=tok.value

                                if tok.type in ('ID','NUMBER') and tval != None:
                                    if '-:' in lex[lexIndex+1].value:
                                            listcomp['list']=f'range({tval},0,-1)'
                                    elif lex[lexIndex+1].value[0] != ':': # custom step ie   value myList 2: x
                                        if lex[lexIndex+1].value[0] == '-':
                                            listcomp['list']=f'range({tval},0,{lex[lexIndex+1].value.split(":")[0]})'
                                        else:
                                            listcomp['list']=f'range(0,{tval},{lex[lexIndex+1].value.split(":")[0]})'
                                    else:
                                        listcomp['list']=f'range({tval})'
                        elif autoEnumerate and ((tok.value in storedVarsHistory and 'type' in storedVarsHistory[tok.value] and storedVarsHistory[tok.value]['type'] in ('LIST','DICT','LISTCOMP')) or tok.type == 'DICT')  \
                        and lexIndex-3 >= 0 and lex[lexIndex-1].type == 'INS' and lex[lexIndex-2].type == 'COMMAGRP' and inLoop[0]==True:
                            # enumerate in for loop like  for index,value in myList
                            if tok.type in 'DICT' or ('type' in storedVarsHistory[tok.value] and storedVarsHistory[tok.value]['type'] == 'DICT'):
                                if lex[lexIndex-2].value.count(',') > 1:
                                    line.append(f'enumerate({tok.value}.items())')
                                else:
                                    line.append(f'{tok.value}.items()')
                            else: line.append(f'enumerate({tok.value})')
                        elif tok.type == 'ID' and ': ' in tok.value: # for type declaration
                            line.append(decideIfIndentLine(indent,f'{tok.value} '))
                            startOfLine=False
                            tok.value=tok.value.split(': ')[0]
                            if lexIndex+1 < len(lex) and lex[lexIndex+1].type in typeAssignables:
                                if lex[lexIndex-1].value in convertType: lex[lexIndex-1].value=convertType[lex[lexIndex-1].value]
                                storedVarsHistory[tok.value]={'value': lex[lexIndex+1].value, 'type': lex[lexIndex-1].value}
                        elif startOfLine and inIf == False:
                            if tok.type == 'ID' and ((tok.value in storedCustomFunctions) or (tok.value in pyBuiltinFunctions and tok.value not in storedVarsHistory and tok.value not in typeTypes and lex[lexIndex + 1].type in typeNewline + typeOperators)) \
                            and '(' not in tok.value and lastType != 'FOR' and functionPassing == False:
                                tok.type = 'FUNCTION' ; tok.value += '(' ; parenScope+=1
                                lex.insert(lexIndex+1,makeToken(tok,')','RPAREN'))

                            doPrint=False
                            if tok.type == 'STRING' and (tok.value.startswith('"""') or tok.value.startswith("'''")):
                                if indent==0 and lastType in typeNewline and lex[lexIndex+1].type in typeNewline and not any(t for t in keepAtTop if t.type == 'STRING'):
                                    keepAtTop.append(tok)
                                else:
                                    line.append(decideIfIndentLine(indent,tok.value))
                            elif lexIndex+1 <= len(lex) and tok.type != 'LISTEND':
                                if lex[lexIndex-1].type == 'CONSTANT' and lex[lexIndex + 1].type in typeNewline:
                                    return AS_SyntaxError('constant requires assignment',f'{lex[lexIndex - 1].value} {tok.value} = 12', lineNumber, data)
                                elif lex[lexIndex+1].type in typeNewline+typeConditionals+('TRY','ELSE') and tok.type != 'FUNCTION' and not fstrQuote:
                                    doPrint=True
                                elif lexIndex-1 >= 0 and ((lex[lexIndex-1].type in typeNewline+('TRY',) and not fstrQuote) or (lexIndex-3>0 and lex[lexIndex-3].type=='LOOP') or (lex[lexIndex-1].type == 'DEFFUNCT' or (lex[lexIndex-1].type == 'TYPE' and lex[lexIndex-2].type == 'DEFFUNCT')) or (lex[lexIndex-1].type == 'ELSE' and lex[lexIndex-2].type in typeNewline) ):
                                    tmp=rParen
                                    rParen+=1
                                    tmpHaveSeenOperator=False
                                    for tmpi in range(lexIndex,len(lex)-1):
                                        if lex[tmpi].type in typeNewline+('FROM',): break
                                        elif (tmpi == lexIndex or tmpHaveSeenOperator) and lex[tmpi].type == 'FUNCTION':
                                            pass
                                        elif lex[tmpi].type not in typePrintable:
                                            rParen-=1 ; break
                                        elif lex[tmpi].type == 'RINDEX' and lex[tmpi+1].type == 'ID':
                                            rParen-=1 ; break
                                        elif tmpi-1 == lexIndex and lex[lexIndex].type == 'ID' and lex[tmpi].type in typeAssignables+('INDEX','LPAREN'):
                                            rParen-=1 ; break
                                        if metaDefaultExpressionWithFunction and lex[tmpi].type in typeOperators:
                                            tmpHaveSeenOperator=True
                                    if tok.type == 'FUNCTION' and not tmpHaveSeenOperator:
                                        rParen-=1
                                    if rParen == tmp or listScope > 0 or (lex[lexIndex].type == 'FUNCTION' and not tmpHaveSeenOperator): # normal
                                        line.append(f'{" "*(indent)}{tok.value} ')
                                    else: # expression which can be print (no assignment)
                                        line.append(decideIfIndentLine(indent,f'{expPrint[-1]}('))
                                        line.append(tok.value)
                                        bigWrap=True
                                else:
                                    line.append(decideIfIndentLine(indent,tok.value+' '))
                            elif lexIndex == len(lex)-1 or (lexIndex+1 == len(lex)-1 and lex[lexIndex+1].type == 'NEWLINE'):
                                doPrint=True # end of script
                            else:
                                line.append(f'{" "*(indent)}{tok.value} ')
                            if doPrint:
                                if compileTo != 'Cython' or (compileTo=='Cython' and (expPrint[-1]!='print' \
                                or (tok.value in storedVarsHistory and 'type' in storedVarsHistory[tok.value] and storedVarsHistory[tok.value]['type'] == 'FSTR'))): # cythonPrint no likely multi token yet :(
                                        line.append(f'{" "*(indent)}{expPrint[-1]}({tok.value})')
                                elif compileTo == 'Cython':
                                    cythonPrint(tok)
                            startOfLine=False
                        else:
                            if lastType == 'ASSIGN' and lexIndex+3 < len(lex) and parenScope == 0:
                                if lex[lexIndex-2].type == 'COMMAGRP':
                                    # var1 , var2 = 12, 'thing'
                                    # ^^ handling typing based on comma position. var1 should be 12 and var2 should be 'thing'
                                    tmpVars=lex[lexIndex-2].value.split(',')
                                    tmpVars=[t.strip() for t in tmpVars]
                                    tmpf=[] ; tmp=[]
                                    for tmpi in range(lexIndex,len(lex)-1):
                                        #print('&^^',lex[tmpi],tmpf)
                                        if lex[tmpi].type in typeNewline:
                                            if tmp != []: tmpf.append(tmp)
                                            break
                                        elif lex[tmpi].type == 'COMMA':
                                            tmpf.append(tmp) ; tmp=[]
                                        else:  tmp.append(lex[tmpi])
                                    if tmpf != []:
                                        for t in range(0,len(tmpf)):
                                            if len(tmpf[t]) > 1:
                                                storeVar(makeToken(tok,tmpVars[t],'ID'), tmpf[t][0], tmpf[t][1])
                                            else: storeVar(makeToken(tok,tmpVars[t],'ID'), tmpf[t][0], makeToken(tok,'then','THEN'))
                                    else: storeVar(lex[lexIndex-2],lex[lexIndex],lex[lexIndex+1],position=lexIndex)
                                else:
                                    if not (lex[lexIndex-2].value in storedVarsHistory and 'staticType' in storedVarsHistory[lex[lexIndex-2].value]):
                                        # regular  var = 12
                                        storeVar(lex[lexIndex-2],lex[lexIndex],lex[lexIndex+1],position=lexIndex)

                            if lexIndex+1 <= len(lex)-1:
                                if tok.type == 'ID' and (lex[lexIndex+1].type == 'BUILTINF' or fstrQuote != ''):
                                    line.append(tok.value)
                                else:
                                    line.append(tok.value+' ')
                            else:
                                line.append(tok.value+' ')
                            startOfLine=False
            elif tok.type == 'LISTCOMP': # idLISTCOMP
                if lex[lexIndex+1].type == 'ASSIGN' or (lex[lexIndex+1].type == 'BUILTINF' and lex[lexIndex+2].type == 'ASSIGN'):
                    # not a list comp, but a type assign
                    line.append(decideIfIndentLine(indent,tok.value))
                    tok.type='ID' ; tok.type = lex[lexIndex-1].type
                elif bracketScope > 0:
                    # not a list comp, but inside of a dict
                    tmp=tok.value.lstrip(':')
                    tok.type = 'COLON' ; tok.value = ':'
                    line.append(': ')
                    listcomp={}
                    for i in miniLex(tmp+' '):
                        lex.insert(lexIndex+1,i)
                elif lex[lexIndex+1].type == 'LIST' and lastType in typeNewline: line.append(decideIfIndentLine(indent,tok.value)) # Python nested typing
                else:
                    if compileTo == 'Cython' and lexIndex-2 > 0 and lex[lexIndex-2].type in typeNewline+('DEFFUNCT',):
                        line.append(decideIfIndentLine(indent,'AStempVar = '))
                    listcomp["x"]=tok.value.split(':')[-1]
                    if 'list' not in listcomp:
                        if lastType == 'BUILTINF' \
                        and lexIndex - 2 > len(lex)-1\
                        and lex[lexIndex-2].type != 'ASSIGN':
                            listcomp['list']=line[-2]+line[-1]
                            line=line[:-2]
                        elif findEndOfFunction(lexIndex-1,goBackwards=True)!=False \
                        and tok.value[0] != ':': # custom step ie   value myList 2: x
                            tmpi=[x for x in range(0,len(line)-1) if lex[findEndOfFunction(lexIndex-1,goBackwards=True)].value in line[x]][0]
                            tval=''.join(line[tmpi:])
                            line=line[:tmpi]
                            if lex[lexIndex+1].value[0] == '-':
                                listcomp['list']=f'range({tval},0,{tok.value.split(":")[0]})'
                            else:
                                listcomp['list']=f'range(0,{tval},{tok.value.split(":")[0]})'
                        else:
                            if len(line)==0: return AS_SyntaxError(f'list comprehension requires iterable','myList : x to print\n\t# or\n\t99 : x',lineNumber,data)
                            if line[-1] == ')':
                                listcomp['list']=''.join(line)
                                line=[]
                            else:
                                listcomp['list']=line[-1]
                                line=line[:-1]
                    else:
                        pass
                        if debug: print('listcomp pizza',listcomp)
                    if startOfLine: line.append(f'{" "*indent}[') ; startOfLine=False
                    else: line.append('[')
            elif tok.type == 'MATCH': # idMATCH
                if switchCase['case']: switchCases.append(switchCase)
                if fstrQuote!='':
                    if 'match' in storedVarsHistory:
                        tok.type='ID' ; line.append('match ') ; continue
                    else:
                        return AS_SyntaxError(f'match is not allowed inside f-string',None, lineNumber, data)
                if lexIndex+2 <= len(lex)-1 and lex[lexIndex+1].type in typeAssignables+('INDEX','BUILTINF','LPAREN'):
                    tmpHideMatch = True
                    if pythonVersion >= 3.10:
                        for tmpi in range(lexIndex+1,len(lex)-1):
                            # find out if of or case
                            if lex[tmpi].type == 'OF':
                                if 'case' in lex[tmpi].value:
                                    tmpHideMatch = False
                                else: break
                    if tmpHideMatch:
                        if lex[lexIndex+2].type == 'INDEX':
                            lex[lexIndex+1].value=lex[lexIndex+1].value+lex[lexIndex+2].value
                            lex[lexIndex+1].type='IGNORE'
                    else:
                        line.append(decideIfIndentLine(indent, 'match '))
                        indentSoon = True ; indent += prettyIndent
                    tmpf=[]
                    for tmpi in range(lexIndex+1,len(lex)-1):
                        if lex[tmpi].type in typeNewline:
                            if not tmpHideMatch and lex[tmpi].type == 'NEWLINE':
                                lex[tmpi].type = 'THEN'
                            break
                        else:
                            tmpf.append(copy(lex[tmpi]))
                            if tmpHideMatch:
                                lex[tmpi].type='IGNORE'
                    if tmpf[-1].type == 'ENDIF':
                        # ENDIF screws up OFs by making a TAB after, thus remove ENDIF if it exists
                        tmpf=tmpf[:-1]
                    if len(tmpf)==0:
                        return AS_SyntaxError('match is not provided a statement.','match variable', lineNumber, data)
                    switchCase={'case':True,'var':tmpf,'firstIf':True,'type':'of'}
                    if not tmpHideMatch:
                        switchCase['type']='case'
                        switchCase['indent']=indent
                elif lex[lexIndex-1].type != 'FOR':
                    return AS_SyntaxError('match is not provided a statement.','match variable',lineNumber,data)
            elif tok.type == 'IGNORENL':
                ignoreNewline=True
            elif tok.type in typeNewline and tok.type != 'END': # idNEWLINE
                if parenScope>0 and tok.type=='TAB': tok.type='IGNORE' ; continue
                # ^ allows tabs inside of parenthesis to be ignored
                if bracketScope>0 and tok.type in {'TAB','NEWLINE'}: tok.type='IGNORE' ; continue
                # ^ allows tabs inside of brackets to be ignored
                if fstrQuote != '':
                    if tok.type != 'THEN':
                        tok.type = 'IGNORE' ; continue
                    elif lastType == 'THEN' or lex[lexIndex+1].type == 'THEN':
                        tok.type = 'IGNORE' ; continue
                    elif not tenary:
                        tok.value='}{' ; line.append(tok.value) ; lastType=tok.type='FSTR' ; continue

                if lexIndex+1 < len(lex) and lex[lexIndex+1].type in {'AND','OR'}:
                    # allows for multiline conditional
                    continue

                if tok.type == 'THEN' and not ('list' in listcomp and 'x' in listcomp):
                    if indentSoon and parenScope == 0 and ':' not in line[-1] :
                        if lexIndex+1 < len(lex) and lex[lexIndex+1].type == 'TAB' and 'while' in line[0]:
                            continue
                        else: line.append(':\n')
                    else:
                        line.append('\n')

                if anyCheck != 'none':
                    return AS_SyntaxError(f'{anyCheck} requires an are or arent.',f'if {anyCheck} False,True,False are True', lineNumber, data)


                if len(code)==0: code.append(''.join(line)) ; line=[]
                if tok.type == 'TAB' and (lastType not in ('THEN','DEFFUNCT','ENDIF') or (not code[-1].endswith(':\n') and lastType != 'ENDIF')) and indentSoon and inReturn==False:
                    line.append(':\n')
                elif tok.type == 'THEN' and indentSoon and lastType not in typeNewline+('DEFFUNCT','ENDIF') and parenScope<=0 and bracketScope<=0 and listScope<=0 and (line and line[-1].endswith(':\n')==False):
                    if line and line[-1] == '\n': line = line[:-1]
                    line.append(':\n') ; indentSoon=False


                if ignoreNewline and tok.type == 'NEWLINE':
                    ignoreNewline=False
                elif lexIndex+1 <= len(lex)-1 and lex[lexIndex+1].type in ('TAB','ELSE'):
                    startOfLine=True
                elif 'list' in listcomp and 'x' in listcomp:
                    if listcomp['list'].split('[')[0] in storedVarsHistory:
                        tmpName=listcomp['list'].split('[')[0]
                        if storedVarsHistory[tmpName]['type'] == 'NUMBER' and 'value' in storedVarsHistory[tmpName]:
                            listcomp['list']=f'range(0,{storedVarsHistory[tmpName]["value"]})'
                        elif 'staticType' in storedVarsHistory[tmpName] and storedVarsHistory[tmpName]['staticType'] == 'int':
                            listcomp['list'] = f'range(0,{tmpName})'
                    if len(line) > 0 and line[-1] == '[':
                        line.append(f'{listcomp["x"]} for {listcomp["x"]} in {listcomp["list"]} ]')
                    elif 'if' in ''.join(line) and 'else' not in ''.join(line):
                        tmpcheck=[i for i in range(0,len(line)-1) if 'if' in line[i]][0]
                        line.insert(tmpcheck, f' for {listcomp["x"]} in {listcomp["list"]} ')
                        line.append(' ]')
                    else:
                        if lex[lexIndex-2].type == 'LISTCOMP' and lex[lexIndex-1].type in typeAssignables and lex[lexIndex-1].value[0] in 'fF' == False\
                        and (lex[lexIndex-3].type == 'NUMBER' or (lex[lexIndex-3].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-3].value]['type']=='NUMBER')):
                                # cool optimization, faster. returns list*number instead of list-comp
                                line=line[:-2]
                                line.append(f'[{lex[lexIndex-1].value}]*{lex[lexIndex-3].value}')
                        else: line.append(f' for {listcomp["x"]} in {listcomp["list"]} ]')
                    if line[0] == '[': line=f'{" "*(indent*prettyIndent)}{"".join(line)}'
                    listcomp={}
                    startOfLine=True

                if bigWrap: # for putting things at the end
                    endOfLineChores(tok)

                if tok.type == 'THEN' and ':' not in tok.value:
                    if fstrQuote!='' and tenary: pass
                    elif indent!= 0:
                        tok.value=f"\n{' '*(storedIndents[-1] if storedIndents!=[] else indent)}" ; tok.type='TAB'
                    # a THEN takes the current indent and turns into a TAB

                # vv indent magic
                if tok.type == 'TAB':
                    check=True



                    if lexIndex+3 < len(lex) \
                    and lex[lexIndex+3].type in ('ELSE','ELIF'): check=False
                    elif lastType == 'THEN' and lexIndex-2 > 0 \
                    and lex[lexIndex-2].type == 'OF': check=False
                    elif lex[lexIndex+1].type == 'THEN': check=False
                    elif lastType == 'ELSE': check=False
                    elif lexIndex < len(lex) - 1 and switchCase['case'] and lex[lexIndex + 1].type == 'OF' and 'indent' in switchCase:
                        check = False ; indent = switchCase['indent']
                    if lastType=='FUNCTION' and lex[lexIndex-1].value[-1]=='(': check=False
                    if check:
                        tmpcheck=tok.value.replace('\t',' ').count(' ')
                        if debug: print('detected:',tok.value.replace('\t','z').replace(' ','x').replace('\n',''))

                        # QUARINTINE

                        oldIndent=indent

                        while tmpcheck % 2 != 0: tmpcheck+=1 # if odd, make even
                        if storedIndents==[]: storedIndents=[0]

                        if switchCase['case'] and not indentSoon and 'indent' in switchCase and tmpcheck <= switchCase['indent']-prettyIndent and lex[lexIndex+1].type != 'OF':
                            switchCase = {'case': False}
                            if switchCases: switchCase = switchCases.pop()
                        if switchCase['case'] and switchCase['type'] == 'case' and 'addExtraIndent' in switchCase and switchCase['addExtraIndent']:
                            tmpcheck=tmpcheck+prettyIndent


                        while tmpcheck < storedIndents[-1]:
                            if len(storedIndents)>1: storedIndents.pop()
                            else: break
                        if not storedIndents: storedIndents=[0]

                        indent=storedIndents[-1]

                        if indentSoon and lastIndent[2] and indent < lastIndent[2][-1]:
                            # after a conditional if the indent is lower, we can assume it was meant to be indented
                            indent+=prettyIndent

                        if debug: print(storedIndents)


                        if debug: print(f"detected={tmpcheck} lastDetected={lastIndent[0]} old={oldIndent} new={indent}")
                        # ^ NEW MAGIC
                        if indent < 0: indent=0


                        if '\n' in tok.value and (lexIndex < len(lex)-1 and lex[lexIndex+1].type !='THEN'):
                            if isinstance(line,str): line=[line] # so weird
                            line.append('\n')
                        lastIndent=[tmpcheck,indent,lastIndent[2],lastIndent[3],lastIndent[4]]
                else:
                    if indent > 0 and tok.type == 'NEWLINE' and notInDef and not ignoreIndentation and not indentSoon:
                        indent=0
                    if notInDef==False and indent==0: indent+=prettyIndent
                    if ( (tok.type == 'NEWLINE' and (len(line)==0 or line[-1].endswith(':\n')==False) ) \
                    or (tok.type=='THEN' and ':' in tok.value and lexIndex+1 < len(lex) and lex[lexIndex+1].type!='TAB') ) \
                    and inIf == True and inReturn==False:
                        if len(line)==0 or line[-1].endswith(':\n') == False:
                            line.append(':\n')
                        elif lexIndex+1 < len(lex) and lex[lexIndex+1].type=='NEWLINE':
                            # when doing multi line conditionals on 0 indent, : at the end seems to break the auto-assume-indent. this is due to the NEWLINE token being after THEN token with value of :
                            lex[lexIndex+1].type='IGNORE'


                if lastIndent[2]!=[] and indent<lastIndent[2][-1]:
                    lastIndent[2].pop()

                if indent<inLoop[1] or indent==0:
                    inLoop[0]=False
                if indent<lastIndent[4]:
                    lastIndent[4]=0

                if combineLines:
                    if isinstance(line,str): line=[line] # so weird
                    line.insert(0,code[-1])
                    code.pop()
                    combineLines=False


                tenary=False
                inFuncArg=False
                inIf=False
                inReturn=False
                startOfLine=True
                if lastType != 'THEN': indentSoon=False
                hasPiped=False
                lastType=tok.type
                lastValue=tok.value

                while len(comments)>0 and startOfLine and ((not optimize and comments[0][1] <= lexIndex) or (optimize and comments[0][2] <= lineNumber)):
                    # comment handlingn
                    line.append(f'\n{comments[0][0]}\n')
                    comments.pop(0)

                code.append(''.join(line))
                line=[]

                if tok.type != 'TAB': continue
            elif tok.type == 'ASSIGN': #idASSIGN
                if pyIs == True and 'is' in tok.value:
                    line.append(decideIfIndentLine(indent,'is '))
                    tok.type='PYIS'
                elif tok.value[0] != ':' and parenScope > 0 and lex[lexIndex-3].type!='COMMA' and (lex[lexIndex-2].type in typeNewline or lex[lexIndex-2].value.startswith('\n ')):
                    line=line[:-1]
                    tmp="".join(line).count('(')-"".join(line).count(')')
                    return AS_SyntaxError(
                                f'You are missing a closing parenthesis',
                                f'equal number of ( and ) like:\n{("".join(line)+")"*tmp) if len(line) > 0 else "this( thing )"}'
                                , lineNumber if line else lineNumber-1 ,data)
                else:
                    safe=True
                    if optimize and optListPlusListToExtend and lastValue in storedVarsHistory and (
                            ('type' in storedVarsHistory[lastValue] and storedVarsHistory[lastValue]['type'] in ('LIST', 'LISTCOMP')) or (
                            'staticType' in storedVarsHistory[lastValue] and storedVarsHistory[lastValue]['staticType'] == 'list' )):
                        if '+' in tok.value or (lex[lexIndex+1].type == 'ID' and lex[lexIndex+1].value == lastValue and lex[lexIndex+2].type=='PLUS'):
                            if '+' not in tok.value:
                                lex[lexIndex + 1].type = lex[lexIndex + 2].type = 'IGNORE'
                            tok.type='LPAREN' ; rParen+=1 ; bigWrap=True ; tok.value='('
                            lex[lexIndex-1].type = 'BUILTINF' ; lex[lexIndex-1].value = lastValue+'.extend'
                            lastValue='(' ; lastType='LPAREN'
                            line[-1]=line[-1][:-1]+'.extend(' ; continue

                    if lastType == 'FUNCTION' and lex[lexIndex-1].value in storedCustomFunctions and '(' not in lex[lexIndex-1].value:
                        storedCustomFunctions.remove(lex[lexIndex-1].value) ; lex[lexIndex-1].type = 'ID'
                    if lastType in {'WHILE', 'IF', 'ELIF', 'ELSE', 'ASYNC', 'RETURN', 'SCOPE', 'BREAK', 'FOR', 'AND', 'OR'}:
                        return AS_SyntaxError(
                            f'Variable \"{lastValue}\" is a reserved keyword, use a different name.',
                            f'variableName {tok.value} {lex[lexIndex + 1].value}', lineNumber, data)
                    doElse=True
                    if (inIf or tenary or fstrQuote!='' or parenScope>0) and tok.value.strip() == 'is':
                        if lex[lexIndex+1].type == 'INS' and lex[lexIndex+1].value.startswith('not') and tok.value.startswith('is'):
                            # is not -> !=
                            line.append('!= ')
                            lex[lexIndex+1].type='IGNORE'
                        else:
                            # if a is 12 -> if a == 12
                            if tok.value.strip() == 'is' and ((lastType == 'STRING' and lex[lexIndex+1].type == 'NUMBER') or (lastType == 'NUMBER' and lex[lexIndex+1].type == 'STRING')):
                                tok.type = lastType ; tok.value = lastValue
                                lex.insert(lexIndex+1,makeToken(tok,'==','EQUAL'))
                            else:
                                line.append('== ')
                        doElse = False
                    elif lastType not in {'ID','INDEX','COMMAGRP','BUILTINF','RINDEX','LISTEND','RPAREN','META'} and findEndOfFunction(lexIndex-1,goBackwards=True)==False: # syntax error
                        if lastType in typeConditionals:
                            return AS_SyntaxError(
                            f'Line {lineNumber} variable assigns to invalid type, conditional.',
                            'variableName %s %s'%(tok.value,lex[lexIndex+1].value)
                            ,lineNumber,data)
                        elif lexIndex < len(lex) and lex[lexIndex+1].type in typeAssignables:
                            if (lex[lexIndex-2].type == 'IF' and lex[lexIndex-3].type == 'ASSIGN') \
                            or code[-1].endswith('= '):
                                line.append('==')
                            else:
                                if iskeyword(lastValue.strip()):
                                    tmp=f'Line {lineNumber} assigns to reserved Python keyword: {lastValue.strip()}'
                                else:
                                    if lastType == 'INTOED': tmpf=lex[lexIndex-2].value
                                    else: tmpf=lastValue
                                    tmp=f'Line {lineNumber} assigns with invalid token: \"{tmpf}\".'
                                return AS_SyntaxError(tmp,'variableName %s %s'%(tok.value.replace(' ',''),lex[lexIndex+1].value),lineNumber,data)
                        elif lex[lexIndex+1].type == 'FSTR' and fstrQuote != '' and pythonVersion >= 3.08:
                            line.append('= ')
                        else:
                            return AS_SyntaxError(f'Line {lineNumber} does not assign to anything.',
                            'variableName is 12'
                            ,lineNumber,data)
                        doElse = False
                    elif pythonVersion < 3.08 and ':' in tok.value:
                        return AS_SyntaxError(f'Walrus assignment not allowed when compiling to {compileTo if compileTo != "Python" else "Python"+str(pythonVersion)}',
                            'a is 12'
                            ,lineNumber,data)
                    elif pythonVersion < 3.08 and fstrQuote != '' and ':' not in tok.value:
                        tmpSafe=False
                        for ii in range(lexIndex-1,0,-1):
                            if lex[ii].type == 'FUNCTION': tmpSafe=True ; break
                            elif lex[ii].type == 'FSTR': break
                        if not tmpSafe:
                            tmp='}' ; tmpf=[]
                            for i in range(lexIndex,0,-1):
                                if lex[i].type in typeNewline: break
                                tmpf.append(lex[i].value)
                            tmpf=tmpf[::-1][:-1]
                            return AS_SyntaxError(
                                f'Assignment not allowed inside F-String when compiling to Python{str(pythonVersion)}',
                                f'{"".join(tmpf)}{tmp}{fstrQuote}'
                                , lineNumber, data)
                        else: doElse=True
                    if doElse:
                        if lastType == 'META' and lex[lexIndex-1].value.replace("$","").split('=')[0].strip() not in inlineReplace:
                            return AS_SyntaxError(
                                'Assignment to meta not allowed when meta is not defined as inline.',
                                f'$ def {lex[lexIndex-1].value.replace("$","")} = "something"'
                                , lineNumber, data)

                        if ':' not in tok.value:
                            tmpIndexScope=0
                            for tt in range(lexIndex-1,0,-1):
                                if lex[tt].type == 'COMMAGRP':
                                    try:
                                        for t in miniLex(lex[lexIndex-1].value.replace(',',' , ')):
                                            if t.type in {'NUMBER','STRING','PIPE','FUNCTION','LPAREN','LPAREN'}:
                                                # not a variable assign
                                                safe=False ; break
                                        if not safe: break
                                    except LexError:
                                        safe = False ; break
                                elif lex[tt].type in {'NUMBER','STRING','PIPE','FUNCTION','PIPEGO','LPAREN'} and tmpIndexScope == 0:
                                    safe=False ; break
                                elif lex[tt].type in typeNewline+('TYPE','CONSTANT')+typeConditionals:
                                    break
                                elif lex[tt].type in {'LINDEX','LIST'}: tmpIndexScope += 1
                                elif lex[tt].type in {'RINDEX','LISTEND'}: tmpIndexScope -= 1
                        else: safe=True

                        if safe:
                            if '=' in tok.value:
                                line.append(tok.value+' ')
                            elif lastType == 'FSTR' and 'is' in tok.value:
                                line.append('== ')
                            else:
                                line.append('= ')
                            try:
                                storedVars[lex[lexIndex-1].value]=lex[lexIndex+1]
                            except :
                                return AS_SyntaxError(f'Line {lineNumber} does not assign to anything.',
                                '%s is 12'%(lex[lexIndex-1].value if lex[lexIndex-1].type == "ID" else "variable")
                                ,lineNumber,data)
                            if ':' not in tok.value and not combineLines: # walrus := is fine to not seperate into another line
                                code.append(''.join(line)) ; line=[] ; combineLines=True
                        else: # function arg?
                            line.append('=')

                    if safe and inIf == False and tok.value in ('=','is ') and lexIndex+2 < len(lex) \
                    and (lex[lexIndex-1].value not in storedVarsHistory or 'staticType' not in storedVarsHistory[lex[lexIndex-1].value]) :
                        if lexIndex+3 < len(lex) and lex[lexIndex+2].type == 'LISTCOMP':
                            storeVar(lex[lexIndex-1],lex[lexIndex+2],lex[lexIndex+3],position=lexIndex+2)
                        else: storeVar(lex[lexIndex-1],lex[lexIndex+1],lex[lexIndex+2],position=lexIndex+1)
                    if safe and (tok.value[0] in {'+','-','**','*','/','//'} or lex[lexIndex+1].type in {'FUNCTION','BUILTINF'} )and lastType == 'ID' \
                    and lex[lexIndex-1].value in storedVarsHistory and 'value' in storedVarsHistory[lex[lexIndex-1].value]:
                        del storedVarsHistory[lex[lexIndex-1].value]['value'] # if value is modified, then we dont know the true value anymore unless its a static value
            elif tok.type in typeConditionals: # idCONDITIONIAL
                if debug: print(indent,lastIndent[2],f'tenary={tenary}')
                if listcomp or ('list' in listcomp and 'x' in listcomp): # for tenary conditionals inside listcomp
                    if lastType != 'THEN':
                        line.append(codeDict[tok.type]+' ')
                elif ((lastType in typeAssignables+('ASSIGN','FUNCTION','BUILTINF','LPAREN','RPAREN','BOOL','IGNORE','INDEX','COMMAGRP','FSTR','RETURN','RINDEX') and tok.value=='if') or tenary) and startOfLine == False:
                    tenary=True
                    # this section is for altered tenary
                    if lastType in {'ASSIGN','RETURN', 'LPAREN', 'FUNCTION'} or (lastType == 'FSTR' and fstrQuote!=''): # alias:  c is if True then a else b
                        search=False ; tmp=[]
                        for tmpi in range(lexIndex+1,len(lex)-1):
                            if lex[tmpi].type == 'ELSE': break
                            elif search:
                                tmp.append(lex[tmpi].value)
                                lex[tmpi].type=lex[tmpi].type='IGNORE'
                            elif lex[tmpi].type == 'THEN' and lex[tmpi+1].type in typeAssignables+('ASSIGN','FUNCTION','BUILTINF','LPAREN','FSTR'):
                                lex[tmpi].type=lex[tmpi].type='IGNORE'
                                search=True
                            elif lex[tmpi].type in typeNewline: break
                        if search:
                            line.append(' '.join(tmp)+' ')
                    if lastType in {'BUILTINF','RPAREN'}: tok.value=' '+tok.value
                    if fstrQuote!='': line.append(f' {tok.value} ')
                    else: line.append(tok.value+' ')
                else:
                    if tok.type in {'ELSE','ELIF','OF'}:
                        if debug and tok.type == 'OF': print(switchCase) # needs to be if, not elif
                        if tok.type == 'ELSE' and not tenary and listScope <= 0: # idELSE
                            if inIf and parenScope == 0:
                                return AS_SyntaxError(
                                    "Did not begin block after 'if' conditional.",
                                    "if True do 'thing' else 'stuff'",
                                    lineNumber, data)
                            tmpFoundIF=False ; tmpFoundReturn=False ; tmpListScope=0
                            for t in range(lexIndex,0,-1): # i guess another tenary detection of sorts
                                if lex[t].type == 'RETURN': tmpFoundReturn=True
                                if (lex[t].type == 'RETURN' and tmpFoundIF) or (lex[t].type == 'ASSIGN' and lex[t-1].type == 'ID' and tmpFoundIF):
                                    line.append(tok.value+' ') ; check=False ; break
                                elif lex[t].type in typeNewline:
                                    check = True ; break
                                elif lex[t].type == 'IF' and tmpListScope>=0: tmpFoundIF=True # tmpListScope would be negative if we're in list, due to going backwards
                                elif lex[t].type in {'LIST','LINDEX'}: tmpListScope+=1
                                elif lex[t].type in {'LISTEND','RINDEX'}: tmpListScope-=1
                                elif lex[t].type == 'INDEX': tmpListScope+=lex[t].value.count('[') ; tmpListScope-=lex[t].value.count(']')

                            if tmpFoundIF and check and lastType not in typeNewline:
                                startOfLine=True
                                if tmpFoundReturn: lex.insert(lexIndex+1,makeToken(tok,':','COLON'))
                            if not check: lastType='ELSE' ; continue

                            if inIf and not tenary and not startOfLine:
                                return AS_SyntaxError('You need to end your conditional expression.',f"if {''.join(line).replace('  ',' ')}do 'something'\n# or\n\tif {''.join(line).replace('  ',' ')}then 'something'\n# or\n\tif {''.join(line).replace('  ',' ')}\n\t\t'something'",lineNumber,data)

                            if bigWrap: endOfLineChores(tok)

                            line.append('\n')
                            if switchCase['case']==False \
                            and (lastType!='TAB' or (lastType=='TAB' and (len(lastIndent[2])>0 and lastIndent[0] > lastIndent[2][-1]))):
                                if lastIndent[2]!=[] and lastType!='END': indent=lastIndent[2][-1]
                                if lastIndent[2]!=[]: lastIndent[2].pop()
                                if lastIndent[2]==[]: lastIndent[2]=[0]
                                if indent > 0 and tenary == False: startOfLine=True
                            elif switchCase['case']==True and 'indent' in switchCase and indent == switchCase['indent']:
                                lastIndent[2].append(indent) # not so sure on this bit
                                switchCase['case']=False
                                if switchCases: switchCase=switchCases.pop()

                            line.append(decideIfIndentLine(indent,f'{codeDict[tok.type]}'))
                            if lex[lexIndex+1].type != 'ENDIF' and inIf == False and parenScope==0:
                                line[-1]+=':\n'
                            startOfLine=True
                            if combineLines:
                                if isinstance(line,str): line=[line] # so weird
                                line.insert(0,code[-1])
                                code.pop()
                                combineLines=False
                            if inIf==False: code.append(''.join(line)) ; line=[]
                            else: indentSoon=startOfLine=False ; line[-1]+=' ' ; inIf=True
                        elif tok.type == 'OF' and switchCase['case']: # idOF
                            if pythonVersion >= 3.10 and 'case' in tok.value:
                                if switchCase['firstIf']:
                                    if switchCase['indent']-prettyIndent == indent:
                                          switchCase['addExtraIndent'] = True
                                    else: switchCase['addExtraIndent'] = False
                                    switchCase['firstIf']=False
                                indent = switchCase['indent']
                                line.append(decideIfIndentLine(indent,codeDict[tok.type]+' '))
                                indentSoon=True
                            elif 'case' in tok.value:
                                tmp=[]
                                for tmpi in range(lexIndex+1,len(lex)-1):
                                    if lex[tmpi].type in typeNewline:
                                        break
                                    else:
                                        tmp.append(lex[tmpi].value)
                                tmp=' '.join(tmp)
                                return AS_SyntaxError("'case' statement only allowed in Python version 3.10 and up.\n\tUse 'of' statement instead.",
                                                      f"match {' '.join([i.value for i in switchCase['var']])}\n\t\tof {tmp}\n\t\t\t'something'",
                                                      lineNumber, data)
                            else:
                                if lastType not in typeNewline:
                                    return AS_SyntaxError(
                                        f'{switchCase["type"]} keyword needs line end syntax (tab, newline, then) then expression, not another {switchCase["type"]}',
                                        f'{switchCase["type"]} 12 do "my fav num"', lineNumber, data)
                                if comment: line.append(f'{" "*indent}# of\n')
                                if switchCase['firstIf'] == True:
                                    line.append(decideIfIndentLine(indent,'if '))
                                    switchCase['firstIf']=False
                                    switchCase['indent'] = indent
                                else:
                                    switchCase['indent'] = indent
                                    line.append(decideIfIndentLine(indent,'elif '))
                                [lex.insert(lexIndex+1,tmptok) for tmptok in reversed(switchCase["var"])]
                                if lex[lexIndex+len(switchCase["var"])+1].type in typeAssignables:
                                    lex.insert(lexIndex+len(switchCase["var"])+1,makeToken(tok,'==','EQUAL'))
                                indentSoon=True
                                startOfLine=False
                                if lastIndent[2] and lastIndent[2][-1] < indent:
                                    lastIndent[2].append(indent)
                                elif lastIndent[2] and lastIndent[2][-1] > indent:
                                    # if last conditional is greater indent than current of, we can remove it
                                    lastIndent[2].pop()
                                if debug: print('>',indent,switchCase['indent'],line)
                        elif tok.type == 'OF' and switchCase['case'] == False: return AS_SyntaxError('match case needs match statement','match myVar\n\tof "some" do 1\n\tof "thing" do 2',lineNumber,data)
                        else:
                            if tok.type == 'ELIF' and lastType not in typeNewline:
                                tmpi=1
                                while -1 < lexIndex-tmpi < len(lex): # im kinda questioning what this is for
                                    if lex[lexIndex-tmpi].type == 'ELSE':
                                        indent-=prettyIndent ; break
                                    elif lex[lexIndex-tmpi].type == 'IF': break
                                    tmpi+=1
                                    # vv could be dubious , but solves it when using then vv
                                    indent-=prettyIndent
                                    if indent<0: indent=0
                            if tenary:
                                line.append(codeDict[tok.type])
                            else:
                                if lastIndent[2] and indent > lastIndent[2][-1]:
                                    indent=lastIndent[2][-1]
                                line.append(f'\n{" "*indent}{codeDict[tok.type]} ')
                                indentSoon=True
                        indent+=prettyIndent
                        if indent <= 0:
                            indent=prettyIndent
                    elif startOfLine:
                        line.append(f'{" "*indent}{codeDict[tok.type]} ')
                        startOfLine=False
                        if lexIndex+1 <= len(lex)-1 and lex[lexIndex+1].type == 'TAB':
                            pass
                        else:
                            indent+=prettyIndent
                            indentSoon=True
                        code.append(''.join(line)) ; line=[] ; combineLines=True
                    else:
                        line.append(codeDict[tok.type]+' ')
                        if tenary==False: indentSoon=True
                    if tok.type == 'WHILE':
                        if not inLoop[0]:
                            inLoop=[True,indent]
                    if tok.type != 'ELSE':
                        inIf=True
                        startOfLine=False
                    if tok.type in {'IF','WHILE'}:
                        if indent >= prettyIndent:
                            while lastIndent[2] and lastIndent[2][-1] <= indent - prettyIndent:
                                lastIndent[2].pop()
                            lastIndent[2].append(indent-prettyIndent)
                        else: lastIndent[2].append(indent)
                        if debug: print(indent,lastIndent[2])
                if startOfLine==False and lastType=='INDEX': line[-1]=f" {line[-1]}"
            elif tok.type == 'FOR': #idFOR
                if   lex[lexIndex+1].value in storedVarsHistory: # reassigns
                    del storedVarsHistory[lex[lexIndex+1].value]
                elif lex[lexIndex+1].value in ASnakeKeywords: reservedIsNowVar.append(lex[lexIndex + 1].value.strip())
                tmpFound = False
                if optimize and optLoopToMap and lastType in typeNewline:
                    tmpFound=optLoopToMap_Function(lexIndex)
                if not optimize or not tmpFound:
                    if 'list' in listcomp and 'x' in listcomp:
                        line.append('for ')
                    elif startOfLine:
                        if lastType == 'LINDEX':
                            if lex[lexIndex+1].type == 'ID': tmp=lex[lexIndex+1].value
                            else: tmp='x'
                            return AS_SyntaxError('When doing Python list comprehensions, you need a variable before the \'for\'.', f'[ {tmp} for {tmp} in range(12) ]', lineNumber, data)
                        if compileTo == 'Cython' and optimize and lastType in typeNewline and lex[lexIndex+1].type == 'ID' and not inLoop[0] and not lastIndent[4] and lex[lexIndex+1].value not in storedVarsHistory and lastIndent[2] == []:
                            check=True
                            if lex[lexIndex+2].type == 'INS' and lex[lexIndex+3].value in storedVarsHistory and 'type' in storedVarsHistory[lex[lexIndex+3].value] and storedVarsHistory[lex[lexIndex+3].value]['type'] in ('TUPLE','LIST','SET'):
                                check=False
                            if check:
                                line.append(decideIfIndentLine(indent, f'cdef Py_ssize_t {lex[lexIndex+1].value}\n'))
                                startOfLine = True
                                storedVarsHistory[lex[lexIndex+1].value]={'type':'Py_ssize_t'}

                        indentSoon=True
                        inIf=True
                        line.append(decideIfIndentLine(indent,f'{tok.value} '))
                        indent+=prettyIndent
                        if not inLoop[0]:
                            inLoop=[True,indent]
                    else:
                        line.append(decideIfIndentLine(indent,f'{tok.value} '))
            elif tok.type == 'LOOP': # idLOOP

                if lexIndex+2 < len(lex) and lex[lexIndex+2].type == 'INS':
                    # loop x in y  is just a for loop alias
                    tok.value='for' ; tok.type='FOR'
                    indentSoon = True ; inIf = True
                    line.append(decideIfIndentLine(indent, f'{tok.value} '))
                    indent += prettyIndent
                    if not inLoop[0]: inLoop = [True, indent]
                    continue
                elif lexIndex+2 < len(lex) and lex[lexIndex+2].type in typeOperators+('LPAREN','COMMA'):
                    return AS_SyntaxError('loop syntax must have no operations on it', 'loop iterable iterator', lineNumber, data)

                if lexIndex+3 >= len(lex) or lex[lexIndex+2].type == 'NEWLINE':
                    return AS_SyntaxError('"loop" needs something after it','loop 12 i',lineNumber,data)
                if lex[lexIndex+2].type == 'ID' and lex[lexIndex+2].value not in storedCustomFunctions and lex[lexIndex+2].value not in storedVarsHistory and (lex[lexIndex+2].value != 'print' or (lex[lexIndex+2].value == 'print' and 'print' in storedVarsHistory)) and lex[lexIndex+3].type not in typeOperators+('ASSIGN','PIPE'):
                    forthingin=lex[lexIndex+2].value ; lex[lexIndex+2].type='IGNORE'
                elif lex[lexIndex+2].type == 'ID' and lex[lexIndex+2].value in storedVarsHistory and ((lex[lexIndex+3].type == 'TAB' and lex[lexIndex+3].value.count(' ') > indent) or (lex[lexIndex+3].type == 'THEN')):
                    # if indented after loop, then we can assume we're trying to redefine this already assigned variable as a iterator variable
                    forthingin = lex[lexIndex + 2].value ; lex[lexIndex + 2].type = 'IGNORE'
                elif lex[lexIndex+2].type == 'INC' and '[' not in lex[lexIndex+2].value:
                    forthingin=lex[lexIndex+2].value.replace('--','').replace('++','')
                elif compileTo == 'Cython' and lex[lexIndex+2].value in storedVarsHistory and 'type' in storedVarsHistory[lex[lexIndex+2].value] and storedVarsHistory[lex[lexIndex+2].value]['type'] == 'Py_ssize_t':
                    forthingin=lex[lexIndex+2].value ; lex[lexIndex+2].type='IGNORE'
                else: forthingin='_'
                #print(lex[lexIndex+2].type == 'ID',compileTo=='Cython',lex[lexIndex+2].value not in storedVarsHistory,lex[lexIndex+2].value not in storedCustomFunctions , lex[lexIndex+3].type not in typeOperators)

                tmpFound = False
                if optimize and optLoopToMap:
                    tmpFound=optLoopToMap_Function(lexIndex, forLoop=False)

                if lexIndex+2 < len(lex) and not tmpFound:
                    if compileTo=='Cython' and forthingin != '_':
                        if lex[lexIndex+1].type == 'NRANGE':
                            if 'to' in lex[lexIndex+1].value:
                                if '-' in lex[lexIndex+1].value.split('to')[0]:
                                    tmp='int'
                                else: tmp='Py_ssize_t'
                            else:
                                if '-' in lex[lexIndex+1].value.split('.'*lex[lexIndex+1].value.count('.'))[0]:
                                    tmp='int'
                                else: tmp='Py_ssize_t'
                        else:
                            if lex[lexIndex+1].type == 'NUMBER' or (lex[lexIndex+1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+1].value]['type'] == 'NUMBER'):
                                tmp='Py_ssize_t'
                            else: tmp=None
                        if tmp!=None and lastIndent[2] == [] and all(True if f'cdef {tmp} {forthingin}\n' not in i else False for i in code) and forthingin not in storedVarsHistory:
                            line.append(decideIfIndentLine(indent,f'cdef {tmp} {forthingin}\n')) ; startOfLine=True
                            storedVarsHistory[forthingin]={'type':convertType[tmp],'staticType':tmp}

                    if lex[lexIndex+1].type == 'MINUS' and lex[lexIndex+2].type in ('ID','NUMBER','BUILTINF'):
                        lex[lexIndex+1]=copy(lex[lexIndex+2])
                        lex[lexIndex + 1].value='-'+lex[lexIndex+1].value
                        lex[lexIndex + 2].type='IGNORE'
                        del lex[lexIndex+2]

                    if lex[lexIndex + 1].type in ('ID', 'NUMBER') and lex[lexIndex + 2].type == 'PIPE' and (lex[lexIndex + 3].type == 'NUMBER' or lex[lexIndex + 3].value in storedVarsHistory and storedVarsHistory[lex[lexIndex + 3].value]['type'] == 'NUMBER'):
                        tmpf=[] ; needsOperator=False
                        for tmpi in range(lexIndex+3,len(lex)):
                            if lex[tmpi].type == 'MINUS' and lex[tmpi+1].type in ('ID','NUMBER','LPAREN','BUILTINF') and lex[tmpi-1].type in typeOperators:
                                tmpf.append(lex[tmpi].value);lex[tmpi].type = 'IGNORE'
                            elif lex[tmpi].type in ('ID','NUMBER','RPAREN','BUILTINF'): needsOperator=True ; lex[tmpi].type='IGNORE'
                            elif lex[tmpi].type in typeOperators and needsOperator:
                                tmpf.append(lex[tmpi].value) ; lex[tmpi].type='IGNORE' ; needsOperator=False
                            elif lex[tmpi].type in ('LPAREN','FUNCTION'):
                                tmpf.append(lex[tmpi].value) ; lex[tmpi].type='IGNORE'
                            elif lex[tmpi].type in ('LINDEX',):
                                tmpf.append(lex[tmpi].value);lex[tmpi].type = 'IGNORE';needsOperator = False

                            if needsOperator and lex[tmpi+1].type in typeOperators:
                                tmpf.append(lex[tmpi].value) ; lex[tmpi].type='IGNORE'
                            elif needsOperator and lex[tmpi+1].type in ('LPAREN','LINDEX','RINDEX','RPAREN'):
                                if lex[tmpi+1].type in ('LPAREN','LINDEX'): needsOperator=False
                                tmpf.append(lex[tmpi].value) ; lex[tmpi].type='IGNORE'
                            elif needsOperator and lex[tmpi+1].type not in typeOperators:
                                if lex[tmpi+1].type == 'ID':
                                    forthingin=lex[tmpi+1].value ; lex[tmpi+1].type='IGNORE'
                                tmpf.append(lex[tmpi].value) ; lex[tmpi].type='IGNORE'
                                break
                        lex[lexIndex+1].type=lex[lexIndex+2].type= 'IGNORE'
                        if compileTo == 'Cython' and forthingin != '_':
                            if lex[lexIndex+1].value[0] == '-': tmp='int'
                            else: tmp='Py_ssize_t'
                            if all(True if f'cdef {tmp} {forthingin}\n' not in i else False for i in code) and forthingin not in storedVarsHistory:
                                line.append(decideIfIndentLine(indent, f'cdef {tmp} {forthingin}\n'));
                                startOfLine = True
                                storedVarsHistory[forthingin] = {'type': tmp, 'staticType': tmp}
                        line.append(f'for {forthingin} in range({lex[lexIndex + 1].value}, {"".join(tmpf)}):')
                        # ^ handles 1 to 12 type of nrange
                    elif (((lex[lexIndex+1].type == 'NUMBER' or lex[lexIndex+1].value.isdigit()) and '.' not in lex[lexIndex+1].value and lex[lexIndex+3].type not in typeOperators) \
                    or (lex[lexIndex+1].type == 'ID' and lex[lexIndex+1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+1].value]['type'] == 'NUMBER') and lex[lexIndex+2].type not in typeOperators):
                        line.append(decideIfIndentLine(indent,f'for {forthingin} in range(0,{abs(int(lex[lexIndex+1].value)) if lex[lexIndex+1].value.isdigit() else lex[lexIndex+1].value}):\n'))
                    elif (lex[lexIndex+1].type == 'ID' and lex[lexIndex+1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+1].value]['type'] in ('LIST','LISTCOMP','DICT','STRING')) or  lex[lexIndex+1].type == 'FUNCTION' and 'range(' in lex[lexIndex+1].value:
                        line.append(decideIfIndentLine(indent,f'for {forthingin} in {abs(int(lex[lexIndex+1].value)) if lex[lexIndex+1].value.isdigit() else lex[lexIndex+1].value}:\n'))
                    elif lex[lexIndex+1].type == 'STRING':
                        line.append(decideIfIndentLine(indent,f'for {forthingin} in {lex[lexIndex + 1].value}:\n'))
                    elif lex[lexIndex+1].type == 'FUNCTION' and any(i for i in ('len','int')+tuple(x for x in storedCustomFunctions if storedCustomFunctions[x]['type'] == 'NUMBER') if lex[lexIndex+1].value.startswith(i)):
                        # if it can be determined function outputs number, accept it
                        if forthingin == '_' and lex[findEndOfFunction(lexIndex+2)+1].type == 'ID':
                            forthingin=lex[findEndOfFunction(lexIndex+2)+1].value
                            lex[findEndOfFunction(lexIndex+2)+1].type='THEN'
                        line.append(decideIfIndentLine(indent,f'for {forthingin} in range(0, '))
                        bigWrap=indentSoon=True ; rParen+=1 ; inLoop=[None,indent]
                        if (forthingin != '_' and lex[findEndOfFunction(lexIndex+2)+2].type in ('NEWLINE','THEN'))\
                        or (forthingin == '_' and lex[findEndOfFunction(lexIndex+2)+1].type in ('NEWLINE','THEN')):
                            inLoop.append(f'{expPrint[-1]}({forthingin})')
                        if forthingin == '_':
                            # need to insert a newline
                            tmptok=copy(tok) ; tmptok.type='THEN' ; tmptok.value=''
                            lex.insert(findEndOfFunction(lexIndex+2)+1,tmptok)
                            del tmptok
                        continue
                    elif lex[lexIndex+2].type == 'PIPE':
                        # same thing as above but for pipes
                        skip=True ; tmpi=1
                        while tmpi < len(lex):
                            if skip == False:
                                if lex[lexIndex+tmpi].type == 'PIPE': pass
                                else:
                                    tmpi-=1
                                    if any(i for i in ('len','int')+tuple(x for x in storedCustomFunctions if storedCustomFunctions[x]['type'] == 'NUMBER') if lex[lexIndex+tmpi].value == i):
                                        if forthingin == '_' and lex[lexIndex+tmpi+1].type == 'ID':
                                            forthingin=lex[lexIndex+tmpi+1].value
                                            lex[lexIndex+tmpi+1].type='THEN'
                                        line.append(decideIfIndentLine(indent,f'for {forthingin} in range(0, '))
                                        bigWrap=indentSoon=True ; rParen+=1 ; inLoop=[None,indent]
                                        if (forthingin != '_' and lex[lexIndex+tmpi+2].type in ('NEWLINE','THEN'))\
                                        or (forthingin == '_' and lex[lexIndex+tmpi+1].type in ('NEWLINE','THEN')):
                                            inLoop.append(f'{expPrint[-1]}({forthingin})')
                                        if forthingin == '_':
                                            # need to insert a newline
                                            tmptok=copy(tok) ; tmptok.type='THEN' ; tmptok.value=''
                                            lex.insert(lexIndex+tmpi+1,tmptok)
                                            del tmptok
                                        tmpi=len(lex)+2
                                    else:
                                        return AS_SyntaxError('"loop" needs a integer number or iterable, not a function pipe unless last function is known to output number','loop 12',lineNumber,data)
                                skip=True
                            else: skip=False
                            tmpi+=1
                        continue
                    elif lex[lexIndex+1].type == 'NRANGE':
                        if lex[lexIndex+2].type == 'NRANGE': tmpi=2
                        else: tmpi=1
                        # i copied this over from NRANGE check, bad!

                        if 'to' in lex[lexIndex+tmpi].value:
                            tmp=lex[lexIndex+tmpi].value.split('to')
                        else:
                            tmp=lex[lexIndex+tmpi].value.split('.'*lex[lexIndex+tmpi].value.count('.'))
                            if lex[lexIndex+tmpi].value.count('.') == 2:
                                if tmp[-1].isdigit(): tmp[-1]=int(tmp[-1])+1
                                else: tmp[-1]+='+1'

                        number1=number2=None
                        if tmp[0].isdigit():
                            number1=int(tmp[0])
                        elif tmp[0] in storedVarsHistory and storedVarsHistory[tmp[0]]['type']=='NUMBER':
                            if 'value' in storedVarsHistory[tmp[0]]:
                                number1=int(storedVarsHistory[tmp[0]]['value'])
                        if isinstance(tmp[-1],int) or tmp[-1].isdigit():
                            number2=int(tmp[-1])
                        elif tmp[-1] in storedVarsHistory and (storedVarsHistory[tmp[-1]]['type']=='NUMBER'):
                            if 'value' in storedVarsHistory[tmp[-1]]:
                                number2=int(storedVarsHistory[tmp[-1]]['value'])
                        if number1 != None and number2 != None and number1 > number2:
                            tmp[-1]+=', -1'

                        line.append(decideIfIndentLine(indent,f'for {forthingin} in range({tmp[0]},{tmp[-1]}):\n'))
                        lex[lexIndex+tmpi].type = 'IGNORE'
                    else:
                        tmpi=1 ; search=True ; noRange=False
                        tmpf=[]
                        # for when a iterable is supplied and not an ID
                        if forthingin == '_' and lex[lexIndex+1].type in ('LPAREN','LBRACKET','LIST'):
                            tmpscope=0
                            start=lex[lexIndex+1].type
                            convert={'LPAREN':'RPAREN','LBRACKET':'RBRACKET','LIST':'LISTEND'}
                            for tmpii in range(lexIndex+1,len(lex)):
                                tmpf.append(lex[tmpii].value)
                                if lex[tmpii].type == start: tmpscope+=1
                                elif lex[tmpii].type == convert[start]: tmpscope-=1
                                if lex[tmpii].type == convert[start]  and tmpscope == 0:
                                    if lex[tmpii+1].type == 'ID' and lex[tmpii+1].type not in typeOperators+typeCheckers:
                                        forthingin=lex[tmpii+1].value ; lex[tmpii+1].type='IGNORE'
                                    lex[tmpii].type = 'IGNORE'
                                    noRange=True ; search=False ; break
                                lex[tmpii].type = 'IGNORE'
                        # end
                        while tmpi < len(lex)-1 and search:
                            if lex[lexIndex+tmpi].type in ('NUMBER','INDEX','ID')+typeOperators:
                                if lex[lexIndex+tmpi].type == 'ID':
                                    if lex[lexIndex+tmpi-1].type not in typeOperators and tmpi>1:
                                        if lexIndex+tmpi+1 < len(lex) and lex[lexIndex+tmpi+1].type in typeOperators: search=False
                                        else:
                                            forthingin=lex[lexIndex+tmpi].value
                                            search=False ; lex[lexIndex+tmpi].type='IGNORE'
                                    else:
                                        if (lex[lexIndex+tmpi].value in storedVarsHistory and (storedVarsHistory[lex[lexIndex+tmpi].value]['type']=='NUMBER' or ('staticType' in storedVarsHistory[lex[lexIndex+tmpi].value] and storedVarsHistory[lex[lexIndex+tmpi].value]['staticType'] == 'int'))) \
                                        or (lex[lexIndex+tmpi].value.replace('[0]','') in storedVarsHistory and storedVarsHistory[lex[lexIndex+tmpi].value.replace('[0]','')]['type']=='NUMBER')\
                                        or (lex[lexIndex+tmpi].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+tmpi].value]['type'] in ('LIST','TUPLE','LISTCOMP','SET','COMMAGRP')+typeAssignables):
                                            if lex[lexIndex+tmpi].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+tmpi].value]['type'] in ('LIST','TUPLE','LISTCOMP','SET','COMMAGRP')+typeAssignables:
                                                noRange=True
                                            tmpf.append(lex[lexIndex+tmpi].value) ; lex[lexIndex+tmpi].type='IGNORE'
                                        else: return AS_SyntaxError('when passing expression to loop, first variable argument must be type assigned or inferred','int a is 12 then loop a i',lineNumber,data)
                                else:
                                    if tmpi==1 and '.' in lex[lexIndex+tmpi].value and lex[lexIndex+tmpi+1] not in ('NUMBER','INDEX','ID')+typeOperators: search=False
                                    elif tmpi>1 and (lex[lexIndex+tmpi-1].type == 'NUMBER' or lex[lexIndex+tmpi-1].value.isdigit()): search=False
                                    else: tmpf.append(lex[lexIndex+tmpi].value) ; lex[lexIndex+tmpi].type='IGNORE'
                            else:
                                if lex[lexIndex+tmpi].type in typeNewline and (lex[lexIndex+tmpi].type != 'TAB' and lex[lexIndex+tmpi+1].type not in typeNewline):
                                    return AS_SyntaxError('"loop" needs something after it',f'loop {"".join(tmpf)}{" "+forthingin if forthingin!="_" else ""} "Hi"',lineNumber,data)
                                search=False
                            tmpi+=1
                        if len(tmpf)==0: return AS_SyntaxError('"loop" needs a integer number or iterable','loop 12 i',lineNumber,data)
                        else:
                            if compileTo=='Cython' and forthingin != '_' and all(True if f'cdef Py_ssize_t {forthingin}\n' not in i else False for i in code):
                                line.append(decideIfIndentLine(indent,f'cdef Py_ssize_t {forthingin}\n')) ; startOfLine=True
                            if noRange:
                                line.append(decideIfIndentLine(indent,f'for {forthingin} in {"".join(tmpf)}:\n'))
                            else:
                                line.append(decideIfIndentLine(indent,f'for {forthingin} in range(0,{"".join(tmpf)}):\n'))
                lex[lexIndex+1].type='IGNORE' ; indent+=prettyIndent ; startOfLine=True
                if lex[lexIndex+2].value == forthingin and (lex[lexIndex+3].type in ('NEWLINE','TAB','FROM') and (lex[lexIndex+3].value in ('\n','from') or lexIndex+4==len(lex)-1)):
                    if compileTo=='Cython' and expPrint[-1]=='print' and f'cdef Py_ssize_t {forthingin}\n' in line:
                        insertAtTopOfCodeIfItIsNotThere('from libc.stdio cimport printf\n')
                        line.append(decideIfIndentLine(indent,f'printf("%ld\\n",{forthingin})\n'))
                    else:
                        line.append(decideIfIndentLine(indent,f'{expPrint[-1]}({forthingin})'))
                    indent-=prettyIndent ; startOfLine=True
                code.append(''.join(line)) ; line=[]
                if not inLoop[0]: inLoop = [True, indent]

            elif tok.type == 'ARE': #idARE
                if lexIndex-1 > 0:
                    if lexIndex+1 < len(lex)-1 :
                        if lex[lexIndex-1].type in ('NUMBER','BOOL'): # error if non iterable
                            return AS_SyntaxError('Your value seems to be non-iterable.',
                            'myList are equal',lineNumber,data)
                        elif anyCheck == 'none':
                            return AS_SyntaxError('You did not supply a all/any.',
                            f'any of {" ".join(line)} are False', lineNumber, data)

                        if tok.value == 'are': tmpcheck=True
                        else: tmpcheck=False
                        line=line[:-1]
                        if lex[lexIndex-2].type == 'ANYOF': tmpfirst=lex[lexIndex-1].value
                        else:
                            tmpfirst=[]
                            for tmpi in range(lexIndex-1,0,-1):
                                if lex[tmpi].type in typeNewline+('ANYOF',):
                                    try: line=line[:-line.index(lex[tmpi+1].value)]
                                    except (IndexError, ValueError): line=[]
                                    break
                                elif lex[tmpi].type == 'IGNORE': pass
                                else: tmpfirst.append(lex[tmpi].value)
                            tmpfirst='('+(''.join(reversed(tmpfirst)))+')'


                        # handle compare
                        if lex[lexIndex+1].type in typeCheckers+('INS',):
                            tmp=2 ; check = lex[lexIndex+1].value
                            if lex[lexIndex + 1].type == 'EQUAL' and lex[lexIndex+1].value.strip() != 'equal' and lex[lexIndex].value != 'are':
                                check = '!='
                            elif lex[lexIndex + 1].type in {'ID','EQUAL','NOTEQ'} and lex[lexIndex + 1].value.replace('not','').strip() == 'equal':
                                check = 'equal'
                                if 'not' in lex[lexIndex+1].value:
                                    if 'are' == lex[lexIndex].value: lex[lexIndex].value = 'arent'
                                    else: lex[lexIndex].value = 'are'
                                if 'are' == lex[lexIndex].value: tmpcheck=True
                                else: tmpcheck=False
                            elif lex[lexIndex + 1].type == 'NOTEQ' and lex[lexIndex].value != 'are':
                                check = '=='
                            elif lex[lexIndex + 1].type == 'INS' and check.strip() == 'not':
                                if lex[lexIndex+2].value.strip() == 'in':
                                    check = 'not in'
                                    lex[lexIndex+2].type = 'IGNORE'
                                    tmp=3
                                else:
                                    if 'are' == lex[lexIndex].value: check = '!='
                                    else: check = '=='
                        else:
                            tmp=1
                            if lex[lexIndex+1].type == 'BOOL':
                                # if _  vs   if _ == True
                                tmp = False
                                check = tmpcheck = ''
                                if lex[lexIndex+1].value == 'False':
                                    tmpcheck='== False'
                            else:
                                if lex[lexIndex + 1].type in {'ID','EQUAL'} and lex[lexIndex + 1].value.strip() == 'equal':
                                    check = 'equal'
                                    if 'are' == lex[lexIndex].value: tmpcheck=True
                                    else: tmpcheck=False
                                elif 'are' == lex[lexIndex].value:
                                   check = '=='
                                else: check = '!='

                        # handle object
                        if tmp:
                            tmpcheck = ''
                            for tt in range(lexIndex + tmp, len(lex) - 1):
                                if lex[tt].type in typeNewline + ('ENDIF','AND','OR'):
                                    break
                                else:
                                    tmpcheck += lex[tt].value
                                    lex[tt].type = 'IGNORE'
                            tmpcheck = f"({tmpcheck})"

                        # check is operator, tmpcheck is object
                        # like check ==     tmpcheck True
                        # tmpfirst is iterable
                        if lex[lexIndex+1].type in ('FUNCTION','BUILTINF') or (lex[lexIndex+1].type in ('IGNORE','NOTEQ','INS') and lex[lexIndex+2].type in ('FUNCTION','BUILTINF')):
                            tmpcheck='' ; tmpScope=0
                            for tmpi in range(lexIndex+(1 if lex[lexIndex+1].type in ('FUNCTION','BUILTINF') else 2),len(lex)-1):
                                if lex[tmpi].type in typeNewline: break
                                elif lex[tmpi].type in ('OR','AND') and tmpScope > 0: break
                                elif lex[tmpi].type == 'FUNCTION' and lex[tmpi].value=='(': tmpScope += 1
                                elif lex[tmpi].type == 'LPAREN': tmpScope += 1
                                elif lex[tmpi].type == 'RPAREN': tmpScope += 1
                                tmpcheck+=lex[tmpi].value ; lex[tmpi].type='IGNORE'
                            line.append(f"{anyCheck}(True if _ {check} {tmpcheck} else False for _ in {tmpfirst})")
                        elif anyCheck == 'all' and check in ('==','!=') and lex[lexIndex-1].type == 'COMMAGRP' and lex[lexIndex+1].type in ('EQUAL','NOTEQ'):
                            if (check == '==' and tmpcheck) or (check=='!=' and tmpcheck=='False'):
                                line.append(f"{tmpfirst}.count({tmpfirst}[0]) == len({tmpfirst})")
                            else:
                                line.append(f"len([True for _ in set({tmpfirst}) if _ {check} {tmpcheck}]) == len({tmpfirst})")
                        else: # just a note: this can be 'any' or 'all', while the above are optimizations for 'all'
                            if check == '==' and tmpcheck == 'True':
                                line.append(f"{anyCheck}(True for _ in {tmpfirst} if _)")
                            elif anyCheck == 'any' and check == 'equal':
                                if lex[lexIndex].value != 'are':
                                    line.append(f"len(set({tmpfirst})) != 1")
                                else:
                                    line.append(f"len(set({tmpfirst})) != len({tmpfirst})")
                            elif anyCheck == 'all' and check == 'equal':
                                if lex[lexIndex].value != 'are':
                                    line.append(f"len(set({tmpfirst})) == len({tmpfirst})")
                                else:
                                    line.append(f"len(set({tmpfirst})) == 1")
                            else:
                                if lex[lexIndex].value != 'are' and check in {'<','>','=<','=>','<=','>='}:
                                    check = {'>':'<','>':'<','=<':'=>','=>':'=<','<=':'=>','>=':'=<'}[check] # converts to opposite check
                                line.append(f"{anyCheck}(True if _ {check} {tmpcheck} else False for _ in {tmpfirst})")
                        lex[lexIndex+1].type = 'IGNORE' ; lex[lexIndex-1].type = 'IGNORE'
                        anyCheck='none'
                    else:
                        return AS_SyntaxError('"are" needs comparison or function in order to check',
                            'are equal\n# or\nmyFunction()',lineNumber,data)
                else:
                    return AS_SyntaxError('"are" needs either a variable, data-type, or function to check!',
                            'if yourVar, "hotdog" are equal then',lineNumber,data)
            elif tok.type == 'ANYOF':
                if 'of ' not in tok.value and (lex[lexIndex+1].type == 'LPAREN' or lastType == 'PIPE'):
                    tok.type='ID' ; line.append('any')
                else:
                    if lexIndex+1 < len(lex)-1 and lex[lexIndex+1].type not in typeNewline:
                        if 'any' in tok.value: anyCheck = 'any'
                        elif 'all' in tok.value: anyCheck = 'all'
                        for tmpi in range(lexIndex,len(lex)-1):
                            # if any of vowels is letter
                            # ^ the above used to fail, couldn't remember the ARE reserved word. is should be fine too!
                            if lex[tmpi].type == 'ARE': break
                            elif lex[tmpi].type == 'ASSIGN' and lex[tmpi].value.strip() == 'is':
                                # is / =  -->  are
                                lex[tmpi].type='ARE' ; lex[tmpi].value='are' ; break
                            elif lex[tmpi].type == "NOTEQ":
                                # isnt / !=  -->  are not
                                lex[tmpi].type = 'ARE';lex[tmpi].value = "are"
                                lex.insert(tmpi+1,makeToken(tok,"INS",'not'))
                                break
                    else: return AS_SyntaxError('"any of" needs something after it','any of (1,2,3)',lineNumber,data)
            elif tok.type == 'END': # idEND
                if bigWrap: # for putting things at the end
                    endOfLineChores(tok)
                tmpi=1 ; skipIf=False
                while -1 < lexIndex-tmpi < len(lex)-1:
                    if lex[lexIndex-tmpi].type in typeConditionals \
                    or lex[lexIndex-tmpi].type in ('FOR','LOOP','PYDEF','DEFFUNCT'):
                        if skipIf: skipIf=False
                        else:
                            if len(storedIndents) > 1:
                                indent=storedIndents[-2]
                                storedIndents=storedIndents[:-1]
                            else: indent-=prettyIndent
                            if indent < 0: indent=0
                            break
                    elif lex[lexIndex-tmpi].type == 'END': skipIf=True
                    tmpi+=1
                line.append('\n') ; startOfLine=True
                if combineLines:
                    if isinstance(line, str): line = [line]  # so weird
                    line.insert(0, code[-1])
                    code.pop()
                    combineLines = False
                code.append(''.join(line)) ; line=[]

            elif tok.type == 'FROM': # idFROM
                if lastType in typeNewline and lex[lexIndex+1].type == 'ASSIGN':
                    return AS_SyntaxError('from is a reserved keyword, dont use it for assignment', f'customVarName = "something"', lineNumber,data)

                inFuncArg=True ; notInDef=True
                if insideDef!='': clearFunctionVars()
                if lexIndex+1 < len(lex)-1:
                    if lex[lexIndex+1].type == 'COMMAGRP':
                        lex[lexIndex+1].type='IGNORE'

                if lastIndent[3]==[]: pass
                else:
                    indent=lastIndent[3][-1]
                    lastIndent[3].pop()

            elif tok.type == 'COMMA': # idCOMMA
                if lexIndex-1 > 0 and lexIndex+1 < len(lex)-1 and len(line)>0:
                    if lex[lexIndex+1].type not in typeNewline+('IGNORE','INC','FSTR','PIPEGO','PIPE','PYPASS') and lex[lexIndex-1].type != 'IGNORE':
                        if lex[lexIndex+1].type == 'RBRACKET':                bracketScope -= 1
                        elif lex[lexIndex+1].type == 'RPAREN':                parenScope   -= 1
                        elif lex[lexIndex+1].type in {'LPAREN', 'FUNCTION'}:  parenScope   += 1
                        elif lex[lexIndex+1].type in {'LISTEND','RINDEX'}:    listScope    -= 1
                        elif lex[lexIndex+1].type in {'LIST', 'LINDEX'}:      listScope    += 1
                        elif lex[lexIndex+1].type == 'META' and lex[lexIndex+1].value.replace('$','').strip() in inlineReplace:
                            lex[lexIndex+1].value=inlineReplace[lex[lexIndex+1].value.replace('$','').strip()]
                        lex[lexIndex+1].value=f'{line[-1]},{lex[lexIndex+1].value}'
                        line=line[:-1]
                        lex[lexIndex+1].type='COMMAGRP' ; tok.type='IGNORE' ; lex[lexIndex-1].type='IGNORE'
                    else: line.append(',')
                else: line.append(',')
            elif tok.type == 'TYPE': # idTYPE
                if lex[lexIndex+1].type == 'ASSIGN' or tok.value.strip() in reservedIsNowVar:
                    tok.type='ID'
                    if tok.value.strip() not in reservedIsNowVar: reservedIsNowVar.append(tok.value.strip())
                    doPrint = True
                    if startOfLine:
                        for tmpi in range(lexIndex+1,len(lex)-1):
                            if lex[tmpi].type in typeNewline: break
                            elif lex[tmpi].type not in typePrintable: doPrint=False
                    if doPrint: line.append(decideIfIndentLine(indent,f'{expPrint[-1]}(')); bigWrap = True; rParen += 1
                    line.append(decideIfIndentLine(indent, tok.value))

                elif lex[lexIndex+1].type not in {'ID','LIST'} and lastType not in ('PIPE','COMMA','FROM','CONSTANT','DEFFUNCT'):
                    return AS_SyntaxError(f"Type must be declared to a variable. '{lex[lexIndex+1].value}' is invalid.",f'{tok.value} variable = value', lineNumber, data)
                elif lastType in typeCheckers:
                    line.append(tok.value)
                elif lex[lexIndex-1].type not in typeNewline+('CONSTANT','DEFFUNCT','TYPEWRAP','IGNORE'):
                    return AS_SyntaxError(f'Invalid token \'{lex[lexIndex-1].type}\' before type declaration.',f'{tok.value} {lex[lexIndex+1].value} = value',lineNumber,data)
                elif lex[lexIndex+1].type == 'ID' and lex[lexIndex+2].type == 'LISTCOMP':
                    tok.type='ID' ; line.append(tok.value)
                elif tok.value  == 'range' and lex[lexIndex+1].type == 'NRANGE': tok.type='IGNORE' ; continue
                else:
                    multiType=False
                    if '[' in tok.value:
                        firstType=tok.value.split('[')[0]
                        secondType=tok.value.split('[')[1].replace(']','')
                        multiType=True
                        if secondType=='bool' and compileTo=='Cython':
                            tok.value='bint'

                    if lexIndex+2 < len(lex):
                        if lex[lexIndex+2].type == 'DEFFUNCT':
                            return AS_SyntaxError('type assigned to the wrong spot',f'myFunction does {tok.value}',lineNumber,data)
                        elif lex[lexIndex+2].type == 'ASSIGN' and any(i for i in ('+','-','*','/') if lex[lexIndex+2].value.startswith(i)):
                            return AS_SyntaxError('When declaring types, you can\'t modify the variable, as it does not have a value yet.',f'{tok.value} {lex[lexIndex+1].value} = something',lineNumber,data)
                    if lastType == 'DEFFUNCT' and pythonVersion > 3.04:
                        if multiType:
                            if line == []:
                                code[-1]=code[-1][:code[-1].rindex(':\n')]+f' -> {firstType.capitalize()}[{secondType}]:\n'
                            else:
                                line[-1]=line[-1].replace(':\n',f' -> {firstType.capitalize()}[{secondType}]:\n')
                            tmp=f'from typing import {firstType.capitalize()}'
                            if any(True for i in code if i == tmp)==False:
                                code.insert(1,tmp)
                        else:
                            if compileTo == 'Cython' and (lastValue.startswith('cpdoes') or lastValue.startswith('cdoes')):
                                tmp=code[-1].split()
                                if tok.value == 'bool': tok.value='bint'
                                tmp.insert(1,tok.value)
                                code[-1] =' '.join(tmp)
                            else:
                                if line == []:
                                    code[-1]=code[-1][:code[-1].rindex(':\n')]+f' -> {tok.value}:\n'
                                else:
                                    line[-1]=line[-1].replace(':\n',' -> %s:\n'%tok.value)
                        indentSoon=False
                    elif lastType == 'PIPE': pass
                    elif lexIndex+1 < len(lex) and lex[lexIndex+1].type in typeAssignables:
                        if multiType:
                            lex[lexIndex+1].value=f"{lex[lexIndex+1].value}: {firstType.capitalize()}[{secondType}]"
                            if lexIndex+2 < len(lex) and lex[lexIndex+2].type in typeAssignables:
                                lex[lexIndex+1].value=f"{lex[lexIndex+1].value}"

                            insertAtTopOfCodeIfItIsNotThere(f'from typing import {firstType.capitalize()}')
                        else:
                            if compileTo == 'Cython' and inLoop[0]==False and lex[lexIndex+1].value not in storedVarsHistory:
                                if lex[lexIndex + 2].type == 'COMMA':
                                    tmp = []
                                    for tmpi in range(lexIndex + 1, len(lex) - 1):
                                        if lex[tmpi].type == 'ID': tmp.append(lex[tmpi].value)
                                        elif lex[tmpi].type == 'COMMA': pass
                                        else: break
                                    for varname in tmp: code.append(f"{' ' * (indent)}cdef {cythonConvertType[tok.value] if tok.value in cythonConvertType else tok.value} {varname}")
                                else:
                                    tok.value = cythonConvertType[tok.value] if tok.value in cythonConvertType else tok.value
                                    if not line or 'DEF ' != line[-1]:
                                        line.append(decideIfIndentLine(indent,f"cdef {tok.value} "))
                            else:# compileTo == 'Python':
                                if pythonVersion >= 3.06:
                                    if lex[lexIndex+2].type == 'COMMA':
                                        tmp=[]
                                        for tmpi in range(lexIndex+1,len(lex)-1):
                                            if lex[tmpi].type == 'ID': tmp.append(lex[tmpi].value)
                                            elif lex[tmpi].type == 'COMMA': pass
                                            else: break
                                        for varname in tmp: code.append(f"{' '*(indent)}{varname}: {tok.value}")
                                    else: lex[lexIndex+1].value=f"{lex[lexIndex+1].value}: {tok.value}"
                                elif lex[lexIndex+2].type in typeNewline:
                                    if tok.value == 'str': tmp=('""','STRING')
                                    elif tok.value == 'int': tmp=('0','NUMBER')
                                    elif tok.value == 'float': tmp=('0.0','NUMBER')
                                    elif tok.value == 'list': tmp = ('[]', 'INDEX')
                                    elif tok.value == 'dict': tmp=('{}','DICT')
                                    else: tmp=('None','BOOL')
                                    lex.insert(lexIndex+2,makeToken(tok,value=tmp[0],type=tmp[1]))
                                    lex.insert(lexIndex+2,makeToken(tok, value='=', type='ASSIGN'))
                                if lexIndex+2 < len(lex) and lex[lexIndex+2].type in typeAssignables:
                                    lex[lexIndex+1].value=f"{lex[lexIndex+1].value}"

                                if lex[lexIndex+2].type=='ID' or (lexIndex+4<len(lex) and lex[lexIndex+3].type=='ASSIGN' and lex[lexIndex+4].type=='ID'):
                                    # turns int a b c is 4
                                    # into a: int then b: int then c: int then a=b=c=4
                                    skip=False ; values=[] ; tmp=None
                                    t=lexIndex+1 ; first=True
                                    while t < len(lex)*2:
                                        if t > len(lex)-1: break
                                        if skip==False and lex[t].type == 'ID':
                                            values.append(copy(lex[t]))
                                            tt=t
                                            if lex[t-1].type not in ('TYPE','ID'):
                                                tmptok=copy(tok)
                                                lex.insert(t,tmptok) ; del tmptok ; tt+=1
                                                skip=True
                                            if first or skip:
                                                tmptok=copy(tok)
                                                tmptok.type='THEN' ; tmptok.value='then'
                                                lex.insert(tt+1,tmptok) ; del tmptok
                                                skip=True
                                        elif lex[t].type == 'THEN': skip=False
                                        elif ((lex[t].type == 'ASSIGN' and lex[t+2].type not in ('ID','ASSIGN','INDEX')) or lex[t].type!='ID'):
                                            tmp=t+1 ; break
                                        t+=1
                                    if tmp != None:
                                        values=values[::-1]
                                        for t in values:
                                            if ':' in t.value: t.value=t.value.split(':')[0]
                                            lex.insert(tmp-1,t)

                    elif lex[lexIndex+1].type == 'COMMAGRP' and '=' in lex[lexIndex+1].value and lastType in typeNewline+('CONST',):
                        tmpf = lex[lexIndex + 1].value.split('=')[0].split(',') ; tmpf=[t.replace(' ','') for t in tmpf]
                        if compileTo == 'Cython' and any(True for t in tmpf if t in storedVarsHistory)==False and inLoop[0]==False and lastIndent[2] == []:
                            tok.value = cythonConvertType[tok.value] if tok.value in cythonConvertType else tok.value
                            line.append(decideIfIndentLine(indent, f"cdef {tok.value} {lex[lexIndex + 1].value.split('=')[0]}\n"))
                        else:
                            line.append(decideIfIndentLine(indent, ' '.join([f"{t}: {tok.value} ;" for t in tmpf])+'\n'  ))
                        startOfLine=True
                    else:
                        if startOfLine: line.append(f'{" "*indent}{tok.value}')
                        else: line.append(tok.value)
                    if multiType: tok.value=firstType
            elif tok.type == 'TYPEWRAP':
                if inIf:
                    tok.type='TYPE'
                    line.append(tok.value)
                else:
                    isCombined=skip=False
                    combine=[[]] ; endtmpi=lexIndex
                    if lastType == 'NEWLINE':
                        tmpExpectedIndent=prettyIndent
                    else:
                        for tmpi in range(lexIndex-1,0,-1):
                            if lex[tmpi].type == 'TAB':
                                tmpExpectedIndent=lex[tmpi].value.count(' ')+prettyIndent ; break
                    for tmpi in range(lexIndex,len(lex)*2):
                        #print(tmpi,lex[tmpi].type,lex[tmpi].value.replace('\n','\\n'),skip)
                        endtmpi+=1
                        if tmpi >= len(lex)-1: break
                        if lex[tmpi].type in ('NEWLINE','FOR','WHILE','LOOP')+typeConditionals: break
                        elif tmpi != lexIndex and lex[tmpi].type == 'TYPEWRAP': break
                        elif lex[tmpi].type == 'TAB':
                            if lex[tmpi].value.count(' ') < tmpExpectedIndent:
                                break
                            lex[tmpi].value=f"\n{' '*(prettyIndent*indent)}"
                            combine.append([])
                        elif lex[tmpi].type == 'ID' and skip==False and lex[tmpi-1].type in {'CONSTANT','THEN','TAB','TYPEWRAP'}:
                            tmptok=copy(tok)
                            tmptok.value=tok.value
                            tmptok.type='TYPE'
                            if lex[tmpi+1].type == 'ID' and compileTo != 'Cython':
                                lex.insert(tmpi,tmptok) ; skip=True
                                tmptok=copy(tok)
                                tmptok.value='\n'+' '*indent
                                tmptok.type='THEN'
                                lex.insert(tmpi+2,tmptok)
                                combine[-1].append(lex[tmpi+1].value)
                                isCombined=True
                            elif compileTo == 'Cython':
                                if lex[tmpi-1].type not in ('ID','TYPE'):
                                    lex.insert(tmpi,tmptok) ; skip=True
                                else: pass
                            else: lex.insert(tmpi,tmptok) ; skip=True

                            if compileTo != 'Cython' and lex[tmpi-2].type == 'ID' and lex[tmpi-1].type == 'THEN' \
                            and lex[tmpi+1].value not in combine[-1] and isCombined:
                                combine[-1].append(lex[tmpi+1].value) ; isCombined=False
                            del tmptok
                        elif skip: skip=False

                    if len(combine) > 0 and compileTo != 'Cython':
                        # if there are multi defines like   a b c is 2
                        # when doing typewraps define their types but dont assign anything
                        # keep the last var the same
                        # but at the end set the first set of vars value like:
                        # c: int = 12
                        # a = b = c
                        combine=[c[::-1] for c in combine] # reverse the order
                        for c in combine:
                            if len(c) > 0:
                                tmptok=copy(tok)
                                tmptok.value='\n'+' '*indent
                                tmptok.type='THEN'
                                lex.insert(endtmpi-1,tmptok)
                                for t in c:
                                    tmptok=copy(tok)
                                    tmptok.value=t
                                    tmptok.type='ID'
                                    lex.insert(endtmpi,tmptok)
                                    del tmptok
                        if lex[-1].type != 'NEWLINE':
                            tmptok=copy(tok) ; tmptok.value='\n' ; tmptok.type='NEWLINE'
                            lex.append(tmptok)

            elif tok.type == 'CONSTANT': # idCONSTANT
                if (lexIndex+3 < len(lex) or lexIndex+2 < len(lex)) and (lex[lexIndex+1].type == 'ID' or lex[lexIndex+2].type == 'ID'):
                    if compileTo != 'Cython' or (compileTo=='Cython' and ((lex[lexIndex+2].type == 'ASSIGN' and lex[lexIndex+3].type not in ('STRING','LIST','DICT','NUMBER','SET') or lex[lexIndex+3].value[0] in 'fF') or (lex[lexIndex+1].type == 'ID' and lex[lexIndex+2].type not in ('STRING','LIST','DICT','NUMBER','SET') or lex[lexIndex+2].value[0] in 'fF') or (lex[lexIndex+2].type == 'ID' and lex[lexIndex+3].type not in ('STRING','LIST','DICT','NUMBER','SET','ASSIGN') or lex[lexIndex+3].value[0] in 'fF'))):
                        # ^^ when Cython, check if it can be compile-time-constant, else defaults to our implementation
                        if (pythonVersion >= 3.04 and pythonVersion < 3.08) or compileTo == 'MicroPython': # old implementation
                            # deprecate ?
                            tmpval=copy(lex[lexIndex+1])
                            if lex[lexIndex+2].type=='ASSIGN': tmpi=3
                            elif lex[lexIndex+1].type=='TYPE':
                                if lex[lexIndex+4].type=='ASSIGN': tmpi=5
                                else: tmpi=4

                                if lex[lexIndex+1].value in convertType: tmpf=convertType[lex[lexIndex+1].value]
                                else: tmpf=lex[lexIndex+1].value
                                storedVarsHistory[lex[lexIndex+2].value]={'value': lex[lexIndex+2].value+'[0]', 'type': tmpf}
                                if pythonVersion > 3.04 and compileTo != 'MicroPython':
                                    insertAtTopOfCodeIfItIsNotThere('from typing import Tuple\n')
                                    lex[lexIndex+1].type=lex[lexIndex+2].type='IGNORE'
                                    if lex[lexIndex+3].type != 'ASSIGN':
                                        lex[lexIndex+3].value=f"{lex[lexIndex+2].value} : Tuple[{lex[lexIndex+1].value}] = ({lex[lexIndex+3].value}"
                                    else:
                                        lex[lexIndex+3].value=f"{lex[lexIndex+2].value} : Tuple[{lex[lexIndex+1].value}] = ({lex[lexIndex+4].value}"
                                        lex[lexIndex+4].type='IGNORE'
                                else:
                                    tmp = '('
                                    if optimize and compileTo == 'MicroPython' and optFuncTricks and optFuncTricksDict['microPythonConst'] and storedVarsHistory[lex[lexIndex + 2].value]['type']=='NUMBER':
                                        tmp = 'const(' ; tmpi=len(lex)+1 # to cancel tuple index propagation
                                        insertAtTopOfCodeIfItIsNotThere('from micropython import const\n')
                                    if lex[lexIndex + 3].type != 'ASSIGN':
                                        lex[lexIndex + 3].value = f"{lex[lexIndex + 2].value} = {tmp}{lex[lexIndex + 3].value}"
                                        lex[lexIndex + 2].type = 'IGNORE'
                                    else:
                                        lex[lexIndex + 3].value = f"{lex[lexIndex + 2].value} = {tmp}{lex[lexIndex + 4].value}"
                                        lex[lexIndex + 2].type = lex[lexIndex + 4].type = 'IGNORE'
                                    if lex[lexIndex+1].type == 'TYPE': lex[lexIndex+1].type='IGNORE'
                                lex[lexIndex+3].type='BUILTINF'
                                tmpval=copy(lex[lexIndex+2])
                            else: tmpi=2

                            if lexIndex+tmpi+1 < len(lex) and lex[lexIndex+tmpi+1].type == 'LISTCOMP': pass
                            elif tmpi>3:
                                for tmptok in range(lexIndex,len(lex)-1):
                                    if lex[tmptok].type in typeNewline:
                                        lex[tmptok].value+=')' ; break
                            else:
                                tmptok=copy(tok)
                                tmptok.type='constLPAREN' ; tmptok.value='('
                                lex.insert(lexIndex+tmpi,tmptok) ; del tmptok
                                if lex[lexIndex+tmpi].type != 'ASSIGN' and lex[lexIndex+tmpi-1].type!='ASSIGN': # tmp-1 dubious, solved a bug
                                    tmptok=copy(tok)
                                    tmptok.type='ASSIGN' ; tmptok.value='='
                                    lex.insert(lexIndex+tmpi,tmptok) ; del tmptok
                            bigWrap=True ; constWrap=True ; miniLineNumber=lineNumber
                            tmpInFunction = False ; tmpParenScope = 0
                            while lexIndex+tmpi < len(lex)-1:
                                if lex[lexIndex+tmpi].type == 'ID' and lex[lexIndex+tmpi].value == tmpval.value:
                                    if not tmpInFunction or tmpParenScope <= 0 or (lex[lexIndex+tmpi+1].type!='ASSIGN' and tmpInFunction):
                                        lex[lexIndex+tmpi].value=f'{lex[lexIndex+tmpi].value}[0]'
                                    if lex[lexIndex+tmpi+1].type == 'ASSIGN' and not tmpInFunction:
                                        return AS_SyntaxError(f'Cannot reassign to constant variable {tmpval.value}',None,miniLineNumber,data,'Compile time error')
                                elif lex[lexIndex+tmpi].type in ('INDEX','LIST'):
                                    if tmpval.value+',' in lex[lexIndex+tmpi].value:
                                       tmp=list(lex[lexIndex+tmpi].value)
                                       tmp.insert(lex[lexIndex+tmpi].value.index(tmpval.value+',')+len(tmpval.value),'[0]')
                                       lex[lexIndex+tmpi].value=''.join(tmp)
                                    elif ','+tmpval.value in lex[lexIndex+tmpi].value:
                                       tmp=list(lex[lexIndex+tmpi].value)
                                       tmp.insert(lex[lexIndex+tmpi].value.index(','+tmpval.value)+len(tmpval.value)+1,'[0]')
                                       lex[lexIndex+tmpi].value=''.join(tmp)
                                    elif lex[lexIndex+tmpi].type == 'INDEX' and any(True for _ in {':','['} if _+tmpval.value in lex[lexIndex+tmpi].value):
                                        # detection in indexes to avoid false variable names
                                        tmp = [_ for _ in {':','['} if _+tmpval.value in lex[lexIndex+tmpi].value][0]
                                        lex[lexIndex + tmpi].value = lex[lexIndex + tmpi].value.replace(tmp+tmpval.value,tmp+tmpval.value+'[0]')
                                elif lex[lexIndex+tmpi].type == 'FUNCTION' or (lex[lexIndex+tmpi].type == 'ID' and lex[lexIndex+tmpi].value == 'print'):
                                    tmpInFunction = True
                                    if lex[lexIndex+tmpi].type == 'FUNCTION':
                                        if lex[lexIndex+tmpi].value == 'globals(' and lex[lexIndex+tmpi+3].type == 'STRING' and lex[lexIndex+tmpi+3].value.replace('"','').replace("'","") == tmpval.value:
                                            return AS_SyntaxError(f'Cannot reassign to constant variable {tmpval.value}','Avoiding globals assignment. The compiler has trouble keeping track of it. It is also confusing for humans.', miniLineNumber, data, 'Compile time error')
                                        if lex[lexIndex+tmpi].value[-1] == '(': tmpParenScope+=1
                                elif lex[lexIndex + tmpi].type == 'LPAREN':
                                    tmpParenScope+=1
                                elif lex[lexIndex + tmpi].type == 'RPAREN':
                                    tmpParenScope-=1
                                elif lex[lexIndex+tmpi].type == 'COMMAGRP' and tmpval.value in lex[lexIndex+tmpi].value:
                                    tmp=lex[lexIndex+tmpi].value.split(',')
                                    for t in range(0,len(tmp)):
                                        if tmp[t] in (tmpval.value,'-'+tmpval.value):
                                            tmp[t]+='[0]'
                                    lex[lexIndex+tmpi].value=','.join(tmp)
                                miniLineNumber+=lex[lexIndex+tmpi].value.count('\n')
                                tmpi+=1

                            if (lex[lexIndex+2].type=='ASSIGN' and lex[lexIndex+4].type == 'LISTCOMP'):
                                tmpf=copy(lex[lexIndex+1]);tmpf.value+='[0]'
                                storeVar(tmpf,lex[lexIndex+4],lex[lexIndex+5],position=lexIndex+4)
                                lex[lexIndex+1].value+=' = ('
                                lex[lexIndex+2].type='IGNORE'
                        else:
                            # newer method
                            if (lex[lexIndex+1].type == 'ID' or (lex[lexIndex+2].type == 'ID' and lex[lexIndex+1].type == 'TYPE')) and lex[lexIndex+2].type in typeAssignables+('ASSIGN',):
                                insertAtTopOfCodeIfItIsNotThere('from typing import Final\n')
                                if lex[lexIndex+1].type == 'TYPE': tmp = 2
                                else: tmp = 1

                                tmpType=None
                                if lex[lexIndex-1].type == 'TYPE':
                                    tmpType=lex[lexIndex-1].value
                                    lex[lexIndex - 1].type = 'IGNORE'
                                elif tmp == 2:
                                    tmpType=lex[lexIndex+1].value
                                    lex[lexIndex + 1].type = 'IGNORE'

                                tmpName=lex[lexIndex+tmp].value
                                lex[lexIndex+tmp].value = lex[lexIndex+tmp].value + ': Final'
                                if tmpType:
                                    if pythonVersion <= 3.08 and '[' in tmpType:
                                        tmp2=tmpType.split('[')[0].capitalize()
                                        insertAtTopOfCodeIfItIsNotThere(f"from typing import {tmp2}\n")
                                        lex[lexIndex + tmp].value += '['+tmp2+'['+'['.join(tmpType.split('[')[1:])+']'
                                    else: lex[lexIndex + tmp].value+=f'[{tmpType}]'
                                    if tmpType in convertType: tmpType2 = convertType[tmpType]
                                    elif '[' in tmpType and tmpType.split('[')[0] in convertType: tmpType2 = convertType[tmpType.split('[')[0]] # for stuff like tuple[str]
                                    storedVarsHistory[tmpName]={'type': tmpType2,'staticType':tmpType}
                                else:
                                    storedVarsHistory[tmpName] = {}
                    elif compileTo == 'Cython':
                        if lex[lexIndex+1].type == 'TYPE' and lex[lexIndex+1].value in {'str','int','float'}:
                            lex[lexIndex+1].type='IGNORE'
                            line.append(decideIfIndentLine(indent,"DEF "))
                else:
                    return AS_SyntaxError('constant needs a variable and value','constant PI is 3.14159',lineNumber,data)
            elif tok.type == 'PIPEGO':
                if lastValue in ('print','print('):
                    code.append(''.join(line)) ; line = []
                    combineLines = True ; lex[lexIndex-1].type='IGNORE'
                if lex[lexIndex+2].type != 'PIPE': # should only add a ( ) if more than one token in expression
                    for tmpi in range(lexIndex+1,len(lex)):
                        if lex[tmpi].type == 'PIPE':
                            tok.type = 'LPAREN';tok.value = '('
                            lex.insert(tmpi,makeToken(tok,')','RPAREN'))
                            lex.insert(lexIndex,tok)
                            break
            elif tok.type == 'PIPE': # idPIPE
                if tok.value == 'pipe':
                    pass
                elif lexIndex+1 < len(lex) and lex[lexIndex+1].type=='NUMBER':
                        line.append(decideIfIndentLine(indent,f"range({lex[lexIndex-1].value}, {lex[lexIndex+1].value})"))
                elif 'into' in tok.value or (lexIndex-1>0 and lex[lexIndex-1].type == 'INTOED') or line==[]:
                    if debug: print(f'{lex[lexIndex+1].value}(',''.join(line),')')
                    startOfLine=True # in fast mode this will create indents where they dont need to be, but when an indent is needed it does it correctly
                    if optimize and optFuncTricks and optFuncTricksDict['boolTonotnot'] and lex[lexIndex+1].value.strip() == 'bool' and line:
                        line.insert(0, decideIfIndentLine(indent, f"(not not "))
                        line.append(')')
                        lex[lexIndex + 1].type = 'INTOED'
                    else:
                        if line==[]: line.append(lex[lexIndex-1].value)
                        if line[0].strip() == '(' and line[-1] == ')':
                            check=True ; tmpParenScope=0
                            for char in ''.join(line):
                                # if INTO piping a group of parens  (thing),(thang) into test
                                # then we dont want to inherit the paren of  (thing) --> test(thing),(thang)
                                # but instead wrap around both  test((thing),(thang))
                                if char == '(': tmpParenScope+=1
                                elif char == ')': tmpParenScope-=1
                                elif char == ',' and tmpParenScope == 0: check=False ; break
                            if check:
                                # inherit paren
                                line=line[1:-1]
                        if lex[lexIndex+1].value[-1] == '(': parenScope+=1
                        line.insert(0,decideIfIndentLine(indent,f"{lex[lexIndex+1].value}("))
                        line.append(')')
                        lex[lexIndex+1].type='INTOED'
                else:
                    if hasPiped == False:
                        tmpfunc=[]
                        tmpi=1
                        if lex[lexIndex-1].value.startswith(':'):
                            lex[lexIndex-1].value=lex[lexIndex-1].value[1:]
                        # piping multi tokens oh boy
                        if lexIndex+tmpi <= len(lex)-2 \
                        and lex[lexIndex-1].type == 'NUMBER' \
                        and lex[lexIndex-2].type == 'MINUS':
                            line=line[:-1]
                            tmpfunc.append(f'(-{lex[lexIndex-1].value})')
                        elif lex[lexIndex-1].type in {'INDEX','RINDEX'} and findEndOfFunction(lexIndex-1,goBackwards=True)!=False:
                            tmp=[i for i in range(0,len(line)-1) if lex[findEndOfFunction(lexIndex-1,goBackwards=True)].value in line[i]][0]
                            tmpfunc.append(f"({''.join(line[tmp:])})")
                            line=line[:tmp]
                        elif lex[lexIndex-1].type == 'RPAREN' or (lex[lexIndex-1].type == 'BUILTINF' and lex[lexIndex-1].value[0]==')'): # ()
                            if any(i for i in line if '(' in i):
                                # this section is poorly written. but it (groups paren) into PIPE
                                tmp=None
                                tmpscope=0
                                for ii in range(len(line)-1,0,-1):
                                    i=line[ii]
                                    if '(' in i:
                                        tmp=ii # dubious?? i guess not
                                        if tmpscope>0:
                                            tmpscope-=i.count('(')
                                            tmpscope+=i.count(')')
                                        if tmpscope==0 and tmp-1 > 0 and line[tmp-1].endswith(' ')==False:
                                            tmp=ii # if ( and previous line doesnt end with space, assume thats a function, set tmp as that
                                            break
                                    elif ')' in i:
                                        tmpscope-=i.count('(')
                                        tmpscope+=i.count(')')
                                    elif tmpscope==0: break
                                    else: tmp=ii
                                funcIndex=findEndOfFunction(lexIndex-1,goBackwards=True,needsToBeFunction=True)
                                if funcIndex!=False and lex[funcIndex-1].type== 'INTOED':
                                    startOfLine=True ; tmpi=len(lex)+1 ; line.insert(0,decideIfIndentLine(indent,f"{lex[lexIndex+1].value}(")) ; line.append(')') ; lex[lexIndex+1].type='INTOED'
                                elif funcIndex!=False and tmp != None:
                                    if "".join(line[tmp:]).lstrip(' ').startswith(lex[funcIndex].value):
                                        tmpfunc.append(f'({"".join(line[tmp:]).lstrip(" ")})')
                                    else:
                                        tmpfunc.append(f'({lex[funcIndex].value}{"".join(line[tmp:])})')
                                    tmp=[i for i in range(0,len(line)) if lex[funcIndex].value in line[i]]
                                    if tmp != []: line=line[:tmp[0]]
                                    # ^ might need len(line)-1
                                else:
                                    if tmp == None:
                                        tmp="".join(line)
                                        if line[-1]!=")": tmp+=')'
                                        if lastType == 'RPAREN': tmp+=')'
                                        tmpfunc.append(f'({tmp}')
                                        line=[]
                                    else:
                                        tmpfunc.append(f'({"".join(line[tmp:])})')
                                        line=line[:tmp]
                            else:
                                return AS_SyntaxError('closing ) is missing opening (',
                                f'(1,2,3) to {lex[lexIndex+1].value}',lineNumber,data)
                        elif lexIndex+2 < len(lex) and lex[lexIndex+2].type=='BUILTINF': # for pesky ", ".join()
                            lex[lexIndex+1].value+=lex[lexIndex+2].value
                            lex[lexIndex+2].value=f'{lex[lexIndex+1].value}({lex[lexIndex-1].value})'
                            lex[lexIndex+2].type='IGNORE' ; tmpi=len(lex)+1
                        elif lex[lexIndex-1].type=='BUILTINF' and findEndOfFunction(lexIndex-2,goBackwards=True,needsToBeFunction=True)!=False:
                            tmp=line.index(lex[findEndOfFunction(lexIndex-2,goBackwards=True,needsToBeFunction=True)].value)
                            tmpfunc.append(f"({''.join(line[tmp:])})") ; line=line[:tmp]
                        elif lex[lexIndex-1].type == 'IGNORE' and lexIndex-2 > 0 and lex[lexIndex-2].type=='DIVMOD':
                            tmpfunc.append(f"({line[-1]})") ; line=line[:-1]
                        elif lex[lexIndex-1].type == "LISTEND":
                            tmpSafe=True
                            for tmpii in range(0,len(line)):
                                if '[' in line[tmpii]:
                                    if tmpii-1 > 0 and line[tmpii-1].endswith(')'):
                                        # is index to function
                                        tmpTheSplitIndex=0
                                        for tmpiii in range(tmpii,0,-1):
                                            if line[tmpiii].endswith('('):
                                                tmpTheSplitIndex=tmpiii
                                        tmpfunc.append('('+(''.join(line[tmpTheSplitIndex:]))+'])')
                                        line=line[:tmpTheSplitIndex]
                                        tmpSafe = False ; break
                            if tmpSafe:
                                for tmpii in range(0, len(line)):
                                    if '[' in line[tmpii]:
                                        tmp=line[tmpii:]
                                        line=line[:tmpii]
                                        line.insert(0,tmp[0].split('[')[0])
                                        tmp[0]=tmp[0].split('[')[-1]+'(['
                                        tmpfunc.append(''.join(tmp)+'])')
                                        break
                        elif lex[lexIndex-1].type == "RINDEX":
                            for tmpii in range(0,len(line)):
                                if '[' in line[tmpii]:
                                    if len(line[tmpii].replace(' ',''))>1:
                                        tmpname = line[tmpii].split('[',1)[0]
                                        line[tmpii]=line[tmpii].rsplit('[')[-1]
                                    else: tmpname=line[tmpii-1]
                                    tmp=line[tmpii:]
                                    line=line[:tmpii-1]
                                    line.insert(0,tmp[0].split('[')[0])
                                    tmp[0]=tmp[0].split('[')[-1]+f'({tmpname}['
                                    tmpfunc.append(''.join(tmp)+')')
                                    break
                        elif lex[lexIndex-1].type == 'FSTR':
                            for ii in range(len(line) - 1, -1, -1):
                                #print(ii,'~',line[ii])
                                if 'f"' in line[ii] or "f'" in line[ii]:
                                    tmpfunc.append('('+''.join(line[ii:])+')')
                                    line=line[:ii] ; break

                        else:
                            if lex[lexIndex-1].type in {'NRANGE','INC'}:
                                tmpfunc.append(f'({line[-1]})')
                            else:
                                # vvvv hack section
                                if line and lex[lexIndex-1].type == 'NUMBER' and lex[lexIndex-1].value == line[-1]:
                                    line=line[:-1]
                                    if line[-1] == '-' and lex[lexIndex-2].type == 'MINUS': line=line[:-1]
                                # ^ this section is a hack. past (even ancient) versions of ASnake handled this case just fine:
                                # f"{12 to chr}"  past/expected: f"{chr(12)}  what-happens-if-you-remove-this-line: f"{12chr(12)}"
                                # i sure hope this line doesn't cause other bugs (which it might), because its going to be a pain to debug this regression.
                                # even weirder, this bug only exists for NUMBER tokens. old asnake even fixes the minus case
                                tmpfunc.append(f'({lex[lexIndex-1].value})') # 'the default'
                            if len(line)>0 and line[-1] != '[' and lex[lexIndex-1].type not in typeAssignables:
                                if line[-1].endswith(':\n'): pass
                                else: line=line[:-1]
                        if debug: print('line',line,'tmpfunc',tmpfunc)
                        tmpfirst=True
                        while lexIndex+tmpi <= len(lex)-1 and lex[lexIndex+tmpi].type != 'PIPE' and lex[lexIndex+tmpi-1].type == 'PIPE' and 'into' not in lex[lexIndex+tmpi-1].value:
                            if lex[lexIndex+tmpi].type in typeCheckers+typeMops+typeOperators+('INC',):
                                return AS_SyntaxError(
                                f'Line {lineNumber} pipes to invalid type, can only pipe to functions.',
                                '12 to str',lineNumber,data)
                            elif lex[lexIndex+tmpi].value not in {'\n ','\n'} \
                            or lex[lexIndex+tmpi].type not in typeNewline:
                                if tmpfirst:
                                    if lexIndex+1<len(lex) and lex[lexIndex+1].type == 'COMMAGRP':
                                        tmpfunc.insert(0,lex[lexIndex+1].value.split(',')[0])
                                        lex[lexIndex+1].value=f"{''.join(tmpfunc)},{','.join(lex[lexIndex+1].value.split(',')[1:])}"
                                        line=line[:-1] ; tmpfunc='' ; break
                                    else: tmpfunc.insert(0,lex[lexIndex+tmpi].value)
                                else:
                                    tmpfunc.insert(0,f'{lex[lexIndex+tmpi].value}(')
                                    tmpfunc.append(')')
                                lex[lexIndex+tmpi].type='IGNORE' # might be problematic but probably isnt
                                tmpi+=2
                                tmpfirst=False
                                if debug: print('$',lex[lexIndex+tmpi-2].value,tmpfunc)
                                if lexIndex+(tmpi-1) > 0 and lexIndex+(tmpi-1) > len(lex)\
                                and lex[lexIndex+(tmpi-1)].type in typeNewline:
                                    tmpi+=len(lex)+1
                            else:
                                tmpi+=len(lex)+1
                        if (startOfLine or len(line)==0 or line==['']) and indent>0: line.append(f"{' '*indent}{''.join(tmpfunc)}")
                        else: line.append(''.join(tmpfunc))
                        if lexIndex+1<len(lex) and lex[lexIndex+1].type == 'COMMAGRP': hasPiped=False
                        else: hasPiped=True
                    else: hasPiped=True
            elif tok.type == 'SHEBANG':
                code[0]=f'{tok.value}\n{code[0]}'
            # printing value without space
            elif tok.type in {'STRRAW','FSTR','SCOPE','BUILTINF','MINUS','IMPORT','INDEX','LPAREN','RPAREN','FUNCTION','BITWISE','FUNCMOD','WITHAS','ENDIF','LBRACKET','RBRACKET', 'FSTRFRMT'}:
                if tok.type in {'BUILTINF','INDEX','FUNCTION'}: # idFUNCTION kinda?
                    if lastType in {'ID','INDEX'}:
                        if tok.type == 'BUILTINF' and tok.value[0]=='.':
                            line[-1]+=tok.value ; continue
                        else:
                            if lastType=='ID' and tok.type=='INDEX':
                                # if an var has an index after it, but there is an assignment later in the line, then we know they are not trying to assign the var to it
                                check=False
                                for t in range(lexIndex,len(lex)):
                                    if lex[t].type in typeNewline: break
                                    elif lex[t].type == 'ASSIGN': check=True
                                if check:
                                    lex[lexIndex-1].type='IGNORE'
                                    tok.value=f"{lex[lexIndex-1].value}{tok.value}"
                                    line[-1]=line[-1].replace(lex[lexIndex-1].value,'')
                            elif inIf: # if im 'lazy' | if im == 'lazy'
                                line.append('== ')
                            else: # im 'lazy' | im = 'lazy'
                                line.append('= ')
                            if lex[lexIndex-2].type == 'TYPE':storeVar(lex[lexIndex-1],tok,lex[lexIndex+1],lex[lexIndex-2],position=lexIndex)
                            else: storeVar(lex[lexIndex-1],tok,lex[lexIndex+1],position=lexIndex)
                    elif optimize and tok.type == 'BUILTINF' and optFuncTricks and optFuncTricksDict['popToDel'] and ((tok.value.endswith('.pop') and checkVarType(tok.value.split('.')[0], 'LIST')) or (tok.value.startswith('AS') and tok.value.endswith('_pop') and checkVarType(tok.value,'BUILTINF') and checkVarType(tok.value[2:].split('_pop')[0],'LIST'))) and lastType in typeNewline \
                    and (((lex[lexIndex+2].type == 'NUMBER' or (lex[lexIndex+2].type=='ID' and checkVarType(lex[lexIndex+2].value,'NUMBER'))) and lex[lexIndex+3].type == 'RPAREN' and lex[lexIndex+4].type in typeNewline) or (lex[lexIndex+2].type == 'MINUS' and (lex[lexIndex+3].type == 'NUMBER' or lex[lexIndex+3].type == 'ID' and checkVarType(lex[lexIndex+3].value,'NUMBER')) and lex[lexIndex+4].type == 'RPAREN' and lex[lexIndex+5].type in typeNewline)):
                        if lex[lexIndex+2].type == 'MINUS' and lex[lexIndex+3].type == 'NUMBER' and lex[lexIndex+3].value == '1':
                            # x.pop(-1) --> x.pop()
                            lex[lexIndex+2].type = lex[lexIndex+3].type = 'IGNORE'
                        elif not tok.value.startswith('AS'):
                            # x.pop(y) --> del x[y]
                            lastType = 'RETURN' ; lastValue = 'del'
                            line.append(decideIfIndentLine(indent,'del '))
                            tok.value=tok.value.split('.')[0] ; tok.type='ID'
                            lex[lexIndex+1].type = 'LINDEX' ; lex[lexIndex+1].value = '['
                            if lex[lexIndex+2].type == 'MINUS':
                                lex[lexIndex+4].type = 'RINDEX' ; lex[lexIndex+4].value = ']'
                            else:
                                lex[lexIndex+3].type = 'RINDEX' ; lex[lexIndex+3].value = ']'
                    if lexIndex+1 < len(lex):
                        if tok.type=='FUNCTION' and lex[lexIndex+1].type == typeAssignables+('ASSIGN','ASSIGN') and tok.value.endswith('()'):
                            tok.value=tok.value.replace('()','')
                            tok.type='ID' ; storedCustomFunctions.remove(tok.value)
                        elif compileTo == 'Cython' and tok.type == 'FUNCTION' and tok.value.replace('(','').strip() == 'print' and lex[lexIndex+2].type == 'RPAREN':
                            tok.type='IGNORE' ; cythonPrint(lex[lexIndex+1])
                            lex[lexIndex+1].type='IGNORE' ; lex[lexIndex+2].type='IGNORE' ; continue # continue might be problematic
                        elif lex[lexIndex+1].type == 'INS': tok.value+=' '

                    if tok.type == 'FUNCTION' and optimize:
                        if optIfTrue and optLoopAttr \
                        and tok.value == 'ASlen' and lex[lexIndex+1].type == 'LPAREN':
                            tok.value+='(' ; lex.remove(lex[lexIndex+1]) # optLoopAttr makes function without '(' at end, so add it

                        if optSortedToSort and tok.value == 'sorted(' and lastType in {'ID','ASSIGN'} and 'sorted' not in storedCustomFunctions:
                            # var is sorted(var) -> var.sort()
                            if lex[lexIndex-1].type == 'ASSIGN': tmp=2
                            else: tmp=1
                            if lex[lexIndex+1].type == 'COMMAGRP': tmpf=lex[lexIndex+1].value.split(',')[0]
                            else: tmpf=lex[lexIndex+1].value
                            if lex[lexIndex-tmp].value == tmpf and lex[lexIndex-tmp].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-tmp].value]['type'] in ('LIST','LISTCOMP'):
                                # only perform this optimization if its known var is a list
                                if inIf: pass
                                # ^ when in := walrus this optimization can change behaviour (returns None instead of list or True), so dont do it
                                else:
                                    tmpCount=0 # for copying indent
                                    while code[-1][tmpCount] == ' ':
                                        tmpCount+=1
                                    code=code[:-1] # removes  var =
                                    tok.type='BUILTINF' ; tok.value=f'\n{" "*tmpCount}{lex[lexIndex-tmp].value}.sort('
                                    if lex[lexIndex + 1].type == 'COMMAGRP': lex[lexIndex+1].value=''.join(lex[lexIndex+1].value.split(',')[1:])
                                    else: lex[lexIndex+1].type = 'IGNORE'
                        elif optIfTrue and tok.value in {'len(','ASlen('} \
                        and lex[lexIndex+1].type == 'ID' and lex[lexIndex+1].value in storedVarsHistory and lex[lexIndex+2].type == 'RPAREN' and lex[lexIndex+3].type in ('GREATER','EQUAL','ASSIGN') \
                        and lex[lexIndex+4].type == 'NUMBER' and lex[lexIndex+4].value == '0' \
                        and (('staticType' in storedVarsHistory[lex[lexIndex+1].value] and storedVarsHistory[lex[lexIndex+1].value]['staticType'] == 'list') \
                        or (storedVarsHistory[lex[lexIndex+1].value]['type'] in ('LIST','LISTCOMP'))):
                            # when var is list, when checking if len is zero
                            # len(x) == 0 -> if not x aka false
                            # len(x) > 0 -> if x aka true
                            tok.type = lex[lexIndex].type = 'IGNORE'
                            if lex[lexIndex + 3].type == 'GREATER':
                                lex[lexIndex + 2].type = lex[lexIndex + 3].type = 'IGNORE'
                            else:
                                lex[lexIndex + 2] = copy(lex[lexIndex + 1])
                                lex[lexIndex + 1].type='INS' ; lex[lexIndex + 1].value='not'
                                lex[lexIndex + 3].type = 'IGNORE'
                            lex[lexIndex + 4].type = 'IGNORE'
                            continue

                        elif optFuncTricks and optFuncTricksDict['intToFloat'] and lex[lexIndex].value == 'float(' and lex[lexIndex+1].type == 'ID' and lex[lexIndex+2].type == 'RPAREN' \
                        and lex[lexIndex+1].value in storedVarsHistory and 'staticType' in storedVarsHistory[lex[lexIndex+1].value] and storedVarsHistory[lex[lexIndex+1].value]['staticType'] == 'int':
                            lex[lexIndex].type = 'LPAREN' ; lex[lexIndex].value='(' ; autoMakeTokens(f"/ 1",lexIndex+1)

                        elif compileTo == 'Cython' and tok.value == 'len(' and lex[lexIndex+1].type == 'STRING' and lex[lexIndex+2].type == 'RPAREN':
                            insertAtTopOfCodeIfItIsNotThere('from libc.string cimport strlen as CYlen\n')
                            tok.value='CYlen('

                    if metaDefaultExpressionWithFunction and (tok.type == 'FUNCTION' or (tok.type == 'BUILTINF' and lex[lexIndex+1].type == 'LPAREN')) and not fstrQuote and lastType in typeNewline:
                        tmpInFunction=0 ; doPrint = False
                        for tt in range(lexIndex,len(lex)-1):
                            if tmpInFunction <= 0 and lex[tt].type in typeOperators:
                                doPrint=True ; break
                            elif lex[tt].type in typeNewline: break
                            elif lex[tt].type == 'LPAREN': tmpInFunction+=1
                            elif lex[tt].type == 'FUNCTION' and '(' in lex[tt].value: tmpInFunction+=1
                            elif lex[tt].type == 'RPAREN': tmpInFunction-=1
                        if doPrint:
                            line.append(decideIfIndentLine(indent,f'{expPrint[-1]}('))
                            bigWrap = True; rParen += 1
                    if metaDefaultExpressionWithFunction and lastType in typeNewline and tok.type == 'BUILTINF' and lex[lexIndex+1].type not in ('LPAREN','ASSIGN'):
                        if lex[lexIndex+1].type in typeNewline: check=True # called bare, thus safe
                        else:
                            check = False ; tmpModFound=False
                            for t in range(lexIndex,len(lex)-1):
                                if lex[t].type in typeNewline: break
                                elif lex[t].type not in typePrintable+('BUILTINF',):
                                    check = False ; break
                                elif lex[t].type == 'BUILTINF' and lex[t + 1].type == 'LPAREN':
                                    check = False ; break
                                elif not tmpModFound and lex[t].type in typeMops:
                                    # only trigger default expression if being operated on, in a way that has no effect
                                    check = True ; tmpModFound = True
                        if check:
                            line.append(decideIfIndentLine(indent, f'{expPrint[-1]}('))
                            bigWrap = True ; rParen += 1


                    if tok.type == 'INDEX':
                        parenScope-=tok.value.count(')') ; parenScope+=tok.value.count('(')
                        listScope -= tok.value.count(']'); listScope += tok.value.count('[')
                    elif tok.value.strip()[-1] == '(': parenScope+=1

                    if tok.type=='BUILTINF' and tok.value.split('.')[0] in storedVarsHistory \
                    and storedVarsHistory[tok.value.split('.')[0]]['type']=='LIST' and len(tok.value.split('.')) > 1 and '.'+tok.value.split('.')[1] in listMods and 'value' in storedVarsHistory[tok.value.split('.')[0]]:
                        del storedVarsHistory[tok.value.split('.')[0]]['value']
                    # ^ if list is being modified, we don't know the value anymore, so forget it
                    elif tok.type=='INDEX' and lexIndex-1>0 and lex[lexIndex-1].type in {'ASSIGN','ID'}:
                        # just storing var in history if it assigns to index without an assignment token
                        if lex[lexIndex-1].type in {'ID','INDEX','COMMAGRP'}: tmpi=1
                        elif lexIndex-2>0 and lex[lexIndex-2].type in {'ID','INDEX','COMMAGRP','BUILTINF'}: tmpi=2
                        else:
                            tmp=True
                            tmpListScope=0
                            for tmpi in range(lexIndex-2,0,-1):
                                if lex[tmpi].type in {'LINDEX','LIST'}: tmpListScope+=1
                                elif lex[tmpi].type in {'RINDEX','LISTEND'}: tmpListScope-=1
                                elif lex[tmpi].type in typeNewline and tmpListScope == 0:
                                    tmp=False ; break
                            if tmp:
                                return AS_SyntaxError('Assignment needs a var ID',f'myVar is {tok.value}',lineNumber,data)

                        if lex[lexIndex-tmpi].value in storedVarsHistory:
                            storedVarsHistory[lex[lexIndex-tmpi].value]['value'] = 'INDEX'
                        else: storeVar(lex[lexIndex-tmpi],tok,lex[lexIndex+1],position=lexIndex)
                    elif tok.type == 'RPAREN':
                        parenScope -= 1
                        if parenScope < 0:
                            return AS_SyntaxError(f'Too many closing parenthesis.', ' '.join(line), lineNumber, data)

                    elif lastType in typeNewline and lex[lexIndex].type=='INDEX':
                        check=True
                        for t in range(lexIndex+1,len(lex)):
                            if lex[t].type in typeNewline: break
                            elif lex[t].type in {'FUNCTION','BUILTINF','ASSIGN','PIPE'}: check=False ; break
                            elif lex[t-1].type == 'INDEX' and lex[t].type == 'INDEX': check=False ; break
                        if check:
                            line.append(decideIfIndentLine(indent,f'{expPrint[-1]}('))
                            rParen+=1 ; bigWrap=True
                elif tok.type == 'LPAREN' and startOfLine and inIf==False:
                    tmp=rParen
                    rParen+=1
                    for tmpi in range(lexIndex,len(lex)-1):
                        if lex[tmpi].type in typeNewline: break
                        elif lex[tmpi].type not in typePrintable:
                            if lex[tmpi].type == 'ASSIGN' and lex[tmpi].value.strip() == 'is':
                                pass
                            else:
                                rParen-=1 ; break
                        elif tmpi-1 == lexIndex and lex[lexIndex].type == 'ID' and lex[tmpi].type in typeAssignables+('INDEX',):
                            rParen-=1 ; break
                    if rParen == tmp: # normal
                        pass
                    else: # expression which can be print (no functions or assignment)
                        line.append(decideIfIndentLine(indent,f'{expPrint[-1]}('))
                        bigWrap=True
                elif tok.type == 'WITHAS' and tok.value.startswith('as'): tok.value=f' {tok.value}'
                elif tok.type == 'WITHAS' and not lastIndent[4]: lastIndent[4] = indent
                elif tok.type == 'RPAREN':
                    parenScope-=1
                    if parenScope < 0:
                        return AS_SyntaxError(f'Too many closing parenthesis.', ' '.join(line), lineNumber, data)
                elif tok.type == 'FSTR': # idFSTR
                    if startOfLine and (lastType in typeNewline+('ELSE','DEFFUNCT') or inLoop[0]):
                        check=True # if pipe then dont assume print , else do
                        tmp=False
                        for t in range(lexIndex + 1, len(lex)):
                            if lex[t].type in typeNewline: break
                            elif lex[t].type == 'PIPE' and tmp: check = False
                            elif lex[t].type == 'FSTR': tmp=check=True
                        if tok.value.replace('f','').startswith('"""') or tok.value.replace('f','').startswith("'''"): check = False
                        if tok.value.replace('F', '').startswith('"""') or tok.value.replace('F', '').startswith("'''"): check = False
                        if check: line.append(decideIfIndentLine(indent,expPrint[-1]+'(')) ; bigWrap=True ; rParen+=1
                    elif lastType in {'ID','INDEX','LISTEND','RINDEX','RPAREN'}:
                        # varName f"number: {num}"  -->  varName = f"number: {num}"
                        tmpCheck=True if inIf else False
                        for t in range(lexIndex-1,0,-1):
                            if lex[t].type in typeNewline: break
                            elif lex[t].type == 'FSTR': tmpCheck = False; break
                            elif not inIf:
                                if lex[t].type == 'LOOP': break
                                elif lex[t].type == 'ID' and lex[t-1].type in typeNewline+('ELSE',): tmpCheck=True

                        if tmpCheck:
                            line.append('== ' if inIf else '=')

                    if fstrQuote in tok.value and ',' in tok.value and fstrQuote in tok.value.split(',')[0]:
                        # splitting it from COMMAGRP
                        for splitIndex in range(len(tok.value)-1,0,-1):
                            if tok.value[splitIndex] == fstrQuote:
                                tok.value = tok.value[:splitIndex+1] # fstring

                    if tok.value[-1] == fstrQuote:
                        # end of fstr
                        fstrQuote=''
                        if tenary == False and lex[lexIndex+1].type == 'ELSE':
                            lex.insert(lexIndex+1,makeToken(tok,'then','THEN'))
                        else: tenary=False
                        # TODO: fix this case (compiles incorrectly) v
                        # 1 if f"{func(0,1) if True else False}x" else 2
                    elif fstrQuote=='':
                        for i in tok.value:
                            if i == '"': fstrQuote=i ; break
                            elif i == "'": fstrQuote=i ; break
                        if fstrQuote == tok.value[-1] and tok.value.count(fstrQuote) == 2 and tok.value[-2] != '\\':
                            # convert to regular string
                            tok.value=tok.value[1:]
                            tok.type='STRING'
                            fstrQuote=''
                    for tmpi in range(lexIndex+1,len(lex)-1):
                        # convert all strings inside fstring to be opposite quote
                        if lex[tmpi].type == 'STRING' and lex[tmpi].value[0] == fstrQuote:
                            if fstrQuote == '"': lex[tmpi].value = "'"+lex[tmpi].value[1:-1]+"'"
                            elif fstrQuote == "'": lex[tmpi].value = '"' + lex[tmpi].value[1:-1] + '"'
                        elif lex[tmpi].type == 'FSTR': break
                elif tok.type == 'SCOPE':
                    tmp=' '.join(tok.value.split()[1:]).split(',')
                    for t in tmp:
                        ttmp=t.strip()
                        if ttmp in storedVarsHistory and 'value' in storedVarsHistory[ttmp]:
                            del storedVarsHistory[ttmp]['value']
                            # if function changes variable, then the last value may not be valid
                elif tok.type in {'MINUS','BITWISE'}:
                    if tok.type in codeDict: tok.value=codeDict[tok.type]
                    if startOfLine and not inIf:
                        doPrint = True
                        for tmpi in range(lexIndex + 1, len(lex) - 1):
                            if lex[tmpi].type in typeNewline:
                                break
                            elif lex[tmpi].type not in typePrintable:
                                doPrint = False
                        if doPrint: line.append(
                            decideIfIndentLine(indent, f'{expPrint[-1]}(')); bigWrap = True; rParen += 1


                if tok.type == 'LBRACKET': bracketScope+=1
                elif tok.type == 'RBRACKET':
                    if bracketScope>0: bracketScope-=1
                elif tok.type == 'LPAREN': parenScope+=1

                if tok.type != 'IGNORE': line.append(decideIfIndentLine(indent,tok.value))
                if tok.type == 'WITHAS' and tok.value.startswith('with'):
                    indentSoon=True ; indent+=prettyIndent
                elif tok.type == 'ENDIF' and bracketScope < 1:
                    tmpScope=0
                    for t in range(lexIndex,0,-1): # ENDIFs can mess up indexes
                        if lex[t].type in ('LINDEX','LIST'): tmpScope+=1
                        elif lex[t].type in ('RINDEX','LISTEND'): tmpScope-=1
                        elif lex[t].type in typeNewline: break
                    if tmpScope < 1:
                        inIf=indentSoon=False ; startOfLine=True
                        if lastType=='ELSE' and tenary==False:
                            if line[0].strip() == 'else': pass
                            else:
                                code[-1]+=':\n' ; line=[]
                        else:
                            if ':' not in line[-1]:
                                line.append(':')
                            line.append('\n')
                            if combineLines:
                                if isinstance(line,str): line=[line]
                                line.insert(0,code[-1])
                                code.pop()
                                combineLines=False
                            code.append(''.join(line)) ; line=[]
                        if lex[lexIndex+1].type not in typeNewline:
                            tmptok=copy(tok)
                            tmptok.type='TAB' ; tmptok.value=f"\n{' '*indent}"
                            lex.insert(lexIndex+1,tmptok) ; del tmptok
                        elif lex[lexIndex+1].type == 'NEWLINE':
                            lex[lexIndex+1].type='TAB' ; lex[lexIndex+1].value=f"\n{' '*indent}"
                    elif lex[lexIndex+1].type not in typeNewline: tok.type = 'COLON'
            elif tok.type in {'INS','LIST','COMMAGRP','BOOL','DICT','SET','ASYNC','RETURN','BREAK','LAMBDA'}: # printing value with space
                if tok.type == 'INS':
                    if 'are in' in tok.value: tok.value='in'
                    elif inIf and inLoop[0]:
                        # handles: for x in array then for y in x
                        # might need to keep track of parenscopes?
                        for tmpi in range(lexIndex,len(lex)-1):
                            if lex[tmpi].type in typeNewline:
                                if lex[tmpi].type == 'TAB': lex[tmpi].type='THEN'
                                break
                    elif optimize and tok.value.strip() == 'in' and not inLoop[0] and listScope<=0 and optFuncTricksDict['inTo__contains__'] and lex[lexIndex+1].type == 'ID' and checkVarType(lex[lexIndex+1].value, ('LIST','TUPLE')) and lex[lexIndex-1].type != 'INS' and lex[lexIndex+2].type not in {'LINDEX','LIST'}:
                        # xValue in yList --> yList.__contains__(xValue)
                        # faster in Pyston , though not for small lists
                        # better to optimize for the large cases probably unless length can be determined
                        lex[lexIndex-1].type = tok.type = 'IGNORE'
                        autoMakeTokens(f".__contains__({lastValue})",lexIndex+1)
                        line=line[:-1]

                    if startOfLine and not inIf:
                        doPrint=True
                        for tmpi in range(lexIndex + 1, len(lex)-1):
                            if lex[tmpi].type in typeNewline:
                                break
                            elif lex[tmpi].type not in typePrintable:
                                doPrint = False
                        if doPrint: line.append(decideIfIndentLine(indent,f'{expPrint[-1]}(')); bigWrap = True; rParen += 1

                # 1,2,3 in a
                if tok.type == 'COMMAGRP': # idCOMMAGRP
                    if lastType == 'ID' and lex[lexIndex-1].value!='print' and inIf == False and findEndOfFunction(lexIndex-1,goBackwards=True)==False:
                        line.append(' = ')
                    elif lastType in 'TYPE' and lex[lexIndex-2].type in typeNewline+('CONST',) and '=' in tok.value:
                        tmp=tok.value.split('=')[0].split(',')
                        for t in tmp: storedVarsHistory[t.replace(' ','')]={'type':convertType[lastValue],'staticType':True}
                    else:
                        tmp = [_ for _ in REfindall(r"""(?:["\'].*?["\'])|(\[\])""",tok.value) if _ and _[0] not in {'"',"'"}]
                        # ^ discards quotes
                        listScope += tmp.count('[')
                        listScope -= tmp.count(']')
                elif tok.type == 'LIST': inIf=True
                elif tok.type == 'RETURN':
                    if 'yield' not in tok.value and lastType not in typeNewline:
                        line.append('\n') ; startOfLine=True
                    inIf=True ; inReturn=True



                if tok.type in {'LIST','BOOL','DICT','SET'} and startOfLine and lexIndex+1 < len(lex) and lex[lexIndex+1].type in typeNewline and lex[lexIndex-1].type not in typeConditionals and inIf==False:
                    line.append(decideIfIndentLine(indent,f'{expPrint[-1]}({tok.value})'))
                elif (fstrQuote!='' or (lastType == 'ID' and line and line[-1][-1] != ' ')) and tok.type != 'IGNORE':
                    line.append(decideIfIndentLine(indent, f' {tok.value} '))
                elif tok.type != 'IGNORE':
                    line.append(decideIfIndentLine(indent,f'{tok.value} '))
                if len(tok.value)>0 and tok.value[0]=='=': tok.value[1:] # removing = for  var = True
                if lexIndex+1 < len(lex) and lex[lexIndex+1].type=='PIPE' and (line==[] or lastType in typeNewline): startOfLine=True
            elif tok.type == 'IGNORE':
                pass
            elif tok.type == 'PYDEF': # support for Python style function define
                if tok.value.startswith('cdef') and compileTo != 'Cython': tok.value=tok.value[1:]
                elif tok.value.startswith('cpdef') and compileTo != 'Cython': tok.value=tok.value[2:]
                funcName = tok.value.split('def')[1].split('(')[0].replace(' ', '') # get function name
                if funcName not in storedCustomFunctions:
                    # create entry in storedCustomFunctions
                    storedCustomFunctions[funcName]={}

                tmpFuncArgs = REsearch(r'\((.*)\)(?=:|\n|$)',tok.value)
                if tmpFuncArgs:
                    # extracts out function argument variables and types
                    tmpFuncArgs = tmpFuncArgs.group()[1:-1]
                    tmpREChecks=(r'[^,]+\[.*\](?:,|$)',r'[^,]+?\((?:.*?,?)*?\)(?: *,|$)',r'[^,]+?\{(?:.*?,?)*?\}(?:,|$)',r'[^,]+(?:,|,?.*?$)')
                    tmpf=[]
                    for REcheck in tmpREChecks:
                        tmp=REfindall(REcheck, tmpFuncArgs)
                        if tmp:
                            for t in tmp:
                                if t:
                                    tmpFuncArgs = tmpFuncArgs.replace(t, '')
                                    tmpf.append(t)
                    tmpFuncArgs={}
                    for t in tmpf:
                        if ':' in t:
                            tmp=t.split(':')
                            tmp2=REsearch(r' *\w+ *(?!\=)',tmp[1])
                            if tmp2:
                                tmpFuncArgs[tmp[0].strip()]=tmp2.group().replace(',','').strip()
                            else:
                                tmpFuncArgs[tmp[0].strip()]=None
                        elif REsearch(fr' *(?:{"|".join(defaultTypes)}) +(?:([^\u0000-\u007F\s]|[a-zA-Z_])([^\u0000-\u007F\s]|[a-zA-Z0-9_])*)+ *,?',t):
                            # ^ if type, then space, and then var name
                            tmp=t
                            if tmp.endswith(','): tmp=tmp[:-1]
                            tmp=tmp.strip() ; tmp = tmp.split(' ')
                            tmpFuncArgs[tmp[1]]=tmp[0]
                            if compileTo != "Cython":
                                # converts to python type declaration
                                if '=' in t:
                                    tmp2=t.split('=')[0]
                                else: tmp2 = t
                                if tmp2.endswith(','): tmp2 = tmp2[:-1]
                                tok.value=tok.value.replace(tmp2,f"{tmp[1]}: {tmp[0]}")
                            else:
                                if tmp[0] == 'bool':
                                    tmpFuncArgs[tmp[1]]='bint'
                                    tok.value=tok.value.replace('bool '+tmp[1],'bint '+tmp[1])
                        else:
                            tmpFuncArgs[t.split('=')[0].strip()]=None
                    for arg in tmpFuncArgs:
                        if arg in ASnakeKeywords: reservedIsNowVar.append(arg)
                else: tmpFuncArgs={}
                storedCustomFunctions[funcName]['args'] = tmpFuncArgs

                if optimize and compileTo != 'MicroPython' and optFuncCache and checkIfImpureFunction(lex.index(tok),True, tmpFuncArgs ) == False:
                    optAddCache()

                if pythonVersion <= 3.08 and '->' in tok.value:
                    tmpRE=REcompile(r' *-> *(?:tuple|list|dict|set|type)')
                    tmp = REsearch(tmpRE, tok.value)
                    if tmp:
                        if pythonVersion >= 3.05:
                            tmp=tmp[0].split(' ')[-1]
                            insertAtTopOfCodeIfItIsNotThere(f'from typing import {tmp.capitalize()}')
                            tok.value=REsub(tmpRE,' -> '+tmp.capitalize(),tok.value)
                        else:
                            tok.value = REsub(r' *-> *(?:tuple|list|dict|set|type)+ *(\[.+ *, *.+\])?', '', tok.value)

                if tok.value.replace(' ','')[-1] != ':': tok.value+=':'
                line.append(decideIfIndentLine(indent,f'{tok.value}\n'))
                startOfLine=True ; indent+=prettyIndent
                #if ''.join(tok.value.split('(')[1:]).count(',') >= ''.join(tok.value.split('(')[1:]).count('=') and ''.join(tok.value.split('(')[1:]).count(',') > 0:
                #    pass # if it has required arguments, dont auto add () when called without it
                #else:
                #    tmp=tok.value.split('def')[1].split('(')[0].replace(' ','')
                #    if tmp not in storedCustomFunctions: storedCustomFunctions[tmp]={}
                #    storedCustomFunctions[tmp]['type']=None
            elif tok.type == 'PYPASS':
                if indent > 0 and tok.value[3] not in {'\t',' ','\n'}:
                    line.append(decideIfIndentLine(indent,tok.value[2:][:-2]))
                else: line.append(tok.value[2:][:-2])
            elif tok.type == 'TRY': # idTRY
                if ':' not in tok.value: tok.value+=':'
                tmp = False
                if tok.value.startswith('except'):
                    if tok.value[:-1].strip() == 'except' and (lex[lexIndex+1].type not in typeNewline+('NOTHING','STRING','NUMBER','LBRACKET','LPAREN','LIST') or (lex[lexIndex+1].type == 'NOTHING' and lex[lexIndex+2].type == 'PIPE')):
                        tok.value=tok.value[:-1] ; tmp=True
                    if lastType not in {'NEWLINE','TAB'}:
                        if lastType!='THEN':
                            line.append('\n') ; startOfLine=True
                        if startOfLine: indent-=prettyIndent
                        if indent<0: indent=0
                line.append(decideIfIndentLine(indent, f'{tok.value}'))
                if not tmp: startOfLine = True
                else: indentSoon=True
                indent += prettyIndent

            elif tok.type == 'NRANGE':
                if lastType == 'ID':
                    if lex[lexIndex-2].type in typeNewline+('TYPE','CONST'):
                          line.append('= ')
                    elif lex[lexIndex-2].value.strip() != 'print': line.append('== ')
                if 'to' in tok.value:
                    tmp=tok.value.split('to')
                    line.append(decideIfIndentLine(indent,f"range({tmp[0]}, {tmp[1]})"))
                else:
                    tmp=tok.value.split('.'*tok.value.count('.'))
                    negative=False
                    number1=number2=None
                    if tmp[0].isdigit():
                        number1=int(tmp[0])
                    elif tmp[0] in storedVarsHistory and storedVarsHistory[tmp[0]]['type']=='NUMBER':
                        if 'value' in storedVarsHistory[tmp[0]]:
                            number1=int(storedVarsHistory[tmp[0]]['value'])
                    if tmp[-1].isdigit():
                        number2=int(tmp[-1])
                    elif tmp[-1] in storedVarsHistory and storedVarsHistory[tmp[-1]]['type']=='NUMBER':
                        if 'value' in storedVarsHistory[tmp[-1]]:
                            number2=int(storedVarsHistory[tmp[-1]]['value'])
                    if number1!=None and number2!=None and number1 > number2:
                        negative=True

                    if tok.value.count('.') == 2:
                        if tmp[-1].isdigit(): tmp[-1]=int(tmp[-1])+1
                        else: tmp[-1]+='+1'
                    if negative:
                        line.append(decideIfIndentLine(indent,f'range({tmp[0]},{tmp[-1]},-1)'))
                    else: line.append(decideIfIndentLine(indent,f'range({tmp[0]},{tmp[-1]})'))

                if lastType == 'INS' and lastValue in {'in ','in'} and lex[lexIndex+1].type not in typeNewline+('ENDIF',):
                    # if its a for loop and experession is after the NRANGE, asssume indent
                    check=False
                    for tmpi in range(lexIndex,0,-1):
                        if lex[tmpi].type in typeNewline: break
                        elif lex[tmpi].type == 'FOR' and lex[tmpi-1].type in typeNewline:
                            check=True ; break
                    if check:
                        indentSoon=True
                        lex.insert(lexIndex+1,makeToken(tok,'do','THEN'))

            elif tok.type == 'META': # idMETA
                tmpf=tok.value.split('=')[0].replace(' ','').replace('$','') # needs replace instead of strip()
                if tmpf in metaDefExp:
                    tmp=tok.value[tok.value.index('=')+1:]
                    if tmp.strip() == '':
                        expPrint.append('')
                    else:
                        while tmp[0] == ' ': tmp=tmp[1:]
                        while tmp[-1] == ' ': tmp=tmp[:-1]
                        if '$' in tmp:
                            if tmp.count('$') > 1:
                                for tmptmp in tmp.split('$'):
                                    if tmptmp.strip() in inlineReplace:
                                        tmp = tmp.replace('$' + tmptmp, inlineReplace[tmptmp.strip()])
                            elif tmp[0] == '$' and tmp.replace('$','') in inlineReplace:
                                tmp = inlineReplace[tmp.replace('$','')]
                        expPrint.append(tmp)
                    expPrint[0]=indent
                elif tmpf in metaIgnoreIndent:
                    ignoreIndentation = metaHandling(tok.value, ignoreIndentation)
                elif tmpf in metaPyFunc:
                    functionPassing = metaHandling(tok.value, functionPassing)
                elif tmpf in metaPyIs:
                    pyIs = metaHandling(tok.value, pyIs)
                elif tmpf in metaPyCompat:
                    pyCompatibility = metaHandling(metaCall, pyCompatibility)
                    if pyCompatibility:
                        pyIs=True ; functionPassing=True ; expPrint.append('') ; autoEnumerate=False ; intVsStrDoLen=False
                elif tmpf in {'asnake','Asnake','ASnake'}:
                    pyIs=False ; functionPassing=False
                elif tmpf in {'Cython','cython'}:
                    compileTo='Cython'
                elif tmpf in {'Python','python'}:
                    compileTo='Python'
                elif tmpf in inlineReplace:
                    tmp=[]
                    for t in miniLex(inlineReplace[tmpf] + ' '):
                        tmp.append(t) # need to reverse it, so append to temporary list we then reverse
                    for t in reversed(tmp):
                        lex.insert(lexIndex+1,t)
                        if debug: print('--', t)
                elif tmpf in metaPyVersion:
                    tmp = float(fixVersionNumber(tok.value.split('=')[-1].strip()))
                    if tmp != pythonVersion:
                        code.append(f'# Compile target changed to {compileTo}{tmp}')
                    pythonVersion = tmp
                    # in pre-phase it already checked if it was float, no need for try except
                elif tmpf in metaIgnoreDefExpFunc:
                    metaDefaultExpressionWithFunction = metaHandling(tok.value, metaDefaultExpressionWithFunction)



            elif tok.type == 'INC': # idINC
                if lexIndex-1 >= 0 and (lex[lexIndex-1].type in typeNewline+('TRY','ENDIF','ELSE') or (lexIndex-3>0 and (lex[lexIndex-3].type=='LOOP' or lex[lexIndex-2].type=='LOOP'))):
                    if lex[lexIndex+1].type in typeNewline+typeConditionals or inIf:
                        doPrint=False
                    else:
                        doPrint=True
                        for tmpi in range(lexIndex+1,len(lex)):
                            if lex[tmpi].type in typeNewline: break
                            elif lex[tmpi].type not in typePrintable+('INC',):
                                doPrint=False
                else: doPrint=False
                if tok.value.startswith('++') or tok.value.startswith('--'):
                    if lastType in ('IF','ELIF') and pythonVersion >= 3.08:
                        tmp=tok.value[2:]
                        line.append(f'({tmp} := {tmp} {tok.value[0]} 1)')
                    else:
                        tmp=tok.value[2:]+' '
                        if lastType == 'ELIF':
                            return AS_SyntaxError(f"{tok.value} cannot be after an elif when compiling to version {pythonVersion}",f'if {tok.value}',lineNumber,data)
                        if doPrint: tmp=f'{expPrint[-1]}({tmp}' ; bigWrap=True ; rParen+=1
                        if (lex[lexIndex+1].type not in typeNewline or lex[lexIndex-1].type not in typeNewline) and lastType == 'INC': line.append('== ')
                        elif lastType in typeAssignables: line.append(' == ')
                        if inIf and lastIndent[2]: oldIndent = indent ; indent = lastIndent[2][-1]


                        startOfLine=True
                        if code[-1].endswith('= '):
                            code.insert(-1,f'{tok.value[2:]}{tok.value[0]}=1\n')
                            line.append(decideIfIndentLine(indent,tok.value[2:]))
                        else:
                            if lastType == 'IF':
                                code.insert(-1,decideIfIndentLine(lastIndent[2][-1],f'{tok.value[2:]}{tok.value[0]}=1\n'))
                                line.append(tmp)
                            elif lastType == 'WHILE':
                                # increment at start
                                bigWrap = True
                                tmpf = f'{tok.value[2:]}{tok.value[0]}=1\n'
                                tmp = False
                                for iw in range(0, len(incWrap)):
                                    if incWrap[iw][0] == tmpf:
                                        incWrap[iw][1] += 1;
                                        tmp = True
                                if not tmp:
                                    incWrap += [[tmpf, 1]]
                                line.append(tok.value[2:])
                            elif inIf and (lastIndent[2] or pythonVersion >= 3.08):
                                if pythonVersion >= 3.08:
                                    line.append(f'({tmp} := {tmp} {tok.value[0]} 1)')
                                else:
                                    tmp=False
                                    for i in range(lexIndex,0,-1):
                                        if lex[i].type in {'IF','WHILE'} and lex[i-1].type in typeNewline: tmp=True ; break
                                        elif lex[i].type in typeNewline: break
                                    if not tmp:
                                        return AS_SyntaxError(f"{tok.value} cannot be after an of when compiling to version {pythonVersion}",f'if {tok.value}', lineNumber, data)

                                    code.insert(-1, decideIfIndentLine(lastIndent[2][-1],f'{tok.value[2:]}{tok.value[0]}=1\n'))
                                    line.append(decideIfIndentLine(indent,tmp))
                            else:
                                line.insert(0,decideIfIndentLine(indent,f'{tok.value[2:]}{tok.value[0]}=1\n'))
                                if lex[lexIndex+1].type in typeNewline and lex[lexIndex-1].type in typeNewline:
                                    pass
                                else:
                                    line.append(decideIfIndentLine(indent,tmp))
                        if inIf: indent = oldIndent
                elif tok.value.endswith('++') or tok.value.endswith('--'):
                    bigWrap=True
                    tmp=tok.value[:-2]+' '

                    tmpIndent = None
                    tmpInWhile = False
                    for tmpi in range(lexIndex-1,0,-1):
                        if lex[tmpi].type in typeNewline:
                            if lex[tmpi].type == 'TAB':
                                tmpIndent = lex[tmpi].value.replace('\t', ' ').count(' ')
                                break
                            elif lex[tmpi].type == 'NEWLINE':
                                tmpIndent = 0
                                break
                        elif lex[tmpi].type == 'WHILE' and not tmpIndent: tmpInWhile = True

                    if tmpInWhile:
                        if tmpIndent:
                            tmpIndent+=prettyIndent
                            tmpLastIndent = tmpIndent
                        else: tmpLastIndent=0
                        # increment at end
                        line.append(tmp)
                        for tmpi in range(lexIndex+1,len(lex)):
                            if tmpIndent == None:
                                if lex[tmpi].type in typeNewline:
                                    if lex[tmpi].type == 'TAB':
                                        tmpIndent = lex[tmpi].value.replace('\t',' ').count(' ')
                                    elif lex[tmpi].type == 'NEWLINE': tmpIndent = 0
                                    tmpLastIndent = tmpIndent
                            else:
                                if lex[tmpi].type in typeNewline:
                                    found=False
                                    if lex[tmpi].type == 'TAB' and lex[tmpi].value.replace('\t', ' ').count(' ') < tmpIndent:
                                        found=True
                                    elif lex[tmpi].type == 'NEWLINE': found=True
                                    else:
                                        if lex[tmpi].type == 'TAB':
                                            tmpLastIndent = lex[tmpi].value.replace('\t', ' ').count(' ')
                                        if lex[tmpi+1].type in typeConditionals and tmpLastIndent:
                                            tmpLastIndent+=prettyIndent
                                    if found:
                                        lex.insert(tmpi, copy(lex[lexIndex]))
                                        if tmpLastIndent > tmpIndent:
                                            if tmpIndent == 0: tmpIndent = prettyIndent
                                            lex.insert(tmpi,makeToken(lex[tmpi],f"\n\t{' '*tmpIndent}",'TAB'))
                                        else:
                                            lex.insert(tmpi, makeToken(lex[tmpi],'then','THEN'))
                                        break

                    else:
                        if doPrint: tmp=f'{expPrint[-1]}({tmp}' ; rParen+=1
                        if lastType == 'INC': line.append('== ')
                        if lex[lexIndex+1].type not in typeNewline or lex[lexIndex-1].type not in typeNewline:
                            line.append(decideIfIndentLine(indent,tmp))

                        tmpf=f'{tok.value[:-2]}{tok.value[-1]}=1\n'
                        tmp=False
                        for iw in range(0,len(incWrap)):
                            if incWrap[iw][0] == tmpf:
                                incWrap[iw][1]+=1 ; tmp = True
                        if not tmp:
                            incWrap += [[tmpf, 1]]
            elif tok.type == 'DIVMOD':
                if lexIndex+1 < len(lex) and lexIndex-1 > 0:
                    if (lex[lexIndex-1].type == 'NUMBER' or  (lex[lexIndex-1].type == 'ID' and lex[lexIndex-1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-1].value]['type'] in ('NUMBER','int','float')))\
                    and (lex[lexIndex+1].type == 'NUMBER' or  (lex[lexIndex+1].type == 'ID' and lex[lexIndex+1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+1].value]['type'] in ('NUMBER','int','float')) or (lex[lexIndex+1].type=='INDEX' and lex[lexIndex+1].value[0] in {'1','2','3','4','5','6','7','8','9','0'})):
                        if lex[lexIndex+1].type == 'INDEX':
                            line=line[:-1] ; lex[lexIndex+1].type='IGNORE'
                            line.append(f'divmod({lex[lexIndex-1].value},{lex[lexIndex+1].value.split("[")[0]})[{"".join(lex[lexIndex+1].value.split("[")[1:])}')
                        else:
                            line=line[:-1] ; lex[lexIndex+1].type='IGNORE'
                            line.append(f'divmod({lex[lexIndex-1].value},{lex[lexIndex+1].value})')
                    else: return AS_SyntaxError('/// needs a number or variable containing a number datatype before and after it','myVar is 12 then a /// 3',lineNumber,data)
                else:
                    return AS_SyntaxError('/// needs a number before and after it','12 /// 3',lineNumber,data)
            elif tok.type == 'PYCLASS':
                line.append(decideIfIndentLine(indent,tok.value))
                indent+=prettyIndent
                if tok.value[-1] != ':':
                    indentSoon=True
            elif tok.type in typeCheckers:
                check = True
                if intVsStrDoLen:
                    tmpAllowedLen={'STRING','LIST','LISTCOMP','DICT','TUPLE','SET'}
                    if (lastType in tmpAllowedLen or (lexIndex-1 > 0 and lex[lexIndex-1].type=='ID' and lex[lexIndex-1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-1].value]['type'] in tmpAllowedLen)) \
                    and lexIndex+1 < len(lex) and (lex[lexIndex+1].type in {'NUMBER','INC'} or ((lex[lexIndex+1].type=='ID' and lex[lexIndex+1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+1].value]['type']=='NUMBER'))):
                        line[-1]=line[-1].replace(lex[lexIndex-1].value,f"len({lex[lexIndex-1].value})")
                        line.append(decideIfIndentLine(indent,f'{codeDict[tok.type]} '))
                        check = False
                    elif (lastType in {'NUMBER','INC'} or (lexIndex-1 > 0 and lex[lexIndex-1].type=='ID' and lex[lexIndex-1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-1].value]['type']=='NUMBER')) \
                    and lexIndex+1 < len(lex) and (lex[lexIndex+1].type in tmpAllowedLen or ((lex[lexIndex+1].type=='ID' and lex[lexIndex+1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+1].value]['type'] in tmpAllowedLen and lex[lexIndex+2].type not in {'LINDEX','INDEX'}))):
                        tmp=False
                        for tmpi in range(lexIndex+1,len(lex)-1):
                            if lex[tmpi].type in typeNewline: break
                            elif lex[tmpi].type in {'RINDEX','LISTEND'}:
                                lex[tmpi].value+=')' ; tmp=(True,tmpi) ; break
                        line.append(f"{codeDict[tok.type]} len(")
                        if tmp == False: line[-1]+=f'{lex[lexIndex+1].value})'; lex[lexIndex+1].type = 'IGNORE'
                        else:
                            if lex[tmp[1]+1].type == 'PIPE' and lex[tmp[1]+2].value == 'len':
                                lex[tmp[1]+2].type='IGNORE' ; lex[tmp[1]+1].type='IGNORE';lex[tmp[1]+1].value='IGNORE'
                        check = False


                    if check and pythonVersion >= 3.08:
                        checkForWalrus = False
                        if lastType == 'RPAREN'  \
                        and lexIndex+1 < len(lex) and (lex[lexIndex+1].type in tuple(tmpAllowedLen)+('NUMBER','INC') or ((lex[lexIndex+1].type=='ID' and checkVarType(lex[lexIndex+1].value,tuple(tmpAllowedLen)+('NUMBER','INC'))))):
                            tmpRange=(range(lexIndex-1,0,-1),False) # False stands for backward
                            checkForWalrus=True
                        elif ((lastType in {'NUMBER','INC'} or (lexIndex-1 > 0 and lex[lexIndex-1].type=='ID' and checkVarType(lex[lexIndex-1].value,'NUMBER'))) \
                        or (lastType == 'RPAREN' and lex[lexIndex-3].type == 'ASSIGN' and ':' in lex[lexIndex-3].value and ((lex[lexIndex-2].type == 'ID' and checkVarType(lex[lexIndex-2].value,'NUMBER')) or (lex[lexIndex-2].type == 'NUMBER') ) )) \
                        and lex[lexIndex+1].type == 'LPAREN':
                            tmpRange = (range(lexIndex+1, len(lex)-1), True)  # True stands for forward
                            checkForWalrus = True

                        if checkForWalrus:
                            # check for walrus , and its type
                            tmpParenScope = 0
                            tmpFoundWalrus = tmpDoLen = False
                            for tmpi in tmpRange[0]:
                                if lex[tmpi].type == 'RPAREN':
                                    tmpParenScope-=1
                                    if (tmpRange[1] and tmpParenScope <= 0):
                                        if tmpDoLen:
                                            lex[lexIndex + 1].type = 'FUNCTION'
                                            lex[lexIndex + 1].value = 'len('
                                            check = False
                                        break
                                elif lex[tmpi].type in {'FUNCTION','LPAREN'}:
                                    if lex[tmpi].type == 'FUNCTION':
                                        tmpParenScope += lex[tmpi].value.count('(')
                                        tmpParenScope -= lex[tmpi].value.count(')')
                                    else:
                                        tmpParenScope += 1
                                    if (not tmpRange[1] and tmpParenScope >= 0):
                                        if tmpDoLen:
                                            lex[tmpi].type='FUNCTION'
                                            lex[tmpi].value='len('
                                            # since it is already compiled we must mess with line
                                            # kinda jank unforunately
                                            tmptmpParenScope=tmpTindex=-1
                                            for t in range(0,len(line)-1):
                                                if '(' == line[t]: tmpTindex=t
                                                tmptmpParenScope += line[t].count('(')
                                                tmptmpParenScope -= line[t].count(')')
                                                if tmpTindex >= 0 and ':=' in line[t]:
                                                    line[tmpTindex]='len('
                                                    break
                                            check = False
                                        break
                                elif lex[tmpi].type == 'ASSIGN':
                                    if ':' in lex[tmpi].value:
                                        tmpFoundWalrus=True
                                        if lex[tmpi+2].type == 'RPAREN' and (lex[tmpi+1].type in {'NUMBER','INC'} or (lex[tmpi+1].type == 'ID' and checkVarType(lex[tmpi+1].value,{'NUMBER','INC'}))):
                                            lex[lexIndex+1].value = f"p!len({lex[lexIndex+1].value})!p" ; lex[lexIndex+1].type = 'PYPASS' ; break
                                        elif lex[tmpi+2].type == 'RPAREN' and (lex[tmpi+1].type in tmpAllowedLen or (lex[tmpi+1].type == 'ID' and checkVarType(lex[tmpi+1].value,tmpAllowedLen))):
                                            tmpDoLen=True
                                    else: break
                                elif lex[tmpi].type in typeNewline+typeConditionals: break
                                elif not tmpRange[1] and tmpFoundWalrus and lex[tmpi].type in {'ID','BUILTINF'}:
                                    if lex[tmpi].value in storedVarsHistory \
                                    and 'type' in storedVarsHistory[lex[tmpi].value] \
                                    and storedVarsHistory[lex[tmpi].value]['type'] in tmpAllowedLen:
                                        tmpDoLen=True
                                elif tmpRange[1] and lex[tmpi].type in {'ID','BUILTINF'} and lex[tmpi+1].type == 'ASSIGN' and ':' in lex[tmpi+1].value \
                                and lex[tmpi].value in storedVarsHistory and 'type' in storedVarsHistory[lex[tmpi].value] \
                                and storedVarsHistory[lex[tmpi].value]['type'] in tmpAllowedLen:
                                    tmpFoundWalrus = tmpDoLen = True

                            if not check: line.append(f'{codeDict[tok.type]} ')

                if check: # standard pass
                    if fstrQuote != '': line.append(f'{codeDict[tok.type]}') # for fstr formatting
                    else: line.append(decideIfIndentLine(indent,f'{codeDict[tok.type]} '))
            elif tok.type == 'RINDEX':
                listScope -= 1
                if listScope < 0: listScope = 0

                tmpSafe=True
                if lex[lexIndex+1].type in typeAssignables and lex[lexIndex+1].type in {'LINDEX','LIST'}:
                    # check for if index, and if next is index, and cancel assignment/comparison if so
                    tmpListScope=listScope
                    for tmpi in range(lexIndex,0,-1):
                        if lex[tmpi].type in typeNewline: break
                        elif lex[tmpi].type in {'LIST','LINDEX'}: tmpListScope -= 1
                        elif lex[tmpi].type in {'LISTEND', 'RINDEX'}: tmpListScope += 1
                        elif tmpListScope == 0 and lex[tmpi].type in {'ID','BUILTINF'} and lex[tmpi+1].type in {'LIST','LINDEX'}:
                            tmpSafe = False

                if lex[lexIndex+1].type in typeAssignables and tmpSafe and lex[lexIndex+1].type != 'LISTEND':
                    if inIf: line.append(tok.value+' == ')
                    else: line.append(tok.value+' = ')
                else: line.append(tok.value)
            elif tok.type == 'LINDEX':
                listScope+=1
                if lastType == 'ID' and lex[lexIndex-2].type == 'TYPE':
                    line.append(' = '+tok.value)
                else: line.append(tok.value)
            elif tok.type == 'DEFEXP':
                if lex[lexIndex+1].type == 'ID' and lex[lexIndex+1].value.strip() == 'print': # todo: replace with noParenFunc
                    pass
                else:
                    line.append(decideIfIndentLine(indent, f'{expPrint[-1]}(')) ; rParen+=1 ; bigWrap=True
            elif tok.type == 'DONTDEXP':
                line.append(decideIfIndentLine(indent, ''))
            elif tok.type == 'HEXDEC':
                tok.type='NUMBER' ; line.append(decideIfIndentLine(indent,tok.value))
            elif tok.type in codeDict:
                if tok.type == 'DEFFUNCT': notInDef=False
                elif lastType == 'BUILTINF' and tok.type in {'AND','OR'} and not startOfLine:
                    line.append(' ')
                if tok.value in ASnakeKeywords and tok.value in reservedIsNowVar:
                    line.append(decideIfIndentLine(indent,f'{tok.value} '))
                else:
                    line.append(decideIfIndentLine(indent,f'{codeDict[tok.type]} '))

            lastType=tok.type
            lastValue=tok.value

            if tok.type not in {'ENDIF','FROM'} or (startOfLine and tok.type in typeConditionals) or inIf==False:
                if not storedIndents: storedIndents=[0]
                elif indent>storedIndents[-1]:
                    storedIndents.append(storedIndents[-1]+prettyIndent)
                elif indent<storedIndents[-1]: storedIndents.pop()


                # vvv uncomment for debugging indentation via including it as comment.
                #if lastType!='DEFFUNCT':
                #    tmp=f'\n# {tok.type,indent,storedIndents,lastIndent[:2]}\n'
                #    if startOfLine and comment and code[-1]!=tmp: code.append(tmp)
            # vvv uncomment to see coresponding AS file line included as comment
            #if lastType!='DEFFUNCT':
            #        tmpf=data.split("\n")[lineNumber-1].replace("\t","")
            #        tmp=f'\n# AS Line {lineNumber-1}: {tmpf}\n'
            #        if startOfLine and comment and code[-1]!=tmp: code.append(tmp)

    if len(line) > 0:
        code.append(''.join(line))
    if compileTo == 'Cython':
        keepAtTop.append(makeToken(lex[0], '#cython: language_level=3', 'CYLEVEL'))
    if keepAtTop:
        tmp=[] ; order=('CYLEVEL','SHEBANG','STRING','IMPORT')
        for o in order:
            for t in keepAtTop:
                if t.type == o:
                    tmp.append(t)
        for t in reversed(tmp):
            code.insert(0,t.value)
    if debug: print('len of lex',len(lex)-1)
    showWarning=True
    if outputInternals:
        metaInformation=[inlineReplace,expPrint,ignoreIndentation,functionPassing,pyIs,autoEnumerate,intVsStrDoLen,metaDefaultExpressionWithFunction,functionLineWrap]
        return ('\n'.join(code), lex, storedVarsHistory,metaInformation)
    else:
        return '\n'.join(code)

