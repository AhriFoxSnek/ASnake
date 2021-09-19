# -*- coding: utf-8 -*-

# dependencies
from sly import Lexer
from autopep8 import fix_code

# standard library
import os
from copy import deepcopy
from time import time # just for timing how long stuff takes, not really needed
import re
from keyword import iskeyword

ASnakeVersion='v0.11.7'

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

    return f'print("""\n{showError}\n""")'
    
defaultTypes='bool|int|float|complex|str|list|tuple|set|dict|bytearray|bytes|enumerate|filter|frozenset|map|memoryview|object|property|range|reversed|slice|staticmethod|super|type|zip'


import ast
import operator
_OP_MAP = {
    ast.Add: operator.add,
    ast.Sub: operator.sub,
    ast.Mult: operator.mul,
    ast.Div: operator.truediv,
    ast.FloorDiv: operator.floordiv,
    ast.Invert: operator.neg,
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
        left = self.visit(node.left)
        right = self.visit(node.right)
        return _OP_MAP[type(node.op)](left, right)
    def visit_Num(self, node):
        return node.n
    def visit_Expr(self, node):
        return self.visit(node.value)
    @classmethod
    def evaluate(cls, expression):
        tree = ast.parse(expression)
        calc = cls()
        return str(calc.visit(tree.body[0]))


class Lexer(Lexer):

    #@_(r'\n+')
    #def ignore_newline(self, t):
    #    self.lineno += len(t.value)

    # Set of token names.   This is always required
    tokens = { ID, NUMBER, PLUS, MINUS, TIMES,
               DIVIDE, RDIVIDE, ASSIGN, LPAREN, RPAREN, STRING, NEWLINE,
               GREATER, GREATEQ, LESS, LESSEQ, EQUAL, ELIF, IF, ELSE, THEN,
               LISTCOMP, WHILE, IGNORENL, NOTHING, FUNCTION, NOTEQ,
               LIST, BUILTINF, INS, IMPORT, TAB, CASE, OF, PIPE, INDEX,
               PYPASS, AND, OR, DEFFUNCT, FROM, RETURN, TYPE, COMMAGRP, FOR,
               END, ARE, ANYOF, BOOL, COMMA, CONSTANT, TRY, LOOP, STRLIT, 
               STRRAW, SET, DICT, MODULO, ASYNC, NRANGE, EXPONENT, PYDEF,
               META, SCOPE, BITWISE, DIVMOD, TYPEWRAP, SHEBANG, COMMENT,
               FUNCMOD, WITHAS, LISTEND, ENDIF, INC, BREAK, LBRACKET, RBRACKET,
               LAMBDA, PYCLASS, ELLIPSIS, BINARY, PIPEGO, DQUOTE, SQUOTE, HEXDEC
             }

    # String containing ignored characters between tokens
    ignore  = ' \t '
    #ignore_comment = r'(?=(\t| ))*?#.+?(#|(?=\n|$))'

    # Regular expression rules for tokens
    SHEBANG = r'#(!| *cython:) *.*'
    COMMENT = r'''(?=(\t| ))*?#.*?(!#|(?=\n|$))'''
    TAB     = r'\n([\t| ]+)'
    NEWLINE = r'\n'
    PYPASS  = r"p!(.|\n)+?!p"
    META    = r'\$ *?((\w+(?!.*=)(?=[\n \]\)\$]))|(.*((=.*)|(?=\n)|$)))'
    FUNCMOD = r'@.*'
    PYDEF   = r'c?def +([\w.\[\d:,\] ]* )?\w+ *\(((?!: *return)[^)])*\)*( *((-> ?)?\w* *):?)'
    PYCLASS = r'class ([a-z]|[A-Z])\w*(\(.*\))?:'
    STRLIT  = r'(r|f)?\"\"\"[\w\W]+?\"\"\"|(r|f)?\'\'\'[\w\W]+?\'\'\''
    INDEX   = r'''((((\w[\w\d_]*)|(\)( |\t)*)|(?!.*("|')))(\[(((.(?! \w))*?:[^#:\n\[]*?)|(([^,])*?)|.*\(.*\))[^\[,\n]*\])+(?= *[\.+)-\\*\n=\w]| *))|\[ *\])'''
    LIST    = r'\['
    LISTEND = r'\]'
    DICT    = r'''(?!['"].*){([^{}]*:(.*?),?\n?)*}(?= then|do|[ +\-\/*\n\[\];])'''
    IF      = r'if(?=[\W\d\n(])'
    ELIF    = r'(, )?elif(?= |\t|\()'
    ELSE    = r'(, *)?else(?= |\n|\t|:|\()'
    FUNCTION= r'\w+\('
    NRANGE  = r'(-?(\d+|\w+(\(.\))?)\.\.\.?(-?\d+|\w+(\(.\))?))|-?\d+ ?to ?-?\d+'
    BUILTINF= """(([a-zA-Z_]+\d*|[^\s\d='";()+\-*]*|(f|u)?\"[^\"]*\"|(f|u)?\'[^\"\']*\')\.[a-zA-Z_]+\d*)+"""
    COMMAGRP= """(?!\[)(([\[\w\d\]=.-]|(((f|r)?\"[^\"]*\")|((f|r)?\'[^\']*\')))+ ?,)+([\[\]\w\d=.-]|((f|r)?\"[^\"]*\")|((f|r)?\'[^\']*\'))+"""
    TRY     = r'((try)|(except *[A-Z]\w*( as \w*)?)|(except)|(finally)) *:?'
    TYPEWRAP= fr'({defaultTypes})( ?\[\w*\])? *: *(#.*)?\n'
    TYPE    = '\\s%s\\s'%f'({defaultTypes})'
    LAMBDA  = r'lambda ?(\w* *,?)*:'
    LISTCOMP= r'\-?\w*: \w+'
    STRING  = r'((f|u|b)?\"(?:\\\\|\\\"|[^\"])*\")|((f|u|b)?\'(?:\\\\|\\\'|[^\'])*\')'
    #SET    = r'{.+?}'
    LBRACKET= r'{(.*:)?'
    RBRACKET= r'}'
    STRRAW  = r"""f?r((f?\"(\\"|.)+?\")|(f?\'(\\'|.)+?\'))"""
    IMPORT  = r"""(^|(?! )|from +[^'"\n ]*) ?c?import [^\n;]*"""
    EQUAL   = r'==|equals?(?= |\t)'
    NOTEQ   = r'!=|isnt|isn\'t|not equal|unequal'
    BITWISE = r'\^|~|\||&|(<<)|(>>)'
    LESSEQ  = r'(<=)|(=<)'
    LESS    = r'<|((is )?less (than )?)|(lesser (than )?)'
    GREATEQ = r'(>=)|(=>)'
    ASSIGN  = r'''=|is( |(?=("|'|{|\[|\()))|(\+|-|\*\*?|\/\/?|:|%)='''
    ENDIF   = r': *'
    DEFFUNCT= r'does(?= |\n|\t)'
    SCOPE   = r'(global|local|nonlocal) (\w* *,?)*'
    THEN    = r'then\s|do\s|then do\s|, then\s|, do\s|, then do\s|;|(:(?=\n)+)'
    WITHAS  = r'(with|(?![^\w\d])as) '
    WHILE   = r'while|until'
    NOTHING = r'(pass|nothing,?)(?= |$|\n)'
    CASE    = r'case +'
    OF      = r'of( |(?=[\W\d]))'
    END     = r'end(?=\s|;)'
    PIPE    = r'(into|to)(?!\S)'
    PIPEGO  = r'pipe(?= |\t)'
    AND     = r'and(?= |\t)'
    OR      = r'or(?= |\t)'
    FROM    = r'from(?= |\t)'
    RETURN  = r'(return|yield +from|yield|del|raise|assert)(?=[\W\d\n]|$)'
    BREAK   = r'(break|continue)(?= |\n|\t)'
    FOR     = r'for(?= |\t)'
    LOOP    = r'loop(?= |\n)'
    ASYNC   = r'(async|await)(?= |\t)'
    CONSTANT= r'const(ant)?'
    ANYOF   = r'(any|all) of'
    INS     = r'(not|in)( |(?=\n))'
    ARE     = r"(arent|aren\'t|are)(?= |\n|\t)"
    BOOL    = r'True|False|None'
    MODULO  = r'%|remainder(?= |\n|\t)'
    INC     = r'((\+{2}|\-{2})[^\[\]\(\)\+\-\/\*\d\s,=][^\s\+\-\/\*,\(\)=]*(\[.*\])?)|([^\[\]\(\)\+\-\/\*\d\s,=][^\s\+\-\/\*,=]*(\[.*\])?(\+{2}|\-{2}))'
    HEXDEC  = r'0x[\da-f]+'
    NUMBER  = r'(0x\d*)|(( \-\d|\d)\d*\.?\d*(e(-|\+)\d+)?\.?_*\d*j?(?!\w+))'
    BINARY  = r'0(o|O|x|X|b|B)\d+'
    MINUS   = r'-|minus(?= |\t)'
    PLUS    = r'\+|plus(?= |\t)'
    EXPONENT= r'\*\*|power(?= |\t)'
    TIMES   = r'\*|times(?= |\t)'
    GREATER = r'>|greater (than )?'
    ID      = r'([^\u0000-\u007F\s]|[a-zA-Z_])([^\u0000-\u007F\s]|[a-zA-Z0-9_])*'
    ELLIPSIS= r'\.\.\.'
    DQUOTE  = r'"'
    SQUOTE  = r"'"
    
    COMMA   = r','
    IGNORENL= r'\\'
    DIVMOD  = r'///'
    RDIVIDE = r'//'
    DIVIDE  = r'/'
    LPAREN  = r'\(|\['
    RPAREN  = r'\)|]'
    

def build(data,optimize=True,comment=True,debug=False,compileTo='Python',pythonVersion=3.9,enforceTyping=False):
    codeDict={'RDIVIDE':'//','DIVIDE':'/','PLUS':'+','MINUS':'-','TIMES':'*','LPAREN':'(','RPAREN':')',
    'ELIF':'elif','ELSE':'else','IF':'if','WHILE':'while','GREATEQ':'>=','GREATER':'>','LESS':'<',
    'LESSEQ':'<=','EQUAL':'==','ASSIGN':'=','NOTHING':'pass','NOTEQ':'!=','BUILTINF':'.','OF':'elif',
    'AND':'and','OR':'or','RETURN':'return','FOR':'for','MODULO':'%','EXPONENT':'**','COMMA':',',
    'LISTEND':']','ELLIPSIS':'...','constLPAREN':'(','COLON':':','LINDEX':'[','RINDEX':']',
    "DQUOTE":'"',"SQUOTE":"'"}
    
    convertType={'int':'NUMBER','float':'NUMBER','Py_ssize_t':'NUMBER','bool':'BOOL','bint':'BOOL','str':'STRING','list':'LIST','dict':'DICT','type':'TYPE','tuple':'TUPLE','set':'SET','bytes':'STRING'}
    
    # useful types of sets of tokens or other things
    typeAssignables=('STRING','NUMBER','ID','LIST','LISTEND','DICT','BINARY','LBRACKET')
    typeOperators=('PLUS','MINUS','TIMES','DIVIDE','RDIVIDE','EXPONENT','BITWISE','MODULO')
    typeCheckers=('LESS','LESSEQ','GREATEQ','GREATER', 'EQUAL', 'PYIS')
    typePrintable=typeAssignables+typeOperators+typeCheckers+('LINDEX','RINDEX','INDEX','LPAREN','RPAREN','MODULO','IGNORE','INC','INS')
    mopConv={'TIMES':'*=','PLUS':'+=','DIVIDE':'/=','RDIVIDE':'//=','MINUS':'-='}
    typeMops=tuple(i for i in mopConv)
    typeConditonals=('IF','ELIF','ELSE','OF','WHILE')
    typeNewline=('NEWLINE','TAB','THEN','END')
    # useful sets of strings
    listMods=('.pop','.append','.extend','.insert','.remove','.reverse','.sort','.copy','.clear')
    pyBuiltinFunctions=('abs', 'delattr', 'hash', 'memoryview', 'set', 'all', 'dict', 'help', 'min', 'setattr', 'any', 'dir', 'hex', 'next', 'slice', 'ascii', 'divmod', 'id', 'object', 'sorted', 'bin', 'enumerate', 'input', 'oct', 'staticmethod', 'bool', 'int', 'open', 'str', 'breakpoint', 'isinstance', 'ord', 'sum', 'bytearray', 'filter', 'issubclass', 'pow', 'super', 'bytes', 'float', 'iter', 'print', 'tuple', 'callable', 'format', 'len', 'property', 'type', 'chr', 'frozenset', 'list', 'range', 'vars', 'classmethod', 'getattr', 'locals', 'repr', 'zip', 'compile', 'globals', 'map', 'reversed', 'complex', 'hasattr', 'max', 'round', 'exec', 'eval', '__import__')
    pyReservedKeywords=('False', 'None', 'True', 'and', 'as', 'assert', 'async', 'await', 'break', 'class', 'continue', 'def', 'del', 'elif', 'else', 'except', 'finally', 'for', 'from', 'global', 'if', 'import', 'in', 'is', 'lambda', 'nonlocal', 'not', 'or', 'pass', 'raise', 'return', 'try', 'while', 'with', 'yield')
    metaPyCompat = {'pythonCompatibility','pycompat','pyCompatibility','pyCompat','pythonCompat'}
    metaPyVersion = {'version','pythonVersion','pyver','PythonVersion','pyVersion'}

    if compileTo == 'PyPy3': pythonVersion=3.7
    elif compileTo == 'Cython': pythonVersion=3.6
    elif compileTo == 'Pyston': pythonVersion=3.8

    if isinstance(pythonVersion, str) and pythonVersion.isdigit():
        pythonVersion=int(pythonVersion)

    if compileTo == 'Cython': code=['#cython: language_level=3\n# Cython compiled by ASnake '+ASnakeVersion]
    else: code=[f'# Python{pythonVersion} compiled by ASnake '+ASnakeVersion]


    def makeToken(clone,value=None,type=None):
        tmptok=deepcopy(clone)
        if value != None: tmptok.value=value
        if type != None: tmptok.type=type
        return tmptok

    def convertEmojiToAscii(tokValue):
        tmp = ''
        if compileTo == 'Cython':
            cutoff = 256  # cython likes ascii only
        else:
            cutoff = 9000  # is first emoji
        for c in tokValue:
            if ord(c) > cutoff:
                if tmp:
                    tmp += str(ord(c)) + 'e'
                else:
                    tmp += 'e' + str(ord(c)) + 'e'
            else:
                tmp += c
        return tmp

    def createFString(tok,token=-1):
        nonlocal lex, lexIndex, debug
        # handle making FSTRING tokens for parsing inside of fstrings
        # turned out optStrFormatToFString needed this so its a function now
        tmpf = [] ; parts = []
        found = checkForEscape = isDict = False
        insideIndex = 0

        for x in tok.value:
            # get quote type
            if x == '"':
                quote = x; break
            elif x == "'":
                quote = x; break

        for part in range(0, len(tok.value)):
            # print(tok.value[part],part,found,checkForEscape,parts)
            if found:
                if checkForEscape:
                    if tok.value[part] == ':' and insideIndex <= 0 and parts != []:
                        isDict = True
                    elif tok.value[part] == '[':
                        insideIndex += 1
                    elif tok.value[part] == ']':
                        insideIndex -= 1
                    # elif tok.value[part] == '{': checkForEscape=False
                if tok.value[part] == '}':
                    if isDict:
                        parts[-1] = (parts[-1][0], parts[-1][1] - 1)
                        parts.append((False, part + 1))
                        isDict = found = False
                    else:
                        parts.append((False, part));
                        found = False

            elif found == False and tok.value[part] == '{':
                tmp = 0
                while part + tmp < len(tok.value) - 1 and tok.value[part + tmp] == '{':
                    tmp += 1;
                    checkForEscape = True
                found = True;
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
        # [print(f.type,f.value) for f in tmpf]
        tok.type = 'IGNORE';
        miniLex = Lexer()
        adjust=1
        for thing in tmpf:
            if thing.type == 'FSTR':
                if token == -1:
                    lex.append(thing)
                else:
                    lex.insert(token+adjust, thing); adjust+=1
                lexIndex += 1
            else:
                pruneDict = False
                if ':=' in thing.value:
                    lex.append(makeToken(tok, '{', 'LBRACKET'))
                    pruneDict=True
                tmp=thing.value[1:-1] if pruneDict else thing.value
                for tmptok in miniLex.tokenize(tmp + ' '):
                    if tmptok.type == 'STRING':
                        if tmptok.value[0] == '"' and quote == '"':
                            tmptok.value = tmptok.value.replace('"', "'", 1)
                            tmptok.value = tmptok.value[:tmptok.value.rfind('"')] + "'"
                        elif tmptok.value[0] == "'" and quote == "'":
                            tmptok.value = tmptok.value.replace("'", '"', 1)
                            tmptok.value = tmptok.value[:tmptok.value.rfind("'")] + '"'
                    elif tmptok.type == 'ID' and tmptok.value.isascii() == False:
                        tmptok.value = convertEmojiToAscii(tmptok.value)
                    if token == -1:
                        lex.append(tmptok)
                    else: lex.insert(token+adjust,tmptok) ; adjust+=1
                    lexIndex += 1
                if pruneDict:
                    lex.append(makeToken(tok, '}', 'RBRACKET'))
                    lexIndex += 1
        del miniLex
        return tok

    # v converts leading tabs to space v
    leadingTabs = re.compile(r"""(?<=\n)\t+(?![\w\s]*('|"){3})""")
    while re.search(leadingTabs, data):
        data = re.sub(leadingTabs, '    ', data)

    #meta
    inlineReplace={}
    comments=[]
    prettyIndent=4
    
    lexer = Lexer()
    lex=[]
    lexIndex=0
    currentTab=0
    bracketScope=0
    lastIndent=[0]
    tabBackup=[currentTab,lastIndent[:]]
    inFrom=False
    crunch=False # for smooshing values into the latest lexIndex
    reservedIsNowVar=[]
    # warning to self, when checking previous token, do not do lexIndex-1, lexIndex is the previous, as current token hasn't been added yet
    for tok in lexer.tokenize('\n'+data+' '):
        # ^^ needs newline at the start
        if crunch:
            if tok.type in typeNewline:
                crunch = False
            else:
                lex[lexIndex].value += tok.value
                continue

        if len(lex)==0: 
            tmptok=deepcopy(tok)
            tok.type='IGNORE'
            lex.append(tok)
            tok.type=tmptok.type
            del tmptok
        elif len(lex) > lexIndex:
            if debug: print(f'lex={lexIndex} lexType={lex[lexIndex].type}\ttype={tok.type}, value={tok.value}')
        
        if tok.type in ('BUILTINF','NUMBER'):
            if tok.value.endswith('\n'):
                tok.value=tok.value[:-1]
                lex.append(tok)
                tmptok=deepcopy(tok)
                if currentTab > 0: tmptok.value=f'\n{" "*currentTab}' ; tmptok.type='TAB'
                else: tmptok.value='\n' ; tmptok.type='NEWLINE'
                lex.append(tmptok)
                lexIndex+=1
                del tmptok
            else:
                if tok.type == 'NUMBER' and pythonVersion < 3.6 and '_' in tok.value: tok.value = tok.value.replace('_', '')
                lex.append(tok)
        elif tok.type == 'HEXDEC':
            tok.type = 'NUMBER' ; lex.append(tok)
        elif tok.type == 'IMPORT' and tok.value.startswith('import this'): # The ASnake Rebellion
                code.append(''.join(("import zlib as ASnake\n","myDress = ",str(b'x\x9c\x95UK\x8f\xda0\x10\xbe\xf3+\xac\xed\x05\xa4he;\xce\xc3U\xf7\xd4\xd3J\x95\xfa:"\x0e\x04\xc2n*6a\t\xa9\xd4\x7f_?\xc7\xe3$@\x8bD\x94xf\xbe\xf9\xe6\xe9E\xf5\xb4.EB\x185\x0f\x96\x90\x94\'$\xcf\x12R\xa6\xea\x80\xd1\x84\xc8BK\n$.\xb9\xfb\x90\xa5{a\xb4\x84G\xa6\xed\x983\x16\xc2\x9a\x18M\x8e\xe0\xadC&\x9c\x85>.\xa8\xb7\xe4\xfa\xcd\xfd\x8b\xd4J\x19\x93\x00\\\x803\xea\x85\xc2\x11e9\x0e\xc6\x9c\x83\x7f\xcf\xdf\xd8\x1am\xce\x11\xad`k\x91\xe8\xc8\xd4\x9e\x06\'&\n\x8e\xf9\x88\xdcr\xce\x0b\xaf-"\xb7\x1c"p\xb4\x8co\x14\x94\xd1gY\x9cU>\xe21\xc2\t\xf8\xf6h\x9a\x00\x83\xe8\xb9\x95\x05\x14\xc9k\xce\xe4\xc4*P@\xc9<\n\xa8{\x04\xf4\x95y\x1e\x19\xa4\xc7\xd4\x8cC(\x0c\xf1\x80\xae\xb3\x14CaSL\xeb:\x96:\x932\x8aE\'\xd3\x13\xf4m\xa3U\xa2r:0\xdf\xdd3y\x94\xd0h3\xf4]o\xc6>o\xa7\xf6\xff\xf1\xee\xc7\xe0\x99\xdb/9\x89\xc3S*r(a\x8e\xb5!\xd6\x12[\xc1\xa8\xc7\xe3\x80:L\xe0v\x0f\xbd!\xa2r\xb1+\x03\x95\xcf\x02\x8f\x92?368|4\xad\xfe\x98{\x16\x01<0\x80md\x08\xb3r\xe4\xca*\x85\xd0`\xa9\x85a\x93(\x9b\xa65\xf9\xec\xec\x86\xba\xc00Hhg\x08\xc8\x16\x07\x86(\x8d\xca\x18/\x1a\x81\n^xWx\xc3\x8c{\n\xad\xa6h\xf2\xd1\x1c\x8d&-^\xa1Yl\x19oL9\xe6\x8e\x86g\xbe\xae7W\xd2\xb4\x82q\xf1\xd1J\xa4\x8e\xe4\xbd\x056\xe3\xdd\xae\x92\xb0\xa7s\xcc\n\xeah{\xc4\xdfi\xeeAq\xa83\xc8W\xb6[H\x94KP\x1e\xddK\xd1\xdd\x18O2^\xb4Z(1\xe10\x02\xf9\xa4\xc6\x02\x94T[xIX\x19\x05.\xed?\\\xee\xf6\x9e\xb5\xf1\xf9I@\xb7W\xd4\xc8\xa3}\x92n\x16\xc7\xa6\xbf|=\xfc\xa8wu{\xf9v\xae\xfbf\xaf^\xfa\xe7\xef?w\x9d\xfazZo\x16\x87\xeeL\xaa\x8a4-9o\xdb\x97zI\x93c\xdd.\xab\xd5\xea#\xf9\xf0L\x8e\xc3o2(\xf9\x82\xa8\xdfm\xb0\xc7\xed\xe9T\xb7\xfbe\xb5\xae\xaa\xcd\xca\x18Tu\x7f\xf9Ro_\x86\xfa\xf3\xeb\xf6\xed\xd4t\xad\xf6\xa8%\xdak\xf3>\xf6z\xdb\x81\xa2dl\xf5\xaf9\xdca\xb3n\xde7\xe4\x13a)Uq\xfc\xe9\x06\xd2\xf4d?\xbcU\x1d@\xcc\x13\xf4Q\xec^\xcfw\xe8h\x0f\xab\xd5\xe2tn\xda\xcb\xf2\xe1\xe1\xf1W\xd7\xa8\xbcM\x00\x95\xca_\x02|\xfa\xf9'),"\n# who wears myDress the best?\nexec(ASnake.decompress(myDress)) ; del ASnake")))
        elif tok.type == 'ID':
            if any(i for i in defaultTypes.split('|') if i == tok.value) and tok.value.strip() not in reservedIsNowVar:
                tok.type='TYPE' # dumb fix for it not detecting types
            elif tok.value.isascii() == False:
                tok.value=convertEmojiToAscii(tok.value)
            lex.append(tok)
        elif tok.type == 'TYPEWRAP':
            tok.value=tok.value.replace(':','').replace('\n','').replace(' ','')
            if lex[lexIndex].type == 'DEFFUNCT': tok.type='TYPE'
            if tok.value in reservedIsNowVar:
                tok.type='ID' ; lex.append(tok)
                lex.append(makeToken(tok,'do','THEN')) ; lexIndex+=1
            else: lex.append(tok)
        elif tok.type in ('TAB','THEN'):
            if lex[lexIndex].type == 'IGNORENL': 
                lex[lexIndex].type='IGNORE'
                tok.type='IGNORE'
                lex=lex[:-1]
                lexIndex-=2
            else:
                if lex[lexIndex].type == 'FUNCTION' and lex[lexIndex].value[-1]=='(':
                    tok.type='IGNORE' # for when theres a tab after an opening function like print(
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
                if bracketScope > 0: tok.type = 'IGNORE'
                if tok.type=='IGNORE': lex.append(tok)
                else: 
                    if lex[lexIndex].type == 'TAB':
                        currentTab=tabBackup[0]
                        lastIndent=tabBackup[1][:]
                        lexIndex-=1
                        lex.pop()
                
                    # ---- this section converts 2 spaces to 4 (prettyIndent). very important
                    if tok.type == 'TAB':
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
                    lex.append(tok)
            inFrom=False
        elif tok.type == 'INDEX': # grouping [0][1] indexes
            if tok.value.startswith(')'):
                tok.value=tok.value[1:]
                while tok.value[0] in (' ','\t'): tok.value=tok.value[1:]
                tmptok=deepcopy(tok) ; tmptok.type='RPAREN' ; tmptok.value=')'
                lex.append(tmptok) ; lexIndex+=1
            if tok.value == '[]':
                tmptok=deepcopy(tok)
                tmptok.type='LIST' ; tmptok.value='['
                lex.append(tmptok) ; del tmptok
                tok.type='LISTEND' ; tok.value=']'
                lexIndex+=1
            elif lex[lexIndex].type=='INDEX' and tok.value.startswith('['):
                tok.value=f'{lex[lexIndex].value}{tok.value}'
                lex[lexIndex].type='IGNORE'
            elif tok.value.split('[')[0] in {'list','tuple','dict','set'} and tok.value.split('[')[1].split(']')[0] in defaultTypes.split('|'):
                # typing inside elements like dict[int, str]
                tok.type='TYPE'
            if tok.type != 'TYPE' and len(re.findall('''(?!('|").*)for (?!.*('|"))''',tok.value)) > 0:
                # the dumb INDEX regex will match list comps.
                # this will detect 'for' inside of it (if not surrounded by quotes)
                # it will parse the inside as ASnake, meanwhile surrounded by [ ]
                miniLex=Lexer()
                tmpval=tok.value.lstrip('[').rsplit(']',1)[0]
                tmpCount=0
                while tok.value[tmpCount] == '[':
                    tmpCount+=1
                for t in range(tmpCount):
                    tmptok=deepcopy(tok) ; tmptok.value='[' ; tmptok.type='LIST'
                    lex.append(tmptok) ; del tmptok ; lexIndex+=1
                for i in miniLex.tokenize(tmpval+' '):
                    if i.type not in typeNewline: # genius, you can go crazy with whitespace in listcomps
                        lex.append(i)
                        lexIndex+=1
                        if debug: print('--',i)
                del miniLex
                tmptok=deepcopy(tok) ; tmptok.value=']' ; tmptok.type='LISTEND'
                lex.append(tmptok) ; del tmptok
            else:
                if tok.type != 'TYPE' and '[' in tok.value:
                    miniLex = Lexer()
                    tmpf=tok.value.split('[',1)
                    tmp=list(miniLex.tokenize(tmpf[0]+' '))
                    if not tmp: tmp='ID'
                    else: tmp=tmp[-1].type
                    if len(tmpf[0]) > 0:
                        lex.append(makeToken(tok,tmpf[0],tmp)) ; lexIndex += 1
                    lex.append(makeToken(tok, '[', 'LINDEX'))
                    tmpscope=1
                    for i in miniLex.tokenize(''.join(tmpf[1:])+' '):
                        if i.type not in typeNewline:
                            if i.type == 'LISTCOMP':
                                miniminiLex = Lexer()
                                lex.append(makeToken(tok,':','COLON'))
                                lex.append([ii for ii in miniminiLex.tokenize(i.value.split(':')[-1])][0])
                                del miniminiLex
                                lexIndex += 2
                            else:
                                if i.type == 'ENDIF': i.type = 'COLON'
                                elif i.type == 'LIST':
                                    if tmpscope == 0: i.type='LINDEX'
                                    else: tmpscope+=1
                                elif i.type == 'LISTEND':
                                    if tmpscope == 0: i.type='RINDEX'
                                    else: tmpscope-=1
                                lex.append(i)
                                lexIndex+=1
                            if debug: print('--',i)
                    if lex[-1].type == 'LISTEND': lex[-1].type='RINDEX'
                    del miniLex
                else:
                    lex.append(tok)
        elif tok.type == 'FROM': inFrom=True ; lex.append(tok)
        elif tok.type == 'COMMAGRP': # splitting into proper tokens a,b,c = d if a,b,c=d
            if (lex[lexIndex].type != 'FROM' and inFrom==False) and tok.value.count('=')==1:
                tmp=tok.value.split(',')
                #print(len(re.findall(r'\b[^\'"\d]',','.join(tmp[:-1]))),tok.value.count(',')+1)
                if ('=' in tmp[-1] or tmp[-1].startswith('is') or tmp[-1].startswith(' is') )\
                and (len(re.findall(r'\b[^\'"\d]',','.join(tmp[:-1])))==tok.value.count(',')+1) \
                and len([i for i in tmp if (i[0] in ("'",'"') and i[-1] in ("'",'"'))])==0:
                    miniLex=Lexer()
                    for t in tmp:
                        for i in miniLex.tokenize(t+' '):
                                lex.append(i)
                                lexIndex+=1
                                if debug: print('--',i)
                        if t is not tmp[-1]:
                            tmptok=deepcopy(tok) ; tmptok.type='COMMA' ; tmptok.value=','
                            lex.append(tmptok) ; lexIndex+=1
                    del miniLex ; del tmptok
                    lexIndex-=1
                    
                else: lex.append(tok)
            else: 
                if inFrom:
                    miniLex=Lexer()
                    for i in miniLex.tokenize(re.sub(r"""(,(?=[^']*(?:'[^']*'[^']*)*$))|,(?=[^"]*(?:"[^"]*"[^"]*)*$)""",' , ',tok.value)):
                        lex.append(i)
                        lexIndex+=1
                        if debug: print('--',i)
                    del miniLex
                    lexIndex-=1 # i dont know why butff i think this works
                    inFrom=False
                else: lex.append(tok)
            if tok.value[-1] == ']' and (lex[lexIndex].type == 'LIST' or '[' not in tok.value):
                tok.value=tok.value[:-1]
                tmptok=deepcopy(tok)
                tmptok.type='LISTEND' ; tmptok.value=']'
                lex.append(tmptok) ; del tmptok ; lexIndex+=1
            elif re.match(r'([^\u0000-\u007F\s]|[a-zA-Z_])([^\u0000-\u007F\s]|[a-zA-Z0-9_])*\[', tok.value):
                # when a index is mistaken for commagrp
                if lex[lexIndex].type == 'TYPE':
                    lex[lexIndex].type = 'ID' ; reservedIsNowVar.append(lex[lexIndex].value.strip())
                miniLex = Lexer()
                tmp=tok.value.split('[')[0]+' ['+'['.join(tok.value.split('[')[1:])
                for i in miniLex.tokenize(tmp+' '):
                    lex.append(i)
                    lexIndex += 1
                    if debug: print('--', i)
                del miniLex
                tok.type='IGNORE'
        elif tok.type in {'STRAW','STRLIT','STRING'}:
            if tok.type in ('STRRAW','STRLIT'): tok.type='STRING'
            if tok.value[0]=='f':
                if len([i for i in ('{','}') if i not in tok.value])>0:
                    tok.value=tok.value[1:] # optimization if f-string doesnt use {} then convert to regular string, better performance
                else:
                    tok=deepcopy(createFString(tok))
            lex.append(tok)
        elif tok.type == 'META':
            if tok.value.replace('$', '').replace(' ', '').startswith('def'):
                # hacky thing to make def an alias of inline. do dont make it elif
                if tok.value[tok.value.index('def')+3]==' ':
                    tok.value='$ inline '+tok.value.split('def',1)[-1]
            metaCall = tok.value.replace('$', '').replace(' ', '')
            if metaCall.startswith('inline'):
                name=''.join(tok.value.split('=')[0].split('inline')[1]).replace(' ','')
                value='='.join(tok.value.split('=')[1:])
                inlineReplace[name] = ''.join(value)
                lexIndex-=1
            elif metaCall.startswith(tuple(metaPyVersion)):
                tmp=tok.value.split('=')[-1].strip()
                try:
                    pythonVersion=float(tmp)
                except ValueError:
                    return AS_SyntaxError(f'Meta {tok.value.split("=")[0].strip()[1:]} must be given a float/decimal.',f'$ {tok.value.split("=")[0].strip()[1:]} = 3.8', None, data)
                lexIndex -= 1
            elif tok.value.split('=')[0].replace(' ','').replace('$','') in inlineReplace or any(i for i in inlineReplace if f'${i}' in tok.value):
                if debug: print('inlineReplace',inlineReplace)
                miniLex=Lexer()
                while '$ ' in tok.value: tok.value = tok.value.replace('$ ', '$')
                while tok.value!='' and len([i for i in inlineReplace if f'${i}' in tok.value]) > 0:
                    tmp=tok.value.split()[0]
                    if ',' in tmp: tmp=tmp.split(',')[0]
                    tmp=tmp.strip()
                    tmp=[i for i in inlineReplace if f'${i}' == tmp]
                    if debug: print(tok.value)
                    if len(tmp)>0:
                        tmp=tmp[0]
                        for t in miniLex.tokenize(inlineReplace[tmp]+' '):#inlineReplace[tok.value.split('=')[0].replace(' ','').replace('$','')]+' '):
                            lex.append(t) ; lexIndex+=1
                            if debug: print('--',t)
                        #print(tok.value.lstrip('$'+tmp)) ; exit()
                        while tok.value[0]==' ': tok.value=tok.value[1:]
                        tok.value=tok.value.lstrip('$'+tmp)
                    for t in miniLex.tokenize(tok.value.rsplit('$')[0]+' '):
                        tok.value=tok.value.replace(t.value,'')
                        lex.append(t) ; lexIndex+=1
                        if debug: print('---',t)
                lexIndex-=1 # i dont know why but i think this works
                del miniLex
            else: lex.append(tok)
        elif tok.type == 'LISTCOMP':
            check=False # checks for    if thing > 4: doStuff
            for tmpindex in range(len(lex)-1,0,-1):
                if lex[tmpindex].type in typeNewline:
                    break
                elif lex[tmpindex].type in typeConditonals or lex[tmpindex].type in ('WHILE','FOR'):
                    check=True ; break

            if tok.value.startswith('else'):
                # else: gets detected as LISTCOMP , so convert to ELSE
                tok.type='ELSE' ; lex.append(tok)
                # then split the string and detect & insert the next token
                tmptok=deepcopy(tok)
                tmptok.value=''.join(tok.value.split(':')[1:])
                miniLex=Lexer()
                tmptok.type=[i for i in miniLex.tokenize(tmptok.value+' ')][0].type
                lex.append(tmptok) ; lexIndex+=1 ; del miniLex
            elif lex[lexIndex].type == 'IF' or check:
                tmptok=deepcopy(tok)
                miniLex = Lexer()
                for t in miniLex.tokenize(''.join(tmptok.value.split(':')[:1])):
                    lex.append(t) ; lexIndex += 1
                    if debug: print('--', t)

                tmptok=deepcopy(tok)
                tmptok.type = 'THEN' ; tmptok.value = 'then '
                lex.append(tmptok) ; lexIndex+=1

                tmptok=deepcopy(tok)
                tmptok.value=''.join(tok.value.split(':')[1:]).strip()
                if tmptok.value not in ('',' '):
                    tmptok.type=[i for i in miniLex.tokenize(tmptok.value+' ')][0].type
                    lex.append(tmptok) 
                del miniLex
            else: lex.append(tok)
        elif tok.type in ('NEWLINE','COMMENT'):
            if lex[lexIndex].type == 'TAB':
                lex.pop() ; lexIndex-=1
                if len(lastIndent)>1: 
                    # if last was tab, revert to not mess up indentation scope
                    currentTab=tabBackup[0]
                    lastIndent=tabBackup[1][:]
            if tok.type == 'COMMENT':
                skip=False
                if tok.value[-1]=='#':
                    for tmpi in range(lexIndex,0,-1):
                        if lex[tmpi].type in typeNewline:
                            tmp=tmpi-1
                            if len(comments) > 0 and comments[-1][1] == tmp:
                                comments[-1][0]=comments[-1][0]+'\n'+tok.value
                                skip=True ; lexIndex-=1
                            break
                else: tmp=lexIndex
                if not skip: comments.append([tok.value,tmp]) ; lexIndex-=1
            else:
                if lexIndex>0 and lex[lexIndex].type == 'IGNORENL': lexIndex-=1 # newline is not real if IGNORENL \
                else: inFrom=False ; lex.append(tok) # newline
        elif tok.type == 'ASSIGN':
            if ':' in tok.value:
                for tmpi in range(lexIndex,0,-1):
                    if lex[tmpi].type in typeNewline: tok.value='=' ; break
                    elif lex[tmpi].type in typeConditonals+('LPAREN',):
                        if lex[tmpi].type != 'ELSE': break
                        else: tok.value='=' ; break
                lex.append(tok)
            elif lex[lexIndex].type == 'LISTCOMP':
                tmp=lex[lexIndex].value.split(':') ; tmp[-1]=tmp[-1].replace(' ','')
                if tmp[-1] in convertType:
                    lex[lexIndex].type='TYPE' ; lex[lexIndex].value=tmp[-1]
                    tmptok=deepcopy(lex[lexIndex])
                    tmptok.type='ID' ; tmptok.value=tmp[0]
                    lex.append(tmptok) ; lexIndex+=1 ; del tmptok
                lex.append(tok)
            elif lex[lexIndex].type == 'PIPEGO':
                lex[lexIndex].type='ID' ; lex.append(tok) ; reservedIsNowVar.append('pipe')
            elif lex[lexIndex].type != 'ID' and (any(True for x in {'nothing', 'minus', 'plus', 'times', 'greater', 'end', 'of', 'case', 'until', 'then', 'do', 'does', 'less', 'than', 'equals', 'power', 'remainder'} if x == lex[lexIndex].value) \
            or any(True for x in [i for i in convertType]+defaultTypes.split('|') if x == lex[lexIndex].value)):
                # !! allows reassignment of reserved keywords !!                    add any new ones here ^
                lex[lexIndex].type = 'ID' ; lex.append(tok) ; reservedIsNowVar.append(lex[lexIndex].value)
            else: lex.append(tok)
        elif lex[lexIndex].type not in typeNewline and tok.type in ('LOOP','WHILE'):
            # loop/while can act as new expression indicators
            lex.append(makeToken(tok,'then','THEN')) ; lex.append(tok) ; lexIndex+=1
        elif tok.type == 'FUNCTION' and lex[lexIndex].type == 'ID' and lex[lexIndex].value.strip() == 'def' and 'def' not in reservedIsNowVar:
            miniLex = Lexer()
            lex[lexIndex].type='IGNORE'
            for t in miniLex.tokenize(lex[lexIndex].value + ' ' + tok.value):
                lex.append(t)
                lexIndex+=1
            lexIndex -= 1
            crunch = True
        elif tok.type == 'FUNCTION' and tok.value[-1]=='(' and tok.value[:-1].strip() in pyReservedKeywords and tok.value[:-1].strip() not in reservedIsNowVar:
            miniLex = Lexer()
            for t in miniLex.tokenize(tok.value.replace('(',' (')+' '):
                if debug: print('---',t)
                lex.append(t)
                lexIndex+=1
            lexIndex -= 1
            del miniLex
        elif tok.type == 'LBRACKET':
            bracketScope+=1 ; lex.append(tok)
        elif tok.type == 'RBRACKET':
            bracketScope-=1 ; lex.append(tok)
        elif tok.type == 'ENDIF' and bracketScope > 0: tok.type='COLON' ; lex.append(tok)
        else:
            if reservedIsNowVar and tok.value in reservedIsNowVar: tok.type='ID'
            lex.append(tok)
        if lex[lexIndex].type == 'TYPE' and tok.type != 'ID' and lex[lexIndex-1].type not in ('PIPE','COMMA','FROM','CONSTANT','DEFFUNCT'):
            lex[lexIndex].type='ID' ; reservedIsNowVar.append(lex[lexIndex].value.strip())
        lexIndex+=1
    lex=[l for l in lex if l.type not in ('IGNORE','IGNORENL')]

    tmptok=deepcopy(lex[-1])
    tmptok.type='NEWLINE' ; tmptok.value='\n'
    lex.append(tmptok) ; del tmptok ; #del inlineReplace
    # ^ need newline at the end , some stuff relies on that


    
    # optimize section
    if optimize == True:

        for tok in lex: # removes wording for the compilerNumberEval function
            if tok.type in ('TIMES', 'PLUS', 'DIVIDE', 'MINUS'):
                tok.value = codeDict[tok.type]

        def compilerNumberEval(toks):
            if isinstance(toks[0],str) == False:
                toks=[f.value for f in toks]
            if len(toks)==1: return toks[0]
            return Calc.evaluate(''.join([f for f in toks]))

        # idOPTARGS
        # vv you can choose to disable specific optimizations
        optFromFunc=True
        optFuncTricks=True
        optFuncTricksDict={ 'randint':True,
                            'listString':True,
                            'TupleSetUnpack':True,
                            'collapseToFalseTrue':True,
                            'EvalLen':True,
                            'optCythonTypeFromConversionFunction':True,
                            'dictlistFunctionToEmpty':True,
                            'boolTonotnot':True,
                            }
        optConstantPropagation=True
        optMathEqual=True
        optListToTuple=True
        optInSet=True
        optWalrus=True
        optLoopAttr=True
        optStrFormatToFString=True
        optCompilerEval=True
        optPow=True
        optDeadVariableElimination=True
        # v these are done in main phase v
        optIfTrue=True
        optSortedToSort=True
        optListPlusListToExtend=True
        optLoopToMap=True
        optFuncCache=True

        # vv incompatible optimizations vv
        if compileTo == 'Cython':
            optWalrus=optStrFormatToFString=optLoopAttr=False
            # v compatible but slower v
            optFuncCache=False
        elif compileTo == 'PyPy3':
            # v seems to be slower for some reason on PyPy but faster on Python v
            optFuncCache=optLoopToMap=optListPlusListToExtend=False
        elif compileTo == 'Pyston':
            # v slower v
            optFuncTricksDict['boolTonotnot']=False

        if pythonVersion < 3.6:
            optStrFormatToFString=False
        elif pythonVersion < 3.8:
            optWalrus = False


        # meta
        pyCompatibility=False

        definedFuncs=[]
        wasImported={}
        doNotModThisToken=[]
        newOptimization=True
        optimizeRound=0
        while newOptimization: # continue to optimize until there is nothing left
            if debug:
                #print(' '.join([t.value for t in lex]))
                print('\t- optimization round =',optimizeRound,'-')
            optimizeRound+=1
            newOptimization=False
            token=0
            for blah in range(0,(len(lex)-1)*2):
                if token <= len(lex)-1:
                    if not optimize:
                        if lex[token].type!='META': continue
                        elif lex[token].type=='META':
                            metaCall = lex[token].value.replace('$', '').replace(' ', '').lower()
                            if metaCall.split('=')[0] in {'optimize', 'optimization', 'optimizing'}:
                                if '=' in metaCall:
                                    if metaCall.split('=')[-1].lower() == 'true':
                                        optimize = True
                                    elif metaCall.split('=')[-1].lower() == 'false':
                                        optimize = True
                                else:
                                    optimize = not optimize
                    #if debug: print('!',blah,token,lex[token])
                    if lex[token].type == 'ID':
                        if optConstantPropagation:
                            tmpi=None
                            if lex[token-1].type not in typeConditonals+('OR','AND','LOOP'):
                                if lex[token+1].type in typeAssignables and lex[token+1].type not in ('LISTEND',) and lex[token+2].type not in ('PIPE','LISTCOMP'):
                                    tmpi=1
                                elif lex[token+1].type == 'ASSIGN' and lex[token+1].value.strip() in ('is','=') and lex[token+2].type in typeAssignables+('LPAREN','LBRACKET','FUNCTION') and lex[token+2].type not in ('LISTEND',) and lex[token+3].type not in ('PIPE','LISTCOMP'):
                                    tmpi=2
                            if lex[token].value == 'print' or lex[token-1].type == 'COMMA': tmpi=None
                            if optMathEqual and token+3 < len(lex) and lex[token+2].value == lex[token].value and lex[token+3].type in typeOperators: tmpi=None
                            # ^ optMathEqual comes online after constant folding, which messes stuff up. so if we detect it, then dont perform folding
                            # v if in from for asnake function def, or inside conditional, then ignore
                            if tmpi != None:
                                for t in range(token,0,-1):
                                    #print('---',lex[t].type)
                                    if lex[t].type == 'FROM': tmpi=None ; break
                                    elif lex[t].type in typeConditonals: tmpi=None ; break
                                    elif lex[t].type == 'FSTR': tmpi = None; break
                                    elif lex[t].type in ('LPAREN','LOOP','FUNCTION'): tmpi=None ; break
                                    elif lex[t].type in typeNewline: break

                            if tmpi != None and lex[token + tmpi + 1].type == 'LINDEX': tmpi=tmpf=None
                            if tmpi != None and token+tmpi < len(lex):
                                tmpf=[] # get expression
                                vartype=lex[token+tmpi].type
                                listScope=0 ; tmpBracketScope=0
                                valueStop=None
                                if vartype in ('LIST','LPAREN'):
                                    if lex[token+tmpi+1].type in ('LISTEND','RPAREN','RINDEX'): token+=1 ; continue
                                    for t in range(token+tmpi,len(lex)-1):

                                        if lex[t].type in typeNewline and listScope==0: valueStop=t ; break
                                        elif lex[t].type in typeNewline: pass
                                        elif lex[t].type == 'FOR': tmpf=None ; break
                                        elif lex[t].type == 'ID' and lex[t].value in definedFuncs: tmpf=None ; break
                                        else:
                                            if lex[t].type == vartype:
                                                listScope+=1
                                            elif (vartype=='LIST' and lex[t].type == 'LISTEND') \
                                            or (vartype=='LPAREN' and lex[t].type == 'RPAREN'): listScope-=1
                                            tmpf.append(deepcopy(lex[t]))
                                else:
                                    tmpNoEqualsAssign=True if tmpi == 1 else False
                                    for t in range(token+tmpi,len(lex)-1):
                                        #print(lex[t].type,lex[t].value)
                                        if tmpNoEqualsAssign:
                                            # fixes  x y 12 ; x ; y
                                            # it captures y 12 for x, it shouldnt
                                            if lex[t].type not in ('ID','ASSIGN'):
                                                tmpNoEqualsAssign=False
                                        if not tmpNoEqualsAssign:
                                            if lex[t].type in typeNewline and listScope==0 and tmpBracketScope==0: valueStop=t ; break
                                            elif lex[t].type == 'ID':
                                                if lex[t].value in definedFuncs or lex[t].value == lex[token].value: tmpf=None ; break
                                                else: tmpf.append(deepcopy(lex[t]))
                                            elif lex[t].value == lex[token].value and lex[t+1].type=='LINDEX': tmpf=None ; break
                                            elif lex[t].type in (vartype,'IGNORE','LPAREN','RPAREN','INS','DEFEXP')+typeOperators:
                                                if lex[t].type == 'LIST':
                                                    listScope+=1
                                                elif lex[t].type == 'LISTEND': listScope-=1
                                                elif (lex[t].type == 'FUNCTION' and lex[t].value.replace('(','') not in pyBuiltinFunctions) \
                                                or (lex[t].type == 'PIPE' and lex[t+1].value not in pyBuiltinFunctions):
                                                    tmpf=[] ; break
                                                if lex[t].type!='IGNORE': tmpf.append(deepcopy(lex[t]))
                                            elif lex[t].type in ('FUNCTION','PIPE'): tmpf=None ; break
                                            elif lex[t].type == 'ASSIGN' and ':' not in lex[t].value: tmpf=[]
                                            else: vartype=None ; tmpf.append(deepcopy(lex[t]))
                                            if lex[t].type == 'LBRACKET': tmpBracketScope+=1
                                            elif lex[t].type == 'RBRACKET': tmpBracketScope-=1

                                if tmpf == []: tmpf=None
                                if tmpf!=None and len(tmpf)>2 and tmpf[0].type in ('ID','BUILTINF') and tmpf[1].type == 'LINDEX' and (tmpf[-1].type == 'RINDEX' or tmpf[-2].type == 'RINDEX'):
                                    tmpf=None # thing[index] folding is slower than using var reference from var=thing[index]
                                if valueStop==None: valueStop=len(lex)-1

                                if tmpf != None: # expression is all same type
                                    if vartype == 'NUMBER' and optCompilerEval:
                                        try:
                                            tmpf=compilerNumberEval(tmpf)
                                        except (TypeError, SyntaxError, ZeroDivisionError):
                                            # fails sometimes with bitwise operators when negative inversion is involved
                                            tmptok=deepcopy(lex[token]) ; tmptok.type='LPAREN' ; tmptok.value='(' ; tmpf.insert(0,tmptok) ; del tmptok
                                            tmptok=deepcopy(lex[token]) ; tmptok.type='RPAREN' ; tmptok.value=')' ; tmpf.append(tmptok) ; del tmptok
                                            tmpf=tmpf[::-1]
                                    elif isinstance(tmpf[0],str)==False:
                                        # for lists/tuples
                                        tmpf=tmpf[::-1]
                                        if len(tmpf) > 1 and tmpf[-1].type!='LIST' and any(True for i in tmpf if i.type == 'LISTCOMP' )==False:
                                            tmptok=deepcopy(lex[token]) ; tmptok.type='LPAREN' ; tmptok.value='(' ; tmpf.append(tmptok) ; del tmptok
                                            tmptok=deepcopy(lex[token]) ; tmptok.type='RPAREN' ; tmptok.value=')' ; tmpf.insert(0,tmptok) ; del tmptok
                                    else: tmpf=[l for l in tmpf if tmpf.type != 'IGNORE']

                                    search=True ; linkType=True ; ignores=[] ; inDef=False ; wasInDefs=False
                                    # wasInDefs is for determining if a later define could break behaviour inside of functions
                                    tmpIDshow=0
                                    for tmpi in range(valueStop+1,len(lex)): # check if we can determine its a constant
                                        if lex[tmpi].type=='INC' or lex[tmpi].type=='INDEX' and lex[tmpi].value.startswith(lex[token].value) or (tmpi+1 < len(lex) and lex[tmpi+1].type=='LINDEX' and lex[tmpi].value == lex[token].value) \
                                        or (((lex[tmpi].type in ('ID','INC') and lex[tmpi].value.replace('++','').replace('--','')==lex[token].value and lex[tmpi-1].type not in ('ELIF','OF','IF','OR','AND')) or (lex[tmpi].value.startswith('locals[') and lex[token].value in ''.join(lex[tmpi].value.split('locals[')[1:]))) and lex[tmpi+1].type in typeAssignables+('ASSIGN',)):
                                            if (lex[tmpi+1].type == 'ASSIGN' and lex[tmpi+2].type == vartype) or lex[tmpi+1].type == vartype or (vartype=='NUMBER' and lex[tmpi].type=='INC'):
                                                pass
                                            else: linkType=False
                                            #if lex[tmpi].value.replace('++','').replace('--','') == lex[token].value: tmpIDshow+=1
                                            if lex[tmpi+1].type=='LINDEX' and vartype=='STRING': pass
                                            else:
                                                ignores.append(tmpi) # if we reach a point where we can determine its no longer constant, then ignore that point onward so the previous code still gets folded
                                            if wasInDefs and lex[tmpi+1].type in typeAssignables+('ASSIGN',): search = False
                                            break
                                        elif lex[tmpi].type == 'BUILTINF' and lex[tmpi].value.split('.')[0] == lex[token].value and '.'+lex[tmpi].value.split('.')[1] in listMods:
                                            search=False ; linkType=False ; break # discards list mods like .append()
                                        elif lex[tmpi].type == 'SCOPE' and lex[token].value in lex[tmpi].value:
                                            search=False ; break # no global var pls
                                        elif lex[tmpi].type in ('WHILE','FOR') or (lex[tmpi].type == 'LOOP' and tmpi+2 < len(lex)-1 and lex[tmpi+2].value == lex[token].value):
                                            pass
                                            #linkType=False # for/while loops? oh boy
                                        elif lex[tmpi].type == 'META' and lex[token].value in '='.join(lex[tmpi].value.split('=')[1:]):
                                            search=False
                                        elif lex[tmpi].type == 'DEFFUNCT':
                                            wasInDefs = True
                                            if lex[tmpi-1].value == lex[token].value:
                                                inDef=True ; search=False
                                                ignores.append([tmpi-1])
                                        elif lex[tmpi].type == 'PYDEF':
                                            wasInDefs = True
                                            if lex[token].value.strip()+'(' in lex[tmpi].value.strip():
                                                inDef = True ; search = False
                                                ignores.append([tmpi])
                                        elif lex[tmpi].type == 'FROM' and inDef:
                                            search=True ; inDef=False
                                            ignores[-1].append(tmpi)
                                        elif lex[tmpi].type == 'TRY': ignores.append(tmpi) ; break
                                        elif lex[tmpi-1].type == 'INS' and lex[tmpi-2].type == 'COMMAGRP' and lex[tmpi].type == 'ID' and lex[tmpi].value == lex[token].value and vartype == 'DICT':
                                            search=False # dicts dont seem to play well with stuff like enumerate
                                        elif lex[tmpi].type == 'ID' and lex[tmpi].value == lex[token].value:
                                            tmpIDshow+=1
                                    if len([True for tmpi in range(token,0,-1) if lex[tmpi].type in ('SCOPE','META',) and lex[token].value in lex[tmpi].value])>0: search=False
                                    # ^^ no global var pls
                                    if len([True for tmpi in range(token,0,-1) if lex[tmpi].type == 'TRY' and 'try' in lex[tmpi].value])>len([True for tmpi in range(token,0,-1) if lex[tmpi].type == 'TRY' and 'except' in lex[tmpi].value]): search=False
                                    # ^^ fixes it in cases where the constant is defined in the except
                                    if tmpIDshow > 1 and isinstance(tmpf[0],str) == False and len(tmpf) > 1 and vartype!="LIST": search=False
                                    # ^^ when a constant involves operations, its better to compute it once and share the value rather than compute the value in many places.

                                    if search or linkType:
                                        inFrom=False # dont replace constants inside of FROM (function args)
                                        tmpindent=0 # keeping track if constant is in indented block
                                        ignore=False # ignoring function blocks
                                        inLoop=[False,0]
                                        for tmpi in range(token-1,0,-1):
                                            if lex[tmpi].type == 'TAB':
                                                tmpindent=lex[tmpi].value.replace('\t',' ').count(' ')
                                                if lex[tmpi+1].type in typeConditonals:
                                                    tmpindent+=1
                                                break
                                            # don't check THENs for indent
                                            elif lex[tmpi].type == 'NEWLINE': break

                                        if len([True for tmpi in range(token,0,-1) if lex[tmpi].type == 'TYPEWRAP' and (lex[tmpi-1].type=='NEWLINE' or (lex[tmpi-1].type in typeNewline and lex[tmpi-1].value.count(' ')<tmpindent))])>0:
                                            linkType=False # if there is a typewrap defining the types, then we shouldnt mess with it
                                            if search == False: token+=1 ; continue # if linktype and search are false, why are we here? leave

                                        if debug:
                                            print('constant-folding:',lex[token].value)
                                            if isinstance(tmpf[0],str) == False:
                                                print('tokens!',lex[token].value,'=',''.join([f.value for f in tmpf[::-1]]))
                                            else: print('str!',lex[token].value,'=',''.join([f for f in tmpf]))

                                        for tmpi in range(valueStop,len(lex)):
                                            if inFrom:
                                                if lex[tmpi].type in typeNewline: inFrom=False
                                            else:
                                                if search and ignores!=[]:
                                                    if isinstance(ignores[0], int) and ignores[0] == tmpi:
                                                        search=False
                                                    elif isinstance(ignores[0], list) and tmpi == ignores[0][0]:
                                                        ignore=True
                                                    if ignore and tmpi == ignores[0][1]:
                                                        ignore=False ; del ignores[0]
                                                    #if debug: print(tmpi,lex[tmpi].type,f'ignore={ignore}',f'skip/end={ignores}')

                                                if search and ignore == False:
                                                    if lex[tmpi].type == 'ID' and lex[tmpi].value==lex[token].value and (lex[tmpi+1].type not in typeAssignables+('ASSIGN',) or (lex[tmpi-1].type in typeConditonals+('OR','AND') and lex[tmpi-1].type!='ELSE') ) and lex[tmpi-1].type not in ('FOR','LOOP'):
                                                        if lex[tmpi-1].type in typeConditonals and lex[tmpi+1].type == 'ASSIGN' and ':' in lex[tmpi+1].value: continue
                                                        tmpsafe=True
                                                        if lex[tmpi-1].type in ('RBRACKET','RPAREN','LISTEND') or lex[tmpi-2].type == 'COMMA':
                                                            for tmpii in range(tmpi,0,-1):
                                                                if lex[tmpii].type == 'LOOP': tmpsafe=False ; break
                                                                elif lex[tmpii].type in typeNewline: break
                                                        if vartype!='STRING' and lex[tmpi+1].type=='LINDEX':tmpsafe=False # so it doesn't replace the var in var[index]
                                                        if vartype in ('LIST','ID') and lex[tmpi-1].type not in typeCheckers+('INS','EQUAL','LPAREN','BITWISE','ANYOF')+typeMops:
                                                            tmpsafe=False # functions can modify lists in place, therefore replacing it with the list can break behaviour
                                                            for tmpii in range(tmpi,0,-1):
                                                                if lex[tmpii].type in typeNewline+('BUILTINF','FUNCTION','LPAREN'): break
                                                                elif lex[tmpii].type == 'LAMBDA':
                                                                    tmpsafe=True
                                                                    if optListToTuple and isinstance(tmpf[0],str) == False:
                                                                        tmpf[-1].value='(' ; tmpf[-1].type='LPAREN'
                                                                        tmpf[0].value = ')' ; tmpf[0].type = 'RPAREN'
                                                                    break
                                                        if lex[tmpi-2].type == 'LOOP' and isinstance(tmpf[0],str) == False and tmpf[0].type == 'RPAREN': tmpsafe=False
                                                        if vartype == 'STRING' and (lex[tmpi-1].type == 'FSTR' or lex[tmpi+1].type == 'FSTR'):
                                                            if isinstance(tmpf[0],str) == False and '\\\\' in tmpf[0].value: tmpsafe=False
                                                            elif '\\\\' in tmpf: tmpsafe=False
                                                        if tmpsafe and lex[tmpi-1].type not in typeNewline:
                                                            # sometimes folding will invalidate the print-on-default-expression feature
                                                            # so on instances where we know it doesn't break behaviour, we make a
                                                            # DEFEXP token to signify it is safe.
                                                            tmpStartOfline=0 ; tmpDefExp=True
                                                            for tt in range(tmpi,0,-1):
                                                                if lex[tt].type in typeNewline: tmpStartOfline=tt ; break
                                                            for tt in range(tmpStartOfline+1,len(lex)-1):
                                                                if lex[tt].type in typeNewline: break
                                                                elif lex[tt].type == 'DEFEXP': tmpDefExp=tmpsafe=False ; break
                                                                elif lex[tt].type not in typePrintable: tmpDefExp=False
                                                            if tmpDefExp and tmpsafe:
                                                                lex.insert(tmpStartOfline+1,makeToken(lex[0],'defExp','DEFEXP'))
                                                                tmpi+=1

                                                        if tmpsafe:
                                                            if debug: print(f'replacing lex #{tmpi} (lastType:{lex[tmpi-1].type})')
                                                            if isinstance(tmpf[0],str) == False: # if tmpf[0] is not string, then its a lex
                                                                if not pyCompatibility: # These fix it not printing in ASnake mode. If in Python mode, don't fix it.
                                                                    if len(tmpf) == 1 and tmpf[0].type == 'STRING' and (tmpf[0].value.startswith('"""') or tmpf[0].value.startswith("'''")) and lex[tmpi-1].type in typeNewline:
                                                                        # multiline strings dont print due to doc-strings, thus when constant folding onto a asnake bare ID,
                                                                        # it wouldnt print even though it should. this adds print token to fix that
                                                                        tmpf.append(makeToken(tmpf[0], 'print', 'ID'))
                                                                    elif lex[tmpi-1].type in typeNewline and lex[tmpi+1].type in typeNewline and lex[tmpi].type == 'ID' and lex[tmpi].value not in definedFuncs:
                                                                        # when it folds onto a bare ID, it should still print
                                                                        tmpf.append(makeToken(tmpf[0], 'print', 'ID'))
                                                                lex[tmpi].type='IGNORE'
                                                                for t in tmpf:
                                                                    lex.insert(tmpi,deepcopy(t))
                                                            else:
                                                                tmpf=''.join(tmpf)
                                                                lex[tmpi].value=tmpf ; lex[tmpi].type=vartype
                                                            newOptimization=True
                                                    elif lex[tmpi].type == 'COMMAGRP' and isinstance(tmpf[0],str) == True:
                                                        # replacing inside of commagrp
                                                        tmp=lex[tmpi].value.split(',')
                                                        for i in range(0,len(tmp)):
                                                            if tmp[i].strip() == lex[token].value:
                                                                if debug: print(f'replacing value {tmp[i]} inside COMMAGRP {lex[tmpi].value}')
                                                                tmp[i]=''.join(tmpf)
                                                                newOptimization = True
                                                        lex[tmpi].value=','.join(tmp)

                                                    elif lex[tmpi].type == 'FROM': inFrom=True
                                                    elif (lex[tmpi].type=='TAB' and lex[tmpi].value.replace('\t',' ').count(' ') < tmpindent) \
                                                    or (lex[tmpi].type == 'NEWLINE' and lex[tmpi].type!='TAB' and tmpindent>0) or (lex[tmpi].type=='THEN' and '\n' in lex[tmpi].value and lex[tmpi].value.replace('\t',' ').count(' ') < tmpindent):
                                                        break # if inside a indented block, try and stay local to that, do not escape it
                                                    elif lex[tmpi].type in ('LOOP','WHILE','FOR'):
                                                        # lookahead for loops in case it changes inside of it
                                                        inLoop=[True,tmpindent]
                                                        for tmpii in range(tmpi,len(lex)):
                                                            #print(lex[tmpii].type)
                                                            if lex[tmpi].type=='INC' or \
                                                            (((lex[tmpii].type in ('ID','INC') and lex[tmpii].value.replace('++','').replace('--','')==lex[token].value \
                                                            and (lex[tmpii-1].type not in typeConditonals+('OR','AND') \
                                                            and lex[tmpi-1].type!='ELSE') ) or (lex[tmpii].value.startswith('locals[') \
                                                            and lex[token].value in ''.join(lex[tmpii].value.split('locals[')[1:])))):
                                                                if lex[tmpii].type == 'INC' or (lex[tmpii].type=='ID' and lex[tmpii+1].type in typeAssignables+('ASSIGN',)):
                                                                    ignores.append(tmpi+1) ; search=False ; break
                                                            elif (lex[tmpii].type=='TAB' and lex[tmpii].value.replace('\t',' ').count(' ') < inLoop[1]) \
                                                            or (lex[tmpii].type == 'NEWLINE' and lex[tmpii].type!='TAB' and inLoop[1]>0) or (lex[tmpii].type=='THEN' and '\n' in lex[tmpii].value and lex[tmpii].value.replace('\t',' ').count(' ') < inLoop[1]):
                                                                inLoop=[False,0] ; break
                                                        if lex[tmpi].type == 'LOOP' and (lex[tmpi+1].type=='ID' and lex[tmpi+1].value==lex[token].value) or (tmpi+2 < len(lex)-1 and lex[tmpi+2].type=='ID' and lex[tmpi+2].value==lex[token].value) \
                                                        and isinstance(tmpf[0],str) == False and tmpf[0].type != 'RPAREN':
                                                            if (lex[tmpi+1].type=='ID' and lex[tmpi+1].value==lex[token].value): tmp=1
                                                            else: tmp=2
                                                            # with loop syntax, fold onto iterable
                                                            if debug: print(f'replacing lex #{tmpi} (lastType:{lex[tmpi-tmp].type})')
                                                            if isinstance(tmpf[0],str) == False:
                                                                lex[tmpi+tmp].type='IGNORE'
                                                                for t in tmpf:
                                                                    lex.insert(tmpi+tmp,t)
                                                            else:
                                                                tmpf=''.join(tmpf)
                                                                lex[tmpi+tmp].value=tmpf ; lex[tmpi+tmp].type=vartype
                                                    elif lex[tmpi].type == 'LISTCOMP' and lex[tmpi].value.split(':')[0] == lex[token].value:
                                                        if isinstance(tmpf[0], str) == True:
                                                            lex[tmpi].value=tmpf+':'+''.join(lex[tmpi].value.split(':')[1:])
                                                            if debug: print(f'replacing lex #{tmpi} (lastType:{lex[tmpi-1].type})')
                                                            newOptimization = True
                                                if tmpi >= len(lex)-1 and linkType and (enforceTyping or compileTo == 'Cython') and lex[token-1].type != 'TYPE'\
                                                and lex[token-2].type != 'LOOP' and lex[token-1].type != 'ID':
                                                    # auto assign type if its known it never changes
                                                    tmptok=deepcopy(lex[token])
                                                    tmptok.type='TYPE'
                                                    if vartype == 'NUMBER' and '.' in tmpf:
                                                        tmptok.value='float'
                                                        lex.insert(token, tmptok)
                                                    else:
                                                        tmp=[i for i in convertType if convertType[i] == vartype]
                                                        if tmp!=[]:
                                                            tmptok.value=tmp[0]
                                                            lex.insert(token,tmptok)
                                                    del tmptok ; token+=1 ; break

                        if optMathEqual and lex[token].lineno: # checking for: a = a + 1 -> a += 1
                            #if token+3 <= len(lex)-1: print('!mathequal:',lex[token],token+3 <= len(lex)-1, lex[token+1].type == 'ASSIGN', lex[token+2].value == lex[token].value , lex[token+3].type in typeOperators)
                            if token+3 <= len(lex)-1 \
                            and lex[token+1].type == 'ASSIGN' and ':' not in lex[token+1].value \
                            and lex[token+2].value == lex[token].value \
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
                                            break
                                    elif lex[t].type in typeNewline:
                                        break
                                    else:
                                        tmpcheck=False
                                        break
                                if tmpcheck and tmptype:
                                    lex[token].lineno=0
                                    # cheap way to skip a second iteration in the case of:  a = a + a * 2
                                    # which otherwise would (shouldn't) end up as:  a *= 2


                                    if tmpbit:
                                        lex[token+1].value=tmpbit ; del tmpbit
                                    else: lex[token+1].value=mopConv[tmptype]
                                    lex.pop(token+2) ; lex.pop(token+2)
                                    newOptimization=True
                        if optWalrus and compileTo != 'Cython' and pythonVersion >= 3.8 and lex[token+1].type in typeAssignables+('ASSIGN','COMMAGRP') \
                        and lex[token-1].type in typeNewline+('TYPE',) and lex[token+2].type!='ID':
                            safe=True
                            if lex[token+1].type == 'ASSIGN':
                                if any(True for i in ('+', '-', '/', '*', ':') if i in lex[token+1].value):
                                    safe=False
                                tmpi=2
                            else: tmpi=1

                            if safe:
                                tmpIndent = 0 ; tmp = 1
                                if lex[token - 1].type == 'TYPE': tmp = 2
                                if lex[token - tmp].type == 'TAB': tmpIndent = lex[token - tmp].value.replace('\t',' ').count(' ')

                                tmpf=[] ; tmpval=lex[token].value
                                for t in range(token+tmpi,len(lex)-1):
                                    if lex[t].type in typeNewline:
                                        tmpi=t+1
                                        if (lex[t].type == 'TAB' and lex[t].value.replace('\t',' ').count(' ') < tmpIndent) \
                                        or (lex[t].type == 'NEWLINE' and tmpIndent > 0):
                                            tmpf=[] # if the assignment is on a higher indent, cancel this optimization
                                        break
                                    elif lex[t].type == 'ASSIGN' and any(True for i in ('+', '-', '/', '*', ':') if i in lex[t].value): tmpf=[] ; break
                                    elif lex[t].type == 'LISTCOMP': tmpf=[] ; break
                                    elif lex[t].type == 'INC': tmpf=[] ; break
                                    else: tmpf.append([lex[t].value,lex[t].type])
                                if tmpf != [] and lex[tmpi].type == 'IF':
                                    search=False
                                    for t in range(tmpi,len(lex)-1):
                                        if lex[t].type=='IF' and lex[t+1].type=='ID' \
                                        and lex[t+1].value == lex[token].value and ((lex[t+2].type != 'ASSIGN' and ':' not in lex[t+2].value) or lex[t+2].value.strip() == 'is'):
                                            search=True
                                            lex[t+1].type='IGNORE'
                                            tmp=[[')','RPAREN']]+tmpf[::-1]+[[':=','ASSIGN'],[tmpval,'ID'],['(','LPAREN']]
                                            if lex[t+2].type in typeAssignables+('BOOL',):
                                                tmp.insert(0,['==','EQUAL'])
                                            for tt in tmp:
                                                tmptok=deepcopy(lex[t])
                                                tmptok.value=tt[0] ; tmptok.type=tt[1]
                                                lex.insert(t+1,tmptok)
                                        if lex[t].type in typeNewline: break
                                    if search:
                                        if debug: print(f'walrus-ing: {lex[token].value}')
                                        for t in range(token,tmpi):
                                            #print(lex[t].type,lex[t].value)
                                            lex[t].type='IGNORE'
                                        if token-1>0 and lex[token-1].type in ('CONSTANT','TYPE'):
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
                                    tmptok=deepcopy(lex[token]) ; tmptok.type='FUNCTION' ; tmptok.value='pow('
                                    lex.insert(token,tmptok) ; del tmptok
                                    tmptok=deepcopy(lex[token]) ; tmptok.type='RPAREN' ; tmptok.value=')'
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
                                    tmptok=deepcopy(lex[token]) ; tmptok.type='FUNCTION' ; tmptok.value='pow('
                                    lex.insert(token-2,tmptok) ; del tmptok
                                    tmptok=deepcopy(lex[token]) ; tmptok.type='RPAREN' ; tmptok.value=')'
                                    lex.insert(token+6-2,tmptok) ; del tmptok
                                    newOptimization=True
                        if lex[token-1].type == 'PIPE':
                                if optFuncTricks:
                                    if optFuncTricksDict['EvalLen'] and optCompilerEval and 'len' not in reservedIsNowVar:
                                        if lex[token].value == 'ASlen' or lex[token].value == 'len' and lex[token-2].type=='STRING' :
                                            if lex[token - 2].value.startswith('"""') or lex[token - 2].value.startswith("'''"):
                                                tmp=6
                                            else: tmp=2
                                            lex[token].value = str(len(lex[token - 2].value) - tmp)
                                            lex[token].type='NUMBER'
                                            del lex[token - 1] ; del lex[token - 2]
                                            newOptimization=True


                    elif lex[token].type == 'META':
                        metaCall=lex[token].value.replace('$','').replace(' ','').lower()
                        if metaCall == 'cython':
                            compileTo='Cython'
                        elif metaCall == 'python':
                            compileTo='Python'
                        elif metaCall.split('=')[0] in {'optimize','optimization','optimizing'}:
                            if '=' in metaCall:
                                if metaCall.split('=')[-1].lower() == 'true': optimize=True
                                elif metaCall.split('=')[-1].lower() == 'false': optimize=True
                            else: optimize = not optimize
                        elif metaCall.split('=')[0] in metaPyCompat:
                            if '=' in metaCall:
                                if 'true' in metaCall.split('=')[1]: pyCompatibility=True
                                else: pyCompatibility=False
                            else:
                                pyCompatibility=not pyCompatibility

                    elif lex[token].type in ('LPAREN','LIST') and lex[token-1].type == 'INS' \
                    and optInSet:
                        tmpscope=1 ; tmp=0 ; tmpf=[]
                        for tmpi in range(token+1,len(lex)):
                            if lex[tmpi].type == lex[token].type: tmpscope+=1
                            elif lex[token].type == 'LPAREN' and lex[tmpi].type == 'RPAREN': tmpscope-=1
                            elif lex[token].type == 'LIST' and lex[tmpi].type == 'LISTEND': tmpscope -= 1
                            if tmpscope == 0: tmp=tmpi ; break
                            else: tmpf.append(deepcopy(lex[tmpi]))
                        # if tmp is 0 then syntax error, but i don't think optimization stage is where to show it
                        if tmp>0 \
                        and lex[tmp+1].type != 'PLUS':
                            if len(tmpf) == 1 and tmpf[0].type == 'COMMAGRP':
                                tmpf=tmpf[0].value.split(',')
                            else: tmpf=[l.value for l in tmpf if l.type != 'COMMA']
                            seen=set()
                            if not any(i in seen or seen.add(i) for i in tmpf) == True:
                                # if all are unique
                                lex[token].type='LBRACKET' ; lex[token].value='{'
                                lex[tmp].type = 'RBRACKET' ; lex[tmp].value = '}'

                    elif lex[token].type in ('LIST','INDEX') and lex[token-1].type == 'INS':
                        if optListToTuple:
                            if token-2 >= 0 and lex[token].type=='LIST' and lex[token].value!='(' and lex[token-1].value != 'print':
                                if (lex[token-1].type == 'ID' \
                                    or lex[token-2].type == 'ID') and \
                                    (lex[token-3].type != 'TYPE' and lex[token-2].type != 'TYPE'):
                                    #and (token+1>len(lex) and lex[token+1].type not in ('COMMA','COMMAGRP')):
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
                                            #if debug: [print(i.type,i.value) for i in lex[token:]]
                                            #return AS_SyntaxError(f'[ is missing a closing ]','[ 1 ,2 , 3 ]',None,data)
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
                        if optFromFunc:
                            importName=lex[token].value.replace('import','').replace(' ','')+'.'
                            if 'from' not in lex[token].value and importName not in wasImported:
                                wasImported[importName]=[]
                    elif lex[token].type in ('BUILTINF','FUNCTION','COMMAGRP') or (lex[token].type == 'TYPE' and lex[token+1].type=='LPAREN'): # COMMAGRP might be dubious, but it does solve some cases for optFromFunc
                        if optFromFunc:
                            tmpf = [i for i in wasImported if i in lex[token].value]
                            if len(tmpf) > 0:
                                # tmpf is moduleName. like random.
                                # func is all of the things after it, like .randint
                                restr='((?: |,)'+tmpf[0].replace('.','\.')+r"""\w*\b(?=([^"'\\]*(\\.|("|')([^"'\\]*\\.)*[^"'\\]*("|')))*))"""
                                # thing\.\w*\b is all thats needed, rest is for excluding it if its in quotes
                                func=re.findall(restr,' '+lex[token].value,re.MULTILINE)
                                if len(func) > 0: func = [f[0] for f in func]

                                if len(func) == 0 or any(i for i in func if i.count('.')>1): pass # must not be multi import like:  ctypes.c_int.from_address
                                elif lex[token] not in doNotModThisToken:
                                    if func[0].split('.')[-1] + '.' in tmpf: # dont not importname.importname.thing()
                                        doNotModThisToken.append(lex[token])
                                    if lex[token].type=='COMMAGRP':
                                        lex[token].value = lex[token].value[:lex[token].value.index(',')+1] + lex[token].value[lex[token].value.index(',')+len(tmpf[0])+1:]
                                    else: lex[token].value=lex[token].value[len(tmpf[0]):] # replaces module.thing to thing
                                    #if lex[token+1].type == 'LPAREN':
                                    #    lex[token].value+='('
                                    #    del lex[token+1]

                                    lex[token].type='FUNCTION'
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
                                                wasImported[tmpf[0]].append(f)
                                    newOptimization=True

                        if optFuncTricks:# and lex[token].type!='COMMAGRP':
                            if optFuncTricksDict['randint']:
                                # a trick to speed up random.randint
                                check=randintOptimize=False
                                randTooBig=18014398509481983 # above this it becomes non-random and is only even
                                if compileTo == 'Cython': randTooBig=2147483647
                                if 'randint' in lex[token].value \
                                and token+5 < len(lex) and lex[token+2].value in ('0','-0') and lex[token+4].value.startswith('0')==False and lex[token+5].type == 'RPAREN':
                                    if lex[token+4].value.isdigit()== False or int(lex[token+4].value) < randTooBig:
                                        randintOptimize=True
                                        lex[token].value='int(random()*%s'%(int(lex[token+4].value)+1 if lex[token+4].value.isdigit() else f"({lex[token+4].value}+1)")
                                        lex[token+1].type=lex[token+2].type=lex[token+3].type=lex[token+4].type='IGNORE'
                                        check=True
                                elif ('randint' == lex[token].value and lex[token+2].type == 'COMMAGRP' and lex[token+2].value.count(',')==1 and lex[token+2].value.split(',')[0] in ('0','0.0','-0','-0.0') \
                                and lex[token+2].value.split(',')[1].isdigit() and int(lex[token+2].value.split(',')[1]) < randTooBig) \
                                or ('randint(' == lex[token].value and lex[token+1].type == 'COMMAGRP' and lex[token+1].value.count(',')==1 and lex[token+1].value.split(',')[0] in ('0','0.0','-0','-0.0') \
                                and lex[token+1].value.split(',')[1].isdigit() and int(lex[token+1].value.split(',')[1]) < randTooBig):
                                    # randint(0,12)  -->  int(random()*13)
                                    if 'randint(' == lex[token].value:
                                        tmp=1 ; lex[token+1].type = 'IGNORE'
                                    else:
                                        tmp=2 ; lex[token+1].type = lex[token+2].type = 'IGNORE'
                                    lex[token].value=f'int(random()*({lex[token+tmp].value.split(",")[1]}+1)'
                                    randintOptimize = check = True
                                elif 'randint' in lex[token].value \
                                and token+2 < len(lex) and lex[token+2].type == 'RPAREN' and lex[token+1].type=='COMMAGRP' and lex[token+1].value.split(',')[0] in ('0','-0') and lex[token+1].value.split(',')[-1].startswith('0')==False:
                                    if lex[token+1].value.isdigit()== False or int(lex[token+1].value.split(',')[-1]) < randTooBig:
                                        randintOptimize=True
                                        if compileTo == 'Cython':
                                            if all(False if i == "from libc.stdlib cimport rand, RAND_MAX, srand\nfrom libc.time cimport time as Ctime\nsrand(Ctime(NULL))\nrand()" else True for i in code ):
                                                code.insert(1,"from libc.stdlib cimport rand, RAND_MAX, srand\nfrom libc.time cimport time as Ctime\nsrand(Ctime(NULL))\nrand()")
                                            if lex[token+1].value.split(',')[-1].isdigit():
                                                lex[token].value=f"(rand() % {int(lex[token+1].value.split(',')[-1])+1})"
                                            else:
                                                lex[token].value=f"(rand() % ({lex[token+1].value.split(',')[-1]} + 1))"
                                            lex[token+2].type='IGNORE'
                                        else:
                                            tmp=lex[token+1].value.split(',')
                                            if tmp[-1].isdigit() and tmp[-1] == '1':
                                                lex[token].value='getrandbits(1'
                                                randintOptimize = False ; newOptimization=True
                                                if 'random.' in wasImported:
                                                    if 'getrandbits' in wasImported['random.']: pass
                                                    else: wasImported['random.'].append('getrandbits')
                                                else: wasImported = {'random.': ['getrandbits']}
                                            else:
                                                lex[token].value='int(random()*%s'%(int(tmp[-1])+1 if tmp[-1].isdigit() else '('+tmp[-1]+'+1)')
                                        lex[token+1].type='IGNORE'
                                        check=True
                                elif (('randint' == lex[token].value or 'random.randint' == lex[token].value) and (token+3 < len(lex) and lex[token+3].type == 'RPAREN' and lex[token+2].type=='COMMAGRP' and lex[token+2].value.split(',')[0] not in ('0','-0') and lex[token+2].value.split(',')[-1].startswith('0')==False)) \
                                or ('randint(' == lex[token].value and lex[token+1].type in ('NUMBER','ID') and lex[token+2].type=='COMMA' and lex[token+3].type in ('NUMBER','ID') and lex[token+4].type=='RPAREN' ):
                                    #x+int((random()*((y+1)-x)))
                                    #x+int((random()*(y-x)))
                                    # when randint minimum is above 1
                                    randintOptimize=True
                                    if lex[token + 2].type == 'COMMA':
                                        tmpmax=lex[token+3].value
                                        tmpmin = lex[token + 1].value
                                    else:
                                        tmpmax = lex[token + 2].value.split(',')[-1]
                                        tmpmin = lex[token + 2].value.split(',')[0]
                                    tmpcheck = True
                                    if (tmpmax.isdigit() and (int(tmpmax) < 10 or int(tmpmax) > randTooBig)) or (tmpmin.isdigit() and tmpmin in ('0','-0','0.0','-0.0')):
                                        check = tmpcheck = False
                                    if tmpcheck:
                                        if compileTo == 'Cython':
                                            if all(False if i == "from libc.stdlib cimport rand, RAND_MAX, srand\nfrom libc.time cimport time as Ctime\nsrand(Ctime(NULL))\nrand()" else True for i in code ):
                                                code.insert(1,"from libc.stdlib cimport rand, RAND_MAX, srand\nfrom libc.time cimport time as Ctime\nsrand(Ctime(NULL))\nrand()")


                                            lex[token].value=f"((rand() % ({tmpmax} - {tmpmin} + 1)) + {tmpmin}"
                                            #if lex[token].value[0] != '(': lex[token].value=f"<int>({lex[token].value}"
                                            lex[token].type=lex[token+1].type=lex[token+2].type='IGNORE'
                                            if lex[token + 3].type != 'RPAREN': lex[token + 3].type = 'IGNORE'
                                            check=True
                                        else:
                                            lex[token].type = lex[token+1].type = lex[token+2].type = 'IGNORE'
                                            if lex[token+3].type != 'RPAREN': lex[token+3].type = 'IGNORE'
                                            lex[token].value='%s+int((random()*(%s - %s))'%(tmpmin,int(tmpmax)+1 if tmpmax.isdigit() else '('+tmpmax+' + 1)',tmpmin)
                                            lex[token].value=f"({lex[token].value})"
                                            check=True

                                        if lex[token + 2].type == 'COMMA':
                                            lex[token+3].type = 'IGNORE'
                                    elif tmpcheck == False and tmpmin in ('0','-0','0.0','-0.0'):
                                        lex[token].value = f'int(random()*({tmpmax}+1)'
                                        if lex[token + 2].type == 'COMMA': lex[token + 3].type = 'IGNORE'
                                        lex[token + 1].type = lex[token + 2].type = 'IGNORE'
                                        check = True
                                elif lex[token+1].type=='COMMAGRP' and 'randint' in lex[token+1].value:
                                    # splitting a randint(0,randint(0,23)) thing
                                    tmptmp=2
                                    tmp=lex[token+1].value.split(',')
                                    #print(tmp,lex[token+1].type)
                                    miniLex=Lexer()
                                    for tt in miniLex.tokenize(tmp[1]+' '):
                                        if 'randint' in tt.value: tt.type='IGNORE'
                                        lex.insert(token+1,tt) ; tmptmp+=1
                                    tmptok=deepcopy(lex[token])
                                    tmptok.value=',' ; tmptok.type='COMMA'
                                    lex.insert(token+1,tmptok) ; del tmptok
                                    for tt in miniLex.tokenize(tmp[0]+' '):
                                        lex.insert(token+1,tt) ; tmptmp+=1
                                    del miniLex
                                    #tmptmp=4
                                    #print(tmptmp,lex[token+tmptmp].type,lex[token+tmptmp].value)
                                    lex[token + tmptmp].type='BUILTINF'
                                    lex[token + tmptmp].value = 'random.randint'
                                    token+=3

                                if check:
                                    # turn token into many tokens for further parsing/optimizing
                                    lex[token].type = 'IGNORE'
                                    miniLex = Lexer()
                                    tmpf = []
                                    for tt in miniLex.tokenize(lex[token].value + ' '):
                                        tmpf.append(tt)
                                    for tt in reversed(tmpf): lex.insert(token + 1, tt)
                                    del miniLex
                                else:
                                    randintOptimize = False

                                if randintOptimize:
                                    newOptimization=True
                                    if 'random.' in wasImported:
                                        if 'random' in wasImported['random.']:
                                            pass
                                        else:
                                            wasImported['random.'].append('random')
                                    else:
                                        wasImported['random.']=['random']

                                if compileTo == 'Cython' and 'random(' == lex[token].value and lex[token+1].type == 'RPAREN':
                                    lex[token+1].type = 'IGNORE'
                                    lex[token].value = "rand() / (RAND_MAX + 1.0)"
                                    if all(False if i == "from libc.stdlib cimport rand, RAND_MAX, srand\nfrom libc.time cimport time as Ctime\nsrand(Ctime(NULL))\nrand()" else True for i in code ):
                                        code.insert(1,"from libc.stdlib cimport rand, RAND_MAX, srand\nfrom libc.time cimport time as Ctime\nsrand(Ctime(NULL))\nrand()")
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
                                        tmptok=deepcopy(lex[token])
                                        tmptok.type = 'THEN' ; tmptok.value = 'then'
                                        lex.insert(token+1,tmptok) ; del tmptok
                                        if lex[token-1].type == 'ID': tmp = 1
                                        else: tmp = 2
                                        tmptok=deepcopy(lex[token-tmp])
                                        lex.insert(token+2,tmptok) ; del tmptok
                                        tmptok=deepcopy(lex[token])
                                        tmptok.type = 'ASSIGN' ; tmptok.value = '+='
                                        lex.insert(token+3,tmptok) ; del tmptok
                                        for tmpi in range(token+2,len(lex)):
                                            if lex[tmpi].type == 'RPAREN': lex[tmpi].type='IGNORE' ; break
                                        newOptimization=True

                            # ^^ list -- vv tuple/set of string
                            if optFuncTricksDict['TupleSetUnpack']:
                                if (lex[token].value.startswith('tuple') or lex[token].value.startswith('set')) \
                                and lex[token].type == 'FUNCTION' and lex[token+1].type == 'STRING' and lex[token+2].type == 'RPAREN':
                                    if lex[token].value.startswith('tuple'):
                                        lex[token].value='(*' ; lex[token+2].value=',)'
                                    else: # set
                                        lex[token].value="{*"+lex[token+1].value+"}"
                                        lex[token+1].type=lex[token+2].type='IGNORE'
                                    newOptimization=True

                            if optFuncTricksDict['EvalLen'] and optCompilerEval and 'len' not in reservedIsNowVar:
                                if lex[token].value == 'ASlen' and lex[token+1].type=='LPAREN' and lex[token+2].type=='STRING' and lex[token+3].type=='RPAREN' \
                                or lex[token].value == 'len(' and lex[token+1].type=='STRING' and lex[token+2].type=='RPAREN':
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

                    elif lex[token].type in ('LOOP','WHILE','FOR'):
                        if optLoopAttr and lex[token-1].type in typeNewline+('DEFFUNCT',):

                            # check to make sure we're not in tabbed list comp sorta deal
                            tmpscope=0
                            for tmpi in range(token-2,0,-1):
                                if lex[tmpi].type=='RPAREN':
                                    tmpscope-=1
                                elif lex[tmpi].type in ('LPAREN','FUNCTION'):
                                    tmpscope+=1
                                elif lex[tmpi].type in typeNewline: break
                            if tmpscope <= 0: # if paren scope negative (complete) or zero (nonexistent)

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
                                    # dont optimize the iterator variables in for loop
                                    if lex[token+1].type=='COMMAGRP':
                                        ignoreVars=lex[token+1].value.split(',')
                                    else:
                                        for ii in range(token+1,len(lex)):
                                            if lex[ii].type == 'INS': break
                                            elif lex[ii].type == 'COMMA': pass
                                            else: ignoreVars.append(lex[ii].value)

                                for tmpi in range(token+1,len(lex)-1):
                                    if lex[tmpi].type == 'TAB':
                                        if lex[tmpi].value.count(' ') <= tmpindent: break
                                        else: firstIndent=True
                                    elif lex[tmpi].type == 'NEWLINE': break
                                    elif lex[tmpi].type=='BUILTINF' and '.' in lex[tmpi].value and lex[tmpi].value[0] not in ('"',"'",'.') and lex[tmpi].value.startswith('self.')==False and lex[tmpi].value.startswith(lex[token+1].value+'.')==False and lex[tmpi].value.split('.')[0] not in ignoreVars and firstIndent:
                                        check=False ; minindent=None
                                        tmpImportName=lex[tmpi].value.split('.')[0]
                                        if tmpImportName+'.' in importcheck: break
                                        elif tmpImportName in importcheck: check=True
                                        if check == False:
                                            for ii in range(token-2,0,-1): # search backwards for evidence its safe to use the variable
                                                #if debug: print(lex[tmpi].value,check,lex[ii].type,lex[ii].value,'|',lex[tmpi].value)
                                                if lex[ii].type in ('ID','BUILTINF') and lex[ii].value.startswith(lex[tmpi].value.split('.')[0])\
                                                and lex[ii+1].type in typeAssignables+('ASSIGN',):
                                                    if (lex[ii-1].type == 'TAB' and lex[ii-1].value.count(' ') >= tmpindent) or (lex[ii-1].type in ('NEWLINE','THEN') or (lex[ii-1].type=='TYPE' and lex[ii-2].type in ('NEWLINE','THEN')) or (lex[ii-1].type=='CONSTANT' and lex[ii-2].type in ('NEWLINE','THEN')) or (lex[ii-1].type=='TYPE' and lex[ii-2].type=='CONSTANT' and lex[ii-3] in ('NEWLINE','THEN')) ):
                                                        check=True ; break
                                                    elif lex[ii-1].type == 'TYPE' and ((lex[ii-2].type=='TAB' and lex[ii-2].value.count(' ') >= tmpindent) or lex[ii-2].type in ('NEWLINE','THEN')): check=True;break
                                                elif lex[ii].type == 'PYDEF':
                                                    tmp=re.search(r"[\w_\d]+\(.*(?=\))",lex[ii].value)
                                                    if tmp!=None and lex[tmpi].value.split('.')[0] in tmp[0].lstrip('('):
                                                        check=True ; break
                                                elif lex[ii].type == 'TAB' and lex[ii].value.count(' ') >= tmpindent and minindent!=None and minindent < lex[ii].value.count(' '):
                                                    break # if it lowers scope then increases scope while backwards, we cant be too sure about this
                                                elif lex[ii].type == 'TAB' and (minindent==None or lex[ii].value.count(' ') < minindent):
                                                    minindent=lex[ii].value.count(' ')
                                            for ii in range(tmpi,0,-1): # search backwards within expression
                                                if lex[ii].type == 'ID' and lex[ii].value.startswith(lex[tmpi].value.split('.')[0]) and lex[ii + 1].type == 'INS' and lex[ii - 1].type == 'FOR':
                                                    check = False # dont want to optimize listcomps as it could call builtin of something that doesnt exist yet: sum(1 for c in word if c.isupper())
                                                    break
                                                elif lex[ii].type in typeNewline: break
                                        if check:
                                            assignCheck.append(lex[tmpi].value.split('.')[0])
                                            minicheck=True
                                            for ii in range(tmpi,token,-1):
                                                if lex[ii].type == 'ID' and lex[ii].value == assignCheck[-1] and lex[ii+1].type == 'LINDEX':
                                                    minicheck=False ; break
                                            if minicheck:
                                                tmpf.append([lex[tmpi].value.rsplit('(')[0],tmpi])
                                    elif lex[tmpi].type == 'ID':
                                        if lex[tmpi].value in assignCheck and lex[tmpi+1].type == 'ASSIGN':
                                            # if the thing in thing.attr is assigned in the loop, don't make this optimzation
                                            tmpf=[i for i in tmpf if lex[tmpi].value != i[0].split('.')[0]]
                                    elif lex[tmpi].type == 'FUNCTION' and lex[tmpi].value[:-1] in pyBuiltinFunctions and lex[tmpi].value!='range(':
                                        # built-in functions are faster when pre-assigned
                                        tmpf.append([lex[tmpi].value[:-1],tmpi])
                                        if lex[tmpi].value[-1]=='(': lex[tmpi].value=lex[tmpi].value[:-1] # bleh fix
                                        lex.insert(tmpi+1,makeToken(tok,'(','LPAREN'))
                                del importcheck ; del assignCheck
                                if len(tmpf) > 0:
                                    lexAdd=0
                                    tmp=[]
                                    if debug: print('attrs:',tmpf)
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

                                        if '.' in t[0]: tmpname=t[0].split('.')[0]+'_'+t[0].split('.')[1]
                                        else: tmpname=t[0]
                                        subBy=1
                                        if f'AS{tmpname}' not in tmp:
                                            if lex[token - 1].type == 'DEFFUNCT': subBy = 0
                                            #
                                            tmptok=deepcopy(lex[token])
                                            tmptok.value=t[0]
                                            tmptok.type='BUILTINF'
                                            lex.insert(token-subBy,tmptok)
                                            #
                                            tmptok=deepcopy(lex[token])
                                            tmptok.value='='
                                            tmptok.type='ASSIGN'
                                            lex.insert(token-subBy,tmptok)
                                            #
                                            tmptok=deepcopy(lex[token])
                                            tmptok.value=f'AS{tmpname}'
                                            tmptok.type='ID'
                                            lex.insert(token-subBy,tmptok)
                                            #
                                            if subBy > 0: # not in def
                                                tmptok=deepcopy(lex[token])
                                                tmptok.value=f'\n{" "*tmpindent}' if tmpindent > 0 else '\n'
                                                tmptok.type='TAB' if tmpindent > 0 else 'NEWLINE'
                                                tmptok.lineno=0 # to not mess up lineNumber for errors
                                                lex.insert(token-subBy,tmptok)
                                            else: # in def
                                                lex.insert(token+3,makeToken(tok,'then','THEN'))
                                            #
                                            lexIndex+=4
                                            lexAdd+=4
                                            tmp.append(f'AS{tmpname}')
                                        lex[t[1]+lexAdd].value=f'AS{tmpname}'
                                    newOptimization = True
                    elif lex[token].type == 'STRING':
                        if optStrFormatToFString and '%s' in lex[token].value and lex[token].value[0] != 'f' and all(True if i not in lex[token].value else False for i in {'%i','%d','%g','%G','%c','%r','%-','%x','%u','%o','%X','%E','%e','%f','%F','%+', '%0','%1','%2','%3','%4','%5','%6','%7','%8','%9'} ):
                            check=False
                            tmpf=[]
                            tmpRparen=0
                            preRparen=0
                            for t in range(token,0,-1):
                                # for setting tmpRparen
                                if lex[t].type in typeNewline+('ENDIF',): break
                                elif lex[t].type == 'LPAREN': preRparen+=1 ; tmpRparen+=1

                            #print('~~~')
                            for t in range(token,len(lex)-1):
                                #print(lex[t].type,tmpRparen)
                                if lex[t].type in typeNewline:
                                    break
                                elif lex[t].type == 'MODULO':
                                    check=True ; lex[t].type='IGNOREtmp'+lex[t].type
                                elif lex[t].type == 'LPAREN' and check: lex[t].type='IGNOREtmp'+lex[t].type ; tmpRparen+=1
                                elif lex[t].type == 'RPAREN' and check:
                                    tmpRparen-=1
                                    if preRparen <= 0:
                                        lex[t].type = 'IGNOREtmp' + lex[t].type
                                    preRparen -= 1
                                    if tmpRparen == 0: break
                                elif lex[t].type == 'COMMA' and tmpRparen <= 0: break
                                elif lex[t].type not in typeAssignables+('COMMA','COMMAGRP','FUNCTION','LINDEX','RINDEX','BUILTINF')+typeOperators+typeCheckers: break
                                else:
                                    if check:
                                        if lex[t].type=='COMMAGRP':
                                            miniLex=Lexer()
                                            doBreak=False
                                            for tt in miniLex.tokenize(lex[t].value.replace(',',' , ')+' '):
                                                if tt.type == 'COMMA' and tmpRparen <= 0: doBreak=True
                                                tmpf.append(tt)
                                            if doBreak: lex[t].type='IGNORE' ; break
                                        else:
                                            if lex[t+2].type == 'COLON': break
                                            #print('!',lex[t+2].type)
                                            tmptok=deepcopy(lex[t])
                                            tmpf.append(tmptok)
                                        lex[t].type='IGNORE'
                            if len(tmpf)>0:
                                lex[token].value='f'+lex[token].value
                                del tmptok
                                for t in range(token,len(lex)-1):
                                    if lex[t].type.startswith('IGNOREtmp'): lex[t].type='IGNORE'
                            else:
                                for t in range(token,len(lex)-1): # restoring types if we cant do the optimization
                                    if lex[t].type.startswith('IGNOREtmp'): lex[t].type=lex[t].type.split('tmp')[-1]
                            if debug and tmpf: print('optStrFormatToFString',[f.type for f in tmpf])
                            tmpfunc=[]
                            while len(tmpf) > 0:
                                if tmpf[0].type == 'STRING' and tmpf[0].value[0] not in ('f','r') and '{' not in tmpf[0].value[0] \
                                and len(tmpf)>1 and tmpf[1].type == 'COMMA':
                                    tmp=tmpf[0].value
                                    if tmp.startswith('"""') or tmp.startswith("'''"):
                                        tmp=tmp[3:-3]
                                    else:
                                        tmp=tmp[1:-1]
                                    lex[token].value=lex[token].value.replace('%s',tmp,1)
                                elif tmpf[0].type == 'COMMA' or len(tmpf)==1:
                                    if len(tmpfunc) > 0  or len(tmpf)==1:
                                        if len(tmpf)==1: tmpfunc.append(tmpf[0].value)
                                        lex[token].value=lex[token].value.replace('%s','{%s}'%''.join(tmpfunc),1)
                                        tmpfunc=[]
                                        newOptimization=True
                                else:
                                    tmpfunc.append(tmpf[0].value)
                                tmpf.pop(0)
                                #print('prune', tmpf)
                            if '{' not in lex[token].value and '}' not in lex[token].value:
                                lex[token].value=lex[token].value[1:]
                            if newOptimization:
                                lex[token] = deepcopy(createFString(lex[token],token))

                    elif lex[token].type == 'DEFFUNCT':
                        definedFuncs.append(lex[token-1].value)
                    elif lex[token].type == 'PYDEF':
                        definedFuncs.append(lex[token].value.split('(')[0])
                    if token > len(lex)-1: break
                    if lex[token].type == 'IGNORE':
                        del lex[token] ; token-=2
                    if optCompilerEval and lex[token].type in ('STRING','NUMBER'):
                        orderOfOps = {'RPAREN': 7, 'LPAREN': 6, 'EXPONENT': 5, 'MODULO': 4, 'TIMES': 4, 'DIVIDE': 4,
                                      'RDIVIDE': 4, 'PLUS': 1, 'MINUS': 1}
                        check=False
                        if token+3 < len(lex)-1 and lex[token+3].type == 'PIPE' and lex[token+3].value == 'to': pass
                        elif lex[token-1].type in typeOperators and lex[token+1].type in typeOperators:
                            # helps follow the order of operations for more accuracy
                            if orderOfOps[lex[token-1].type] <= orderOfOps[lex[token+1].type]:
                                check=True
                            #print('~',check,orderOfOps[lex[token-1].type],lex[token].type,orderOfOps[lex[token+1].type])
                        else:
                            # needed for some reason
                            check=True
                            #print(lex[token-1].type in typeOperators , lex[token-1].type, lex[token+1].type in typeOperators,lex[token+1].type)
                        if check:
                            if lex[token].type == 'NUMBER' and lex[token+1].type in typeOperators and (lex[token+2].type == 'NUMBER' or (lex[token+2].type == 'LPAREN' and lex[token+3].type == 'NUMBER')):
                                tmpscope=0 ; tmpf=[] ; fail=False
                                for tmpi in range(token,len(lex)):
                                    if lex[tmpi].type in typeOperators+('LPAREN','RPAREN','NUMBER') and tmpscope>=0:
                                        if lex[tmpi].type=='LPAREN': tmpscope+=1
                                        elif lex[tmpi].type=='RPAREN': tmpscope-=1
                                        if tmpscope>=0: tmpf.append(lex[tmpi])
                                    else:
                                        if tmpscope>0: fail=True
                                        while tmpf[-1].type in typeOperators+('LPAREN',): tmpf=tmpf[:-1]
                                        break
                                if len(tmpf) == 0 : break
                                if fail == False:
                                    try:
                                        lex[token].value=compilerNumberEval(tmpf)
                                        for die in range(token+1,token+len(tmpf)):
                                            lex[die].type='IGNORE'
                                        newOptimization=True
                                    except (TypeError, ZeroDivisionError): pass
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
                                if debug: print(f'merging into {lex[token].value}')
                                newOptimization=True
                                lex[token+1].type=lex[token+2].type='IGNORE'
                            if lex[token].type == 'STRING' and lex[token+1].type == 'TIMES' and lex[token+2].type == 'NUMBER' and lex[token+2].value != '0' and '.' not in lex[token+2].value \
                            and lex[token].value.startswith("'''")==False and lex[token].value.startswith('"""')==False:
                                quotes=None
                                for c in range(len(lex[token].value)):
                                    if lex[token].value[c] in ('"',"'"):
                                        quotes=lex[token].value[c] ; break
                                for c in range(len(lex[token].value)):
                                    if lex[token].value[c] == quotes:
                                        lex[token].value=quotes+(lex[token].value[c+1:-1]*int(lex[token+2].value))+quotes
                                        break
                                lex[token+1].type=lex[token+2].type='IGNORE'
                                newOptimization=True
                    if optCompilerEval and newOptimization and lex[token-1].type == 'LPAREN' and lex[token+1].type == 'RPAREN' and lex[token-2].type in ('LPAREN','ASSIGN')+typeNewline+typeOperators and lex[token].type not in ('FUNCTION','COMMAGRP'):
                        #print(lex[token-2].type,lex[token-1].type,lex[token].type,lex[token+1].type,888888888888)
                        lex[token+1].type=lex[token-1].type='IGNORE' ; token-=2

                    token+=1

        # last optimizations
        for token in range(0,len(lex)-1):
            if token > len(lex)-1: break
            if optFromFunc and lex[token].type == 'IMPORT':
                # this is its import name (ie random.): wasImported,lex[token].value.replace('import ','')+'.'
                importedName = lex[token].value.replace('import', '').strip() + '.'
                if debug: print('import: ',lex[token].value.split()[1], wasImported[importedName] if importedName in wasImported else '[]')
                if importedName in wasImported:
                    if len(wasImported[importedName]) > 0:
                        lex[token].value=lex[token].value.replace('import','').replace(' ','')
                        tmpf=[i for i in wasImported if i[:-1] in lex[token].value][0]
                        if len(wasImported[tmpf]) > 1 or wasImported[tmpf][0] != '':
                            lex[token].value='from %s import %s'%(lex[token].value,', '.join([wasImported[tmpf][i] for i in range(0,len(wasImported[tmpf]))]))
                        else: lex[token].value=f'from {lex[token].value} import *'
                elif 'from ' in lex[token].value:
                    if len(wasImported) > 0:
                        importName = lex[token].value.split('import')[0].replace('from ','').replace(' ','')+'.'
                        if importName in wasImported:
                            for fromImport in wasImported[importName]:
                                if fromImport not in lex[token].value.split('import')[-1]:
                                    lex[token].value=lex[token].value+', %s'%fromImport
            elif optDeadVariableElimination and lex[token].type == 'ID' and lex[token].value != 'print':
                if ((lex[token + 1].type in typeAssignables and lex[token+1].type!='LIST') or (lex[token + 1].type=='ASSIGN' and lex[token + 1].value in ('=','is','is '))) and lex[token - 1].type in typeNewline + ('CONSTANT', 'TYPE'):
                    delPoint = tmpIndent = None ; check = True ; tmpReplaceWithPass = False
                    tmpCurrentIndent = 0
                    # tmpIndent is var's indent, tmpCurrentIndent is iterations indent
                    for tmpi in range(token-1, 0, -1):
                        #print(lex[token].value, '!', lex[tmpi].value, lex[tmpi].type)
                        if lex[tmpi].type == 'TAB':
                            if tmpIndent==None: tmpIndent = lex[tmpi].value.replace('\t', ' ').count(' ')
                            if delPoint==None: delPoint=tmpi
                            tmpCurrentIndent = lex[tmpi].value.replace('\t', ' ').count(' ')
                        elif lex[tmpi].type == 'NEWLINE':
                            if tmpIndent==None: tmpIndent = 0
                            if delPoint==None: delPoint=tmpi
                            tmpCurrentIndent = 0
                            #break
                        elif lex[tmpi].type == 'THEN':
                            if delPoint==None: delPoint=tmpi
                        elif lex[tmpi].type == 'SCOPE' and lex[token].value in lex[tmpi].value :
                            check=False ; break
                        elif lex[tmpi].type == 'FROM': break
                        elif lex[tmpi].type in ('ID', 'INC', 'BUILTINF','FUNCTION') and (lex[tmpi].value.replace('(','').replace('+','').replace('-', '') == lex[token].value or lex[token].value+'.' in lex[tmpi].value):
                            #if tmpIndent != None and tmpCurrentIndent >= tmpIndent and lex[tmpi+1].type not in ('ASSIGN',):
                            check = False
                        elif lex[tmpi].type == 'PYDEF' and lex[token].value in lex[tmpi].value.split('(')[-1]:
                            check = False ; break
                        elif lex[tmpi].type in typeConditonals and delPoint:
                            # prevents dead variables defined in conditionals from breaking syntax
                            tmpReplaceWithPass = True
                        elif lex[tmpi].type == 'PYCLASS' and delPoint:
                            check=False
                    if delPoint == None or tmpIndent == None: check=False
                    if check:
                        breakOnNextNL=False ; ttenary=False
                        for tmpi in range(token + 1, len(lex) - 1):
                            #print(lex[token].value,'!',lex[tmpi].value,lex[tmpi].type,ttenary)
                            if lex[tmpi].type in ('ID', 'INC', 'BUILTINF','FUNCTION') and (lex[tmpi].value.replace('(','').replace('+','').replace('-', '') == lex[token].value or lex[token].value+'.' in lex[tmpi].value):
                                check=False ; break
                            elif lex[tmpi].type == 'INDEX' and lex[tmpi].value.startswith(lex[token].value):
                                check=False ; break
                            elif lex[tmpi].type == 'LISTCOMP' and lex[tmpi].value.split(':')[0] == lex[token].value: check=False ; break
                            elif lex[tmpi].type == 'COMMAGRP' and any(True for x in lex[tmpi].value.split(',') if x.strip() == lex[token].value or lex[token].value+'.' in x.strip()): check=False ; break
                            elif lex[tmpi].type == 'FROM':
                                for tmpii in range(tmpi, 0, -1):
                                    if lex[tmpii].type == 'TAB':
                                        tmp = lex[tmpii].value.replace('\t', ' ').count(' ');break
                                    elif lex[tmpii].type == 'NEWLINE':
                                        tmp = 0;break
                                if tmp < tmpIndent:
                                    breakOnNextNL=True
                            elif lex[tmpi].type == 'ASSIGN' and ':' not in lex[tmpi].value and lex[tmpi+1].type == 'IF':
                                ttenary=True
                            elif lex[tmpi].type == 'ELSE' and ttenary: ttenary=False
                            elif breakOnNextNL and not ttenary and lex[tmpi].type in typeNewline: break

                    if check:  # remove the var
                        ttenary = False ; tmpPass=False ; tmpEnd=0
                        for tmpi in range(delPoint+1, len(lex)*2):
                            if tmpi >= len(lex)-1:
                                tmpEnd = tmpi-1 ; break
                            if lex[tmpi].type == 'ASSIGN' and lex[tmpi+1].type == 'IF': ttenary=True
                            elif ttenary and lex[tmpi].type == 'ELSE': ttenary=False

                            if not ttenary and lex[tmpi].type in typeNewline:
                                tmpEnd=tmpi ; break
                            else:
                                if tmpReplaceWithPass:
                                    lex[tmpi].type = 'NOTHING'
                                    tmpReplaceWithPass=False
                                elif lex[tmpi].type == 'INC' and lex[tmpi].value.replace('+','').replace('-','').strip() != lex[token].value:
                                    lex.insert(tmpi+1,makeToken(lex[tmpi],'then','tmpPass')) ; tmpPass=True
                                elif lex[tmpi].type == 'tmpPass': pass
                                else:
                                    lex[tmpi].type = 'IGNORE'
                        if tmpPass:
                            # for removing the INCs from the expression while still keeping their effects by splitting them into new lines
                            # tmpPass is so the previous section doesnt delete the THEN inserts
                            for tmpi in range(delPoint + 1, tmpEnd*3):
                                if tmpi >= len(lex) - 1: break
                                if lex[tmpi].type == 'tmpPass':
                                    lex[tmpi].type='THEN'
                        if debug: print('eliminated variable:', lex[token].value)
            #print(' '.join([t.value for t in lex]))
        # clean up vv
        l = 0
        while l < len(lex):
            if optListToTuple and lex[l].type=='optLIST':
                if lex[l].value=='(': lex[l].type='LPAREN'
                else: lex[l].type='LIST'
            elif optMathEqual and lex[l].type=='ID' and lex[l+1].type=='ASSIGN' \
            and lex[l+1].value not in {'is','=',':='} and lex[l+1].lineno == 0:
                lex[l + 1].lineno=1
            elif lex[l].type == 'IGNORE': del lex[l] ; l-=1
            l+=1
        del wasImported


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
            storedVarsHistory=deepcopy(tmpCopy)
            insideDef = ''

    def storeVar(var1,var2,var3,staticType=None,position=None):
        nonlocal storedVarsHistory, lex, lexIndex, insideDef
        var1=deepcopy(var1) ; var2=deepcopy(var2) ; var3=deepcopy(var3)
        # var1 = ID , var2 = newValue , var3 = check if theres more going on
        if ':' in var1.value:
            var1.value=var1.value.split(':')[0].replace(' ','')
        if var1.type == 'ID' and '*' in var1.value: var1.value=var1.value.replace('*','')
        #if var2 != None and var2.type in ('FUNCTION','BUILTINF','INDEX'):
        #    return

        if ',' in var1.value:
            # if ID has multiple variables, recurse over them
            tmp=deepcopy(var1) ; tmp.value=tmp.value.split(',')[1]
            storeVar(tmp,var2,var3)
            var1.value=var1.value.split(',')[0]

        if var3.type in typeAssignables+typeNewline+('LISTCOMP','RPAREN','COMMAGRP','COMMA','LINDEX','LPAREN') and var2 != None:
            if var2.type == 'ID' and var2.value in storedVarsHistory and 'value' in storedVarsHistory[var2.value]:
                if enforceTyping and var1.value in storedVarsHistory and 'staticType' in storedVarsHistory[var1.value]\
                and storedVarsHistory[var2.value]['type'] != storedVarsHistory[var1.value]['staticType']:
                    return AS_SyntaxError(f'{var1.value} is of type {storedVarsHistory[var1.value]["type"]}, can\'t inherit {var2.value}\'s type {storedVarsHistory[var2.value]["type"]}.',f'{var1.value} should only be of type {storedVarsHistory[var1.value]["type"]}',lineNumber,data,'Enforced Static Type Error:')
                else:
                    storedVarsHistory[var1.value]={'value':storedVarsHistory[var2.value]['value'],'type':storedVarsHistory[var2.value]['type']}
            else:
                if var2.value[0]=='(' and var2.value[-1]==')': var2.value=var2.value[1:-1]
                storedVarsHistory[var1.value]={'value':var2.value,'type':var2.type}
            if notInDef: insideDef = ''
            if insideDef != '': storedVarsHistory[var1.value]['insideFunction'] = insideDef
        if staticType != None:
            if var1.value not in storedVarsHistory:
                storedVarsHistory[var1.value]={}
            storedVarsHistory[var1.value]['type']=convertType[staticType.value]
            storedVarsHistory[var1.value]['staticType']=staticType.value

        if var2 != None and var2.type == 'ID' and var2.value in storedVarsHistory \
        and storedVarsHistory[var2.value]['type'] in ('LIST','LISTCOMP') and var3.type in typeNewline:
            # copy list on assignment, screw references
            if lex[lexIndex].value==var2.value: lex[lexIndex].value+='[:]'
            elif lex[lexIndex+1].value==var2.value: lex[lexIndex+1].value+='[:]'

        if position != None and var1.value in storedVarsHistory:
            tmp=[]
            for tmpi in range(position,len(lex)):
                if lex[tmpi].type in typeNewline: break
                else: tmp.append(lex[tmpi])
            storedVarsHistory[var1.value]['line']=tmp



    def findEndOfFunction(lexIndex,goBackwards=False,needsToBeFunction=False):
        if goBackwards:
            scope=0
            for i in range(lexIndex,0,-1):
                #if debug: print(i,lex[i].type)
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
        nonlocal storedVarsHistory, line, code
        check=False # for checking if printf needs to be imported
        if tok.value in storedVarsHistory and 'value' in storedVarsHistory[tok.value]:
            tmpval=storedVarsHistory[tok.value]['value']
            tmptype=storedVarsHistory[tok.value]['type']
        else: tmpval=tok.value ; tmptype=tok.type
        if tmptype in ('STRING','STRLIT','STRRAW'):
            if any(i for i in ('r','f') if i == tmpval[0]):
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
            else: line.append(f'{" "*(indent)}printf("%d\\n",{tmpval})\n')
            check=True
        else: line.append(f'{" "*(indent)}print({tmpval})')

        if any(i for i in code if 'from libc.stdio cimport printf\n' in i): check=False
        if check: code.insert(1,'from libc.stdio cimport printf\n')

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
            if not forLoop and loopSyntaxCount < 3:
                if lex[tmpi].type in typeNewline+('FUNCTION',):
                    if lex[tmpi].type == 'FUNCTION': expressionStart = tmpi-1
                    else: expressionStart = tmpi
                    loopSyntaxCount=3
                elif lex[tmpi].type not in typeMops:
                    loopSyntaxCount+=1
                if loopSyntaxCount > 2:
                    if lex[lexIndex+1].value in storedVarsHistory: return False # for when it isn't a iteration variable
                    recordIter = tmpFirstIndent = False
                    iterVar = lex[tmpi].value
                    tmpIter.append(deepcopy(lex[tmpi-1]))
                    if expressionStart == None: expressionStart = tmpi
                    if lex[tmpi].type == 'TAB': tmpindent = lex[tmpi].value.replace('\t', ' ').count(' ')
                continue
            if lex[tmpi].type in typeNewline:
                if tmpFirstIndent:
                    recordIter = tmpFirstIndent = False
                    expressionStart = tmpi
                    if lex[tmpi].type == 'TAB': tmpindent = lex[tmpi].value.replace('\t',' ').count(' ')
                else:
                    if tmpindent and lex[tmpi].type == 'TAB' and lex[tmpi].value.replace('\t',' ').count(' ') == tmpindent:
                        return False
                    else:
                        break
            elif tmpFound:
                lex[tmpi].type = 'IGNORE'
            elif tmpFirstIndent and recordIter and lex[tmpi].type != 'ENDIF':
                if forLoop and (lex[tmpi].type == 'NUMBER' or (lex[tmpi].value in storedVarsHistory and 'type' in storedVarsHistory[
                lex[tmpi].value] and storedVarsHistory[lex[tmpi].value]['type'] == 'NUMBER')) and tmpIter == [] and lex[tmpi+1].type in typeNewline+('ENDIF',):
                    tmpIter.append(makeToken(lex[tmpi],'range(','FUNCTION'))
                    tmpIter.append(deepcopy(lex[tmpi]))
                    tmpIter.append(makeToken(lex[tmpi], ')', 'RPAREN'))
                else: tmpIter.append(deepcopy(lex[tmpi]))
            elif tmpFirstIndent and forLoop and lex[tmpi].type == 'INS' and lex[tmpi].value in ('in ', ' in', 'in'):
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
                elif optLoopAttr and '_append' in lex[tmpi].value and lex[tmpi].value.startswith('AS'):
                    tmpVal = lex[tmpi].value[2:-7]
                    break

        if not forLoop:
            if lex[lexIndex + 1].type == 'NUMBER' or (
                    lex[lexIndex + 1].value in storedVarsHistory and 'type' in storedVarsHistory[
                    lex[lexIndex + 1].value] and storedVarsHistory[lex[lexIndex + 1].value]['type'] == 'NUMBER'):
                tmpIter=[]
                tmpIter.append(makeToken(lex[lexIndex], 'range(', 'FUNCTION'))
                tmpIter.append(deepcopy(lex[lexIndex + 1]))
                tmpIter.append(makeToken(lex[lexIndex], ')', 'RPAREN'))
            else:
                tmpIter = [deepcopy(lex[lexIndex + 1])]
        if isAppending and tmpVal and tmpVal in storedVarsHistory and 'type' in storedVarsHistory[tmpVal] \
        and storedVarsHistory[tmpVal]['type'] in ('LIST', 'LISTCOMP') and 'line' in storedVarsHistory[tmpVal]:
            lex[tmpi].type = 'IGNORE'
            for ii in range(tmpi,len(lex)-1):
                if lex[ii].type in typeNewline:
                    tmpPos=ii-1 ; break
            if storedVarsHistory[tmpVal]['type'] == 'LIST' and len(storedVarsHistory[tmpVal]['line']) <= 2:
                # empty list
                #line.append(decideIfIndentLine(indent, f'{tmpVal} = list(map({func},'))
                tmpPreTokens.append(makeToken(lex[lexIndex], tmpVal, 'ID'))
                tmpPreTokens.append(makeToken(lex[lexIndex], 'list(', 'FUNCTION'))
                tmpPreTokens.append(makeToken(lex[lexIndex], 'map(', 'FUNCTION'))
                tmpPreTokens.append(makeToken(lex[lexIndex], func, 'FUNCTION'))
                tmpPreTokens.append(makeToken(lex[lexIndex], ',', 'COMMA'))
            else:  # non empty list
                line.append(decideIfIndentLine(indent, f'{tmpVal}.extend(map({func},'))
            tmpParen = 2
            tmpFound = True
        elif not isAppending and expressionStart != None and expressionStart+4 < len(lex):
            tmpFound = False

            # list( is needed for globals in functions. in instances where its known function is pure, we should get rid of it
            if lex[expressionStart+1].type == 'FUNCTION' and lex[expressionStart+2].type != 'COMMAGRP' and lex[expressionStart+3].type == 'RPAREN' and lex[expressionStart+4].type in typeNewline:
                if expressionStart + 3 + 2 < len(lex)-1 and lex[expressionStart + 3 + 2].type == 'LOOP': return False
                lex[expressionStart+1].type = lex[expressionStart+2].type = 'IGNORE'
                tmpf = lex[expressionStart + 1].value.replace("(","")
                if tmpf in storedCustomFunctions and storedCustomFunctions[tmpf]['pure']:
                    tmpList = '' ; tmpParen = 1
                else:
                    tmpList = 'list(' ; tmpParen = 2
                tmpFound = True
                tmpPos = expressionStart + 3
                line.append(decideIfIndentLine(indent, f'{tmpList}map({tmpf}, '))
            elif expressionStart+4 < len(lex)-1 and lex[expressionStart+2].type == 'PIPE'  and lex[expressionStart+1].type != 'COMMAGRP' and lex[expressionStart+3].type == 'ID' and lex[expressionStart+4].type in typeNewline:
                if lex[expressionStart + 1 + 4].type == 'LOOP': return False
                for i in range(4):
                    lex[expressionStart + i].type = 'IGNORE'
                tmpf=lex[expressionStart+3].value
                if tmpf in storedCustomFunctions and storedCustomFunctions[tmpf]['pure']:
                    tmpList='' ; tmpParen = 1
                else: tmpList='list(' ; tmpParen = 2
                tmpFound = True
                tmpPos = expressionStart + 1
                line.append(decideIfIndentLine(indent, f'{tmpList}map({tmpf}, '))



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
        #[print(l.type,l.value) for l in lex]
        return tmpFound

    def checkIfImpureFunction(lexIndex,pyDef,functionArgNames):
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
            if lex[lexIndex + tmp].type == 'FUNCTION' and (lex[lexIndex + tmp].value.replace('(', '') in ('open', 'ASopen','input') \
            or (lex[lexIndex + tmp].value.replace('(', '') not in tuple([i for i in storedCustomFunctions if 'pure' in storedCustomFunctions[i] and storedCustomFunctions[i]['pure']]) + pyBuiltinFunctions)):
                if lex[lexIndex + tmp].value.replace('(', '') == lex[lexIndex].value:
                    pass
                else: impure = True ; break
            elif lex[lexIndex + tmp].type == 'BUILTINF' and any(i for i in listMods + ('.open', lex[lexIndex].value + '.') if i in lex[lexIndex + tmp].value):
                impure = True ; break
            elif (lex[lexIndex + tmp].type == 'ID' and lex[lexIndex + tmp].value in storedVarsHistory) \
            or (lex[lexIndex + tmp].value in storedCustomFunctions and 'pure' in storedCustomFunctions[lex[lexIndex + tmp].value] and storedCustomFunctions[lex[lexIndex + tmp].value]['pure'] == False):
                if lex[lexIndex + tmp].value not in functionArgNames:
                    impure = True ; break  # ^ TODO check if ID is const, if it is then its not impure. sorta does this when constants put a [0] on the var name
            elif not pyDef and lex[lexIndex + tmp].type == 'FROM':
                withoutFromSafe = False ; break
            elif lex[lexIndex + tmp].type == 'TAB' and lex[lexIndex + tmp].value.count(' ') < tmpIndent:
                if pyDef: break
                else: withoutFromSafe = True
            elif lex[lexIndex + tmp].type == 'NEWLINE' and tmpIndent==0:
                if pyDef: break
                else: withoutFromSafe=True
            elif lex[lexIndex + tmp].type == 'SCOPE': impure=True ; break
            tmp += 1
        if not pyDef and impure and withoutFromSafe:
            impure = False
        return impure
    def optAddCache():
        nonlocal code, line, lastType, pythonVersion, startOfLine
        tmpCache = 'cache' if pythonVersion >= 3.9 else 'lru_cache'
        if any(i for i in code if f'from functools import {tmpCache}\n' in i) == False:
            code.insert(1, f'from functools import {tmpCache}\n')
        startOfLine = True
        if lastType == 'ASYNC':
            pass  # seems to mess up ASYNC functions
        else:
            line.append(decideIfIndentLine(indent, f'@{tmpCache}{"(maxsize=128)" if tmpCache == "lru_cache" else ""}\n', False))


    line=[]
    anyCheck='all' # for any of syntax
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
    incWrap=['',0]

    # meta
    expPrint=[0,'print']
    ignoreIndentation=False
    functionPassing=False
    pyIs=False
    autoEnumerate=True
    intVsStrDoLen=True

    listcomp={}
    lexIndex=-1
    lastIndent=[0,0,[],[]] # last counted string indent, last indent, last if indent , function indents
    lineNumber=-1 # -1 because two newlines inserted automatically at the start, and linenumber should begin at 1
    storedIndents=[0]
    storedVars={}
    storedCustomFunctions={}
    storedVarsHistory={} # {'ASVarExample':{'type':'STRING','value':'AS is cool! sometimes'}}
    switchCase={'case':False}
    for tok in lex:
        if lexIndex+1 <= len(lex)-1:
            lexIndex+=1
        if hasPiped and lastType != 'PIPE': hasPiped=None
        elif hasPiped==None: hasPiped=False
        # ^ delays hasPiped settings to false by one instruction, for multiple pipes per line. hasPiped is to prevent pipe overrides
        if switchCase['case'] and 'indent' in switchCase and indent<switchCase['indent'] and tok.type != 'OF':
            #print(indent,switchCase['indent']);exit()
            if lex[lexIndex].type == 'ELSE': pass  # ELSE is an exception where switch-case doesn't end yet
            else: switchCase={'case':False}
        # ^ resets switch-case if it detects indentation is smaller or equal, as this means the switch is over
        if expPrint[0] > indent:
            expPrint[0]=indent
            if len(expPrint) > 2:
                expPrint.pop()
        if tok.type=='IGNORE' and tok.value[-1]==')' and parenScope>0: parenScope-=1
        if debug: print(lineNumber,lexIndex,indent,f'({parenScope}','{'+str(bracketScope),f'[{listScope}','%s'%('T' if startOfLine == True else 'F'),'%s'%('T' if indentSoon == True else 'F'),'%s'%('T' if inIf == True else 'F'),'%s'%('T' if (hasPiped or hasPiped==None) else 'F'),'%s'%('T' if notInDef == True else 'F'),tok.type,tok.value.replace('\n','\\n').replace('\t','\\t'))
        if inFuncArg == False or tok.type in typeNewline:
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
                        if compileTo=='Cython' and optimize:
                            if lex[lexIndex+tmp-1].type == 'LINDEX' and lex[lexIndex+tmp].type == 'MINUS': cyWrapAround=False
                            elif lex[lexIndex+tmp-2].type == 'LINDEX' and (lex[lexIndex+tmp-1].type == 'NUMBER' or (lex[lexIndex+tmp-1].type == 'ID' and lex[lexIndex+tmp-1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+tmp-1].value]['type'] == 'NUMBER'  and 'line' in storedVarsHistory[lex[lexIndex+tmp-1].value] and any(True for i in storedVarsHistory[lex[lexIndex+tmp-1].value]['line'] if i.type == 'MINUS')==False)) \
                            and lex[lexIndex+tmp].type == 'RINDEX': cyWrapAround=True

                        tmp+=1
                    functionArgNames = []
                    if all(True if i.type == 'COMMAGRP' else False for i in tmpf):
                        tmpf=' '.join([i.value for i in tmpf])
                        # TODO: make it like below? maybe not
                    else:
                        if debug: print('defunct arg parsing')
                        tmptype=None
                        assign=False
                        newtmp=[]
                        kwargs=None
                        noKwargs=True if len([i for i in tmpf if '*' not in i.value])==0 else False # cython no likely * kwargs
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
                                    else: i.value='**'+i.value
                                    kwargs=None

                                if i.value in convertType:
                                    tmptype=i
                                    if compileTo=='Cython' and tmptype.value=='bool' and noKwargs:
                                        tmpfirst=f'cdef extern from "stdbool.h":\n{" "*prettyIndent}ctypedef bint bool'
                                        if any(True for i in code if i == tmpfirst)==False:
                                            code.insert(1,tmpfirst)
                                elif i.type == 'ASSIGN': assign=True
                                elif i.type in ('TIMES','EXPONENT'): kwargs=i.type ; continue
                                elif assign:
                                    newtmp[-1]+=f' = {i.value}'
                                    assign=False
                                elif tmptype!=None:
                                    storeVar(i,None,lex[0],tmptype)
                                    if compileTo == 'Cython' and noKwargs:
                                        newtmp.append(f'{tmptype.value} {i.value}')
                                    else:
                                        newtmp.append(f'{i.value}: {tmptype.value}')
                                    tmptype=None
                                elif i.type == 'PYPASS': newtmp.append(i.value[2:-2])
                                else: newtmp.append(i.value) ; functionArgNames.append(i.value)
                            elif i.type == 'COMMA' and kwargs!=None:
                                newtmp.append('*')
                                kwargs=None
                        newtmp=[i for i in newtmp if i != ' ']
                        tmpf=', '.join(newtmp)

                    # v impure function detection
                    if impure == False: impure=checkIfImpureFunction(lexIndex,False,functionArgNames)

                    # storing function metadata
                    storedCustomFunctions[tok.value]={}
                    if lex[lexIndex+2].type == 'TYPE':
                        storedCustomFunctions[tok.value]['type']=convertType[lex[lexIndex+2].value]
                    else:
                        storedCustomFunctions[tok.value]['type']=None
                    storedCustomFunctions[tok.value]['pure']=not impure
                    insideDef=tok.value

                    if scopey:
                        storedCustomFunctions[tok.value]['scopes']=scopey
                    if optimize and optFuncCache and not impure:
                        optAddCache()
                    if compileTo == 'Cython' and optimize: # append cy function mods
                        if cyWrapAround:
                            if any(i for i in code if 'cimport cython\n' in i)==False:
                                code.insert(1,'cimport cython\n')
                            line.append(decideIfIndentLine(indent,'@cython.wraparound(False)\n',False))


                    if debug: print(f"{impure =}") #; print(storedCustomFunctions)
                    line.append(decideIfIndentLine(indent,f'def {tok.value}({tmpf}):\n'))
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
                elif tok.value in storedCustomFunctions and '(' not in tok.value and lastType!='FOR' and functionPassing==False:
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
                        tmptok=deepcopy(tok)
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
                elif (tok.type == 'NUMBER' or (tok.value in storedVarsHistory and storedVarsHistory[tok.value]['type']=='NUMBER')) and lastType == 'INS' and lexIndex-3>0 and lex[lexIndex-2].type=='ID' and lex[lexIndex-3].type=='FOR' \
                and (lexIndex+2 < len(lex) and (lex[lexIndex+1].type != 'PIPE' and lex[lexIndex+2].value != 'range')):
                    line.append(f'range({tok.value})') # converts bare numbers into ranges when in for loop
                else:
                    if lastType in typeAssignables+('BUILTINF',) and inFuncArg == False and tok.type!='LISTEND' and lastType!='LIST' \
                    and tok.type not in ('LBRACKET','RBRACKET') and lex[lexIndex-1].type not in ('LBRACKET','RBRACKET'):
                            if (inIf or lex[lexIndex-2].type=='LPAREN' or fstrQuote!='') and not (tok.type=='LIST' and lastType=='STRING'):
                                # if im 'lazy' | if im == 'lazy'
                                line.append('== ')
                            elif lastType in ('ID','OF','BUILTINF') and lex[lexIndex-1].value.strip() not in ('print','print(') and lex[lexIndex-2].value.strip() not in ('print','print('):
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
                            tmptok=deepcopy(tok) ; tmptok.type='MINUS' ; tmptok.value='-'
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
                        elif tok.value in storedVarsHistory and storedVarsHistory[tok.value]['type'] in ('LIST','DICT','LISTCOMP') and autoEnumerate \
                        and lexIndex-3 >= 0 and lex[lexIndex-1].type == 'INS' and lex[lexIndex-2].type == 'COMMAGRP' and inLoop[0]==True:
                            # enumerate in for loop like  for index,value in myList
                            if storedVarsHistory[tok.value]['type'] == 'DICT':
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
                            doPrint=False
                            if tok.type == 'STRING' and tok.value.startswith('"""') or tok.value.startswith("'''"):
                                lineNumber+=tok.value.count('\n')
                                line.append(decideIfIndentLine(indent,tok.value)) ; doPrint=False
                            elif lexIndex+1 <= len(lex):
                                if lex[lexIndex+1].type in typeNewline+typeConditonals+('TRY','ELSE'):
                                    doPrint=True
                                elif lexIndex-1 > 0 and (lex[lexIndex-1].type in typeNewline+('TRY','ELSE') or (lexIndex-3>0 and lex[lexIndex-3].type=='LOOP') or (lex[lexIndex-1].type == 'DEFFUNCT' or (lex[lexIndex-1].type == 'TYPE' and lex[lexIndex-2].type == 'DEFFUNCT'))):
                                    tmp=rParen
                                    rParen+=1
                                    for tmpi in range(lexIndex,len(lex)-1):
                                        if lex[tmpi].type in typeNewline+('FROM',): break
                                        elif lex[tmpi].type not in typePrintable:
                                            rParen-=1 ; break
                                        elif lex[tmpi].type == 'RINDEX' and lex[tmpi+1].type == 'ID':
                                            rParen-=1 ; break
                                        elif tmpi-1 == lexIndex and lex[lexIndex].type == 'ID' and lex[tmpi].type in typeAssignables+('INDEX','LPAREN'):
                                            rParen-=1 ; break
                                    if rParen == tmp: # normal
                                        line.append(f'{" "*(indent)}{tok.value} ')
                                    else: # expression which can be print (no functions or assignment)
                                        line.append(decideIfIndentLine(indent,f'{expPrint[-1]}({tok.value}'))
                                        bigWrap=True
                                else: line.append(decideIfIndentLine(indent,tok.value+' '))
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
                                    # regular  var = 12
                                    storeVar(lex[lexIndex-2],lex[lexIndex],lex[lexIndex+1],position=lexIndex)
                            if lexIndex+1 <= len(lex)-1:
                                if tok.type == 'ID' and lex[lexIndex+1].type == 'BUILTINF':
                                    line.append(tok.value)
                                else:
                                    line.append(tok.value+' ')
                            else:
                                line.append(tok.value+' ')
                            startOfLine=False
            elif tok.type == 'LISTCOMP': # idLISTCOMP
                if lex[lexIndex+1].type == 'ASSIGN' or (lex[lexIndex+1].type == 'BUILTINF' and lex[lexIndex+2].type == 'ASSIGN'):
                    # not a list comp, but a type assign
                    line.append(tok.value)
                    tok.type='ID' ; tok.type = lex[lexIndex-1].type
                elif bracketScope > 0:
                    # not a list comp, but inside of a dict
                    tmp=tok.value.lstrip(':')
                    tok.type = 'COLON' ; tok.value = ':'
                    line.append(': ')
                    listcomp={}
                    miniLex=Lexer()
                    for i in miniLex.tokenize(tmp+' '):
                        lex.insert(lexIndex+1,i)
                elif lex[lexIndex+1].type == 'LIST' and lastType in typeNewline: line.append(tok.value) # Python nested typing
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
            elif tok.type == 'CASE':
                if fstrQuote!='':
                    return AS_SyntaxError(f'case is not allowed inside f-string',None, lineNumber, data)
                if lexIndex+2 <= len(lex)-1 and lex[lexIndex+1].type in typeAssignables+('INDEX','BUILTINF','LPAREN'):
                    if lex[lexIndex+2].type == 'INDEX':
                        lex[lexIndex+1].value=lex[lexIndex+1].value+lex[lexIndex+2].value
                        lex[lexIndex+1].type='IGNORE'
                    tmpf=[]
                    for tmpi in range(lexIndex+1,len(lex)):
                        if lex[tmpi].type in typeNewline: break
                        else: tmpf.append(deepcopy(lex[tmpi])) ; lex[tmpi].type='IGNORE'
                    if len(tmpf)==0:
                        return AS_SyntaxError(f'case is not provided a statement.','case variable', lineNumber, data)
                    switchCase={'case':True,'var':tmpf,'firstIf':True}
                else:
                    return AS_SyntaxError(f'case is not provided a statement.',
                        'case variable'
                        ,lineNumber,data)
            elif tok.type == 'IGNORENL':
                ignoreNewline=True
                lineNumber+=1
            elif tok.type != 'END' and tok.type in typeNewline: # idNEWLINE
                if parenScope>0 and tok.type=='TAB': tok.type='IGNORE' ; continue
                # ^ allows tabs inside of parenthesis to be ignored
                if bracketScope>0 and tok.type in ('TAB','NEWLINE'): tok.type='IGNORE' ; continue
                # ^ allows tabs inside of brackets to be ignored
                if fstrQuote != '':
                    if tok.type != 'THEN':
                        tok.type = 'IGNORE' ; continue
                    elif not tenary:
                        tok.value='}{' ; line.append(tok.value) ; lastType=tok.type='FSTR' ; continue

                if lexIndex+1 < len(lex) and lex[lexIndex+1].type in ('AND','OR'):
                    # allows for multiline conditional
                    continue

                if tok.type == 'THEN' and ('list' in listcomp and 'x' in listcomp)==False:
                    if indentSoon and parenScope == 0 and ':' not in line[-1] :
                        if debug: print('!',line[0])
                        if lexIndex+1 < len(lex) and lex[lexIndex+1].type == 'TAB' and 'while' in line[0]:
                            continue
                        else: line.append(':\n')
                    else:
                        line.append('\n')



                if len(code)==0: code.append(''.join(line)) ; line=[]
                if tok.type == 'TAB' and (lastType not in ('THEN','DEFFUNCT','ENDIF') or code[-1].endswith(':\n')==False) and indentSoon and inReturn==False:
                    line.append(':\n')
                    if lexIndex < len(lex) - 1 and lex[lexIndex + 1].type == 'OF' and switchCase['case'] == True and \
                            switchCase['firstIf'] == False:
                        return AS_SyntaxError(
                            f'of keyword needs line end syntax (tab, newline, then) then expression, not another of',
                            f'of 12 do "my fav num"', lineNumber, data)

                if ignoreNewline and tok.type == 'NEWLINE':
                    ignoreNewline=False
                elif lexIndex+1 <= len(lex)-1 and lex[lexIndex+1].type in ('TAB','ELSE'):
                    startOfLine=True
                elif 'list' in listcomp and 'x' in listcomp:
                    if listcomp['list'].split('[')[0] in storedVarsHistory:
                        if storedVarsHistory[listcomp['list'].split('[')[0]]['type'] == 'NUMBER' and 'value' in storedVarsHistory[listcomp['list'].split('[')[0]]:
                            listcomp['list']=f'range(0,{storedVarsHistory[listcomp["list"].split("[")[0]]["value"]})'
                    if len(line) > 0 and line[-1] == '[':
                        line.append(f'{listcomp["x"]} for {listcomp["x"]} in {listcomp["list"]} ]')
                    elif 'if' in ''.join(line) and 'else' not in ''.join(line):
                        tmpcheck=[i for i in range(0,len(line)-1) if 'if' in line[i]][0]
                        line.insert(tmpcheck, f' for {listcomp["x"]} in {listcomp["list"]} ')
                        line.append(' ]')
                    else:
                        if lex[lexIndex-2].type == 'LISTCOMP' and lex[lexIndex-1].type in typeAssignables and lex[lexIndex-1].value.startswith('f') == False\
                        and (lex[lexIndex-3].type == 'NUMBER' or (lex[lexIndex-3].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-3].value]['type']=='NUMBER')):
                                # cool optimization, faster. returns list*number instead of list-comp
                                line=line[:-2]
                                line.append(f'[{lex[lexIndex-1].value}]*{lex[lexIndex-3].value}')
                        else: line.append(f' for {listcomp["x"]} in {listcomp["list"]} ]')
                    if line[0] == '[': line=f'{" "*(indent*prettyIndent)}{"".join(line)}'
                    listcomp={}
                    startOfLine=True

                if bigWrap: # for putting things at the end
                    if constWrap:
                        if len([i for i in line if ',' in i])>1: line.append(')')
                        else: line.append(',)')
                        if comment: line.append(' # constant')
                        constWrap=False

                    tmp='' # for big functions but parsing ASnake inside
                    while rParen > 0:
                        tmp+=')' ; rParen-=1 ; parenScope=parenScope-1 if parenScope > 0 else 0
                    if tmp !='':
                        if line[-1].endswith('\n'): line.insert(-1,tmp)
                        else: line.append(tmp)

                    # for LOOP syntax i guess
                    if inLoop[0] == None:
                        if tok.type not in typeNewline:
                            line.append(':\n')
                        indent+=prettyIndent
                        startOfLine=True
                        inLoop[0]=True
                        if len(inLoop) > 2:
                            line.append(decideIfIndentLine(indent,inLoop[2]))
                            del inLoop[2]


                    if incWrap[0]!='':
                        if isinstance(line, str): line = [line]
                        while incWrap[1] > 0:
                            line.append('\n') ; startOfLine=True
                            line.append(decideIfIndentLine(indent,incWrap[0]))
                            incWrap[1]-=1
                        incWrap=['',0]

                    bigWrap=False

                if ignoreIndentation and tok.type=='TAB' :
                    tok.type='THEN'

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
                    if lastType=='FUNCTION' and lex[lexIndex-1].value[-1]=='(': check=False
                    if check:
                        tmpcheck=tok.value.replace('\t',' ').count(' ')
                        if debug: print('detected:',tok.value.replace('\t','z').replace(' ','x').replace('\n',''))

                        # QUARINTINE


                        oldIndent=indent

                        while tmpcheck % 2 != 0: tmpcheck+=1 # if odd, make even
                        if storedIndents==[]: storedIndents=[0]


                        while (tmpcheck)<storedIndents[-1]:
                            if len(storedIndents)>1: storedIndents.pop()
                            else: break
                        if storedIndents==[]: storedIndents=[0]

                        indent=storedIndents[-1]

                        if indentSoon and lastIndent[2] and indent == lastIndent[2][-1]:
                            # after a conditional if the indent it lower, we can assume it was meant to be indented
                            indent+=prettyIndent

                        if debug: print(storedIndents)


                        if debug: print(f"detected={tmpcheck} lastDetected={lastIndent[0]} old={oldIndent} new={indent}")
                        # ^ NEW MAGIC
                        if indent < 0: indent=0


                        if '\n' in tok.value and (lexIndex < len(lex)-1 and lex[lexIndex+1].type !='THEN'):
                            if isinstance(line,str): line=[line] # so weird
                            line.append('\n')
                        lastIndent=[tmpcheck,indent,lastIndent[2],lastIndent[3]]
                        if '\n' in tok.value and tok.lineno==1: lineNumber+=1
                else:
                    if indent > 0 and tok.type == 'NEWLINE' and notInDef and not ignoreIndentation and not indentSoon:
                        indent=0
                    if notInDef==False and indent==0: indent+=prettyIndent
                    if ( (tok.type == 'NEWLINE' and (len(line)==0 or line[-1].endswith(':\n')==False) ) \
                    or (tok.type=='THEN' and ':' in tok.value and lexIndex+1 < len(lex) and lex[lexIndex+1].type!='TAB') ) \
                    and inIf == True and inReturn==False:
                        if len(line)==0 or line[-1].endswith(':\n') == False:
                            line.append(':\n')
                            if lexIndex<len(lex)-1 and lex[lexIndex + 1].type == 'OF' and switchCase['case']==True and switchCase['firstIf']==False:
                                return AS_SyntaxError(
                                    f'of keyword needs line end syntax (tab, newline, then) then expression, not another of',
                                    f'of 12 do "my fav num"', lineNumber, data)
                        elif lexIndex+1 < len(lex) and lex[lexIndex+1].type=='NEWLINE':
                            # when doing multi line conditionals on 0 indent, : at the end seems to break the auto-assume-indent. this is due to the NEWLINE token being after THEN token with value of :
                            lex[lexIndex+1].type='IGNORE'

                    if tok.lineno!=0: lineNumber+=tok.value.count('\n')
                    # lineno == 0 means ignore; do not increment LineNumber


                if lastIndent[2]!=[] and indent<lastIndent[2][-1]:
                    lastIndent[2].pop()

                if indent<inLoop[1] or indent==0:
                    inLoop[0]=False
                inLoop[1]=indent

                if combineLines:
                    if isinstance(line,str): line=[line] # so weird
                    line.insert(0,code[-1])
                    code.pop()
                    combineLines=False


                if lexIndex+1<len(lex)-1 and lex[lexIndex+1].type=='ELSE' and len(lastIndent[2]) > 0 \
                and (lastIndent[1] == indent or indent == 0):
                    if line!=[] and re.search('^ *',line[0].replace('\n','')).group().count(' ')==lastIndent[2][-1]:
                        if line!=[]:
                            line[0]=re.sub('^ *',(re.search('^ *',line[0]).group()+(' '*prettyIndent)),line[0])
                        for tmpi in range(len(code)-1,0,-1):
                            tmpindent='' if indent == 0 else ' '*lastIndent[2][-1]
                            tmp=code[tmpi].replace('\n','')
                            if tmp.startswith(f"{tmpindent}if") \
                            or tmp.startswith(f"{tmpindent}elif") \
                            or tmp.startswith('else') \
                            or tmp == f"{' '*(indent+prettyIndent)}else:": break
                            else:
                                if re.search('^ *',tmp).group().count(' ')==lastIndent[2][-1]:
                                    if debug: print('replacingIndent!!',tmp)
                                    tmpindent=re.search('^ *',code[tmpi]).group()+' '*prettyIndent
                                    code[tmpi]=re.sub('^ *',tmpindent,code[tmpi])
                # ^^ fixing indentation when there is statements that are the same indent when there is an else. we know those should be indented.
                tenary=False
                inFuncArg=False
                inIf=False
                inReturn=False
                startOfLine=True
                if lastType != 'THEN': indentSoon=False
                hasPiped=False
                lastType=tok.type # used to be in the NEWLINE else up there, put back if problems?
                #print(line)
                code.append(''.join(line))
                line=[]
                if len(comments)>0 and ((comments[0][1] == lexIndex and startOfLine) or (startOfLine and comments[0][1] < lexIndex)):
                    #print(len(comments)>0 , comments[0][1] == lexIndex , startOfLine,comments[0][1])
                    if comment: line.append(f'\n{comments[0][0]}\n')
                    comments.pop(0)

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
                                ,lineNumber,data)
                else:
                    if optimize and optListPlusListToExtend and lastValue in storedVarsHistory and (
                            storedVarsHistory[lastValue]['type'] in ('LIST', 'LISTCOMP') or (
                            'staticType' in storedVarsHistory[lastValue] and storedVarsHistory[lastValue]['staticType'] == 'list' )):
                        if '+' in tok.value or (lex[lexIndex+1].type == 'ID' and lex[lexIndex+1].value == lastValue and lex[lexIndex+2].type=='PLUS'):
                            if '+' not in tok.value:
                                lex[lexIndex + 1].type = lex[lexIndex + 2].type = 'IGNORE'
                            tok.type='LPAREN' ; rParen+=1 ; bigWrap=True ; tok.value='('
                            lex[lexIndex-1].type = 'BUILTINF' ; lex[lexIndex-1].value = lastValue+'.extend'
                            lastValue=lex[lexIndex-1].value ; lastType='BUILTINF'
                            line[-1]=line[-1][:-1]+'.extend(' ; continue

                    if lastType == 'FUNCTION' and lex[lexIndex-1].value in storedCustomFunctions and '(' not in lex[lexIndex-1].value:
                        storedCustomFunctions.remove(lex[lexIndex-1].value) ; lex[lexIndex-1].type = 'ID'
                    if lastType in {'WHILE', 'IF', 'ELIF', 'ELSE', 'ASYNC', 'RETURN', 'SCOPE', 'BREAK', 'FOR', 'AND', 'OR'}:
                        return AS_SyntaxError(
                            f'Variable \"{lastValue}\" is a reserved keyword, use a different name.',
                            f'variableName {tok.value} {lex[lexIndex + 1].value}', lineNumber, data)
                    if (inIf or fstrQuote!='' or parenScope>0) and tok.value.strip() == 'is':
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
                    elif lastType not in {'ID','INDEX','COMMAGRP','BUILTINF','RINDEX','LISTEND'} and findEndOfFunction(lexIndex-1,goBackwards=True)==False: # syntax error
                        if lastType in typeConditonals:
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
                                else: tmp=f'Line {lineNumber} assigns with invalid type: \"{lastType}\".'
                                return AS_SyntaxError(tmp,'variableName %s %s'%(tok.value.replace(' ',''),lex[lexIndex+1].value),lineNumber,data)
                        elif lex[lexIndex+1].type == 'FSTR' and fstrQuote != '' and pythonVersion >= 3.8:
                            line.append('= ')
                        else:
                            return AS_SyntaxError(f'Line {lineNumber} does not assign to anything.',
                            'variableName is 12'
                            ,lineNumber,data)
                    elif pythonVersion < 3.8 and ':' in tok.value:
                        return AS_SyntaxError(f'Walrus assignment not allowed when compiling to {compileTo if compileTo != "Python" else "Python"+str(pythonVersion)}',
                            'a is 12'
                            ,lineNumber,data)
                    elif pythonVersion < 3.8 and fstrQuote != '' and ':' not in tok.value:
                        tmp='}' ; tmpf=[]
                        for i in range(lexIndex,0,-1):
                            if lex[i].type in typeNewline: break
                            tmpf.append(lex[i].value)
                        tmpf=tmpf[::-1][:-1]
                        return AS_SyntaxError(
                            f'Assignment not allowed inside F-String when compiling to Python{str(pythonVersion)}',
                            f'{"".join(tmpf)}{tmp}{fstrQuote}'
                            , lineNumber, data)
                    else:
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
                    if inIf == False and tok.value in ('=','is ') and lexIndex+2 < len(lex) \
                    and (lex[lexIndex-1].value not in storedVarsHistory or 'staticType' not in storedVarsHistory[lex[lexIndex-1].value]) :
                        if lexIndex+3 < len(lex) and lex[lexIndex+2].type == 'LISTCOMP':
                            storeVar(lex[lexIndex-1],lex[lexIndex+2],lex[lexIndex+3],position=lexIndex+2)
                        else: storeVar(lex[lexIndex-1],lex[lexIndex+1],lex[lexIndex+2],position=lexIndex+1)
                    if (tok.value[0] in {'+','-','**','*','/','//'} or lex[lexIndex+1].type in {'FUNCTION','BUILTINF'} )and lastType == 'ID' \
                    and lex[lexIndex-1].value in storedVarsHistory and 'value' in storedVarsHistory[lex[lexIndex-1].value]:
                        del storedVarsHistory[lex[lexIndex-1].value]['value'] # if value is modified, then we dont know the true value anymore unless its a static value
            elif tok.type in typeConditonals: # idCONDITIONIAL
                if debug: print(indent,lastIndent[2],f'tenary={tenary}')
                if len(listcomp) > 0 or ('list' in listcomp and 'x' in listcomp): # for tenary conditionals inside listcomp
                    if lastType != 'THEN':
                        line.append(codeDict[tok.type]+' ')
                elif ((lastType in typeAssignables+('ASSIGN','FUNCTION','BUILTINF','LPAREN','RPAREN','BOOL','IGNORE','INDEX','COMMAGRP','FSTR','RETURN') and tok.value=='if') or tenary) and startOfLine == False:
                    tenary=True
                    # this section is for altered tenary
                    if lastType in ('ASSIGN','RETURN') or (lastType == 'FSTR' and fstrQuote!=''): # alias:  c is if True then a else b
                        search=False ; tmp=[]
                        for tmpi in range(lexIndex+1,len(lex)-1):
                            if lex[tmpi].type == 'ELSE': break
                            elif search:
                                tmp.append(lex[tmpi].value)
                                lex[tmpi].type=lex[tmpi].type='IGNORE'
                            elif lex[tmpi].type == 'THEN' and lex[tmpi+1].type in typeAssignables+('ASSIGN','FUNCTION','BUILTINF','LPAREN'):
                                lex[tmpi].type=lex[tmpi].type='IGNORE'
                                search=True
                            elif lex[tmpi].type in typeNewline: break
                        if search:
                            line.append(' '.join(tmp)+' ')
                    if lastType in ('BUILTINF','RPAREN'): tok.value=' '+tok.value
                    line.append(tok.value+' ')
                else:
                    if tok.type in ('ELSE','ELIF','OF'):
                            if tok.type == 'OF' and debug: print(switchCase) # needs to be if, not elif
                            if tok.type == 'ELSE' and tenary==False and listScope <= 0: # idELSE
                                tmpFoundIF=False ; tmpFirstNL=False ; tmpFoundReturn=False ; tmpListScope=0
                                for t in range(lexIndex,0,-1): # i guess another tenary detection of sorts
                                    if lex[t].type == 'RETURN': tmpFoundReturn=True
                                    if (lex[t].type == 'RETURN' and tmpFoundIF) or (lex[t].type == 'ASSIGN' and lex[t-1].type == 'ID' and tmpFoundIF):
                                        line.append(tok.value+' ') ; check=False ; break
                                    elif lex[t].type in typeNewline:
                                        if tmpFirstNL: break
                                        check=True ; tmpFirstNL=True
                                    elif lex[t].type == 'IF' and tmpListScope>=0: tmpFoundIF=True # tmpListScope would be negative if we're in list, due to going backwards
                                    elif lex[t].type in ('LIST','LINDEX'): tmpListScope+=1
                                    elif lex[t].type in ('LISTEND','RINDEX'): tmpListScope-=1
                                    elif lex[t].type == 'INDEX': tmpListScope+=lex[t].value.count('[') ; tmpListScope-=lex[t].value.count(']')

                                if tmpFoundIF and check and lastType not in typeNewline:
                                    startOfLine=True
                                    if tmpFoundReturn: lex.insert(lexIndex+1,makeToken(tok,':','COLON'))
                                if not check: lastType='ELSE' ; continue

                                if inIf and not tenary and not startOfLine:
                                    return AS_SyntaxError('You need to end your conditional expression.',f"if {''.join(line).replace('  ',' ')}do 'something'\n# or\n\tif {''.join(line).replace('  ',' ')}then 'something'\n# or\n\tif {''.join(line).replace('  ',' ')}\n\t\t'something'",lineNumber,data)

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
                            elif tok.type == 'OF' and switchCase['case'] == True:
                                if comment: line.append(f'{" "*indent}# of\n')
                                if switchCase['firstIf'] == True:
                                    line.append(decideIfIndentLine(indent,'if '))
                                    switchCase['firstIf']=False
                                    switchCase['indent']=indent
                                else:
                                    indent=switchCase['indent']
                                    line.append(decideIfIndentLine(indent,'elif '))
                                [lex.insert(lexIndex+1,tmptok) for tmptok in reversed(switchCase["var"])]
                                if lex[lexIndex+len(switchCase["var"])+1].type in typeAssignables:
                                    lex.insert(lexIndex+len(switchCase["var"])+1,makeToken(tok,'==','EQUAL'))
                                indentSoon=True ; indent+=prettyIndent
                                startOfLine=False
                                if debug: print('>',indent,switchCase['indent'],line)
                            elif tok.type == 'OF' and switchCase['case'] == False: return AS_SyntaxError('switch case needs case statement','case myVar\n\tof "some" do 1\n\tof "thing" do 2',lineNumber,data)
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
                        inLoop=[True,indent]
                    if tok.type != 'ELSE':
                        inIf=True
                        startOfLine=False
                    if tok.type == 'IF':
                        if indent >= prettyIndent:
                            lastIndent[2].append(indent-prettyIndent)
                        else: lastIndent[2].append(indent)
                        if debug: print(indent,lastIndent[2])
                if startOfLine==False and lastType=='INDEX': line[-1]=f" {line[-1]}"
            elif tok.type == 'FOR':
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
                        if compileTo == 'Cython' and optimize and lastType in typeNewline and lex[lexIndex+1].type == 'ID' and inLoop[0]==False and lex[lexIndex+1].value not in storedVarsHistory and lastIndent[2] == []:
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
                        inLoop=[True,indent]
                    else:
                        line.append(decideIfIndentLine(indent,f'{tok.value} '))
            elif tok.type == 'LOOP': # idLOOP

                if lexIndex+2 < len(lex) and lex[lexIndex+2].type == 'INS':
                    # loop x in y  is just a for loop alias
                    tok.value='for' ; tok.type='FOR'
                    indentSoon = True ; inIf = True
                    line.append(decideIfIndentLine(indent, f'{tok.value} '))
                    indent += prettyIndent ; inLoop = [True, indent] ; continue
                elif lexIndex+2 < len(lex) and lex[lexIndex+2].type in typeOperators+('LPAREN','COMMA'):
                    return AS_SyntaxError('loop syntax must have no operations on it', 'loop iterable iterator', lineNumber, data)

                if lexIndex+3 >= len(lex) or lex[lexIndex+2].type == 'NEWLINE':
                    return AS_SyntaxError('"loop" needs something after it','loop 12 i',lineNumber,data)
                if lex[lexIndex+2].type == 'ID' and lex[lexIndex+2].value not in storedCustomFunctions and lex[lexIndex+2].value not in storedVarsHistory and (lex[lexIndex+2].value != 'print' or (lex[lexIndex+2].value == 'print' and 'print' in storedVarsHistory)) and lex[lexIndex+3].type not in typeOperators+('ASSIGN','PIPE'):
                    forthingin=lex[lexIndex+2].value ; lex[lexIndex+2].type='IGNORE'
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
                            storedVarsHistory[forthingin]={'type':tmp,'staticType':True}

                    if lex[lexIndex+1].type == 'MINUS' and lex[lexIndex+2].type in ('ID','NUMBER','BUILTINF'):
                        lex[lexIndex+1]=deepcopy(lex[lexIndex+2])
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
                                storedVarsHistory[forthingin] = {'type': tmp, 'staticType': True}
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
                            tmptok=deepcopy(tok) ; tmptok.type='THEN' ; tmptok.value=''
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
                                            tmptok=deepcopy(tok) ; tmptok.type='THEN' ; tmptok.value=''
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
                        elif tmp[-1] in storedVarsHistory and storedVarsHistory[tmp[-1]]['type']=='NUMBER':
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
                            endtmpi=0
                            start=lex[lexIndex+1].type
                            convert={'LPAREN':'RPAREN','LBRACKET':'RBRACKET','LIST':'LISTEND'}
                            for tmpii in range(lexIndex+1,len(lex)):
                                tmpf.append(lex[tmpii].value)
                                if lex[tmpii].type == convert[start]:
                                    if lex[tmpii+1].type == 'ID' and lex[tmpii+1].type not in typeOperators+typeCheckers:
                                        forthingin=lex[tmpii+1].value ; lex[tmpii+1].type='IGNORE'
                                    lex[tmpii].type = 'IGNORE'
                                    endtmpi=tmpii ; noRange=True ; search=False ; break
                                lex[tmpii].type = 'IGNORE'
                        # end
                        if debug: print(lex[lexIndex].type,lex[lexIndex+1].type,lex[lexIndex+1].value,lex[lexIndex+2].type)
                        while tmpi < len(lex)-1 and search:
                            if lex[lexIndex+tmpi].type in ('NUMBER','INDEX','ID')+typeOperators:
                                if lex[lexIndex+tmpi].type == 'ID':
                                    if lex[lexIndex+tmpi-1].type not in typeOperators and tmpi>1:
                                        if lexIndex+tmpi+1 < len(lex) and lex[lexIndex+tmpi+1].type in typeOperators: search=False
                                        else:
                                            forthingin=lex[lexIndex+tmpi].value
                                            search=False ; lex[lexIndex+tmpi].type='IGNORE'
                                    else:
                                        if (lex[lexIndex+tmpi].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+tmpi].value]['type']=='NUMBER') \
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
                        if len(tmpf)==0: return AS_SyntaxError('"loop" needs a integer number or iterable','loop 12',lineNumber,data)
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
                        if any(i for i in code if 'from libc.stdio cimport printf\n' in i): pass
                        else: code.insert(1,'from libc.stdio cimport printf\n')
                        line.append(decideIfIndentLine(indent,f'printf("%d\\n",{forthingin})\n'))
                    else:
                        line.append(decideIfIndentLine(indent,f'{expPrint[-1]}({forthingin})'))
                    indent-=prettyIndent ; startOfLine=True
                code.append(''.join(line)) ; line=[]
                inLoop=[True,indent]

            elif tok.type == 'ARE': #idARE
                if lexIndex-1 > 0:
                    if lexIndex+1 < len(lex)-1 :
                        if lex[lexIndex-1].type in ('NUMBER','BOOL'): # error if non iterable
                            return AS_SyntaxError('your value seems to be non-iterable',
                            'myList are equal',lineNumber,data)

                        if tok.value == 'are': tmpcheck=True
                        else: tmpcheck=False
                        line=line[:-1]
                        if lex[lexIndex-1].type == 'COMMAGRP': # group to check
                            tmpfirst=f'({lex[lexIndex-1].value})'
                        elif lex[lexIndex-2].type == 'ANYOF': tmpfirst=lex[lexIndex-1].value
                        else:
                            tmpfirst=[]
                            for tmpi in range(lexIndex-1,0,-1):
                                if lex[tmpi].type in typeNewline+('ANYOF',):
                                    try: line=line[:-line.index(lex[tmpi+1].value)]
                                    except (IndexError, ValueError): line=[]
                                    break
                                else: tmpfirst.append(lex[tmpi].value)
                            tmpfirst=''.join(reversed(tmpfirst))

                        # vv thing to check group with/against
                        if lexIndex+2 < len(lex) and lex[lexIndex+2].type in typeAssignables+('BOOL','INDEX'):
                            check=lex[lexIndex+1].value
                            tmpcheck=lex[lexIndex+2].value
                            lex[lexIndex+2].type = 'IGNORE'
                        elif lex[lexIndex+1].type == 'INS' and lex[lexIndex+1].value.strip() == 'not':
                            if tok.value.strip() == 'are':
                                check='!='
                            else: check = '=='
                        elif lex[lexIndex+1].type in typeAssignables+('BOOL',):
                            check='==' ; tmpcheck=lex[lexIndex+1].value
                        else: check = '=='

                        if check == 'equal':
                            check = '=='
                        elif check in ('unequal','not equal','not ',' not','not','!='):
                            check='!='
                        if check == '==' and tmpcheck in ('False',False):
                            check='!=' ; tmpcheck='True'
                        # check is operator, tmpcheck is object
                        # like check ==     tmpcheck True
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
                            else:
                                line.append(f"{anyCheck}(True for _ in {tmpfirst} if _ {check} {tmpcheck})")
                        lex[lexIndex+1].type = 'IGNORE' ; lex[lexIndex-1].type = 'IGNORE'
                        anyCheck='all'
                    else:
                        return AS_SyntaxError('"are" needs comparison or function in order to check',
                            'are equal\n# or\nmyFunction()',lineNumber,data)
                else:
                    return AS_SyntaxError('"are" needs either a variable, data-type, or function to check!',
                            'if yourVar, "hotdog" are equal then',lineNumber,data)
            elif tok.type == 'ANYOF':
                if lexIndex+1 < len(lex)-1 :
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
            elif tok.type == 'END':
                tmpi=1 ; skipIf=False
                while -1 < lexIndex-tmpi < len(lex)-1:
                    if lex[lexIndex-tmpi].type in typeConditonals \
                    or lex[lexIndex-tmpi].type in ('FOR','LOOP','PYDEF','DEFFUNCT'):
                        if skipIf: skipIf=False
                        else:
                            if len(storedIndents) > 1:
                                indent=storedIndents[-2]
                            else: indent-=prettyIndent
                            if indent < 0: indent=0
                            break
                    elif lex[lexIndex-tmpi].type == 'END': skipIf=True
                    tmpi+=1
                line.append('\n') ; startOfLine=True
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
                    if lex[lexIndex+1].type not in typeNewline+('IGNORE','INC','FSTR','PIPEGO') and lex[lexIndex-1].type != 'IGNORE':
                        lex[lexIndex+1].value=f'{line[-1]},{lex[lexIndex+1].value}'
                        line=line[:-1]
                        lex[lexIndex+1].type='COMMAGRP' ; tok.type='IGNORE' ; lex[lexIndex-1].type='IGNORE'
                        if lex[lexIndex+1].value[-1]==')': parenScope-=1
                        if lex[lexIndex+1].value[-1]=='}': bracketScope-=1
                        if lex[lexIndex+1].value[-1]==']': listScope -= 1
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

                elif lex[lexIndex+1].type != 'ID' and lastType not in ('PIPE','COMMA','FROM','CONSTANT','DEFFUNCT'):
                    return AS_SyntaxError('Type must be declared to a variable.',f'{tok.value} variable = value', lineNumber, data)
                elif lastType not in typeNewline+('CONSTANT','DEFFUNCT','TYPEWRAP'):
                    return AS_SyntaxError('Invalid token before type declaration.',f'{tok.value} {lex[lexIndex+1].value} = value',lineNumber,data)
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
                            tok.value='bool'

                    if compileTo=='Cython' and tok.value=='bool':
                        tmp=f'cdef extern from "stdbool.h":\n{" "*prettyIndent}ctypedef bint bool'
                        if any(True for i in code if i == tmp)==False:
                            code.insert(1,tmp)
                        #tok.value='bint'
                    if lexIndex+2 < len(lex):
                        if lex[lexIndex+2].type == 'DEFFUNCT':
                            return AS_SyntaxError('type assigned to the wrong spot',f'myFunction does {tok.value}',lineNumber,data)
                        elif lex[lexIndex+2].type == 'ASSIGN' and any(i for i in ('+','-','*','/') if lex[lexIndex+2].value.startswith(i)):
                            return AS_SyntaxError('When declaring types, you can\'t modify the variable, as it does not have a value yet.',f'{tok.value} {lex[lexIndex+1].value} = something',lineNumber,data)
                    if lastType == 'DEFFUNCT':
                        if multiType:
                            if line == []:
                                code[-1]=code[-1][:code[-1].rindex(':\n')]+f' -> {firstType.capitalize()}[{secondType}]:\n'
                            else:
                                line[-1]=line[-1].replace(':\n',f' -> {firstType.capitalize()}[{secondType}]:\n')
                            tmp=f'from typing import {firstType.capitalize()}'
                            if any(True for i in code if i == tmp)==False:
                                code.insert(1,tmp)
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
                            tmp=f'from typing import {firstType.capitalize()}'
                            if any(True for i in code if i == tmp)==False:
                                code.insert(1,tmp)
                        else:
                            if compileTo == 'Cython' and inLoop[0]==False and lex[lexIndex+1].value not in storedVarsHistory:
                                    line.append(decideIfIndentLine(indent,f"cdef {tok.value} "))
                            else:# compileTo == 'Python':
                                if pythonVersion >= 3.6: lex[lexIndex+1].value=f"{lex[lexIndex+1].value}: {tok.value}"
                                elif lex[lexIndex+2].type in typeNewline:
                                    if tok.value == 'str': tmp=('""','STRING')
                                    elif tok.value == 'int': tmp=('0','NUMBER')
                                    elif tok.value == 'float': tmp=('0.0','NUMBER')
                                    elif tok.value == 'list': tmp = ('[]', 'INDEX')
                                    elif tok.value == 'dict': tmp=('{}','DICT')
                                    else: tmp=('None','BOOL')
                                    lex.insert(lexIndex+2,makeToken(tok,value=tmp[0],type=tmp[1]))
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
                                            values.append(deepcopy(lex[t]))
                                            tt=t
                                            if lex[t-1].type not in ('TYPE','ID'):
                                                tmptok=deepcopy(tok)
                                                lex.insert(t,tmptok) ; del tmptok ; tt+=1
                                                skip=True
                                            if first or skip:
                                                tmptok=deepcopy(tok)
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
                            line.append(decideIfIndentLine(indent, f"cdef {tok.value} {lex[lexIndex + 1].value.split('=')[0]}\n"))
                        else:
                            line.append(decideIfIndentLine(indent, ' '.join([f"{t}: {tok.value} ;" for t in tmpf])+'\n'  ))
                        startOfLine=True
                    else:
                        if startOfLine: line.append(f'{" "*indent}{tok.value}')
                        else: line.append(tok.value)
                    if multiType: tok.value=firstType
            elif tok.type == 'TYPEWRAP':
                tmptype=tok.value.replace(':','').replace('\n','').replace(' ','')
                isCombined=skip=False
                combine=[[]] ; endtmpi=lexIndex
                for tmpi in range(lexIndex,len(lex)*2):
                    #print(tmpi,lex[tmpi].type,lex[tmpi].value.replace('\n','\\n'),skip)
                    endtmpi+=1
                    if tmpi >= len(lex)-1: break
                    if lex[tmpi].type in ('NEWLINE','FOR','WHILE','LOOP')+typeConditonals: break
                    elif tmpi != lexIndex and lex[tmpi].type == 'TYPEWRAP': break
                    elif lex[tmpi].type == 'TAB':
                        lex[tmpi].value=f"\n{' '*(prettyIndent*indent)}"
                        combine.append([])
                    elif lex[tmpi].type == 'ID' and skip==False and lex[tmpi-1].type in ('CONSTANT','THEN','TAB','TYPEWRAP'):
                        tmptok=deepcopy(tok)
                        tmptok.value=tok.value
                        tmptok.type='TYPE'
                        if lex[tmpi+1].type == 'ID' and compileTo != 'Cython':
                            lex.insert(tmpi,tmptok) ; skip=True
                            tmptok=deepcopy(tok)
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
                            tmptok=deepcopy(tok)
                            tmptok.value='\n'+' '*indent
                            tmptok.type='THEN'
                            lex.insert(endtmpi-1,tmptok)
                            for t in c:
                                tmptok=deepcopy(tok)
                                tmptok.value=t
                                tmptok.type='ID'
                                lex.insert(endtmpi,tmptok)
                                del tmptok
                    if lex[-1].type != 'NEWLINE':
                        tmptok=deepcopy(tok) ; tmptok.value='\n' ; tmptok.type='NEWLINE'
                        lex.append(tmptok)

            elif tok.type == 'CONSTANT': # idCONSTANT
                if (lexIndex+3 < len(lex) or lexIndex+2 < len(lex)) and (lex[lexIndex+1].type == 'ID' or lex[lexIndex+2].type == 'ID'):
                    if compileTo != 'Cython' or (compileTo=='Cython' and ((lex[lexIndex+2].type == 'ASSIGN' and lex[lexIndex+3].type not in ('STRING','LIST','DICT','NUMBER','SET')  or lex[lexIndex+3].value.startswith('f')) or (lex[lexIndex+1].type == 'ID' and lex[lexIndex+2].type not in ('STRING','LIST','DICT','NUMBER','SET') or lex[lexIndex+2].value.startswith('f')) or (lex[lexIndex+2].type == 'ID' and lex[lexIndex+3].type not in ('STRING','LIST','DICT','NUMBER','SET','ASSIGN') or lex[lexIndex+3].value.startswith('f')))):
                        # ^^ when Cython, check if it can be compile-time-constant, else defaults to our implementation
                        tmpval=deepcopy(lex[lexIndex+1])
                        if lex[lexIndex+2].type=='ASSIGN': tmpi=3
                        elif lex[lexIndex+1].type=='TYPE':
                            if lex[lexIndex+4].type=='ASSIGN': tmpi=5
                            else: tmpi=4

                            if lex[lexIndex+1].value in convertType: tmpf=convertType[lex[lexIndex+1].value]
                            else: tmpf=lex[lexIndex+1].value
                            storedVarsHistory[lex[lexIndex+2].value]={'value': lex[lexIndex+2].value+'[0]', 'type': tmpf}
                            if any(i for i in code if 'from typing import Tuple\n' in i): pass
                            else: code.insert(1,'from typing import Tuple\n')
                            lex[lexIndex+1].type=lex[lexIndex+2].type='IGNORE'
                            if lex[lexIndex+3].type != 'ASSIGN':
                                lex[lexIndex+3].value=f"{lex[lexIndex+2].value} : Tuple[{lex[lexIndex+1].value}] = ({lex[lexIndex+3].value}"
                            else:
                                lex[lexIndex+3].value=f"{lex[lexIndex+2].value} : Tuple[{lex[lexIndex+1].value}] = ({lex[lexIndex+4].value}"
                                lex[lexIndex+4].type='IGNORE'
                            lex[lexIndex+3].type='BUILTINF'
                            tmpval=deepcopy(lex[lexIndex+2])
                        else: tmpi=2

                        #print(lex[lexIndex+tmpi].value) ; exit()
                        if lex[lexIndex+tmpi+1].type == 'LISTCOMP': pass
                        elif tmpi>3:
                            for tmptok in range(lexIndex,len(lex)-1):
                                if lex[tmptok].type in typeNewline:
                                    lex[tmptok].value+=')' ; break
                        else:
                            tmptok=deepcopy(tok)
                            tmptok.type='constLPAREN' ; tmptok.value='('
                            lex.insert(lexIndex+tmpi,tmptok) ; del tmptok
                            if lex[lexIndex+tmpi].type != 'ASSIGN' and lex[lexIndex+tmpi-1].type!='ASSIGN': # tmp-1 dubious, solved a bug
                                tmptok=deepcopy(tok)
                                tmptok.type='ASSIGN' ; tmptok.value='='
                                lex.insert(lexIndex+tmpi,tmptok) ; del tmptok
                            #old method: lex[lexIndex+tmpi].value=f'({lex[lexIndex+tmpi].value}'
                        bigWrap=True ; constWrap=True ; miniLineNumber=lineNumber
                        while lexIndex+tmpi < len(lex)-1:
                            #print('woo',lex[lexIndex+tmpi].type,lex[lexIndex+tmpi].value)
                            if lex[lexIndex+tmpi].type == 'ID' and lex[lexIndex+tmpi].value == tmpval.value:
                                   lex[lexIndex+tmpi].value=f'{lex[lexIndex+tmpi].value}[0]'
                                   if lex[lexIndex+tmpi+1].type == 'ASSIGN':
                                        #lex[lexIndex+tmpi].value=f"print('ASnake Warning: Cannot reassign to constant variable {tmpval.value}') ; {lex[lexIndex+tmpi].value}"
                                        return AS_SyntaxError(f'Cannot reassign to constant variable {tmpval.value}',None,miniLineNumber,data,'Compile time error')
                            elif lex[lexIndex+tmpi].type in ('INDEX','FUNCTION','LIST'):
                                if tmpval.value+',' in lex[lexIndex+tmpi].value:
                                   tmp=list(lex[lexIndex+tmpi].value)
                                   tmp.insert(lex[lexIndex+tmpi].value.index(tmpval.value+',')+len(tmpval.value),'[0]')
                                   lex[lexIndex+tmpi].value=''.join(tmp)
                                elif ','+tmpval.value in lex[lexIndex+tmpi].value:
                                   tmp=list(lex[lexIndex+tmpi].value)
                                   tmp.insert(lex[lexIndex+tmpi].value.index(','+tmpval.value)+len(tmpval.value)+1,'[0]')
                                   lex[lexIndex+tmpi].value=''.join(tmp)
                            elif lex[lexIndex+tmpi].type == 'COMMAGRP' and tmpval.value in lex[lexIndex+tmpi].value:
                                tmp=lex[lexIndex+tmpi].value.split(',')
                                for t in range(0,len(tmp)):
                                    if tmp[t] in (tmpval.value,'-'+tmpval.value):
                                        tmp[t]+='[0]'
                                lex[lexIndex+tmpi].value=','.join(tmp)
                            miniLineNumber+=lex[lexIndex+tmpi].value.count('\n')
                            tmpi+=1

                        if (lex[lexIndex+2].type=='ASSIGN' and lex[lexIndex+4].type == 'LISTCOMP'):
                            tmpf=deepcopy(lex[lexIndex+1]);tmpf.value+='[0]'
                            storeVar(tmpf,lex[lexIndex+4],lex[lexIndex+5],position=lexIndex+4)
                            lex[lexIndex+1].value+=' = ('
                            lex[lexIndex+2].type='IGNORE'
                        #if lex[lexIndex+1].type == 'ID' and lex[lexIndex+2].type in ('FUNCTION','BUILTINF'):
                        #    lex[lexIndex+1].value+=' ='
                    elif compileTo == 'Cython':
                        if lex[lexIndex+1].type == 'TYPE': lex[lexIndex+1].type='IGNORE'
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
                    startOfLine=True # in fast mode this will create indents where they dont need to be, but when a indent is needed it does it correctly
                    if optimize and optFuncTricks and optFuncTricksDict['boolTonotnot'] and lex[lexIndex+1].value.strip() == 'bool' and line:
                        line.insert(0, decideIfIndentLine(indent, f"(not not "))
                        line.append(')')
                        lex[lexIndex + 1].type = 'INTOED'
                    else:
                        if line==[]: line.append(lex[lexIndex-1].value)
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
                        elif lex[lexIndex-1].type in ('INDEX','RINDEX') and findEndOfFunction(lexIndex-1,goBackwards=True)!=False:
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
                            for tmpii in range(0,len(line)):
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
                            if lex[lexIndex-1].type in ('NRANGE','INC'):
                                tmpfunc.append(f'({line[-1]})')
                            else: tmpfunc.append(f'({lex[lexIndex-1].value})')
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
                            elif lex[lexIndex+tmpi].value not in ('\n ','\n') \
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
            elif tok.type in ('STRRAW','FSTR','SCOPE','BUILTINF','MINUS','IMPORT','INDEX','LPAREN','RPAREN','FUNCTION','BITWISE','FUNCMOD','WITHAS','ENDIF','LBRACKET','RBRACKET'):
                if tok.type in ('BUILTINF','INDEX','FUNCTION'):
                    if lastType in ('ID','INDEX'):
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
                    if lexIndex+1 < len(lex):
                        if tok.type=='FUNCTION' and lex[lexIndex+1].type == typeAssignables+('ASSIGN','ASSIGN') and tok.value.endswith('()'):
                            tok.value=tok.value.replace('()','')
                            tok.type='ID' ; storedCustomFunctions.remove(tok.value)
                        elif compileTo == 'Cython' and tok.type == 'FUNCTION' and tok.value.replace('(','').strip() == 'print' and lex[lexIndex+2].type == 'RPAREN':
                            tok.type='IGNORE' ; cythonPrint(lex[lexIndex+1])
                            lex[lexIndex+1].type='IGNORE' ; lex[lexIndex+2].type='IGNORE' ; continue # continue might be problematic
                        elif lex[lexIndex+1].type == 'INS': tok.value+=' '

                    if tok.type == 'FUNCTION' and optimize:
                        if optFuncTricks and optLoopAttr and optFuncTricksDict['collapseToFalseTrue'] \
                        and tok.value == 'ASlen' and lex[lexIndex+1].type == 'LPAREN':
                            tok.value+='(' ; lex.remove(lex[lexIndex+1]) # optLoopAttr makes function without '(' at end, so add it

                        if optSortedToSort and tok.value == 'sorted(' and lastType in ('ID','ASSIGN') and 'sorted' not in storedCustomFunctions:
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
                                    code=code[:-1] # removes  var =
                                    tok.type='BUILTINF' ; tok.value=f'{lex[lexIndex-tmp].value}.sort('
                                    if lex[lexIndex + 1].type == 'COMMAGRP': lex[lexIndex+1].value=''.join(lex[lexIndex+1].value.split(',')[1:])
                                    else: lex[lexIndex+1].type = 'IGNORE'
                        elif optFuncTricks and optFuncTricksDict['collapseToFalseTrue'] and tok.value in {'len(','ASlen('} \
                        and lex[lexIndex+1].type == 'ID' and lex[lexIndex+1].value in storedVarsHistory and lex[lexIndex+2].type == 'RPAREN' and lex[lexIndex+3].type in ('GREATER','EQUAL','ASSIGN') \
                        and lex[lexIndex+4].type == 'NUMBER' and lex[lexIndex+4].value == '0' \
                        and (('staticType' in storedVarsHistory[lex[lexIndex+1].value] and storedVarsHistory[lex[lexIndex+1].value]['staticType'] == 'list') \
                        or (storedVarsHistory[lex[lexIndex+1].value]['type'] in ('LIST','LISTCOMP'))):
                            # when var is list, when checking if len is zero
                            # len(x) == 0 -> if not x aka false
                            # len(x) > 0 -> if x aka true
                            tok.type = 'IGNORE'
                            if lex[lexIndex + 3].type == 'GREATER':
                                lex[lexIndex + 2].type = lex[lexIndex + 3].type = 'IGNORE'
                            else:
                                lex[lexIndex + 2] = deepcopy(lex[lexIndex + 1])
                                lex[lexIndex + 1].type='INS' ; lex[lexIndex + 1].value='not'
                                lex[lexIndex + 3].type = 'IGNORE'
                            lex[lexIndex + 4].type = 'IGNORE'


                    if tok.type == 'INDEX':
                        parenScope-=tok.value.count(')') ; parenScope+=tok.value.count('(')
                        listScope -= tok.value.count(']'); listScope += tok.value.count('[')
                    if tok.value[-1] == '(': parenScope+=1
                    if tok.type=='BUILTINF' and tok.value.split('.')[0] in storedVarsHistory \
                    and storedVarsHistory[tok.value.split('.')[0]]['type']=='LIST' and '.'+tok.value.split('.')[1] in listMods and 'value' in storedVarsHistory[tok.value.split('.')[0]]: del storedVarsHistory[tok.value.split('.')[0]]['value']
                    # ^ if list is being modified, we dont know the value anymore, so forget it
                    elif tok.type=='INDEX' and lexIndex-1>0 and lex[lexIndex-1].type in ('ASSIGN','ID'):
                        # just storing var in history if it assigns to index without an assignment token
                        if lex[lexIndex-1].type in ('ID','INDEX','COMMAGRP'): tmpi=1
                        elif lexIndex-2>0 and lex[lexIndex-2].type in ('ID','INDEX','COMMAGRP','BUILTINF'): tmpi=2
                        else: return AS_SyntaxError(f'Assignment needs a var ID',f'myVar is {tok.value}',lineNumber,data)
                        if lex[lexIndex-tmpi].value in storedVarsHistory:
                            storedVarsHistory[lex[lexIndex-tmpi].value]['value'] = 'INDEX'
                        else: storeVar(lex[lexIndex-tmpi],tok,lex[lexIndex+1],position=lexIndex)
                    elif tok.type == 'RPAREN': parenScope=parenScope-1 if parenScope > 0 else 0
                    # vv i dont think i want to print this. i guess only data types will print vv
                    #elif lastType in typeNewline and lexIndex+1 < len(lex) and lex[lexIndex+1].type in typeNewline:
                    #    line.append(decideIfIndentLine(indent,'print(')) ; tok.value+=')'
                    elif lastType in typeNewline and lex[lexIndex].type=='INDEX':
                        check=True
                        for t in range(lexIndex+1,len(lex)):
                            if lex[t].type in typeNewline: break
                            elif lex[t].type in ('FUNCTION','BUILTINF','ASSIGN','PIPE'): check=False ; break
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
                elif tok.type == 'RPAREN': parenScope=parenScope-1 if parenScope > 0 else 0
                elif tok.type == 'FSTR':
                    if startOfLine and lastType in typeNewline+('ELSE','DEFFUNCT'):
                        check=True # if pipe then dont assume print , else do
                        for t in range(lexIndex + 1, len(lex)):
                            if lex[t].type in typeNewline: break
                            elif lex[t].type == 'PIPE': check = False; break
                        if tok.value.replace('f','').startswith('"""') or tok.value.replace('f','').startswith("'''"): check = False
                        if check: line.append(decideIfIndentLine(indent,expPrint[-1]+'(')) ; bigWrap=True ; rParen+=1
                    if tok.value[-1] == fstrQuote:
                        fstrQuote=''
                        if tenary == False and lex[lexIndex+1].type == 'ELSE':
                            lex.insert(lexIndex+1,makeToken(tok,'then','THEN'))
                    elif fstrQuote=='':
                        for i in tok.value:
                            if i == '"': fstrQuote=i ; break
                            elif i == "'": fstrQuote=i ; break
                elif tok.type == 'SCOPE':
                    tmp=' '.join(tok.value.split()[1:]).split(',')
                    for t in tmp:
                        ttmp=t.strip()
                        if ttmp in storedVarsHistory and 'value' in storedVarsHistory[ttmp]:
                            del storedVarsHistory[ttmp]['value']
                            # if function changes variable, then the last value may not be valid


                if tok.type == 'LBRACKET': bracketScope+=1
                elif tok.type == 'RBRACKET':
                    if bracketScope>0: bracketScope-=1
                elif tok.type == 'LPAREN': parenScope+=1

                if tok.type != 'IGNORE': line.append(decideIfIndentLine(indent,tok.value))
                if lexIndex+1 < len(lex) and lex[lexIndex+1].type == 'PIPE': startOfLine=True
                # ^ why do i need that
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
                        if lastType=='ELSE':
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
                            tmptok=deepcopy(tok)
                            tmptok.type='TAB' ; tmptok.value=f"\n{' '*indent}"
                            lex.insert(lexIndex+1,tmptok) ; del tmptok
                        elif lex[lexIndex+1].type == 'NEWLINE':
                            lex[lexIndex+1].type='TAB' ; lex[lexIndex+1].value=f"\n{' '*indent}"
            elif tok.type in ('INS','LIST','COMMAGRP','BOOL','DICT','SET','ASYNC','RETURN','BREAK','LAMBDA'): # printing value with space
                if tok.type == 'INS':
                    if 'are in' in tok.value: tok.value='in'
                    elif inIf and inLoop[0]:
                        # handles: for x in array then for y in x
                        # might need to keep track of parenscopes?
                        for tmpi in range(lexIndex,len(lex)-1):
                            if lex[tmpi].type in typeNewline:
                                if lex[tmpi].type == 'TAB': lex[tmpi].type='THEN'
                                break

                    if startOfLine and not inIf:
                        doPrint=True
                        for tmpi in range(lexIndex + 1, len(lex)-1):
                            if lex[tmpi].type in typeNewline:
                                break
                            elif lex[tmpi].type not in typePrintable:
                                doPrint = False
                        if doPrint: line.append(decideIfIndentLine(indent,f'{expPrint[-1]}(')); bigWrap = True; rParen += 1

                # 1,2,3 in a
                if tok.type == 'COMMAGRP':
                    if lastType == 'ID' and lex[lexIndex-1].value!='print' and inIf == False and findEndOfFunction(lexIndex-1,goBackwards=True)==False:
                        line.append(' = ')
                    elif lastType in 'TYPE' and lex[lexIndex-2].type in typeNewline+('CONST',) and '=' in tok.value:
                        tmp=tok.value.split('=')[0].split(',')
                        for t in tmp: storedVarsHistory[t.replace(' ','')]={'type':convertType[lastValue],'staticType':True}
                    else:
                        listScope += tok.value.count('[')
                        listScope -= tok.value.count(']')
                elif tok.type == 'LIST': inIf=True
                elif tok.type == 'RETURN' and 'yield' not in tok.value:
                    if lastType not in typeNewline:
                        line.append('\n') ; startOfLine=True
                    inIf=True ; inReturn=True
                if tok.type in ('BOOL','LIST','SET') and lexIndex>0 and lex[lexIndex-1].type == 'ID':
                # var = True
                    if inIf:
                        if tok.type=='BOOL' and optimize and optIfTrue and tok.value=='True' and ((lex[lexIndex-1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-1].value]['type'] == 'BOOL') or lex[lexIndex-1].type == 'OF'):
                            tok.value='' ; tok.type='IGNORE'
                        else:
                            tok.value=f'== {tok.value}'
                    else:
                        storeVar(lex[lexIndex-1],lex[lexIndex],lex[lexIndex+1],position=lexIndex)
                        tok.value=f'= {tok.value}'
                elif inIf and tok.type == 'BOOL' and optimize and optIfTrue and tok.value=='True' and lastType in ('EQUAL','ASSIGN') and ':' not in lastValue and lex[lexIndex-2].type=='ID' and lex[lexIndex-2].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-2].value]['type'] == 'BOOL':
                    # optIfTrue
                    tok.value='' ; tok.type='IGNORE'
                    lex[lexIndex-1].type='IGNORE'
                    line=line[:-1]
                if tok.type == 'COMMAGRP' and tok.value.endswith('('): parenScope+=1



                if tok.type in ('LIST','BOOL','DICT','SET') and  startOfLine and lexIndex+1 < len(lex) and lex[lexIndex+1].type in typeNewline and lex[lexIndex-1].type not in typeConditonals and inIf==False:
                    line.append(decideIfIndentLine(indent,f'{expPrint[-1]}({tok.value})'))
                else: line.append(decideIfIndentLine(indent,f'{tok.value} '))
                if len(tok.value)>0 and tok.value[0]=='=': tok.value[1:] # removing = for  var = True
                if lexIndex+1 < len(lex) and lex[lexIndex+1].type=='PIPE' and (line==[] or lastType in typeNewline): startOfLine=True
            elif tok.type == 'IGNORE':
                pass
            elif tok.type == 'PYDEF': # support for Python style function define
                if optimize and checkIfImpureFunction(lex.index(tok),True,(None,)) == False:
                    # TODO: replace (None,) with function argument vars ^
                    optAddCache()
                if tok.value.replace(' ','')[-1] != ':': tok.value+=':'
                line.append(decideIfIndentLine(indent,f'{tok.value}\n'))
                startOfLine=True ; indent+=prettyIndent
                if ''.join(tok.value.split('(')[1:]).count(',') >= ''.join(tok.value.split('(')[1:]).count('=') and ''.join(tok.value.split('(')[1:]).count(',') > 0:
                    pass # if it has required arguments, dont auto add () when called without it
                else:
                    tmp=tok.value.split('def')[1].split('(')[0].replace(' ','')
                    if tmp not in storedCustomFunctions: storedCustomFunctions[tmp]={}
                    storedCustomFunctions[tmp]['type']=None
            elif tok.type == 'PYPASS':
                line.append(tok.value[2:][:-2])
                lineNumber+=tok.value.count('\n')
            elif tok.type == 'TRY':
                if ':' not in tok.value: tok.value+=':'
                else: tok.value+='\n'#indentSoon=True
                if tok.value.startswith('except') and lastType not in ('NEWLINE','TAB'):
                    if lastType!='THEN':
                        line.append('\n') ; startOfLine=True
                    indent-=prettyIndent
                    if indent<0: indent=0
                line.append(decideIfIndentLine(indent,f'{tok.value}\n'))
                indent+=prettyIndent ; startOfLine=True
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

                if lastType == 'INS' and lastValue in ('in ','in') and lex[lexIndex+1].type not in typeNewline:
                    # if its a for loop and experession is after the NRANGE, asssume indent
                    check=False
                    for tmpi in range(lexIndex,0,-1):
                        if lex[tmpi].type in typeNewline: break
                        elif lex[tmpi].type == 'FOR' and lex[tmpi-1].type in typeNewline:
                            check=True ; break
                    if check:
                        indentSoon=True
                        lex.insert(lexIndex+1,makeToken(tok,'do','THEN'))

            elif tok.type == 'META':
                tmpf=tok.value.split('=')[0].replace(' ','').replace('$','') # needs replace instead of strip()
                if tmpf in ('defexp','defaultExpression','defaultPrint','expPrint','defprint'):

                    tmp=tok.value[tok.value.index('=')+1:]
                    while tmp[0] == ' ': tmp=tmp[1:]
                    while tmp[-1] == ' ': tmp=tmp[:-1]
                    expPrint.append(tmp)
                    expPrint[0]=indent
                elif tmpf in {'ignoreIndentation','ignoreIndent','noindent','noIndent','noIndentation'}:
                    tmp=tok.value.split('=')
                    if len(tmp) > 1:
                        tmp=tmp[1]
                        if tmp.lower() in ('true','yes','on'):
                            ignoreIndentation=True
                        elif tmp.lower() in ('no','false','off'):
                            ignoreIndentation=False
                        else: ignoreIndentation=True
                    else: ignoreIndentation=True
                elif tmpf in {'funcPass','funcpass','passFunction','functionPass','pyfunc','pyFunc'}:
                    tmp=tok.value.split('=')
                    if len(tmp) > 1:
                        tmp=tmp[1]
                        if tmp.lower() in ('true','yes','on'):
                            functionPassing=True
                        elif tmp.lower() in ('no','false','off'):
                            functionPassing=False
                        else: functionPassing=True
                    else: functionPassing=True
                elif tmpf in {'pyis','pythonIs','pyIs','isPython','pythonis','isIdentity'}:
                    tmp=tok.value.split('=')
                    if len(tmp) > 1:
                        tmp=tmp[1]
                        if tmp.lower() in ('true','yes','on'):
                            pyIs=True
                        elif tmp.lower() in ('no','false','off'):
                            pyIs=False
                        else: pyIs=True
                    else: pyIs=True
                elif tmpf in metaPyCompat:
                    pyIs=True ; functionPassing=True ; expPrint.append('') ; autoEnumerate=False ; intVsStrDoLen=False
                elif tmpf in {'asnake','Asnake','ASnake'}:
                    pyIs=False ; functionPassing=False
                elif tmpf in {'Cython','cython'}:
                    compileTo='Cython'
                elif tmpf in {'Python','python'}:
                    compileTo='Python'
                elif tmpf in inlineReplace:
                    miniLex = Lexer()
                    tmp=[]
                    for t in miniLex.tokenize(inlineReplace[tmpf] + ' '):
                        tmp.append(t) # need to reverse it, so append to temporary list we then reverse
                    for t in reversed(tmp):
                        lex.insert(lexIndex+1,t)
                        if debug: print('--', t)
                    del miniLex
                elif tmpf in metaPyVersion:
                    pythonVersion = float(tok.value.split('=')[-1].strip()) # in pre-phase it already checked if it was float



            elif tok.type == 'INC': #idINC
                if lexIndex-1 > 0 and (lex[lexIndex-1].type in typeNewline+('TRY','ENDIF','ELSE') or (lexIndex-3>0 and (lex[lexIndex-3].type=='LOOP' or lex[lexIndex-2].type=='LOOP'))):
                    if lex[lexIndex+1].type in typeNewline+typeConditonals or inIf:
                        doPrint=False
                    else:
                        doPrint=True
                        for tmpi in range(lexIndex+1,len(lex)):
                            if lex[tmpi].type in typeNewline: break
                            elif lex[tmpi].type not in typePrintable:
                                doPrint=False
                else: doPrint=False
                if tok.value.startswith('++') or tok.value.startswith('--'):
                    if lastType in ('IF','ELIF') and pythonVersion >= 3.8:
                        tmp=tok.value[2:]
                        line.append(f'({tmp} := {tmp} {tok.value[0]} 1)')
                    else:
                        tmp=tok.value[2:]+' '
                        if lastType == 'ELIF':
                            return AS_SyntaxError(f"{tok.value} cannot be after an elif when compiling to {compileTo}",f'if {tok.value}',lineNumber,data)
                        if doPrint: tmp=f'{expPrint[-1]}({tmp}' ; bigWrap=True ; rParen+=1
                        if lex[lexIndex+1].type not in typeNewline or lex[lexIndex-1].type not in typeNewline:
                            if lastType == 'INC': line.append('== ')
                            line.append(decideIfIndentLine(indent,tmp))
                        if inIf: indent-=prettyIndent
                        startOfLine=True
                        if code[-1].endswith('= '):
                            code.insert(-1,f'{tok.value[2:]}{tok.value[0]}=1\n')
                        else:
                            if lastType == 'IF': #in ('IF','WHILE'):
                                code.insert(-1,decideIfIndentLine(indent,f'{tok.value[2:]}{tok.value[0]}=1\n'))
                            elif lastType == 'WHILE':
                                # increment at start
                                bigWrap = True
                                incWrap = [f'{tok.value[2:]}{tok.value[0]}=1\n',incWrap[1]+1]
                            else:
                                line.insert(0,decideIfIndentLine(indent,f'{tok.value[2:]}{tok.value[0]}=1\n'))
                        if inIf: indent+=prettyIndent
                elif tok.value.endswith('++') or tok.value.endswith('--'):
                    bigWrap=True
                    tmp=tok.value[:-2]+' '
                    if lastType == 'WHILE':
                        # increment at end
                        line.append(tmp)
                        tmpIndent=None
                        for tmpi in range(lexIndex,len(lex)):
                            if tmpIndent == None:
                                if lex[tmpi].type in typeNewline:
                                    if lex[tmpi].type == 'TAB':
                                        tmpIndent = lex[tmpi].value.replace('\t',' ').count(' ')
                                    else: tmpIndent = 0
                            else:
                                if lex[tmpi].type in typeNewline:
                                    found=False
                                    if lex[tmpi].type == 'TAB' and lex[tmpi].value.replace('\t', ' ').count(' ') < tmpIndent:
                                        found=True
                                    elif lex[tmpi].type == 'NEWLINE': found=True
                                    if found:
                                        lex.insert(tmpi, deepcopy(lex[lexIndex]))
                                        lex.insert(tmpi, makeToken(lex[tmpi],'then','THEN'))
                                        break

                    else:
                        if doPrint: tmp=f'{expPrint[-1]}({tmp}' ; rParen+=1
                        if lastType == 'INC': line.append('== ')
                        if lex[lexIndex+1].type not in typeNewline or lex[lexIndex-1].type not in typeNewline:
                            line.append(decideIfIndentLine(indent,tmp))
                        incWrap=[f'{tok.value[:-2]}{tok.value[-1]}=1\n',incWrap[1]+1]
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
            elif tok.type in typeCheckers:
                if intVsStrDoLen and (lastType in ('STRING','LIST','LISTCOMP','DICT','TUPLE') or (lexIndex-1 > 0 and lex[lexIndex-1].type=='ID' and lex[lexIndex-1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-1].value]['type'] in ('STRING','LIST','LISTCOMP','DICT','TUPLE'))) \
                and lexIndex+1 < len(lex) and (lex[lexIndex+1].type in ('NUMBER','INC') or ((lex[lexIndex+1].type=='ID' and lex[lexIndex+1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+1].value]['type']=='NUMBER'))):
                    line[-1]=line[-1].replace(lex[lexIndex-1].value,f"len({lex[lexIndex-1].value})")
                    line.append(decideIfIndentLine(indent,f'{codeDict[tok.type]} '))
                elif intVsStrDoLen and (lastType in ('NUMBER','INC') or (lexIndex-1 > 0 and lex[lexIndex-1].type=='ID' and lex[lexIndex-1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex-1].value]['type']=='NUMBER')) \
                and lexIndex+1 < len(lex) and (lex[lexIndex+1].type in ('STRING','LIST','LISTCOMP','DICT','TUPLE') or ((lex[lexIndex+1].type=='ID' and lex[lexIndex+1].value in storedVarsHistory and storedVarsHistory[lex[lexIndex+1].value]['type'] in ('STRING','LIST','LISTCOMP','DICT','TUPLE')))):
                    tmp=False
                    for tmpi in range(lexIndex,len(lex)):
                        if lex[tmpi].type in typeNewline: break
                        elif lex[tmpi].type == 'RINDEX':
                            lex[tmpi].value+=')' ; tmp=(True,tmpi) ; break
                    line.append(f"{codeDict[tok.type]} len(")
                    if tmp == False: line[-1]+=f'{lex[lexIndex+1].value})'; lex[lexIndex+1].type = 'IGNORE'
                    else:
                        if lex[tmp[1]+1].type == 'PIPE' and lex[tmp[1]+2].value == 'len':
                            lex[tmp[1]+2].type='IGNORE' ; lex[tmp[1]+1].type='IGNORE';lex[tmp[1]+1].value='IGNORE'
                else: line.append(decideIfIndentLine(indent,f'{codeDict[tok.type]} '))
            elif tok.type == 'RINDEX':
                listScope -= 1
                if listScope < 0: listScope = 0
                if lex[lexIndex+1].type in typeAssignables:
                    if inIf: line.append(tok.value+' == ')
                    else: line.append(tok.value+' = ')
                else: line.append(tok.value)
            elif tok.type == 'LINDEX':
                listScope+=1
                if lastType == 'ID' and lex[lexIndex-2].type == 'TYPE':
                    line.append(' = '+tok.value)
                else: line.append(tok.value)
            elif tok.type == 'DEFEXP':
                line.append(decideIfIndentLine(indent, f'{expPrint[-1]}(')) ; rParen+=1 ; bigWrap=True
            elif tok.type in codeDict:
                if tok.type == 'DEFFUNCT': notInDef=False
                elif lastType == 'BUILTINF' and tok.type in ('AND','OR') and not startOfLine:
                    line.append(' ')
                line.append(decideIfIndentLine(indent,f'{codeDict[tok.type]} '))

            if lastType=='TRY': indentSoon=False

            lastType=tok.type
            lastValue=tok.value

            if tok.type not in ('ENDIF','FROM') or (startOfLine and tok.type in typeConditonals) or inIf==False:
                if storedIndents==[]: storedIndents=[0]
                elif indent>storedIndents[-1]:
                    storedIndents.append(storedIndents[-1]+prettyIndent) # change to .append(indent) if problems
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
    if debug: print('len of lex',len(lex)-1)
    return '\n'.join(code)
        
    
def execPy(code,fancy=True,pep=True,run=True,execTime=False,headless=False):
    if pep:
        if execTime:
            s=time()
        code=fix_code(code)
        if execTime:
            print('autopep8 time:', time() - s)
    if fancy:
        print(code.replace('\n\n','\n'))
        if run: print('\t____________\n\t~ Python Eval\n')
    if run:
        import subprocess as sp
        import platform
        if 'linux' in platform.system().lower():
            proc = sp.Popen("ls -ls /usr/bin/python* | awk '/-> python3/ {print $10 ;exit}'", shell=True, stdout=sp.PIPE, stdin=sp.PIPE)
            pyCall = proc.stdout.readline().decode().split('/')[-1].strip()
        else:
            pyCall = 'python'

        if headless:
            with open('ahrscriptExec.py','w') as f:
                f.write(code)
            if execTime:
                s = time()
            child = sp.Popen(f'{pyCall} ahrscriptExec.py', stdout=sp.PIPE, cwd=os.getcwd(), shell=True)
            child.communicate()
        else:
            if execTime:
                s = time()
            sp.run([pyCall, "-c", code])


        if fancy:
            print('\t____________')
        if execTime:
            print('exec time:',time()-s)
    #elif fancy:
    #    print('run was disabled, no output')
    try: os.remove('ahrscriptExec.py')
    except: pass
    #exec(code,None,locals())

        
if __name__ == '__main__':
    from argparse import ArgumentParser, FileType
    argParser=ArgumentParser()
    argParser.add_argument('-r', '--run', action='store_true', help="Compiles file in memory then runs it.")
    #argParser.add_argument('-e', '--eval', action='store_true', help="Compiles ASnake in a string to Python and runs it.")
    argParser.add_argument('-v', '--version', action='store', help="Specify which Python version to compile to.")
    argParser.add_argument('-c', '--compile', action='store_true', help="Compiles file to .py and writes it on disk. On Cython will attempt to compile to .so file.")
    argParser.add_argument('-o', '--optimize', action='store_true', help="Toggles optimization on and off. On by default.")
    argParser.add_argument('-f', '--fast', action='store_true', help="Turns off code formatting, and turns off optimization. Useful for fast compile times.")
    argParser.add_argument('-nc', '--no-comment', action='store_true', help="Turns off comments in the compiled file.")
    argParser.add_argument('-np', '--no-print', action='store_true', help="Doesn't print the compiled file on console.")
    argParser.add_argument('-jr', '--just-run', action='store_true', help="Will run compiled version of file if it exists, otherwise will compile and run it.")
    argParser.add_argument('-cy', '--cython', '--Cython', action='store_true', help="Compiles the code to Cython and .pyx")
    argParser.add_argument('-pp', '--pypy', '--PyPy', action='store_true', help="Compiles to be compatible with latest PyPy3 Runtime.")
    argParser.add_argument('-ps', '--pyston', '--Pyston', action='store_true', help="Compiles to be compatible with Pyston runtime.")
    argParser.add_argument('-a', '--annotate', action='store_true',help="When compiling to Cython, will compile a html file showing Python to C conversions.")
    argParser.add_argument('-d', '--debug', action='store_true', help="Debug info for compiler developers.")
    argParser.add_argument('-t', '--test', action='store_true', help="Headless debug for compiler developers.")
    argParser.add_argument("file", type=FileType("r"), help="Your ASnake file to compile.")

    compileTo='Python' ; pythonVersion=3.9
    enforceTyping=compileAStoPy=runCode=headless=debug=justRun=False
    comment=optimize=pep=fancy=True

    args = argParser.parse_args()
    try:
        ASFile=args.file.name
        if not os.path.isfile(ASFile): raise Exception
        data=args.file.read()
    except: print('Couldn\'t open file :(\nCheck to make sure the path is correct.');exit()
    del args.file
    if args.run: runCode=True
    if args.test: headless=True
    if args.fast: pep=False ; optimize=False
    if args.optimize:
        if optimize: optimize=False
        else: optimize=True
    if args.debug: debug=True
    if args.no_print: fancy=False
    if args.compile: compileAStoPy=True
    if args.cython: compileTo='Cython'
    if args.pyston: compileTo='Pyston'
    if args.pypy: compileTo='PyPy3'
    if args.no_comment: comment=False
    if args.just_run:
        justRun=True ; fancy=False ; runCode=True ; pep=False
    if args.version:
        try: pythonVersion=float(args.version)
        except: pass

    if compileTo=='Cython': enforceTyping=True
    
    s=time()
    if (compileTo == 'Cython' and justRun) == False:
        code=build(data,comment=comment,optimize=optimize,debug=debug,compileTo=compileTo,pythonVersion=pythonVersion,enforceTyping=enforceTyping)
    else: code=''
    print('build time:',time()-s)
    for i in range(2): # removes double newlines (ignores strings)
        code=re.sub(r"""\n\n(?=([^"'\\]*(\\.|("|')([^"'\\]*\\.)*[^"'\\]*("|')))*[^"']*$)""",'\n',code) 
    if compileAStoPy:
        filePath='/'.join(ASFile.split('/')[:-1])+'/'
        ASFile='.'.join(ASFile.rsplit('.')[:-1])
        ASFile = "".join(x for x in ASFile.split('/')[-1] if x.isalnum())
        fileName=f'{ASFile}.py{"x" if compileTo=="Cython" else ""}'
        if pep: code=fix_code(code)
        if filePath=='/': filePath=''
        with open(filePath+fileName,'w',encoding='utf-8') as f:
            f.write(code)
        if compileTo == 'Cython':
            if "'" in fileName:
                fileName=fileName.replace("'","\\'")
            from platform import system as OSName
            from subprocess import check_output
            if 'windows' in OSName().lower():
                py3Command='"'+(check_output(['WHERE', 'python']).decode().split('\n')[0]).replace('\r','')+'"'
            else: # linux
                if len(check_output(['which', 'python3']).decode()) > 0:
                    py3Command='python3'
                else: py3Command='python'
            with open('ASsetup.py', 'w') as f:
                f.write(f"""
{'import numpy' if 'import numpy' in data else ''}
from setuptools import setup
try:
    from Cython.Build import cythonize
except ModuleNotFoundError:
    print('Cython is not installed, ASnake is unable to compile to .so file.\\nThe .pyx file still compiled.\\nDo something like:\\n\\t{"python" if "windows" in OSName().lower() else py3Command} -m pip install cython') ; exit()
setup(ext_modules = cythonize('{filePath + fileName}',annotate={True if args.annotate else False}),
{'include_dirs=[numpy.get_include(),"."]' if 'import numpy' in data else 'include_dirs=["."]'})""")
            os.system(f'{py3Command} ASsetup.py build_ext --inplace')
            os.remove('ASsetup.py')
            if runCode:
                execPy(code,run=False,execTime=False,pep=pep,headless=headless,fancy=fancy)
                if '/' in ASFile: tmp=f"import sys\nsys.path.append('{ASFile.split('/')[-1]}')\nimport {ASFile.split('/')[-1]}"
                else: tmp=f'import {ASFile}'
                execPy(tmp,run=runCode,execTime=True,pep=False,headless=headless,fancy=False)
        if fancy: print(f'{ASFile}.asnake compiled to {fileName}')
    else:
        if compileTo == 'Cython':
            ASFile='.'.join(ASFile.rsplit('.')[:-1])
            execPy(code,run=False,execTime=False,pep=pep,headless=headless,fancy=fancy)
            if '/' in ASFile:
                tmpASFile=ASFile.split('/')[-1].replace("'","\\'")
                ASFile=ASFile.replace("'","").replace("_",'')
                tmp=f"import sys\nsys.path.append('{tmpASFile}');import {ASFile.split('/')[-1]}"
            else: tmp=f'import {ASFile}'
            execPy(tmp,run=runCode,execTime=True,pep=False,headless=False,fancy=False)
        else:
            execPy(code,run=runCode,execTime=True,pep=pep,headless=headless,fancy=fancy)
    #print(PyToAS(data))
