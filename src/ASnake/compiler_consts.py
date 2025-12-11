codeDict={'RDIVIDE':'//','DIVIDE':'/','PLUS':'+','MINUS':'-','TIMES':'*','LPAREN':'(','RPAREN':')',
    'ELIF':'elif','ELSE':'else','IF':'if','WHILE':'while','GREATEQ':'>=','GREATER':'>','LESS':'<',
    'LESSEQ':'<=','EQUAL':'==','ASSIGN':'=','NOTHING':'pass','NOTEQ':'!=','BUILTINF':'.','OF':'elif',
    'AND':'and','OR':'or','RETURN':'return','FOR':'for','MODULO':'%','EXPONENT':'**','COMMA':',',
    'LISTEND':']','ELLIPSIS':'...','constLPAREN':'(','COLON':':','LINDEX':'[','RINDEX':']',
    "DQUOTE":'"',"SQUOTE":"'", 'LBRACKET':'{','RBRACKET':'}','PYIS':' is '}

convertType={'int':'NUMBER','float':'NUMBER','Py_ssize_t':'NUMBER','bool':'BOOL','bint':'BOOL','str':'STRING','list':'LIST','dict':'DICT','type':'TYPE','tuple':'TUPLE','set':'SET','bytes':'STRING','object':'ID','range':'FUNCTION','complex':'NUMBER','frozenset':'FUNCTION','bytearray':'STRING','memoryview':'FUNCTION','filter':'FUNCTION'}
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
metaElseVersion = {'elseVersion','elseIfVersion','elsever','versionElse'}
metaFunctionLineWrap = {'funcWrap','functionLineWrap','functionWrap','fwrap'}
metaPyFunc = {'funcPass','funcpass','passFunction','functionPass','pyfunc','pyFunc'}
metaDefExp = {'defexp','defaultExpression','defaultPrint','expPrint','defprint'}
metaIgnoreDefExpFunc = {'noDefExpOnFunc', 'defExpIgnoreFunction', 'defaultExpressionIgnoreFunction', 'ignoreDefExpFunction'}

