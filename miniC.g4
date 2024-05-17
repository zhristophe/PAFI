grammar miniC;

compilationUnit
    :   translationUnit? EOF
    ;

translationUnit
    :   (modeMarker {setMarker(*root_);})?
    (   functionDefinition                         
    |   statement           
    |   declaration 
    |   startOrEndMarker 
    )+ 
    ;

startOrEndMarker
    :   StartMarker    
    |   EndMarker   
    ;

condMarker
    :   CondMarker Identifier
    ;

modeMarker
    :   ModeMarker Identifier
    ;

// expressions
expression
    :   primaryExpression
    |   expression
        (   '[' expression ']'     
        |   '(' args=argumentExpressionList? ')'
        |   arrow=('.' | '->') id=Identifier 
        |   '++'                 
        |   '--'                    
        )
    |   ('sizeof' | '_Alignof') '(' typeName ')'    
    |   '(' typeName ')' expression               
    |   op=('++' | '--'
        |   '+' | '-'
        |   '!' | '~'
        |   '*'
        |   '&'
        |   'sizeof'
        ) expression                                
    |   expression op=('*'|'/'|'%') expression     
    |   expression op=('+'|'-') expression          
    |   expression op=('<<'|'>>') expression  
    |   expression op=('<'|'<='|'>'|'>=') expression
    |   expression op=('=='|'!=') expression        
    |   expression '&' expression               
    |   expression '^' expression               
    |   expression '|' expression                 
    |   expression '&&' expression                 
    |   expression '||' expression             
    |   <assoc=right> expression
        op=('='
        |   '+='
        |   '-='
        |   '*='
        |   '/='
        |   '%='
        |   '<<='
        |   '>>='
        |   '&='
        |   '^='
        |   '|='
        )
        expression                                  
    ;

constantExpression
    :   expression
    ;

primaryExpression
    :   Identifier       
    |   IntegerConstant   
    |   FloatingConstant  
    |   CharacterConstant
    |   StringLiteral   
    |   '(' commaExpression ')'
    ;

genericSelection
    :   '_Generic' '(' expression ',' genericAssocList ')'
    ;

genericAssocList
    :   genericAssociation (',' genericAssociation)*
    ;

genericAssociation
    :   (typeName | 'default') ':' expression
    ;

argumentExpressionList
    :   expression (',' expression )*
    ;

commaExpression
    :   expression
    |   commaExpression ',' expression
    ;

// Declarations
declaration 
    :   declarationSpecifiers initDeclaratorList ';'
    |   declarationSpecifiers ';'
    ;

declarationSpecifiers
    :   (   storageClassSpecifier
        |   typeSpecifier
        |   typeQualifier
        |   functionSpecifier
        |   alignmentSpecifier
        )+
    ;

initDeclaratorList
    :   initDeclarator (',' initDeclarator)* 
    ;

initDeclarator
    :   declarator ('=' initializer)?
    ;

storageClassSpecifier
    :   'typedef'      
    |   'extern'       
    |   'static'       
    |   '_Thread_local' 
    |   'auto'         
    |   'register'    
    ;

typeSpecifier
    :   'void'     
    |   'char'     
    |   'short'    
    |   'int'   
    |   'long'      
    |   'float'   
    |   'double'    
    |   'signed'   
    |   'unsigned' 
    |   '_Bool'     
    |   '_Complex' 
    |   atomicTypeSpecifier    
    |   structOrUnionSpecifier
    |   enumSpecifier       
    |   typedefName
    ;

structOrUnionSpecifier 
    :   structOrUnion id=Identifier? '{' structDeclarationList '}'
    |   structOrUnion id=Identifier
    ;

structOrUnion
    :   'struct'
    |   'union'
    ;

structDeclarationList
    //:   structDeclaration+
    :   structDeclaration*
    ;

structDeclaration // The first two rules have priority order and cannot be simplified to one expression.
    :   specifierQualifierList structDeclaratorList ';'
    |   specifierQualifierList ';'
    |   staticAssertDeclaration
    ;

specifierQualifierList
    :   (typeSpecifier | typeQualifier)+
    ;

structDeclaratorList
    :   structDeclarator (',' structDeclarator)*
    ;

structDeclarator
    :   declarator 
    ;

enumSpecifier
    :   'enum' Identifier? '{' enumeratorList ','? '}'
    |   'enum' Identifier
    ;

enumeratorList
    :   enumerator (',' enumerator)*
    ;

enumerator
    :   enumerationConstant ('=' constantExpression)?
    ;

enumerationConstant
    :   Identifier
    ;

atomicTypeSpecifier
    :   '_Atomic' '(' typeName ')'
    ;

typeQualifier
    :   'const'
    |   'restrict'
    |   'volatile'
    |   '_Atomic'
    ;

functionSpecifier
    :   'inline'
    |   '_Noreturn'
    ;

alignmentSpecifier
    :   '_Alignas' '(' (typeName | constantExpression) ')'
    ;

declarator
    :   pointer? directDeclarator
    ;

directDeclarator
    :   Identifier         
    |   '(' declarator ')'
    |   directDeclarator '[' typeQualifierList? expression? ']'
    |   directDeclarator '(' parameterTypeList ')'
    ;

pointer
    :  '*' typeQualifierList? pointer?
    ;

typeQualifierList
    :   typeQualifier+
    ;

parameterTypeList
    :   parameterList (',' '...')?
    ;

parameterList
    :   parameterDeclaration (',' parameterDeclaration)*
    ;

parameterDeclaration
    :   declarationSpecifiers declarator 
    |   declarationSpecifiers abstractDeclarator
    |   declarationSpecifiers
    ;

identifierList
    :   Identifier (',' Identifier)*
    ;

typeName
    :   specifierQualifierList abstractDeclarator?
    ;

abstractDeclarator
    :   pointer
    |   pointer? directAbstractDeclarator
    ;

directAbstractDeclarator
    :   '(' abstractDeclarator ')'
    |   '[' typeQualifierList? expression? ']'
    |   '(' parameterTypeList? ')'
    |   directAbstractDeclarator '[' typeQualifierList? expression? ']'
    |   directAbstractDeclarator '(' parameterTypeList? ')'
    ;

typedefName
    :   Identifier
    ;

initializer
    :   expression
    |   '{' initializerList ','? '}' 
    ;

initializerList
    :   (designation? initializer)? (',' designation? initializer)*
    ;

designation
    :   designatorList '='
    ;

designatorList
    :   designator+
    ;

designator
    :   '[' constantExpression ']'
    |   '.' Identifier
    ;

staticAssertDeclaration
    :   '_Static_assert' '(' constantExpression ',' StringLiteral ')' ';'
    ;

statement
    :   compoundStatement
    |   expressionStatement
    |   iterationStatement
    |   jumpStatement
    |   signStatement
    ;

labeledStatement
    :   Identifier ':' statement
    |   'case' constantExpression ':' statement
    |   'default' ':' statement
    ;

compoundStatement
    :   '{' blockItemList? '}'
    ;

blockItemList
    :   (   startOrEndMarker 
        |   blockItem
        )+
    ;

blockItem
    :   statement   
    |   declaration 
    ;

expressionStatement
    :   commaExpression? ';' {makeExprStmt($ctx->commaExpression());}
    ;

selectionStatement
    :   condMarker? 'if' '(' commaExpression ')' statement ('else' statement)?
    ;

iterationStatement
    :   condMarker? 'while' '(' commaExpression ')' statement
    |   condMarker? 'do' statement 'while' '(' commaExpression ')' ';'
    |   condMarker? 'for' '(' forCondition ')' statement
    ;

forCondition
	:   (commaExpression? ';' | declaration) commaExpression? ';' commaExpression?
	;

jumpStatement
    :   (   'continue'               
        |   'break'                  
        |   'return' commaExpression? 
        |   'goto' Identifier
        )
        ';'
    ;

signStatement
    :   '@' commaExpression ';'
    ;

functionDefinition 
    :   declarationSpecifiers declarator compoundStatement
    ;

declarationList
    :   declaration+
    ;

// Markers
StartMarker : '%start';
EndMarker : '%end';
CondMarker : '%cond';
ModeMarker : '%mode';

// Keywords
Auto : 'auto';
Break : 'break';
Case : 'case';
Char : 'char';
Const : 'const';
Continue : 'continue';
Default : 'default';
Do : 'do';
Double : 'double';
Else : 'else';
Enum : 'enum';
Extern : 'extern';
Float : 'float';
For : 'for';
Goto : 'goto';
If : 'if';
Inline : 'inline';
Int : 'int';
Long : 'long';
Register : 'register';
Restrict : 'restrict';
Return : 'return';
Short : 'short';
Signed : 'signed';
Sizeof : 'sizeof';
Static : 'static';
Struct : 'struct';
Switch : 'switch';
Typedef : 'typedef';
Union : 'union';
Unsigned : 'unsigned';
Void : 'void';
Volatile : 'volatile';
While : 'while';

Alignas : '_Alignas';
Alignof : '_Alignof';
Atomic : '_Atomic';
Bool : '_Bool';
Complex : '_Complex';
Generic : '_Generic';
Imaginary : '_Imaginary';
Noreturn : '_Noreturn';
StaticAssert : '_Static_assert';
ThreadLocal : '_Thread_local';

// Separators
LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

// Operators
Less : '<';
LessEqual : '<=';
Greater : '>';
GreaterEqual : '>=';
LeftShift : '<<';
RightShift : '>>';

Plus : '+';
PlusPlus : '++';
Minus : '-';
MinusMinus : '--';
Star : '*';
Div : '/';
Mod : '%';

And : '&';
Or : '|';
AndAnd : '&&';
OrOr : '||';
Caret : '^';
Not : '!';
Tilde : '~';

Question : '?';
Colon : ':';
Semi : ';';
Comma : ',';

Assign : '=';
// '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
StarAssign : '*=';
DivAssign : '/=';
ModAssign : '%=';
PlusAssign : '+=';
MinusAssign : '-=';
LeftShiftAssign : '<<=';
RightShiftAssign : '>>=';
AndAssign : '&=';
XorAssign : '^=';
OrAssign : '|=';

Equal : '==';
NotEqual : '!=';

Arrow : '->';
Dot : '.';
At : '@';
Ellipsis : '...';

// Identifiers
Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

// Literals
IntegerConstant
    :   Digit+
    // :   DecimalConstant IntegerSuffix?
    // |   OctalConstant IntegerSuffix?
    // |   HexadecimalConstant IntegerSuffix?
    // |	BinaryConstant
    ;

FloatingConstant
    :   DecimalFloatingConstant
    |   HexadecimalFloatingConstant
    ;

CharacterConstant
    :   '\'' CCharSequence '\''
    |   'L\'' CCharSequence '\''
    |   'u\'' CCharSequence '\''
    |   'U\'' CCharSequence '\''
    ;

StringLiteral
    :   EncodingPrefix? '"' SCharSequence? '"'
    ;

// Fragment rules
fragment
IdentifierNondigit
    :   Nondigit
    |   UniversalCharacterName
    //|   // other implementation-defined characters...
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

fragment
UniversalCharacterName
    :   '\\u' HexQuad
    |   '\\U' HexQuad HexQuad
    ;

fragment
HexQuad
    :   HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
    ;

fragment
BinaryConstant
	:	'0' [bB] [0-1]+
	;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexadecimalConstant
    :   HexadecimalPrefix HexadecimalDigit+
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;

fragment
IntegerSuffix
    :   UnsignedSuffix LongSuffix?
    |   UnsignedSuffix LongLongSuffix
    |   LongSuffix UnsignedSuffix?
    |   LongLongSuffix UnsignedSuffix?
    ;

fragment
UnsignedSuffix
    :   [uU]
    ;

fragment
LongSuffix
    :   [lL]
    ;

fragment
LongLongSuffix
    :   'll' | 'LL'
    ;

fragment
DecimalFloatingConstant
    :   FractionalConstant ExponentPart? FloatingSuffix?
    |   DigitSequence ExponentPart FloatingSuffix?
    ;

fragment
HexadecimalFloatingConstant
    :   HexadecimalPrefix (HexadecimalFractionalConstant | HexadecimalDigitSequence) BinaryExponentPart FloatingSuffix?
    ;

fragment
FractionalConstant
    :   DigitSequence? '.' DigitSequence
    |   DigitSequence '.'
    ;

fragment
ExponentPart
    :   [eE] Sign? DigitSequence
    ;

fragment
Sign
    :   [+-]
    ;

DigitSequence
    :   Digit+
    ;

fragment
HexadecimalFractionalConstant
    :   HexadecimalDigitSequence? '.' HexadecimalDigitSequence
    |   HexadecimalDigitSequence '.'
    ;

fragment
BinaryExponentPart
    :   [pP] Sign? DigitSequence
    ;

fragment
HexadecimalDigitSequence
    :   HexadecimalDigit+
    ;

fragment
FloatingSuffix
    :   [flFL]
    ;

fragment
CCharSequence
    :   CChar+
    ;

fragment
CChar
    :   ~['\\\r\n]
    |   EscapeSequence
    ;

fragment
EscapeSequence
    :   SimpleEscapeSequence
    |   OctalEscapeSequence
    |   HexadecimalEscapeSequence
    |   UniversalCharacterName
    ;

fragment
SimpleEscapeSequence
    :   '\\' ['"?abfnrtv\\]
    ;

fragment
OctalEscapeSequence
    :   '\\' OctalDigit OctalDigit? OctalDigit?
    ;

fragment
HexadecimalEscapeSequence
    :   '\\x' HexadecimalDigit+
    ;

fragment
EncodingPrefix
    :   'u8'
    |   'u'
    |   'U'
    |   'L'
    ;

fragment
SCharSequence
    :   SChar+
    ;

fragment
SChar
    :   ~["\\\r\n]
    |   EscapeSequence
    |   '\\\n'   // Added line
    |   '\\\r\n' // Added line
    ;

// Whitespace and comments
MultiLineMacro
    :   '#' (~[\n]*? '\\' '\r'? '\n')+ ~ [\n]+ 
        -> channel (HIDDEN)
    ;

Directive
    :   '#' ~ [\n]*
        -> channel (HIDDEN)
    ;

// ignore the following asm blocks:
/*
    asm
    {
        mfspr x, 286;
    }
 */
AsmBlock
    :   'asm' ~'{'* '{' ~'}'* '}' 
        -> channel(HIDDEN)
    ;

Whitespace
    :   [ \t]+ -> channel(HIDDEN)
    ;

Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> channel(HIDDEN)
    ;

BlockComment
    :   '/*' .*? '*/'
        -> channel(HIDDEN)
    ;

LineComment
    :   '//' ~[\r\n]*
        -> channel(HIDDEN)
    ;
