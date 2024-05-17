parser grammar miniCParser;

options {
    tokenVocab = miniCLexer;
}
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
    // |   (type){list} Compound Literal(C99)
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
    // |   expression '?' expression ':' expression
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
    // |   enumerationConstant
    |   IntegerConstant   
    |   FloatingConstant  
    |   CharacterConstant
    |   StringLiteral   
    |   '(' commaExpression ')'
    // |   genericSelection
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
    // |   staticAssertDeclaration 
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
    // |   declarator? ':' constantExpression 
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
    // |   directDeclarator '[' 'static' typeQualifierList? expression ']'
    // |   directDeclarator '[' typeQualifierList 'static' expression ']'
    // |   directDeclarator '[' typeQualifierList? '*' ']'
    |   directDeclarator '(' parameterTypeList ')'
    // |   directDeclarator '(' identifierList? ')'
    // |   Identifier ':' DigitSequence  // bit field
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
    // |   '[' 'static' typeQualifierList? expression ']'
    // |   '[' typeQualifierList 'static' expression ']'
    // |   '[' '*' ']'
    |   '(' parameterTypeList? ')'
    |   directAbstractDeclarator '[' typeQualifierList? expression? ']'
    // |   directAbstractDeclarator '[' 'static' typeQualifierList? expression ']'
    // |   directAbstractDeclarator '[' typeQualifierList 'static' expression ']'
    // |   directAbstractDeclarator '[' '*' ']'
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
    // |   labeledStatement
    |   expressionStatement
    // |   selectionStatement
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
    // |   'switch' '(' commaExpression ')' statement
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
