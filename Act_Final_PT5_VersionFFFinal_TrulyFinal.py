import ply.yacc as yacc
import ply.lex as lex
import sys

tokens = (
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'EQUALS',
    'LPAREN',
    'RPAREN',
    'LCOR',
    'RCOR',
    'ENDOF',
    'COMA', 
    'TWODOT',
    'SMLTHAN',
    'BGRTHAN',
    'MIDDLE',
    'CTE_I',
    'CTE_F',

    #String Tokens
    'ID',
    'PROG', 
    'VAR',
    'INT',
    'FLOAT',
    'PRINT', 
    'CTESTRING',
    'MIENTRAS',
    'SI',
    'SINO',
    'sepVAR',
    )

# Tokens

t_PLUS    = r'\+'
t_MINUS   = r'\-'
t_TIMES   = r'\*'
t_DIVIDE  = r'\/'
t_EQUALS  = r'\='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LCOR    = r'\['
t_RCOR    = r'\]'
t_ENDOF   = r'\;'
t_COMA    = r'\,'  
t_TWODOT  = r'\:'
t_MIDDLE  = r'<>'
t_SMLTHAN = r'\<'
t_BGRTHAN = r'\>'

#Tabla Semantica
#                +     -     *     /
#int   int   | int   int   int   int
#int   float | float float float float
#float int   | float float float float
#float float | float float float float

symbolTable = {}
valueTable = {}
varlist=[]
printme=[]
remember_me = []
cachevarlist=[]

Semantica = {
    1:['int','int','+'],
    2:['int','int','-'],        
    3:['int','int','*'],
    4:['int','int','/'],

    5:['int','float','+'],
    6:['int','float','-'],        
    7:['int','float','*'],
    8:['int','float','/'],

    9:['float','int','+'],
    10:['float','int','-'],        
    11:['float','int','*'],
    12:['float','int','/'],
    
    13:['float','float','+'],
    14:['float','float','-'],        
    15:['float','float','*'],
    16:['float','float','/'],
    }     
P_Direccion = {'int': 100, 'float': 200}
P_Operadores=[]
P_Operandos=[]
P_Tipos=[]
P_Saltos=[]
cuadruplo=[]
ifcont=0
n = 1

#Reserve
reserved = {
    'prog':'PROG',
    'var':'VAR',
    'mientras':'MIENTRAS',
    'si':'SI',
    'sino':'SINO',
    'print':'PRINT',
    'int':'INT',
    'float':'FLOAT',
}
#Check reserve
def isReserve(t):
    reserved_type = reserved.get(t.value, False)
    if reserved_type:
        t.type = reserved_type
        return t  # Return token with reserved type
    return None # Discard non-reserved tokens

### ---Recomended pre-configurations ---
# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

##Complex Tokens ##    
def t_ID(t):
    r'[a-zA-Z]+'
    isReserve(t)
    return t

def t_CTESTRING(t):
    r'\"[a-zA-Z]+\"'
    isReserve(t)
    return t

def t_CTE_F(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_CTE_I(t):
    r'\d+'
    t.value = int(t.value)
    return t

#1
def p_expression_programa(p): #Ajustar
    ''' 
    solve : PROG ID ENDOF vars cuerpo
          | PROG ID ENDOF cuerpo 
    '''
    
#2
def p_expression_vars(p): #SinProbar
    ''' 
    vars : VAR Yvar
    '''
    p[0] = p[2]
    
def p_expression_Yvar(p): #Bien
    ''' 
    Yvar : tipo TWODOT Xvar ENDOF Zvar 
    '''
    for x in varlist:
        #Anadir a pilas
        P_Tipos.append(p[1])
        symbolTable[x] = p[1]
    p[0] = symbolTable
    varlist.clear()


def p_expression_Xvar(p): #Bien
    ''' 
    Xvar : ID COMA Xvar
         | ID 
    '''
    if p[1] in cachevarlist:
        print("WARNING !! variable  " + p[1] + "  already declared!")       
    else:
        varlist.append(p[1]) 
        cachevarlist.append(p[1])  

def p_expression_Zvar(p):
    ''' 
    Zvar : Yvar
         | empty
    '''    
    p[0] = p[1]

#3
def p_expression_type(p):#Bien
    '''  
    tipo : INT
         | FLOAT
    '''
    p[0] = p[1]
    
#4
def p_expression_cuerpo(p): #SinProbar
    ''' 
    cuerpo : LCOR Wcuerpo RCOR
    '''
    p[0] = p[2]
    
def p_expression_Wcuerpo(p):#Bien
    ''' 
    Wcuerpo : estatuto Wcuerpo
            | empty 
    '''
    p[0] = p[1]

#5
def p_expression_estatuto(p):#Bien
    '''  
    estatuto : asigna
             | condicion
             | ciclo
             | escritura
    '''
    p[0] = p[1]
    
#6
def p_expression_asigna(p): #Bien
    '''  
    asigna : ID EQUALS expresion ENDOF
    '''
    P_Operandos.append(p[1])
    try:
        if p[2] == '=' :  
            P_Operadores.append(p[2])

        if(P_Operadores[0] == '='):
            right = P_Operandos.pop()
            left = P_Operandos.pop()
            op = P_Operadores.pop()           
            result = ("Nel")
            cuadruplo.append([op, left, result, right])
                   
    except:
        valueTable[p[1]] = p[3]
    

#7
def p_expression_ciclo(p): #fallando
    '''  
    ciclo : MIENTRAS LPAREN expresion RPAREN seen_B cuerpo ENDOF
    ''' 
    try: 
        global ifcont
        end = P_Saltos.pop()
        comeback = P_Saltos.pop()
        cuadruplo.append(["Goto", comeback, end, len(cuadruplo)]) 
        cuadruplo[ifcont-1][2] = end  
        cuadruplo[ifcont-1][3] = len(cuadruplo)  
        
    except:
        print("error")

def p_seen_B(p):
    "seen_B :"
    try:
        global ifcont
        P_Saltos.append(ifcont)
        result = P_Operandos.pop()
        cuadruplo.append(["GotoF", result, "Nel", "___"])
        ifcont = len(cuadruplo)
        P_Saltos.append(ifcont-1)
    except:
        print("error")    

#8
def p_expression_expresion(p): #Falta asignar bool
    '''  
    expresion  : exp
               | exp SMLTHAN exp
               | exp BGRTHAN exp
               | exp MIDDLE exp
    '''
    try:
        if p[2] == '<' : 
            P_Operadores.append(p[2])
        elif p[2] == '>' :
            P_Operadores.append(p[2])

        if(P_Operadores[0] == '<' or P_Operadores[0] == '>'):
            global n
            right = P_Operandos.pop()
            left = P_Operandos.pop()
            op = P_Operadores.pop()
            result = (f"t{n}")
            cuadruplo.append([op, left, right, result])
            P_Operandos.append(result)      
            n = n + 1
                   
    except:
        p[0] = p[1]

#9
def p_expression_exp(p): #Solo jala var con var o valor con valor
    '''  
    exp : termino
        | termino PLUS exp
        | termino MINUS exp
    '''
    try:
        if p[2] == '+' :  
            P_Operadores.append(p[2])
        elif p[2] == '-' :  
            P_Operadores.append(p[2])

        if(P_Operadores[0] == '+' or P_Operadores[0] == '-'):
            global n
            right = P_Operandos.pop()
            left = P_Operandos.pop()
            op = P_Operadores.pop()
            result = (f"t{n}")
            cuadruplo.append([op, left, right, result])
            P_Operandos.append(result)
            n = n + 1

            #Separated due to instability isues with variables
            try:
                righttype = P_Tipos.pop()
                lefttype = P_Tipos.pop()
                resulttype = [righttype, lefttype, op]
                key = [k for k, v in Semantica.items() if v == resulttype]
                if key[0] in range(0, 17):
                    if(key[0] in range(0, 5)):
                        P_Tipos.append('int')
                    elif(key[0] in range(5, 17)):
                        P_Tipos.append('float')
                    else:
                        print("Invalide tipe")
                else:
                    print("Invalide tipe")
            except:
                print("Type Empty")
    except:
        p[0] = p[1]

#10
def p_expression_condicion(p): #fallando
    '''  
    condicion : SI LPAREN expresion RPAREN seen_A cuerpo ENDOF
              | SI LPAREN expresion RPAREN seen_A cuerpo SINO cuerpo ENDOF
    '''
    try: 
        global ifcont
        end = P_Saltos.pop()
        cuadruplo[ifcont-1][2] = end  
        cuadruplo[ifcont-1][3] = len(cuadruplo)  
        
    except:
        print("error")

def p_seen_A(p):
    "seen_A :"
    try:
        global ifcont
        result = P_Operandos.pop()
        cuadruplo.append(["GotoF", result, "Nel", "___"])
        ifcont = len(cuadruplo)
        P_Saltos.append(ifcont-1)
    except:
        print("error")            

#11
def p_expression_termino(p): #Bien
    '''  
    termino : factor
            | factor TIMES termino
            | factor DIVIDE termino
    '''
    try:
        if p[2] == '*' :  
            P_Operadores.append(p[2])
        elif p[2] == '/' :  
            P_Operadores.append(p[2])

        if(P_Operadores[0] == '*' or P_Operadores[0] == '/'):
            global n
            right = P_Operandos.pop()
            left = P_Operandos.pop()
            op = P_Operadores.pop()
            result = (f"t{n}")
            cuadruplo.append([op, left, right, result])
            P_Operandos.append(result)
            n = n + 1

            #Separated due to instability isues with variables
            try:
                righttype = P_Tipos.pop()
                lefttype = P_Tipos.pop()
                resulttype = [righttype, lefttype, op]
                key = [k for k, v in Semantica.items() if v == resulttype]
                if key[0] in range(0, 17):
                    if(key[0] in range(0, 5)):
                        P_Tipos.append('int')
                    elif(key[0] in range(5, 17)):
                        P_Tipos.append('float')
                    else:
                        print("Invalide tipe")
                else:
                    print("Invalide tipe")
            except:
                print("Type Empty")
    except:
        p[0] = p[1] 

#12
def p_expression_factor(p): #Bien
    '''  
    factor : varcte 
           | LPAREN expresion RPAREN
           | PLUS varcte
           | MINUS varcte
    '''
    if p[1] == '+' :  
        p[0] = p[2]     
    elif p[1] == '-' :  
        p[0] = -p[2]       
    elif p[1] == '(' :  
        p[0] = p[2] 
    else:
        p[0] = p[1]

    
#13
def p_expression_varcte(p): #Bien
    '''  
    varcte : ID
           | CTE_I
           | CTE_F
    '''    
    P_Operandos.append(p[1])
    p[0] = p[1]

#14
def p_expression_escritura(p): 
    '''  
    escritura : PRINT LPAREN Xescritura RPAREN ENDOF
    '''    
    global remember_me
    try:
        pval = []
        for x in printme:
           pval.append(x)

        cuadruplo.append([p[1], pval, "Nel", "Nel"])
        remember_me = pval
        printme.clear()
    except:
        print("error")
        p[0] = p[3]

def p_expression_Xescritura(p): 
    '''  
    Xescritura : varcte_2
               | CTESTRING
               | varcte_2 COMA Xescritura
               | CTESTRING COMA Xescritura
    '''   
    printme.append(p[1])     
    p[0] = p[1]

def p_expression_varcte_2(p): #Bien
    '''  
    varcte_2 : ID
             | CTE_I
             | CTE_F
    '''    
    p[0] = p[1]

#15
def p_empty(p):
    'empty :'
    pass

lexer = lex.lex()
parser = yacc.yacc()

#while True:
try:
    parser.parse(
        '''
        prog test;
        var int: a, b, c, d;
        [   
            a = 3 + (2 + 2) * 5;
            b = a * 3;
            c = a / b;
            
            si (5 > 1) [
                d = 10.50;
            ];        

            print("hola", a, b, c, d, 88);

        ]
        '''
        )
except EOFError:
    print(EOFError)

FValues={}

def existsval(val):
    if val in FValues:
        return FValues[val]
    else:
        return val

for u in cuadruplo:
    try:
        if (u[0] == '+'):
            a = existsval(u[1])
            b = existsval(u[2])
            FValues[u[3]] = (a + b)
        elif (u[0] == '-'):
            a = existsval(u[1])
            b = existsval(u[2])
            FValues[u[3]] = (a - b)
        elif (u[0] == '*'):
            a = existsval(u[1])
            b = existsval(u[2])
            FValues[u[3]] = (a * b)
        elif (u[0] == '/'):
            a = existsval(u[1])
            b = existsval(u[2])
            FValues[u[3]] = (a / b)
        elif (u[0] == '<'):
            a = existsval(u[1])
            b = existsval(u[2])
            FValues[u[3]] = (a < b)
        elif (u[0] == '>'):
            a = existsval(u[1])
            b = existsval(u[2])
            FValues[u[3]] = (a > b)
        elif (u[0] == '='):
            a = existsval( u[1])
            FValues[u[3]] = (a)       
    except:
        FValues[u[3]] = str(u[0])

store_me=[]
for p in remember_me:
    if(p in FValues):
        store_me.append(FValues[p])
    else:
        store_me.append(p)

print("Vars and Types:       ", symbolTable)
print("Remaining Types:      ", P_Tipos)
print("Remaining Operators:  ", P_Operadores)
print("Remaining Operands:   ", P_Operandos)
print("Quadruple: ",'\n' , cuadruplo)
print("You printed:  ", store_me)
