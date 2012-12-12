unit CargadorCobol;

{author. Nolvis Urquiza Elías. nurquiza@grm.desoft.cu ==> Logical}

interface

uses
  Classes, SysUtils,
  UPalabrasRes;

{COBOL Character Set
Character  Meaning
---------  ------------------------
0 to 9	   Digits
A to Z	   Uppercase letters
a to z	   Lowercase letters
           Space
+	         Plus sign
–	         Minus sign or hyphen
*	         Asterisk
/	         Oblique stroke/slash
=	         Equal sign
$	         Dollar sign
.	         Period or decimal point
,	         Comma or decimal point
;	         Semicolon
"	         Quotation mark
'	         Apostrophe
(	         Left parenthesis
)	         Right parenthesis
>	         Greater Than symbol
<	         Less Than symbol
:	         Colon
Lowercase letters can be used in character strings and text words; except when
used in nonnumeric literals and except for some picture symbols, each lowercase
letter is equivalent to the corresponding uppercase letter.
This COBOL implementation is restricted to the above character set, but the
content of nonnumeric literals, comment lines, comment entries and data can
include any of the characters available under the character encoding scheme
used for the COBOL source program.}


type

  //Estructura del programa en memoria
  TCobolPrg = class
    ProgramId: string;
    DecimalPoint: char;

    //begin-prueba
    LVarWorking: TStringList;
    //end-prueba

    constructor Create;
    destructor  Destroy; override;
  end;

  //Cargador del programa
  TTokenId = ( tkNull,
               tkUndefined,
               tkSepSpace,
               tkSepComma,
               //
               tkSepColon,
               //
               tkSepSemiColon,
               tkSepPeriod,
               tkSepLeftPar,
               tkSepRightPar,
               tkSepQuestionMark,
               //tkUserDefinedWord,
               //tkSystemName,
               tkReservedWord,
               //tkFunctionName,
               tkCobolWord,
               tkPictureString,
               tkStringLiteral,
               tkFloatLiteral,
               tkIntegerLiteral,
               tkRelOpGreater,
               tkRelOpLess,
               tkRelOpEqual,
               tkRelOpGreaterOrEqual,
               tkRelOpLessOrEqual,
               tkAritOpMas,
               tkAritOpMenos,
               tkAritOpMul,
               tkAritOpDiv,
               tkAritOpExp,
               //
               tkSQLStatement,
               //
               tkEndPrg,
               //
               tkBlockHeader,//encabezamiento de sección o párrafo
                              //en la procedure division (que puede ser
                              //tambien la palabra clave PARAGRAPH)
                              //ej. 2000-PROCESO
               tkCommentEntry);

  TTokenData = record
    Tipo: TTokenId;
    Valor: string;
    nLine, nCol: integer;
    ValorSQL: TStringList;
    PalRes: TPalResId;
  end;

  TTokenCob = class
    private
      Data: TTokenData;
      Next, Prev: TTokenCob;
      //
      function GetValorPrint: string;
      function FormatToken(valor: string; MaxL: integer): string;
      function GetArea: char;
    public
      constructor Create(var AData: TTokenData);
      destructor  Destroy; override;
      function    EsNumeroEntero: boolean;

      property Tipo:TTokenId read Data.Tipo write Data.Tipo;
      property Valor: string read Data.Valor;
      property ValorPrint: string read GetValorPrint;
      property nLine: integer read Data.nLine;
      property nCol: integer read Data.nCol;
      property Area: char read GetArea;
      property PalRes: TPalResId read Data.PalRes;
  end;

  PResult = ^TResult;
  TResult = record
    fname: string;
    nLine,nCol: integer;
  end;

  TListToken = class
    private
      First,Last,Curr: TTokenCob;
      FDecimalPoint: char;
    public
      procedure Add(AToken: TTokenCob);
      constructor Create;
      destructor Destroy;override;
      //
      procedure GoFirst;
      procedure GoLast;
      procedure GoNext;
      procedure GoPrev;
      procedure FindNext_ResWord(AToken: TTokenCob; sword: string);
      function  InDivision(divName: string): boolean;
      function  InSection(secName: string): boolean;
      function  InEnd: boolean;
      //
      property  DecimalPoint: char read FDecimalPoint write FDecimalPoint;
    private
      procedure CargarIdentificationDiv(CobolPrg: TCobolPrg);
      procedure CargarDataDivision(CobolPrg: TCobolPrg);
      procedure CargarVariables(CobolPrg: TCobolPrg);
      procedure CargarProcDivision(CobolPrg: TCobolPrg);
      //
      procedure PrepararFormato(Tipo: TTokenId; var sInit, sFin: string);
    public
      procedure Clear();
      procedure MostrarTokens(List: TStrings);
      procedure CargarPrg(CobolPrg: TCobolPrg);
      procedure BuscarMOVE(AList: TList);
      procedure PonerColores(List: TStringList);
  end;

  TLexerHack = (lhNormal,
                lhPictureStr,
                lhSQLStatement);

  TIdDivision = (DivNull, DivData, DivProcedure);
  TLexerResult = (lrLexerOK, lrFicheroIncorrecto, lrTokenIndefinido);

  TCobolLexer = class
    private
      sline: string;
      nLine, posL: integer;
      //
      F: TextFile;
      fFileName: string;
      //
      Token, TokenStack: TTokenData;
      DecimalPoint: char;
      Division: TIdDivision;
      //PalAnt: TPalResId;
      ListPrg: TStringList;
      //
      UsarPalRes: boolean;
    private
      function  NextLine: boolean;
      procedure IgnorarEspaciosLinea;
      procedure IgnorarSepLinea;
      function  SgteLineaNoVacia: boolean;
      function  CurrentChar: string;
      function  LeerCobolWord: string;
      function  LeerStringLiteral: string;
      function  LeerPictureStr: string;
      function  LeerNumero_Auto(sInicio: string; EstadoIni: integer; var Token: TTokenId): string;
      procedure GuardarToken(ATipo: TTokenId; AValor: string; ALine,ACol: integer);
      function  GetArea(nCol: integer): char;
      procedure ClearToken();
      procedure NextToken(LexerHack: TLexerHack; IgnoreSepSpace: boolean);
      function  BuscarPalRes(sword: string): integer;
      function  IsInfo(s: string): boolean;
      procedure InitFile;
    public
      constructor Create(UsarPalRes: boolean);
      function    CargarTokens(fname: string; List: TListToken; ListPrg: TStringList): TLexerResult;
  end;


implementation

uses StrUtils;

{ TCargadorCobol }

function TCobolLexer.NextLine: boolean;
{1..6   = sequence number area
 7      = indicator area
          '*' : comentario
          '/' : comentario
          'D' : debugging line
          '-' : continuation of the previous line without spaces or the
                continuation of a nonnumeric literal
 8..11  = area A
 12..72 = area B
 73..   = area R}
begin
  Result := false;
  while not Eof(F) and not Result do
  begin
    Readln(F,sline);
    //cargar la linea
    if ListPrg <> nil then ListPrg.Add(sline);
    //
    Inc(nLine);
    //ignorar:
    // - si la longitud de la linea es <= 7 la considero
    //   Blank Line
    // - si en la posicion 7 hay un '*' o '/' la linea es
    //   un comentario
    if (Length(sline) > 7) and not (sline[7] in ['*','/']) then
    begin
      //ignorar area R
      if Length(sline) > 72 then sline := Copy(sline,1,72);
      //si la linea esta en blanco de 8..72 la considero Blank Line
      sline := TrimRight(sline);
      if Length(sline) > 7 then
      begin                         
        Result := true;
        PosL := 8;
      end;          
    end;
  end;
  if not Result then
  begin
    sline  := ''; PosL := 1;
  end;
end;

function TCobolLexer.SgteLineaNoVacia: boolean;
begin
  //busca siguiente linea no vacia
  //ignora lineas de continuación vacías
  Result := true;
  repeat
    if not NextLine() then Result := false//fin de archivo
    else
    begin
      PosL := 8;
      if sline[7] = '-' then IgnorarEspaciosLinea;
    end;
  until not Result or (PosL <= Length(sline));
end;

function TCobolLexer.CurrentChar: string;
begin
  if PosL <= Length(sline) then Result := sline[PosL]
  else
  if SgteLineaNoVacia() and (sline[7] = '-') then
    Result := sline[PosL]
  else Result := '';
end;

procedure TCobolLexer.IgnorarEspaciosLinea;
begin
  while (PosL <= Length(sline)) and
        (sline[PosL] = ' ') do Inc(PosL);
end;


procedure TCobolLexer.IgnorarSepLinea;
begin
  while (PosL <= Length(sline)) and
        (sline[PosL] in [' ', ',', ';']) do Inc(PosL);
end;

procedure TCobolLexer.NextToken;
var
  sword: string;
  Continuar: boolean;
  p: integer;

procedure NextTokenDefault;
var
  s,schar: string;
  ATokenId: TTokenId;
begin
  Token.nLine := nLine;
  Token.nCol := PosL;
  //
  case sline[posL] of
    {separadores}
    //separador <space>
    ' ':
    begin
      Token.Tipo := tkSepSpace;
      Inc(PosL);
      IgnorarEspaciosLinea;
    end;

    //separador <colon>
    ':':
    begin
      Inc(PosL);
      Token.Tipo := tkSepColon;
    end;

    //separador <comma>
    //el ',' deberia estar seguido por espacios
    //pero el microfocus se lo traga sin espacios
    ',':
    begin
      Inc(PosL);
      schar := CurrentChar();
      if (DecimalPoint = ',') and
         (schar <> '') and (schar[1] in ['0'..'9','E','e']) then
      begin
        Token.Valor := LeerNumero_Auto(',',1,ATokenId);
        Token.Tipo := ATokenId;
      end
      else
      begin
        Token.Tipo := tkSepComma;
        IgnorarEspaciosLinea;
      end;
    end;

    //separador <semicolon>
    //el ';' deberia estar seguido por espacios
    //pero el microfocus se lo traga sin espacios
    ';':
    begin
      Inc(PosL);
      Token.Tipo := tkSepSemiColon;
      IgnorarEspaciosLinea;
    end;

    //separador <period>
    //el '.' deberia estar seguido por espacios
    //pero el microfocus se lo traga sin espacios
    '.':
    begin
      Inc(PosL);
      schar := CurrentChar();
      if (DecimalPoint = '.') and
         (Length(schar) > 0) and (schar[1] in ['0'..'9','E','e']) then
      begin
        Token.Valor := LeerNumero_Auto('.',1,ATokenId);
        Token.Tipo := ATokenId;
      end
      else
      begin
        Token.Tipo := tkSepPeriod;
        IgnorarEspaciosLinea;
      end;
    end;

    //separador <left-paren>
    '(':
    begin
      Inc(PosL);
      Token.Tipo := tkSepLeftPar;
    end;

    //separador <right-paren>
    ')':
    begin
      Inc(PosL);
      Token.Tipo := tkSepRightPar;
    end;

    //OJO: TODO: Pseudo-text delimiters

    //separador Question Mark
    '?':
    begin
      Inc(PosL);
      Token.Tipo := tkSepQuestionMark;
    end;

    {operadores aritméticos (subconjunto de reserved-words)}
    //mas
    '+':
    begin
      Inc(PosL);
      schar := CurrentChar();
      if (Length(schar) > 0) and (schar[1] in ['0'..'9',DecimalPoint]) then
      begin
        Token.Valor := LeerNumero_Auto('+',0,ATokenId);
        Token.Tipo := ATokenId;
      end
      else Token.Tipo := tkAritOpMas;
    end;

    //menos
    '-':
    begin
      Inc(PosL);
      schar := CurrentChar();
      if (Length(schar) > 0) and (schar[1] in ['0'..'9',DecimalPoint]) then
      begin
        Token.Valor := LeerNumero_Auto('-',0,ATokenId);
        Token.Tipo := ATokenId;
      end
      else Token.Tipo := tkAritOpMenos;
    end;

    //mul
    '*':
    begin
      Inc(PosL);
      schar := CurrentChar();
      if (Length(schar) > 0) and (schar[1] = '*') then
      begin
        Inc(PosL);
        Token.Tipo := tkAritOpExp;//'**'
      end
      else Token.Tipo := tkAritOpMul;//'*'
    end;

    //div
    '/':
    begin
      Inc(PosL);
      Token.Tipo := tkAritOpDiv;
    end;

    {relational operators (subconjunto de reserved-words)}
    //equal
    '=':
    begin
      Inc(PosL);
      Token.Tipo := tkRelOpEqual;
    end;

    //less, less-or-equal
    '<':
    begin
      Inc(PosL);
      schar := CurrentChar();
      if (Length(schar) > 0) and (schar[1] = '=') then
      begin
        Inc(PosL);
        Token.Tipo := tkRelOpLessOrEqual;//'<='
      end
      else Token.Tipo := tkRelOpLess;//'<'
    end;

    //greater, greater-or-equal
    '>':
    begin
      Inc(PosL);
      schar := CurrentChar();
      if (Length(schar) > 0) and (schar[1] = '=') then
      begin
        Inc(PosL);
        Token.Tipo := tkRelOpGreaterOrEqual;//'>='
      end
      else Token.Tipo := tkRelOpGreater;//'>'
    end;

    {cadena de caracteres}
    '''','"':
    begin
      Token.Tipo := tkStringLiteral;
      Token.nLine := nLine;
      Token.Valor := LeerStringLiteral();
    end;

    {<cobol-word>, <integer-literal>, <float-literal>}
    //si comienza con <digit>, se deben leer todos los digitos
    //consecutivos.
    // - Si el sgte char es <letra> o '-' entonces es <cobol-word>
    // - Si el sgte char es el Punto Decimal entonces es <float-literal>
    // - En otro caso (fin de palabra): valorar
    '0'..'9':
    begin
      //leer todos los digitos consecutivos
      s := '';
      schar := sline[posL];
      repeat
        s := s + schar;                        
        Inc(PosL);
        schar := CurrentChar();
      until (Length(schar) = 0) or not (schar[1] in ['0'..'9']);
      //ver si es numero o <cobol-word>
      if Length(schar) = 0 then
      begin
        Token.Tipo := tkCobolWord;
        if (Division = DivProcedure) then
        begin
          if GetArea(Token.nCol) = 'A' then Token.Tipo := tkBlockHeader
          else if GetArea(Token.nCol) = 'B' then Token.Tipo := tkIntegerLiteral;
          //nota: TODO: en un PERFORM digitos: digitos saldra como <integer-literal>
        end;
        Token.Valor := s;
      end
      else
      if schar[1] in ['A'..'Z', 'a'..'z', '-'] then
      begin
        //continuar leyendo <cobol-word> ==> nombre de parrafo
        s := s + LeerCobolWord;
        if (Division = DivProcedure) and (GetArea(Token.nCol) = 'A') then
          Token.Tipo := tkBlockHeader
        else Token.Tipo := tkCobolWord;
        Token.Valor := s;
      end
      else
      if schar[1] = DecimalPoint then
      begin
        //leer numero
        Token.Valor := LeerNumero_Auto(s,6,ATokenId);
        Token.Tipo := ATokenId;
      end
      else
      begin
        Token.Tipo := tkCobolWord;
        if (Division = DivProcedure) then
        begin
          if GetArea(Token.nCol) = 'A' then Token.Tipo := tkBlockHeader
          else if (GetArea(Token.nCol) = 'B') then Token.Tipo := tkIntegerLiteral;
          //nota: en un PERFORM digitos: digitos saldra como <integer-literal>
        end;
        Token.Valor := s;
      end;
    end;

    'A'..'Z', 'a'..'z':
    begin
      //leer cobol-words
      Token.nLine := nLine;
      s := LeerCobolWord();
      if (Division = DivProcedure) and (GetArea(Token.nCol) = 'A') then
        Token.Tipo := tkBlockHeader
      else
      if not UsarPalRes then Token.Tipo := tkCobolWord
      else
      begin
        p := BuscarPalRes(S);
        if p <> -1 then
        begin
          Token.Tipo := tkReservedWord;
          Token.PalRes := TPalResId(p);
        end
        else Token.Tipo := tkCobolWord;
      end;
      Token.Valor := s;
      {estas palabras claves pueden ser : reserved-word o function-name,
      dependiendo del contexto: 'LENGTH','RANDOM','SUM'}
    end;

    else
    begin
      {token desconocido}
      //leer todo hasta espacio o fin de linea
      Token.nLine := nLine;
      while (PosL <= Length(sline)) and (sline[PosL] <> ' ') do
      begin
        s := s + sline[PosL];
        Inc(PosL);
      end;
      Token.Tipo := tkUndefined;
      Token.Valor := s;
    end;
  end;//case
end;

begin
  ClearToken();
  //
  if TokenStack.Tipo <> tkNull then
  begin
    Token := TokenStack;
    TokenStack.Tipo := tkNull;
    Exit;
  end;
  //
  if PosL > Length(sline) then
    if not NextLine() then
    begin
      Token.Tipo := tkEndPrg;
      Exit;
    end;
  //validar Lexer-Hack
  case LexerHack of
    lhPictureStr:
    begin
      IgnorarEspaciosLinea();
      Token.Tipo := tkPictureString;
      if (PosL > Length(sline)) then Continuar := SgteLineaNoVacia()
      else Continuar := true;
      if Continuar then
      begin
        Token.nLine := nLine;
        IgnorarEspaciosLinea();
        Token.nCol := PosL;
        if sline[PosL] = 'I' then //IS opt
        begin
          sword := LeerCobolWord();
          Token.Tipo := tkCobolWord;
          Token.Valor := sword;
        end
        else
        begin
          Token.Tipo := tkPictureString;
          Token.Valor := LeerPictureStr();
        end;
      end;
    end;

    lhSQLStatement:
    begin
      //OJO: TODO:validar el caso que haya una variable
      //con nombre ':END-EXEC', en cuyo caso habrá que buscar
      //otro END-EXEC==>caso extremo
      
      IgnorarEspaciosLinea();
      Token.ValorSQL := TStringList.Create();
      Token.nLine := nLine;
      Token.nCol := PosL;
      //
      Continuar := true;
      if PosL > Length(sline) then Continuar := SgteLineaNoVacia();
      if Continuar then
      begin
        p := Pos('END-EXEC', sline);
        while (p = 0) and Continuar do
        begin
          Token.ValorSQL.Add(Copy(sline, PosL, Length(sline)-PosL+1));
          Continuar := SgteLineaNoVacia();
          if Continuar then
          begin
            p := Pos('END-EXEC', sline);
            PosL := 1;
          end
        end;
        if Continuar then
        begin
          if (Trim(Copy(sline,PosL,p - PosL)) <> '') then
            Token.ValorSQL.Add(Copy(sline,PosL,p - PosL));
          Token.Tipo := tkSQLStatement;
          PosL := p;
        end; 
      end;
    end

    else
    //Comportamiento por defecto de NextToken
    if IgnoreSepSpace then
    begin
      repeat
        ClearToken();
        //validar fin de linea
        if PosL > Length(sline) then
          if NextLine() then
          begin
            PosL := 8;
            NextTokenDefault();
          end
          else Token.Tipo := tkEndPrg
        else NextTokenDefault();
      until not (Token.Tipo in [tkSepSpace, tkSepComma, tkSepSemiColon])
    end
    else NextTokenDefault();
  end;
  //PARCHE: Para evitar explote con sentencias
  //como esta: 3120-FIN.EXIT.
  //El '.E' se toma como inicio de numero y explota
  //ver programa: BG3CNDC0.CBL
  if (Token.Tipo = tkCobolWord) and
     (Token.nCol >= 8) and (Token.nCol <= 11) and
      (DecimalPoint = '.') and
      (posL <= Length(sLine)) and (UpperCase(Copy(sLine,PosL,2)) = '.E') then
  begin
    GuardarToken(tkSepPeriod, '', nLine, PosL);
    Inc(PosL);
  end;
end;

function TCobolLexer.LeerCobolWord: string;
var
  s: string;
begin
  Result := UpCase(sline[PosL]);
  Inc(PosL);
  s := CurrentChar();
  while (Length(s) > 0) and (s[1] in ['A'..'Z', 'a'..'z', '0'..'9','-']) do
  begin
    Result := Result + UpCase(s[1]);
    Inc(PosL);
    s := CurrentChar();
  end;
end;

function TCobolLexer.LeerStringLiteral;
var
  Delim: char;
  FinCadena: boolean;
  cntD: integer;

function ContinCadena: boolean;
begin
  Result := false;
  posL := 7;
  if (Length(sline) >= 7) and (sline[7] = '-') then
  begin
    //la cadena debe continuar en el area B(12..72) pero como
    //el prg esta bien voy a ignorar espacios desde la posicion 8
    posL := 8;
    IgnorarEspaciosLinea();
    if posL > Length(sline) then Result := true
    else if sline[posL] = Delim then
    begin
      Result := true;
      Inc(posL);
    end
  end;
end;

begin
  Result := '';
  Delim := sline[posL]; Inc(PosL);
  FinCadena := false;
  cntD := 0;
  while not FinCadena do
  begin
    if (posL > Length(sline)) then
    begin
      NextLine();
      if not ContinCadena() then FinCadena := true;
    end
    else
    begin
      if sline[posL] = Delim then
      begin
        Inc(cntD);
        if cntD = 2 then
        begin
          Result := Result + Delim;
          cntD := 0;
        end;
        Inc(PosL);
      end
      else
      begin
        if cntD > 0 then FinCadena := true
        else
        begin
          Result := Result + sline[posL];
          Inc(posL);
        end
      end
    end
  end;
end;

function TCobolLexer.LeerPictureStr;
var
  FinCadena: boolean;
  LineComma, ColComma: integer;
  LinePeriod, ColPeriod: integer;
begin
  //-notar que a sline se le hace un TrimRight en NextLine()
  //-el pic-string terminará al encontrar:
  // ' '  ó  ', '  ó  '; '  ó  ';'
  LineComma := 0; ColComma := 0;
  LinePeriod := 0; ColPeriod := 0;
  Result := '';
  FinCadena := false;
  while not FinCadena do
  begin
    if PosL <= Length(sline) then
    begin
      if sline[PosL] = ' ' then
        if Result[Length(Result)] = ',' then//', '
        begin
          GuardarToken(tkSepComma, '', nLine,PosL - 1);
          Result := Copy(Result,1,Length(Result)-1);
          IgnorarEspaciosLinea();
          FinCadena := true;
        end
        else FinCadena := true
      else
      if sline[PosL] = ';' then FinCadena := true
      else
      begin
        Result := Result + UpCase(sline[PosL]);
        if sline[PosL] = ',' then
        begin
          LineComma := nline;
          ColComma := posL;
        end
        else
        if sline[PosL] = '.' then
        begin
          LinePeriod := nline;
          ColPeriod := posL;
        end;
        Inc(PosL);
      end
    end
    else
    begin
      if SgteLineaNoVacia() then
      begin
        PosL := 8;
        if sline[7] = '-' then
        begin
          PosL := 12;//picture-string debe continuar en area B
          IgnorarEspaciosLinea();
        end
        else
        begin
          if Result[Length(Result)] = ',' then
          //',<end-of-line' y la sgte linea no es de continuacion
          begin
            GuardarToken(tkSepComma, '', LineComma, ColComma);
            Result := Copy(Result,1,Length(Result)-1);
          end;
          FinCadena := true;
        end;
      end
      else FinCadena := true;
    end
  end;
  //- si temina en '.', ese '.' se considera un <period-sep>
  //- si termina en varios '..' o más, el último es un <period-sep> y el resto es parte del pic-string
  if Result[Length(Result)] = '.' then
  begin
    GuardarToken(tkSepPeriod, '', LinePeriod, ColPeriod);
    Result := Copy(Result,1,Length(Result)-1);
  end;
end;

function TCobolLexer.LeerNumero_Auto(sInicio: string; EstadoIni: integer;
  var Token: TTokenId): string;
var
  Estado,ALine,ACol: integer;
  schar: string;

procedure AceptarCaracter(NEstado: integer);
begin
  Result := Result + schar;
  Inc(PosL);
  Estado := NEstado;
end;

begin
  Token := tkIntegerLiteral;
  ALine := 0;ACol := 0;//para evitar warning
  Result := sInicio;
  Estado := EstadoIni;
  while Estado <> -1 do
  begin
    schar := CurrentChar();
    case Estado of
      0:
        if schar[1] in ['+','-'] then AceptarCaracter(0)
        else if schar[1] = DecimalPoint then AceptarCaracter(1)
        else if schar[1] in ['0'..'9'] then AceptarCaracter(6)
        else
        begin
          //ERROR
          Estado := -1;
          Token := tkUndefined;
        end;
      1:
        if schar[1] in ['0'..'9'] then AceptarCaracter(2)
        else if schar[1] in ['E','e'] then AceptarCaracter(3)
        else
        begin
          //ERROR
          Estado := -1;
          Token := tkUndefined;
        end;
      2:
        if (Length(schar) > 0) and (schar[1] in ['0'..'9']) then AceptarCaracter(2)
        else if (Length(schar) > 0) and (schar[1] in ['E','e']) then AceptarCaracter(3)
        else
        begin
          Estado := -1;//fin
          Token := tkFloatLiteral;
        end;
      3:
        if schar[1] in ['+','-'] then AceptarCaracter(4)
        else if schar[1] in ['0'..'9'] then AceptarCaracter(5)
        else
        begin
          //ERROR
          Estado := -1;
          Token := tkUndefined;
        end;
      4:
        if (Length(schar) > 0) and (schar[1] in ['0'..'9']) then AceptarCaracter(5)
        else
        begin
          Estado := -1;//fin
          Token := tkFloatLiteral;
        end;
      5:
        if (Length(schar) > 0) and (schar[1] in ['0'..'9']) then AceptarCaracter(5)
        else
        begin
          Estado := -1;//fin
          Token := tkFloatLiteral;
        end;
      6:
        if (Length(schar) > 0) and (schar[1] in ['0'..'9']) then AceptarCaracter(6)
        else if (Length(schar) > 0) and (schar[1] = DecimalPoint) then
        begin
          ALine := nline;
          ACol := PosL;
          AceptarCaracter(7);                           
        end
        else
        begin
          Estado := -1;//fin
          Token := tkIntegerLiteral;
        end;
      7:
        if (Length(schar) > 0) and (schar[1] in ['0'..'9']) then AceptarCaracter(2)
        else if (Length(schar) > 0) and (schar[1] in ['E','e']) then AceptarCaracter(3)
        else
        begin
          Estado := -1;//fin
          Token := tkIntegerLiteral;
          //quitar el separador decimal
          Result := Copy(Result,1,Length(Result)-1);
          //
          if DecimalPoint = '.' then
            GuardarToken(tkSepPeriod, '', ALine, ACol)
          else GuardarToken(tkSepComma, '', ALine, ACol);
        end;            
    end;
  end;
end;

procedure TCobolLexer.InitFile;
begin
  nLine := 0;
  PosL := 1;
  sline := '';
  //
  ClearToken();
  TokenStack.Tipo := tkNull;;
  //
  DecimalPoint := '.';
  Division := DivNull;
  //
  ListPrg := nil;
end;

{ TokenCob }

constructor TTokenCob.Create(var AData: TTokenData);
begin
  inherited Create();
  Data := AData;
end;

destructor TTokenCob.Destroy;
begin
  Data.ValorSQL.Free;
  inherited;
end;

function TTokenCob.EsNumeroEntero: boolean;
var
  i: integer;
begin
  if Data.Tipo = tkIntegerLiteral then Result := true
  else if Data.Tipo = tkCobolWord then
  begin
    i := 1;
    while (i <= Length(Data.Valor)) and (Data.Valor[i] in ['0'..'9']) do Inc(i);
    Result := i > Length(Data.Valor);
  end
  else Result := false;
end;

function TTokenCob.FormatToken(valor: string; MaxL: integer): string;
var
  i: integer;
begin
  SetLength(Result,MaxL);
  for i := 1 to MaxL do
    if i <= Length(valor) then Result[i] := valor[i]
    else Result[i] := ' ';
end;

function TTokenCob.GetArea: char;
begin
  if (Data.nCol >= 8) and (Data.nCol <= 11) then Result := 'A'
  else if (Data.nCol >= 12) and (Data.nCol <= 72) then Result := 'B'
  else Result := 'X';
end;

function TTokenCob.GetValorPrint: string;
begin
  case Data.Tipo of
    tkSepSpace:
      Result := FormatToken(' ',40)+'<space>';
    tkSepComma:
      Result := FormatToken(',',40)+'<sep comma>';
    tkSepColon:
      Result := FormatToken(':',40)+'<sep colon>';
    tkSepSemiColon:
      Result := FormatToken(';',40)+'<sep semicolon>';
    tkSepPeriod:
      Result := FormatToken('.',40)+'<sep period>';
    tkSepLeftPar:
      Result := FormatToken('(',40)+'<sep par-izq>';
    tkSepRightPar:
      Result := FormatToken(')',40)+'<sep par-der>';
    tkSepQuestionMark:
      Result := FormatToken('?',40)+'<sep question-mark>';
    tkReservedWord:
      Result := FormatToken(Data.Valor,40)+'<reserved-word>';
    tkCobolWord:
      Result := FormatToken(Data.Valor,40)+'<cobol-word>';
    tkStringLiteral:
      Result := FormatToken('"'+Data.Valor +'"',40)+'<string-literal>';
    tkFloatLiteral:
      Result := FormatToken(Data.Valor,40)+'<float-literal>';
    tkIntegerLiteral:
      Result := FormatToken(Data.Valor,40)+'<integer-literal>';
    tkPictureString:
      Result := FormatToken(Data.Valor,40)+'<picture-string>';
    //
    tkUndefined:
      Result := FormatToken(Data.Valor,40)+'<UNDEFINED>';
    //
    tkAritOpMas:
      Result := FormatToken('+',40)+'<arit-op-mas>';
    tkAritOpMenos:
      Result := FormatToken('-',40)+'<arit-op-menos>';
    tkAritOpMul:
      Result := FormatToken('*',40)+'<arit-op-mul>';
    tkAritOpDiv:
      Result := FormatToken('/',40)+'<arit-op-div>';
    tkAritOpExp:
      Result := FormatToken('**',40)+'<arit-op-exp>';
    //
    tkRelOpGreater:
      Result := FormatToken('>',40)+'<rel-op-greater>';
    tkRelOpLess:
      Result := FormatToken('<',40)+'<rel-op-less>';
    tkRelOpEqual:
      Result := FormatToken('=',40)+'<rel-op-equal>';
    tkRelOpGreaterOrEqual:
      Result := FormatToken('>=',40)+'<rel-op-greater-or-equal>';
    tkRelOpLessOrEqual:
      Result := FormatToken('<=',40)+'<rel-op-less-or-equal>';
    //
    tkSQLStatement:
      Result := FormatToken(Data.ValorSQL[0],40)+'<sql-statement>';
    //
    tkCommentEntry:
      Result := FormatToken(Data.Valor,40)+'<comment-entry>';
    tkBlockHeader:
      Result := FormatToken(Data.Valor,40)+'<block-header>';
    //
    tkEndPrg:
      Result := FormatToken(' ',40)+'<end-prg>';
  end;
  Result := FormatToken(Result,70) + '('+IntToStr(Data.nLine)+':'+IntToStr(Data.nCol)+')';
end;

procedure TCobolLexer.GuardarToken(ATipo: TTokenId; AValor: string; ALine,ACol: integer);
begin
  with TokenStack do
  begin
    Tipo := ATipo;
    Valor := AValor;
    nLine := ALine;
    nCol := ACol;
  end;
end;

function TCobolLexer.GetArea(nCol: integer): char;
begin
  if (nCol >= 8) and (nCol <= 11) then Result := 'A'
  else if (nCol >= 12) and (nCol <= 72) then Result := 'B'
  else Result := 'X';
end;

function TCobolLexer.CargarTokens(fname: string; List: TListToken;
  ListPrg: TStringList): TLexerResult;
var
  InfoHead, IsComent, EndPrg: boolean;
  i: integer;
  s: string;

procedure CrearToken(ATipo: TTokenId; AValor: string; ALine, ACol: integer);
begin
  Token.Tipo := ATipo;
  Token.Valor := AValor;
  Token.nLine := ALine;
  Token.nCol := ACol;
end;

procedure ProcesarToken;
begin
  if Token.Tipo = tkUndefined then Result := lrTokenIndefinido;
  if not (Token.Tipo in [tkSepSpace, tkSepComma, tkSepSemiColon, tkEndPrg]) then
  begin
    //en PERFORM XXX, identificar a XXX como tkBlockHedaer
    //en THRU YYY, identificar a YYY como tkBlockHedaer
    if Token.Tipo = tkCobolWord then
    begin
      if (List.Last <> nil) and (List.Last.Tipo = tkReservedWord) and
         ((List.Last.PalRes = prPERFORM) or (List.Last.PalRes = prTHRU)) then
        Token.Tipo := tkBlockHeader;
    end;
    //
    List.Add(TTokenCob.Create(Token));
  end;
end;

procedure BuscarInicio;
begin
  while (PosL <= Length(sline)) and
        (sline[PosL] in [' ', ',', ';']) do Inc(PosL);
end;

function FicheroValido: boolean;
var
  s,schar: string;
begin
  if NextLine() then
  begin
    IgnorarEspaciosLinea();//falta validar el caso de que haya algun ',' 0 ';'
    schar := CurrentChar();
    while (Length(schar) > 0) and (schar[1] in ['A'..'Z', 'a'..'z']) and
          (Length(s) < 14) do
    begin
      s := s + UpCase(schar[1]);
      Inc(PosL);
      schar := CurrentChar();
    end;
    if (s = 'IDENTIFICATION') or
       (Copy(s,1,2) = 'ID') and ((Length(s) = 2) or (s[3] = ' ')) then Result := true
    else Result := false;
  end
  else Result := false;
end;                         

begin
  fFileName := fname;
  AssignFile(F,fFileName);
  Reset(F);
  //
  List.Clear;
  List.DecimalPoint := '.';
  //
  if FicheroValido() then
  begin
    Reset(F);
    InitFile();
    Result := lrLexerOK;
  end
  else
  begin
    Result := lrFicheroIncorrecto;
    Exit;
  end;
  Result := lrLexerOK;
  if ListPrg <> nil then Self.ListPrg := ListPrg;
  //
  try
    {procesar identification division}
    for i := 1 to 4 do
    begin
      NextToken(lhNormal,true);
      ProcesarToken();//IDENTIFICATION,DIVISION,'.',PROGRAM-ID
    end;
    //
    NextToken(lhNormal,true);
    if Token.Tipo = tkSepPeriod then
    begin
      ProcesarToken();//'.'(opcional)
      NextToken(lhNormal,true);
    end;
    ProcesarToken();//nombre de programa

    NextToken(lhNormal,true);
    if Token.Tipo = tkSepPeriod then
    begin
      ProcesarToken();//'.'(opcional)
      NextToken(lhNormal,true);
    end;

    //párrafos: AUTHOR,INSTALLATION,DATE-WRITTEN,DATE-COMPILED,SECURITY
    InfoHead := false;
    if Token.Tipo = tkEndPrg then EndPrg := true
    else
    begin
      EndPrg := false;
      InfoHead := IsInfo(Token.Valor);
    end;
    while not EndPrg and InfoHead do
    begin
      ProcesarToken();//nombre párrafo: AUTHOR,INSTALLATION,...
      Token.Tipo := tkCommentEntry;
      //nota: tomare comment-entry desde la columna siguiente al párrafo
      Inc(Token.nCol, Length(Token.Valor));
      if Token.nLine = nLine then
      begin
        Token.Valor :=
          Trim(Copy(sline,Token.nCol,Length(sline)-Token.nCol+1));
        EndPrg := not NextLine();
      end
      else Token.Valor := '';
      //
      InfoHead := false;
      IsComent := true;
      while not EndPrg and IsComent do
      begin
        IgnorarSepLinea();
        i := PosL;
        while (i <= Length(sline)) and
              (sline[i] in ['A'..'Z','a'..'z','-']) do Inc(i);
        if (PosL < i) and (PosL >= 8) and (PosL <= 11) then
        begin
          s := Copy(sline, PosL, i - PosL);
          if IsInfo(s) then
          begin
            InfoHead := true;
            IsComent := false;
            if Token.Valor <> '' then ProcesarToken();//comment entry
            CrearToken(tkReservedWord, UpperCase(s), nLine, PosL);
          end
          else if (s = 'ENVIRONMENT') or (s = 'DATA') or (s = 'PROCEDURE') then
                 IsComent := false;
        end;
        if IsComent then
        begin
          Token.Valor := Token.Valor + '|' +
            Trim(Copy(sline, PosL, Length(sline)-PosL+1));
          EndPrg := not NextLine();
        end;
      end;//while
    end;//while
    if (Token.Tipo <> tkCommentEntry) or (Token.Valor <> '') then
      ProcesarToken();//si hubo InfoHead: comment entry
                      //si no hubo InfoHead: prENVIRONMENT, prDATA, prPROCEDURE
    if EndPrg then Token.Tipo := tkEndPrg;

    {procesar el resto del programa}
    if Token.Tipo <> tkEndPrg then
      NextToken(lhNormal,true);//ENVIRONMENT DIV, DATA DIV, PROCEDURE DIV
    while Token.Tipo <> tkEndPrg do   
    begin
      ProcesarToken();
      if (GetArea(Token.nCol) = 'A') and (Token.Valor = 'DATA') then
      begin
        Division := DivData;
        NextToken(lhNormal,true);
      end
      else
      if (GetArea(Token.nCol) = 'A') and (Token.Valor = 'PROCEDURE') then
      begin
        Division := DivProcedure;
        NextToken(lhNormal,true);
      end
      else
      {validar los lexer-hack}
      //ver el decimal-point
      if Token.Valor = 'DECIMAL-POINT' then
      begin
        NextToken(lhNormal,true);//IS opt            
        if Token.Valor = 'IS' then
        begin
          ProcesarToken();
          NextToken(lhNormal,true);
        end;
        if Token.Valor = 'COMMA' then
           begin
             DecimalPoint := ',';
             List.DecimalPoint := Self.DecimalPoint;
           end;
      end
      else
      //lexer-hack: picture-string
      if ((Token.Valor = 'PIC') or (Token.Valor = 'PICTURE')) then
      begin
        NextToken(lhPictureStr,true);//IS opt or picture-string
        if Token.Valor = 'IS' then
        begin
          ProcesarToken();
          NextToken(lhPictureStr,true);
        end;
      end
      //lexer-hack: SQL Statement
      else
      if Token.Valor = 'EXEC' then
      begin
        NextToken(lhNormal,true);//CICS, SQL, ...
        if Token.Valor = 'SQL' then
        begin
          ProcesarToken();
          NextToken(lhSQLStatement,true);
        end
      end
      else NextToken(lhNormal,true);
    end;//while
    ProcesarToken();//<end-prg>
  finally
    CloseFile(F);
  end;
end;

{ TCobolPrg }

constructor TCobolPrg.Create;
begin
  LVarWorking := TStringList.Create();
end;

destructor TCobolPrg.Destroy;
begin
  LVarWorking.Free;
  //
  inherited;
end;

{ TListToken }

procedure TListToken.Add(AToken: TTokenCob);
begin
  if First = nil then          
  begin
    First := AToken;
    Last := First;
    AToken.Prev := nil;
  end
  else
  begin
    Last.Next := AToken;
    AToken.Prev := Last;
    Last := AToken;
  end;
  Last.Next := nil;
end;

procedure TListToken.Clear;
begin
  while First <> nil do
  begin
    Curr := First;
    First := First.Next;
    Curr.Free;
  end;
  Last := nil;
end;

constructor TListToken.Create;
begin
  inherited Create;
  First := nil;
  Last := nil;
  Curr := nil;
end;

destructor TListToken.Destroy;
begin
  Clear();
  inherited;
end;

procedure TListToken.FindNext_ResWord(AToken: TTokenCob; sword: string);
begin
  Curr := AToken;
  while not ((Curr.Tipo = tkCobolWord) and
             (Curr.Valor = sword)) do
  begin
    Curr := Curr.Next;
  end;
end;

procedure TListToken.GoFirst;
begin
  Curr := First;
end;

procedure TListToken.GoLast;
begin
  Curr := Last;
end;

procedure TListToken.GoNext;
begin
  Curr := Curr.Next;
end;

function TListToken.InDivision(divName: string): boolean;
begin
  Result := (Curr <> nil) and
            (Curr.Valor = divName) and
            (Curr.GetArea() = 'A') and
            (Curr.Next <> nil)and
            (Curr.Next.Valor = 'DIVISION');
end;

function TListToken.InSection(secName: string): boolean;
begin
  Result := (Curr <> nil) and
            (Curr.Valor = secName) and
            (Curr.GetArea() = 'A') and
            (Curr.Next <> nil)and
            (Curr.Next.Valor = 'SECTION');
end;

function TListToken.InEnd: boolean;
begin
  Result := Curr = nil;
end;

procedure TListToken.MostrarTokens(List: TStrings);
begin
  List.BeginUpdate;
  try
    GoFirst;
    while Curr <> nil do
    begin
      if Curr.Tipo <> tkSepSpace then List.Add(Curr.ValorPrint);
      GoNext();
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TListToken.CargarPrg(CobolPrg: TCobolPrg);
begin
  CobolPrg.DecimalPoint := Self.DecimalPoint;
  //
  GoFirst();
  //IDENTIFICATION DIVISION
  CargarIdentificationDiv(CobolPrg);

  //ENVIRONMENT DIVISION
  if (Curr.Tipo = tkCobolWord) and
     (Curr.Valor = 'ENVIRONMENT') then
  begin
    repeat
      GoNext();
    until InDivision('DATA') or
          InDivision('PROCEDURE') or
          InEnd();
  end;

  //DATA DIVISION
  if InDivision('DATA') then CargarDataDivision(CobolPrg);

  //PROCEDURE DIVISION
  if InDivision('PROCEDURE') then CargarProcDivision(CobolPrg);
end;

procedure TListToken.CargarIdentificationDiv(CobolPrg: TCobolPrg);
begin
  FindNext_ResWord(First,'PROGRAM-ID');
  GoNext();
  if Curr.Tipo = tkSepPeriod then GoNext();
  CobolPrg.ProgramId := Curr.Valor;
  //
  GoNext();
end;

procedure TListToken.CargarDataDivision(CobolPrg: TCobolPrg);
begin
  repeat
    GoNext();
  until InSection('WORKING-STORAGE') or
        InDivision('PROCEDURE') or
        InEnd();
  if InSection('WORKING-STORAGE') then
  begin
    GoNext();
    CargarVariables(CobolPrg);
  end;
end;

procedure TListToken.CargarProcDivision(CobolPrg: TCobolPrg);
begin

end;

procedure TListToken.CargarVariables(CobolPrg: TCobolPrg);
var
  nivel,nombre,tipo: string;
begin
  GoNext();//'.'
  GoNext();//nivel or copy
  if Curr.Valor <> 'COPY' then
  begin
    repeat
      if Curr.Valor = 'EXEC' then
      begin
        //ignorar
        nombre := '';
        tipo := '';
        if Curr.Valor = 'COPY' then;
      end
      else
      if Curr.Valor = 'COPY' then
      begin
        //ignorar nombre de copy
        GoNext();
        nombre := Curr.Valor;
        tipo := 'copy';
        GoNext();
      end
      else
      begin
        nivel := Curr.Valor;
        GoNext();
        nombre := Curr.Valor;
        GoNext();
        if (Curr.Tipo = tkCobolWord) and
           ((Curr.Valor = 'PIC') or
            (Curr.Valor = 'PICTURE')) then tipo := 'elemento'
        else
        if (Curr.Tipo = tkCobolWord) and
           (Curr.Valor = 'REDEFINES') then tipo := 'redefine'
        else tipo := 'grupo';
      end;

      if (nombre <> '') and
         (nombre <> 'FILLER') then
        CobolPrg.LVarWorking.Add(nombre+'('+tipo+')');

      //buscar '.' final de la declaración
      while Curr.Tipo <> tkSepPeriod do
        GoNext();
      repeat
        GoNext();
      until Curr.Tipo <> tkSepPeriod;
      //
    until InSection('LINKAGE') or
          InDivision('PROCEDURE') or
          InEnd();
  end;
end;

procedure TListToken.BuscarMOVE;
var
  P: PResult;
  Temp: TTokenCob;
  T1,T2: TTokenCob;
begin
  GoFirst();
  //
  repeat
    GoNext();
  until InEnd() or
        InDivision('PROCEDURE');

  //PROCEDURE DIVISION
  if InDivision('PROCEDURE') then
  while not InEnd() do
  begin
     if (Curr.Tipo = tkCobolWord) and
        (Curr.Valor = 'MOVE')
     then
     begin
       Temp := Curr;
       GoNext();
       while (Curr.Tipo <> tkSepColon) and (Curr.Valor <> 'TO') and
             not InEnd() do GoNext();
       if not InEnd() then
         if Curr.Tipo = tkSepColon then
         begin
           T1 := Curr.Prev;
           GoNext();
           T2 := Curr;             
           //TODO
           if ((T1.Tipo = tkSepLeftpar) or
                T1.EsNumeroEntero and (T1.Prev.Tipo = tkSepLeftPar))
              and
              ((T2.Tipo = tkSepRightpar) or
                T2.EsNumeroEntero and (T2.Next.Tipo = tkSepRightPar)) then
           begin
             //ignorar
           end
           else
           begin
             New(P);
             P^.fname := '';
             P^.nLine := Temp.nLine;
             P^.nCol := Temp.nCol;
             AList.Add(P);
           end
         end
         else GoNext();
     end
     else GoNext();
  end;
end;

procedure TListToken.GoPrev;
begin
  Curr := Curr.Prev;
end;

procedure TCobolLexer.ClearToken;
begin
  Token.Tipo := tkUndefined;
  Token.Valor := '';
  Token.ValorSQL := nil;
  Token.PalRes := prVacio;
end;

procedure TListToken.PonerColores(List: TStringList);

const ctLongLine = 1000;

var
  i: integer;
  TokenLine: integer;
  sISpan, sFSpan, s: string;
  //prueba
  sline: pchar;
  p: integer;

function CambiarEspacios(s: string): string;
var
  i,j,n: integer;
begin
  n := 0;
  for i := 1 to Length(s) do
    if s[i] = ' ' then Inc(n);
  //
  if n = 0 then Result := s
  else
  begin
    SetLength(Result, Length(s) + 5*n);
    j := 1;
    for i := 1 to Length(s) do
    begin
      if s[i] = ' ' then
      begin
        Move('&nbsp;',Result[j],6);
        Inc(j,6)
      end
      else
      begin
        Result[j] := s[i];
        Inc(j);
      end
    end;//for
  end;
end;

function EsComentario(nline: integer): boolean;
begin
  Result := (Length(List[nline]) >= 7) and (List[nline][7] in ['*','/']);
end;

procedure ProcComent(nline: integer);
begin
  List[nline] :=
    Copy(List[nline], 1, 7)+
    '<span class="CO">'+CambiarEspacios(Copy(List[nline], 8, 65))+'</span>';
end;

procedure FinLinea();
begin
  if p > 0 then List[i] := Copy(List[i],1,7) + Copy(sline, 0, p);
  p := 0;
end;

procedure AddCad(s: string);
begin
  if s <> '' then StrCopy(sline + p, pchar(s));
  Inc(p, Length(s));
end;

begin
  GoFirst(); i := 0;
  GetMem(sline, ctLongLine);
  try
    p := 0;
    while not InEnd() or (i < List.Count) do
    begin
      if InEnd() then TokenLine := MaxInt
      else TokenLine := Curr.nLine - 1;
      if TokenLine > i then
      {procesar lineas hasta encontrar la linea del token}
      begin
        if EsComentario(i) then ProcComent(i)
        else List[i] := Copy(List[i], 1, 7)+CambiarEspacios(Copy(List[i],8,65));
        //
        FinLinea();//<<fin de linea>>
        Inc(i);
      end
      else if TokenLine = i then
      {procesar Token}
      begin
        PrepararFormato(Curr.Tipo, sISpan, sFSpan);
        //
        if ((Curr.Prev = nil) or (Curr.Prev.nLine < Curr.nLine)) and
           (p = 0) then
        {es el primer token de la linea y no se ha procesado lo
         que está antes de él, en la misma linea}
        begin
          s := Copy(List[i], 8, Curr.nCol - 8);
          if Length(s) > 0 then AddCad(CambiarEspacios(s));
        end;
        //
        if Curr.Next = nil then
        {es ultimo token del programa}
        begin
          AddCad(sISpan+
            CambiarEspacios(Copy(List[i], Curr.nCol, 72 - Curr.nCol + 1))+sFSpan);
          //
          FinLinea();//<<fin de linea>>
          Inc(i);
        end
        else
        if Curr.Next.nLine = i + 1 then
        {el siguiente esta en la misma linea}
        begin
          AddCad(sISpan+
            CambiarEspacios(Copy(List[i], Curr.nCol, Curr.Next.nCol - Curr.nCol))+
            sFSpan);
        end
        else
        {el siguiente esta en otra linea}
        begin
          AddCad(sISpan+
            CambiarEspacios(Copy(List[i], Curr.nCol, 72 - Curr.nCol + 1))+
            sFSpan);
          //
          FinLinea();//<<fin de linea>>
          Inc(i);
          //procesar lineas hasta el siguiente token
          while i + 1 < Curr.Next.nLine do
          begin
            if EsComentario(i) then ProcComent(i)
            else                                         
            if Length(List[i]) > 7 then
              if (List[i][7] = '-') {or (Curr.Tipo = tkCommentEntry)} then //linea de continuacion del token
                List[i] :=
                  Copy(List[i], 1, 7)+sISpan+CambiarEspacios(Copy(List[i], 8, 65))+sFSpan
              else
                List[i] := Copy(List[i], 1, 7)+CambiarEspacios(Copy(List[i], 8, 65));
            //
            Inc(i);
          end;
          //ya estamos en la linea del siguiente token. ver si antes del
          //comienzo de dicho token hay continuación del token actual
          if (Curr.Next.nCol > 8) and ((List[i][7] = '-') {or (Curr.Tipo = tkCommentEntry)}) then
            AddCad(sISpan+CambiarEspacios(Copy(List[i], 8, Curr.Next.nCol - 8))+sFSpan)
        end;
        GoNext();
      end;//procesar token
    end;//while
  finally
    FreeMem(sline, ctLongLine);
  end;  
end;

procedure TListToken.PrepararFormato(Tipo: TTokenId; var sInit, sFin: string);
begin
  case Tipo of
    tkReservedWord:
    begin
      sInit := '<span class="RW">';
      sFin := '</span>';
    end;
    tkCobolWord:
    begin
      sInit := '<span class="CW">';
      sFin := '</span>';
    end;
    tkBlockHeader:
    begin
      sInit := '<span class="IB">';
      sFin := '</span>';
    end
    else
    begin
      sInit := '';
      sFin := '';
    end
  end;//case
end;                                      

constructor TCobolLexer.Create(UsarPalRes: boolean);
begin
  Self.UsarPalRes := UsarPalRes;
end;

function TCobolLexer.BuscarPalRes(sword: string): integer;
var
  left,right: integer;
  medio: integer;
  RCmp: integer;
begin
  //búsqueda binaria
  left := 0;
  right := ctnPalRes - 1;
  Result := -1;
  while (Left <= Right) and (Result = -1) do
  begin
    medio := (left + right) div 2;
    RCmp := CompareStr(sword, ArrPalRes[medio]);
    if RCmp = 0 then Result := medio
    else if RCmp > 0 then left := medio + 1
    else right := medio - 1;
  end;
end;

function TCobolLexer.IsInfo(s: string): boolean;
const
  InfoHeaders: array[0..4] of string =
    ('AUTHOR','INSTALLATION','DATE-WRITTEN','DATE-COMPILED','SECURITY');
var
  i: integer;
begin
  Result := false;
  i := 0;
  while i < 5 do
  begin
    if s = InfoHeaders[i] then
    begin
      Result := true;
      i := 5;
    end
    else Inc(i);
  end
end;

end.
