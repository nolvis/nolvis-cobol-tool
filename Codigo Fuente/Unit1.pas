unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, CargadorCobol, ExtCtrls, Menus, OleCtrls,
  SHDocVw, MSHTML, ActiveX;

type

  TipoExt = (teCBL, teCOB);
  TExtBuscar = set of TipoExt;

  TForm1 = class(TForm)
    Panel2: TPanel;
    Label2: TLabel;
    edPath: TEdit;
    ckbSubCarpetas: TCheckBox;
    ckbCBL: TCheckBox;
    ckbCOB: TCheckBox;
    edPatron: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel3: TPanel;
    lbInci: TListBox;
    Panel4: TPanel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    tvResult: TTreeView;
    paHeader: TPanel;
    lHeader: TLabel;
    Panel5: TPanel;
    WebBrowser1: TWebBrowser;
    Panel6: TPanel;
    btnMostrarTodos: TButton;
    btnBuscarPatron: TButton;
    lMsg: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    procedure tvResultChange(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
    procedure edPatronKeyPress(Sender: TObject; var Key: Char);
    procedure btnBuscarPatronClick(Sender: TObject);
    procedure btnMostrarTodosClick(Sender: TObject);
  private
    cntProce, cntEncon: integer;
    CurrNode: TTreeNode;
    procedure BuscarProgramas(SoloMostrar: boolean);
    procedure BorrarTVResult();
    //
    procedure LoadBlankDoc(WebBrowser: TWebBrowser);
    procedure CheckDocReady(WebBrowser: TWebBrowser);
    procedure LoadDocFromStream(WB: TWebBrowser; Stream: TStream);
    procedure LoadFromList(WebBrowser: TWebBrowser; List: TStringList);
    procedure CargarPrograma(fname: string);
  public
    constructor Create(Owner: TComponent);override;
  end;

var
  Form1: TForm1;

implementation

uses Math, UrsProfiler;

{$R *.dfm}

constructor TForm1.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  CurrNode := nil;
end;

procedure TForm1.tvResultChange(Sender: TObject; Node: TTreeNode);
var
  fname: string;
  P: PResult;
  OldPos,NewPos: integer;
begin
  if (CurrNode = nil) or
     (Node <> CurrNode) and (Node.Parent <> CurrNode) then
  begin
    if Node.Parent = nil then CurrNode := Node
    else CurrNode := Node.Parent;
    //
    fname := CurrNode.Text;
    {lbPrg.Items.BeginUpdate;
    lbPrg.Items.LoadFromFile(fname);
    lbPrg.Items.EndUpdate;}
    CargarPrograma(fname);
  end;

  if Node.Parent <> nil then
  begin
    P := PResult(Node.Data);
    //
    OldPos := TWebbrowser(WebBrowser1).OleObject.Document.Body.ScrollTop;
    NewPos := P^.nLine * 16;
    WebBrowser1.OleObject.Document.ParentWindow.ScrollBy(0, NewPos - OldPos);
    //
    lHeader.Caption := Node.Parent.Text;
  end
  else lHeader.Caption := Node.Text;
  lHeader.Caption := ' '+lHeader.Caption;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  lHeader.Caption := '';
  edPath.SetFocus;
end;

procedure TForm1.BorrarTVResult();
var
  i: integer;
  P: PResult;
begin
  for i := 0 to tvResult.Items.Count - 1 do
  begin
    P := PResult(tvResult.Items[i].Data);
    if P <> nil then Dispose(P);
  end;
  tvResult.Items.Clear();
end;

procedure TForm1.edPatronKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then btnMostrarTodosClick(btnBuscarPatron);
end;

procedure TForm1.LoadBlankDoc(WebBrowser: TWebBrowser);
begin
  WebBrowser.Navigate('about:blank', EmptyParam, EmptyParam, EmptyParam, EmptyParam);
  while WebBrowser.ReadyState <> READYSTATE_COMPLETE do
  begin
    Application.ProcessMessages;
    Sleep(0);
  end;
end;

procedure TForm1.CheckDocReady(WebBrowser: TWebBrowser);
begin
  if not Assigned(WebBrowser.Document) then
    LoadBlankDoc(WebBrowser);
end;

procedure TForm1.LoadDocFromStream(WB: TWebBrowser; Stream: TStream);
begin
  CheckDocReady(WB);
  (WB.Document as IPersistStreamInit).Load(TStreamAdapter.Create(Stream));
end;

procedure TForm1.LoadFromList(WebBrowser: TWebBrowser; List: TStringList);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create();
  List.SaveToStream(Stream);
  Stream.Seek(0,0);
  LoadDocFromStream(WebBrowser, Stream);
  Stream.Free;
end;

procedure TForm1.CargarPrograma(fname: string);
var
  List: TStringList;
  s: string;
  sline,s1,s2: string;
  //
  Lexer: TCobolLexer;
  ListToken: TListToken;
  i: integer;

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

begin
  Lexer := TCobolLexer.Create(true);
  ListToken := TListToken.Create;
  List := TStringList.Create;
  try
    Lexer.CargarTokens(fname,ListToken,List);
    ListToken.PonerColores(List);
    for i := 0 to List.Count - 1 do
    begin
      sline := List[i];
      s1 := CambiarEspacios(Copy(sline,1,7));
      s2 := Copy(sline,8,1000);
      //
      s := '<tr><td class="c1">'+s1+'</td><td class="c2">'+s2+'</td><td class="c3">&nbsp;</td></tr>';
      List[i] := s;
    end;
  finally
    Lexer.Free;
    ListToken.Free;
  end;
  List.Insert(0,'<html><head>');
  List.Insert(1,'<style>');
  List.Insert(2,'.c1 {width: 57px; color: white;}');
  List.Insert(3,'.c2 {width: 520px; color: black; background-color: #ffffff;}');
  List.Insert(4,'.c3 {color: white; background-color: #008080;}');
  List.Insert(5,'.CO {color: black; background-color: #c0c0c0; width:100%;}');
  List.Insert(6,'.RW {color: #008000;}');
  List.Insert(7,'.CW {color: #800000;}');
  List.Insert(8,'.IB {color: #008080;}');
  List.Insert(9,'</style></head>');
  List.Insert(10,'<body style="margin: 16px 0px 16px 0px; background-color: #008080;">');
  List.Insert(11,'<table cellpadding="0" cellspacing="0" border="0" style ="width: 800px; font-family: Courier New; font-size: 10pt;color: black; background-color: #008080;">');
  //
  List.Add('</table></body></html>');
  //OJO: TODO: grabar directamente en el Stream que usar� el WebBrowser
  LoadFromList(WebBrowser1,List);
  //
  List.Free;
end;

{TEST: procedure TForm1.Button2Click(Sender: TObject);
var
  Lexer: TCobolLexer;
  ListToken: TListToken;
  Sum,Time: Extended;
  i: integer;
  fname: string;
  List: TStringList;
begin
  fname := edPath.Text;
  if fname[Length(fname)] <> '\' then fname := fname + '\';
  fname := fname + edPatron.Text+'.cbl';
  //
  Sum := 0;
  for i := 1 to 100 do
  begin
    rsProfiler.Clear;
    rsProfiler[0].Start;
    //
    Lexer := TCobolLexer.Create(true);
    ListToken := TListToken.Create;
    List := tStringList.Create();
    try
      Lexer.CargarTokens(fname,ListToken,List);
      ListToken.PonerColores(List);
    finally
      Lexer.Free;
      ListToken.Free;
      List.Free;
    end;
    //
    rsProfiler[0].Stop;
    Time := rsProfiler[0].TimeMS;
    Sum := Sum + Time;
  end;
  //
  lTime.Caption := FloatToStrF(Sum/100,fffixed,10,2)+' �s';
end;}

procedure TForm1.BuscarProgramas;
var
  sPath, sPatron: string;
  ListG: TStringList;
  SubCarpetas: boolean;
  ExtBuscar: TExtBuscar;
  //
  Lexer: TCobolLexer;
  ListToken: TListToken;
  //
  Time1,Time2: TDateTime;

procedure ProcesarFichero(fname: string);
var
  i: integer;
  AList: TList;
  ANode: TTreeNode;
  ALine,ACol: integer;
  s: string;
  Res: TLexerResult;
  //
  {TEST: ListF: TStringList;}
begin
  Inc(cntProce);
  //
  {TEST: ListF := TStringList.Create();
  ListToken.MostrarTokens(ListF);
  ListF.SaveToFile('E:\Tmp\'+ExtractFileName(fname)+'.txt');
  ListF.Free;}
  //
  if SoloMostrar then
  begin
    Inc(cntEncon);
    tvResult.Items.AddChild(nil,fname);
  end
  else
  begin
    ListToken.Clear();
    Res := Lexer.CargarTokens(fname,ListToken,nil);
    AList := TList.Create;
    try
    case Res of
      lrLexerOK:
        begin
          ListToken.BuscarMOVE(AList);
          if AList.Count > 0 then
          begin
            Inc(cntEncon);
            ANode := tvResult.Items.AddChild(nil,fname);
            for i := 0 to AList.Count - 1 do
            begin
              ALine := PResult(AList[i]).nLine;
              ACol := PResult(AList[i]).nCol;
              s := 'MOVE ('+IntToStr(ALine)+','+IntToStr(ACol)+')';
              tvResult.Items.AddChildObject(ANode,s,AList[i]);
            end;
          end;
        end;

      lrFicheroIncorrecto:
        lbInci.Items.Add('Fichero incorrecto: '+fname);

      lrTokenIndefinido:
        lbInci.Items.Add('Token indefinido: '+fname);
    end;
    finally
      AList.Free;
    end;
  end;
end;

procedure ProcesarDIR(sPath, sPatron: string);
var
  sr: TSearchRec;
  FileAttrs: integer;
  ArrDIR: array of string;
  sBuscar: string;
  i: integer;
begin
  //procesar archivos '.cbl'
  if (teCBL in ExtBuscar) then
  begin
    FileAttrs := faArchive;
    sBuscar := sPath+sPatron+'.cbl';
    if FindFirst(sBuscar, FileAttrs, sr) = 0 then
    begin
      repeat
        if (sr.Attr and FileAttrs) = sr.Attr then
          ProcesarFichero(sPath+sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;
  //procesar archivos '.cob'
  if (teCOB in ExtBuscar) then
  begin
    FileAttrs := faArchive;
    sBuscar := sPath+sPatron+'*.cob';
    if FindFirst(sBuscar, FileAttrs, sr) = 0 then
    begin
      repeat
        if (sr.Attr and FileAttrs) = sr.Attr then
          ProcesarFichero(sPath+sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;
  //procesar subcarpetas
  if SubCarpetas then
  begin
    FileAttrs := faDirectory;
    sBuscar := sPath+'*';
    if FindFirst(sBuscar, FileAttrs, sr) = 0 then
    begin
      repeat
        if ((sr.Attr and FileAttrs) = sr.Attr) and
            (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          SetLength(ArrDIR,Length(ArrDIR)+1);
          ArrDIR[High(ArrDIR)] := sr.Name;
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    for i := 0 to High(ArrDIR) do
      ProcesarDIR(sPath+ArrDIR[i]+'\',sPatron);
    SetLength(ArrDIR,0);
  end;
end;

begin
  //validar el camino
  sPath := edPath.Text;
  if sPath = '' then Exit;
  if sPath[Length(sPath)] <> '\' then sPath := sPath + '\';
  //validar opciones
  ExtBuscar := [];
  if ckbCBL.Checked then ExtBuscar := ExtBuscar + [teCBL];
  if ckbCOB.Checked then ExtBuscar := ExtBuscar + [teCOB];
  if ExtBuscar = [] then
  begin
    ShowMessage('Debe seleccionar al menos una extensi�n.');
    Exit;
  end;
  SubCarpetas := ckbSubCarpetas.Checked;
  //
  lHeader.Caption := '';
  tvResult.Items.BeginUpdate;
  tvResult.OnChange := nil;
  BorrarTVResult();
  tvResult.Items.EndUpdate;
  tvResult.Items.BeginUpdate;
  //
  ListG := TStringList.Create();
  lMsg.Visible := true;
  lMsg.Update;
  //
  lbInci.Items.Clear;
  lbInci.Update;
  Lexer := TCobolLexer.Create(false);
  ListToken := TListToken.Create();

  sPatron := Trim(edPatron.Text);
  cntProce := 0;
  cntEncon := 0;
  if sPatron = '' then sPatron := '*';
  Time1 := Time();
  try
    ProcesarDIR(sPath,sPatron);
    lbInci.Items.Add('Archivos procesados : '+IntToStr(cntProce));
    lbInci.Items.Add('Archivos encontrados: '+IntToStr(cntEncon));
    Time2 := Time();
    Time1 := Time2 - Time1;
    lbInci.Items.Add(DateTimeToStr(Time1));
    PageControl1.ActivePageIndex := 1;
  finally
    ListG.Free;
    Lexer.Free;
    ListToken.Free;
    tvResult.Items.EndUpdate();
    tvResult.OnChange := tvResultChange;
    lMsg.Hide;
  end;
end;

procedure TForm1.btnBuscarPatronClick(Sender: TObject);
begin
  BuscarProgramas(false);
end;

procedure TForm1.btnMostrarTodosClick(Sender: TObject);
begin
  BuscarProgramas(true);
end;

initialization
  OleInitialize(nil);

finalization
  OleUninitialize
//end

end.
