unit uMCPClientEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.JSon,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Grids,
  uMakerAi.Core, uMakerAi.ToolFunctions, uMakerAi.MCPClient.Core, Vcl.Buttons;

type
  TFMCPClientEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    EditProtocol: TComboBox;
    Label1: TLabel;
    lblName: TLabel;
    EditName: TEdit;
    BtnTest: TButton;
    BtnSetDefaultParams: TButton;
    Panel3: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel4: TPanel;
    PageControl1: TPageControl;
    TabParams: TTabSheet;
    sgProperties: TStringGrid;
    TabLog: TTabSheet;
    MemoLog: TMemo;
    Panel5: TPanel;
    LblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure sgPropertiesDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect: TRect; State: TGridDrawState);
    procedure BtnTestClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BtnSetDefaultParamsClick(Sender: TObject);
  private
    procedure MCPLogEvent(Sender: TObject; const Msg: string);
    procedure MCPStatusEvent(Sender: TObject; const StatusMsg: string);
    Procedure SetDefaultParams(ActParams, VarEnv: TStringList);

  public
    Procedure SetProperties(Properties: TStrings; VarEnv: TStrings);
    Procedure GetProperties(Properties: TStrings; VarEnv: TStrings);
  end;

var
  FMCPClientEditor: TFMCPClientEditor;

implementation

{$R *.dfm}

procedure TFMCPClientEditor.BtnSetDefaultParamsClick(Sender: TObject);
begin
  SetDefaultParams(Nil, Nil);
end;

procedure TFMCPClientEditor.BtnTestClick(Sender: TObject);
Var
  MCPClient: TMCPClientCustom;
  jTools: TJSonObject;
  ActParams, VarEnv: TStringList;
begin

  MemoLog.Lines.Clear;

  ActParams := TStringList.Create;
  VarEnv := TStringList.Create;
  Try
    GetProperties(ActParams, VarEnv);
    SetProperties(ActParams, VarEnv);
  Finally
    ActParams.Free;
    VarEnv.Free;
  End;

  Case EditProtocol.ItemIndex of
    0:
      MCPClient := TMCPClientStdIo.Create(Self);
    1:
      MCPClient := TMCPClientHttp.Create(Self);
    2:
      MCPClient := TMCPClientSSE.Create(Self);
    3:
      MCPClient := TMCPClientMakerAi.Create(Self);
  End;

  Try
    MCPClient.OnLog := MCPLogEvent;
    MCPClient.OnStatusUpdate := MCPStatusEvent;

    Var
    Properties := TStringList.Create;
    VarEnv := TStringList.Create;
    Try
      GetProperties(Properties, VarEnv);

      MCPClient.Params.Assign(Properties);
      MCPClient.EnvVars.Assign(VarEnv);

    Finally
      Properties.Free;
      VarEnv.Free;
    End;

    MCPClient.Initialize;
    jTools := MCPClient.ListTools;

    If Assigned(jTools) then
    Begin
      // MemoLog.Lines.Text := jTools.Format;
      PageControl1.ActivePage := TabLog;
      ShowMessage('The server test ran successfully.');
    End
    Else
    Begin
      PageControl1.ActivePage := TabLog;
      ShowMessage('Error starting server. more info in log');
    End;

  Finally
    MCPClient.Free;
  End;

end;

procedure TFMCPClientEditor.FormCreate(Sender: TObject);
begin
  sgProperties.ColWidths[0] := 150; // Columna Propiedad
  sgProperties.ColWidths[1] := 250; // Columna Valor

  sgProperties.Cells[0, 0] := 'Properties';
  sgProperties.Cells[1, 0] := 'Value';

  PageControl1.ActivePage := TabParams;
end;

procedure TFMCPClientEditor.FormResize(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  // Ajustar el ancho de las columnas al redimensionar el formulario
  AvailableWidth := sgProperties.ClientWidth - 4; // Margen para scrollbar
  // sgProperties.ColWidths[0] := AvailableWidth div 2;
  sgProperties.ColWidths[1] := AvailableWidth - sgProperties.ColWidths[0];
end;

procedure TFMCPClientEditor.SetDefaultParams(ActParams, VarEnv: TStringList);
Var
  DefParams: TStringList;
  I: Integer;
  Prop, Value: String;
  MCPClient: TMCPClientCustom;
begin
  MCPClient := TMCPClientCustom.Create(Self);

  If Not Assigned(ActParams) then
    ActParams := TStringList.Create;

  If Not Assigned(VarEnv) then
    VarEnv := TStringList.Create;

  Try
    DefParams := MCPClient.GetDefaultParams;
    Try
      GetProperties(ActParams, VarEnv);

      For I := 0 to ActParams.Count - 1 do
      Begin
        Prop := ActParams.Names[I];
        Value := ActParams.ValueFromIndex[I];
        DefParams.Values[Prop] := Value;
      End;
      SetProperties(DefParams, VarEnv);

    Finally
      DefParams.Free;
    End;
  Finally
    MCPClient.Free;
    ActParams.Free;
  End;

end;

procedure TFMCPClientEditor.SetProperties(Properties: TStrings; VarEnv: TStrings);
Var
  I, Idx: Integer;
begin

  sgProperties.RowCount := Properties.Count + VarEnv.Count + 1; // +1 para el encabezado
  Idx := 0;

  if Properties.Count > 0 then
  begin

    for I := 0 to Properties.Count - 1 do
    begin
      sgProperties.Cells[0, Idx + 1] := Properties.Names[I];
      sgProperties.Cells[1, Idx + 1] := Properties.ValueFromIndex[I];
      Inc(Idx);
    end;
  end;

  if VarEnv.Count > 0 then
  begin

    for I := 0 to VarEnv.Count - 1 do
    begin
      sgProperties.Cells[0, Idx + 1] := 'Env:' + VarEnv.Names[I];
      sgProperties.Cells[1, Idx + 1] := VarEnv.ValueFromIndex[I];
      Inc(Idx);
    end;
  end;

end;

procedure TFMCPClientEditor.GetProperties(Properties: TStrings; VarEnv: TStrings);
var
  I: Integer;
  PropName, PropValue: string;
begin
  Properties.Clear;
  VarEnv.Clear;

  for I := 1 to sgProperties.RowCount - 1 do
  begin
    PropName := Trim(sgProperties.Cells[0, I]);
    PropValue := sgProperties.Cells[1, I]; // No trim del valor por si tiene espacios intencionados

    if PropName <> '' then
    Begin
      If SameText(Copy(PropName, 1, 4), 'Env:') then
      Begin
        PropName := Copy(PropName, 5, Length(PropName));
        VarEnv.Values[PropName] := PropValue;
      End
      Else
        Properties.Values[PropName] := PropValue;
    End;
  end;
end;

procedure TFMCPClientEditor.MCPLogEvent(Sender: TObject; const Msg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      MemoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' - ' + Msg);
      MemoLog.SelStart := MemoLog.GetTextLen;
      MemoLog.SelLength := 0;
      MemoLog.Perform(WM_VSCROLL, SB_BOTTOM, 0);
    end);

end;

procedure TFMCPClientEditor.MCPStatusEvent(Sender: TObject; const StatusMsg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      LblStatus.Caption := '   Status: ' + StatusMsg;
      Application.processMessages;
    end);
end;

procedure TFMCPClientEditor.sgPropertiesDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect: TRect; State: TGridDrawState);
begin
  if (ARow = 0) or (ACol = 0) then
  begin
    sgProperties.Canvas.Brush.Color := clBtnFace;
    sgProperties.Canvas.Font.Style := [fsBold];
    sgProperties.Canvas.FillRect(Rect);
    sgProperties.Canvas.TextRect(Rect, Rect.Left + 4, Rect.Top + 4, sgProperties.Cells[ACol, ARow]);
  end;
end;

end.
