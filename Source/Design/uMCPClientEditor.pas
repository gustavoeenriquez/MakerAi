// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMCPClientEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.JSon, System.Rtti, System.TypInfo,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Grids,
  uMakerAi.Core, uMakerAi.Tools.Functions, uMakerAi.MCPClient.Core, Vcl.Buttons;

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

{procedure TFMCPClientEditor.BtnTestClick(Sender: TObject);
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
  Else
    MCPClient := TMCPClientStdIo.Create(Self);
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
}

procedure TFMCPClientEditor.BtnTestClick(Sender: TObject);
var
  MCPClient: TMCPClientCustom;
  jTools: TJSONObject;
  Properties, VarEnv: TStringList;
begin
  MemoLog.Lines.Clear;
  MemoLog.Lines.Add('--- Iniciando Test de Conexión ---');

  // 1. Factory: Crear la instancia correcta
  case EditProtocol.ItemIndex of
    0: MCPClient := TMCPClientStdIo.Create(nil);   // Stdio
    1: MCPClient := TMCPClientHttp.Create(nil);    // HTTP
    2: MCPClient := TMCPClientSSE.Create(nil);     // SSE
    3: MCPClient := TMCPClientMakerAi.Create(nil); // MakerAi
  else
    MCPClient := TMCPClientStdIo.Create(nil);
  end;

  Properties := TStringList.Create;
  VarEnv := TStringList.Create;
  try
    try
      // 2. Asignar Eventos (Deben ser thread-safe, ver abajo)
      MCPClient.OnLog := MCPLogEvent;
      MCPClient.OnStatusUpdate := MCPStatusEvent;

      // 3. Obtener Configuración de la UI
      GetProperties(Properties, VarEnv);

      // Validación rápida para SSE
      if (MCPClient is TMCPClientSSE) and (Pos('http', Properties.Values['URL']) = 0) then
      begin
        ShowMessage('Error: Para SSE el parámetro "URL" es obligatorio (ej: http://localhost:3000/sse)');
        Exit;
      end;

      // 4. Inyectar Parámetros
      MCPClient.Params.Assign(Properties);
      MCPClient.EnvVars.Assign(VarEnv);

      MemoLog.Lines.Add(Format('Protocolo: %s', [GetEnumName(TypeInfo(TToolTransportType), Ord(MCPClient.TransportType))]));

      // 5. Inicializar (Conecta, hace Handshake y verifica capacidades)
      // Nota: Si usaste el fix de "CheckSynchronize" en WaitForInitialization, esto no bloqueará la UI.
      if MCPClient.Initialize then
      begin
        MemoLog.Lines.Add('>> Inicialización Exitosa. Solicitando herramientas...');

        // 6. Probar una llamada real (ListTools)
        jTools := MCPClient.ListTools;
        try
          if Assigned(jTools) then
          begin
            MemoLog.Lines.Add('>> Herramientas recibidas:');
            MemoLog.Lines.Add(jTools.Format(2)); // Formatear bonito

            PageControl1.ActivePage := TabLog;
            ShowMessage('✅ ¡Conexión Exitosa!' + sLineBreak +
                        'Se detectaron ' + IntToStr(jTools.GetValue<TJSONArray>('tools').Count) + ' herramientas.');
          end
          else
          begin
            PageControl1.ActivePage := TabLog;
            ShowMessage('⚠️ Conectado, pero ListTools devolvió vacío o nulo.');
          end;
        finally
          jTools.Free;
        end;
      end
      else
      begin
        PageControl1.ActivePage := TabLog;
        ShowMessage('❌ Error al inicializar el cliente.' + sLineBreak + 'Revisa el Log para más detalles.');
      end;

    except
      on E: Exception do
      begin
        PageControl1.ActivePage := TabLog;
        MemoLog.Lines.Add('EXCEPTION: ' + E.Message);
        ShowMessage('Excepción Crítica: ' + E.Message);
      end;
    end;
  finally
    Properties.Free;
    VarEnv.Free;
    MCPClient.Free; // Importante: Esto cierra el socket y detiene los hilos
  end;
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
