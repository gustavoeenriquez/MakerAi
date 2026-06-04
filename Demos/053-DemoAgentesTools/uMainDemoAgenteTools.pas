unit uMainDemoAgenteTools;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.Objects, FMX.TabControl, FMX.WebBrowser;

type
  TFMainDemo = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    MemoPregunta: TMemo;
    Layout7: TLayout;
    MemoRespuesta: TMemo;
    Layout8: TLayout;
    Label1: TLabel;
    BtnBusquedaWeb: TButton;
    Label2: TLabel;
    BtnCrearTabla: TButton;
    BtnLoadData: TButton;
    BtnBuscarRAGV: TButton;
    Splitter1: TSplitter;
    BtnBusquedaVQL: TButton;
    BtnPdfToText: TButton;
    OpenDialog1: TOpenDialog;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Image1: TImage;
    BtnCrearImagen: TButton;
    TabWeb: TTabItem;
    WebBrowser1: TWebBrowser;
    BtnGetHtml: TButton;
    procedure BtnBusquedaWebClick(Sender: TObject);
    procedure BtnCrearTablaClick(Sender: TObject);
    procedure BtnLoadDataClick(Sender: TObject);
    procedure BtnBuscarRAGVClick(Sender: TObject);
    procedure BtnBusquedaVQLClick(Sender: TObject);
    procedure BtnPdfToTextClick(Sender: TObject);
    procedure BtnCrearImagenClick(Sender: TObject);
    procedure BtnGetHtmlClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMainDemo: TFMainDemo;

implementation

{$R *.fmx}

uses uDmAgentesTool;

procedure TFMainDemo.BtnBuscarRAGVClick(Sender: TObject);
Var
  Dm: TFDataModule;
  Pregunta, Respuesta: String;
begin
  Dm := TFDataModule.Create(Nil);
  Try
    Pregunta := MemoPregunta.Lines.Text;
    Respuesta := Dm.BuscarRAGV(Pregunta);
    MemoRespuesta.Lines.Text := Respuesta;
  Finally
    Dm.Free;
  End;

end;

procedure TFMainDemo.BtnBusquedaVQLClick(Sender: TObject);
Var
  Dm: TFDataModule;
  Pregunta, Respuesta: String;
begin
  Dm := TFDataModule.Create(Nil);
  Try
    Pregunta := MemoPregunta.Lines.Text;
    Respuesta := Dm.BuscarVQL(Pregunta);
    MemoRespuesta.Lines.Text := Respuesta;
  Finally
    Dm.Free;
  End;
end;

procedure TFMainDemo.BtnBusquedaWebClick(Sender: TObject);
Var
  Dm: TFDataModule;
  Pregunta, Respuesta: String;
begin
  Dm := TFDataModule.Create(Nil);
  Try
    Pregunta := MemoPregunta.Lines.Text;
    Respuesta := Dm.WebSearch(Pregunta);
    MemoRespuesta.Lines.Text := Respuesta;
  Finally
    Dm.Free;
  End;
end;

procedure TFMainDemo.BtnCrearImagenClick(Sender: TObject);
Var
  Dm: TFDataModule;
  Pregunta: String;
  Respuesta : TBitMap;
begin
  Dm := TFDataModule.Create(Nil);
  Try
    Pregunta := MemoPregunta.Lines.Text;
    Respuesta := Dm.CreateImage(Pregunta);
    Image1.Bitmap.Assign(Respuesta);
  Finally
    Dm.Free;
  End;
end;

procedure TFMainDemo.BtnCrearTablaClick(Sender: TObject);
Var
  Dm: TFDataModule;
begin
  Dm := TFDataModule.Create(Nil);
  Try
    If Dm.CrearTablaVectoRAG then
      ShowMessage('La tabla se creó con éxito');
  Finally
    Dm.Free;
  End;
end;

procedure TFMainDemo.BtnGetHtmlClick(Sender: TObject);
Var
  Dm: TFDataModule;
  Pregunta, Respuesta : String;
begin
  Dm := TFDataModule.Create(Nil);
  Try
     Pregunta := MemoPregunta.Lines.Text;

     Respuesta := Dm.CreateDashBoard(Pregunta);

     MemoRespuesta.Lines.Text := Respuesta;

     WebBrowser1.LoadFromStrings(Respuesta,'localhost');
  Finally
    Dm.Free;
  End;
end;

procedure TFMainDemo.BtnLoadDataClick(Sender: TObject);
Var
  Dm: TFDataModule;
begin
  Dm := TFDataModule.Create(Nil);
  Try
    Var
    NoItems := Dm.LoadRAGV;
    ShowMessage('Se crearon ' + NoItems.toString + ' items nuevos');
  Finally
    Dm.Free;
  End;
end;

procedure TFMainDemo.BtnPdfToTextClick(Sender: TObject);
Var
  Dm: TFDataModule;
begin
  If OpenDialog1.Execute then
  Begin
    Dm := TFDataModule.Create(Nil);
    Try
      Var
      Result := Dm.PdfToText(OpenDialog1.FileName);
      MemoRespuesta.Lines.Text := Result;
    Finally
      Dm.Free;
    End;
  End;
end;

end.
