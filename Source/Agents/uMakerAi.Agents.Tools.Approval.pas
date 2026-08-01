// MIT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
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

unit uMakerAi.Agents.Tools.Approval;

// Herramienta de suspensión human-in-the-loop.
// Cuando un nodo ejecuta esta tool, la ejecución del grafo se suspende
// y se dispara el evento TAIAgentManager.OnSuspend.
// La ejecución se reanuda llamando a TAIAgentManager.ResumeThread.
//
// Uso:
//   LNode.Tool := TAiWaitApprovalTool.Create(LNode);
//   TAiWaitApprovalTool(LNode.Tool).SuspendReason := 'Requiere aprobación';
//   TAiWaitApprovalTool(LNode.Tool).ContextKey    := 'resultado_previo';

interface

uses
  System.SysUtils, System.StrUtils,
  uMakerAi.Agents, uMakerAi.Agents.Attributes, uMakerAi.Agents.EngineRegistry;

type
  [TToolAttribute('WaitApproval',
                  'Suspende la ejecución hasta que un operador humano apruebe el paso',
                  'Control')]
  TAiWaitApprovalTool = class(TAiToolBase)
  private
    FSuspendReason: string;
    FContextKey:    string;
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string;
                      var AOutput: string); override;
  published
    [TToolParameterAttribute('Razón de suspensión',
                             'Mensaje legible que explica por qué se requiere aprobación',
                             'Se requiere aprobación del operador')]
    property SuspendReason: string read FSuspendReason write FSuspendReason;

    [TToolParameterAttribute('Clave de contexto',
                             'Clave del Blackboard cuyo valor se incluye como contexto',
                             '')]
    property ContextKey: string read FContextKey write FContextKey;
  end;

implementation

{ TAiWaitApprovalTool }

procedure TAiWaitApprovalTool.Execute(ANode: TAIAgentsNode;
  const AInput: string; var AOutput: string);
var
  LContext: string;
  LReason:  string;
  LKey:     string;
begin
  // Pass-through: cuando se reanude, AInput es la respuesta del humano
  AOutput := AInput;

  if not Assigned(ANode) then
    Exit;

  // ResumeThread vuelve a ejecutar este nodo (DoExecute con aBeforeNode=nil),
  // asi que la tool corre DOS veces: al suspender y al reanudar. La marca en
  // el blackboard distingue ambos casos —mismo patron que el demo
  // 10-HumanInTheLoop— y viaja en el checkpoint, por lo que sobrevive a un
  // reinicio del proceso.
  LKey := 'approval.' + ANode.Name + '.pending';
  if Assigned(ANode.Graph) and
     (ANode.Graph.Blackboard.GetString(LKey) = '1') then
  begin
    // Reanudacion: no suspender de nuevo; AOutput ya lleva la respuesta.
    ANode.Graph.Blackboard.SetString(LKey, '');
    Exit;
  end;

  LContext := '';
  if (FContextKey <> '') and Assigned(ANode.Graph) then
    LContext := ANode.Graph.Blackboard.GetString(FContextKey);

  LReason := IfThen(FSuspendReason <> '',
                    FSuspendReason,
                    'Waiting for human approval');

  if Assigned(ANode.Graph) then
    ANode.Graph.Blackboard.SetString(LKey, '1');
  ANode.Suspend(LReason, LContext);
end;

initialization
  TEngineRegistry.Instance.RegisterTool(
    TAiWaitApprovalTool, 'uMakerAi.Agents.Tools.Approval');

end.
