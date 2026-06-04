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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Agents.Tools.Approval;

// Herramienta de suspensi?n human-in-the-loop.
// Cuando un nodo ejecuta esta tool, la ejecuci?n del grafo se suspende
// y se dispara el evento TAIAgentManager.OnSuspend.
// La ejecuci?n se reanuda llamando a TAIAgentManager.ResumeThread.
//
// Uso:
//   LNode.Tool := TAiWaitApprovalTool.Create(LNode);
//   TAiWaitApprovalTool(LNode.Tool).SuspendReason := 'Requiere aprobaci?n';
//   TAiWaitApprovalTool(LNode.Tool).ContextKey    := 'resultado_previo';

interface

uses
  System.SysUtils, System.StrUtils,
  uMakerAi.Agents, uMakerAi.Agents.Attributes, uMakerAi.Agents.EngineRegistry;

type
  [TToolAttribute('WaitApproval',
                  'Suspende la ejecuci?n hasta que un operador humano apruebe el paso',
                  'Control')]
  TAiWaitApprovalTool = class(TAiToolBase)
  private
    FSuspendReason: string;
    FContextKey:    string;
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string;
                      var AOutput: string); override;
  published
    [TToolParameterAttribute('Raz?n de suspensi?n',
                             'Mensaje legible que explica por qu? se requiere aprobaci?n',
                             'Se requiere aprobaci?n del operador')]
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
begin
  LContext := '';
  if (FContextKey <> '') and Assigned(ANode) and Assigned(ANode.Graph) then
    LContext := ANode.Graph.Blackboard.GetString(FContextKey);

  // Pass-through: cuando se reanude, AInput es la respuesta del humano
  AOutput := AInput;

  LReason := IfThen(FSuspendReason <> '',
                    FSuspendReason,
                    'Waiting for human approval');

  if Assigned(ANode) then
    ANode.Suspend(LReason, LContext);
end;

initialization
  TEngineRegistry.Instance.RegisterTool(
    TAiWaitApprovalTool, 'uMakerAi.Agents.Tools.Approval');

end.
