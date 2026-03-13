// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Herramienta de suspension human-in-the-loop.
// Cuando un nodo ejecuta esta tool, la ejecucion del grafo se suspende
// y se dispara el evento TAIAgentManager.OnSuspend.
// La ejecucion se reanuda llamando a TAIAgentManager.ResumeThread.
//
// Uso:
//   LNode.Tool := TAiWaitApprovalTool.Create(LNode);
//   TAiWaitApprovalTool(LNode.Tool).SuspendReason := 'Requiere aprobacion';
//   TAiWaitApprovalTool(LNode.Tool).ContextKey    := 'resultado_previo';

unit uMakerAi.Agents.Tools.Approval;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  uMakerAi.Agents,
  uMakerAi.Agents.EngineRegistry;

type
  TAiWaitApprovalTool = class(TAiToolBase)
  private
    FSuspendReason: string;
    FContextKey:    string;
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string;
                      var AOutput: string); override;
  published
    property SuspendReason: string read FSuspendReason write FSuspendReason;
    property ContextKey:    string read FContextKey    write FContextKey;
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

  if FSuspendReason <> '' then
    LReason := FSuspendReason
  else
    LReason := 'Waiting for human approval';

  if Assigned(ANode) then
    ANode.Suspend(LReason, LContext);
end;

initialization
  TEngineRegistry.Instance.RegisterTool(
    TAiWaitApprovalTool,
    'uMakerAi.Agents.Tools.Approval',
    'WaitApproval',
    'Suspende la ejecucion hasta que un operador humano apruebe el paso',
    'Control');

end.
