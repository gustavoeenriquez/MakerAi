// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Agents.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_AgentsHPP
#define uMakerAi_AgentsHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.TypInfo.hpp>
#include <System.Bindings.Evaluator.hpp>
#include <System.Bindings.Helper.hpp>
#include <System.Bindings.Expression.hpp>
#include <System.Bindings.Consts.hpp>
#include <System.JSON.hpp>
#include <System.Math.hpp>
#include <System.Bindings.EvalSys.hpp>
#include <System.Bindings.Factories.hpp>
#include <System.Bindings.EvalProtocol.hpp>
#include <System.Bindings.ObjEval.hpp>
#include <System.Threading.hpp>
#include <System.Rtti.hpp>
#include <System.SyncObjs.hpp>
#include <System.Types.hpp>
#include <System.StrUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Agents
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE TAgentPrintRef;
typedef System::DelphiInterface<TAgentPrintRef> _di_TAgentPrintRef;
class DELPHICLASS TAIBlackboard;
class DELPHICLASS TAiToolBase;
class DELPHICLASS TAiAgentsToolSample;
class DELPHICLASS TAIAgentsBase;
class DELPHICLASS TAIAgentsLink;
class DELPHICLASS TAIAgentsNode;
class DELPHICLASS TAIAgents;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TMsgState : unsigned char { msYes, msNo, msOK, msCancel, msAbort, msRetry, msIgnore, msAll, msNoToAll, msYesToAll, msHelp, msClose };

typedef System::Set<TMsgState, TMsgState::msYes, TMsgState::msClose> TMsgStates;

enum DECLSPEC_DENUM TJoinMode : unsigned char { jmAny, jmAll };

enum DECLSPEC_DENUM TLinkMode : unsigned char { lmFanout, lmConditional, lmManual, lmExpression };

typedef void __fastcall (__closure *TAIAgentsOnPrint)(System::TObject* Sender, System::UnicodeString Value);

__interface TAgentPrintRef  : public System::IInterface 
{
	virtual void __fastcall Invoke(System::TObject* Sender, System::UnicodeString Value) = 0 ;
};

typedef void __fastcall (__closure *TAIAgentsOnEnd)(TAIAgentsNode* Node, System::UnicodeString Value);

typedef void __fastcall (__closure *TAIAgentsNodeOnExecute)(TAIAgentsNode* Node, TAIAgentsNode* BeforeNode, TAIAgentsLink* Link, System::UnicodeString Input, System::UnicodeString &Output);

typedef void __fastcall (__closure *TAIAgentsLinkOnExecute)(TAIAgentsNode* Node, TAIAgentsLink* Link, bool &IsOk, bool &Handled);

typedef void __fastcall (__closure *TAIAgentsOnError)(System::TObject* Sender, TAIAgentsNode* Node, TAIAgentsLink* Link, System::Sysutils::Exception* E, bool &Abort);

typedef void __fastcall (__closure *TAIAgentsOnConfirm)(System::TObject* Sender, TAIAgentsNode* Node, const System::UnicodeString AQuestion, TMsgStates Buttons, System::UnicodeString &AResponse, TMsgState &AModalResult);

typedef void __fastcall (__closure *TAIAgentsOnEnterNode)(System::TObject* Sender, TAIAgentsNode* Node);

typedef void __fastcall (__closure *TAIAgentsOnExitNode)(System::TObject* Sender, TAIAgentsNode* Node);

typedef void __fastcall (__closure *TAIAgentsOnStart)(System::TObject* Sender, const System::UnicodeString Input);

typedef void __fastcall (__closure *TAIAgentsOnFinish)(System::TObject* Sender, const System::UnicodeString Input, const System::UnicodeString Output, System::UnicodeString Status, System::Sysutils::Exception* E);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAIBlackboard : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Syncobjs::TCriticalSection* FLock;
	
protected:
	System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Rtti::TValue>* FData;
	
public:
	__fastcall TAIBlackboard();
	__fastcall virtual ~TAIBlackboard();
	void __fastcall Clear();
	void __fastcall SetValue(const System::UnicodeString AKey, const System::Rtti::TValue &AValue);
	bool __fastcall TryGetValue(const System::UnicodeString AKey, /* out */ System::Rtti::TValue &AValue);
	void __fastcall SetString(const System::UnicodeString AKey, const System::UnicodeString AValue);
	System::UnicodeString __fastcall GetString(const System::UnicodeString AKey, const System::UnicodeString ADefault = System::UnicodeString());
	void __fastcall SetInteger(const System::UnicodeString AKey, int AValue);
	int __fastcall GetInteger(const System::UnicodeString AKey, const int ADefault = 0x0);
	void __fastcall SetBoolean(const System::UnicodeString AKey, bool AValue);
	bool __fastcall GetBoolean(const System::UnicodeString AKey, const bool ADefault = false);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiToolBase : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FDescription;
	System::UnicodeString FID;
	void __fastcall SetDescription(const System::UnicodeString Value);
	void __fastcall SetID(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall Execute(TAIAgentsNode* ANode, const System::UnicodeString AInput, System::UnicodeString &AOutput) = 0 ;
	
public:
	void __fastcall Run(TAIAgentsNode* ANode, const System::UnicodeString AInput, System::UnicodeString &AOutput);
	
__published:
	__property System::UnicodeString Description = {read=FDescription, write=SetDescription};
	__property System::UnicodeString ID = {read=FID, write=SetID};
public:
	/* TComponent.Create */ inline __fastcall virtual TAiToolBase(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiToolBase() { }
	
};


class PASCALIMPLEMENTATION TAiAgentsToolSample : public TAiToolBase
{
	typedef TAiToolBase inherited;
	
protected:
	virtual void __fastcall Execute(TAIAgentsNode* ANode, const System::UnicodeString AInput, System::UnicodeString &AOutput);
public:
	/* TComponent.Create */ inline __fastcall virtual TAiAgentsToolSample(System::Classes::TComponent* AOwner) : TAiToolBase(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiAgentsToolSample() { }
	
};


class PASCALIMPLEMENTATION TAIAgentsBase : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FDescription;
	System::UnicodeString FID;
	void __fastcall SetDescription(const System::UnicodeString Value);
	void __fastcall SetID(const System::UnicodeString Value);
	
__published:
	__property System::UnicodeString Description = {read=FDescription, write=SetDescription};
	__property System::UnicodeString ID = {read=FID, write=SetID};
public:
	/* TComponent.Create */ inline __fastcall virtual TAIAgentsBase(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TAIAgentsBase() { }
	
};


class PASCALIMPLEMENTATION TAIAgentsLink : public TAIAgentsBase
{
	typedef TAIAgentsBase inherited;
	
private:
	TAIAgentsNode* FNextNo;
	TAIAgentsNode* FNextB;
	TAIAgentsNode* FNextC;
	TAIAgentsNode* FNextA;
	TAIAgentsNode* FNextD;
	TAIAgents* FGraph;
	TAIAgentsLinkOnExecute FOnExecute;
	int FNoCycles;
	int FMaxCycles;
	bool FReady;
	TAIAgentsNode* FSourceNode;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,TAIAgentsNode*>* FConditionalTargets;
	TLinkMode FMode;
	System::UnicodeString FConditionalKey;
	System::UnicodeString FManualTargetsKey;
	System::UnicodeString FExpressionA;
	System::UnicodeString FExpressionB;
	System::UnicodeString FExpressionC;
	System::UnicodeString FExpressionD;
	void __fastcall SetNextA(TAIAgentsNode* const Value);
	void __fastcall SetNextB(TAIAgentsNode* const Value);
	void __fastcall SetNextC(TAIAgentsNode* const Value);
	void __fastcall SetNextD(TAIAgentsNode* const Value);
	void __fastcall SetNextNo(TAIAgentsNode* const Value);
	void __fastcall SetGraph(TAIAgents* const Value);
	void __fastcall SetOnExecute(const TAIAgentsLinkOnExecute Value);
	void __fastcall SetMaxCycles(const int Value);
	void __fastcall SetMode(const TLinkMode Value);
	
protected:
	__property bool Ready = {read=FReady, write=FReady, nodefault};
	__property int NoCycles = {read=FNoCycles, write=FNoCycles, nodefault};
	void __fastcall BuildManualTargets(const System::UnicodeString TargetsCSV, /* out */ System::Generics::Collections::TList__1<TAIAgentsNode*>* &Nodes);
	void __fastcall CreateAndQueueTask(TAIAgentsNode* ANodeToExecute, TAIAgentsNode* ASourceNode, TAIAgentsLink* ACurrentLink);
	
public:
	__fastcall virtual TAIAgentsLink(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAIAgentsLink();
	void __fastcall Print(System::UnicodeString Value);
	void __fastcall DoExecute(TAIAgentsNode* Sender);
	void __fastcall AddConditionalTarget(const System::UnicodeString AKey, TAIAgentsNode* ANode);
	
__published:
	__property TAIAgentsNode* NextA = {read=FNextA, write=SetNextA};
	__property TAIAgentsNode* NextB = {read=FNextB, write=SetNextB};
	__property TAIAgentsNode* NextC = {read=FNextC, write=SetNextC};
	__property TAIAgentsNode* NextD = {read=FNextD, write=SetNextD};
	__property TAIAgentsNode* NextNo = {read=FNextNo, write=SetNextNo};
	__property TAIAgents* Graph = {read=FGraph, write=SetGraph};
	__property TAIAgentsLinkOnExecute OnExecute = {read=FOnExecute, write=SetOnExecute};
	__property int MaxCycles = {read=FMaxCycles, write=SetMaxCycles, default=1};
	__property TLinkMode Mode = {read=FMode, write=SetMode, default=0};
	__property System::UnicodeString ConditionalKey = {read=FConditionalKey, write=FConditionalKey};
	__property System::UnicodeString ManualTargetsKey = {read=FManualTargetsKey, write=FManualTargetsKey};
	__property System::UnicodeString ExpressionA = {read=FExpressionA, write=FExpressionA};
	__property System::UnicodeString ExpressionB = {read=FExpressionB, write=FExpressionB};
	__property System::UnicodeString ExpressionC = {read=FExpressionC, write=FExpressionC};
	__property System::UnicodeString ExpressionD = {read=FExpressionD, write=FExpressionD};
};


class PASCALIMPLEMENTATION TAIAgentsNode : public TAIAgentsBase
{
	typedef TAIAgentsBase inherited;
	
private:
	System::UnicodeString FOutput;
	System::UnicodeString FInput;
	TAIAgentsLink* FNext;
	TAIAgents* FGraph;
	System::Generics::Collections::TList__1<TAIAgentsLink*>* FInEdges;
	TAIAgentsNodeOnExecute FOnExecute;
	System::UnicodeString FPromptName;
	System::UnicodeString FMsgError;
	bool FError;
	System::Syncobjs::TCriticalSection* FJoinLock;
	TJoinMode FJoinMode;
	TAiToolBase* FTool;
	System::Generics::Collections::TDictionary__2<TAIAgentsLink*,System::UnicodeString>* FJoinInputs;
	void __fastcall SetInput(const System::UnicodeString Value);
	void __fastcall SetNext(TAIAgentsLink* const Value);
	void __fastcall SetOutput(const System::UnicodeString Value);
	void __fastcall SetGraph(TAIAgents* const Value);
	void __fastcall SetOnExecute(const TAIAgentsNodeOnExecute Value);
	void __fastcall SetPromptName(const System::UnicodeString Value);
	void __fastcall SetJoinMode(const TJoinMode Value);
	void __fastcall SetTool(TAiToolBase* const Value);
	
protected:
	virtual void __fastcall DoExecute(TAIAgentsNode* aBeforeNode, TAIAgentsLink* aLink);
	void __fastcall Reset();
	
public:
	__fastcall virtual TAIAgentsNode(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAIAgentsNode();
	void __fastcall Print(System::UnicodeString Value);
	void __fastcall ForceFinalExecute();
	TMsgState __fastcall RequestConfirmation(const System::UnicodeString AQuestion, TMsgStates Buttons, System::UnicodeString &AResponse);
	bool __fastcall RequestInput(const System::UnicodeString ACaption, const System::UnicodeString APrompt, System::UnicodeString &AValue);
	__property bool Error = {read=FError, nodefault};
	__property System::UnicodeString MsgError = {read=FMsgError};
	
__published:
	__property System::UnicodeString Input = {read=FInput, write=SetInput};
	__property System::UnicodeString Output = {read=FOutput, write=SetOutput};
	__property TAIAgentsLink* Next = {read=FNext, write=SetNext};
	__property TAIAgents* Graph = {read=FGraph, write=SetGraph};
	__property TAIAgentsNodeOnExecute OnExecute = {read=FOnExecute, write=SetOnExecute};
	__property System::UnicodeString PromptName = {read=FPromptName, write=SetPromptName};
	__property TJoinMode JoinMode = {read=FJoinMode, write=SetJoinMode, default=0};
	__property TAiToolBase* Tool = {read=FTool, write=SetTool};
};


class PASCALIMPLEMENTATION TAIAgents : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TAIAgentsNode* FEndNode;
	TAIAgentsNode* FStartNode;
	TAIAgentsOnPrint FOnPrint;
	_di_TAgentPrintRef FOnPrintRef;
	System::Generics::Collections::TList__1<TAIAgentsNode*>* FNodes;
	System::Generics::Collections::TList__1<TAIAgentsLink*>* FLinks;
	TAIAgentsOnEnd FOnEnd;
	TAIAgentsOnError FOnError;
	TAIAgentsOnConfirm FOnConfirm;
	bool FBusy;
	bool FAbort;
	TAIBlackboard* FBlackboard;
	bool FCompiled;
	System::Generics::Collections::TList__1<System::Threading::_di_ITask>* FActiveTasks;
	System::Syncobjs::TCriticalSection* FActiveTasksLock;
	TAIAgentsOnExitNode FOnExitNode;
	TAIAgentsOnEnterNode FOnEnterNode;
	int FMaxConcurrentTasks;
	unsigned FTimeoutMs;
	TAIAgentsOnFinish FOnFinish;
	TAIAgentsOnStart FOnStart;
	System::UnicodeString FDescription;
	void __fastcall SetMaxConcurrentTasks(const int Value);
	void __fastcall SetEndNode(TAIAgentsNode* const Value);
	void __fastcall SetStartNode(TAIAgentsNode* const Value);
	void __fastcall SetOnPrint(const TAIAgentsOnPrint Value);
	void __fastcall SetOnEnd(const TAIAgentsOnEnd Value);
	void __fastcall SetOnError(const TAIAgentsOnError Value);
	void __fastcall SetOnConfirm(const TAIAgentsOnConfirm Value);
	void __fastcall SetOnEnterNode(const TAIAgentsOnEnterNode Value);
	void __fastcall SetOnExitNode(const TAIAgentsOnExitNode Value);
	void __fastcall SetOnFinish(const TAIAgentsOnFinish Value);
	void __fastcall SetOnStart(const TAIAgentsOnStart Value);
	void __fastcall SetDescription(const System::UnicodeString Value);
	
protected:
	System::Threading::TThreadPool* FThreadPool;
	void __fastcall DoPrint(System::TObject* Sender, System::UnicodeString Value);
	void __fastcall DoPrintFromRef(System::TObject* Sender, System::UnicodeString Value);
	void __fastcall AddComponentToList(TAIAgentsBase* AComponent);
	void __fastcall RemoveComponentFromList(TAIAgentsBase* AComponent);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TAIAgents(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAIAgents();
	void __fastcall Abort();
	System::Threading::_di_ITask __fastcall Run(System::UnicodeString Msg);
	TAIAgentsNode* __fastcall FindNode(const System::UnicodeString AName);
	void __fastcall DoError(TAIAgentsNode* Node, TAIAgentsLink* Link, System::Sysutils::Exception* E);
	TMsgState __fastcall DoConfirm(TAIAgentsNode* Node, const System::UnicodeString AQuestion, TMsgStates Buttons, System::UnicodeString &AResponse);
	void __fastcall SetOnPrintEvent(const _di_TAgentPrintRef APrintProc);
	void __fastcall ClearGraph();
	TAIAgents* __fastcall AddNode(const System::UnicodeString AName, TAIAgentsNodeOnExecute AExecuteProc);
	TAIAgents* __fastcall AddEdge(const System::UnicodeString AStartNodeName, const System::UnicodeString AEndNodeName);
	TAIAgents* __fastcall AddConditionalEdge(const System::UnicodeString AStartNodeName, const System::UnicodeString AConditionalLinkName, System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>* AConditionalTargets);
	TAIAgents* __fastcall SetEntryPoint(const System::UnicodeString ANodeName);
	TAIAgents* __fastcall SetFinishPoint(const System::UnicodeString ANodeName);
	void __fastcall Compile();
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveStateToStream(System::Classes::TStream* AStream);
	void __fastcall LoadStateFromStream(System::Classes::TStream* AStream);
	__property bool Busy = {read=FBusy, nodefault};
	__property TAIBlackboard* Blackboard = {read=FBlackboard};
	
__published:
	__property TAIAgentsNode* StartNode = {read=FStartNode, write=SetStartNode};
	__property TAIAgentsNode* EndNode = {read=FEndNode, write=SetEndNode};
	__property TAIAgentsOnPrint OnPrint = {read=FOnPrint, write=SetOnPrint};
	__property TAIAgentsOnEnd OnEnd = {read=FOnEnd, write=SetOnEnd};
	__property TAIAgentsOnError OnError = {read=FOnError, write=SetOnError};
	__property TAIAgentsOnConfirm OnConfirm = {read=FOnConfirm, write=SetOnConfirm};
	__property TAIAgentsOnEnterNode OnEnterNode = {read=FOnEnterNode, write=SetOnEnterNode};
	__property TAIAgentsOnExitNode OnExitNode = {read=FOnExitNode, write=SetOnExitNode};
	__property int MaxConcurrentTasks = {read=FMaxConcurrentTasks, write=SetMaxConcurrentTasks, default=4};
	__property TAIAgentsOnStart OnStart = {read=FOnStart, write=SetOnStart};
	__property TAIAgentsOnFinish OnFinish = {read=FOnFinish, write=SetOnFinish};
	__property unsigned TimeoutMs = {read=FTimeoutMs, write=FTimeoutMs, default=60000};
	__property System::UnicodeString Description = {read=FDescription, write=SetDescription};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
extern DELPHI_PACKAGE bool __fastcall EvalCondition(const System::UnicodeString Expr, System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Rtti::TValue>* Vars);
}	/* namespace Agents */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_AGENTS)
using namespace Umakerai::Agents;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_AgentsHPP
