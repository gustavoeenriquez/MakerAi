// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.RAG.Graph.Core.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_RAG_Graph_CoreHPP
#define uMakerAi_RAG_Graph_CoreHPP

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
#include <System.JSON.hpp>
#include <System.Variants.hpp>
#include <System.JSON.Writers.hpp>
#include <System.JSON.Types.hpp>
#include <System.IOUtils.hpp>
#include <System.Rtti.hpp>
#include <System.StrUtils.hpp>
#include <Xml.XMLDoc.hpp>
#include <Xml.XMLIntf.hpp>
#include <Xml.xmldom.hpp>
#include <uMakerAi.Embeddings.core.hpp>
#include <uMakerAi.Embeddings.hpp>
#include <uMakerAi.RAG.Vectors.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Rag
{
namespace Graph
{
namespace Core
{
//-- forward type declarations -----------------------------------------------
struct TQueryStep;
struct TQueryPlan;
class DELPHICLASS TMatchNodePattern;
class DELPHICLASS TMatchEdgePattern;
class DELPHICLASS TMatchClause;
class DELPHICLASS TGraphMatchQuery;
class DELPHICLASS TStringWrapper;
struct TNodeDataRecord;
struct TEdgeDataRecord;
class DELPHICLASS TAiRagGraphDriverBase;
class DELPHICLASS TAiRagGraphNode;
class DELPHICLASS TAiRagGraphEdge;
class DELPHICLASS TAiRagGraph;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDegreeType : unsigned char { dtIn, dtOut, dtTotal };

enum DECLSPEC_DENUM TGraphExportFormat : unsigned char { gefDOT, gefGraphML, gefGraphMkai };

enum DECLSPEC_DENUM TMergeStrategy : unsigned char { msAddNewOnly, msOverwrite, msKeepExisting };

struct DECLSPEC_DRECORD TQueryStep
{
public:
	System::UnicodeString SourceVariable;
	System::UnicodeString EdgeLabel;
	System::UnicodeString TargetVariable;
	System::UnicodeString TargetNodeLabel;
	bool IsReversed;
};


struct DECLSPEC_DRECORD TQueryPlan
{
public:
	System::UnicodeString AnchorPrompt;
	System::UnicodeString AnchorVariable;
	System::DynamicArray<TQueryStep> Steps;
	System::UnicodeString ResultVariable;
};


enum DECLSPEC_DENUM TGraphDirection : unsigned char { gdOutgoing, gdIncoming, gdBoth };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMatchNodePattern : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString Variable;
	System::UnicodeString NodeLabel;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Variant>* Properties;
	__fastcall TMatchNodePattern();
	__fastcall virtual ~TMatchNodePattern();
	bool __fastcall Matches(TAiRagGraphNode* ANode);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMatchEdgePattern : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString Variable;
	System::UnicodeString EdgeLabel;
	TGraphDirection Direction;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Variant>* Properties;
	__fastcall TMatchEdgePattern();
	__fastcall virtual ~TMatchEdgePattern();
	bool __fastcall Matches(TAiRagGraphEdge* AEdge, TGraphDirection AActualDirection);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMatchClause : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString SourceNodeVar;
	TMatchEdgePattern* EdgePattern;
	System::UnicodeString TargetNodeVar;
	__fastcall TMatchClause(System::UnicodeString ASourceNodeVar, TMatchEdgePattern* AEdgePattern, System::UnicodeString ATargetNodeVar);
	__fastcall virtual ~TMatchClause();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGraphMatchQuery : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Generics::Collections::TObjectList__1<TMatchNodePattern*>* FNodePatterns;
	System::Generics::Collections::TObjectList__1<TMatchClause*>* FMatchClauses;
	TMatchNodePattern* __fastcall GetNodePatternByVariable(const System::UnicodeString AVar);
	
public:
	__fastcall TGraphMatchQuery();
	__fastcall virtual ~TGraphMatchQuery();
	void __fastcall AddNodePattern(TMatchNodePattern* ANodePattern);
	void __fastcall AddMatchClause(TMatchClause* AClause);
	__property System::Generics::Collections::TObjectList__1<TMatchNodePattern*>* NodePatterns = {read=FNodePatterns};
	__property System::Generics::Collections::TObjectList__1<TMatchClause*>* MatchClauses = {read=FMatchClauses};
	__property TMatchNodePattern* NodePatternByVariable[const System::UnicodeString AVar] = {read=GetNodePatternByVariable};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringWrapper : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString Value;
	__fastcall TStringWrapper(const System::UnicodeString AValue);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStringWrapper() { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TNodeDataRecord
{
public:
	System::UnicodeString ID;
	System::UnicodeString NodeLabel;
	System::UnicodeString Name;
	System::UnicodeString PropertiesJSON;
	System::UnicodeString EmbeddingStr;
};


struct DECLSPEC_DRECORD TEdgeDataRecord
{
public:
	System::UnicodeString ID;
	System::UnicodeString EdgeLabel;
	System::UnicodeString Name;
	System::UnicodeString SourceNodeID;
	System::UnicodeString TargetNodeID;
	double Weight;
	System::UnicodeString PropertiesJSON;
	System::UnicodeString EmbeddingStr;
};


class PASCALIMPLEMENTATION TAiRagGraphDriverBase : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TAiRagGraph* FGraph;
	
protected:
	virtual bool __fastcall FindNodeDataByID(const System::UnicodeString ANodeID, /* out */ TNodeDataRecord &ANodeData) = 0 ;
	virtual bool __fastcall FindEdgeDataByID(const System::UnicodeString AEdgeID, /* out */ TEdgeDataRecord &AEdgeData) = 0 ;
	virtual void __fastcall GetNodeEdges(TAiRagGraphNode* ANode) = 0 ;
	virtual void __fastcall AddNode(TAiRagGraphNode* ANode) = 0 ;
	virtual void __fastcall AddEdge(TAiRagGraphEdge* AEdge) = 0 ;
	virtual void __fastcall DeleteNode(const System::UnicodeString ANodeID) = 0 ;
	virtual void __fastcall DeleteEdge(const System::UnicodeString AEdgeID) = 0 ;
	virtual System::DynamicArray<System::UnicodeString> __fastcall GetUniqueNodeLabels() = 0 ;
	virtual System::DynamicArray<System::UnicodeString> __fastcall GetUniqueEdgeLabels() = 0 ;
	virtual TAiRagGraphNode* __fastcall FindNodeByName(const System::UnicodeString AName, const System::UnicodeString ANodeLabel) = 0 ;
	virtual System::DynamicArray<TAiRagGraphNode*> __fastcall FindNodesByLabel(const System::UnicodeString ALabel) = 0 ;
	virtual System::DynamicArray<TAiRagGraphNode*> __fastcall FindNodesByProperty(const System::UnicodeString AKey, const System::Variant &AValue) = 0 ;
	virtual System::DynamicArray<System::UnicodeString> __fastcall FindNodeNamesByLabel(const System::UnicodeString ANodeLabel, const System::UnicodeString ASearchText, int ALimit) = 0 ;
	virtual System::DynamicArray<TAiRagGraphNode*> __fastcall SearchNodes(const System::UnicodeString APrompt, int ADepth, int ALimit, double APrecision) = 0 ;
	virtual System::DynamicArray<TAiRagGraphNode*> __fastcall Query(const TQueryPlan &APlan, int ADepth, int ALimit, double APrecision) = 0 ;
	__property TAiRagGraph* Graph = {read=FGraph};
	
public:
	System::UnicodeString __fastcall EmbeddingToString(const Umakerai::Embeddings::Core::TAiEmbeddingData AData);
	System::UnicodeString __fastcall PropertiesToJSONString(System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Variant>* const AProperties);
	void __fastcall AssignToGraph(TAiRagGraph* AGraph);
public:
	/* TComponent.Create */ inline __fastcall virtual TAiRagGraphDriverBase(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiRagGraphDriverBase() { }
	
};


class PASCALIMPLEMENTATION TAiRagGraphNode : public Umakerai::Rag::Vectors::TAiEmbeddingNode
{
	typedef Umakerai::Rag::Vectors::TAiEmbeddingNode inherited;
	
private:
	System::UnicodeString FNodeLabel;
	System::UnicodeString FName;
	System::Generics::Collections::TObjectList__1<TAiRagGraphEdge*>* FInternalOutgoingEdges;
	System::Generics::Collections::TObjectList__1<TAiRagGraphEdge*>* FInternalIncomingEdges;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Variant>* FProperties;
	TAiRagGraph* FOwnerGraph;
	System::UnicodeString FID;
	bool FEdgesLoaded;
	System::Generics::Collections::TObjectList__1<TAiRagGraphEdge*>* __fastcall GetIncomingEdges();
	System::Generics::Collections::TObjectList__1<TAiRagGraphEdge*>* __fastcall GetOutgoingEdges();
	
protected:
	void __fastcall AddOutgoingEdge(TAiRagGraphEdge* AEdge);
	void __fastcall AddIncomingEdge(TAiRagGraphEdge* AEdge);
	void __fastcall RemoveOutgoingEdge(TAiRagGraphEdge* AEdge);
	void __fastcall RemoveIncomingEdge(TAiRagGraphEdge* AEdge);
	
public:
	__fastcall TAiRagGraphNode(TAiRagGraph* AOwnerGraph, int ADim);
	__fastcall virtual ~TAiRagGraphNode();
	System::Json::TJSONObject* __fastcall PropertiesToJSON();
	void __fastcall EnsureEdgesAreLoaded();
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString NodeLabel = {read=FNodeLabel, write=FNodeLabel};
	__property System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Variant>* Properties = {read=FProperties};
	__property System::Generics::Collections::TObjectList__1<TAiRagGraphEdge*>* OutgoingEdges = {read=GetOutgoingEdges};
	__property System::Generics::Collections::TObjectList__1<TAiRagGraphEdge*>* IncomingEdges = {read=GetIncomingEdges};
	__property TAiRagGraph* OwnerGraph = {read=FOwnerGraph};
};


class PASCALIMPLEMENTATION TAiRagGraphEdge : public Umakerai::Rag::Vectors::TAiEmbeddingNode
{
	typedef Umakerai::Rag::Vectors::TAiEmbeddingNode inherited;
	
private:
	System::UnicodeString FEdgeLabel;
	System::UnicodeString FName;
	TAiRagGraphNode* FFromNode;
	TAiRagGraphNode* FToNode;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Variant>* FProperties;
	TAiRagGraph* FOwnerGraph;
	System::UnicodeString FID;
	double FWeight;
	
public:
	__fastcall TAiRagGraphEdge(TAiRagGraph* AOwnerGraph, int ADim);
	__fastcall virtual ~TAiRagGraphEdge();
	System::Json::TJSONObject* __fastcall PropertiesToJSON();
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString EdgeLabel = {read=FEdgeLabel, write=FEdgeLabel};
	__property TAiRagGraphNode* FromNode = {read=FFromNode, write=FFromNode};
	__property TAiRagGraphNode* ToNode = {read=FToNode, write=FToNode};
	__property double Weight = {read=FWeight, write=FWeight};
	__property System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Variant>* Properties = {read=FProperties};
	__property TAiRagGraph* OwnerGraph = {read=FOwnerGraph};
};


class PASCALIMPLEMENTATION TAiRagGraph : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Umakerai::Rag::Vectors::TAiRAGVector* FNodes;
	Umakerai::Rag::Vectors::TAiRAGVector* FEdges;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiRagGraphNode*>* FNodeRegistry;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiRagGraphEdge*>* FEdgeRegistry;
	Umakerai::Embeddings::Core::TAiEmbeddingsCore* FEmbeddings;
	Umakerai::Rag::Vectors::TAiRagIndexType FInMemoryIndexType;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Generics::Collections::TList__1<TAiRagGraphNode*>*>* FNodeLabelIndex;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiRagGraphNode*>* FNodeNameIndex;
	int FUpdateCount;
	TAiRagGraphDriverBase* FDriver;
	int __fastcall GetNodeCount();
	int __fastcall GetEdgeCount();
	Umakerai::Rag::Vectors::TAiRAGVector* __fastcall GetNodesRAGVector();
	Umakerai::Rag::Vectors::TAiRAGVector* __fastcall GetEdgesRAGVector();
	void __fastcall SetInMemoryIndexType(const Umakerai::Rag::Vectors::TAiRagIndexType Value);
	void __fastcall RebuildIndexes();
	void __fastcall SetDriver(TAiRagGraphDriverBase* const Value);
	
protected:
	void __fastcall UnregisterNode(TAiRagGraphNode* ANode);
	void __fastcall UnregisterEdge(TAiRagGraphEdge* AEdge);
	System::DynamicArray<TAiRagGraphNode*> __fastcall ExpandNodeList(System::Generics::Collections::TList__1<TAiRagGraphNode*>* AInitialNodes, int ADepth);
	System::UnicodeString __fastcall GetContextualizedText(System::DynamicArray<TAiRagGraphNode*> ASubgraphNodes);
	TAiRagGraphNode* __fastcall InternalAddNode(TAiRagGraphNode* ANode, bool AShouldPersist);
	TAiRagGraphEdge* __fastcall InternalAddEdge(TAiRagGraphEdge* AEdge, bool AShouldPersist);
	
public:
	__fastcall virtual TAiRagGraph(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiRagGraph();
	TAiRagGraphNode* __fastcall InternalHydrateNode(const TNodeDataRecord &ANodeData);
	TAiRagGraphEdge* __fastcall InternalHydrateEdge(const TEdgeDataRecord &AEdgeData);
	TAiRagGraphNode* __fastcall AddNode(System::UnicodeString AID, System::UnicodeString ALabel, System::UnicodeString AName)/* overload */;
	TAiRagGraphNode* __fastcall AddNode(TAiRagGraphNode* ANode)/* overload */;
	TAiRagGraphEdge* __fastcall AddEdge(TAiRagGraphNode* AFromNode, TAiRagGraphNode* AToNode, System::UnicodeString AID, System::UnicodeString ALabel, System::UnicodeString AName)/* overload */;
	TAiRagGraphEdge* __fastcall AddEdge(TAiRagGraphNode* AFromNode, TAiRagGraphNode* AToNode, System::UnicodeString AID, System::UnicodeString ALabel, System::UnicodeString AName, double AWeight)/* overload */;
	TAiRagGraphEdge* __fastcall AddEdge(TAiRagGraphEdge* AEdge)/* overload */;
	void __fastcall DeleteNode(TAiRagGraphNode* ANode)/* overload */;
	void __fastcall DeleteNode(System::UnicodeString AID)/* overload */;
	void __fastcall DeleteEdge(TAiRagGraphEdge* AEdge)/* overload */;
	void __fastcall DeleteEdge(System::UnicodeString AID)/* overload */;
	void __fastcall Clear();
	TAiRagGraphNode* __fastcall FindNodeByID(System::UnicodeString AID);
	TAiRagGraphEdge* __fastcall FindEdgeByID(System::UnicodeString AID);
	System::DynamicArray<TAiRagGraphNode*> __fastcall FindNodesByLabel(System::UnicodeString ALabel);
	TAiRagGraphEdge* __fastcall FindEdge(TAiRagGraphNode* AFromNode, TAiRagGraphNode* AToNode, System::UnicodeString AEdgeLabel);
	TAiRagGraphNode* __fastcall FindNodeByName(System::UnicodeString AName, System::UnicodeString ANodeLabel);
	void __fastcall SaveToStream(System::Classes::TStream* AStream)/* overload */;
	void __fastcall SaveToStream(System::Classes::TStream* AStream, bool aFull)/* overload */;
	void __fastcall SaveToDot(const System::UnicodeString AFileName);
	void __fastcall SaveToMakerAi(const System::UnicodeString AFileName, const bool aFull = true);
	void __fastcall SaveToGraphML(const System::UnicodeString AFileName);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	void __fastcall SaveToFile(const System::UnicodeString AFileName, TGraphExportFormat AFormat, bool aFull = true)/* overload */;
	void __fastcall SaveToFile(const System::UnicodeString AFileName, bool aFull)/* overload */;
	System::DynamicArray<System::TObject*> __fastcall GetShortestPath(TAiRagGraphNode* AStartNode, TAiRagGraphNode* AEndNode);
	System::DynamicArray<TAiRagGraphNode*> __fastcall GetNodesByDegree(int ATop = 0xa, TDegreeType ADegreeType = (TDegreeType)(0x2));
	double __fastcall GetClosenessCentrality(TAiRagGraphNode* ANode);
	System::DynamicArray<TAiRagGraphNode*> __fastcall Search(const System::UnicodeString APrompt, const int ADepth = 0x0, int ALimit = 0x5, const double APrecision = 5.000000E-01);
	System::UnicodeString __fastcall SearchText(const System::UnicodeString APrompt, int ADepth = 0x0, bool ShowProperties = false, const int ALimit = 0x3, const double APrecision = 5.000000E-01);
	System::DynamicArray<TAiRagGraphNode*> __fastcall Query(const TQueryPlan &APlan, int ADepth = 0x0, const int ALimit = 0x5, const double APrecision = 5.000000E-01);
	System::DynamicArray<System::DynamicArray<System::TObject*> > __fastcall GetAllShortestPaths(TAiRagGraphNode* AStartNode, TAiRagGraphNode* AEndNode);
	System::DynamicArray<System::Generics::Collections::TDictionary__2<System::UnicodeString,System::TObject*>*> __fastcall Match(TGraphMatchQuery* AQuery, int ADepth = 0x0);
	System::DynamicArray<System::UnicodeString> __fastcall GetUniqueNodeLabels();
	System::DynamicArray<System::UnicodeString> __fastcall GetUniqueEdgeLabels();
	System::DynamicArray<TAiRagGraphNode*> __fastcall FindNodesByProperty(const System::UnicodeString AKey, const System::Variant &AValue);
	System::DynamicArray<TAiRagGraphNode*> __fastcall GetNeighbors(TAiRagGraphNode* ANode, TGraphDirection ADirection = (TGraphDirection)(0x0));
	int __fastcall CountNodesByLabel(const System::UnicodeString ALabel);
	int __fastcall CountEdgesByLabel(const System::UnicodeString ALabel);
	TAiRagGraph* __fastcall ExtractSubgraph(System::DynamicArray<TAiRagGraphNode*> ANodes);
	void __fastcall MergeNodes(TAiRagGraphNode* ASurvivingNode, TAiRagGraphNode* ASubsumedNode, TMergeStrategy APropertyMergeStrategy = (TMergeStrategy)(0x0));
	System::DynamicArray<System::UnicodeString> __fastcall FindNodeNamesByLabel(const System::UnicodeString ANodeLabel, const System::UnicodeString ASearchText, int ALimit = 0xa);
	bool __fastcall EdgeExistsInMemory(const System::UnicodeString AEdgeID);
	__property int NodeCount = {read=GetNodeCount, nodefault};
	__property int EdgeCount = {read=GetEdgeCount, nodefault};
	__property Umakerai::Rag::Vectors::TAiRAGVector* Nodes = {read=GetNodesRAGVector};
	__property Umakerai::Rag::Vectors::TAiRAGVector* Edges = {read=GetEdgesRAGVector};
	
__published:
	__property Umakerai::Embeddings::Core::TAiEmbeddingsCore* Embeddings = {read=FEmbeddings, write=FEmbeddings};
	__property Umakerai::Rag::Vectors::TAiRagIndexType InMemoryIndexType = {read=FInMemoryIndexType, write=SetInMemoryIndexType, nodefault};
	__property TAiRagGraphDriverBase* Driver = {read=FDriver, write=SetDriver};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
extern DELPHI_PACKAGE System::Variant __fastcall JSONValueToVariant(System::Json::TJSONValue* AJsonValue);
extern DELPHI_PACKAGE System::Json::TJSONValue* __fastcall VariantToJSONValue(const System::Variant &AValue);
extern DELPHI_PACKAGE Umakerai::Embeddings::Core::TAiEmbeddingData __fastcall StringToEmbedding(const System::UnicodeString AVectorString);
extern DELPHI_PACKAGE void __fastcall JSONStringToProperties(const System::UnicodeString AJSONString, System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Variant>* const AProperties);
}	/* namespace Core */
}	/* namespace Graph */
}	/* namespace Rag */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_RAG_GRAPH_CORE)
using namespace Umakerai::Rag::Graph::Core;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_RAG_GRAPH)
using namespace Umakerai::Rag::Graph;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_RAG)
using namespace Umakerai::Rag;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_RAG_Graph_CoreHPP
