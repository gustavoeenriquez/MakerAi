// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.RAG.Graph.Builder.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_RAG_Graph_BuilderHPP
#define uMakerAi_RAG_Graph_BuilderHPP

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
#include <System.JSON.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Variants.hpp>
#include <uMakerAi.RAG.Vectors.hpp>
#include <uMakerAi.Embeddings.core.hpp>
#include <uMakerAi.Embeddings.hpp>
#include <uMakerAi.RAG.Graph.Core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Rag
{
namespace Graph
{
namespace Builder
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiRagGraphBuilder;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TAiRagGraphBuilder : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Umakerai::Rag::Graph::Core::TAiRagGraph* FGraph;
	Umakerai::Embeddings::Core::TAiEmbeddingsCore* FEmbeddings;
	
protected:
	void __fastcall MergeNodeProperties(Umakerai::Rag::Graph::Core::TAiRagGraphNode* ANode, System::Json::TJSONObject* ANewProperties, Umakerai::Rag::Graph::Core::TMergeStrategy AStrategy);
	void __fastcall MergeEdgeProperties(Umakerai::Rag::Graph::Core::TAiRagGraphEdge* AEdge, System::Json::TJSONObject* ANewProperties, Umakerai::Rag::Graph::Core::TMergeStrategy AStrategy);
	Umakerai::Rag::Graph::Core::TAiRagGraphNode* __fastcall GetOrCreateNode(System::Json::TJSONObject* ANodeObject, Umakerai::Rag::Graph::Core::TMergeStrategy AMergeStrategy);
	System::UnicodeString __fastcall GenerateTextForEmbedding(System::UnicodeString AName, System::UnicodeString ANodeLabel, System::Json::TJSONObject* AProperties, System::UnicodeString AAdditionalText = System::UnicodeString());
	
public:
	__fastcall virtual TAiRagGraphBuilder(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiRagGraphBuilder();
	void __fastcall Process(System::UnicodeString AJsonTripletArray, Umakerai::Rag::Graph::Core::TMergeStrategy AMergeStrategy = (Umakerai::Rag::Graph::Core::TMergeStrategy)(0x0));
	Umakerai::Rag::Graph::Core::TAiRagGraphNode* __fastcall FindExistingNode(System::UnicodeString AName, System::UnicodeString ALabel);
	
__published:
	__property Umakerai::Rag::Graph::Core::TAiRagGraph* Graph = {read=FGraph, write=FGraph};
	__property Umakerai::Embeddings::Core::TAiEmbeddingsCore* Embeddings = {read=FEmbeddings, write=FEmbeddings};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Builder */
}	/* namespace Graph */
}	/* namespace Rag */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_RAG_GRAPH_BUILDER)
using namespace Umakerai::Rag::Graph::Builder;
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
#endif	// uMakerAi_RAG_Graph_BuilderHPP
