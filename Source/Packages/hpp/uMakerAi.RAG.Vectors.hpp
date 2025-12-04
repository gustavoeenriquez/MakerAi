// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.RAG.Vectors.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_RAG_VectorsHPP
#define uMakerAi_RAG_VectorsHPP

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
#include <System.Math.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Classes.hpp>
#include <System.JSON.hpp>
#include <REST.Json.hpp>
#include <System.NetEncoding.hpp>
#include <uMakerAi.Embeddings.core.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Rag
{
namespace Vectors
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiEmbeddingMetaData;
class DELPHICLASS TAiEmbeddingNode;
class DELPHICLASS TAIEmbeddingIndex;
class DELPHICLASS TAIBasicEmbeddingIndex;
class DELPHICLASS TAIEuclideanDistanceIndex;
class DELPHICLASS THNSWNode;
class DELPHICLASS THNSWIndex;
class DELPHICLASS TAiRAGVector;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAiRagIndexType : unsigned char { TAIBasicIndex, TAIHNSWIndex, TAIEuclideanIndex };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiEmbeddingMetaData : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::TObject* FTagObject;
	System::Classes::TStrings* FData;
	System::UnicodeString FTagString;
	void __fastcall SetData(System::Classes::TStrings* const Value);
	void __fastcall SetTagObject(System::TObject* const Value);
	void __fastcall SetFTagString(const System::UnicodeString Value);
	
public:
	__fastcall TAiEmbeddingMetaData();
	__fastcall virtual ~TAiEmbeddingMetaData();
	__property System::Classes::TStrings* Data = {read=FData, write=SetData};
	__property System::TObject* TagObject = {read=FTagObject, write=SetTagObject};
	__property System::UnicodeString TagString = {read=FTagString, write=SetFTagString};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiEmbeddingNode : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Umakerai::Embeddings::Core::TAiEmbeddingData FData;
	int FDim;
	System::TObject* FTagObject;
	int FTag;
	System::UnicodeString FText;
	System::Json::TJSONObject* FjData;
	double FIdx;
	int FOrden;
	System::UnicodeString FModel;
	void __fastcall SetData(const Umakerai::Embeddings::Core::TAiEmbeddingData Value);
	__classmethod double __fastcall DotProduct(TAiEmbeddingNode* const A, TAiEmbeddingNode* const B);
	__classmethod double __fastcall Magnitude(TAiEmbeddingNode* const A);
	void __fastcall SetjData(System::Json::TJSONObject* const Value);
	void __fastcall SetTag(const int Value);
	void __fastcall SetTagObject(System::TObject* const Value);
	void __fastcall SetText(const System::UnicodeString Value);
	void __fastcall SetIdx(const double Value);
	void __fastcall SetOrden(const int Value);
	void __fastcall SetModel(const System::UnicodeString Value);
	
public:
	__fastcall TAiEmbeddingNode(int aDim);
	__fastcall virtual ~TAiEmbeddingNode();
	__classmethod double __fastcall CosineSimilarity(TAiEmbeddingNode* const A, TAiEmbeddingNode* const B);
	__classmethod System::Json::TJSONArray* __fastcall ToJsonArray(TAiEmbeddingNode* Val)/* overload */;
	__classmethod TAiEmbeddingNode* __fastcall FromJSON(System::Json::TJSONObject* AJSONObject);
	System::Json::TJSONArray* __fastcall ToJsonArray()/* overload */;
	System::Json::TJSONObject* __fastcall ToJSON();
	void __fastcall SetDataLength(int aDim);
	__property Umakerai::Embeddings::Core::TAiEmbeddingData Data = {read=FData, write=SetData};
	__property System::Json::TJSONObject* jData = {read=FjData, write=SetjData};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property System::TObject* TagObject = {read=FTagObject, write=SetTagObject};
	__property int Tag = {read=FTag, write=SetTag, nodefault};
	__property int Dim = {read=FDim, nodefault};
	__property double Idx = {read=FIdx, write=SetIdx};
	__property int Orden = {read=FOrden, write=SetOrden, nodefault};
	__property System::UnicodeString Model = {read=FModel, write=SetModel};
};


typedef void __fastcall (__closure *TOnDataVecAddItem)(System::TObject* Sender, TAiEmbeddingNode* aItem, TAiEmbeddingMetaData* MetaData, bool &Handled);

typedef void __fastcall (__closure *TOnDataVecSearch)(System::TObject* Sender, TAiEmbeddingNode* Target, int aLimit, double aPrecision, TAiRAGVector* &aDataVec, bool &Handled);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAIEmbeddingIndex : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TAiRAGVector* FDataVec;
	bool FActive;
	void __fastcall SetDataVec(TAiRAGVector* const Value);
	
public:
	__fastcall virtual TAIEmbeddingIndex();
	__fastcall virtual ~TAIEmbeddingIndex();
	virtual void __fastcall BuildIndex(TAiRAGVector* Points);
	virtual int __fastcall Add(TAiEmbeddingNode* Point);
	virtual TAiRAGVector* __fastcall Search(TAiEmbeddingNode* Target, int aLimit, double aPrecision);
	virtual bool __fastcall Connect(System::UnicodeString aHost, System::UnicodeString aPort, System::UnicodeString aLogin, System::UnicodeString aPassword);
	__property TAiRAGVector* DataVec = {read=FDataVec, write=SetDataVec};
	__property bool Active = {read=FActive, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAIBasicEmbeddingIndex : public TAIEmbeddingIndex
{
	typedef TAIEmbeddingIndex inherited;
	
public:
	__fastcall virtual TAIBasicEmbeddingIndex();
	__fastcall virtual ~TAIBasicEmbeddingIndex();
	virtual void __fastcall BuildIndex(TAiRAGVector* Points);
	virtual TAiRAGVector* __fastcall Search(TAiEmbeddingNode* Target, int aLimit, double aPrecision);
};

#pragma pack(pop)

typedef System::Generics::Collections::TPair__2<double,TAiEmbeddingNode*> TL2Pair;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAIEuclideanDistanceIndex : public TAIEmbeddingIndex
{
	typedef TAIEmbeddingIndex inherited;
	
public:
	__fastcall virtual TAIEuclideanDistanceIndex();
	__fastcall virtual ~TAIEuclideanDistanceIndex();
	virtual void __fastcall BuildIndex(TAiRAGVector* Points);
	virtual TAiRAGVector* __fastcall Search(TAiEmbeddingNode* Target, int aLimit, double aPrecision);
};

#pragma pack(pop)

typedef System::DynamicArray<System::Generics::Collections::TList__1<int>*> TConnListArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION THNSWNode : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FID;
	TAiEmbeddingNode* FVector;
	TConnListArray FConnections;
	
public:
	__fastcall THNSWNode(int aID, TAiEmbeddingNode* aVector, int aNumLevels);
	__fastcall virtual ~THNSWNode();
	__property int ID = {read=FID, nodefault};
	__property TAiEmbeddingNode* Vector = {read=FVector};
	__property TConnListArray Connections = {read=FConnections};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION THNSWIndex : public TAIEmbeddingIndex
{
	typedef TAIEmbeddingIndex inherited;
	
private:
	System::Generics::Collections::TDictionary__2<int,THNSWNode*>* FNodes;
	int FEntryPoint;
	int FMaxLevel;
	double FLevelMult;
	int FEfConstruction;
	int FMaxConnections;
	int __fastcall GetRandomLevel();
	void __fastcall InsertConnection(THNSWNode* Node, int Level, int TargetID);
	System::Generics::Collections::TList__1<int>* __fastcall SearchLayer(TAiEmbeddingNode* Query, int EntryPoint, int Level, int Ef);
	
public:
	__fastcall virtual THNSWIndex();
	__fastcall virtual ~THNSWIndex();
	virtual void __fastcall BuildIndex(TAiRAGVector* Points);
	virtual int __fastcall Add(TAiEmbeddingNode* Point);
	virtual TAiRAGVector* __fastcall Search(TAiEmbeddingNode* Target, int aLimit, double aPrecision);
};


class PASCALIMPLEMENTATION TAiRAGVector : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FActive;
	TAIEmbeddingIndex* FRagIndex;
	Umakerai::Embeddings::Core::TAiEmbeddingsCore* FEmbeddings;
	System::Generics::Collections::TList__1<TAiEmbeddingNode*>* FItems;
	TOnDataVecAddItem FOnDataVecAddItem;
	TOnDataVecSearch FOnDataVecSearch;
	int FDim;
	System::UnicodeString FModel;
	System::UnicodeString FNameVec;
	System::UnicodeString FDescription;
	TAiRagIndexType FInMemoryIndexType;
	Umakerai::Embeddings::Core::TOnGetEmbedding FOnGetEmbedding;
	void __fastcall SetActive(const bool Value);
	void __fastcall SetRagIndex(TAIEmbeddingIndex* const Value);
	void __fastcall SetEmbeddings(Umakerai::Embeddings::Core::TAiEmbeddingsCore* const Value);
	System::Generics::Collections::TList__1<TAiEmbeddingNode*>* __fastcall GetItems();
	void __fastcall SetOnDataVecAddItem(const TOnDataVecAddItem Value);
	void __fastcall SetOnDataVecSearch(const TOnDataVecSearch Value);
	void __fastcall SetDescription(const System::UnicodeString Value);
	void __fastcall SetNameVec(const System::UnicodeString Value);
	void __fastcall SetInMemoryIndexType(const TAiRagIndexType Value);
	
protected:
	Umakerai::Embeddings::Core::TAiEmbeddingData __fastcall DoOnGetEmbedding(System::UnicodeString aInput, System::UnicodeString aUser, int aDimensions = 0xffffffff, System::UnicodeString aModel = System::UnicodeString(), System::UnicodeString aEncodingFormat = L"float");
	
public:
	__fastcall virtual TAiRAGVector(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAiRAGVector();
	void __fastcall SaveToStream(System::Classes::TMemoryStream* Stream);
	void __fastcall LoadFromStream(System::Classes::TMemoryStream* Stream);
	void __fastcall SaveToFile(System::UnicodeString FileName);
	void __fastcall LoadFromFile(System::UnicodeString FileName);
	bool __fastcall Connect(System::UnicodeString aHost, System::UnicodeString aPort, System::UnicodeString aLogin, System::UnicodeString aPassword);
	TAiRAGVector* __fastcall Search(TAiEmbeddingNode* Target, int aLimit, double aPrecision)/* overload */;
	TAiRAGVector* __fastcall Search(System::UnicodeString Prompt, int aLimit, double aPrecision)/* overload */;
	virtual System::UnicodeString __fastcall SearchText(System::UnicodeString aPrompt, int aLimit = 0xa, double aPresicion = 5.000000E-01)/* overload */;
	virtual System::UnicodeString __fastcall SearchText(TAiEmbeddingNode* aPrompt, int aLimit = 0xa, double aPresicion = 5.000000E-01)/* overload */;
	virtual System::UnicodeString __fastcall SearchText(System::UnicodeString aPrompt, TAiRAGVector* DataVec)/* overload */;
	void __fastcall BuildIndex();
	virtual System::NativeInt __fastcall AddItem(TAiEmbeddingNode* aItem, TAiEmbeddingMetaData* MetaData)/* overload */;
	virtual TAiEmbeddingNode* __fastcall AddItem(System::UnicodeString aText, TAiEmbeddingMetaData* MetaData = (TAiEmbeddingMetaData*)(0x0))/* overload */;
	virtual bool __fastcall AddItemsFromJSonArray(System::Json::TJSONArray* aJSonArray, TAiEmbeddingMetaData* MetaData = (TAiEmbeddingMetaData*)(0x0));
	virtual void __fastcall AddItemsFromPlainText(System::UnicodeString aText, TAiEmbeddingMetaData* MetaData = (TAiEmbeddingMetaData*)(0x0), int aLenChunk = 0x200, int aLenOverlap = 0x50);
	TAiEmbeddingNode* __fastcall CreateEmbeddingNode(System::UnicodeString aText, Umakerai::Embeddings::Core::TAiEmbeddingsCore* aEmbeddings = (Umakerai::Embeddings::Core::TAiEmbeddingsCore*)(0x0));
	int __fastcall Count();
	void __fastcall Clear();
	void __fastcall Rerank(TAiEmbeddingNode* Target)/* overload */;
	void __fastcall Rerank(System::UnicodeString NewPrompt)/* overload */;
	__property TAIEmbeddingIndex* RagIndex = {read=FRagIndex, write=SetRagIndex};
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property System::Generics::Collections::TList__1<TAiEmbeddingNode*>* Items = {read=GetItems};
	
__published:
	__property TOnDataVecAddItem OnDataVecAddItem = {read=FOnDataVecAddItem, write=SetOnDataVecAddItem};
	__property TOnDataVecSearch OnDataVecSearch = {read=FOnDataVecSearch, write=SetOnDataVecSearch};
	__property Umakerai::Embeddings::Core::TOnGetEmbedding OnGetEmbedding = {read=FOnGetEmbedding, write=FOnGetEmbedding};
	__property Umakerai::Embeddings::Core::TAiEmbeddingsCore* Embeddings = {read=FEmbeddings, write=SetEmbeddings};
	__property System::UnicodeString Model = {read=FModel};
	__property int Dim = {read=FDim, nodefault};
	__property System::UnicodeString NameVec = {read=FNameVec, write=SetNameVec};
	__property System::UnicodeString Description = {read=FDescription, write=SetDescription};
	__property TAiRagIndexType InMemoryIndexType = {read=FInMemoryIndexType, write=SetInMemoryIndexType, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Vectors */
}	/* namespace Rag */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_RAG_VECTORS)
using namespace Umakerai::Rag::Vectors;
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
#endif	// uMakerAi_RAG_VectorsHPP
