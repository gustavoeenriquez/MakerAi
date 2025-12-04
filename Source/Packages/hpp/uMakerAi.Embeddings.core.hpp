// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Embeddings.core.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Embeddings_coreHPP
#define uMakerAi_Embeddings_coreHPP

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
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.JSON.hpp>
#include <System.Math.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Embeddings
{
namespace Core
{
//-- forward type declarations -----------------------------------------------
struct TAiSimilarityResult;
struct TAiEmbeddingDataRec;
class DELPHICLASS TAiEmbeddingsCore;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<double> TAiEmbeddingData;

typedef System::DynamicArray<System::DynamicArray<double> > TAiEmbeddingList;

struct DECLSPEC_DRECORD TAiSimilarityResult
{
public:
	int Index;
	double Score;
	TAiEmbeddingData Vector;
};


typedef System::DynamicArray<TAiSimilarityResult> TAiSimilarityList;

typedef void __fastcall (__closure *TOnGetEmbedding)(System::TObject* Sender, const System::UnicodeString aInput, const System::UnicodeString aUser, const System::UnicodeString aModel, const System::UnicodeString aEncodingFormat, int aDimensions, TAiEmbeddingData &aEmbedding);

struct DECLSPEC_DRECORD TAiEmbeddingDataRec
{
private:
	TAiEmbeddingData FData;
	
public:
	static TAiEmbeddingDataRec __fastcall FromArray(const TAiEmbeddingData A);
	TAiEmbeddingData __fastcall ToArray();
	__property TAiEmbeddingData Data = {read=FData, write=FData};
	static TAiEmbeddingDataRec __fastcall _op_Addition(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B);
	static TAiEmbeddingDataRec __fastcall _op_Subtraction(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B);
	static TAiEmbeddingDataRec __fastcall _op_Multiply(const TAiEmbeddingDataRec A, const double Scalar);
	static double __fastcall _op_Multiply(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B);
	static TAiEmbeddingDataRec __fastcall _op_Division(const TAiEmbeddingDataRec A, const double Scalar);
	static bool __fastcall _op_Equality(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B);
	static bool __fastcall _op_Inequality(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B);
	static TAiEmbeddingData __fastcall StringToEmbedding(const System::UnicodeString AVectorString);
	static System::UnicodeString __fastcall EmbeddingToString(const TAiEmbeddingData AEmbedding);
	double __fastcall Magnitude();
	void __fastcall Normalize();
	
	friend TAiEmbeddingDataRec operator +(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B) { return TAiEmbeddingDataRec::_op_Addition(A, B); }
	friend TAiEmbeddingDataRec operator -(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B) { return TAiEmbeddingDataRec::_op_Subtraction(A, B); }
	friend TAiEmbeddingDataRec operator *(const TAiEmbeddingDataRec A, const double Scalar) { return TAiEmbeddingDataRec::_op_Multiply(A, Scalar); }
	friend double operator *(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B) { return TAiEmbeddingDataRec::_op_Multiply(A, B); }
	friend TAiEmbeddingDataRec operator /(const TAiEmbeddingDataRec A, const double Scalar) { return TAiEmbeddingDataRec::_op_Division(A, Scalar); }
	friend bool operator ==(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B) { return TAiEmbeddingDataRec::_op_Equality(A, B); }
	friend bool operator !=(const TAiEmbeddingDataRec A, const TAiEmbeddingDataRec B) { return TAiEmbeddingDataRec::_op_Inequality(A, B); }
};


class PASCALIMPLEMENTATION TAiEmbeddingsCore : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TOnGetEmbedding FOnGetEmbedding;
	void __fastcall SetData(const TAiEmbeddingData Value);
	
protected:
	TAiEmbeddingData FData;
	System::UnicodeString FModel;
	int FDimensions;
	int Ftotal_tokens;
	int Fprompt_tokens;
	
public:
	__fastcall virtual TAiEmbeddingsCore(System::Classes::TComponent* aOwner);
	virtual TAiEmbeddingData __fastcall CreateEmbedding(System::UnicodeString aInput, System::UnicodeString aUser, int aDimensions = 0xffffffff, System::UnicodeString aModel = System::UnicodeString(), System::UnicodeString aEncodingFormat = L"float");
	System::Json::TJSONArray* __fastcall ToJsonArray()/* overload */;
	__classmethod System::Json::TJSONArray* __fastcall ToJsonArray(TAiEmbeddingData Val)/* overload */;
	__classmethod TAiEmbeddingData __fastcall ToEmbeddingData(System::Json::TJSONArray* Value);
	__classmethod double __fastcall Magnitude(const TAiEmbeddingData V);
	__classmethod double __fastcall DotProduct(const TAiEmbeddingData A, const TAiEmbeddingData B);
	__classmethod double __fastcall CosineSimilarity(const TAiEmbeddingData A, const TAiEmbeddingData B);
	__classmethod double __fastcall EuclideanDistance(const TAiEmbeddingData A, const TAiEmbeddingData B);
	__classmethod void __fastcall Normalize(TAiEmbeddingData &V);
	__classmethod TAiSimilarityResult __fastcall FindNearest(const TAiEmbeddingData Query, const TAiEmbeddingList Candidates);
	__classmethod TAiSimilarityList __fastcall FindTopK(const TAiEmbeddingData Query, const TAiEmbeddingList Candidates, int K);
	__classmethod TAiEmbeddingData __fastcall VectorAdd(const TAiEmbeddingData A, const TAiEmbeddingData B);
	__classmethod TAiEmbeddingData __fastcall VectorSubtract(const TAiEmbeddingData A, const TAiEmbeddingData B);
	__classmethod TAiEmbeddingData __fastcall AverageEmbedding(const TAiEmbeddingList Vectors);
	__property TAiEmbeddingData Data = {read=FData, write=SetData};
	
__published:
	__property System::UnicodeString Model = {read=FModel, write=FModel};
	__property int Dimensions = {read=FDimensions, write=FDimensions, nodefault};
	__property int prompt_tokens = {read=Fprompt_tokens, nodefault};
	__property int total_tokens = {read=Ftotal_tokens, nodefault};
	__property TOnGetEmbedding OnGetEmbedding = {read=FOnGetEmbedding, write=FOnGetEmbedding};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiEmbeddingsCore() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Core */
}	/* namespace Embeddings */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_EMBEDDINGS_CORE)
using namespace Umakerai::Embeddings::Core;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_EMBEDDINGS)
using namespace Umakerai::Embeddings;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_Embeddings_coreHPP
