{ ===============================================================================
  uMakerAi.ErrorCodes

  Shared error code enum for MakerAi DLL wrappers.
  The integer values are duplicated as local const blocks inside each DLL
  source file to keep the DLLs free of external unit dependencies.

  License: MIT
=============================================================================== }
unit uMakerAi.ErrorCodes;

interface

type
  TLlamaErrorCode = (
    llamaErrNone          = 0,   // success
    llamaErrUnknown       = 1,   // unclassified exception
    llamaErrFileNotFound  = 2,   // model path does not exist
    llamaErrIO            = 3,   // file unreadable / permission denied
    llamaErrInvalidArg    = 4,   // bad argument (wrong extension, dims <= 0, etc.)
    llamaErrModelLoad     = 5,   // llama_model_load returned nil
    llamaErrContextCreate = 6,   // llama_init_from_model returned nil
    llamaErrTokenize      = 7,   // tokenization failed or prompt too long
    llamaErrDecode        = 8,   // llama_decode / llama_encode returned non-zero
    llamaErrNoEmbeddings  = 9,   // get_embeddings_seq returned nil
    llamaErrSampler       = 10,  // sampler chain creation failed
    llamaErrDllLoad       = 11,  // LoadLibrary failed
    llamaErrEntryPoint    = 12   // GetProcAddress failed for required function
  );

const
  LLAMA_ERR_NONE           = 0;
  LLAMA_ERR_UNKNOWN        = 1;
  LLAMA_ERR_FILE_NOT_FOUND = 2;
  LLAMA_ERR_IO             = 3;
  LLAMA_ERR_INVALID_ARG    = 4;
  LLAMA_ERR_MODEL_LOAD     = 5;
  LLAMA_ERR_CONTEXT_CREATE = 6;
  LLAMA_ERR_TOKENIZE       = 7;
  LLAMA_ERR_DECODE         = 8;
  LLAMA_ERR_NO_EMBEDDINGS  = 9;
  LLAMA_ERR_SAMPLER        = 10;
  LLAMA_ERR_DLL_LOAD       = 11;
  LLAMA_ERR_ENTRY_POINT    = 12;

implementation

end.
