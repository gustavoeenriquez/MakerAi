unit uMakerAi.MD.Highlighter;

{
  AITextMD.Highlighter
  --------------------
  Simple syntax tokenizer for fenced code blocks.
  Returns a flat list of TSHToken records (kind + text) that the renderer
  can turn into styled Skia paragraph runs.

  Supported languages: pascal/delphi, python, javascript/typescript,
  c/c++/csharp, sql, json, bash/shell, generic (strings + comments only).
}

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Character, System.Generics.Collections;

type

  TSHTokenKind = (
    htPlain,        // plain code (default code color)
    htKeyword,      // reserved word
    htString,       // string / char literal
    htComment,      // comment (line or block)
    htNumber,       // integer, float, hex literal
    htPreprocessor, // preprocessor / compiler directive
    htDiffAdd,      // diff: added line (starts with +)
    htDiffRemove    // diff: removed line (starts with -)
  );

  TSHToken = record
    Kind: TSHTokenKind;
    Text: string;
    class function Make(AKind: TSHTokenKind; const AText: string): TSHToken; static; inline;
  end;

  TCodeHighlighter = class
  private
    class function NormLang(const S: string): string; static;
    class function GetKeywords(const ALang: string): TArray<string>; static;
    class function IsWordChar(C: Char; AFirst: Boolean): Boolean; static; inline;
    class function IsDigit(C: Char): Boolean; static; inline;
    class function IsHexDigit(C: Char): Boolean; static; inline;
  public
    { Returns True if we have keyword support for ALang }
    class function SupportsLang(const ALang: string): Boolean; static;

    { Tokenize ACode for ALang.  Returns [] for unknown/empty language. }
    class function Tokenize(const ACode, ALang: string): TArray<TSHToken>; static;
  end;

implementation

{ TSHToken }

class function TSHToken.Make(AKind: TSHTokenKind; const AText: string): TSHToken;
begin
  Result.Kind := AKind;
  Result.Text := AText;
end;

{ TCodeHighlighter --- helpers }

class function TCodeHighlighter.NormLang(const S: string): string;
begin
  Result := LowerCase(Trim(S));
  if Result = 'delphi'     then Result := 'pascal';
  if Result = 'js'         then Result := 'javascript';
  if Result = 'ts'         then Result := 'typescript';
  if Result = 'c++'        then Result := 'cpp';
  if Result = 'c#'         then Result := 'csharp';
  if Result = 'sh'         then Result := 'bash';
  if Result = 'shell'      then Result := 'bash';
  if Result = 'zsh'        then Result := 'bash';
  if Result = 'patch'      then Result := 'diff';
end;

class function TCodeHighlighter.IsWordChar(C: Char; AFirst: Boolean): Boolean;
begin
  if AFirst then
    Result := C.IsLetter or (C = '_')
  else
    Result := C.IsLetterOrDigit or (C = '_');
end;

class function TCodeHighlighter.IsDigit(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

class function TCodeHighlighter.IsHexDigit(C: Char): Boolean;
begin
  Result := IsDigit(C) or
            ((C >= 'a') and (C <= 'f')) or
            ((C >= 'A') and (C <= 'F'));
end;

class function TCodeHighlighter.GetKeywords(const ALang: string): TArray<string>;
begin
  if ALang = 'pascal' then
    Result := [
      'absolute','abstract','and','array','as','asm','assembler',
      'begin','boolean','byte','cardinal','case','cdecl','char','class',
      'const','constructor','contains','default','deprecated','destructor',
      'div','do','downto','dynamic','else','end','except','experimental',
      'exports','extended','external','false','file','final','finalization',
      'finally','for','forward','function','goto','helper','if','implementation',
      'in','index','inherited','initialization','inline','integer','interface',
      'is','label','library','longint','message','mod','name','near','nil',
      'not','object','of','operator','or','out','overload','override',
      'package','packed','pascal','platform','private','procedure','program',
      'property','protected','public','published','raise','read','readonly',
      'real','record','reintroduce','repeat','requires','resident','result',
      'sealed','set','shl','shortint','shr','single','stdcall','stored',
      'strict','string','then','threadvar','to','true','try','type','unit',
      'until','uses','var','virtual','while','with','word','write','writeonly',
      'xor','double','widestring','unicodestring','ansistring','shortstring',
      'int64','uint64','nativeint','nativeuint','pointer','pansichar',
      'pwidechar','bytebool','wordbool','longbool']

  else if ALang = 'python' then
    Result := [
      'and','as','assert','async','await','break','class','continue','def',
      'del','elif','else','except','false','finally','for','from','global',
      'if','import','in','is','lambda','none','nonlocal','not','or','pass',
      'raise','return','true','try','while','with','yield',
      'int','str','float','bool','list','dict','set','tuple','type',
      'len','range','print','input','open','super','self','cls',
      'property','staticmethod','classmethod','abstractmethod']

  else if (ALang = 'javascript') or (ALang = 'typescript') then
    Result := [
      'abstract','any','as','async','await','boolean','break','case','catch',
      'class','const','constructor','continue','debugger','declare','default',
      'delete','do','else','enum','export','extends','false','finally','for',
      'from','function','get','if','implements','import','in','instanceof',
      'interface','is','keyof','let','module','namespace','never','new','null',
      'number','object','of','override','package','private','protected','public',
      'readonly','require','return','set','static','string','super','switch',
      'symbol','this','throw','true','try','type','typeof','undefined','unique',
      'unknown','var','void','while','with','yield']

  else if ALang = 'cpp' then
    Result := [
      'alignas','alignof','and','and_eq','asm','auto','bitand','bitor','bool',
      'break','case','catch','char','char8_t','char16_t','char32_t','class',
      'compl','concept','const','consteval','constexpr','constinit','const_cast',
      'continue','co_await','co_return','co_yield','decltype','default','delete',
      'do','double','dynamic_cast','else','enum','explicit','export','extern',
      'false','float','for','friend','goto','if','inline','int','long','mutable',
      'namespace','new','noexcept','not','not_eq','nullptr','operator','or',
      'or_eq','private','protected','public','register','reinterpret_cast',
      'requires','return','short','signed','sizeof','static','static_assert',
      'static_cast','struct','switch','template','this','thread_local','throw',
      'true','try','typedef','typeid','typename','union','unsigned','using',
      'virtual','void','volatile','wchar_t','while','xor','xor_eq',
      'string','vector','map','pair','nullptr_t','size_t','int32_t',
      'int64_t','uint32_t','uint64_t','std']

  else if ALang = 'csharp' then
    Result := [
      'abstract','as','base','bool','break','byte','case','catch','char',
      'checked','class','const','continue','decimal','default','delegate','do',
      'double','else','enum','event','explicit','extern','false','finally',
      'fixed','float','for','foreach','goto','if','implicit','in','int',
      'interface','internal','is','lock','long','namespace','new','null',
      'object','operator','out','override','params','private','protected',
      'public','readonly','ref','return','sbyte','sealed','short','sizeof',
      'stackalloc','static','string','struct','switch','this','throw','true',
      'try','typeof','uint','ulong','unchecked','unsafe','ushort','using',
      'virtual','void','volatile','while','async','await','var','dynamic',
      'get','set','value','yield','partial','where','from','select','let',
      'orderby','group','join','into','ascending','descending','on','equals',
      'record','init','with','nint','nuint']

  else if ALang = 'sql' then
    Result := [
      'add','all','alter','and','any','as','asc','backup','between','by',
      'case','check','column','constraint','create','cross','database',
      'default','delete','desc','distinct','drop','else','end','exec',
      'exists','foreign','from','full','group','having','in','index',
      'inner','insert','into','is','join','key','left','like','limit',
      'not','null','on','or','order','outer','primary','procedure',
      'replace','right','rownum','select','set','table','top','truncate',
      'union','unique','update','values','view','where','with',
      'begin','commit','rollback','transaction','grant','revoke',
      'int','varchar','char','text','date','datetime','timestamp',
      'float','decimal','numeric','bigint','smallint',
      'true','false']

  else if ALang = 'bash' then
    Result := [
      'if','then','else','elif','fi','case','esac','for','while','until',
      'do','done','in','function','return','exit','break','continue',
      'local','declare','readonly','export','unset','shift','set',
      'echo','printf','read','test','true','false','source',
      'cd','ls','pwd','cat','grep','sed','awk','find','mkdir','rm',
      'cp','mv','chmod','chown','sudo','apt','yum','brew']

  else
    Result := [];
end;

class function TCodeHighlighter.SupportsLang(const ALang: string): Boolean;
var
  N: string;
begin
  N := NormLang(ALang);
  Result := (N = 'pascal')     or (N = 'python')     or
            (N = 'javascript') or (N = 'typescript')  or
            (N = 'cpp')        or (N = 'csharp')      or
            (N = 'sql')        or (N = 'json')        or
            (N = 'bash')       or (N = 'c')           or
            (N = 'diff');
end;

{ =========================================================================
  Main tokenizer
  ========================================================================= }

class function TCodeHighlighter.Tokenize(const ACode, ALang: string): TArray<TSHToken>;
var
  Lang    : string;
  I, Len  : Integer;
  C       : Char;
  Buf     : string;
  Tokens  : TList<TSHToken>;
  Keywords: TArray<string>;
  KWSet   : TDictionary<string, Boolean>;

  procedure Emit(AKind: TSHTokenKind; const AText: string);
  begin
    if AText <> '' then
      Tokens.Add(TSHToken.Make(AKind, AText));
  end;

  procedure FlushBuf(AKind: TSHTokenKind);
  begin
    if Buf <> '' then
    begin
      Emit(AKind, Buf);
      Buf := '';
    end;
  end;

  function Peek(AOffset: Integer = 1): Char;
  begin
    if I + AOffset <= Len then
      Result := ACode[I + AOffset]
    else
      Result := #0;
  end;

  // Consume line comment from current position (after opening marker)
  procedure ReadLineComment;
  begin
    while I <= Len do
    begin
      C := ACode[I];
      Buf := Buf + C;
      Inc(I);
      if C = #10 then Break;
    end;
    FlushBuf(TSHTokenKind.htComment);
  end;

  // Consume block comment; AEndMark is '*/', '*)', '}' or similar
  procedure ReadBlockComment(const AEndMark: string);
  var
    EndLen: Integer;
  begin
    EndLen := Length(AEndMark);
    while I <= Len do
    begin
      C := ACode[I];
      Buf := Buf + C;
      Inc(I);
      if (Buf.Length >= EndLen) and
         (Copy(Buf, Buf.Length - EndLen + 1, EndLen) = AEndMark) then
        Break;
    end;
    FlushBuf(TSHTokenKind.htComment);
  end;

  // Consume string literal with delimiter ADelim, handling backslash-escape
  procedure ReadString(ADelim: Char; ATriple: Boolean);
  var
    TripleMark: string;
    Escaped   : Boolean;
  begin
    Escaped    := False;
    TripleMark := string(ADelim) + string(ADelim) + string(ADelim);

    while I <= Len do
    begin
      C := ACode[I];
      Buf := Buf + C;
      Inc(I);

      if Escaped then
      begin
        Escaped := False;
        Continue;
      end;

      if C = '\' then
      begin
        Escaped := True;
        Continue;
      end;

      if ATriple then
      begin
        if (Buf.Length >= 3) and
           (Copy(Buf, Buf.Length - 2, 3) = TripleMark) then
          Break;
      end
      else
      begin
        if C = ADelim then Break;
        if C = #10    then Break;  // unterminated single-line string
      end;
    end;
    FlushBuf(TSHTokenKind.htString);
  end;

  // Pascal single-quoted string (no backslash escape; doubled quote = literal)
  procedure ReadPascalString;
  begin
    while I <= Len do
    begin
      C := ACode[I];
      Buf := Buf + C;
      Inc(I);
      if C = #39 then  // closing apostrophe
      begin
        if (I <= Len) and (ACode[I] = #39) then  // doubled-quote escape
        begin
          Buf := Buf + ACode[I];
          Inc(I);
        end
        else
          Break;
      end;
    end;
    FlushBuf(TSHTokenKind.htString);
  end;

  // Read number: decimal, hex (0x), float, possible suffix
  procedure ReadNumber;
  begin
    // First digit (or '$' for Pascal hex) already appended to Buf
    while I <= Len do
    begin
      C := ACode[I];
      if IsDigit(C) or IsHexDigit(C) or (C = '.') or (C = '_') or
         (C = 'x') or (C = 'X') or (C = 'e') or (C = 'E') or
         (C = '+') or (C = '-') or (C = 'n') or (C = 'u') or
         (C = 'L') or (C = 'f') or (C = 'F') then
      begin
        Buf := Buf + C;
        Inc(I);
      end
      else
        Break;
    end;
    FlushBuf(TSHTokenKind.htNumber);
  end;

  // Read identifier and classify as keyword or plain
  procedure ReadIdentifier;
  begin
    while I <= Len do
    begin
      C := ACode[I];
      if IsWordChar(C, False) then
      begin
        Buf := Buf + C;
        Inc(I);
      end
      else
        Break;
    end;
    // JSON special values
    if Lang = 'json' then
    begin
      if (Buf = 'true') or (Buf = 'false') or (Buf = 'null') then
        FlushBuf(TSHTokenKind.htKeyword)
      else
        FlushBuf(TSHTokenKind.htPlain);
      Exit;
    end;
    if KWSet.ContainsKey(LowerCase(Buf)) then
      FlushBuf(TSHTokenKind.htKeyword)
    else
      FlushBuf(TSHTokenKind.htPlain);
  end;

begin
  if ACode = '' then Exit(nil);

  Lang := NormLang(ALang);
  if Lang = 'c' then Lang := 'cpp';  // share C++ keywords

  Keywords := GetKeywords(Lang);
  KWSet    := TDictionary<string, Boolean>.Create(Length(Keywords) * 2);
  Tokens   := TList<TSHToken>.Create;
  try
    for var KW in Keywords do
      KWSet.AddOrSetValue(KW, True);

    I   := 1;
    Len := Length(ACode);
    Buf := '';

    { ---- Diff: line-by-line tokenizer (early exit) ---- }
    if Lang = 'diff' then
    begin
      var LineStart := 1;
      while I <= Len do
      begin
        while (I <= Len) and (ACode[I] <> #10) do Inc(I);
        if I <= Len then Inc(I);  // consume the line feed
        var Line := Copy(ACode, LineStart, I - LineStart);
        var DiffKind: TSHTokenKind;
        if Line.StartsWith('+++') or Line.StartsWith('---') or
           Line.StartsWith('diff ') or Line.StartsWith('index ') or
           Line.StartsWith('new file') or Line.StartsWith('deleted file') then
          DiffKind := TSHTokenKind.htPreprocessor
        else if Line.StartsWith('@@') then
          DiffKind := TSHTokenKind.htComment
        else if (Line.Length > 0) and (Line[1] = '+') then
          DiffKind := TSHTokenKind.htDiffAdd
        else if (Line.Length > 0) and (Line[1] = '-') then
          DiffKind := TSHTokenKind.htDiffRemove
        else
          DiffKind := TSHTokenKind.htPlain;
        if Line <> '' then
          Tokens.Add(TSHToken.Make(DiffKind, Line));
        LineStart := I;
      end;
      Result := Tokens.ToArray;
      Exit;
    end;
    { ------------------------------------------------------------------ }

    while I <= Len do
    begin
      C := ACode[I];

      // ---- Pascal-specific ----
      if Lang = 'pascal' then
      begin
        // Pascal hex: $FF
        if C = '$' then
        begin
          Buf := C; Inc(I);
          while (I <= Len) and IsHexDigit(ACode[I]) do
          begin
            Buf := Buf + ACode[I]; Inc(I);
          end;
          FlushBuf(TSHTokenKind.htNumber);
          Continue;
        end;
        // Pascal char literal: #13 #10 #65
        if C = '#' then
        begin
          Buf := C; Inc(I);
          while (I <= Len) and IsDigit(ACode[I]) do
          begin
            Buf := Buf + ACode[I]; Inc(I);
          end;
          FlushBuf(TSHTokenKind.htNumber);
          Continue;
        end;
        // Pascal single-quote string
        if C = #39 then
        begin
          Buf := C; Inc(I);
          ReadPascalString;
          Continue;
        end;
        // Pascal brace comment or compiler directive
        if C = '{' then
        begin
          Buf := C; Inc(I);
          ReadBlockComment('}');
          Continue;
        end;
        // Pascal paren-star comment: (* ... *)
        if (C = '(') and (Peek = '*') then
        begin
          Buf := C + ACode[I + 1]; Inc(I, 2);
          ReadBlockComment('*)');
          Continue;
        end;
      end;

      // ---- Python-specific ----
      if Lang = 'python' then
      begin
        if C = '#' then
        begin
          Buf := C; Inc(I);
          ReadLineComment;
          Continue;
        end;
        // Triple double-quoted strings
        if (C = '"') and (Peek = '"') and (Peek(2) = '"') then
        begin
          Buf := '"""'; Inc(I, 3);
          ReadString('"', True);
          Continue;
        end;
        // Triple single-quoted strings
        if (C = #39) and (Peek = #39) and (Peek(2) = #39) then
        begin
          Buf := #39 + #39 + #39; Inc(I, 3);
          ReadString(#39, True);
          Continue;
        end;
      end;

      // ---- SQL-specific ----
      if Lang = 'sql' then
      begin
        if (C = '-') and (Peek = '-') then
        begin
          Buf := '--'; Inc(I, 2);
          ReadLineComment;
          Continue;
        end;
      end;

      // ---- Bash-specific ----
      if Lang = 'bash' then
      begin
        if C = '#' then
        begin
          Buf := C; Inc(I);
          ReadLineComment;
          Continue;
        end;
      end;

      // ---- C/C++/C# preprocessor ----
      if (Lang = 'cpp') or (Lang = 'csharp') then
      begin
        if (C = '#') and ((I = 1) or (ACode[I - 1] = #10)) then
        begin
          Buf := C; Inc(I);
          while (I <= Len) and (ACode[I] <> #10) do
          begin
            if (ACode[I] = '\') and (I + 1 <= Len) and (ACode[I + 1] = #10) then
            begin
              Buf := Buf + ACode[I] + ACode[I + 1]; Inc(I, 2);
            end
            else
            begin
              Buf := Buf + ACode[I]; Inc(I);
            end;
          end;
          FlushBuf(TSHTokenKind.htPreprocessor);
          Continue;
        end;
      end;

      // ---- Universal: line comment // ----
      if (C = '/') and (Peek = '/') and
         (Lang <> 'sql') and (Lang <> 'bash') then
      begin
        Buf := '//'; Inc(I, 2);
        ReadLineComment;
        Continue;
      end;

      // ---- Universal: block comment /* ... */ ----
      if (C = '/') and (Peek = '*') and
         (Lang <> 'sql') and (Lang <> 'bash') and (Lang <> 'pascal') then
      begin
        Buf := '/*'; Inc(I, 2);
        ReadBlockComment('*/');
        Continue;
      end;

      // ---- Universal: double-quoted string ----
      if (C = '"') and (Lang <> 'pascal') then
      begin
        Buf := C; Inc(I);
        ReadString('"', False);
        Continue;
      end;

      // ---- Single-quoted string (non-Pascal) ----
      if (C = #39) and (Lang <> 'pascal') then
      begin
        Buf := C; Inc(I);
        ReadString(#39, False);
        Continue;
      end;

      // ---- Template literal / backtick (JS/TS) ----
      if (C = '`') and ((Lang = 'javascript') or (Lang = 'typescript')) then
      begin
        Buf := C; Inc(I);
        ReadString('`', False);
        Continue;
      end;

      // ---- JSON string ----
      if (C = '"') and (Lang = 'json') then
      begin
        Buf := C; Inc(I);
        ReadString('"', False);
        Continue;
      end;

      // ---- Numbers ----
      if IsDigit(C) or
         ((C = '0') and ((Peek = 'x') or (Peek = 'X'))) then
      begin
        Buf := C; Inc(I);
        ReadNumber;
        Continue;
      end;

      // ---- Identifier / keyword ----
      if IsWordChar(C, True) then
      begin
        Buf := C; Inc(I);
        ReadIdentifier;
        Continue;
      end;

      // ---- Everything else: plain (operators, punctuation, whitespace) ----
      Emit(TSHTokenKind.htPlain, string(C));
      Inc(I);
    end;

    // Flush any remaining buffer
    if Buf <> '' then
      Emit(TSHTokenKind.htPlain, Buf);

    Result := Tokens.ToArray;
  finally
    Tokens.Free;
    KWSet.Free;
  end;
end;

end.
