
# MakerAI Suite: Advanced AI Components for Delphi


## 📌 Project Description

The **MakerAI Suite** is a comprehensive set of Delphi components designed to seamlessly integrate artificial intelligence into your applications. With support for state-of-the-art models and functionalities, the suite includes tools for natural language processing, audio transcription, image generation, task orchestration, and retrieval-augmented generation (RAG).

### Core Components

- **Chat**: Integration with models like Anthropic, Gemini, Grok, Groq, Mistral, Ollama, and OpenAI.
- **Embeddings**: Vector-based text representations using Grok, Groq, Mistral, Ollama, and OpenAI.
- **Audio**: Powered by Whisper (OpenAI), for transcription and translation.
- **RAG**: Retrieval-augmented generation using all supported models.
- **Graph**: Task orchestration via TAiGraph, enabling visual workflow creation.

---

## 🌟 Key Features

### TAiChat
- **Broad Compatibility**: Supports multiple AI models like GPT-4, Claude, Mistral, and more.
- **File Attachments**: Processes images, audios, and other media inputs.
- **Async Mode**: Real-time feedback for improved user experiences.
- **Tool Integration**: Enables interaction with external tools for queries and tasks.

### TAiAudio
- **Transcription**: Converts audio into text.
- **Translation**: Translates audio content between languages.
- **Voice Synthesis**: Generates spoken audio from text.

### TAiDalle
- **Image Generation**: Creates images from text descriptions.
- **Image Editing**: Modifies existing images using masks.
- **Variations**: Produces alternate versions of an image.

### TAiGraph
- **Visual Task Orchestration**: Simplifies workflow design using graph-based structures.
- **Modular Design**: Facilitates the integration of AI components and external tools.
- **Contextual Workflows**: Builds dynamic systems that adapt to changing contexts.

### RAG Integration
- **Contextual Queries**: Combines language models with context retrieved from vector databases.
- **Database Support**: Works with any vector database, like Pinecone or PostgreSQL (pg_vector) and in-memory embeddings.
- **Scalability**: Handles large datasets for advanced AI-powered systems.

---

## 🎯 Use Cases

### 🌐 **TAiChat**
1. **Virtual Assistants**: Manages complex queries and provides contextual support.
2. **Sentiment Analysis**: Detects tones in social media or survey data.
3. **Content Generation**: Summarizes, generates reports, or writes articles.

### 🎧 **TAiAudio**
1. **Automatic Subtitling**: Creates subtitles for videos.
2. **Voice Assistants**: Enables voice commands for chatbots and applications.
3. **Meeting Documentation**: Transcribes conferences or interviews.

### 🎨 **TAiDalle**
1. **Visual Design**: Creates illustrations from descriptions.
2. **Prototyping**: Generates quick visual concepts.
3. **Creative Editing**: Enhances images using AI.

### 🧩 **TAiGraph**
1. **Task Automation**: Builds workflows for business processes.
2. **AI-Enhanced Operations**: Integrates AI models into dynamic, adaptive pipelines.
3. **Visual System Design**: Creates modular systems with intuitive graph interfaces.

### 🔍 **RAG**
1. **Semantic Search**: Retrieves precise information from large datasets.
2. **Knowledge-Based Systems**: Enhances AI responses with specific contextual knowledge.
3. **Custom AI Assistants**: Builds powerful tools for industries like healthcare or finance.

---

## 📚 Examples

### 🛠️ TAiChat
```delphi
var
  Chat: TAiChat;
begin
  Chat := TAiChat.Create(nil);
  try
    Chat.ApiKey := 'your-api-key';
    Chat.Model := 'gpt-4';
    Chat.AddMessage('What is the capital of France?', 'user');
    ShowMessage(Chat.Run);
  finally
    Chat.Free;
  end;
end;
```

### 🧩 TAiChat with images
```delphi
var
     Res: String;
     MediaFile: TAiMediaFile;
   begin
     MediaFile := TAiMediaFile.Create;
     MediaFile.LoadFromFile('ruta/del/archivo.jpg');
     Res := Chat.AddMessageAndRun('Describe esta imagen', 'user', [MediaFile]);
     ShowMessage(Res);
     MediaFile.Free;
   end;
```

### 🔍 TAiChat with voice files
```delphi
Var
  Res: String;
  MediaFile: TAiMediaFile;
  Msg: TAiChatMessage;
  FileName: String;
begin

  MediaFile := TAiMediaFile.Create;
  MediaFile.LoadFromFile('c:\temp\prompt.wav');

  Try
    Msg := AiOpenChat1.AddMessageAndRunMsg(MemoPrompt.Lines.Text, 'user', [MediaFile]);
  Finally
    FreeAndNil(MediaFile);
  End;

  MemoResponse.Lines.Text := Msg.Content;

  If (Msg.MediaFiles.Count > 0) and (Assigned(Msg.MediaFiles[0].Content)) then
  Begin
    FileName := 'c:\temp\respuesta3' + Cons.ToString + '.wav';
    Msg.MediaFiles[0].Content.Position := 0;
    Msg.MediaFiles[0].Content.SaveToFile(FileName);

    Try
      MediaPlayer1.FileName := FileName;
      MediaPlayer1.Play;
    Finally
    End;
  End;

```

---

## 🛠️ Setup

### Requirements
1. Delphi 11 or higher.
2. API keys for supported models (e.g., OpenAI, Anthropic).
3. Dependencies:
   - `System.Net.HttpClient`
   - `System.JSON`
   - `REST.Client`

### Installation
1. Clone this repository.
2. Configure API keys in the component properties (e.g., `ApiKey`).
3. Follow the examples to integrate components into your Delphi project.

---

## 📜 License

This project is licensed under the [MIT License](LICENSE).

---

## 👤 Author

**Gustavo Enríquez**  
- LinkedIn: [Profile](https://www.linkedin.com/in/gustavo-enriquez-3937654a/)  
- YouTube: [Channel](https://www.youtube.com/@cimamaker3945)  
- GitHub: [Repository](https://github.com/gustavoeenriquez/)  

Want to contribute? Feel free to fork and suggest improvements!

