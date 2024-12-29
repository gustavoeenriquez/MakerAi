Here's a complete presentation optimized for your project on GitHub, integrating the capabilities of TAiChat, TAiAudio y TAiDalle:  

---

Este archivo readme también está disponible en [Español](README.MD)


# TAiChat: Advanced Connectivity with AI Models for Delphi  

![Banner del Proyecto](https://via.placeholder.com/1024x300)  

## 📌 Project Description  

**TAiChat** is a component designed to simplify the integration of artificial intelligence language (LLM) models into applications developed with Delphi. This project provides tools to interact with multiple industry-leading APIs, such as OpenAI, Anthropic, Gemini, and more, giving developers flexibility and power to create innovative solutions.

The project also includes additional components, **TAiAudio** y **TAiDalle**, which expand capabilities to audio transcription, speech synthesis, and image generation.

---

## 🌟 Key features  

### TAiChat  
- **Extensive Compatibility**: Connection with models such as GPT-4, Claude, Mistral, among others.  
- **Multimedia**: Processes images, audios, and other attachments.  
- **Asynchronous Mode**: Real-time feedback for enhanced experiences.  
- **Function Execution**: Enables external tools for searches and calculations.  

### TAiAudio  
- **Transcription**: Convert audio to text.  
- **Translation**: Translate audio content between languages.  
- **Speech Synthesis**: Generates spoken audio from text.  

### TAiDalle  
- **Image Generation**: Create images from textual descriptions.  
- **Image Editing**: Modify existing images with masks.  
- **Variations**: Generate alternate versions of images.  

---

## 🎯 Use Cases  

### 🌐 **TAiChat**  
1. **Virtual Assistants**: They manage complex queries and offer contextual support.  
2. **Sentiment Analysis**: Understanding the tone in social networks and surveys.  
3. **Content Generation**: Creation of summaries, reports or articles.  

### 🎧 **TAiAudio**  
1. **Automatic Subtitling**: Generation of subtitles for videos.  
2. **Voice Assistants**: Voice commands for chatbots and applications.  
3. **Automatic Documentation**: Transcription of meetings and conferences.  

### 🎨 **TAiDalle**  
1. **Visual Design**: Creation of illustrations from descriptions.  
2. **Prototypes**: Generation of quick visual concepts.  
3. **Creative Editing**: Modifying images with artificial intelligence.  

---

## 📚 Examples of Use  

### 🛠️ TAiChat  
```delphi
var
  Chat: TAiChat;
begin
  Chat := TAiChat.Create(nil);
  try
    Chat.ApiKey := 'your-api-key';
    Chat.Model := 'gpt-4';
    Chat.AddMessage('What is the capital city of France?', 'user');
    ShowMessage(Chat.Run);
  finally
    Chat.Free;
  end;
end;
```

### 🎧 TAiAudio  
```delphi
var
  AiAudio: TAiAudio;
  TranscriptionText: String;
begin
  AiAudio := TAiAudio.Create(nil);
  try
    AiAudio.ApiKey := 'your-api-key';
    TranscriptionText := AiAudio.Transcription('ruta/audio.mp3', 'audio.mp3', 'Transcribe this');
    ShowMessage(TranscriptionText);
  finally
    AiAudio.Free;
  end;
end;
```

### 🎨 TAiDalle  
```delphi
var
  DalleComponent: TAiDalle;
  GeneratedImage: TAiDalleFile;
begin
  DalleComponent := TAiDalle.Create(nil);
  try
    DalleComponent.ApiKey := 'your-api-key';
    GeneratedImage := DalleComponent.Generate(
      'An enchanted forest at sunset',
      TiaSize1024,  // 1024x1024
      1             // Generate 1 imagen
    );
    GeneratedImage.Image.SaveToFile('bosque.png');
  finally
    DalleComponent.Free;
  end;
end;
```

---

## 🛠️ Configuration  

### **Requirements**  
1. Delphi 11 or higher.  
2. OpenAI API key.  
3. Installed dependencies:  
   - `System.Net.HttpClient`  
   - `System.JSON`  
   - `REST.Client`  

### **Installation**  
1. Download or clone this repository.  
2. Set your API key in the component properties (e.g. `ApiKey`).  
3. Follow the examples to integrate TAiChat, TAiAudio, or TAiDalle into your project.  

---

## 📜 License  

This project is licensed under the [MIT license](LICENSE).  

---

## 👤 Author  

**Gustavo Enríquez**  
- LinkedIn: [Perfil](https://www.linkedin.com/in/gustavo-enriquez-3937654a/)  
- YouTube: [Canal](https://www.youtube.com/@cimamaker3945)  
- GitHub: [Repositorio](https://github.com/gustavoeenriquez/)  

Do you want to contribute? Feel free to fork and propose improvements!  



