# MakerAI - Universal LLM Connector for Delphi

[![GitHub Stars](https://img.shields.io/github/stars/gustavoeenriquez/MakerAi?style=social)](https://github.com/gustavoeenriquez/MakerAi)
[![GitHub Issues](https://img.shields.io/github/issues/gustavoeenriquez/MakerAi)](https://github.com/gustavoeenriquez/MakerAi/issues)
[![License](https://img.shields.io/github/license/gustavoeenriquez/MakerAi)](LICENSE)

**MakerAI is not just an API wrapper for LLM services.** It's a tool designed to optimize the use of LLMs in production environments.  Focus on the essence of your business, leaving the technical complexities to MakerAI.

**One component. All AIs. Full Compatibility.**

Tired of dealing with fragmented and complex AI APIs? **MakerAI** offers `TAiConnection`, a **universal** Delphi component that simplifies integration with the leading Language Models (LLMs) and Artificial Intelligences on the market.

**With MakerAI, you connect *once* and forget about the differences.**

## Key Features

*   **Universal LLM Connector:** Compatible with:
    *   OpenAI
    *   Gemini (Google)
    *   Claude (Anthropic)
    *   Grok (xAI)
    *   DeepSeek
    *   Mistral
    *   Ollama (local models)
    *   Groq
    *   And more to come!
*   **Native Multimodal Support:**
    *   Processes input documents such as: Audio, Images, Text, PDFs, and Videos.
    *   Native generation of: Audio, Images, Videos, and Text.
*   **Native RAG Support:** Integrates Retrieval-Augmented Generation (RAG) capabilities, enabling the LLM to leverage external knowledge sources for more accurate and context-aware responses.
*   **Function Calling (Tool Functions):** Allows AI to execute functions directly in your application.
*   **Cross-Platform Compatibility:**
    *   Windows (VCL & FMX)
    *   Linux (FMX)
    *   Android (FMX)
*   **Extensible Framework:** Easily adaptable to integrate new AI providers.
*   **Simple to Use:** Minimizes complexity, allowing you to focus on your application's logic.

## Slogan

**MakerAI: Universal LLM Connector - One component, infinite possibilities.**

## Why use MakerAI?

*   **Time Saving:** Forget about rewriting code for each AI API.
*   **Flexibility:** Switch AI providers instantly without modifying your application.
*   **Multimodal Power:** Create applications that see, hear, and generate multimedia content.
*   **Portability:** Deploy your applications on multiple platforms.
*   **Future-Proof:** MakerAI is constantly updated to integrate the latest advancements in the world of AI.

## Updates

### New Version 2.1.002 (25 Jul 2025)
- Fixed some reported bugs.
- Update demo 001.
- Implementation of Transcription, Reasoning, DeepResearch and Code Interpreter.
- VEO3 support and VEO2 Image to Video 
- Support for taiconnection to work within threads

### New Version 2.1.001 (15 Jul 2025)
- Fixed some reported bugs.
- Add demo 001.

### New Version 2.1.000 (14 Jul 2025)
- Added CustomModels utility to allow customization of model behavior.
- Enabled generation of text files such as html, js, css, txt, pas, c, py, etc.
- Fixed several reported bugs.

## Installation

1.  Download the source code.
2.  Open the design project (`MakerAI.dpk`, `MakerAiDsg.dpk`) corresponding to your IDE (VCL).
3.  Install the package.
4.  Add path to Delphi Library paths

## Basic Usage Example 

```delphi
procedure TForm1.Button1Click(Sender: TObject);
var
  Response: string;
begin
  //Make sure you have configured DriverName and Model in the Object Inspector
  Response := AiConnection1.AddMessageAndRun('What is the capital of France?', 'user', []);
  Memo1.Lines.Add('IA: ' + Response);
end;
```

Image Usage Example
```delphi
procedure TForm1.Button2Click(Sender: TObject);
var
  MediaFile : TAiMediaFile;
  Response : string;
begin
  MediaFile := TAiMediaFile.Create;
  try
    MediaFile.LoadFromFile('C:\my_image.jpg'); // Replace with the path to your image
    Response := AiConnection1.AddMessageAndRun('Describe this image', 'user', [MediaFile]);
    Memo1.Lines.Add('IA: ' + Response);
  finally
    MediaFile.Free;
  end;
end;
```
## Roadmap

*   Support for more platforms and models
*   New AI Drivers
*   Visual components to facilitate the creation of user interfaces for AI

## License

This project is licensed under the MIT license.

## Contributions

Contributions are welcome! If you have ideas to improve MakerAI, feel free to open an issue or submit a pull request.

## Support

If you have any questions or need help, you can contact me through:

*   [GitHub Issues](https://github.com/gustavoeenriquez/MakerAi/issues)
*   Email: gustavoeenriquez@gmail.com
*   [Telegram group](https://t.me/+7LaihFwqgsk1ZjQx)

  

## Acknowledgements

I thank the Delphi community for their support.