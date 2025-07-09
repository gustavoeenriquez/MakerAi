# MakerAI - Universal LLM Connector for Delphi

[![GitHub Stars](https://img.shields.io/github/stars/gustavoeenriquez/MakerAi?style=social)](https://github.com/gustavoeenriquez/MakerAi)
[![GitHub Issues](https://img.shields.io/github/issues/gustavoeenriquez/MakerAi)](https://github.com/gustavoeenriquez/MakerAi/issues)
[![License](https://img.shields.io/github/license/gustavoeenriquez/MakerAi)](LICENSE)

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

## Installation

1.  Download the source code.
2.  Open the design project (`.dpk`) corresponding to your IDE (VCL or FMX).
3.  Install the package.

## Basic Usage Example (VCL)

```delphi
procedure TForm1.Button1Click(Sender: TObject);
var
  Response: string;
begin
  //Make sure you have configured DriverName and Model in the Object Inspector
  Response := AiConnection1.AddMessageAndRun('What is the capital of France?', 'user', []);
  Memo1.Lines.Add('IA: ' + Response);
end;
Use code with caution.
Markdown
Image Usage Example (VCL)
Generated delphi
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
Delphi
Roadmap
Support for more platforms.
New AI Drivers.
Visual components to facilitate the creation of user interfaces for AI.
License
This project is licensed under the MIT license.
Contributions
Contributions are welcome! If you have ideas to improve MakerAI, feel free to open an issue or submit a pull request.
Support
If you have any questions or need help, you can contact me through:
GitHub Issues
Email: gustavoeenriquez@gmail.com
Acknowledgements
I thank the Delphi community for their support.