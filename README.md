# MakerAI Suite: The Definitive AI Framework for Delphi

🌐 **Official Website:** [https://makerai.cimamaker.com](https://makerai.cimamaker.com)

[![GitHub Stars](https://img.shields.io/github/stars/gustavoeenriquez/MakerAi?style=social)](https://github.com/gustavoeenriquez/MakerAi)
[![GitHub Issues](https://img.shields.io/github/issues/gustavoeenriquez/MakerAi)](https://github.com/gustavoeenriquez/MakerAi/issues)
[![License](https://img.shields.io/github/license/gustavoeenriquez/MakerAi)](LICENSE.txt)
[![Telegram](https://img.shields.io/badge/Join-Telegram%20Chat-blue.svg)](https://t.me/+7LaihFwqgsk1ZjQx)

**Build powerful, intelligent, and context-aware applications with a 100% native AI framework for Delphi.**

MakerAI Suite is a comprehensive framework designed for Delphi developers to integrate state-of-the-art AI capabilities into their VCL and FMX applications. It covers the entire AI lifecycle, from model connectivity to the creation of autonomous agents.

---

## The Power of MakerAI Suite

MakerAI Suite provides the building blocks to create sophisticated, high-performance AI systems.

*   🚀 **Create Sophisticated AI Agents:** Orchestrate complex workflows, equip your agents with short and long-term memory, and allow them to execute tools to interact with your application and the outside world.
*   🧠 **Connect AI with Your Own Data (RAG):** Leverage the powerful **Retrieval-Augmented Generation (RAG)** engine. Enable language models to answer questions based on your documents, databases, or any private knowledge source.
*   🛠️ **Enjoy 100% Native Performance:** MakerAI is pure Delphi. This ensures optimal performance, seamless integration, and compilation without heavy external dependencies.
*   🌐 **Access a Global Ecosystem of Models:** The suite includes the acclaimed universal connector, `TAiConnection`, for unified integration with OpenAI, Gemini, Claude, Ollama, Groq, and many more.
*   🏗️ **Enterprise-Grade Architecture:** Its robust and modular design serves as the perfect foundation for AI in your **ERP, CRM, LMS, and industrial solutions**.

---

## ✨ Key Features

MakerAI Suite is organized into powerful and easy-to-use modules:

#### 🧠 **RAG (Retrieval-Augmented Generation) Engine**
*   **Vector Databases:** Compatible with PostgreSQL+pgvector, SQLite, Firebird, and more.
*   **Commercial Databases:** Integration with Pinecone, Weaviate, Milvus, Qdrant, etc.
*   **In-Memory Engine:** Ideal for rapid prototyping, with persistence to disk.

#### 🔄 **Workflow Engine (Graphs)**
*   **Visual and Code-Based Orchestration:** Define complex workflows by connecting nodes and edges, either programmatically or by dragging components onto a form.
*   **Autonomous Agents:** Facilitates the creation of agents that follow a defined logic to complete complex tasks.

#### 📞 **Function Calling and Tools**
*   **Extend the AI:** Allows the language model to execute native functions in your Delphi application to perform actions, such as querying a database, sending an email, or controlling a device.

#### 💾 **Conversational Memory Management**
*   **Short and Long-Term Memory:** Equips agents with the ability to recall past interactions for more coherent and personalized conversations.

#### 🔌 **MCP (Model Context Protocol) Support**
*   **Native MCP Client:** Connect your application to external tool servers that follow the MCP standard (via HTTP, stdio).
*   **Native MCP Server:** Expose your own Delphi application's functions as tools for any MCP client to consume.

#### 🌐 **Universal LLM Connector**
*   **One Component to Rule Them All:** `TAiConnection` provides access to **OpenAI, Gemini, Claude, Groq, Ollama, Mistral,** and more, through a unified interface.
*   **Native Multimodal Support:** Natively process and generate text, images, audio, and more.

---

## 📦 Detailed Installation Guide

Follow these steps carefully to ensure a correct installation in the Delphi IDE.

#### **Step 1: Get the Source Code**

Clone the repository using Git (recommended) or download the ZIP file from GitHub.
```bash
git clone https://github.com/gustavoeenriquez/MakerAi.git
```
Unzip the file to a folder of your choice. For example: `C:\Delphi\MakerAI`.

#### **Step 2: Configure Paths in the Delphi IDE**

This is a crucial step. You must tell the Delphi IDE where to find the MakerAI source files.

1.  Open your Delphi IDE (10.3 Rio, 11 Alexandria, or higher).
2.  Go to the menu: **Tools > Options...**.
3.  In the Options dialog, navigate to **Language > Delphi > Library**.
4.  Ensure you have the correct platform selected (e.g., **Windows 32-bit**). You will need to repeat this step for each platform you intend to use.
5.  Click the `...` button next to the "Library path" field.
6.  In the new window, add the path to **each of the subfolders** inside the MakerAI `Source` directory. The folder structure is as follows:

    ```
    ...\MakerAi\Source\Agents
    ...\MakerAi\Source\Chat
    ...\MakerAi\Source\Core
    ...\MakerAi\Source\Design
    ...\MakerAi\Source\MCPClient
    ...\MakerAi\Source\MCPServer
    ...\MakerAi\Source\Packages
    ...\MakerAi\Source\RAG
    ...\MakerAi\Source\Tools
    ...\MakerAi\Source\Utils
    ```
    *Tip: Add the full path to each of these folders to prevent compilation issues.*

7.  Click **OK** and then **Save** to store the configuration.

#### **Step 3: Compile and Install the Components**

This will install the components into the Tool Palette.

1.  In Delphi, go to **File > Open Project...**.
2.  Navigate to the folder where you cloned the repository and open the project group file: `...\MakerAi\Source\Packages\MakerAI.groupproj`.
3.  In the *Project Manager*, right-click on the design-time package `MakerAiDsg.dpk` and select **Compile**.
4.  Once it has compiled without errors, right-click on `MakerAiDsg.dpk` again and select **Install**.
5.  An IDE message will confirm that the components have been installed successfully.

Congratulations! MakerAI Suite is now installed and ready to be used in your projects.

---


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


## 🔄 Changelog

**(NEW) Version 2.6.0 (August 20, 2025)**
*   **New Compatibility!** Added official support for Delphi 10.3 Rio and Delphi 11 Alexandria.
*   **Fix (Ollama):** Fixed a bug in the Ollama connector that occurred during asynchronous calls.

**Version 2.5.0.02 (August 17, 2025)**
*   Added MCP Server on Windows and Linux with support for StdIO, HTTP, and MakerAiHttp modes.
*   Added Agent support for complex workflows.

**Version 2.5.0.01 (August 4, 2025)**
*   Added support for MCP (Model Context Protocol) Servers on Windows and Linux.
*   New component `TGeminiEmbedding` (contribution by Pedro Luque).
*   Reorganized source code into folders for improved readability.

**(The rest of the history remains)...**

---

## 🤝 Contributing

Contributions are welcome. If you have ideas, improvements, or find a bug, please open an issue or submit a pull request. Together, we can make MakerAI the definitive AI tool for the Delphi community.

## 💬 Community & Support

*   **Official Website:** [https://makerai.cimamaker.com](https://makerai.cimamaker.com)
*   **GitHub Issues:** For reporting bugs and suggesting features.
*   **Telegram Group:** [Join the conversation](https://t.me/+7LaihFwqgsk1ZjQx) for help and to share your projects.
*   **Email:** gustavoeenriquez@gmail.com

## 📜 License

This project is licensed under the **MIT License**. See the [LICENSE.txt](LICENSE.txt) file for details.