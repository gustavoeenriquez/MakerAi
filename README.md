
# MakerAI 3.0 — The AI Operating System for Delphi

🌐 **Official Website:** [https://makerai.cimamaker.com](https://makerai.cimamaker.com)

[![GitHub Stars](https://img.shields.io/github/stars/gustavoeenriquez/MakerAi?style=social)](https://github.com/gustavoeenriquez/MakerAi)
[![GitHub Issues](https://img.shields.io/github/issues/gustavoeenriquez/MakerAi)](https://github.com/gustavoeenriquez/MakerAi/issues)
[![License](https://img.shields.io/github/license/gustavoeenriquez/MakerAi)](LICENSE.txt)
[![Telegram](https://img.shields.io/badge/Join-Telegram%20Chat-blue.svg)](https://t.me/+7LaihFwqgsk1ZjQx)

**MakerAI 3.0 is not just a component suite — it’s a complete AI orchestration framework for Delphi developers.**

With full support for RAG, MCP, autonomous agents, and graph-based orchestration, you can design and deploy intelligent, self-learning, and context-aware systems natively in Delphi.

---

## The Power of MakerAI 3.0: The AI Orchestration Platform

MakerAI 3.0 provides the architecture to manage the entire lifecycle of complex AI applications, acting as the operating system for intelligence within your software.

*   🚀 **Orchestrate Autonomous Agents:** Create powerful agents that manage state, utilize advanced **Semantic Memory**, and execute complex workflows defined by the internal graph engine.
*   🧠 **RAG Engine 2.0 (Knowledge & Context):** Connect AI to your private data with the robust Retrieval-Augmented Generation (RAG) V2.0 engine, supporting massive indexing, multi-model environments, and hybrid embeddings.
*   🛠️ **Enjoy 100% Native Performance:** MakerAI is pure Delphi. This ensures optimal performance, seamless integration, and compilation without heavy external dependencies across all supported Delphi versions (**10.3 to 12 Athens**).
*   🌐 **Universal LLM Connectivity (AiConnection V3):** The unified connector, `TAiConnection`, features enhanced connectivity and stability for seamless integration with OpenAI, Gemini, Claude, Ollama, Groq, and many more.
*   🏗️ **Enterprise-Grade Architecture:** Ideal foundation for integrating AI into mission-critical systems like **ERP, CRM, LMS, and industrial solutions**.

---

## ✨ Key Features (Version 3.0 Modules)

MakerAI 3.0 introduces next-generation features focused on complexity and reliability.

#### 🧠 **RAG (Retrieval-Augmented Generation) Engine 2.0**
The completely overhauled RAG engine allows the language models to use your private knowledge with unprecedented precision.
*   **Massive Indexing:** Efficiently index millions of documents for enterprise-scale applications.
*   **Hybrid Embeddings:** Utilize a combination of sparse and dense vectors for superior search relevance.
*   **Multi-Model Support:** Seamlessly integrate RAG across different LLM backends (OpenAI, Gemini, Ollama, etc.).

#### 💾 **Advanced Semantic Memory**
*   **Unified Memory Model:** New semantic memory layer deeply integrated with the RAG engine and the Workflow Graphs, allowing agents to form personalized, long-term context that transcends single conversations.
*   **Short and Long-Term Recall:** Equip agents with the ability to recall specific past interactions and generalize long-term learnings.

#### 🔄 **Workflow Engine (Graphs) & Agents**
*   **Visual and Code-Based Orchestration:** Define complex logical flows by connecting nodes and edges. Agents follow this defined logic, utilizing memory and tools to achieve goals.

#### 📞 **MakerAI Tools System (Function Calling)**
*   **Unified Tool Management:** A standardized and improved system for defining and exposing native Delphi functions (Tools) to the language model, enabling the AI to interact with your application's core logic (e.g., querying databases, sending commands).

#### 🗣️ **Voice Monitoring & Progressive Transcription**
*   **Activation Commands:** Integrated support for "wake-word" detection.
*   **Live Transcription:** Progressive audio transcription, ideal for real-time applications and low-latency interaction.

#### 🎨 **Professional Visual Components (ChatUI)**
*   **Chat UI:** Professional-grade, easy-to-use visual components for quickly building multimedia-integrated chat interfaces (text, images, audio, video).

#### 🔌 **MCP (Model Context Protocol) Support**
*   **Native MCP Client & Server:** Connect to or expose your Delphi application functions as standard MCP tools, ensuring seamless interoperability with other platforms.

#### 🌐 **Universal LLM Connector (AiConnection V3)**
*   **Enhanced Connectivity:** Improved performance and stability for accessing all major providers (OpenAI, Gemini, Claude, Groq, Ollama, Mistral).
*   **Native Multimodal Support:** Process and generate text, images, and audio seamlessly.

---

## 📦 Detailed Installation Guide (Modular V3.0)

Follow these steps carefully to ensure a correct installation of the four core packages in the Delphi IDE.

#### **Step 1: Get the Source Code**

Clone the repository using Git (recommended) or download the ZIP file from GitHub.
```bash
git clone https://github.com/gustavoeenriquez/MakerAi.git
```
Unzip the file to a folder of your choice. For example: `C:\Delphi\MakerAI`.

#### **Step 2: Configure Paths in the Delphi IDE**

This is a crucial step. You must tell the Delphi IDE where to find the MakerAI source files.

1.  Open your Delphi IDE (**10.3 Rio, 10.4 Sydney, 11 Alexandria, or 12 Athens**).
2.  Go to the menu: **Tools > Options...**.
3.  In the Options dialog, navigate to **Language > Delphi > Library**.
4.  Ensure you have the correct platform selected (e.g., **Windows 32-bit**). You will need to repeat this step for each platform you intend to use.
5.  Click the `...` button next to the "Library path" field.
6.  In the new window, add the path to **each of the subfolders** inside the MakerAI `Source` directory:

    ```
    ...\MakerAi\Source\Agents
    ...\MakerAi\Source\Chat
    ...\MakerAi\Source\ChatUI       <-- NUEVO (Componentes Visuales)
    ...\MakerAi\Source\Core
    ...\MakerAi\Source\Design
    ...\MakerAi\Source\MCPClient
    ...\MakerAi\Source\MCPServer
    ...\MakerAi\Source\Packages
    ...\MakerAi\Source\RAG
    ...\MakerAi\Source\Resources    <-- NUEVO (Recursos)
    ...\MakerAi\Source\Tools
    ...\MakerAi\Source\Utils
    ```
    *Tip: Add the full path to each of these folders to prevent compilation issues.*

7.  Click **OK** and then **Save** to store the configuration.

#### **Step 3: Compile and Install the Packages**

MakerAI 3.0 is built on a modular architecture using four packages. You must compile and install them in the correct order.

1.  In Delphi, go to **File > Open Project...**.
2.  Navigate to the folder where you cloned the repository and open the project group file: `...\MakerAi\Source\Packages\MakerAI.groupproj`.
3.  **Compile Runtime Packages (No Installation Required):**
    *   Right-click on `MakerAi.dpk` and select **Compile**. (Core functionality)
    *   Right-click on `MakerAi.RAG.Drivers.dpk` and select **Compile**. (DB connections for RAG)
4.  **Compile and Install Visual/Design Packages:**
    *   Right-click on the visual package `MakerAi.UI.dpk` and select **Compile**, then right-click again and select **Install**. (Registers ChatUI components)
    *   Right-click on the design-time package `MakerAiDsg.dpk` and select **Compile**, then right-click again and select **Install**. (Registers core non-visual components)

An IDE message will confirm that the components have been installed successfully into the Tool Palette.

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

**(MAJOR) Version 3.0 (Q4 2025 / Q1 2026)**
*   **Architectural Shift:** MakerAI upgraded from component suite to a complete AI Orchestration Platform / AI Operating System.
*   **RAG Engine 2.0:** Complete overhaul introducing Hybrid Embeddings, massive indexing support, and multi-model compatibility.
*   **Advanced Semantic Memory:** New memory model integrated with RAG and Workflow Graphs for complex, context-aware agents.
*   **AiConnection V3:** Enhanced performance, stability, and connectivity for all major LLM providers.
*   **New Source Structure:** Added `ChatUI` and `Resources` folders.
*   **Modular Packaging:** Implementation of four packages: `MakerAi.bpl`, `MakerAi.RAG.Drivers.bpl`, `MakerAi.UI.bpl`, and `MakerAiDsg.bpl`.
*   **New Feature:** Integrated Voice Monitoring with progressive transcription and activation commands.
*   **New Feature:** Professional visual components for building advanced multimedia chat interfaces (`ChatUI`).
*   **Compatibility:** Official support for Delphi 10.3, 10.4 Sydney, 11 Alexandria, and 12 Athens.
*   **Tools:** Unified MakerAI Tools system for standardized function calling.

**Version 2.6.0 (August 20, 2025)**
*   **New Compatibility!** Added official support for Delphi 10.3 Rio and Delphi 11 Alexandria.
*   **Fix (Ollama):** Fixed a bug in the Ollama connector that occurred during asynchronous calls.

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