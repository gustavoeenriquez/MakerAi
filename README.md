***

# MakerAI 3.1 Suite — The AI Operating System for Delphi

🌐 **Official Website:** [https://makerai.cimamaker.com](https://makerai.cimamaker.com)

[![GitHub Stars](https://img.shields.io/github/stars/gustavoeenriquez/MakerAi?style=social)](https://github.com/gustavoeenriquez/MakerAi)
[![GitHub Issues](https://img.shields.io/github/issues/gustavoeenriquez/MakerAi)](https://github.com/gustavoeenriquez/MakerAi/issues)
[![License](https://img.shields.io/github/license/gustavoeenriquez/MakerAi)](LICENSE.txt)
[![Telegram](https://img.shields.io/badge/Join-Telegram%20Chat-blue.svg)](https://t.me/+7LaihFwqgsk1ZjQx)
[![Delphi Supported Versions](https://img.shields.io/badge/Delphi%20Support-11%20Alexandria%20to%2013%20Florence-blue.svg)](https://www.embarcadero.com/products/delphi)

**MakerAI 3.1 is the definitive AI orchestration framework for Delphi developers.**

Now featuring support for **GPT-5.1**, **Gemini 3.0**, and **Claude 4.5**, MakerAI allows you to design and deploy intelligent, self-learning, and context-aware systems natively in Delphi with full support for RAG, Knowledge Graphs, MCP Servers, and autonomous agents.

---

## 🚀 What's New in MakerAI 3.1 (Dec 2025)

MakerAI 3.1 represents a massive leap forward in native AI capabilities:

- 🧠 **Next-Gen Models:** Full support for **OpenAI GPT-5.1** & **Sora 2** (Video), **Gemini 3.0** (NanoBanana & Veo 3), and **Claude 4.5**.
- 🎨 **Professional FMX UI:** New set of FireMonkey visual components for building modern, multimodal chat interfaces (Text + Voice + Vision) effortlessly.
- 🎯 **High-Precision RAG:** Added **Embedding Rerank** functionality to drastically improve search relevance in large document sets.
- 🕸️ **RagGraph Engine:** A new graph-based knowledge engine that goes beyond vector similarity.
- 🔗 **MCP Server Framework:** Create your own MCP Servers natively using **SSE** or **DataSnap**.

---

## 📊 Feature Support Matrix

Overview of capabilities across supported providers in MakerAI 3.1.

| Feature Category | Feature | OpenAI (GPT-5.1) | Claude (4.5) | Gemini (3.0) | Ollama (Local) | LM Studio | Groq | DeepSeek | Kimi 2 |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| **Chat** | Text Generation | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| | JSON Mode | ✅ | ❌ | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ |
| | JSON Schema | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| **Input** | Image Input | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| | PDF / Files | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ |
| | Video Input | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ |
| | Audio Input | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **Output (Gen)** | Image Gen | ✅ | ❌ | ✅ | ❌ | ❌ | ✅ | ❌ | ❌ |
| | Video Gen (**Sora 2/Veo 3**) | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ |
| | Audio (TTS) | ⚠️ | ❌ | ⚠️ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **Tools** | Function Calling | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| | Web Search | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | ❌ | ❌ |
| | Code Interpreter | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ |
| | Computer Use | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **MakerAI Native** | **RAG (Vector/Graph)** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| | **MCP Client/Server**| ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| | **Agents/Voice** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

> **Legend:** ✅ = Full Support (Sync & Async) | ⚠️ = Partial Support (Sync Only) | ❌ = Not Supported by Provider/Driver

---

## 🛠️ MakerAI 3.1 Core Modules & Components

The suite is composed of non-visual and visual components designed for maximum modularity.

### 🌐 Universal Connectivity & LLMs (AiConnection V3)

| Component | Description |
|:---|:---|
| `TAIChatConnections` | Unified universal connector for managing messages, contexts, and sessions with any LLM provider. |
| `TAIChatOpenAi` | **Updated:** Support for **GPT-5.1** and **Sora 2** (Video Generation). |
| `TAIChatGemini` | **Updated:** Support for Gemini 3.0 (**NanoBanana, Veo 3**). |
| `TAIChatClaude` | **Updated:** Driver for Anthropic **Claude 4.5**. |
| `TAIChatOllama` | **Updated:** Native API support for local models. |
| `TAIChatLMStudio` | **NEW:** Driver for connecting to local LM Studio instances. |
| `TAIChatKimi` | **NEW:** Driver for Kimi 2 AI. |
| `TAIChatGroq` | Driver for Groq (LPU high-speed processing). |
| `TAIChatDeepSeek` | Driver for DeepSeek AI. |

### 🎨 Visual Multimodal Interface (FMX Only)

| Component | Description |
|:---|:---|
| `TAIChatList` | A professional, visually rich chat container for FireMonkey. Supports markdown rendering, image previews, and video playback inline. |
| `TAIMultimodalInput` | An all-in-one input bar that handles text, voice recording (whisper-ready), and attachment dropping/selection seamlessly. |
| `VoiceMonitor` | Engine for real-time voice processing, including wake-word detection and progressive transcription. |

### 🧠 Knowledge & Memory Engines (RAG)

| Component | Description |
|:---|:---|
| `RAGVector` | Vector and embedding-based RAG system. **New:** Includes **Rerank** functionality to re-order search results for maximum context precision. |
| `RAGGraph` | **NEW:** Graph-based RAG system. Creates and queries knowledge graphs for deep context understanding and relationship mapping. |
| `GraphDB` | Component for administering and querying the graph-based knowledge database. |

### 📞 Tools & Protocol (MCP & Functions)

| Component | Description |
|:---|:---|
| `TAIFunctions` | Enables and manages the *Function Calling* system for the LLM to interact with native Delphi functions. |
| **MCP Client** | Client implementation to consume external MCP servers. |
| `MCPServer` | **NEW:** Framework to build your own MCP Servers in Delphi. Supports **SSE (Server-Sent Events)** and **DataSnap** protocols. |

---

## 📦 Detailed Installation Guide (Modular V3.0)

Follow these steps carefully to ensure a correct installation of the core packages.

### Step 1: Get the Source Code

```bash
git clone https://github.com/gustavoeenriquez/MakerAi.git
```

### Step 2: Configure Paths in the Delphi IDE

Add the following subfolders to your **Library Path** (Tools > Options > Language > Delphi > Library):

```
...\MakerAi\Source\Agents
...\MakerAi\Source\Chat
...\MakerAi\Source\ChatUI       <-- (FMX Visual Components)
...\MakerAi\Source\Core
...\MakerAi\Source\Design
...\MakerAi\Source\MCPClient
...\MakerAi\Source\MCPServer
...\MakerAi\Source\Packages
...\MakerAi\Source\RAG
...\MakerAi\Source\Resources
...\MakerAi\Source\Tools
...\MakerAi\Source\Utils
```

### Step 3: Compile and Install the Packages

Compile and Install in this specific order:

1.  `MakerAi.dpk` (Runtime Core)
2.  `MakerAi.RAG.Drivers.dpk` (Database Connectors)
3.  `MakerAi.UI.dpk` (FMX Visual Components)
4.  `MakerAiDsg.dpk` (Design-Time Editors)

---

## 💡 Usage: Connecting to GPT-5.1

```delphi
procedure TForm1.Button1Click(Sender: TObject);
var
  Response: string;
begin
  // Ensure DriverName is 'OpenAI' and Model is 'gpt-5.1-turbo'
  AiConnection1.DriverName := 'OpenAI';
  AiConnection1.Model := 'gpt-5.1-turbo';
  
  Response := AiConnection1.AddMessageAndRun('Explain the theory of relativity in one sentence.', 'user', []);
  Memo1.Lines.Add('GPT-5.1: ' + Response);
end;
```

---

## ⚠️ Known Issues & Limitations

Please be aware of the following behaviors in version 3.1.0:

1.  **MCP SSE Server:** The Server-Sent Events (SSE) implementation for the MCP Server framework is currently experimental. Connectivity is not yet 100% functional, and you may experience intermittent drops.
2.  **Linux Compilation:** Compiling for Linux currently requires significant manual adjustments to library paths and dependencies. Seamless support is planned for a future patch.
3.  **Delphi Compatibility:**
    -   **Full Support:** Delphi 11 Alexandria, 12 Athens, and 13 Florence.
    -   **Limited Support:** Delphi 10.4 Sydney (some advanced features or new FMX components may not function as expected).

---

## 🔄 Changelog

### Version 3.1.0 (December 3, 2025) - OFFICIAL RELEASE

-   **New Model Support:**
    -   **OpenAI:** Added full support for `Responses` API, `gpt-5.1` models, and **Sora 2** (Video).
    -   **Gemini:** Added support for Gemini 3.0 series (**NanoBanana**, **Veo 3**).
    -   **Claude:** Added support for Claude 4.5 API.
-   **Visual Interface (FMX):**
    -   Added professional-grade FMX visual components: `TAIChatList` and `TAIMultimodalInput` for rich text/voice/video interaction.
-   **RAG Engine:**
    -   Introduced **Rerank** capabilities in embeddings to improved search precision.
    -   Introduced `RAGGraph` for Knowledge Graph based retrieval.
-   **New Drivers:**
    -   Added `TAIChatLMStudio` for local inference compatibility.
    -   Added `TAIChatKimi` (Kimi 2).
    -   Updated `TAIChatOllama` to support the new native local API.
-   **MCP Protocol:**
    -   Added **MCP Server** framework with support for **SSE (Server-Sent Events)** and **MakerAI+DataSnap** protocols.

### Version 2.6.0 (August 2025)
-   Added Delphi 11 Alexandria official support.
-   Preliminary support for Ollama async calls.

---

## 🤝 Contributing

Contributions are welcome. Please open an issue or submit a pull request on GitHub.

---

## 💬 Community & Support

- **Official Website:** [https://makerai.cimamaker.com](https://makerai.cimamaker.com)
- **Email:** gustavoeenriquez@gmail.com

---

## 📜 License

This project is licensed under the MIT License. See the [LICENSE.txt](LICENSE.txt) file for details.