# MakerAI 3.x Suite — The AI Operating System for Delphi

🌐 **Official Website:** [https://makerai.cimamaker.com](https://makerai.cimamaker.com)

[![GitHub Stars](https://img.shields.io/github/stars/gustavoeenriquez/MakerAi?style=social)](https://github.com/gustavoeenriquez/MakerAi)
[![GitHub Issues](https://img.shields.io/github/issues/gustavoeenriquez/MakerAi)](https://github.com/gustavoeenriquez/MakerAi/issues)
[![License](https://img.shields.io/github/license/gustavoeenriquez/MakerAi)](LICENSE.txt)
[![Telegram](https://img.shields.io/badge/Join-Telegram%20Chat-blue.svg)](https://t.me/+7LaihFwqgsk1ZjQx)
[![Delphi Supported Versions](https://img.shields.io/badge/Delphi%20Support-11%20Alexandria%20to%2013%20Florence-blue.svg)](https://www.embarcadero.com/products/delphi)

**MakerAI is the definitive AI orchestration framework for Delphi developers.**

Supporting **GPT-5.1**, **Gemini 3.0**, **Claude 4.5**, local models, and hybrid deployments, MakerAI allows you to design and deploy intelligent, self-learning, and context-aware systems natively in Delphi with full support for **RAG**, **Knowledge Graphs**, **MCP Servers**, **Agents**, and **Native Capability Tools (ChatTools)**.

---

## 🚀 What's New in MakerAI 3.2 (Capability Release)

MakerAI **3.2** introduces a major architectural milestone: **native capability tools** integrated directly into the core framework.

### 🔧 Native ChatTools Framework (NEW)

MakerAI now includes a **first-class, native ChatTools framework** located in:

```
AiMaker\Source\Tools
```

This framework allows AI models to *see, read, hear, speak, search, and act* by attaching deterministic real-world skills such as:

- 📄 PDF processing
- 👁️ Vision & OCR pipelines
- 🔊 Speech (TTS / STT)
- 🌐 Web search & external data access
- 🧮 Code interpretation & execution helpers

**Key principles:**

- Clear separation between **Intelligence (LLMs)** and **Capabilities (Tools)**
- Deterministic, explainable orchestration
- No hard dependency on expensive multimodal models
- Full compatibility with local and cloud providers

> 📌 Advanced and experimental tools are documented separately in the *MakerAI ChatTools* repository. This README focuses only on the **native, bundled framework**.

---

## 🚀 What's New in MakerAI 3.1.001 (Dec-10-2025)

MakerAI 3.1 represents a massive leap forward in native AI capabilities:

- 🧠 **Next-Gen Models:** Full support for **OpenAI GPT-5.1** & **Sora 2** (Video), **Gemini 3.0** (NanoBanana & Veo 3), and **Claude 4.5**.
- 🎨 **Professional FMX UI:** New set of FireMonkey visual components for building modern, multimodal chat interfaces (Text + Voice + Vision).
- 🎯 **High-Precision RAG:** Added **Embedding Rerank** functionality to drastically improve search relevance.
- 🕸️ **RagGraph Engine:** A graph-based knowledge engine that goes beyond vector similarity.
- 🔗 **MCP Server Framework:** Create your own MCP Servers natively using **SSE** or **DataSnap**.

---

## 📊 Feature Support Matrix (3.1+)

| Feature Category | Feature | OpenAI (GPT-5.1) | Claude (4.5) | Gemini (3.0) | Ollama (Local) | LM Studio | Groq | DeepSeek | Kimi 2 |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| **Chat** | Text Generation | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| | JSON Mode | ✅ | ❌ | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ |
| | JSON Schema | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| **Input** | Image Input | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| | PDF / Files | ✅ | ✅ | ✅ | ⚠️ | ⚠️ | ❌ | ❌ | ❌ |
| **Output** | Image Gen | ✅ | ❌ | ✅ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **MakerAI Native** | **RAG (Vector / Graph)** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| | **MCP Client / Server** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| | **Native ChatTools** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

> **Legend:** ✅ = Full Support | ⚠️ = Tool-Assisted | ❌ = Not Supported

---

## 🛠️ Core Modules Overview

### 🌐 Universal Connectivity & LLMs

- `TAIChatConnections`
- `TAIChatOpenAi`
- `TAIChatGemini`
- `TAIChatClaude`
- `TAIChatOllama`
- `TAIChatLMStudio`
- `TAIChatKimi`
- `TAIChatGroq`
- `TAIChatDeepSeek`

### 🧠 Knowledge & Memory (RAG)

- `RAGVector` (with Rerank)
- `RAGGraph`
- `GraphDB`

### 🧩 Native Capability Tools (ChatTools) — v3.2+

- `IAiPdfTool`
- `IAiVisionTool`
- `IAiSpeechTool`
- `IAiWebSearchTool`
- `IAiCodeInterpreter`

Designed for **standalone**, **bridge**, **function-calling**, or **native multimodal** orchestration modes.

### 🔗 Protocols & Integration

- MCP Client
- MCP Server (SSE / DataSnap)

---

## 📦 Installation Guide (Unchanged)

> The installation process remains identical to 3.1.

```bash
git clone https://github.com/gustavoeenriquez/MakerAi.git
```

Add all `Source/*` folders to the Delphi Library Path and compile packages in the documented order.

---

## 🔄 Changelog

### Version 3.2.0 (Capability Release)

- Introduced **Native ChatTools Framework**
- Unified deterministic tool orchestration
- Tool-assisted multimodal support for local models
- Stable interfaces for third-party extensions

### Version 3.1.0 (December 2025)

- GPT-5.1, Gemini 3.0, Claude 4.5 support
- FMX Multimodal UI
- RAG Rerank + Graph
- MCP Server Framework

---

## 🤝 Contributing

Contributions are welcome via GitHub issues or pull requests.

---

## 💬 Community & Support

- **Website:** https://makerai.cimamaker.com
- **Email:** gustavoeenriquez@gmail.com

---

## 📜 License

MIT License — see `LICENSE.txt` for details.

