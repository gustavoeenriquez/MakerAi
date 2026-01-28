# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

ChatUI is a Delphi FMX visual components library providing chat interface components for AI applications. Part of the MakerAI framework. All components are registered in the 'MakerAI UI' component palette.

## Components

### TChatList (`uMakerAi.UI.ChatList.pas`)
Scrollable chat message container managing bubble layout and persistence.
- Wraps TVertScrollBox with internal TFlowLayout (`FContentLayout`)
- Handles message grouping (consecutive same-user messages via `GroupConsecutiveMessages`)
- JSON-based serialization: `SaveToStream()` / `LoadFromStream()` with version control
- Properties: `InboundColor`, `OutboundColor`, `MaxBubbleWidthPercent` (0.1-1.0), `AutoScroll`
- Events forwarded from bubbles: `OnBubbleAvatarClick`, `OnBubbleMoreOptionsClick`, `OnMediaFileDblClick`
- Attachment context menu: Assign `AttachmentPopupMenu` property, access `ActiveMediaFile` in menu handlers

### TChatBubble (`uMakerAi.UI.ChatBubble.pas`)
Individual message bubble with rich content support.
- Custom-painted control with tail styles: `tsSimple`, `tsOrganic`, `tsComic`, `tsSharp`
- Content via `FContentLayout`: TMemo (text), TImage (images), TLayout (documents)
- `AddContent(Text, MediaFiles)` - Clears and rebuilds content (media first, text last)
- `AppendText(Fragment)` - For streaming responses; creates TMemo if needed, calls `OnRecalculateRequired`
- Header section: Avatar (via `Images`/`ImageIndex` or direct `Avatar` bitmap), Title, Timestamp
- `TailPosition`: `tpLeft` (inbound) / `tpRight` (outbound)
- Serialization: `ToJsonObject()` / `LoadFromJsonObject()` with Base64-encoded media

### TChatInput (`uMakerAi.UI.ChatInput.pas`)
Multi-modal input control with text, media attachments, and voice.
- Regions: Image carousel (top, visible when attachments exist), TMemo (center), Button bar (bottom)
- Attachment handling: File dialog, clipboard paste, drag-drop (files + URLs), screenshot
- `ValidExtensions` property: Comma-separated list (e.g., 'jpg,jpeg,png,pdf,mp3')
- Voice integration via `VoiceMonitor: TAIVoiceMonitor` property
- Main event: `OnSendEvent(Sender, Prompt, MediaFiles, AudioStream)`
- `OnCancel` event triggered when user clicks send button during busy state
- `Busy` property: When True, shows cancel icon and disables input
- Call `ChatInput.Busy := False` after processing to re-enable input

### TScreenCapture (`uMakerAi.Utils.ScreenCapture.pas`)
Screen region selection and capture utility (Windows/macOS).
- `TScreenCapture.SelectArea(out Rect)`: Shows overlay form for region selection, returns True if selected
- `TScreenCapture.CaptureArea(Rect)`: Captures specified screen region as TBitmap
- Supports multi-monitor setups via virtual screen metrics

## Architecture Patterns

**Event Forwarding**: Bubble events propagate through TChatList to consumers.

**Content Layout**: Both TChatBubble and TChatInput use TLayout containers for flexible child positioning.

**Media Abstraction**: Uses `TAiMediaFile` from Core layer for uniform image/document/audio handling.

**State Management**: TChatInput `Busy` property controls UI state (send vs cancel button).

**Size Calculation**: `RecalculateSize(MaxWidth)` measures TMemo content, adds header/padding/icons, constrains to bounds.

**Resource Loading**: Icons loaded from embedded `UIResources.res` via `LoadImageFromResource()`. Document type icons (pdf, doc, audio, video, image, unknown) stored in `FDocIcons` dictionary.

## Thread Safety

- UI updates during streaming must use `TThread.Queue` or `TThread.Synchronize`
- `TChatInput.DoSendEvent` sets `Busy := True` synchronously before firing event
- `TChatList.ScrollToBottom` uses `TThread.ForceQueue` to ensure layout is complete

## Serialization Format

Chat history is persisted as JSON with structure:
```json
{
  "version": 1,
  "bubbles": [
    {
      "username": "User",
      "title": "User",
      "timestamp": "14:30",
      "tailPosition": 0,
      "bubbleColor": 4294967295,
      "imageIndex": -1,
      "text": "Hello",
      "mediaFiles": [...]
    }
  ]
}
```

## Integration

```delphi
// Display messages with attachments
ChatList.AddBubble('Hello', 'User', nil, True);  // Inbound (left tail)
ChatList.AddBubble('Response', 'Assistant', MediaFiles, False);  // Outbound (right tail)

// Handle input
procedure TForm1.ChatInput1SendEvent(Sender: TObject; const Prompt: string;
  MediaFiles: TAiMediaFiles; AudioStream: TMemoryStream);
begin
  ChatList.AddBubble(Prompt, 'You', MediaFiles, True);
  // Process with TAiChat driver, then:
  ChatList.AddBubble(Response, 'Assistant', nil, False);
  ChatInput.Busy := False;  // Re-enable input
end;

// Streaming response
Bubble := ChatList.AddBubble('', 'Assistant', nil, False);
// In OnReceiveData callback:
TThread.Queue(nil, procedure begin
  Bubble.AppendText(Fragment);  // Progressive rendering
end);

// Save/restore chat
ChatList.SaveToStream(FileStream);
ChatList.LoadFromStream(FileStream);

// Handle attachment double-click
procedure TForm1.ChatList1MediaFileDblClick(Sender: TObject;
  const ABubble: TChatBubble; const AMediaFile: TAiMediaFile);
begin
  ShellExecute(0, 'open', PChar(AMediaFile.FullFileName), nil, nil, SW_SHOW);
end;
```

## Key Constants

```delphi
// TChatBubble layout
HEADER_HEIGHT = 30;
AVATAR_SIZE = 24;
ICON_SIZE = 16;
CONTENT_PADDING = 8;

// TChatInput
IMAGE_WIDTH = 50;    // Attachment thumbnail size
MAX_MEMO_HEIGHT = 200;
MIN_FRAME_HEIGHT = 60;
```

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
