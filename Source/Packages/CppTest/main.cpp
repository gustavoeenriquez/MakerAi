// MakerAI C++ header parse test for bcc64x (Clang 20)
// Tests that Delphi-generated .hpp files are syntactically valid
// under RAD Studio 13.1 (C++23 default, Win64x platform)

#include <uMakerAi.Core.hpp>
#include <uMakerAi.Chat.Messages.hpp>
#include <UMakerAi.Chat.hpp>
#include <uMakerAi.Chat.AiConnection.hpp>

int _tmain(int argc, _TCHAR* argv[])
{
    // Verify TAiCapability enum values are accessible
    Umakerai::Core::TAiCapabilities caps;
    caps << Umakerai::Core::TAiCapability::cap_Image;
    caps << Umakerai::Core::TAiCapability::cap_Reasoning;

    bool hasImage = caps.Contains(Umakerai::Core::TAiCapability::cap_Image);

    return 0;
}
