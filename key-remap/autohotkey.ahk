#HotIf !WinActive("ahk_class Emacs")
+space::Send "{vk15sc138}"

#HotIf WinActive("ahk_class UnrealWindow")
+8::Send("{NumpadMult}")
$-::Send("{NumpadSub down}")
$- up::Send("{NumpadSub up}")

#HotIf
RWin::Send "{AppsKey}"
