Set Args = WScript.Arguments
For i = 0 to Args.Count - 1
    Params = Params + "  " + Args(i)
Next

MsgBox "VBS-Script update has been downloaded and successfully started "+Chr(10)+"Update als VBS-Skript heruntergeladen und gestartet."+Chr(10)+"Parameters are:" +Params
