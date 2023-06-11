###################################################################
#
# A Tiny module exposing PowerShell commands
#
###################################################################

@{
    GetCommand      = Get-Command Get-Command

    GetVariable     = Get-Command Get-Variable

    SetVariable     = Get-Command Set-Variable

    GetModule       = Get-Command Get-Module

    ImportModule    = Get-Command Import-Module

    InvokeItem      = Get-Command Invoke-Item

    GetCimInstance  = Get-Command Get-Ciminstance

    ls              = Get-Command Get-ChildItem

    grep            = Get-Command Select-String

    FormatHex       = Get-Command Format-Hex

    AddType         = Get-Command Add-Type

    GetService      = Get-Command Get-Service

    GetCimClass     = Get-Command Get-CimClass

    ipconfig        = Get-Command ipconfig.exe

    GetNetAdapter   = Get-Command Get-NetAdapter

    GetHelp         = Get-Command Get-Help
}

