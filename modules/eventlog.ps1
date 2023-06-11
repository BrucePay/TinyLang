###################################################################
#
# Event Log wrapper module.
#
###################################################################

Import-Module WindowsCompatibility
Import-WinMOdule microsoft.powershell.management

@{
    Clear  = get-command Clear-EventLog

    Get    = get-command Get-EventLog

    Limit  = get-command Limit-EventLog

    New    = get-command New-EventLog

    Remove = get-command Remove-EventLog

    Show   = get-command Show-EventLog

    Write  = get-command Write-EventLog
}
