###################################################################################
#
# The Tiny Language Interpreter
# =============================
#
# Copyright Bruce Payette (c) 2023
#
# This PowerShell script implements a dynamic functional/imperative
# scripting language named 'Tiny'.
#
# Documentation and tests are embedded in this script as comments. The
# documentation comments are:
#    #O for operation documentation
#    #H for function documentation
#    #L for TinyList methods.
#    #G for the grammar
#
# The script 'tinydoc.tiny' will extract these comments and generate
# an HTML page containing the information.
#
# Test comments are of the form
#    #T <testName> <testText>
#    #M <testName> <MacOS-specific test text>
#    #X <testName> <Linux-specific test text>
#    #W <testName> <Windows-specific test text>
#    #U <testName> <UNIX (non-Windows) test text>
#
# The script 'tinytest.tiny' will extract and run these tests as well as run additional
# tests in the 'tests' subdirectory. Tests in the subdirectory are written in a Tiny
# DSL defined by the TinyTestUtils module.
#
###################################################################################

using namespace System
using namespace System.Text
using namespace System.Text.RegularExpressions
using namespace System.Collections
using namespace System.Collections.Generic
using namespace System.Collections.ObjectModel
using namespace System.Collections.Specialized
using namespace System.Management.Automation
using namespace System.Management.Automation.RunSpaces
using namespace System.Management.Automation.Internal

[CmdletBinding(DefaultParameterSetName='Repl')]
param (
    [Parameter(Position=0, ParameterSetName='script')]
    [string]
        $scriptToRun,

    [Parameter(ParameterSetName='expression')]
    [string]
        $Expression,

    [Parameter()]
    [Switch]
        $Time,

    [Parameter()]
    [Switch]
        $DetailedErrors,

    [Parameter(ParameterSetName='norepl')]
    [switch]
        $NoRepl,

    [Parameter(Position=1, ParameterSetName='script', ValueFromRemainingArguments)]
    [object[]]
        $TinyScriptArguments
)

# A debugging function
function say {
    [console]::writeline("$args")
}

Set-StrictMode -version latest

[regex]::CacheSize = 20

# Make sure the 'matches' variable always exists
Set-Variable -Name "matches" -Value $null

# Value returned from a return statement
$FunctionReturnValue = $null

###################################################################################
#
# The 'tiny' Tokenizer and Token classes
#

# The various kinds of tokens in the language
enum TokenKind {
    keyword         = 0
    name            = 1
    arrow           = 2
    lcurly          = 3
    rcurly          = 4
    lparen          = 5
    rparen          = 6
    lsquare         = 7
    rsquare         = 8
    orbar           = 9
    colon           = 10
    patOp           = 11
    semicolon       = 12
    number          = 13
    string          = 14
    regex           = 15
    badstring       = 16
    typeliteral     = 17
    dot             = 18
    comma           = 19
    operator        = 20
    elseif          = 21
    else            = 22
    in              = 23
    catch           = 24
    finally         = 25
    StartPP         = 26
    EndPP           = 27
    PinnedVar       = 28
    UnaryOperator   = 29
    dummy           = 30
    max_token       = 30
}

#######################################################################
#
# Class representing a Tiny token.
#
class Token
{
    Token([TokenKind] $kind, [string] $value, [int] $index, [int] $LineNo, [string] $src, [string] $filename) {
        $this.kind     = $kind
        $this.Value    = $value
        $this.Index    = $Index
        $this.LineNo   = $lineno
        $this.Src      = $src
        $this.Filename = $fileName
    }

    # Create a dummy token to use as a placeholder
    Token() {
        $this.kind     = [TokenKind]::dummy
        $this.Value    = [tokenizer]::CurrentString()
        $this.Index    = [Tokenizer]::Offset
        $this.LineNo   = [Tokenizer]::CurrentLineNumber
        $this.Src      = [tokenizer]::string
        $this.FileName = [scopes]::Getvariable('__file')
    }

    [TokenKind] $Kind
    [string]    $Value
    [String]    $Src
    [int]       $index
    [int]       $LineNo
    [string]    $FileName

    [string] ToString() { return "[Token(Kind=$($this.Kind),Value='$($this.Value)')]" }
}

#######################################################################
#
# A class representing the state of the tokenizer.
# Since the tokenizer is static, it's state must be saved away
# when tokening a new file in midstream which is what happens
# when a script loads a module or another script. Note that modules
# are loaded at script parse time.
#
class TokenizerState {

    TokenizerState($String, $FileName, $Offset, $currentLineNumber) {
        $this.String    = $string
        $this.FileName  = $filename
        $this.Offset    = $offset
        $this.CurrentLineNumber = $currentLineNumber
    }

    [string]
        $String

    # The filename the string came from
    [string]
        $FileName

    # The current offset in the string being tokenized
    [int]
        $Offset

    # The current line number in the file being parsed
    [int]
        $currentLineNumber
}

#######################################################################
#
# The Tiny tokenizer 
#

class Tokenizer
{
    # The string being tokenized
    hidden static [string]
        $String

    # The filename the string came from
    hidden static [string]
        $FileName

    # The current offset in the string being tokenized
    hidden static [int]
        $Offset

    # Utility method for compiling regular expressions for the token table.
    hidden static [regex] CompileRegex ($str) {
        return [regex]::new("\G($str)", 'IgnoreCase,CultureInvariant,Compiled')
    }

    # Values for matching the tokens in the language.
    # THIS TABLE MUST BE IN THE SAME ORDER AS THE TokenKind ENUM
    hidden static
        $TokenTable = @(
            <# [TokenKind]::keyword     0 #>  [tokenizer]::compileregex('fn|def|if|while|foreach|break|continue|' +
                                                'return|matchlist|match|throw|try|undef|import|const')

            <# [TokenKind]::name        1 #>  [tokenizer]::compileregex('[_a-z][a-z0-9_]*')
            <# [TokenKind]::arrow       2 #>  '->'
            <# [TokenKind]::lcurly      3 #>  '{'
            <# [TokenKind]::rcurly      4 #>  '}'
            <# [TokenKind]::lparen      5 #>  '('
            <# [TokenKind]::rparen      6 #>  ')'
            <# [TokenKind]::lsquare     7 #>  '['
            <# [TokenKind]::rsquare     8 #>  ']'
            <# [TokenKind]::orbar       9 #>  '|'
            <# [TokenKind]::colon      10 #>  ':'
            <# [TokenKind]::patOp      11 #>  '::'
            <# [TokenKind]::semicolon  12 #>  ';'
            <# [TokenKind]::number     13 #>  [tokenizer]::compileregex('0x[0-9a-f_]+|' + # hex integers
                                                '[+-]{0,1}\.[0-9_]+(e[+-]{0,1}[0-9]+){0,1}|' +
                                                '[+-]{0,1}[0-9_]+i|' + # BigInts
                                                '[+-]{0,1}[0-9][0-9_]*(\.[0-9_]+){0,1}(e[+-]{0,1}[0-9]+){0,1}')

            <# [TokenKind]::string     14 #>  [tokenizer]::compileregex('"([^"]|"")*"|' +
                                                "'([^']|'')*'|" +
                                                '`[^` \t\r\n]+( |$)')

            <# [TokenKind]::regex      15 #>  [tokenizer]::compileregex('r/([^/]|//)*/(c)?(:[_a-z][_a-z0-9]*)?')

            <# [TokenKind]::badstring  16 #>  [tokenizer]::compileregex('"([^"]|"")*$|' +
                                                "'([^']|'')*$")  # unterminated string

            <# [TokenKind]::typeliteral 17 #> [tokenizer]::compileregex('\[<[a-z0-9., \[\]]+>\]')

            <# [TokenKind]::dot        18 #>  '.'
            <# [TokenKind]::comma      19 #>  ','

            # all the binary operators; order is important - shorter operators must go last
            <# [TokenKind]::operator   20 #>
                                              [tokenizer]::compileregex( '`[a-z]+`|-f|\+=|\*=|-=|/=|\?=|' +
                                                '<=|>=|==|!=|=|!~|/~|-~|~|\*~|\+\+|\:\+|:>|<:|!:>|!<:|>|<|' +
                                                '&&|\|\||\?\?|\.\.|isnot|is|as|\*\*|do|then|\:\:\}|\:\:|' +
                                                '\|>|\?\*\.|\?\.|\*\.|\.|\[<|\?\[|\[|\{|\(|->|[+*/%-]' )

            <# [TokenKind]::elseif     21 #>  'elseif'
            <# [TokenKind]::else       22 #>  'else'
            <# [TokenKind]::in         23 #>  'in'
            <# [TokenKind]::catch      24 #>  'catch'
            <# [TokenKind]::finally    25 #>  'finally'
            <# [TokenKind]::StartPP    26 #>  '{::'
            <# [TokenKind]::EndPP      27 #>  '::}'
            <# [TokenKind]::PinnedVar  28 #>  [tokenizer]::compileregex('\^[_a-z][a-z0-9_]*')
            <# [TokenKind]::UnaryOperator 29 #> [tokenizer]::compileregex('->|[!+-]')
            <# [Tokenkind]::dummy      30 #>  'dummy'
        )

    # Maintains a count of lines in the text being processed.
    static [int] $CurrentLineNumber

    # Start tokenizing a new string
    static Start ($string) {
        [tokenizer]::String = $string
        [tokenizer]::Offset = 0
        [tokenizer]::CurrentLineNumber = 0
    }

    # used when processing module imports where the current
    # token stream is pushed so the new token stream can be processed.
    static PushTokenizerState() {
        [Tokenizer]::TokenizerStack.Push(
            [TokenizerState]::New(
                [Tokenizer]::string,
                [Tokenizer]::filename,
                [Tokenizer]::offset,
                [tokenizer]::CurrentLineNumber
                ))
    }

    static PopTokenizerState() {
        $state = [Tokenizer]::TokenizerStack.Pop()
        [Tokenizer]::String   = $state.String
        [Tokenizer]::FileName = $state.FileName
        [Tokenizer]::Offset   = $state.Offset
        [tokenizer]::CurrentLineNumber = $state.CurrentLineNumber
    }

    static [Stack[TokenizerState]] $TokenizerStack =  [Stack[TokenizerState]]::new()

    #
    # Method to eat whitespace and comments in the input stream. Handles
    # both line and block comments. It also increments the line count as it
    # hits newlines.
    #
    static EatWhitespace() {
        $inComment = $false
        $inBlockComment = $false
        while ([tokenizer]::Offset -lt [tokenizer]::String.Length) {
            $cc = [tokenizer]::String[[tokenizer]::Offset]
            if ($inBlockComment) {
                if ([tokenizer]::Offset -lt [tokenizer]::String.Length-1 -and
                    $cc -eq '#' -and
                    [Tokenizer]::String[[tokenizer]::Offset+1] -eq '>')
                {
                    [Tokenizer]::Offset++
                    $inBlockComment = $false
                }
            }
            elseif ($inComment) {
                if ($cc -eq "`n") {
                    [tokenizer]::CurrentLineNumber++
                    $inComment = $false
                }
            }
            elseif ($cc -eq '#') {
                $inComment = $true
            }
            elseif ($cc -eq '<' -and
                    [tokenizer]::Offset -lt [tokenizer]::String.Length-1 -and
                    [Tokenizer]::String[[Tokenizer]::Offset+1] -eq '#')
            {
                [Tokenizer]::Offset++
                $InBlockComment = $true
            }
            elseif ($cc -eq "`n") {
                [tokenizer]::CurrentLineNumber++
            }
            elseif (-not [char]::IsWhiteSpace($cc)) {
                break;
            }
            [tokenizer]::Offset++
        }
    }

    #
    # The main tokenizer method. You call it passing in the Kind of the token you're looking for
    # If the next token in the stream is of the right kind, then it will be returned otherwise
    # null will be returned. This means that tokens are contextual.
    #
    static [Token] Next ([TokenKind] $TokenKindExpected) {
        if ([tokenizer]::offset -ge [tokenizer]::String.length) {
            return $null
        }

        [Tokenizer]::EatWhitespace()
        $tokenStart = [tokenizer]::Offset

        # Handle tokens with tricky rules
        switch ($TokenKindExpected) {
            ([TokenKind]::LSquare) {
                if ([Tokenizer]::String[[Tokenizer]::Offset] -eq '[') {
                    if ([tokenizer]::offset + 1 -lt [tokenizer]::string.length) {
                        $c = [tokenizer]::string[[tokenizer]::Offset + 1]
                        switch ($c) {
                            '<' { # [<typename>]
                                return $null
                            }
                        }
                    }

                    [tokenizer]::Offset++
                    return [token]::new($TokenKindExpected, '[', $tokenStart, [tokenizer]::CurrentLineNumber,
                                [tokenizer]::string, [tokenizer]::FileName)
                }
                else {
                    return $null
                }
            }

            ([TokenKind]::TypeLiteral) {
                if ([Tokenizer]::String[[Tokenizer]::Offset] -eq '[') {
                    if ([tokenizer]::offset + 1 -lt [tokenizer]::string.length) {
                        $c = [tokenizer]::string[[tokenizer]::Offset + 1]
                        if ($c -ne '<') {
                            return $null
                        }
                        # definitely have a type literal now, if the regex doesn't match, it's an error.
                        $pat = [Tokenizer]::TokenTable[$TokenKindExpected]
                        $res = $pat.Match([tokenizer]::string, [tokenizer]::offset)
                        if ($res.Success) {
                            $val = $res.Value
                            [tokenizer]::offset += $res.length
                            return [token]::new($TokenKindExpected, $val, $tokenStart, [tokenizer]::CurrentLineNumber,
                                [tokenizer]::string, [tokenizer]::FileName)
                        }
                        else {
                            errorMessage -dummy "Malformed type literal"
                        }
                    }
                }
                else {
                    return $null
                }
            }

            ([TokenKind]::Colon) {
                if ([Tokenizer]::String[[Tokenizer]::Offset] -eq ':') {
                    if ([Tokenizer]::offset + 1 -lt [Tokenizer]::string.length) {
                        $c = [Tokenizer]::string[[Tokenizer]::Offset + 1]
                        switch ($c) {
                            ':' { # ::
                                return $null
                            }
                            '>' { # :>
                                return $null
                            }
                            '+' { # :+
                                return $null
                            }
                        }
                        [Tokenizer]::Offset++
                    }
                    return [Token]::new($TokenKindExpected, ':', $tokenStart, [tokenizer]::CurrentLineNumber,
                                [Tokenizer]::string, [Tokenizer]::FileName)
                }
                else {
                    return $null
                }
            }

            ([TokenKind]::Orbar) {
                if ([Tokenizer]::String[[Tokenizer]::Offset] -eq '|') {
                    if ([tokenizer]::offset + 1 -lt [tokenizer]::string.length) {
                        $c = [tokenizer]::string[[tokenizer]::Offset + 1]
                        if ($c -eq '|') {
                            return $null
                        }
                        [tokenizer]::Offset++
                    }
                    return [token]::new($TokenKindExpected, '|', $tokenStart, [tokenizer]::CurrentLineNumber,
                            [tokenizer]::string, [tokenizer]::FileName)
                }
                else {
                    return $null
                }
            }
        }

        # Handle simple tokens with no ambiguity that just require a string match
        $pat = [Tokenizer]::TokenTable[$TokenKindExpected]

        if ($pat -is [string]) {
            $str = [tokenizer]::string
            $len = $str.length
            $toffset = [tokenizer]::Offset
            if ($toffset + $pat.Length -gt $len) {
                return $null
            }

            for ($poffset = 0; $poffset -lt $pat.length; $poffset++) {
                if ($pat[$poffset] -ne $str[$toffset++]) {
                    return $null
                }
            }

            [tokenizer]::offset += $pat.length

            $token = [token]::new($TokenKindExpected, $pat, $tokenStart, [tokenizer]::CurrentLineNumber,
                [tokenizer]::string, [tokenizer]::FileName)
            return $token
        }

        # Handle the more complex tokens using regular expressions
        $res = $pat.Match([Tokenizer]::string, [tokenizer]::offset)
        if ($res.Success) {
            $len = $res.Length
            $val = $res.Value

            # If this is a keyword or operator ending in a letter that is followed by a letter
            # then this is not a valid keyword/operator token, return null
            if (($TokenKindExpected -eq [TokenKind]::Keyword -or $TokenKindExpected -eq [TokenKind]::Operator) -and
                [char]::IsLetter($val[-1]) -and
                ([tokenizer]::offset + $len -lt [tokenizer]::string.length) -and
                ([char]::IsLetter([tokenizer]::string[[tokenizer]::Offset + $len])))
            {
                return $null
            }

            # Due to token ambiguity, the arrow operator or
            # the type literal prefix '[<' may show up when requesting a binary operator.
            # If so, it's not a valid token in the current context so we just return null.
            elseif ($TokenKindExpected -eq [TokenKind]::Operator -and ($val -eq '->' -or $val -eq '[<' -or $val -eq '::}')) {
                return $null
            }

            [tokenizer]::offset += $len
            return [token]::new($TokenKindExpected, $val, $tokenStart, [tokenizer]::CurrentLineNumber,
                    [tokenizer]::string, [tokenizer]::FileName)
        }
        else {
            return $null
        }
    }

    #
    # Sets the tokenizer offset back to the offset of the ungot token
    #
    static Unget ([token] $token) {
        if ($null -eq $token) {
            errorMessage -dummy "unget: token was null"
        }
        [tokenizer]::offset = $token.index
    }

    #
    # true if at the end of the string being parsed
    #
    static [bool] AtEof() {
        [Tokenizer]::EatWhitespace()
        return [tokenizer]::offset -ge [tokenizer]::string.length
    }

    #
    # Get the character at the current offset in the input stream.
    #
    static [char] CurrentCharacter() {
        [Tokenizer]::EatWhitespace()
        if ([tokenizer]::offset -ge [tokenizer]::string.length) {
            return $null
        }
        return [tokenizer]::string[[tokenizer]::Offset]
    }

    #
    # Get the printable fragment of the input stream around the current offset.
    #
    static [string] CurrentString() {
        if ([tokenizer]::offset -ge [tokenizer]::string.length) {
            return "<EOF>"
        }
        elseif ([tokenizer]::string.length -lt [tokenizer]::offset+21) {
            return [tokenizer]::string.substring([tokenizer]::offset)
        }
        else {
            return -join ([tokenizer]::string[
                [tokenizer]::Offset .. ([tokenizer]::offset+20)])
        }
    }

    static [string] PositionMessage () {
        return [tokenizer]::PositionMessage($null)
    }

    #
    # Compute and return a position message such that the position associated with
    # an error is indicated.
    #
    static [string] PositionMessage ([Token] $token) {
        try {
            $toffset = if ($token) { $token.Index } else { [tokenizer]::offset }
            $src =  if ($token) { $token.Src } else { [tokenizer]::string }
            if ($toffset -ge $src.Length) {
                $toffset = $src.Length-1
            }

            if ($token) {
                $lineNumber = $token.LineNo
            }
            else {
                # compute the line number by walking back through the text
                $back = $toffset
                $lineNumber = 0
                while ($back -gt 0) {
                    if ($src[$back] -eq "`n") {
                        $linenumber++
                    }
                    $back--
                }
            }

            # find the beginning of the line
            $back = $toffset
            while ($back -gt 0) {
                if ($src[$back] -eq "`n") {
                    if ($back -lt $toffset) {
                        $back++
                    }
                    break;
                }
                $back--
            }

            # And the end
            $forward = $toffset
            while ($forward -lt $src.length) {
                if ($src[$forward] -eq "`n") {
                    break
                }
                $forward++
            }

            $beforeString = $src.Substring($back, $toffset - $back)
            $afterString= $src.Substring($toffset, $forward-$toffset)
            # BUGBUGBUG - there should be no exception here
            try {
                if ($token.FileName) {
                    return "$($token.FileName):$($lineNumber+1): $beforeString>>>$afterString"
                }
                else {
                    return "Line $($lineNumber+1): $beforeString>>>$afterString"
                }
            }
            catch {
                return "Line $($lineNumber+1): $beforeString>>>$afterString"
            }
        }
        catch {
            $_ | Format-List -force * | Out-Host
            $_.Exception | Out-Host
            $token | Out-Host
            return "<PositionMessage failed>"
        }
    }
}

###########################################################
#
# Exception class used as the base for all Tiny exceptions
#
class TinyException : Exception {

    # The PowerShell error record associated with this error message.
    [System.Management.Automation.ErrorRecord]
        $ErrorRecord

    # The exception associated with this error. May be null.
    # If $ErrorRecord is non-null, then this should be the same
    # as $ErrorRecord.Exception
    [Exception]
        $Exception

    # The token associated with this error
    [Token]
        $Token

    # The PowerShell script stack
    [string]
        $StackTrace

    TinyException ([string] $message, [Token] $Token) : base($message) {
        $this.Token = $token
        $this.StackTrace = (Get-PSCallStack) -join "`n"
    }

    TinyException ([Exception] $exception, [Token] $Token) : base($exception.message) {
        $this.Token = $token
        $this.Exception = $exception
        $this.StackTrace = (Get-PSCallStack) -join "`n"
    }
    TinyException( [ErrorRecord] $errorRecord, [Token] $token) : base($errorRecord.Exception.Message) {
        $this.Token = $token
        $this.ErrorRecord = $errorRecord
        $this.Exception = $errorRecord.Exception
        $this.StackTrace = $errorRecord.ScriptStackTrace.ToString()
    }

    [string] ToString() {
        $msg = $this.Message
        if ([Tiny]::DetailedErrors) {
            $msg += "`n" + $this.StackTrace
            if ($this.Exception) {
                $msg += "`n----------------------------------------`n" +
                    $this.Exception | Format-List -Force * | Out-String
            }
        }
        return "ERROR: $msg`n$([tokenizer]::PositionMessage($this.token))"
    }

    [string] DetailedError() {
        $old = [tiny]::DetailedErrors
        [tiny]::DetailedErrors = $true
        $msg = ''
        try {
            $msg = $this.ToString()
        }
        finally {
            [Tiny]::DetailedErrors = $old
        }

        return $msg
    }
}

#
# Utility function for creating Tiny errors
#
function ErrorMessage ($msgOrErrorRecord, [Token] $token = $null, [switch] $dummy) {
    if ($dummy -and $null -eq $token) {
        # Generate a dummy token to record the position
        $token = [Token]::new()
    }
    $err = [TinyException]::New($msgOrErrorRecord, $token)
    [scopes]::scopes[-1]['TinyError'] = $err
    $global:TinyError = $err
    throw $err
}

###################################################################################
#
# This exception is thrown when the runtime fails to bind the function parameters
# during a function call. This is typically due to a failed pattern match or
# type constraint mismatch.
#
class BindException : TinyException
{
    BindException ([string] $message, [Token] $Token) : base($message, $token) { }
    BindException ([Exception] $exception, [Token] $Token) : base($exception, $token) { }
    BindException( [ErrorRecord] $errorRecord, [Token] $token) : base($errorRecord, $token) { }
}


###################################################################################
#
# This exception is thrown when the text being parsed is correct but incomplete.
#
class IncompleteParseException : TinyException
{

    IncompleteParseException ([string] $message, [Token] $Token) : base($message, $token) { }
    IncompleteParseException ([Exception] $exception, [Token] $Token) : base($exception, $token) { }
    IncompleteParseException ( [ErrorRecord] $errorRecord, [Token] $token) : base($errorRecord, $token) { }
}

#
# Utility function for throwing incomplete parse exceptions
#
function parseError ($msg, $token = $null) {
    # Generate a dummy token to record the position
    if ($null -eq $token) {
        $token = [Token]::new()
    }

    $err = [IncompleteParseException]::New($msg, $token)
    throw $err
}

###################################################################################
#
# The Tiny expression tree node classes. These classes constitute the executable representation
# of a Tiny script. A script is compiled directly into these Expression classes without building
# an intermediate AST. Evaluation is done by calling the eval() function on the root of the
# expression tree which then recursively evaluates the rest of the tree.
#
#
# The base class for all expressions in Tiny.
#
class Expression
{
    [Token]
        $Token

    Visit($invokable) {
        $invokable.Invoke($this)
    }

    [object] Eval() { throw "Expression: This method must be overridden in the derived class." }
}

###################################################################################
#
# Base class for LValue (assignable) expressions like variables, array elements, etc.
#
class Assignable : Expression
{
    Set( $object ) { throw "Assignable: This method must be overridden in the derived class." }

    
    [object] $InitialValue = [AutomationNull]::Value
}

###################################################################################
#
# The base class for all literals (strings, numbers, etc.)
#
class LiteralBase : Expression { }

###################################################################################
#
# Literal value expression
#
class Literal : LiteralBase
{
    [object]
        $Value

    Literal ([Token] $token, $value) {
        $this.token = $token
        $this.value = $value
    }

    [object] Eval() { return $this.Value }

    [string] ToString() { return "[Literal($($this.Value))]" }
}

###################################################################################
#
# Pattern R-value expression
#
class PatternLiteral : Expression
{
    [object]
        $Value

    PatternLiteral ([Token] $token, $value) {
        $this.token = $token
        $this.value = $value
    }

    [object] Eval() { return $this.Value.Eval() }

    [string] ToString() { return "[PatternLiteral($($this.Value))]" }
}

###################################################################################
#
# Represents a double-quoted string with expandable elements e.g. <pre>"Hi $name, today is ${getdate().DayOfWeek}"</pre>
#
#T ExpandString_Test1 a = 1; "$a" == '1'
#T ExpandString_Test2 a = 10; " $a " == ' 10 '
#T ExpandString_Test3 aaa = 10; bee = 20; "[$aaa,$bee]" == '[10,20]'
#T ExpandString_Test4 aaa = '$tinyhome'; "aaa=$aaa" == 'aaa=$tinyhome' # make sure no recursive expansion
#T ExpandString_Test5 "2+3=${2+3}" == '2+3=5'
#T ExpandString_Test6 "The sum is ${[1..10].sum()}." == 'The sum is 55.'
#BUGBUGBUG - only limited support of lambdas inside ${ ... } (only one level of nested parens.)
#T ExpandString_Test7 "abc ${ [1..10].sum{it} }" == 'abc 55'
# Also see LiteralString_ tests.
class ExpandableString : Expression {
    [string]
        $String

    ExpandableString ([Token] $token, $string) {
        $this.Token = $token
        $this.String = $string
    }

    [object] Eval() {
        return [ExpandableString]::ExpandStr($this.String)
    }

    # BUGBUGBUG - should do both variables and expressions in one pass
    static [string] ExpandStr($str) {

        # process variables first
        $matches = @([Regex]::Matches($str, '\$([a-z_][0-9a-z_]*)|\$\{([^{]*\{[^}]*\}[^}]*|[^}]*)\}', 'CultureInvariant,Ignorecase'))
        [array]::Reverse($matches)
        foreach ($m in $matches) {
            if ($name = $m.Groups[1].Value) {
                $val = [scopes]::GetVariable($name)
                $str = $str.substring(0, $m.Index) + $val +
                   $str.Substring($m.Index + $m.Length)
            }
            else {
            # process expressions next
                $expr = $m.Groups[2].Value
                $str = $str.substring(0, $m.Index) +
                    [tiny]::evalexpr($expr) + 
                    $str.Substring($m.Index + $m.Length)
            }
        }
        return $str
    }
}

###################################################################################
#
# Regex literal expression
#
class RegexLiteral : LiteralBase
{
    [Regex]
        $Value

    [string]
        $VarToSet

    RegexLiteral ([Token] $token, [string] $varToSet, $pattern) {
        if ($pattern -isnot [regex]) {
            $pattern = [regex]::new([string] $pattern, 'CultureInvariant,Ignorecase')
        }
 
        $this.token    = $token
        $this.VarToSet = if ($VarToSet.Length -gt 0) { $varToSet } else { 'matches' }
        $this.value    = $pattern
    }

    [object] Eval() { return $this }

    [bool] Match ([string] $str) {
        $m = $this.Value.Match($str)
        if ($m.Success) {
            [scopes]::scopes[0][$this.VarToSet] = [MatchStatement]::GroupsToHash($m)
            return $true
        }
        else {
            return $false
        }
    }

    [string] ToString() { return [string] $($this.Value) }
}

###################################################################################
#
# Variable or function name expression
#
class Variable : Assignable
{
    # The name of the associated variable.
    [string]
        $Name

    Variable ([Token] $token) {
        $this.token = $token
        $this.name  = $token.value
    }

    # Get's the value of the variable associated with this node
    [object] Eval() {
        try {
            return [scopes]::GetVariable($this.Name)
        }
        catch {
            errormessage $_ $this.token
        }

        # Here to shut up the compiler; will never be run
        return $null
    }

    # Set's the value of the variable associated with this node.
    # The return value is used in pattern matching to decide if the match succeeded
    [bool] Set ($value) {

        [scopes]::SetVariable($this.Name, $value)

        # In the normal case, setting a variable always succeedes
        return $true
    }

    [string] ToString() { return $this.Name }
}

###################################################################################
#
# Implementation of a readonly variable
#
class ReadonlyVariable : Variable {

    ReadonlyVariable ([Token] $token) :base($token) { }

    [bool] Set ($value) {

        # Quietly don't set readonly variables but still return success
        return $true
    }

    [string] ToString() { return "[ReadOnly] $($this.Name)" }
}

###################################################################################
#
# Implementation of a type-constrained variable
#
class TypeConstrainedVariable : Variable {

    TypeConstrainedVariable ([Token] $token, [Type] $typeConstraint) : base($token) {
        $this.TypeConstraint = $typeConstraint
    }

    # Optional type constraint for the variable node
    [type]
        $TypeConstraint

    [bool] Set ($value) {

        # Enforce type constraints which are strict in Tiny. The object being assigned must be exactly that type
        if ($null -ne $this.TypeConstraint -and $value -isnot $this.TypeConstraint) {
#BUGBUGBUG - this would always silently fail when it really only should do so in a pattern...
            return $false
            # throw [BindException]::New("Binding value '$value' to variable '[<$($this.TypeConstraint)>] $($this.name)' failed.", $this.Token)
        }

        [scopes]::SetVariable($this.Name, $value)
        return $true
    }

    [string] ToString() { return "[<$($this.TypeConstraint)>] $($this.Name)" }
}

###################################################################################
#
# Implementation of a pinned variables.  Pinned variables are treated as values
# instead of assignables in lvalue contexts like assignments and pattern matching.
#
class PinnedVariable : Variable {

    PinnedVariable ([Token] $token) :base($token) {
        # Fix up the name for a pinned variable.
        $this.Name = $token.Value.Substring(1);
    }

    [bool] Set ($value) {

        return [scopes]::GetVariable($this.Name) -eq $value
    }

    [string] ToString() { return "^$($this.Name)" }
}

###################################################################################
#
# Represents a named constant like true, false or null. Constants are 'assignable' in
# the context of pattern matching.
#
class Constant : Assignable
{
    [object]
        $Value

    [string]
        $Name

    # If true, then setting the value referenced by this node will quietly fail.
    [bool]
        $SilentSet

    [bool]
        $is_

    Constant([Token] $token, $value) {
        $this.token = $token
        if ($token.Value -eq 'null') {
            $this.SilentSet = $true
        }
        elseif ($this.token.value -eq '_') {
            $this.SilentSet = $true
            $this.is_ = $true
        }
        $this.Value = $value
        $this.Name = $token.Value
    }

    [object] Eval() {
        return $this.Value
    }

    [bool] Set($value) {

        # for pattern matching 'null' matches the null value; '_' matches anything
        if ($this.is_) {
            return $true
        }

        return $this.Value -eq $value
    }

    # Return a useful tostring representation.
    [string] ToString() {
        return "[constant($($this.name) = $($this.value)]"
    }
}

###################################################################################
#
# Base class for the pattern types
#

class PatternBase : Assignable { }

###################################################################################
#
# Implements a list pattern expression e.g. <pre>a::b::c</pre>
#
class Pattern : PatternBase
{
    [Expression[]]
        $elements

    [string] ToString() {
       return '(' + ($this.elements -join ' :: ') + ')'
    }

    Pattern ($token, $elements) {
        $this.Token = $token
        $this.Elements = $elements
    }

    Visit($invokable) {
        foreach ($val in $this.elements()) {
            if ($val -and $val -is [Expression]) {
                $val.Visit($invokable)
            }
        }
        $invokable.Invoke($this)
    }

    # Add an element to the pattern list.
    Add($element) {
        $this.Elements = $this.Elements + $element
    }

    [object] Eval() {
        $result = [TinyList]::New()
#BUGBUGBUG partial implementation
        foreach ($element in $this.Elements) {
            if ($element -is [Constant]) {
                $result.Add($element.Eval())
            }
            elseif ($element -is [Literal]) {
                $result.Add($element.Eval())
            }
            elseif ($element -is [Assignable]) {
                $val = $element.Eval()
                if ($val -is [TinyList] -or $val -is [IENumerable]) {
                    $result.AddRange($val)
                }
                else {
                    $result.Add($val)
                }
            }
            elseif ($element -is [ArrayLiteral]) {
                $result.AddRange($element.Eval())
            }
        }
        return $result
    }

    [object] MatchList([TinyList] $lists, [TinyLambda] $function) {
        $result = [TinyList]::New()
        foreach ($l in $lists.list) {
            if ($l -isnot [TinyList]) {
                errorMessage "MatchList: the argument must be a list of lists" $this.Token
            }
            if ($this.Set($l)) {
                $result.Add($function.Dot(@($l), $l, [AutomationNull]::Value, $null))
            }
        }
        return $result
    }

    [bool] Set ($value) {

        if ($null -eq $value) {
            return $false
        }

        #BUGBUGBUG this is very inefficient
        if (($value -is [IList] -or $value -is [IEnumerable] -or $value -is [Array]) -and $value -isnot [string]) {
            $value = [TinyList]::new($value)
        }

        if ($value -is [TinyList]) {
            if ($this.Elements.Length -eq 2 -and $this.Elements[0] -is [LiteralBase] -and $null -eq $this.Elements[1]) {
                return $value.Equals($this.Elements[0].Eval())
            }

            # if the lists are the same size, just do 1:1 assignment
            elseif ($value.List.Count -eq $this.elements.Length) {
                $max = $this.elements.Length-1
                for ($i=0; $i -lt $max; $i++) {
                    $val = $value.List[$i]
                    if (-not $this.ProcessElement($this.elements[$i], $val)) {
                        return $false
                    }
                    [scopes]::SetVariable('_'+$i, $value.List[$i])
                }

                # handle the final item and make sure it is always a list
                if (-not $this.ProcessElement($this.elements[-1],
                    [TinyList]::New($value.List.GetRange($max, $value.List.Count-$max)))) {
                    return $false
                }
            }
            # If the value list is larger, assign the excess values to the last variable
            elseif ($value.List.Count -gt $this.elements.Length) {
                $max = $this.elements.Length - 1
                for ($i=0; $i -lt $max; $i++) {
                    if (-not $this.ProcessElement($this.elements[$i], $value.List[$i])) {
                        return $false
                    }
                    [scopes]::SetVariable('_'+$i, $value.List[$i])
                }
                # handle the remainder of the list
                if (-not $this.ProcessElement($this.elements[-1],
                    [TinyList]::New($value.List.GetRange($max, $value.List.Count-$max)))) {
                    return $false
                }
            }
            else {
                # If the list of values is smaller, set the remaining variables to null
                $max = $value.List.Count
                for ($i=0; $i -lt $max; $i++) {
                    if (-not $this.ProcessElement($this.elements[$i], $value.List[$i])) {
                        return $false
                    }
                    [scopes]::SetVariable('_'+$i, $value.List[$i])
                }
                # if there's only 1 too many variables set it to [] instead of null
                if ($this.elements.Length - $max -eq 1) {
                    if (-not $this.ProcessElement($this.elements[-1], [TinyList]::new())) {
                        return $false
                    }
                }
                else {
                    for ($i = $max; $i -lt $this.elements.Length; $i++) {
                        if (-not $this.ProcessElement($this.elements[$i], $null)) {
                            return $false
                        }
                    }
                }
            }
        }
        else {
            if ($this.Elements.Length -eq 2 -and $null -eq $this.Elements[1]) {
                return $this.ProcessElement($this.Elements[0], $value)
            }
            else {
                return $false
            }
        }
        return $true
    }

    # Bind values to elements. For non-variable pattern elements,
    # the matched element is stored in a variable corresponding to
    # the pattern element position e.g. the first pattern maps to _0, the
    # second is _1 etc.
    [bool] ProcessElement($element, $value) {
        [bool] $result = $true

        # If the element is a function call, evaluate it and use the result in matching
        if ($element -is [FunctionCall]) {
            $element = $element.Eval()
        }

        # If it's a constant just compare the value
        if ($element -is [Constant]) {
            # Matching on _ always succeeds but null must exactly match null
            $result = $element.Set($value)
        }
        elseif ($element -is [Assignable]) {
            $result = $element.Set($value)
        }
        elseif ($element -is [RegexLiteral]) {
            $result = $element.Match($value)
        }
        elseif ($element -is [Literal]) {
            $lval = $element.Value
            if ($lval -is [TinyLambda]) {
                $result = $lval.Dot(@($value), $value, [AutomationNull]::Value, $null)
            }
            else {
                $result = $lval -eq $value
            }
        }
        elseif ($element -is [ArrayLiteral]) {
#BUGBUG - should validate the array literal pattern at compile time
            if ($value -isnot [TinyList]) {
                $result = $false
            }
            else {
                $lval = $element.Eval()
                if ($lval.Count -eq 1 -and $lval.list[0] -is [Pattern]) {
                   $result = $lval.list[0].Set($value)
                }
                else {
                    $result = $lval -eq $value
                }
            }
        }
        else {
            $result = $element -eq $value
        }
        return $result
    }
}

###################################################################################
#
# Test matching against an object instead of a hashtable
#T PropertyPattern_Test1  getdate() ~ {:: hour minute second ::} then hour != null && minute != null && second != null
# Test binding without explicit names
#T PropertyPattern_Test2 { a:1 b:2 c:3 } ~ {:: a b c ::} then a == 1 && b == 2 && c == 3
# Test for failure when not all of the specified fields are present.
#T PropertyPattern_Test3 { a:1 b:2 } ~ {:: a b c ::} == false
# Test with a string constant
#T PropertyPattern_Test4 { a:10 b:'Hi' c:30 } ~ {:: a b:'Hi'  c ::} then a == 10 && b == 'Hi' && c == 30
# Test with a regex constant
#T PropertyPattern_Test5 { a:10 b:'Hi' c:30 } ~ {:: a b:r/H/  c ::} then a == 10 && b == 'Hi' && c == 30
# Test with an arrayliteral
#T PropertyPattern_Test6 { a:10 b:[1,2,3] c:30 } ~ {:: a b:[1,2,3] c ::} then a == 10 && b == [1,2,3] && c == 30
# Test with a function in the pattern
#T PropertyPattern_Test7 { a:10 b:'Hi' c:30 } ~ {:: a b:regex('H')  c ::} then a == 10 && b == 'Hi' && c == 30
# Test with a type literal - need a semicolo to distinguish the type literal from a cast
#T PropertyPattern_Test8 { a:'eh' b:getdate() c:'sea' } ~ {:: a b:[<DateTime>]; c ::} then a == 'eh' && b is [<DateTime>] && c == 'Sea'
# Test with a lambda in the pattern
#T PropertyPattern_Test9 { a:'eh' b:'bee' c:'sea' } ~ {:: a b:{it == 'bee'} c ::} then a == 'eh' && b == 'bee' && c == 'Sea'
# Test with a pattern which contains a pattern as an element
#T PropertyPattern_Test10 { a:'eh' b:[1,2] c:'sea' } ~ {:: a b:(x::y::_) c ::} then a == 'eh' && b == [1, 2] && x == 1 && y == 2 && c == 'sea'
# PropertyPattern_Test11 ls().map{it ~ {:: name length ::} then length}.sum() > 0
# A Property Pattern literal e.g. <pre>{:: foo:2 bar:x baz: y ::} </pre>
#
# Implements property patterns
#
class PropertyPattern : PatternBase
{
    [OrderedDictionary]
        $elements

    [string] ToString() {
       return '{:: ' + ($this.elements -join ' ' ) + '::}'
    }

    PropertyPattern ([Token] $token, [OrderedDictionary] $elements) {
        $this.Token = $token
        $this.Elements = $elements
    }

    Visit($invokable) {
        foreach ($val in $this.elements()) {
            if ($val -and $val -is [Expression]) {
                $val.Visit($invokable)
            }
        }
        $invokable.Invoke($this)
    }

    [object] Eval() { return $this }

    [bool] Set ($value) {

        if ($null -eq $value) {
            return false
        }

        $isOrderedDictionary = $value -is [OrderedDictionary]
        $isIDictionary = $value -is [IDictionary]
        foreach ($element in $this.elements.GetEnumerator()) {
            if ($isOrderedDictionary) {
                if (-not $value.Contains($element.Key)) {
                    return $false
                }
            }
            elseif ($isIDictionary) {
                if (-not $value.ContainsKey($element.Key)) {
                    return $false
                }
            }

            try {
                $propertyValue = $value.($element.Key)
            }
            catch {
                # The property didn't exist so return false
                return $false
            }

            #BUGBUG - is this necessary
            #if (-not $this.ProcessElement($element, $propertyValue.PSObject.BaseObject)) {
            if (-not $this.ProcessElement($element, $propertyValue)) {
                return $false
            }
        }

        return $true
    }

    # Bind values to elements. For non-variable pattern elements,
    # the matched element is stored in a variable corresponding to
    # the pattern element position e.g. the first patternmaps to _0, the
    # second is _1 etc.
    [bool] ProcessElement($element, $value) {

        [bool] $result = $false
        $valToSet = $null
        $lval = $null
        # If it's a constant, just compare the value
        if ($element.Value -is [Constant]) {
            # Matching on _ or null always succeeds
            # BUGBUGBUG - this isn't right _ always succeeds. null should match the actual value null
            if ($null -eq $element.Value) {
                $result = $true
            }
            else {
                $result = $element.Value -eq $value
            }
            if ($result) {
                $valToSet = $value
            }
        }
        elseif ($element.Value -is [Assignable]) {
            $result = $element.Value.Set($value)
        }
        elseif ($element.Value -is [PatternLiteral]) {
            $result = $element.Value.Value.Set($value)
            if ($result) {
                $valToSet = $value
            }
        }
        elseif ($element.Value -is [RegexLiteral]) {
            $result = $element.Value.Match($value)
            if ($result) {
                $valToSet = $value
            }
        }
        elseif ($element.Value -is [Literal]) {
            $lval = $element.Value.Value
            if ($lval -is [TinyLambda]) {
                $result = $lval.Dot(@($value), $value, [AutomationNull]::Value, $null)
            }
            elseif ($lval -is [type]) {
                $result = $value -is $lval
            }
            else {
                $result = $lval -eq $value
            }

            if ($result) {
                $valToSet = $value
            }
        }
        elseif ($element.Value -is [FunctionCall]) {
            $lval = $element.value.Eval()
            if ($lval -is [Regex]) {
                $result = $lval.Match($value).Success
            }
            else {
                $result = $lval -eq $value
            }
            if ($result) {
                $valToSet = $value
            }
        }
        elseif ($element.Value -is [ArrayLiteral]) {
#BUGBUG - should validate the array literal pattern at compile time
            if ($value -isnot [TinyList]) {
                $result = $false
            }
            else {
                $lval = $element.Value.Eval()
                if ($lval.Count -eq 1 -and $lval.list[0] -is [Pattern]) {
                   $result = $lval.list[0].Set($value)
                }
                else {
                    $result = $lval -eq $value
                }
            }
            if ($result) {
                $valToSet = $value
            }
        }
        elseif ($element.value -is [BinOp] -and $element.value.Op -eq '::') {
            #BUGBUGBUGBUG - this shouldn't be necessary but it is
            $pattern = $element.value.Eval()
            $result = $pattern.Set($value)
            if ($result) {
                $valToSet = $value
            }
        }
        else {
            errorMessage "Pattern elements of type $($element.Value.gettype())) are not supported" $this.Token
        }

        # If so indicated, set a variable with the element name and element.value
        if ($null -ne $valToSet) {
            [scopes]::scopes[0][$element.Key] = $valToSet
        }
        return $result
    }
}

###################################################################################
#
# A static class that defines the methods implementing the
# Tiny operator behaviour (along with some utility methods).
#
class TinyOperators {

    #BUGBUGBUG - the error message that comes back from 2 + 'abc' is
    #BUGBUGBUG incoherent "string not in correct format" - need a better error
    # Implements the + operator.
    static [object] Plus([Expression] $left, [Expression] $right) {
        $lvalue = $left.Eval()
        $rvalue = $right.Eval()
        if ($lvalue -is [IDictionary] -and $rvalue -is [IDictionary]) {
            # Use Tiny semantics to merge 2 dictionaries; not PowerShell's
            foreach ($pair in $rvalue.GetEnumerator()) {
                $key   = $pair.Key
                $value = $pair.Value
                if (-not $lvalue.COntains($key)) {
                    if ($value -is [TinyLambda]) {
                        $value = $value.Clone()
                        $value.TinyThis = $lvalue
                    }
                    $lvalue[$key] = $value
                }
            }
            return $lvalue
        }
        else {
            return ($lvalue + $rvalue)
        }
    }

    #T Operator_Cons1 1 :+ 2 == [1, 2]
    #T Operator_Cons2 1 :+ [2] == [1, 2]
    #T Operator_Cons3 1 :+ [] == [1]
    #T Operator_Cons4 1 :+ [2, 3] == [1, 2, 3]
    #T Operator_Cons5 [] :+ [2, 3] == [[], 2, 3]
    #T Operator_Cons6 [1,2] :+ [3, 4] == [[1, 2], 3, 4]
    #T Operator_Cons7 null :+ [1,2,3] == [1,2,3]
    #T Operator_Cons8 1 :+ null == [1]
    #T Operator_Cons9 (null :+ null)[0] == null
    #T Operator_ConsA r = {a:1 b:2} :+ {c:3}; r[0].a == 1 && r[1].c == 3
    #T Operator_ConsB {a:1 b:2} :+ {c:3} ~ {::a:1 b:2::}::{::c:3::}::_
    # Implements the :+ operator.
    static [object] Cons ([Expression] $left, [Expression] $right) {
        $lval = $left.Eval()
        $rval = $right.Eval()
        if ($null -ne $lval -and $null -ne $rval) {
            if ($rval -is [TinyList]) {
                $rval.List.Insert(0, $lval)
                $rval.Count = $rval.List.Count
                return $rval
            }
            else {
                return [TinyList]::new(@($lval, $rval))
            }
        }
        elseif ($null -eq $rval) {
            if ($lval -is [TinyList]) {
                $rval.Count = $rval.list.count
                return $lval
            }
            else {
                return [TinyList]::new(@($lval))
            }
        }
        else {
            if ($rval -is [TinyList]) {
                return $rval
            }
            else {
                return [TinyList]::new(@($rval))
            }
        }
    }

    # Implements the ++ list append operator. Similar to cons but copies the RHS list.
    #BUGBUGBUG - do we need this operator?
    static [object] ListPrePend([Expression] $left, [Expression] $right) {
        $lval = $left.Eval()
        $rval = $right.Eval()
        if ($null -ne $lval -and $null -ne $rval) {
            if ($rval -is [TinyList]) {
                # copy the rval list
                $rval = [TinyList]::new($rval.List)
                $rval.List.Insert(0, $lval)
                $rval.Count = $rval.List.Count
                return $rval
            }
            else {
                return [TinyList]::new(@($lval, $rval))
            }
        }
        elseif ($null -eq $rval) {
            if ($lval -is [TinyList]) {
                # Copy the list
                return [TinyList]::new($lval.List)
            }
            else {
                return [TinyList]::new(@($lval))
            }
        }
        else {
            if ($rval -is [TinyList]) {
                return [tinylist]::new($rval.List)
            }
            else {
                return [TinyList]::new(@($rval))
            }
        }
    }

    # Implements the - operator.
    static [object] Subtract([Expression] $left, [Expression] $right) {
        $lval = $left.Eval()
        $rval = $right.Eval()
        if ($lval -is [string]) {
            return $lval -replace $rval
        }
        else {
            return $lval - $rval
        }
    }

    # Implements the * operator.
    static [object] Times([Expression] $left, [Expression] $right) {
         return $left.Eval() * $right.Eval()
    }

    # Implements the / operator.
    static [object] Divide($left, $right) {
        $lval = $left.Eval()
        $rval = $right.Eval()
        if ($lval -is [IList]) {
            # divide the list into RHS length chunks
            return [TinyList]::Partition($lval, $rval)
        }
        else {
            return $left.Eval() / $right.Eval()
        }
    }

    # Implements the % operator.
    static [object] Modulus($left, $right) {
        return $left.Eval() % $right.Eval()
    }

    # Implements the ** operator.
    static [object] Exponentiation([Expression] $left, [Expression] $right) {
        return [math]::pow( [double] ($left.Eval()), [double] ($right.Eval()) )
    }

    # Implements the |> operator.
    static [object] PipeOperator([Expression] $left, [Expression] $right) {
        if ($right -is [FunctionCall]) {
            return $right.EvalWithExtraArg($left)
        }
        elseif ($right -is [PropertyExpression]) {
            if ($right.Right -is [MethodInvocation]) {
                return $right.EvalWithExtraArg($left)
            }
            else {
                $invokable = $right.Eval()
                $lvalue = $left.Eval()

                if ($invokable -is [TinyLambda]) {
                    return $invokable.Invoke(@($lvalue), $lvalue, [AutomationNull]::Value, $null)
                }
                elseif ($invokable -is [List[TinyLambda]]) {
                    # BUGBUGBUG - duplicated from class FunctionCall - need to refactor.
                    $lindex = 1
                    foreach ($lambda in $invokable) {
                        $arity = $lambda.Parameters.Length;
                        $lindex++
      
                        # BUGBUGBUG need to handle parameterless functions
                        # Arity must match in function sets
                        if ($null -eq $lambda.Parameters -or $arity -ne 1) {
                            continue
                        }
      
                        $bindSuccess = $true
                        $result = $lambda.Invoke(@($lvalue), $lvalue, [AutomationNull]::Value, $null, [ref] $bindSuccess)
                        if ($bindSuccess -eq $false) {
                            continue;
                        }
                        else {
                            return $result
                        }
                    }
                    #BUGBUGBUG - need to generate error at top level of function set
                    #errorMessage "Function set '$val': no matching function found for arguments: $($arguments -join ', ')"
                    return [AutomationNull]::Value
                }
                else {
                    return $invokable.Invoke($lvalue)
                }
            }
        }
        else {
            :return_from_function do {
                if ($right -is [Constant]) {
                    $invokable = $right.Value
                }
                else {
                    # See if the right expression returned an invokable
                    $invokable = $right.Eval()
                }

                $lvalue = $left.Eval()

                if ($invokable -is [List[TinyLambda]]) {
                    # BUGBUGBUG - duplicated from class FunctionCall - need to refactor.
                    $lindex = 1
                    foreach ($lambda in $invokable) {
                        $arity = $lambda.Parameters.Length;
                        $lindex++
      
                        # BUGBUGBUG need to handle parameterless functions
                        # Arity must match in function sets
                        if ($null -eq $lambda.Parameters -or $lambda.Parameters.Length -ne 1) {
                            continue
                        }
      
                        $bindSuccess = $true
                        $result = $lambda.Invoke(@($lvalue), $lvalue, [AutomationNull]::Value, $null, [ref] $bindSuccess)
                        if ($bindSuccess -eq $false) {
                            continue;
                        }
                        else {
                            return $result
                        }
                    }
                    #BUGBUGBUG - need to generate error at top level of function set
                    #errorMessage "Function set '$val': no matching function found for arguments: $($arguments -join ', ')"
                    return [AutomationNull]::Value
                }
                elseif ($invokable -is [TinyLambda]) {
                    return $invokable.Invoke(@($lvalue), $lvalue, [AutomationNull]::Value, $null)
                }
                elseif ($invokable -is [CommandInfo]) {
                    return & $invokable $lvalue
                }
                else {
                    if ($null -eq $lvalue) {
                        $lvalue = @()
                    }
                    return $invokable.Invoke($lvalue)
                }
            }
            until ($false)

            # We get here from a 'return' statement
            return $script:FunctionReturnValue
#gohere
        }
    }

    # Implements the < operator
    static [bool] LessThan([Expression] $left, [Expression] $right) {
        return $left.Eval() -lt $right.Eval()
    }

    # Implements the <= operator
    static [bool] LessThanOrEqual([Expression] $left, [Expression] $right) {
        return $left.Eval() -le $right.Eval()
    }

    # Implements the > operator
    static [bool] GreaterThan([Expression] $left, [Expression] $right) {
        return $left.Eval() -gt $right.Eval()
    }

    # Implements the >= operator
    static [bool] GreaterThanOrEqual([Expression] $left, [Expression] $right) {
        return $left.Eval() -ge $right.Eval()
    }

    # Implements the == operator
    static [bool] EqualEqual ([Expression] $left, [Expression] $right) {
        $lval = $Left.Eval();
        if ($lval -is [TinyList]) {
            return $lval.Equals($right.Eval())
        }
        else {
            return $lval -eq $right.Eval()
        }
    }

    # Implements the != operator
    static [bool] NotEqual ([Expression] $left, [Expression] $right) {
        $lval = $Left.Eval();
        if ($lval -is [TinyList]) {
            return -not $lval.Equals($right.Eval())
        }
        else {
            return $lval -ne $right.Eval()
        }
    }

    # Pattern matching operator '~'. Works with regexes, patterns and property patterns.
    static [bool] Tilde ([Expression] $left, [Expression] $right) {
        $lhs = $left.Eval()

#BUGBUGBUGBUGBUG - this shouldn't be necessary
        if ($right -is [BinOp] -and $right.Op -eq '::') {
            $pattern = $right.Eval()
            return $pattern.Set($lhs)
        }

        if ($right -is [PatternLiteral]) {
            return $right.Value.Set($lhs)
        }

        if ($right -is [Pattern] -or $right -is [PropertyPattern]) {
            return $right.Set($lhs)
        }
#BUGBUGBUGBUGBUG - this shouldn't be necessary

        $rhs = $right.Eval()
        if ($rhs -is [RegexLiteral]) {
            return $rhs.Match($lhs)
        }

        $result = ([string] $lhs) -match $rhs
        [scopes]::SetVariable('Matches', $matches)
        return $result
    }

    # Implements the !~ operator. True if pattern match fails.
    static [bool] NotTilde ([Expression] $left, [Expression] $right) {
        return -not [TinyOperators]::Tilde($left, $right)
    }

    # Regex split operator /~
    static [object] SlashTilde ([Expression] $left, [Expression] $right) {
        $lval = $left.Eval()
        if ($lval -is [TinyList]) {
            $lval = $lval.list
        }
        $rval = $right.Eval()
        if ($rval -is [RegexLiteral]) {
            $rval = $rval.Value
        }
        if ($rval -isnot [string] -and $rval -isnot [regex]) {
            errorMessage "The right hand side of the '/~' operator must be a string or regex."
        }
        return [TinyList]::New($lval -split $rval)
    }

    # Regex replace operator -~
    static [object] MinusTilde ([Expression] $left, [Expression] $right) {
        $lval = $left.Eval()
        $rval = $right.Eval()
        #BUGBUG - not sure if I want this to work on arrays
        if ($lval -is [TinyList]) {
            $lval = $lval.list
        }

        if ($rval -is [RegexLiteral]) {
            $rval = $rval.Value
        }
        # for -~ [ 'matchPath', 'replacePart']
        elseif ($rval -is [TinyList]) {
            $rval = $rval.list
            if ($rval[0] -is [RegexLiteral]) {
                $rval[0] = $rval[0].Value
            }
        }
        return ($lval -replace $rval)
    }

    # Operator '*~' that matches all patterns, regex, list or property patterns.
    static [object] StarTilde ([Expression] $left, [Expression] $right) {
        $lval = $left.Eval()
        $rval = $right.Eval()

        $result = $null
        if ($rval -is [Pattern] -or $rval -is [PropertyPattern])
        {
            if ($lval -is [TinyList]) {
                $lval = $lval.List
            }
            $result = [TinyList]::New($lval.where{ $rval.Set($_) })
        }
        elseif ($rval -is [RegexLiteral]) {
            #BUGBUGBUG - what should matchall do with the variable component of the regex literal??
            $result = [TinyList]::New(@($rval.Value.Matches([string] $lval)))
        }
        else {
            #BUGBUGBUG - what should this do with the variable component of the regex literal??
            $result = [TinyList]::new(@([regex]::Matches([string] $lval, [string] $rval,
                "IgnoreCase,CultureInvariant")))
        }
        return $result
    }

    # Implements the 'is' operator.
    static [object] Is ([Expression] $left, [Expression] $right) {
        $lval = $left.eval()
        $rval = $right.eval()
        return $lval -is $rval
    }

    # Implements the 'IsNot' operator.
    static [object] IsNot ([Expression] $left, [Expression] $right) {
        $lval = $left.eval()
        $rval = $right.eval()
        return $lval -isnot $rval
    }

    # A routine that implements the core logic for the contains operators.
    # It is also used as the implementation of the ':>' operator.
    static [bool] ContainsCoreImpl ([Expression] $setExpr, [Expression] $valueExpr) {
        $set = $setExpr.Eval()
        $value = $valueExpr.Eval()
        if ($set -is [IDictionary]) {
            return $set.get_Keys() -contains $value
        }
        elseif ($set -is [TinyList]) {
            return $set.list -contains $value
        }
        else {
            return $set -contains $value
        }
    }

    # Implements the !:> operator
    static [bool] NotContainsImpl ([Expression] $set, [Expression] $value) {
        return -not [TinyOperators]::ContainsCoreImpl($set, $value)
    }

    # Implements the <: operator
    static [bool] InImpl ([Expression] $value, [Expression] $set) {
        return [TinyOperators]::ContainsCoreImpl($set, $value)
    }

    # Implements the !<: operator
    static [bool] NotInImpl ([Expression] $value, [Expression] $set) {
        return -not [TinyOperators]::ContainsCoreImpl($set, $value)
    }

    # List Pattern construction operator '::'
    static [Pattern] PatternOperator([Expression] $left, [Expression] $right) {
        #BUGBUG - review this code for error handling
        if ($left -is [BinOp]) {
            $left = $left.Eval()
        }

        if ($right -is [BinOp]) {
            $right = $right.Eval()
        }

        if ($left -is [Pattern]) {
            $left.Add($right)
            return $left
        }

        return [pattern]::new($null, @($left, $right))
    }

    # Implements the += assignment operator
    static [object] PlusEquals([Expression] $left, [Expression] $right) {
        if ($left -is [Constant] -and -not $left.SilentSet) {
            errorMessage "cannot change the value of constant '$($left.token.value)'"
        }

        $cv = $left.Eval()
        $rvalue = $right.Eval()
        if ($null -eq $rvalue) {
            # Adding null is a no-op so just return the current value
            $val = $cv
        }
        elseif ($cv -is [IDictionary] -and $rvalue -is [IDictionary]) {
            foreach ($pair in $rvalue.GetEnumerator()) {
                $key   = $pair.Key
                $value = $pair.Value
                if (-not $cv.Contains($key)) {
                    # Clone lambdas so the this pointer points to the new hashtable
                    if ($value -is [TinyLambda]) {
                        $value = $value.Clone()
                        $value.TinyThis = $cv
                    }
                    $cv[$key] = $value
                }
            }
            $val = $cv
        }
        elseif ($cv -is [TinyList]) {
            $cv.AddRange($rvalue)
            $val = $cv
        }
        else {
            $val = $left.Set(($val = $cv + $rvalue))
        }
        return $val
    }

    # Implements the *= assignment operator
    static [object] StarEquals([Expression] $left, [Expression] $right) {
        if ($left -is [Constant] -and -not $left.SilentSet) {
            errorMessage "cannot change the value of constant '$($left.token.value)'"
        }

        $cv = $left.Eval()
        $val = $right.Eval()
        if ($cv -is [TinyList]) {
            # replicate the list RHS times
            $val = [int] $val
            if ($val -gt 1) {
                $cv.addrange($cv.list * ($val-1))
                $val = $cv
            }
        }
        else {
            [void] $left.Set(($val = $cv * $val))
        }
        return $val
    }

    # Implements the -= assignment operator
    static [object] MinusEquals([Expression] $left, [Expression] $right) {
        if ($left -is [Constant] -and -not $left.SilentSet) {
            errorMessage "cannot change the value of constant '$($left.token.value)'"
        }

        $cv = $left.Eval()
        $val = $right.Eval()
        if ($cv -is [TinyList]) {
            foreach ($v in $val) {
                [void] $cv.list.Remove($v)
            }
            $val = $cv
        }
        else {
            [void] $left.Set(($val = $cv - $val))
        }
        return $val
    }

    # Implements the /= operator.
    static [object] SlashEquals([Expression] $left, [Expression] $right) {

        if ($left -is [Constant] -and -not $left.SilentSet) {
            errorMessage "cannot change the value of constant '$($left.token.value)'"
        }

        $cv = $left.Eval()
        $val = $right.Eval()
        if ($cv -is [TinyList]) {
            # divide the list into RHS length chunks
            $partitioned = [TinyList]::Partition($cv, $val)
            $cv.list.Clear()
            $cv.list.AddRange($partitioned.list)
            $val = $cv
            # update the count property on the TinyList
            $val.Count = $val.list.count
        }
        else {
            $left.Set(($val = $cv / $val))
        }
        return $val
    }

    # Implements the basic '=' assignment operator.
    static [object] Equals([Expression] $left, [Expression] $right) {

        if ($left -is [Constant] -and -not $left.SilentSet) {
            errorMessage "cannot change the value of constant '$($left.token.value)'"
        }

        $val = $right.Eval()

        # Check for pinned or type-constrained variables first as
        # they are subclasses of variable.
        if ($left -is [PinnedVariable] -or $left -is [TypeConstrainedVariable]) {
            $val = $left.Set($val)
        }
        elseif ($left -is [Variable]) {
            $left.Set($val)
        }
        elseif ($left -is [PatternBase]) {
            $val = $left.Set($val)
        }
        else {
            # just set properties, array elements, etc.
            $left.Set($val)
        }
        return $val
    }

    # Implements the 'as' operator
    static [object] AsImpl([Expression] $left, [Expression] $right) {
        $lval = $left.eval()
        $rval = $right.eval()
        return $lval -as $rval
    }

    # Implements the 'then' operator
    static [object] ThenImpl([Expression] $left, [Expression] $right) {
        if ($left.eval()) {
            $result = $right.eval()
            if ($result -is [TinyLambda]) {
                return $result.body.eval() #BUGBUGBUGBUG
                ##return $result.Dot(@($lval), [AutomationNull]::Value, [AutomationNull]::Value, $null)
            }
            else {
                return $result
            }
        }
        return $null
    }

    # Implements the 'do' statement-sequencing operator
    static [object] DoImpl([Expression] $left, [Expression] $right) {
        if ($left -is [Literal] -and $left.Value -is [TinyLambda]) {
            $null = $left.Value.Dot()
        }
        else {
            $null = $left.eval()
        }
        if ($right -is [Literal] -and $right.Value -is [TinyLambda]) {
            $result = $right.Value.Dot()
        }
        else {
            $result = $right.eval()
        }
        return $result
    }

    # Implements the range operator.
    static [TinyList] Range([Expression] $left, [Expression] $right) {
        if ($left -is [BinOp] -and $left.op -eq '..') {
            $lval = $left.left.eval()
            $rval = $left.right.eval()
            $step = $right.eval()
        }
        else {
            $lval = $left.eval()
            $rval = $right.Eval()
            $step = 1
        }
        return [TinyOperators]::RangeImpl($lval, $rval, $step)
    }

    static [TinyList] RangeImpl($lower, $upper) {
        return RangeImpl($lower, $upper, 1)
    }

    # Core implementation of the range operator.
    static [TinyList] RangeImpl($lower, $upper, $step) {
        $wasChar = $false
        if ($lower -is [string]) {
            $wasChar = $true
            if ($lower.length -ne 1) {
                errorMessage 'Invalid string argument for lower bound of the range operation. Should be a single character like "a"'
            }
            $lower = [int][char] $lower
        }

        if ($upper -is [string]) {
            $wasChar = $true
            if ($upper.length -ne 1) {
                errorMessage 'Invalid string argument for upper bound of the range operation. Should be a single character like "a"'
            }
            $upper = [int][char] $upper
        }

        if ($lower -eq $upper) {
            return [TinyList]::new($lower)
        }

        $result = [TinyList]::New()
        if ($lower -lt $upper) {
            while ($lower -le $upper) {
                $result.Add($lower)
                $lower += $step
            }
        }
        else {
            while ($upper -le $lower) {
                $result.Add($lower)
                $lower -= $step
            }
        }

        if ($wasChar) { 
            $strlist = foreach ($item in $result.list) { [string][char] $item }
            $result = [TinyList]::new($strlist)
        }

        return $result
    }

    # Implementation of the '??' operator
    static [object] QuestionQuestion([Expression] $left, [Expression] $right) {
        $lvalue = $left.eval()
        if ($null -ne $lvalue) {
            return $lvalue
        }

        return $right.eval()
    }

    # Implementation of the '&&' operator
    static [bool] AndAnd([Expression] $left, [Expression] $right) {
        if (-not [Tiny]::AsBool($left.Eval())) {
            return $false
        }

        return [Tiny]::AsBool($right.Eval())
    }

    # Implementation of the '||' operator.
    static [bool] OrOr([Expression] $left, [Expression] $right) {
        if ([Tiny]::AsBool($left.Eval())) {
            return $true
        }

        return [Tiny]::AsBool($right.Eval())
    
    }

    # Implementation of the '?=' operator.
    static [object] QuestionEquals([Expression] $left, [Expression] $right) {
        $val = $cv = try { $left.Eval() } catch { $null }
        if ($null -eq $cv) {
            $val = $right.Eval()
            $left.Set($val)
        }
        return $val
    }
}

##############################################################################################
#
# The following table defines the precedence and behavior for all of the operators in the
# Tiny language. It is available in the Tiny environment in the 'OperatorTable' variable.
# At compile time, the comiler creates a BinOp node with the Action field populated from this table.
# The value of the Action property must be a PSMethod info, either from a class method
# implementing the behaviour or the 'InvokeReturnAsIs' method info from a scriptblock.
# All of the used entries are class method infos both for performance and for reliability
# of the implementing code. There are a number of entries where the action is unused.
# The behaviour for these operators is defined through classes and the actions are
# scriptblock placeholders that throw an error message if they are, for some reason, called.
#

class OperatorTableEntry {
    [int] $Prec
    [PSMethod] $Action
}

$script:OperatorTable = @{
    #T Operator_LessThan1 5 < 10
    #T Operator_LessThan2 (5 < 2) == false
    #T Operator_LessThan3 (2 < 2) == false
    #T Operator_LessThan4 ('abc' < 'abcd')
    #T Operator_LessThan5 ('abcd' < 'abc') == false
    #O < See if LHS is less than the RHS e.g. <pre>5 &lt; 10</pre><pre>"abc" &lt; "abcd"</pre><pre>[1,2,3] &lt; [4,5,6]</pre>
    '<'  = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::LessThan
    }

    #T Operator_GreaterThan1 10 > 5
    #T Operator_GreaterThan2 (2 > 10) == false
    #T Operator_GreaterThan3 (2 > 2) == false
    #T Operator_GreaterThan4 'abcd' > 'abc'
    #T Operator_GreaterThan5 ('abcd' > 'abcde') == false
    #BUGBUG  Operator_GreaterThan6 [1, 2, 3] > [1, 2] - arrays are not IComparable
    #T Operator_GreaterThan7 10 > "5"  # numeric compare
    #T Operator_GreaterThan8 ('10' > 9) == false # lexicographical compare
    #O > See if LHS is greater than the RHS; works with any comparable object; lists are compared by value not reference
    '>'  = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::GreaterThan
    }

    #T Operator_LessThanOrEqual1 5 <= 10
    #T Operator_LessThanOrEqual2 (5 <= 2) == false
    #T Operator_LessThanOrEqual3 (2 <= 2)
    #T Operator_LessThanOrEqual4 ('ab' <= 'abc')
    #T Operator_LessThanOrEqual5 ('abc' <= 'abc')
    #T Operator_LessThanOrEqual6 ('abcd' <= 'abc') == false
    #O <= See if LHS is less than or equals to the RHS; PowerShell coersion rules are used
    '<=' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::LessThanOrEqual
    }

    #T Operator_GreaterThanOrEqual1 10 >= 5
    #T Operator_GreaterThanOrEqual2 (2 >= 10) == false
    #T Operator_GreaterThanOrEqual3 (2 >= 2)
    #T Operator_GreaterThanOrEqual4 ('abc' >= 'ab')
    #T Operator_GreaterThanOrEqual5 ('abc' >= 'abc')
    #T Operator_GreaterThanOrEqual6 ('ab' >= 'abc') == false
    #O >= see if LHS is greater than or equal to the RHS
    '>=' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::GreaterThanOrEqual
    }

    #T Operator_EqualsNumber1  2 == 2
    #T Operator_EqualsNumber2  (2 == 3) == false
    #T Operator_EqualsNumber3  2 == 2.0  # numeric types unify
    #T Operator_EqualsNumber4  2 == "2"  # as do numeric strings (PowerShell semantics)
    #T Operator_EqualsNumber5  (2 == 2.12) == false  # precision is preserved
    #T Opetator_EqualsNumber6  (2 == 'abc') == false  # no error but comparison fails
    #T Operator_EqualsString1  "abc" == "abc"
    #T Operator_EqualsString2  "ABC" == "abc" # case insensitive comparison
    #T Operator_EqualsArray1   [1,2,3] == [1,2,3]
    #T Operator_EqualsArray2   [1,'hi',3] == [1,'HI',3] # polymorphic comparison; case insensitive string comparison
    #T Operator_EqualsArray3   ['1','hi',3, 4] == [1,'HI','3', 4] # polymorphic comparison; case insensitive string comparison
    #O == See if the two sides are equal; lists are compared by value e.g. <pre>2 == 2</pre><pre>'abc' == 'abc'</pre><pre>[1, 2, 3] == [1, 2, 3]</pre>
    '==' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::EqualEqual
    }

    #T Operator_NotEqualsNumber  2 != 3
    #T Operator_NotEqualsString  "abc" != "def"
    #T Operator_NotEqualsArray   [1,2,3] != [1,2,3,4]
    #O != See if the two sides are not equal e.g. <pre>[1, 2, 3] != [4, 5, 6]</pre>
    '!=' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::NotEqual
    }

    #T Operator_RegexMatchOperator1 'abc' ~ r/abc/
    #T Operator_RegexMatchOperator2 'xyz' ~ r/abc/ == false
    #T Operator_RegexMatchOperator3 'ABC' ~ r/abc/ == true
    #T Operator_RegexMatchOperator4 'ABC' ~ r/abc/c == false
    #T Operator_MatchOperatorWithPattern1 [1,2,3] ~ a::2::{it == 3}::_ then a == 1
    #T Operator_MatchOperatorWithPinnedVar1 a = 'abc'; ['abc'] ~ ^a::_
    #T Operator_MatchOperatorWithPinnedVar2 a = 'abc'; ['abc', 'abc'] ~ ^a::^a::_
    #T Operator_MatchOperatorWithPinnedVar3 a = 'abc'; ['abc', 'def'] ~ ^a::a::_ then (a == 'def')
    #T Operator_MatchOperatorWithPinnedVar4 [1,2,1] ~ a::2::^a::_ # Rebind in pattern
    #T Operator_MatchOperatorWithPinnedVar5 n = 13; ^n = 13 then true
    #T Operator_MatchOperatorWithPinnedVar6 n = 13; if (^n = 31) { false } else { true } # pinned var assignment should fail
    #T Operator_MatchWithRegexPattern1   [1, 'abc', 2] ~ 1::r/^[abc]/::2::_ == true
    #T Operator_MatchWithRegexPattern2   [1, 'dabc', 2] ~ 1::r/^[abc]/::2::_ == false
    #T Operator_MatchWithRegexPattern3   [1, 'abc', 27] ~ 1::r/^[abc]/::x::_ && x == 27
    #O ~ Regular expression or pattern match operator; returns true if the pattern matches; for regex matches, the 'matches' variable is set; does not iterate over lists <pre>'abc' ~ '[ac]'</pre> <pre>[1,2,3,4] ~ (1::a::{it > 2}::b)</pre> returns true and sets 'a' to 1 and 'b' to 4
    '~'  = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::Tilde
    }

    #T Operator_NotMatch1 'xyz' !~ r/abc/ == true
    #T Operator_NotMatch2 'abc' !~ r/abc/ == false
    #BUGBUGBUG - add tests for not matching list and property patterns
    #O !~ The regex notmatch operator
    '!~' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::NotTilde
    }

    #T Operator_Split1 'a b c' /~ ' ' == ['a', 'b', 'c']
    #T Operator_Split2 'a b c' /~ r/\s+/  == ['a', 'b', 'c']
    #O /~ The regex split operator e.g. <pre>"a b c" /~ ' '</pre> results in <pre>['a', 'b', 'c']</pre>
    '/~' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::SlashTilde
    }

    #T Operator_Replace1 'abcabc' -~ r/a/ == 'bcbc'
    #T Operator_Replace2 'abcabc' -~ [r/a/, '_'] == '_bc_bc'
    #o -~ The regex replace operator, sets the Matches variable e.g. <pre>"abcd" -~ '[bc]'</pre> results in <pre>'ad'</pre>
    '-~' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::MinusTilde
    }

    #T Operator_Matches1     ('aaa' *~ r/a/).count == 3
    #O *~ The regex 'all matches' operator. This operator returns MatchInfo objects for all matches in a string <pre>'abc' *~ '[abc]+'</pre>
    '*~' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::StarTilde
    }

    #T Operator_Contains         [1,2,3,4,5] :> 3
    #T Operator_Contains2        ( [1,2,3,4,5] :> 10 ) == false
    #T Operator_ContainsDict     {foo:1 bar: 2} :> 'foo'
    #T Operator_ContainsDict2    ({foo:1 bar: 2} :> 'baz') == false
    #O :> The contains operator (the list to check for containment is on LHS); if the LHS is a dictionary, the keys are checked  e.g. <pre> [1,2,3,4,5] :> 3</pre>
    ':>' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::ContainsCoreImpl
    }

    #T Operator_In       5 <: [1 .. 10]
    #T Operator_In2      (20 <: [1 .. 10]) == false
    #T Operator_InDict   "foo" <: {foo:1 bar:2}
    #T Operator_InDict2  ("baz" <: {foo:1 bar:2}) == false
    #O <: The in operator (the list to check is on RHS) e.g. <pre>5 &lt;: [1 .. 10]</pre>
    '<:' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::InImpl
    }

    #T Operator_NotContains [1,2,3,4,5] !:> 10
    #O !:>  The not contains operator e.g. <pre>[1,2,3,4,5] !:&gt; 10</pre>
    '!:>' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOPerators]::NotContainsImpl
    }

    #T Operator_NotIn 20 !<: [1 .. 10]
    #O !<: The not in operator e.g. <pre> 10 !&lt;: [1, 2 .. 5, 6]</pre>
    '!<:' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::NotInImpl
    }

    #T Operator_and1         (true && true) == true
    #T Operator_and2         (true && false) == false
    #T Operator_and3         (false && true) == false
    #T Operator_and4         (false && false) == false
    #T Operator_and5         (1 && 1) == true
    #T Operator_and6         (1 && 0) == false
    #T Operator_and7         (0 && 1) == false
    #T Operator_and8         (0 && 0) == false
    #T Operator_and9         ([1] && [1]) == true
    #T Operator_and10        ([1] && []) == false
    #T Operator_and11        ([] && [1]) == false
    #T Operator_and12        ([] && []) == false
    #T Operator_AndShortCircuit (x = 4) && (x = 6); x == 6
    #T Operator_AndShortCircuit2 x = 0; false && (x = 15); x == 0
    #O && Logical and; PowerShell truthy conversions are used; evaluation is short-circuited  e.g. <pre>function(1) && function(2)</pre>
    '&&' = [OperatorTableEntry] @{
        prec = 2;
        action = [TinyOperators]::AndAnd
    }

    #T Operator_or1          (true || true) == true
    #T Operator_or2          (true || false) == true
    #T Operator_or3          (false || true) == true
    #T Operator_or4          (false || false) == false
    #T Operator_or5          (1 || 1) == true
    #T Operator_or6          (1 || 0) == true
    #T Operator_or7          (0 || 1) == true
    #T Operator_or8          (0 || 0) == false
    #T Operator_or9          ([1] || [1,2]) == true
    #T Operator_or10         ([1,2] || []) == true
    #T Operator_or11         ([] || [0]) == true
    #T Operator_or12         ([] || []) == false
    #T Operator_or13         ('abc' ||'abc') == true
    #T Operator_or14         ("abcd" || "") == true
    #T Operator_or15         ('' || "abcd") == true
    #T Operator_or16         ('' || 0) == false
    #T Operator_orShortCircuit x = 4 ; true || (x = 6); x == 4
    #T Operator_orShortCircuit2 x = null; 0 || (x = 15); x == 15
    #O || Logical or; PowerShell truthy conversions are used e.g. <pre>function(1) || function(2)</pre>
    '||' = [OperatorTableEntry] @{
        prec = 2;
        action = [TinyOperators]::OrOr
    }

    #T Operator_Is1  'abc' is [<string>]
    #T Operator_Is2  ('abc' is [<int>] ) == false
    #T Operator_is3  {} is [<IDictionary>]
    #T Operator_Is4  123i is [<BigInt>]
    #O is The type comparison operator; checks to see if the LHS is of the type on the RHS e.g. <pre>3.14 is [<double>]</pre>
    'is' = [OperatorTableEntry] @{
        prec = 3;
        action = [TinyOperators]::Is
    }

    #T Operator_IsNot1 123 isnot [<string>]
    #T Operator_IsNot2 t = [<int>]; "hi" isnot t
    #O isnot The not type comparison operator; checks to see if the LHS is not of the type on the RHS e.g. <pre>3.14 isnot [<string>]</pre>
    'isnot' = [OperatorTableEntry] @{
        prec = 3;
        action = [TinyOperators]::IsNot
    }

    #T Operator_As1   ("123" as [<int>]) == 123
    #T Operator_As2   (123 as [<string>]).GetType() == [<string>]
    #T OPerator_As3   (123.123 as [<int>]) == 123
    #T Operator_As4   ('abc' as [<int>]) == null
    #T Operator_As5   ('abc' as [<bool>]) == true
    #T Operator_As6   ('' as [<bool>]) == false
    #T Operator_As7   (1 as [<bool>]) == true
    #T Operator_As8   (0 as [<bool>]) == false
    #O as The type conversion operator; converts the LHS to an object of type specified by the RHS e.g. <pre>"123" as [<int>]</pre>
    'as' = [OperatorTableEntry] @{
        prec = 3
        action = [TinyOperators]::AsImpl
    }

    #T Operator_Then  (true then 2) == 2
    #T Operator_Then2 (false then 2) == null
    #O then The conditional evaluation operator - if the LHS evaluates to true, the RHS is evaluated and that value is returned; if not then null is returned. If the RHS is a lambda, it is evaluated with the LHS value available in ''It''  e.g. <pre>"N is 1234" ~ '([0-9]+)' then matches[1]</pre> returns 1234.And <pre>6/2 then {"It is " + it}</pre> returns "It is 3".
    'then' = [OperatorTableEntry] @{
        prec = 2
        action = [TinyOperators]::ThenImpl
    }

    #T Operator_Do1 x = 0; true do (x = 3.14); x == 3.14
    #T Operator_Do2 x = 0; false do (x = 3.14); x == 3.14
    #T Operator_Do3 true do true == true
    #T Operator_Do4 false do true == true
    #T Operator_Do5 x = 0; y = 0; {x = 'abc'} do {y = 'def'}; (x == 'abc') && (y == 'def')
    #T Operator_Do6 x = 0; y = 0; {x = 'abc'} do (y = 'def'); (x == 'abc') && (y == 'def')
    #T Operator_Do7 x = 0; y = 0; (x = 'abc') do {y = 'def'}; (x == 'abc') && (y == 'def')
    #T Operator_Do8 1 do 2 do 3 == 3
    #O do The expression sequencing operator. This operaotr evaluates the LHS, discards the result, then returns the result of evaluating the RHS. e.g. <pre>1 do 2 do 3 == 3</pre>
    'do' = [OperatorTableEntry] @{
        prec = 2
        action = [TinyOperators]::DoImpl
    }

    #T Operator_NullCoalesce  null ?? 10 == 10
    #T Operator_NullCoalesce2  20 ?? 10 == 20
    #O ?? The null coalescing operator; if the result of evaluating LHS is not null then it is returned otherwise the result of evaluating the RHS is returned e.g. <pre>var = null ?? 'it was null'</pre> set 'var' to 'it was null'
    '??' = [OperatorTableEntry] @{
        prec = 4;
        action = [TinyOperators]::QuestionQuestion
    }

    #T Operator_RangeOperator1        1..5 == [1,2,3,4,5]
    #T Operator_RangeOperator2        5..1 == [5, 4, 3, 2, 1]
    #T Operator_RangeOperator3        -3 .. 3  == [-3, -2, -1, 0, 1, 2, 3]
    #T Operator_RangeStep            0..6..2 == [0,2,4,6]
    #T Operator_RangeOpInArray       [1..5] == [1,2,3,4,5]
    #T Operator_RangeOpInArray1      [0, 1..5, 6] == [0,1,2,3,4,5,6]
    #T Operator_RangeStepInArray     [10, 11, 0..6..2, 12, 13] == [10, 11, 0, 2, 4, 6, 12, 13]
    #O .. range operator (lower .. upper [.. step]); usually used in an array literal e.g. <pre>[1 .. 10]</pre> <pre>[0 .. 10 .. 2]</pre>
    '..' = [OperatorTableEntry] @{
        prec = 5
        action = [TinyOperators]::Range
    }

    #T Operator_AddNumbers           2 +3 == 5
    #T Operator_AddStrings           "ab" + "cd" == "abcd"
    #T Operator_AddStringAndNumber   'abc' + 5 == 'abc5'
    #T Operator_AddNumberAndString   5 + '5' == 10
    #T Operator_AddArrays            [1,2,3] + [4,5,6] == [1,2,3,4,5,6]
    #T Operator_AddArrayAndScalar    [1,2,3] + 4 == [1,2,3,4]
    #O + The addition operator; PowerShell semantics; strings and arrays are concatenated e.g. <pre>"a string " + 1234</pre>
    '+' = [OperatorTableEntry] @{
        prec = 6;
        action = [TinyOperators]::Plus
    }

    #T Operator_Cons10 1 :+ 2 == [1, 2]
    #T Operator_Cons11 1 :+ [2, 3] == [1, 2, 3]
    #T Operator_Cons12 [1] :+ [2, 3] == [[1], 2, 3]
    #T Operator_Cons13 [1, 2] :+ [2, 3] == [[1, 2], 2, 3]
    # Make sure :+ updates the count on a tiny list
    #T Operator_Cons14 (1 :+ [2,3]).count == 3
    #O :+ The 'cons' operator, combines the left and right arguments into a list e.g. <pre>1 :+ 2 == [1, 2]</pre> <pre> 1 :+ [2, 3] == [1, 2, 3]</pre> <pre>[1] :+ [2, 3] == [[1], 2, 3]</pre>
    ':+' = [OperatorTableEntry] @{
        prec = 12
        action = [TinyOperators]::Cons
    }

    #BUGBUGBUG - not sure if we need this or not.
    # The non-mutating cons operator
    '++' = [OperatorTableEntry] @{
        prec = 6
        Action = [TinyOperators]::ListPrePend
    }

    #T Operator_SubtractionWithNumbers 7 - 4 == 3
    #T Operator_SubtractionWithArrays  [1,2,3,1,2,3] - 3 == [1,2,1,2]
    #T Operator_SubtractionWithStrings 'abcabc' - 'bc' == 'aa'
    #O - The subtraction operator; PowerShell semantics plus it works on arrays to remove elements and strings to remove patterns e.g. <pre>10 - 5</pre> <pre>[1,2,3,4] - [2,4]</pre> returns [1,3]
    '-' = [OperatorTableEntry] @{
        prec = 6;
        action = [TinyOperators]::Subtract
    }

    #T Operator_MultiplyNumbers 3 * 5 == 15
    #T Operator_MultiplyStrings 'a' * 3 == 'aaa'
    #T Operator_MultiplyArray   [1,2,3] * 2 == [1,2,3,1,2,3]
    #O * The multiplication operator; PowerShell semantics e.g. <pre>10*3</pre> <pre>"*" * 10</pre>results in a string of 10 '*' <pre>[1,2,3] * 5</pre> returns a new list that repeats the original list 5 times
    '*' = [OperatorTableEntry] @{
        prec = 8;
        action = [TinyOperators]::Times
    }

    #T Operator_DivisionOnNumbers  10 / 2 == 5
    #T Operator_DivisionOnArrays   [1 .. 9] / 3 == [[1,2,3], [4,5,6], [7,8,9]]
    #O / The division operator (works on arrays - it partitions them) e.g. <pre> 10 / 2 </pre> <pre>[1 .. 10] / 3</pre> results in [[1,2,3], [4,5,6], [7,8,9], 10]
    '/' = [OperatorTableEntry] @{
        prec = 8;
        action = [TinyOperators]::Divide
    }

    #T Operator_ModulusOperator 10 % 3 == 1
    #O % The modulus operator
    '%' = [OperatorTableEntry] @{
        prec = 8;
        action = [TinyOperators]::Modulus
    }

    #T Operator_Exponentiation1 10 ** 2 == 100
    #O ** exponentiation operator e.g. <pre>2 ** 10</pre>
    '**' = [OperatorTableEntry] @{
        prec = 10;
        action = [TinyOperators]::Exponentiation
    }

    #T Operator_Format1 '{0}' -f 3.14 == "3.14"
    #T Operator_Format2 '{0} {1}' -f [3.14, 'abc'] == "3.14 abc"
    #T Operator_Format3 '{0}-{1}-{2}' -f [3.14, 'abc', 2+2] == "3.14-abc-4"
    #T Operator_Format4 'str: ' + '{0}-{1}-{2}' -f [3.14, 'abc', 2+2] == "str: 3.14-abc-4"
    #O -f Format operator like PowerShell's e.g.<pre> "first {0} second {0}" -f [1, 2]</pre>
    '-f' = [OperatorTableEntry] @{
        prec = 10
        action = {
            param ([Expression] $left, [Expression] $right)
            $fmtString = $left.Eval()
            $argsArray = $right.Eval()
            if ($argsArray -is [TinyList]) {
                $argsArray = $argsArray.List
            }
            return ($fmtString -f @($argsArray))
        }.InvokeReturnAsIs
    }

    #T Operator_PipeOperator1 import 'math'; 9 |> math.sqrt == math.sqrt(9)
    #T Operator_PipeOperator2 [1, 2, 3] |> map { it * 3 } == [3, 6, 9]
    #T Operator_PipeOperator3 [1, 2, 3] |> map { n -> n * 3 } == [3, 6, 9]
    #T Operator_PipeOperator4 fn MyFunc l f -> l.map{ it * f }; [1,2,3] |> myfunc(2) == [2, 4, 6]
    #T Operator_PipeOperator5 [1 .. 10] |> reduce{ x, y -> x + y } == 55
    #T Operator_PipeOperator6 [1 .. 10] |> reduce{ it + it2 } == 55
    # method call with pipe operator instead of function.
    #T Operator_PipeOperator7 obj = {sum: {x, y -> x+y}}; 2 |> obj.sum(3) == 5
    #T Operator_PipeOperator8 9 |> [<System.Math>].Sqrt() == 3
    # piping with value expressions on the right-hand side.
    #T Operator_PipeOperator9  import 'math'; [2, 1, 6, 5, 9, 8, 4, 3, 7] |> sort |> sum |> math.sqrt |> math.floor == 6
    #T Operator_PipeOperator10 15 |> {x -> x * 2} == 30
    #T Operator_PipeOperator11 12 |> {It * 2} == 24
    # Verify that the 'this' pointer is set properly when invoked by the pipe operator as a property
    #T Operator_PipeOperator12 o = {v:2 s:{x -> x.map{it * this.v}}}; [1, 2, 3] |> o.s == [2, 4, 6]
    #O |> The pipe operator - applies a sequence of functions to a value e.g. <pre>[1..100] |&gt; match('^2') |&gt; sum() |&gt; sqrt() |&gt; floor() == 15</pre><pre>9 |&gt;[<System.Math>].Sqrt() == 3</pre><pre>16 |&gt; [<math>].Sqrt</pre><pre>10 |&gt; {it/5} == 2</pre>
    '|>' = [OperatorTableEntry] @{
        prec = 11;
        action = [TinyOperators]::PipeOperator
    }

    # Test the special forms for calling functions
    #T Call_WithLambda fn foo l -> l(); foo {123} == 123
    #T Call_WithString fn foo l -> l; foo "abc" == "abc"
    #T Call_WithDictionary fn foo l -> l; ( foo { a:1 b:22 } ).b  == 22
    #T Call_WithStringAndLambda fn foo s {h = {}; h[s] = body; h} ( foo 'abc' { 13 } ).abc() == 13
    #T Call_WithStringAndDictionary fn foo s {h = {}; h[s] = body; h} ( foo 'abc' {zork: 27 } ).abc.zork == 27
    #T Call_WithList fn foo l -> l; foo([1,2,3]) == [1, 2, 3]
    #T Call_WithStringAndLambda2 fn doit str { str + body() }; doit 'abc' { 'def' } == 'abcdef'
    # Test varargs capability with 'args' variable; note: args is always an array
    #T Call_WithArgs fn foo x, y, args -> args; foo(1, 2, 3, 4, 5) == [3, 4, 5]
    #T Call_WithArgs2 fn foo x, y, args -> args; foo(1, 2, 3) == [3]
    #T Call_WithArgs3 fn foo x, y, args -> args; foo(1, 2) == []
    #T Call_WithArgs4 fn foo x, y, args -> args; foo() == []
    #O ( Function and method invocation (actual implementation is with a custom class, but we need the recedence here.)
    '('   = [OperatorTableEntry] @{
        prec = 13
        action = { throw 'Action unused for "("' }.InvokeReturnAsIs
    }

    #O { Function and method invocation with a single lambda argument
    '{'   = [OperatorTableEntry] @{
        prec = 13
        action = { throw 'Action unused for "{"' }.InvokeReturnAsIs
    }

    #T Operator_Index1 a = [1,2,3]; a[1] == 2
    #T Operator_Index2 a = [1,2,3]; a[1] = 20; a == [1, 20, 3]
    #O [ Array index operation
    '['   = [OperatorTableEntry] @{
        prec = 15
        action = { throw 'Action unused for "["' }.InvokeReturnAsIs
    }

    #O ?[ Array index operation with null handling
    '?['  = [OperatorTableEntry] @{
        prec = 15
        action = { throw 'Action unused for "?["' }.InvokeReturnAsIs
    }

    #T Operator_PropertyRef1 "abc".length == 3
    #O . Property and method reference
    '.'   = [OperatorTableEntry] @{
        prec = 18;
        action = { throw 'Action unused for "."' }.InvokeReturnAsIs
    }

    #O ?. Property and method reference with null handling e.g. <pre>nul ?. foo == null</pre><pre>"abc"?.noSuchProp == null</pre>
    '?.'  = [OperatorTableEntry] @{
        prec = 18
        action = { throw 'Action unused for "?."' }.InvokeReturnAsIs
    }

    #T Operator_StarDot1  'abc' *. length == [3] # works on scalar but always returns array
    #T Operator_StarDot2  ['abc', 'defg', 'h'] *. length == [3, 4, 1]
    #T Operator_StarDot3  ['abc', 'defg', 'h'] *. substring(0, 1) == ['a', 'd', 'h'] # can use methods too
    #T Operator_StarDot4  [<Diagnostics.Process>].getprocesses() *. ws |> average > 0 # works on arbitrary ienumerables.
    #O *. Property and method references on collections. e.g. <pre>File.GetProcesses()*.WS.Sum()</pre>
    '*.'  = [OperatorTableEntry] @{
        prec = 18
        action = { throw 'Action unused for "*."' }.InvokeReturnAsIs
    }

    #T Operator_QuestionStarDot1  'abc' ?*. length == [3] # works on scalar but always returns array
    #T Operator_QuestionStarDot2  null ?*. length == null # works on scalar but always returns array
    #T Operator_QuestionStarDot3  ['abc', getdate(), 'h'] ?*. length == [3, 1]
    #T Operator_QuestionStarDot4  ['abc', null, 'h'] ?*. substring(0, 1) == ['a', 'h'] # can use methods too
    #T Operator_QuestionStarDot5  [<diagnostics.process>].getprocesses() *. ws |> average > 0 # works on arbitrary ienumerables.
    #O ?*. Property and method references on collections with null handling. e.g. <pre>ls()?*.length.Sum()</pre>
    '?*.'  = [OperatorTableEntry] @{
        prec = 18
        action = { throw 'Action unused for "?*."' }.InvokeReturnAsIs
    }

    #O :: The pattern composition operator e.g. <pre> a::b </pre><pre>'bob'::a::{it > 3}::_</pre>
    '::' = [OperatorTableEntry] @{
        prec = 20
        action = [TinyOperators]::PatternOperator
    }

    #BUGBUGBUG There is no %= operator
    #T Operator_Assign1 x = 3.14; 3.14 == x
    #T Operator_Assign2 x = 0; x += 2; 2 == x
    #T Operator_Assign3 x = 2; x *= 3; 6 == x
    #T Operator_Assign4 x = 9; x /= 3; 3 == x
    #T Operator_Assign5 x = 9; x -= 5; x == 4
    #T Operator_Assign7 a = b = c = 3; a == 3 && b == 3 && c == 3
    #T Operator_Assign8 r = if ((a = 14)) { a }; r == a && r == 14
    #T Operator_Assign9 r = [1,2,3]; r += 4; r == [1, 2, 3, 4]
    #T Operator_Assign10 s = "hi"; s += " there"; s == "hi there"
    #T Operator_Assign11 s = if (true) { "true" } else { 'false' }; s == 'true'
    #T Operator_Assign12 s = 1; s ?= 2; s == 1
    #T Operator_Assign13 s = null; s ?= 2; s == 2
    #T Operator_Assign14 s = 0; s ?= 2; s == 0
    #T Operator_Assign15 v = [1,2,3]; v[1] = 20; v == [1,20,3]
    #T Operator_Assign16 v = {a:1; b:2}; v.b = 20; v.b == 20
    #T Operator_Assign17 v = {a:1; b:2}; v['b'] = 20; v.b == 20
    #T Operator_Assign18 v = {a:{x: 10 y: 20} b:2}; v.a.x = 200; v.a.x == 200
    #T Operator_Assign19 v = {a:{x: 10 y: 20} b:2}; v['a']['x'] = 200; v.a.x == 200
    #T Operator_Assign20 (p1::p2 = [1,2]) then true
    #T Operator_Assign21 p1::p2 = [10, 20]; p1 == 10 && p2 == [20]
    #T Operator_Assign22 p1::p2::_ = [100, 200]; p1 == 100 && p2 == 200
    #T Operator_Assign23 p1::p2::p3::p4 = ['abc']; p1 == 'abc' && p2 == null && p3 == null && p4 == null
    #T Operator_Assign30 _ = 123; _ == null
    #T Operator_Assign31 null = 123; _ == null
    #T Operator_Assign32 [<int>] n = 123 then true  # Type-constrained variables
    #T Operator_Assign33 if ([<int>] n = 'abc') { false } else { true }
    #T Operator_PlusAssign1 a = 3; a += 1; a == 4
    #T Operator_PlusAssign2 a = "abc"; a += "d"; a == "abcd"
    #T Operator_PlusAssign3 a = [1, 2, 3]; a += 4; a == [1, 2, 3, 4]
    #T Operator_PlusAssign4 a = [1, 2, 3]; a += [4, 5]; a == [1, 2, 3, 4, 5]
    #T Operator_PlusAssign5 a = {a:1 b:2}; a += {b:20 c:30}; a.a == 1 && a.b == 2 && a.c == 30
    #O += The plus-assign operator e.g. <pre> a = 1; a += 1; a == 2</pre>
    '+=' = [OperatorTableEntry] @{
        prec = 2
        action = [TinyOperators]::PlusEquals
    }

    #T Operator_MinusAssign1 a = 3; a -= 1; a == 2
    #T Operator_MinusAssign2 a = [1, 2, 3]; a -= 2; a == [1, 3]
    #O -= The minus-assign operator. Works on numbers, strings and arrays e.g. <pre> a = 5; a -= 2; a == 3</pre><pre> a = [1, 2, 3]; a -= 2; a == [1, 3]</pre>
    '-=' = [OperatorTableEntry] @{
        prec = 2
        action = [TinyOperators]::MinusEquals
    }

    #T Operator_TimesAssign1 a = 3; a *= 2; a == 6
    #T Operator_TimesAssign2 a = [1, 2]; a *= 2; a == [1, 2, 1, 2]
    #T Operator_TimesAssign3 a = 'ab'; a *= 3; a == 'ababab'
    #O *= The times-assign operator. Like the times operator, it works on numbers, strings and lists e.g. <pre> a = 2; a *= 3; a == 6</pre><pre>a = 'a'; a *= 3; a == 'aaa'</pre>
    '*=' = [OperatorTableEntry] @{
        prec = 2;
        action = [TinyOperators]::StarEquals
    }

    #T Operator_DivideAssign1 a = 9; a /= 3; a == 3
    #T Operator_DivideAssign2 a = [1,2,3,4,5]; a /= 2; a == [[1, 2], [3, 4], [5]]
    #T Operator_DivideAssign3 a = [1,2,3,4,5]; a /= 2; a.count == 3
    #O /= The divide-assign operator, Like the / operator this works on number and arrayse.g. <pre> a = 12; a /= 3; a == 4</pre><pre>a = [1,2,3,4,5]; a /= 2; a == [[1, 2], [3, 4], [5]]<pre>
    '/=' = [OperatorTableEntry] @{
        prec = 2;
        action = [TinyOperators]::SlashEquals
    }

    #T Operator_QuestionAssign1 a = null; a ?= 3.14; a == 3.14
    #T Operator_QuestionAssign2 a = "hi"; a ?= 3.14; a == "hi"
    #O ?= do the assignment only if the variable is null (or does not exist) e.g. <pre> a ?= 123 </pre>
    '?=' = [OperatorTableEntry] @{
        prec = 2;
        action = [TinyOperators]::QuestionEquals
    }

    #T Operator_SimpleAssign1 a = 13; a == 13
    #T Operator_SimpleAssign2 a = [1, 2, 3]; a[1] = 20; a == [1, 20, 3]
    #T Operator_SimpleAssign3 a = {a:1 b:2}; a.a = 20; a['a'] == 20
    #T Operator_SimpleAssign4 a = {a:1 b:2}; a['b'] = 25; a.b == 25
    #O = basic assignment; can have patterns on the LHS; can chain assignments e.g. <pre>a = 1</pre><pre>a::b::_ = [1, 2]</pre><pre>a = b = c = 3</pre>
    '=' =  [OperatorTableEntry]@{
        prec = 2;
        action = [TinyOperators]::Equals
    }
}

################################################################
#
# Unary operator expression implementation
#
class UnaryOperator : Expression
{
    [Expression]
        $Expr

    Visit($invokable) {
        $this.Expr.Visit($invokable)
    }

    [string] ToString() { return "[UnaryOperator($($this.Token.Value))]" }

    UnaryOperator ([Token] $op, $expr) {
        $this.Token = $op
        $this.Expr = $expr
    }

    [object] Eval() {
        $val = $this.Expr.Eval()
        if ($this.Token.Value -eq '!') {
            return -not [tiny]::AsBool($val)
        }

        if ($this.Token.Value -eq '-') {
            try {

                if ($val -is [TinyList]) {
                    # Negate the individual members BUGBUGBUG does this even make sense?
                    return [TinyList]::new(@(foreach ($item in $val.list) {
                        - ($item)
                    }))
                }

                return -($val)
            }
            catch {
                errorMessage $_ $this.Token
            }
        }

        if ($this.Token.Value -eq '+') {
            try {
                # unary plus sums a list
                if ($val -is [TinyList]) {
                    return $val.Sum()
                }
                else {
                    return + ($val)
                }
            }
            catch {
                errorMessage $_ $this.Token
            }
            return $null
        }

        errorMessage "UnaryOperator: unknown operator $($this.Token)" $this.Token

        return $null
    }
}

################################################################
#
# Class that implements a cast operation.
#
class CastOperator : Expression
{
    [Expression]
        $Expr

    [Type]
        $Type

    Visit($invokable) {
        $this.Expr.Visit($invokable)
    }

    CastOperator([Token] $Token, [type] $Type, [Expression] $Expr) {
        $this.Token = $token
        $this.Type = $type
        $this.Expr = $expr
    }

    [object] Eval() {
        $val = $this.Expr.Eval()

        # special handling for bool because of TinyList
        if ($this.Type -eq [bool]) {
            return [Tiny]::AsBool($val)
        }

        try {
            return [LanguagePrimitives]::ConvertTo($val, $this.type)
        }
        catch {
            errorMessage $_ $this.Token
        }
        return $null
    }
}


################################################################
#
#G BinaryOperator = 'as' | '??' | '.' | '|>' | '?[' | '*' | '>=' | '*.' | '-='
#G                  | 'is' | '>' | '?.' | '-' | '!:>' | '**' | ':>' | '..' | '::' | '%'
#G                  | '<=' | '/=' | 'then' | '~' | '(' | '<' | '==' | '+=' | '!~' | '&&'
#G                  | '/~' | '=' | '?=' | '/' | '+' | '*~' | '*=' | '[' | 'isnot' | '{'
#G                  | '||' | '!<:' | '<:' | '-~' | '!=' ;;
#G
#
# Binary operator expression
#
class BinOp : Expression
{
    [Expression]
        $left

    [Expression]
        $right

    [String]
        $op

    [Object]
        $Operation

    Visit($invokable)
    {
        $this.left.Visit($invokable)
        $this.right.Visit($invokable)
        $invokable.Invoke($this)
    }

    [string] ToString() { return "[BinOp($($this.Op))]" }

    BinOp ([expression] $left, [expression] $right, [Token] $token, [string] $op) {
        $this.left  = $left
        $this.right = $right
        $this.token = $token
        $this.op    = $op
        # Get the method info for the scriptblock for this operator and cache it.
        $opAction = $script:OperatorTable[$op].action
        $this.operation = $opaction
        if ($null -eq $this.operation) {
            errorMessage "Operation for $($token.value) is null." $token
        }
    }

    [object] Eval() {
        $exception = $null
        try {
            # Invoke the method that implements this operator
            # It's up to the method to eval left and right.
            # This is necessary for the shortcut operators like
            # 'foo && bar'.
            $result = $this.Operation.Invoke($this.left, $this.right)
            return $result
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($exception -is [TinyException]) {
                if ($null -eq $exception.Token) {
                    $exception.Token = $this.Token
                }
                throw $exception
            }
            else {
                errorMessage $exception $this.Token
            }
        }

        # here to shut up the compiler; will never be run
        return $null
    }
}

################################################################
#
# A hard-coded addition binary operator on the theory that
# this will be faster than BinOp doing a method invoke.
#
class AddOp : BinOP {

    [Expression]
        $left

    [Expression]
        $right

    [String]
        $op

    [Object]
        $Operation

    AddOP ([expression] $left, [expression] $right, [Token] $token, [string] $op)
        : base ($left, $right, $token, $op) {
    }

    Visit($invokable)
    {
        $this.left.Visit($invokable)
        $this.right.Visit($invokable)
        $invokable.Invoke($this)
    }

    [object] Eval() {
        $exception = $null
        try {
            $lvalue = $this.left.Eval()
            $rvalue = $this.right.Eval()
            if ($lvalue -is [IDictionary] -and $rvalue -is [IDictionary]) {
                # Use Tiny semantics to merge 2 dictionaries; not PowerShell's
                foreach ($pair in $rvalue.GetEnumerator()) {
                    $key   = $pair.Key
                    $value = $pair.Value
                    if (-not $lvalue.COntains($key)) {
                        if ($value -is [TinyLambda]) {
                            $value = $value.Clone()
                            $value.TinyThis = $lvalue
                        }
                        $lvalue[$key] = $value
                    }
                }
                return $lvalue
            }
            else {
                if ($null -ne $lvalue -and $null -ne $rvalue) {
                    return ($lvalue + $rvalue)
                }
                elseif ($null -eq $rvalue) {
                    return ($lvalue)
                }
                else {
                    return $rvalue
                }
            }
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($exception -is [TinyException]) {
                if ($null -eq $exception.Token) {
                    $exception.Token = $this.Token
                }
                throw $exception
            }
            else {
                errorMessage $exception $this.Token
            }
        }

        # here to shut up the compiler; will never be run
        return $null
    }
}

################################################################
#
# A function call expression; handles
#      foo(2, 3)
# but also
#      {x, y -> x+y}(2,3)
#
class FunctionCall : Expression
{
    [expression[]]
        $ArgumentExpressions

    [object]
        $body

    [object]
        $functionToCall

    # If the name of the function is a constant, this member will hold it
    [string]
        $name

    [object]
        $ExtraArg

    [object]
        $ConstantInvokable

    # Pattern used in tracing functions
    static [regex]
        $TracePattern

    Visit($invokable) {
        foreach ($arg in $this.ArgumentExpressions) {
            $arg.Visit($invokable)
        }
        if ($this.Body) { $this.body.Visit($invokable) }
        $invokable.Invoke($this)
    }

    FunctionCall ($token, $functionToCall, [Expression[]] $argumentExpressions, [object] $body) {
        $this.token = if ($token) { $token } else { [token]::new() }
        $this.functionToCall = $functionToCall
        if ($functionToCall -is [Variable]) {
            $this.Name = $functionTocall.name
        }
        #BUGBUGBUG handle constant functions - need to make sure its an invokable...
        elseif ($functionToCall -is [Constant]) {
            $this.Name = $functionTocall.token.value
            $this.ConstantInvokable = $functionToCall.Value
        }
        elseif ($functionToCall -is [Literal] -and $functionToCall.Value -is [string] ) {
            $this.name = $this.functionTocall.Value
        }
        $this.ArgumentExpressions = $argumentExpressions
        $this.body = $body
    }

    # Used by the pipe (|>) operator to invoke the function with an extra argument.
    [object] EvalWithExtraArg($arg) {
        try {
            $this.ExtraArg = $arg
            return $this.Eval()
        }
        finally {
           $this.ExtraArg = $null
        }
    }

    [object] Eval() {

        if ($this.ArgumentExpressions) {
            $argsLength = $this.ArgumentExpressions.Length
        }
        else {
            $argsLength = 0
        }

        $argOffset = $exprOffset = 0

        if ($this.extraArg) {
            $argsLength++
            $argOffset++
            $arguments = [object[]]::new($argsLength)
            $arguments[0] = $this.extraArg.Eval()
        }
        else {
            $arguments = [object[]]::new($argsLength)
        }

        # Evaluate the arguments
        while ($argOffset -lt $argsLength) {
            $arguments[$argOffset++] = $this.ArgumentExpressions[$exprOffset++].Eval()
        }

        try {
            if (-not ($val = $this.Name)) {
                if ($this.functionToCall -is [Expression]) {
                    $val = $this.FunctionToCall.Eval()
                }
                else {
                    $val = $this.FunctionToCall
                }
            }

            if ($null -eq $val) {
                errorMessage ("Function to call was null; FunctionToCall: " +
                              "$($this.FunctionToCall)'") $this.Token
            }

            if ($null -ne [FunctionCall]::Tracepattern -and [FunctionCall]::TracePattern.Match($val).Success) {
                Write-Host -fore yellow "r/$([FunctionCall]::TracePattern)/ >> $val ( $($arguments -join ', ') )"
            }

            if ($val -is [string]) {
                # Execute a function bound in the variable or constant table.
                :return_from_function do {
                    # See if there is a constant function first.
                    if ($null -eq ($func = $this.ConstantInvokable)) {
                        # Try lambdas assigned to a variable
                        # BUGBUGBUG if the function was found in the constants table should change this to be a constant call
                        if ($null -eq ($func = [scopes]::TryGetVariable($val))) {
                            errorMessage "function '$($val)' is not defined" $this.Token
                        }
                    }

                    if ($func -is [TinyLambda]) {
                        # Set the 'body' magic var.
                        $bodyVal = $null
                        if ($null -ne $this.body) {
                            $bodyVal = if ($this.body -is [TinyLambda]) {
                                $this.body
                            }
                            else {
                                $this.Body.Eval()
                            }
                        }

                        $a1 = if ($argsLength -gt 0) { $arguments[0] } else { [AutomationNull]::Value }
                        $a2 = if ($argsLength -gt 1) { $arguments[1] } else { [AutomationNull]::Value }
                        $result = $func.Invoke($arguments, $a1, $a2, $bodyval)
#say "Returning from TinyLambda 0, result = $result"
                        return $result
                    }
                    elseif ($func -is [List[TinyLambda]]) {
                        $lindex = 1
                        foreach ($lambda in $func) {
                            $arity = $lambda.Parameters.Length;
                            if ($null -ne [FunctionCall]::TracePattern -and [FunctionCall]::TracePattern.Match($val).Success) {
                                Write-Host -fore yellow (
                                    "r/$([FunctionCall]::tracepattern)/ >>>> [$lindex] " +
                                    "$val/$arity ($($arguments -join ', '))"
                                )
                            }
                            $lindex++

                            # BUGBUGBUG need to handle parameterless functions
                            # Arity must match in function sets
                            if ($null -eq $lambda.Parameters -or $arguments.Length -ne $lambda.Parameters.Length) {
                                if ($null -ne [FunctionCall]::tracePattern -and [FunctionCall]::tracePattern.Match($val).Success) {
                                    Write-Host -fore yellow (
                                        "r/$([FunctionCall]::TracePattern)/ >>>>>> [$lindex] $val/$arity ($($arguments -join ', ')) " +
                                        "- Arity didn't matched; bind failed, skipping this overload."
                                    )
                                }
                                continue
                            }

                            $bindSuccess = $true
                            $a1 = if ($argsLength -gt 0) { $arguments[0] } else { [AutomationNull]::Value }
                            $a2 = if ($argsLength -gt 1) { $arguments[1] } else { [AutomationNull]::Value }
                            $result = $lambda.Invoke($arguments, $a1, $a2, $null, [ref] $bindSuccess)
                            if ($bindSuccess -eq $false) {
                                if ($null -ne [FunctionCall]::TracePattern -and [FunctionCall]::tracePattern.Match($val).Success) {
                                    Write-Host -fore yellow (
                                        "r/$([FunctionCall]::tracepattern)/ >>>>>> [$lindex] $val/$arity ($($arguments -join ', ')) " +
                                        "- bind failed, skipping this overload."
                                    )
                                }
                                continue;
                            }
                            else {
#say "Returning from FunctionSet, result = $result"
                                return $result
                            }
                        }

                        #BUGBUGBUG - need to generate error at top level of the function set
                        #errorMessage "Function set '$val': no matching function found for arguments: $($arguments -join ', ')"
                        return [AutomationNull]::Value
                    }
                    elseif ($func -is [ScriptBlock]) {
                        # set up the 'body' magic var.
                        $bodyVal = $null
                        if ($null -ne $this.body) {
                            $bodyVal = if ($this.body -is [TinyLambda]) {
                                $this.body
                            }
                            else {
                                $this.Body.Eval()
                            }
                            $newArguments = [object[]]::new($arguments.Length+1)
                            $arguments.CopyTo($newArguments, 0)
                            $newArguments[-1] = $bodyVal
                            $arguments = $newArguments
                        }

                        $result = $func.InvokeReturnAsIs($arguments)
                        return $result
                    }
                    elseif ($func -is [CommandInfo]) {
                        $pipelineinput = @()
                        if ($null -ne $this.ExtraArg) {
                            $pipelineinput = $arguments[0]
                            if ($pipelineinput -is [TinyList]) {
                                $pipeLineInput = @($pipeLineinput.list)
                            }
                        }
                        else {
                            $pipelineInput = @($pipelineinput)
                        }

                        if ($pipelineInput.Length -gt 0) {
                            #Remove the pipeline input from the arguments
                            $newargs = [object[]]::new($arguments.length-1)
                            # Copy(sourceArray, sourceIndex, destinationArray, destinationIndex, length)
                            [array]::Copy($arguments, 1, $newargs, 0, $arguments.length-1)
                        }
                        else {
                            $newargs = $arguments
                        }

                        if ($newArgs.length -eq 1 -and $newArgs[0] -is [IDictionary]) {
                            $newArgs = $newargs[0]
                        }

                        # handle the 'body' magic var.
                        $bodyVal = @{}
                        if ($null -ne $this.body) {
                            $bodyVal = if ($this.body -is [TinyLambda]) {
                                            $this.body
                                        }
                                        else {
                                            $this.Body.Eval()
                                        }
                        }

                        if ($newargs) {
                            if ($pipelineInput.Length -gt 0) {
                                $result =  $pipelineInput | & $func @newargs @bodyVal
                            }
                            else {
                                $result =  & $func @newargs @bodyVal
                            }
                        }
                        else {
                            if ($pipelineinput.Length -gt 0) {
                                $result = $pipelineinput | & $func @bodyVal
                            }
                            else {
                                $result = & $func @bodyVal
                            }
                        }

                        if ($result -is [IList]) {
                            $result = [TinyList]::New($result)
                        }
                        return $result
                    }
                    else {
#say "Handling generic method invoke for: '$val' type: $($func.name)"
                        # Handle the 'body' magic var.
                        $bodyVal = $null
                        if ($null -ne $this.body) {
                            $bodyVal = if ($this.body -is [TinyLambda]) {
#say "body is lambda $($this.body)"
                                $this.body
                            }
                            else {
#say "body is value $($this.body)"
                                $this.Body.Eval()
                            }

                            $newArguments = [object[]]::new($arguments.Length + 1)
                            $arguments.CopyTo($newArguments, 0)
                            $newArguments[-1] = $bodyVal
                            $arguments = $newArguments
                        }

#say "Arguments are '$($arguments -join ', ')'"
                        $result = $func.Invoke($arguments)
#say "Returning from Generic Invoke 1, result = '$result' $($func.Name)"
                        return $result
                    }
                }
                until ($false)

                # We get here from a 'return' statement
                return $script:FunctionReturnValue
            }
            elseif ($val -is [ScriptBlock]) {
                # and the 'body' magic var.
                $bodyVal = $null
                if ($null -ne $this.body) {
                    $bodyVal = if ($this.body -is [TinyLambda])
                        { $this.body }
                    else
                        { $this.Body.Eval() }

                    $newArguments = [object[]]::new($arguments.Length+1)
                    $arguments.CopyTo($newArguments, 0)
                    $newArguments[-1] = $bodyVal
                    $arguments = $newArguments
                }

                # Is this really necessary
                # return $val.InvokeReturnAsIs($arguments).PSObject.BaseObject
                return $val.InvokeReturnAsIs($arguments)
            }
            elseif ($val -is [IDictionary]) {
                # The sequence a = {} (2 + 2) * 4 is likely enough that a special error is generated
                errorMessage ("Dictionaries are not invokeable; " +
                              "if you have {...} ( .. ) put a ';' between them: " +
                              "{...}; ( ... ).") $this.Token
            }
            elseif ($val -is [TinyLambda]) {
                # Set the 'body' magic var.
                $bodyVal = $null
                if ($null -ne $this.body) {
                    $bodyVal = if ($this.body -is [TinyLambda])
                        { $this.body }
                    else
                        { $this.Body.Eval() }
                }
                $a1 = if ($argsLength -gt 0) { $arguments[0] } else { [AutomationNull]::Value }
                $a2 = if ($argsLength -gt 1) { $arguments[1] } else { [AutomationNull]::Value }
                $result = $val.Invoke($arguments, $a1, $a2, $bodyval)
#say "Returning from TinyLambda 2, result = $result"
                return $result
            }
            else {
                # Handle generic invokables...
#say 'Handling generic invokable'

                # and the 'body' magic var.
                $bodyVal = $null
                if ($null -ne $this.body) {
                    $bodyVal = if ($this.body -is [TinyLambda]) {
                                    $this.body
                               }
                               else {
                                    $this.Body.Eval()
                               }
                    $newArguments = [object[]]::new($arguments.Length+1)
                    $arguments.CopyTo($newArguments, 0)
                    $newArguments[-1] = $bodyVal
                    $arguments = $newArguments
                }

#say "Arguments: $arguments"
                :return_from_function do {
                    $result = $val.Invoke($arguments)
#say "al invoke returned $result"
                    return $result
                }
                until ($false)

                # We get here from a 'return' statement
                return $script:FunctionReturnValue
            }
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($exception -is [TinyException]) {
                if ($null -eq $exception.Token) {
                    $exception.Token = $this.Token
                }
                throw $exception
            }
            else {
                errorMessage $exception  $this.Token
            }
        }
        # never reached; here to shut up the compiler
        return $null
    }

    [string] ToString() { return "[FunctionCall($($this.FunctionToCall))]" }
}

################################################################
#
# Class that implements an array index assignment e.g. arr[13] = 123
#
class ArrayIndexExpression : Assignable
{
    [Expression]
        $Value

    [Expression]
        $indexExpr

    # If the index is constant, this member will hold it
    [object]
        $ConstantindexValue

    [bool]
        $HandleNull

    Visit($invokable)
    {
        $this.value.Visit($invokable)
        $this.indexExpr.Visit($invokable)
        $invokable.Invoke($this)
    }

    ArrayIndexExpression ($token, [Expression] $Value, [Expression] $expression, [bool] $handleNull) {
        $this.Token = $token
        $this.Value = $Value
        $this.IndexExpr = $expression
        if ($expression -is [Literal]) {
            $this.ConstantIndexValue = $expression.Value
        }
        $this.HandleNull = $handleNull
    }

    [object] Eval () {
        $array = $this.Value.Eval()
        if (-not $array) {
            if ($this.HandleNull) {
                return $null
            }
            else {
                errormessage "Can't index into a null array" $this.Token
            }
        }

        # Use the constant value if there is one
        $indexValue = $this.ConstantIndexValue
        if ($null -eq $indexValue) {
            $indexValue = $this.indexExpr.Eval()
            if ($null -eq $indexValue) {
                errormessage "Array index evaluated to null" $this.Token
            }
        }

        $result = $null
        try {
            if ($array -is [TinyList]) {
                $result = $array.list[$indexValue]
            }
            else {
                $result = $array[$indexValue]
            }
        } catch {
            errorMessage $_ $this.Token
        }
        return $result
    }

    Set ($value) {
        $array = $this.Value.Eval()
        if (-not $array) {
            errormessage "Can't index into a null array" $this.Token
        }

        # Use the constant value if there is one
        $indexValue = $this.ConstantIndexValue
        if ($null -eq $indexValue) {
            $indexValue = $this.indexExpr.Eval()
            if ($null -eq $indexValue) {
                errormessage "Array index evaluated to null" $this.Token
            }
        }

        try {
            if ($array -is [TinyList]) {
                $array.list[$indexValue] = $value
            }
            else {
                $array[$indexValue] = $value
            }
        }
        catch {
            errorMessage $_ $this.Token
        }
    }
}

################################################################
#
# Class that implements a property read, property assign or method invocation expression.
# This class handles both instance and static member operations as well as collections
# with the *. operator.
# e.g. abc.prop = "hi"; println("prop is ${abc.prop}"); "abcdef".substring(1,3)
#       [<string>].Compare("hi", "there")  ["abc", "de", "f"] *. length
#
class PropertyExpression : Assignable {

    [Expression]
        $Left

    [Expression]
        $Right

    [Bool]
        $HandleNull

    # If true then the operator is '*.' instead of '.'
    [Bool]
        $HandleCollection

    [string]
        $MemberName

    [string] ToString() {
        return ".$($this.membername)"
    }

    Visit($invokable)
    {
        $this.left.Visit($invokable)
        $this.right.Visit($invokable)
        $invokable.Invoke($this)
    }

    PropertyExpression ($token, [Expression] $left, [Expression] $Right, [bool] $HandleNull, [bool] $HandleCollection) {
        $this.Token = $token
        $this.Left  = $Left
        $this.Right = $Right
        $this.HandleCollection = $HandleCollection

        # Figure out the name of the thing to invoke
        $name = $null
        if ($right -is [MethodInvocation]) {
            $name = $this.Right.MethodNameExpr
            if ($name -is [Variable]) {
                $name = $name.name
            }
            elseif ($name -is [Constant]) {
                $name = $name.Token.Value
            }
        }
        elseif ($this.Right -is [Literal]) {
            $name = $this.Right.value
        }
        elseif ($this.Right -is [Variable]) {
            $name = $this.Right.Name
        }
        elseif ($this.Right -is [Constant]) {
#BUGBUGBUG shouldn't this just be Right.Value ?
            $name = $this.Right.Token.Value
        }

        $this.MemberName = $name

        if ($HandleNull -eq $false -and $left -is [PropertyExpression]) {
            # Propigate null-handling up the tree
            $this.HandleNull = $left.HandleNull
        }
        else {
            $this.HandleNull = $HandleNull
        }
    }

    [object]
        $ExtraArg

    # Used by the pipe (|>) operator.
    [object] EvalWithExtraArg($arg) {
        $this.ExtraArg = $arg
        try {
            return $this.Eval()
        }
        finally {
           $this.ExtraArg = $null
        }
    }

    [object] Eval() {

        # Figure out the name of the thing to invoke.
        if (-not ($name = $this.MemberName)) {
            $name = $this.Right.Eval()
            if ($null -eq $name) {
                errorMessage 'The expression must result in a non-null method name.' $this.token
           }
        }

        # Figure out what to invoke it on.
        $lvalue = $this.Left.Eval()
        if ($null -eq $lvalue) {
            if ($this.HandleNull) {
                return $null
            }
            errorMessage "Cannot access property or method '$name' on a null value" $this.Token
        }

        if ($this.HandleCollection -and $lvalue -isnot [TinyList] -and $lvalue -isnot [IEnumerable]) {
            errorMessage 'The *. operator requires a collection to work on.' $this.Token
        }

        $members = $null # needed to make the variable analyser happy

        # Handle static members first
        if ($lvalue -is [type] -and
           ((($members = $lvalue.GetMembers("public,static")) -and $members.Name -contains $name) -or
           $name -eq 'new'))
        {
            if ($this.right -is [MethodInvocation]) {
                 return $this.Right.Invoke($lvalue, $this.ExtraArg, $true)
            }
            else {
                return $lvalue::($name)
            }
        }
        # Handle instance members
        else {
            $result = $null
            try {
                if ($this.HandleCollection) {
                    $result = [TinyList]::new()
                    $list = if ($lvalue -is [TinyList]) { $lvalue.list } else { $lvalue }
                    # Iterate through all of items in the list
                    if ($this.right -is [MethodInvocation]) {
                        foreach ($lvalue in $list) {
                            try {
                                $result.Add($this.Right.Invoke($lvalue, $this.ExtraArg,  $false))
                            }
                            catch [PropertyNotFoundException] {
                                if (-not $this.HandleNull) {
                                    throw $_
                                }
                            }
                        }
                    }
                    else {
                        foreach ($lvalue in $list) {
                            try {
                                $result.Add($lvalue.($name))
                            }
                            catch [PropertyNotFoundException] {
                                if (-not $this.HandleNull) {
                                    throw $_
                                }
                            }
                        }
                    }
                }
                else {
                    if ($this.right -is [MethodInvocation]) {
                        try {
                            $result = $this.Right.Invoke($lvalue, $this.ExtraArg,  $false)
                        }
                        catch [PropertyNotFoundException] {
                            if (-not $this.HandleNull) {
                                throw $_
                            }
                        }
                    }
                    else {
                        try {
                            $result = $lvalue.($name)
                        }
                        catch [PropertyNotFoundException] {
                            if (-not $this.HandleNull) {
                                throw $_
                            }
                        }
                    }
                }
            }
            catch {
                $exception = $_.Exception
                while ($exception -is [MethodInvocationException]) {
                    $exception = $exception.GetBaseException()
                }

                if ($exception -is [PropertyNotFoundException] -and $this.HandleNull) {
                    return $null
                }

                if ($exception -is [TinyException]) {
                    if ($null -eq $exception.Token) {
                        $exception.Token = $this.Token
                    }
                    throw $exception
                }
                else {
                    errorMessage $exception $this.Token
                }
            }

            return $result
        }
    }

    Set ($value) {
        $obj = $this.Left.Eval()
        if ($null -eq $obj) {
            errormessage "Can't set a property on a null object" $this.token
        }

        # Figure out the name of the thing to invoke
        if (-not ($name = $this.MemberName)) {
            $name = $this.Right.Eval()
            if ($null -eq $name) {
                errorMessage 'The expression must result in a non-null method name.' $this.token
           }
        }

        if ($obj -is [type] -and $obj.getmembers("public,static").Name -contains $name) {
            $obj::($name) = $value
        }
        else {
            $obj.($name) = $value
        }
    }
}

################################################################
#
# Represents an assignment operation. Adds Set() behwviour
# on top of the normal BinOp protocol. Evals are still handled
# by the delegate. Set is only used for chained assignments
# as in 'a = b = c = 3'
#
class Assignment : BinOp
{
    Assignment ([Expression] $left, [Expression] $right, [Token] $token, [string] $op)
        : base($left, $right, $token, $op) { }

    Set($value) {
        if ($this.Op -ne '=') {
            errorMessage 'Only simple assignments can be chained e.g. a = b = c = 3' $this.Token
        }

        if ($this.Right -is [Assignable] -or $this.Right -is [Assignment]) {
            $this.Right.Set($value)
        }

        $this.Left.Set($value)
    }
}

################################################################
#
# The base class that all Tiny statements derive from
#
class Statement : Expression { }

################################################################
#
# The if statement implementation
#
class IfStatement : Statement
{
    [Expression]
        $condition

    [Expression]
        $ifPart

    [Expression]
        $elsePart

    Visit($invokable)
    {
        $this.condition.Visit($invokable)
        $this.ifPart.Visit($invokable)
        if ($this.elsePart) { $this.ElsePart.Visit($invokable) }
        $invokable.Invoke($this)
    }

    IfStatement ($token, $condition, $ifPart, $elsePart) {
        $this.Token = $token
        $this.condition = $condition
        $this.ifPart = $ifPart
        $this.elsePart = $elsePart
    }

    [object] Eval() {
        $condVal = [Tiny]::AsBool($this.Condition.Eval())
        if ($condVal) {
            return $this.ifPart.Eval()
        }
        else {
            if ($this.elsePart) {
                return $this.elsePart.eval()
            }
            else {
                return $null
            }
        }
    }
}

################################################################
#
# the while statement; loops while a condition is true
# example: while (val < 10) { println("val is " + val); val += 1 }
#
class WhileStatement : Statement
{
    [Expression]
        $condition

    [StatementList]
        $body

    WhileStatement ([Token] $token, [Expression] $condition, [StatementList] $body) {
        $this.Token = $token
        $this.condition = $condition
        $this.body = $body
    }

    Visit($invokable) {
        $this.condition.Visit($invokable)
        $this.body.Visit($invokable)
        $invokable.Invoke($this)
    }

    [object] Eval() {
        [object[]] $result = @(:inner while ($true) {
            if (-not [Tiny]::AsBool($this.condition.Eval())) {
                break
            }

            $this.body.Eval()
        }) -ne $null # filter out nulls

        if ($result.Count) {
            return [TinyList]::new($result)
        }
        else {
            return $null
        }
    }
}

################################################################
#
# The 'foreach' statement; iterates over a collection of values
# example: foreach (val in [1,2,3,4,5]) { println("val is $i") }
#
class foreachStatement : Statement
{
    [Assignable]
        $loopVariable

    [Expression]
        $enumerable

    [object[]]
        $body

    Visit($invokable) {
        $this.enumerable.Visit($invokable)
        $this.body.Visit($invokable)
        $invokable.Invoke($this)
    }

    foreachStatement ($token, [Assignable] $name, [Expression] $enumerable, [StatementList] $body) {
        $this.Token = $token
        $this.loopvariable = $name
        $this.enumerable = $enumerable
        $this.body = $body
    }

    [object] Eval() {
        $list = $this.enumerable.Eval()
        if ($list -is [TinyList]) {
            $list = $list.list
        }

        $bodyExpr = $this.Body

        if ($this.loopVariable -is [Pattern] -or $this.loopVariable -is [PropertyPattern]) {
            [object[]] $result = @(:inner foreach ($value in $list) {
                # with patterns, only execute the loop body if the match succeeded
                if ($this.loopVariable.Set($value)) {
                    $bodyExpr.eval()
                }
            }) -ne $null # filter out nulls
        }
        else {
            [object[]] $result = @(:inner foreach ($value in $list) {
                [void] $this.loopVariable.Set($value)
                $bodyExpr.eval()
            }) -ne $null
        }

        if ($result.Count) {
            return [TinyList]::new($result)
        }
        else {
            return $null
        }
    }
}

################################################################
#
# The 'match' statement; matches a value against a set of patterns
# example: match val | 1 -> "one" | 2 -> two | -> "default"
#
class MatchStatement : Statement
{
    [Expression]
        $MatchValue

    [OrderedDictionary]
        $Pairs

    [Expression]
        $DefaultAction

    [bool]
        $EvalList

    Visit($invokable) {
        $this.MatchValue.Visit($invokable)
        foreach ($pair in $this.Pairs) {
            if ($pair.Key -is [Expression]) {
                $pair.Key.Visit($invokable)
            }
            if ($pair.Value -is [Expression]) {
                $pair.Value.Visit($invokable)
            }
        }
        $invokable.Invoke($this)
    }

    MatchStatement ([Token] $Token, [object] $MatchValue, [OrderedDictionary] $Pairs,
            [Expression] $DefaultAction, $evalList) {
        $this.Token = $Token
        $this.MatchValue = $MatchValue
        $this.DefaultAction = $DefaultAction
        $this.EvalList = $evalList
        $updatedPairs = [OrderedDictionary]::new()
#say "match pairs" $pairs
#foreach ($p in $pairs.GetEnumerator()) { say "key" $p.key "value" $p.value }
        foreach ($p in $pairs.GetEnumerator()) {
            $key = $p.Key
#BUGBUGBUGBUGBUG
            # Fold patterns
            if ($key -is [PatternLiteral]) {
                $updatedPairs[$Key.Value] = $p.Value
            }
            # Fold literals (note: OrderedDictionary doesn't hold value types...?)
            elseif ($key -is [Literal] -and $null -ne $key.Value -and
                $key.Value.GetType().IsValueType -eq $false) {
                $updatedPairs[$key.Value] = $p.Value
            }
            # Otherwise just copy the case expression
            else {
                $updatedPairs[$p.Key] = $p.Value
            }
        }

        $this.Pairs = $UpdatedPairs
    }

    [object] Eval() {
        $valToMatch = $this.MatchValue.Eval()
        if ($this.EvalList) {
            if ($valToMatch -is [TinyList]) {
                $valToMatch = $valToMatch.List
            }

            $result = [TinyList]::New()
            foreach ($item in $valToMatch) {
                $val = $this.ProcessItem( $item )
                if ($null -ne $val) {
                    $result.Add($val)
                }
            }

            return $result
        }
        else {
            return $this.ProcessItem( $valToMatch )
        }
    }

    # Handles evaluating an action in the case of a match
    static [object] ProcessAction([Expression] $action, $it) {
        if ($action -is [Literal] -and $action.Value -is [TinyLambda]) {
            # Eval the body directly with no additional binding
            return $action.Value.Body.Eval()
        }
        elseif ($action -isnot [Assignment]) {
            return $action.Eval()
        }
        else {
            $null = $action.Eval()
            return $null
        }
    }

    # Handles the matching process
    [object] ProcessItem($valToMatch) {
        $old_it = [AutomationNull]::Value
        if ($this.EvalList) {
            $old_it = $null
            if ([scopes]::scopes[0].TryGetValue('it', [ref] $old_it) -eq $false) {
                $old_it = [AutomationNull]::Value
            }
        }

        $result = $null
        $m = $null
        [bool] $gotMatch = $false
        try {
            foreach ($p in $this.Pairs.GetEnumerator()) {
                # Set the 'it' variable for actions if the keyword was 'matchlist'
                if ($this.EvalList) {
                    [scopes]::scopes[0]['it'] = $valToMatch
                }

                # handle or'ed conditions
                $key = $p.Key
                # match on constant regular expressions
                if ($key -is [RegexLiteral]) {
                    if ($key.Match($valToMatch)) {
                        $gotMatch = $true
                        $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                        break
                    }
                }
                # Match on constant Type objects
                elseif ($key -is [Type] -and $key -ne [MatchStatement]) {
                    if ($valToMatch -is $key) {
                        $gotMatch = $true
                        $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                        break
                    }
                }
                # lambda conditions
                elseif ($key -is [TinyLambda]) {
                    if ($key.dot(@($valToMatch), $valToMatch, [AutomationNull]::Value, $null)) {
                        $gotMatch = $true
                        $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                        break
                    }
                }
                # Pattern matching
                elseif ($key -is [Pattern]) {
                    if ($valToMatch -is [TinyList] -and $valToMatch.Count -ge $key.elements.Length-1) {
                        if ($key.Set($valToMatch)) {
                            $gotMatch = $true
                            $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                            break
                        }
                    }
                }
                # BUGBUGBUG if [Assignable] is used, then methods & properties can't
                #           be used as values to match.
                # elseif ($key -is [Assignable]) {
                elseif ($key -is [PropertyPattern] -or $key -is [Variable]) {
                    if ($key.Set($valToMatch)) {
                        $gotMatch = $true
                        $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                        break
                    }
                }
                elseif ($key -is [Constant] -and ($key.Token.Value -eq '_' -or $key.Value -eq $valToMatch)) {
                    $gotMatch = $true
                    $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                    break
                }
                elseif ($key -is [Expression]) {
                    $caseValue = $Key.Eval()
                    # Expression returned a type
                    if ($caseValue -is [type] -and $valToMatch -is $caseValue) {
                        $gotMatch = $true
                        $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                        break
                    }
                    #BUGBUG - need to deal with expressions returning property patterns
                    # Expression returned a pattern
                    elseif ($caseValue -is [Pattern]) {
                        if ($valToMatch -is [TinyList] -and $valToMatch.Count -ge $caseValue.elements.Length-1) {
                            if ($casevalue.Set($valToMatch)) {
                                $gotMatch = $true
                                $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                                break
                            }
                        }
                    }
                    # Expression returned a regex
                    elseif ($caseValue -is [RegexLiteral]) {
                        $m = $caseValue.Match($valToMatch)
                        if ($m) {
                            $gotMatch = $true
                            $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                            break
                        }
                    }
                    elseif ($caseValue -is [Regex]) {
                        # Wrap the regex in a RegexLiteral to get the match-variable setting behaviour
                        $rel = [RegexLiteral]::new($null, 'matches', $caseValue)
                        $m = $rel.Match($valToMatch)
                        if ($m) {
                            $gotMatch = $true
                            $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                            break
                        }
                    }
                    elseif ($caseValue -eq $valToMatch) {
                        $gotMatch = $true
                        $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                        break
                    }
                }
                elseif ($key -eq $valToMatch) {
                    $gotMatch = $true
                    $result = [MatchStatement]::ProcessAction($p.Value, $valToMatch)
                    break
                }
            }
            if (-not $gotMatch -and $this.DefaultAction) {
                $result = [MatchStatement]::ProcessAction($this.defaultAction, $valToMatch)
            }
        }
        finally {
            if ($this.EvalList) {
                # If 'it' had a previous value in the current scope, restore it otherwise remove the variable.
                if ($old_it -ne [AutomationNull]::Value) {
                    [scopes]::scopes[0]['it'] = $old_it
                }
                else {
                    [scopes]::scopes[0].Remove('it')
                }
            }
        }
        return $result
    }

    # Utility that emulates how PowerShell handles the $match variable
    static [HashTable] GroupsToHash ($matchData) {
        $result = @{}
        foreach ($g in $matchData.groups) {
            $result[[int]($g.name)] = $g.value
        }
        return $result
    }
}

################################################################
#
# Implements the 'break' statement
#
class BreakStatement : Statement
{
    BreakStatement([Token] $token) { $this.Token = $token }

    Eval() { breakInner }
}

# Classes can't do non-local breaks so we have the class call a function.
function breakInner { break "inner" }

################################################################
#
# Implements the 'continue' statement
#
class ContinueStatement : Statement
{
    ContinueStatement([Token] $token) { $this.Token = $token }

    Eval() { continueInner }
}
function continueInner { continue "inner" }

################################################################
#
# Implements the 'return' statement
#
class ReturnStatement : Statement
{
    [Expression]
        $ValueToReturn

    Visit($invokable) {
        if ($this.ValueToReturn) {
            $this.ValueToREturn.visit($invokable)
        }
        $invokable.Invoke($this)
    }

    ReturnStatement([Token] $Token, [Expression] $valueToReturn) {
        $this.Token = $token
        $this.ValueToReturn = $valueToReturn
    }

    Eval () {
        $script:FunctionReturnValue =
            if ($null -ne $this.ValueToReturn) {
                $this.ValueToReturn.Eval()
            }
            else {
                $null
            }

        # Now cause the non-local break.
        returnFromFunction
    }
}
function returnFromFunction { break return_from_function }

################################################################
#
# Implements the 'throw' statement
#
class ThrowStatement : Statement
{
    [Expression] $WhatToThrow

    Visit($invokable) {
        if ($this.WhatToThrow) {
            $this.WhatToThrow.Visit($invokable)
        }
        $invokable.Invoke($this)
    }

    ThrowStatement([Token] $token, [Expression] $whatToThrow) {
        $this.Token = $token
        $this.WhatToThrow = $whatToThrow
    }

    Eval() {
        $value = $null
        if ($null -ne $this.WhatToThrow) {
            $value = $this.WhatToThrow.Eval()
        }

        if ($null -eq $value) {
            $value = "<<Exception>>"
        }

        errorMessage $value $this.Token
    }
}

################################################################
#
# Implements an array literal expression
#
class ArrayLiteral : LiteralBase
{
    [Expression[]]
        $expressions

    Visit($invokable) {
        if ($this.expressions) {
            foreach($expr in $this.expressions) {
                if ($expr -and $expr -is [Expression]) {
                    $expr.Visit($invokable)
                }
            }
        }
        $invokable.Invoke($this)
    }

    ArrayLiteral ([Token] $token, [object[]] $expressions) {
        $this.token = $token
        $this.expressions = $expressions
    }

    [object] Eval() {
        $array = [TinyList]::new()
        if ($this.Expressions) {
            foreach ($e in $this.Expressions) {
                # if the element is a range operation, append it to the result
                if ($e -is [BinOp] -and $e.Op -eq "..") {
                    $val = $e.Eval()
                    if ($null -ne $val -and $val -is [TinyList]) {
                        $array.AddRange($val.list)
                    }
                    else {
                        $array.add($val)
                    }
                }
                else {
                    $array.Add($e.Eval())
                }
            }
        }
        return $array
    }
}

################################################################
#
# Implements the try/catch/finally statement
#
class TryCatchStatement : Statement
{
    [StatementList]
        $TryPart

    [StatementList]
        $CatchPart

    [StatementList]
        $FinallyPart

    Visit($invokable) {
        if ($null -ne $this.tryPart)     { $this.tryPart.Visit($invokable) }
        if ($null -ne $this.catchPart)   { $this.catchPart.Visit($invokable) }
        if ($null -ne $this.finallyPart) { $this.finallyPart.Visit($invokable) }

        $invokable.Invoke($this)
    }

    TryCatchStatement ([token] $token, [statementlist] $tryPart, [statementlist] $catchpart,
            [statementlist] $finallyPart) {
        $this.Token = $token
        $this.TryPart = $tryPart
        $this.CatchPart = $catchPart
        $this.FinallyPart = $finallyPart
    }

    [object] Eval() {
        $result = $null
        try {
            $result = $this.tryPart.Eval()
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($this.CatchPart) {
                $current_scope = [scopes]::scopes[0]
                $old_it = $current_scope['it']
                $current_scope['it'] = $exception
                try {
                    $result = $this.CatchPart.Eval()
                }
                finally {
                    $current_scope['it'] = $old_it
                }
            }
            else {
                throw $exception
            }
        }
        finally {
            if ($this.FinallyPart) {
                $null = $this.FinallyPart.Eval()
            }
        }
        return $result
    }
}

################################################################
#
# Test that the 'this' pointer is set properly for lambdas bound in object literals.
#T ObjectLiteral_BoundLambdas1 o = {v:3.14 f:{this.v}}; m = o.f; m() == 3.14
#T ObjectLiteral_BoundLambdas2 o = {v:3.14 f:{this.v}}; m = o.f; m.TinyThis == o
#T ObjectLiteral_BoundLambdas3 {x -> x * 2}.TinyThis == null
#
# Represents the instantiation of an object literal/hashtable
#
class ObjectLiteral : LiteralBase
{
    [System.Collections.IDictionary]
        $Members

    ObjectLiteral ($token, $members) {
        $this.Token = $token
        $this.members = $members
    }

    Visit($invokable) {
        if ($this.Members) {
            foreach ($val in $this.Members.get_Values()) {
                if ($val -and $val -is [Expression]) {
                    $val.Visit($invokable)
                }
            }
        }
        $invokable.Invoke($this)
    }

    [object] Eval() {
        $obj = [ordered] @{}
        if ($null -ne $this.members) {
            foreach ($p in $this.members.GetEnumerator()) {
                $value = $p.Value.Eval()
                # Attach the 'this' pointer t new instances of lambdas defined in this object
                if ($value -is [TinyLambda]) {
                    $value = $value.Clone()
                    $value.TinyThis = $obj
                }
                elseif ($value -is [List[TinyLambda]]) {
                    # Handle multimethods
                    $newList = [List[TinyLambda]]::new()
                    foreach ($lambda in $value) {
                        $clone = $lambda.Clone()
                        $clone.TinyThis = $obj
                        $newList.Add($clone)
                    }
                    $value = $newlist
                }
                $obj[$p.Key] = $value
            }
        }
        return $obj
    }
}

################################################################
#
# Represents a method invocation
#
class MethodInvocation : Expression
{
    [Expression[]] $ArgumentExpressions

    [Expression] $methodNameExpr

    [string] $methodName

    [string] ToString() {
        return ".$($this.methodname)"
    }

    Visit($invokable) {
        if ($this.ArgumentExpressions) {
            foreach ($val in $this.ArgumentExpressions) {
                if ($val -and $val -is [Expression]) {
                    $val.Visit($invokable)
                }
            }
        }

        $this.methodNameExpr.Visit($invokable)
        $invokable.Invoke($this)
    }

    MethodInvocation ([Token] $token, [Expression] $methodNameExpr, $argumentExpressions) {
        $this.Token = $token
        $this.MethodNameExpr = $methodNameExpr
        if ($methodNameExpr -is [Literal]) {
            $this.methodName = $this.methodNameExpr.Value
        }
        elseif ($methodNameExpr -is [Variable]) {
            $this.MethodName = $this.methodNameExpr.Name
        }
        elseif ($methodNameExpr -is [Constant]) {
            $this.MethodName = $this.methodNameExpr.Token.Value
        }

        $this.ArgumentExpressions = $argumentExpressions
    }

    Eval() {
        errorMessage "MethodInvocation node: don't use the eval method, use the Invoke() method instead." $this.Token
    }

    [object] Invoke ($object, $extraArg, [bool] $useStatic) {

        if ($this.ArgumentExpressions) {
            $argsLength = $this.ArgumentExpressions.Length
        }
        else {
            $argsLength = 0
        }
        $argOffset = $exprOffset = 0

        if ($null -ne $extraArg) {
            $argsLength++
            $argOffset++
            $arguments = [object[]]::new($argsLength)
            $arguments[0] = $extraArg.Eval()
        }
        else {
            $arguments = [object[]]::new($argsLength)
        }

        # Evaluate the arguments
        while ($argOffset -lt $argsLength) {
            $arguments[$argOffset++] = $this.ArgumentExpressions[$exprOffset++].Eval()
        }

        # Figure out what the method name should be
        if (-not ($name = $this.MethodName)) {
            $name = $null

            $script:FunctionReturnValue = $null
            :return_from_function do {
                $name = $this.methodNameExpr.Eval()
            }
            while ($false)

            if ($null -eq $name) {
                $name = $script:FunctionReturnValue
            }

            if (-not $name) {
                errorMessage "A method name must not be null or empty" $this.Token
            }

            if ($name -isnot [string]) {
                errorMessage "Method names should be strings; not '$name' of type [<$($name.GetType())>]" $this.token
            }
        }

        $result = $null
        if ($useStatic) {
            if ($name -eq 'New') {
                return New-Object -TypeName $object -ArgumentList $arguments
            }

            $mi = $object::($name)
            # No static method so try instance method
            if ($null -eq $mi)
            {
                $mi = $object.($name)
            }
            if ($null -eq $mi) {
                errormessage "Static method '$($name)' does not exist" $this.Token
            }
            $result = $mi.invoke($arguments)
        }
        else {
            $mi = $object.($name)
            if ($null -eq $mi) {
                if ($object -is [TinyList]) {
                    # if the method doesn't exist on the wrapper, fall back to the wrapped list
                    $object = $object.list
                    $mi = $object.($name)
                }

                if ($null -eq $mi) {
                     errormessage "method '$($name)' does not exist" $this.Token
                }
            }

            try {
                $script:FunctionReturnValue = $null
                $result = [AutomationNull]::Value
                :return_from_function do {
                    if ($mi -is [ScriptBlock]) {
                        $result = $mi.InvokeReturnAsIs($arguments)
                    }
                    elseif ($mi -is [TinyLambda]) {
                        $result = $mi.Invoke($arguments, [AutomationNull]::Value, [AutomationNull]::Value, $null)
                    }
                    elseif ($mi -is [List[TinyLambda]]) {
                        # BUGBUGBUG - duplicated from class FunctionCall - need to refactor.
                        $lindex = 1
                        foreach ($lambda in $mi) {
                            $arity = $lambda.Parameters.Length;
                            $lindex++
          
                            # BUGBUGBUG need to handle parameterless functions
                            # Arity must match in function/method sets
                            if ($arity -ne $arguments.length) {
                                continue
                            }
          
                            $bindSuccess = $true
                            $arg1, $arg2, $rest = $arguments
                            if (-not $arg1) {
                                $arg1 = [AutomationNull]::Value
                            }

                            if (-not $arg2) {
                                $arg2 = [AutomationNull]::Value
                            }
                            $result = $lambda.Invoke($arguments, $arg1, $arg2, $null, [ref] $bindSuccess)
                            if ($bindSuccess -eq $false) {
                                continue;
                            }
                            else {
                                break
                            }
                        }
                    }
                    elseif ($mi -is [CommandInfo]) {
                        $pipelineinput = @()
                        if ($null -ne $ExtraArg) {
                            $pipelineinput = $arguments[0]
                            if ($pipelineinput -is [TinyList]) {
                                $pipeLineInput = @($pipeLineinput.list)
                            }
                        }
                        else {
                            $pipelineInput = @($pipelineinput)
                        }

                        if ($pipelineInput.Length -gt 0) {
                            $newargs = [object[]]::new($arguments.length-1)
                            # static void Copy(array srcArray, int srcIndex, array destArray, int destIndex, int length)
                            [array]::Copy($arguments, 1, $newargs, 0, $arguments.length-1)
                        }
                        else {
                            $newargs = $arguments
                        }

                        if ($newArgs.length -eq 1 -and $newArgs[0] -is [IDictionary]) {
                            $newArgs = $newargs[0]
                        }

                        if ($null -ne $newargs) {
                            if ($pipelineInput.Length -gt 0) {
                                $result =  $pipelineInput | & $mi @newargs
                            }
                            else {
                                $result =  & $mi @newargs
                            }
                        }
                        else {
                            if ($pipelineinput.Length -gt 0) {
                                $result = $pipelineinput | & $mi
                            }
                            else {
                                $result = & $mi
                            }
                        }

                        if ($result -is [IList]) {
                            $result = [TinyList]::New($result)
                        }
                    }
                    elseif ($mi -is [TinyLambda]) {
                        # Get the 'body' magic variable.
                        $bodyVal = $null
                        if ($null -ne $this.body) {
                            $bodyVal = if ($this.body -is [TinyLambda]) {
                                $this.body
                            }
                            else {
                                $this.Body.Eval()
                            }

                            $newArguments = [object[]]::new($arguments.Length+1)
                            $arguments.CopyTo($newArguments, 0)
                            $newArguments[-1] = $bodyVal
                            $arguments = $newArguments

                        }
                        $result = $mi.Invoke($arguments)
                    }
                    else {
                        # Other invokable type such as MethodInfos
                        $result = $mi.Invoke($arguments)
                    }
                }
                while ($false)

                if ($null -eq $result) {
                    $result = $script:FunctionReturnValue
                }
            }
            catch {
                $exception = $_.Exception
                while ($exception -is [MethodInvocationException]) {
                    $exception = $exception.GetBaseException()
                }

                if ($exception -is [TinyException]) {
                    if ($null -eq $exception.Token) {
                        $exception.Token = $this.Token
                    }
                    throw $exception
                }
                else {
                    errorMessage $exception $this.Token
                }
            }
        }
        return $result
    }
}

################################################################
#
# Implements a statement list
#
class StatementList : Expression
{
    [Expression[]]
        $Statements

    StatementList ($token, [Expression[]] $statements) {
        $this.token = $token
        $this.statements = $statements
    }

    Visit($invokable) {
        foreach ($val in $this.Statements) {
            if ($val -and $val -is [Expression]) {
                $val.Visit($invokable)
            }
        }
        $invokable.Invoke($this)
    }

    [object] Eval() {
        $result = $null
        foreach ($stmt in $this.statements) {
            $result = $stmt.Eval()
            if ($stmt -is [Assignment]) {
                $result = $null
            }
        }

        return $result
    }
}

###################################################################################
#
# Runtime data structures that hold system and user-defined variables
#
###################################################################################

#
# A list wrapper that primarily exists to get around PowerShell's
# automatic enumeration behavior. This is unfortunate as it results
# in a lot of additional complexity and performance overhead. On the
# other hand, it does make it simple to add "behavioural" methods like
# map() or distinct() in one place.
#
class TinyList
{
    [System.Collections.Generic.List[object]]
        $List

    [int]
        $Count

    TinyList() {
        $this.List = [System.Collections.Generic.List[object]]::new()
        $this.Count = 0
    }

    TinyList ([System.Collections.IEnumerable] $listToUse) {
        $this.List = [System.Collections.Generic.List[object]]::new()
        $this.List.AddRange(@($listToUse))
        $this.Count = $this.List.Count
    }

    TinyList ([System.Array] $listToUse) {
        $this.List = [System.Collections.Generic.List[object]]::new()
        $this.List.AddRange(@($listToUse))
        $this.Count = $this.List.Count
    }

    TinyList ([TinyList] $listToUse) {
        $this.List = [System.Collections.Generic.List[object]]::new()
        $this.List.AddRange($listToUse.list)
        $this.Count = $this.List.Count
    }

    TinyList ($object) {
        $this.list = [System.Collections.Generic.List[object]]::new()
        $this.list.Add($object)
        $this.Count = $this.List.Count
    }

    #T TinyList_Copy l = [1,2,3]; m = l.Copy(); l[1] = 20; l == [1, 20, 3] && m == [1, 2, 3]
    #L Make a shallow copy of this list i.e. the list is copied but not the items in the list. e.g. <pre> l = [1,2,3]</pre><pre>m = l.Copy()</pre><pre>l[1] = 20</pre><pre>l == [1, 20, 3] && m == [1, 2, 3]</pre>
    [TinyList] Copy() {
        return [TinyList]::New($this.List)
    }

    #T TinyList_Clear1 a = [1,2,3]; a.clear(); a.count == 0
    #L Clear this list e.g. <pre>l = [1,2,3]</pre><pre>l.clear()</pre><pre>l.count == 0</pre>
    [void] Clear() {
        $this.List.Clear()
        $this.Count = 0
    }

    #T TinyList_Add1 l = []; l.Add(3.14); l.count == 1 && l[0] == 3.14
    #T TinyList_Add2 l = []; l.Add(1); l.Add(2); l.Add(3); l == [1,2,3]
    #L Add an element to the list. (This API modifies it's list.) e.g. <pre>l = []</pre><pre>l.Add(1)</pre><pre>l.Add(2)</pre><pre>l == [1, 2]</pre>
    [void] Add ([object] $obj) {
        $this.list.Add($obj)
        $this.Count++
    }

    #T TinyList_AddRange1 l = []; l.AddRange([3,2,1].List); l.count == 3 && l == [3, 2, 1]
    #L Add a collection of items to the list. (Modifies the list.)
    [void] AddRange ([System.Collections.IEnumerable] $ilist) {
        if ($null -ne $ilist) {
            $this.list.AddRange(@($ilist))
            $this.Count = $this.List.Count
        }
    }

    #T TinyList_AddRange2 l = []; l.AddRange([3,2,1]); l.count == 3 && l == [3, 2, 1]
    [void] AddRange ([TinyList] $tinyList) {
        # Adding null is a no-op not and error.
        if ($null -ne $tinyList) {
            $this.list.AddRange($tinyList.List)
            $this.Count = $this.List.Count
        }
    }

    #T TinyList_GetRange1  [1..10].GetRange(3,4) == [4, 5, 6, 7]
    #T TinyList_GetRange2  [1..10].GetRange(5,10) == [6, 7, 8, 9, 10]
    #T TinyList_GetRange3  [1..10].GetRange(9,1) == [10]
    #T TinyList_GetRange4  [1..10].GetRange(10,10) == []
    #T TinyList_GetRange5  [1..10].GetRange(20,10) == []
    #L Get a subrange of a list; if more items are requested than available, the list will be truncated to what's avaiable
    [TinyList] GetRange([int] $start, [int] $length) {
        $start  = [math]::min($start, $this.list.Count)
        $length = [math]::min($length, $this.list.Count - $start)
        return [TinyList]::New($this.list.GetRange($start, $length))
    }

    #T TinyList_Slice1 [1..10].slice(5,-1)  == [6..10]
    #T TinyList_Slice2 [1..10].slice(-4,-1) == [7,8,9,10]
    #L Returns a slice of the list as an ArraySegment[object]. This method differes from GetRange() in that negative indexes count from the end of the list e.g. <pre>[1..10].slice(-4,-1) == [7,8,9,10]</pre>
#BUGBUGBUG - it returns a tiny list, not an array segment!
    [TinyList] Slice([int] $start, $end = -1) {
        $length = $this.list.Count
        if ($length -lt 1) {
            return [TinyList]::new()
        }

        if ($start -lt 0) {
            $start = $length+$start
        }

        if ($end -lt 0) {
            $length += $end-$start+1
        }
        else {
            $len = $end - $start + 1;
            if ($len -lt $length) {
                $length = $len
            }
        }

        return [TinyList]::New($this.list.GetRange($start, $length))
    }

    #T TinyList_InsertRange1 list = [1..10]; list.insertRange(5, null); list == [1..10]
    #T TinyList_InsertRange2 list = [1..10]; list.insertRange(5, []); list ==[1..10]
    #T TinyList_InsertRange3 list = [1..10]; list.insertrange(5, [10,20,30]); list == [1,2,3,4,5,10,20,30,6,7,8,9,10]
    #L Insert a list of itens at a specific point in an existing list
    InsertRange([int] $index, $listToInsert) {
        if ($null -eq $listToInsert) {
            return
        }

        if ($listToInsert -is [TinyList]) {
            $listToInsert = $listToInsert.list
        }

        if ($listToInsert.count -eq 0) {
            return
        }

        if ($listToInsert -isnot [IList]) {
            $listToInsert = @($listToInsert)
        }

        $index = [math]::min($index, $this.list.count)
        $this.list.InsertRange($index, $listToInsert)
        $this.Count = $this.List.Count
    }

    #T TinyList_Insert1 list = [1..10]; list.insert(5, null); list == [1..10]
    #T TinyList_Insert2 list = [1..10]; list.insert(5, []); list ==[1..5,[],6..10]
    #T TinyList_Insert3 list = [1..10]; list.insert(5, [10,20,30]); list == [1,2,3,4,5,[10,20,30],6,7,8,9,10]
    #L Insert an item at a specific point in an existing list
    Insert([int] $index, $objectToInsert) {
        if ($null -eq $objectToInsert) {
            return
        }

        # If it's a list, turn it into a tinylist for consistency sake.
        if ($objectToInsert -is [IList]) {
            $objectToInsert = [TinyList]::New($objectToInsert)
        }

        $index = [math]::min($index, $this.list.count)
        $this.list.Insert($index, $objectToInsert)
        $this.Count = $this.List.Count
    }

    #T TinyList_RemoveRange1 list = [1..10]; list.RemoveRange(2, 3); list == [1, 2, 6, 7, 8, 9, 10]
    #T TinyList_RemoveRange2 list = [1..10]; list.RemoveRange(2, 10) ; list == [1,2]
    #T TinyList_RemoveRange3 list = [1..10]; list.RemoveRange(2, 100) ; list == [1,2]
    #T TinyList_RemoveRange4 list = []; list.RemoveRange(2, 10); list == []
    #T TinyList_RemoveRange5 list = null; list?.RemoveRange(2, 10); list == null
    #T TinyList_RemoveRange6 list = [1..10]; list.RemoveRange(-2, 2) ; list == [1..8]
    #T TinyList_RemoveRange7 list = [1..10]; list.RemoveRange(5, 3) ; list == [1..5, 9, 10]
    #T TinyList_RemoveRange8 list = [1..10]; list.RemoveRange(-2, -3) ; list == [1..5, 9, 10]
    #T TinyList_RemoveRange9 a=[1,2,3,4]; a.RemoveRange(1,2); a == [1,4]
    #T TinyList_RemoveRange10 a=[1,2,3,4]; a.RemoveRange(-5,2); a == [1..4]
    #L Remove a range of items from a TinyList. Negative index & count are offset from the end of the list. This method mutates the object and returns nothing. e.g.<pre>a=[1,2,3,4]</pre><pre>a.RemoveRange(1,2)</pre><pre>a == [1,4]</pre><pre>a=[1,2,3,4]</pre><pre>a.RemoveRange(-1,2)</pre><pre>a == [1,2]</pre><pre></pre><pre>a=[1,2,3,4]</pre><pre>a.RemoveRange(-2,-2)</pre><pre>a == [3,4]</pre>
    RemoveRange([int] $index, [int] $numberToRemove) {
        if ($numberToRemove -eq 0 -or $this.Count -eq 0) {
            return
        }

        if ($index -lt 0) {
            $index = $this.List.Count + $index
            # BUGBUG - should probably check to see if index+numberToRemove overlaps with the start of the list and remove the overlap.
            if ($index -lt 0) {
                return
            }
        }

        if ($index -gt $this.List.Count) {
            return
        }

        if ($index -lt 0) {
            $index = 0
        }

        if ($numberToRemove -lt 0) {
            $index += $numberToRemove
            if ($index -lt 0) {
                $index = 0
            }
            $numberToRemove = [math]::abs($numberToRemove)
        }

        if ($index + $numberToremove -gt $this.Count) {
            $numberToRemove = $this.Count-$index
        }

        $this.list.RemoveRange($index, $numberToRemove)
        $this.Count = $this.list.count
    }

    #T TinyList_RemoveAt1 a = [1,2,3]; a.RemoveAt(1);  a == [1,3]
    #T TinyList_RemoveAt2 a = [1,2,3]; a.RemoveAt(2);  a == [1,2]
    #T TinyList_RemoveAt3 a = [1,2,3]; a.RemoveAt(-1); a == [1,2]
    #T TinyList_RemoveAt4 a = [1,2,3]; a.RemoveAt(0);  a == [2,3]
    #T TinyList_RemoveAt5 a = [1,2,3]; a.RemoveAt(-3); a == [2,3]
    #T TinyList_RemoveAt6 a = [1,2,3]; a.RemoveAt(10); a == [1,2,3]
    #T TinyList_RemoveAt7 a = [1,2,3]; a.RemoveAt(-10); a == [1,2,3]
    #T TinyList_RemoveAt8 list = null; list?.RemoveAt(2); list == null
    #L Remove a single item from a TinyList. This method mutates the object and returns nothing. Negative numbers are indexed from the end. e.g. <pre>a=[1,2,3]<pre></pre>a.RemoveAt(1)<pre></pre>a == [1,3]</pre><pre>a=[1,2,3,4]<pre><pre></pre></pre>a.RemoveAt(-2)<pre></pre>a == [1,2,4]</pre>
    RemoveAt([int] $index) {
        if ($this.Count -eq 0) {
            return
        }

        if ($index -lt 0) {
            $index = $this.List.Count + $index
            if ($index -lt 0) {
                return
            }
        }

        if ($index -gt $this.List.Count) {
            return
        }
        $this.List.RemoveAt($index)
        $this.Count = $this.list.count
    }

    #T TinyList_Item1 l = [1,2,3]; l.item(1) == 2
    #L Return the item at the specified index.
    [object] Item($index) { return $this.List[$index] }

    #T TinyList_Take1 [1,2,3,5].take(2) == [1,2]
    #L Take num items from the beginning of the list non-destructively. If more items are requested than the list contains, only return the available items.
    [TinyList] Take ([int] $num) {
        if ($num -gt $this.List.Count) {
            $num = $this.List.Count
        }
        return [TinyList]::New($this.list.GetRange(0, $num))
    }

    #T TinyList_TakeUntil1 [1,2,3,5].TakeUntil {n -> n == 3} == [1,2]
    #T TinyList_TakeUntil2 [1,2,3,5].TakeUntil {it == 3} == [1,2]
    #L Take items from the list until the specified lambda returns true. The current item is available in 'it' and the index is in 'it2'
    [TinyList] TakeUntil ([TinyLambda] $lambda) {
        $max = $this.list.Count - 1
        for ($i =0; $i -lt $max; $i++) {
            $current = $this.list[$i]
            if ($lambda.Dot(@($current, $i), $current, $i, $null)) {
                break
            }
        }
        return [TinyList]::new($this.List.GetRange(0, $i))
    }

    #T TinyList_Skip1 [1,2,3,4].skip(2) == [3, 4]
    #L Skip the first num items in a list. If the number to skip is greater than the number of items in the list; an empty list is returned.
    [TinyList] Skip ([int] $num) {
        if ($num -gt $this.List.Count) {
            return [TinyList]::new()
        }
        return [TinyList]::New($this.list.GetRange($num, $this.list.Count - $num))
    }

    #T TinyList_SkipNullOrEmpty [1, null, 2, "", 3].SkipNullOrEmpty() == [1, 2, 3]
    #L Skip all null or empty items in a list.
    [TinyList] SkipNullOrEmpty() {
        return [TinyList]::New(@($this.list.where{$null -ne $_ -and '' -ne $_}))
    }

    #T TinyList_SkipUntil1 [1,2,3,4].SkipUntil{n -> n > 2} == [3, 4]
    #T TinyList_SkipUntil2 [1,2,3,4].SkipUntil{it > 2} == [3, 4]
    #L Skip items in the list until the specified lambda returns true. The current item is available in 'it' and the index is in 'it2'
    [TinyList] SkipUntil ([TinyLambda] $lambda) {
        $max = $this.list.Count
        for ($i =0; $i -le $max; $i++) {
            $current = $this.list[$i]
            if ($lambda.Dot(@($current), $current, [AutomationNull]::Value, $null)) {
                break
            }
        }
        return [TinyList]::new($this.List.GetRange($i, $max-$i))
    }

    #T TinyList_Push1 l = [1,2,3]; l.push(7); l.count == 4 && l[0] == 7
    #L Push an item at the front of a list. (Modifies the list.)
    Push ([object] $obj) {
        $this.List.Insert(0, $obj)
        $this.count++
    }

    #T TinyList_Pop1 l = [1,2,3]; x = l.Pop(); x == 1 && l.Count == 2
    #L Destructively pop an item from the beginning of a list. (Modifies the list.)
    [object] Pop() {
        if ($this.list.count -gt 0) {
            $result = $this.list[0]
            $this.list.removeat(0)
            $this.count--
            return $result
        }
        else {
            return $null
        }
    }

    #T TinyList_Head1 l = [1,2,3]; x = l.Head(); x == 1 && l.Count == 3
    #L Nondestructively retrieve the first item from a list
    [object] Head() {
        if ($this.list.count -gt 0) { return $this.list[0] } else { return $null }
    }

    #T TinyList_Tail1 l = [1,2,3]; t = l.Tail(); l == [1, 2, 3] && t == [2, 3]
    #T TinyList_Tail2 l = [1,2,3]; t = l.Tail(); t.Count == 2
    #L Non-destructively retrieve all but the first element from a list.
    [TinyList] Tail() {
        if ($this.list.count -eq 0) {
            return [TinyList]::new()
        }
        else {
            $lst = [TinyList]::new()
            $lst.list = $this.List.GetRange(1, $this.List.Count-1)
            $lst.count = $lst.list.count
            return $lst
        }
    }

    #T TinyList_First1 l = [1,2,3]; x = l.first(); x == 1 && l.Count == 3
    #L Get the first element from a list. (Same as Head().)
    [object] First() {
        if ($this.list.count -gt 0) { return $this.list[0] } else { return $null }
    }

    #T TinyList_First2 l = [1,2,3]; x = l.first(2); x == [1, 2]
    #T TinyList_First3 l = [1,2,3]; x = l.first(10); x == [1, 2, 3]
    #T TinyList_First4 l = [1,2,3]; x = first(2); l == [1,2,3] # Doesn't mutate the original list.
    #L Get the first n elements from a list.
    [object] First([int] $n) {
        if ($this.list.count -lt $n) {
            return [TinyList]::New($this.List)
        }
        else {
            return [TinyList]::new($this.List.GetRange(0, $n))
        }
    }

    #T TinyList_FirstLambda1 [1..10].first{it % 4 == 0} == 4
    #T TinyList_FirstLambda2 [1..10].first{it % 13 == 0} == null
    #T TinyList_FirstLambda3 ['a'..'z'].first{it2 == 6} == 'g' # index is available in it2
    #L Find the first element in a last where the lambda evaluates to true.
    [object] First([TinyLambda] $func) {
        $listToProcess = $this.list
        $index = 0
        foreach ($obj in $listToProcess) {
            if ($func.Dot(@($obj, $index), $obj, $index, $null)) {
                return $obj
            }
            $index++
        }
        return $null
    }

    #T TinyList_Last1 l = [4, 5, 6]; l.last() == l[-1]
    #L Return the last element in a list.
    [object] Last() {
        if ($this.list.count -gt 0) { return $this.list[-1] } else { return $null }
    }

    #T TinyList_Last2 l = [4, 5, 6]; l.last(2) == [5, 6]
    #T TinyList_Last3 l = [4, 5, 6]; l.last(5) == [4, 5, 6] # tests requests for > max items
    #T TinyList_Last4 l = [4, 5, 6]; x = l.last(2); l == [4, 5, 6] # doesn't modifiy the original
    #L Return the last N elements in a list
    [object] Last([int] $n) {
        if ($this.List.Count -lt $n) {
            return [TinyList]::New(@($this.List))
        }
        else {
            $len = $this.List.Count
            return [TinyList]::New($this.List.GetRange($len-$n, $n))
        }
    }

    #T TinyList_SetItem1 l = [4, 5, 6]; l.SetItem(1, 50); l == [4, 50, 6]
    # Set the item at the specified index to the provided value.
    SetItem($index, $value) {
        if ($index -gt $this.list.Count) {
           errorMessage "Set: index $index is beyond the array bounds $($this.list.count)"
        }
        $this.List[$index] = $value
    }

    # Add an item or items to this list; tested in the '+' operator tests
    # BUGBUGBUG Does not modify the current list.
    static [TinyList] op_Addition ([TinyList] $list, $value) {
        $result = [TinyList]::new($list.list)
        if ($null -ne $value) {
            if ($value -is [TinyList]) {
                $result.AddRange($value.List)
            }
            else {
                $result.Add($value)
            }
        }
        return $result
    }

    #T TinyList_Subtract1 l = [1,2,3,4]; l = l - 2; l == [1,3,4]
    #T TinyList_Subtract2 l = [1,2,1,2]; l = l - 2; l == [1,1]
    #T TinyList_Subtract3 l = [1,2,3,4,1,2,3]; l = l - [2, 3]; l == [1, 4, 1]
    #T TinyList_Subtract4 l = [1,2,3,4]; l = l - [5, 6]; l == [1,2,3,4]
    #T TinyList_Subtract5 l = []; l = l - [5, 6]; l == []
    #T TinyList_Subtract6 l = [1,2,3,4]; l = l - [4, 1, 2, 3]; l == []
    # Subtraction removes the item or items specified on the RHS
    # This API doesn't modify the original list but rather returns
    # a new one.
    static [TinyList] op_Subtraction ([TinyList] $list, $value) {
        $nl = [TinyList]::New()
        $nl.list.addrange($list.list)
        if ($value -is [TinyList]) { $value = $value.list }
        foreach ($v in $value) {
            # Remove all instances of the object from the list
            while ($nl.list.Remove($v)) { }
        }
        return $nl
    }

    #T TinyList_Multiply1 [1] * 3 == [1, 1, 1]
    #T TinyList_Multiply2  [] * 3 == []
    # Multiply repeats the list 'value' times
    static [TinyList] op_Multiply ([TinyList] $list, $value) {
        $nl = $null
        if ($value -is [TinyList]) {
            $nl = foreach ($x in $list.list) {
                foreach ($y in $value.list) {
                    $x * $y
                }
            }
        }
        elseif ($null -ne ($ival = $value -as [int])) {
            $nl = $list.list * $ival
        }
        else {
            errorMessage "multiply error: only [<int>] and [<TinyList>] values can be on the RHS of the '*' operator, values of type [<$($value.GetType())>] are not allowed."
        }
        return [TinyList]::new($nl)
    }

    #T TinyList_Division1 [1..6] / 2 == [[1,2], [3,4], [5,6]]
    #T TinyList_Division2 [] / 1 == []
    # Divide the list into RHS sized chunks e.g. <pre>[1 .. 10] / 3 == [[1, 2, 3], [4, 5, 6], [7, 8, 9], 10]</pre>
    static [TinyList] op_Division ([TinyList] $list, [int] $value) {
        return [TinyList]::Partition($list, $value)
    }

    #T TinyList_Partition1 [<TinyList>].Partition([1..6], 2) == [[1,2], [3,4], [5,6]]
    #T TinyList_Partition2 [<TinyList>].Partition([], 1) == []
    #T TinyList_Partition3 [<TinyList>].Partition(null, 1) == []
    #L Static routine to partition a list into sublists used by the split method e.g. <pre>[TinyList]::Partition([1..10], 3)</pre>
    static [TinyList] Partition ([TinyList] $list, [int] $size) {
        if ($null -eq $list) {
            return [TinyList]::new()
        }
        return [TinyList]::Partition($list.list, $size)
    }

    static [TinyList] Partition ([IList] $list, [int] $size) {
        if ($null -eq $list) {
            return [TinyList[]]::new()
        }

        if ($list.Count -eq 0) {
            return $list
        }

        $result = [TinyList]::New()
        for ($offset = 0; $offset+$size -lt $list.Count; $offset += $size) {
            $temp = $list.GetRange($offset, $size)
            $result.Add([TinyList]::new($temp))
        }

        $temp = $list.GetRange($offset, $list.count-$offset)
        $result.Add([TinyList]::new($temp))
        return $result
    }

    #T TinyList_Sum1 [1..10].Sum() == 55
    #T TinyList_Sum2 ['a', 'b', 'c'].sum() == 'abc'
    #L Sum all of the items in the list e.g. <pre>[1 .. 10].sum()</pre> or <pre>['a', 'b', 'c'].sum() </pre>
    [object] Sum() { return $this.Sum($null) }

    #T TinyList_Sum3 [1..10].Sum{n -> n * 2} == 110
    #T TinyList_Sum4 [1,2,3].sum{it + it2} == 9
#BUGBUGBUG This works on OrderedDictionry (i.e. {a:1}) by "accident" as they can be indexed with integers. Need to think about this. 
    #L Sum the result of evaluating a lambda on all of the items in the list. The item is passed as 'it' and the index is passed as 'it2'  e.g. <pre>shell('gps').sum{it.WorkingSet}</pre>
    [object] Sum ([TinyLambda] $lambda) {
        $lst = $this.list
        $cnt = $lst.Count
        if ($cnt -eq 0) { return $null }
        $current = $lst[0]
        if ($lambda) {
            $current = $lambda.Dot(@($current), $current, 0, $null)
        }

        if ($cnt -eq 1) { return $current }
        for ($i=1; $i -lt $cnt; $i++) {
            $val = $lst[$i]
            # Pass the value as it and the index as it2.
            if ($lambda) { $current += $lambda.Dot(@($val, $i), $val, $i, $null) } else { $current += $val }
        }

        return $current
    }

    #T TinyList_Product1 [1..10].Product() == 3628800
    #L Multiply all of the items in the list e.g. <pre>[1 .. 10].product()</pre>
    [object] Product() { return $this.Product($null) }

    #T TinyList_Product2 [{a:1}, {a:2}, {a:3}, {a:4}].product{it.a} == 24
    #L Multiply the result of evaluating a lambda on all of the items in the list. The item is passed as 'it' and the index is passed as 'it2'  e.g. <pre>shell('gps').product{it.WorkingSet}</pre>
    [object] Product([TinyLambda] $lambda) {
        $lst = $this.list
        $cnt = $lst.Count
        if ($cnt -eq 0) { return $null }
        $first = $lst[0]
        if ($lambda) {
            $first = $lambda.Dot(@($first), $first, 0, $null)
        }

        if ($cnt -eq 1) { return $first }
        for ($i=1; $i -lt $cnt; $i++) {
            $val = $lst[$i]
            # Pass the value as 'it' and the index as it2.
            if ($lambda) { $first *= $lambda.Dot(@($val, $i), $val, $i, $null) } else { $first *= $val }
        }

        return $first
    }

    #T TinyList_All1 [1,2,3,4].All()
    #T TinyList_All2 [0, 1, 2, 3].All() == false
    #L Returns true if all of the elements in the list are true e.g. <pre>[1,2,3].All() == true</pre>
    [bool] All() {
        return $this.All($null)
    }

    #T TinyList_All3 [2,4,6].All{it % 2 == 0}
    #T TinyList_All4 [3,4,6].All{it % 2 == 0} == false
    #L Returns true if the result of evaluating a lambda on all of the items in the list returns true. The item is passed as 'it' and the index is passed as 'it2'  e.g. <pre>[1,2,3].all{it > 0}</pre>
    [bool] All([TinyLambda] $lambda) {
        $lst = $this.list
        $cnt = $lst.Count
        if ($cnt -eq 0) { return $false }
        for ($i=0; $i -lt $cnt; $i++) {
            $val = $lst[$i]
            # Pass the value as it and the index as it2.
            if ($lambda) {
                if (-not $lambda.Dot(@($val), $val, $i, $null)) {
                    return $false
                }
            }
            else {
                if (-not $val) {
                    return $false
                }
            }
        }

        return $true
    }

    #T TinyList_Any1 [0, 0, 1, 0].Any()
    #T TinyList_Any2 [0, 0, 0, 0].Any() == false
    #L Returns true if any of the elements in the list are true
    [bool] Any () {
        return $this.Any($null)
    }

    #T TinyList_Any3 [5, 6 , 1, 7].Any{it % 2 == 0}
    #T TinyList_Any4 [5, 3 , 1, 7].Any{it % 2 == 0} == false
    #T TinyList_Any5 [].Any{it % 2 == 0} == false
    #L Returns true if the result of evaluating a lambda on any of the items in the list returns true. The item is passed as 'it' and the index is passed as 'it2'  e.g. <pre>[1,2,3].all{it > 0}</pre>
    [bool] Any ([TinyLambda] $lambda) {
        $lst = $this.list
        $cnt = $lst.Count
        if ($cnt -eq 0) { return $false }
        for ($i=0; $i -lt $cnt; $i++) {
            $val = $lst[$i]
            # Pass the value as it and the index as it2.
            if ($lambda) {
                if ([bool] $lambda.Dot(@($val), $val, $i, $null)) {
                    return $true
                }
            }
            else {
                if ([bool] $val) {
                    return $true
                }
            }
        }

        return $false
    }

    #T TinyList_ToString1 [1, 2, 3].ToString() == '[1, 2, 3]'
    #T TinyList_ToString2 [].ToString() == '[]'
    #L Convert the list into a string e.g. <pre>[1, 2, 3].ToString() == '[1, 2, 3]'</pre>
    [string] ToString () { return  "[" + ($this.List -join ', ') + "]" }

    #T TinyList_ToHash1 h = [1, 1, 1, 2, 3].ToHash(); h is [<IDictionary>] && h[1] == 3 && keys(h).Count == 3
    #L Convert a list into a hashtable. The list element is added as a key with the value being a count of the number of times the element appears in the list.
    [Hashtable] ToHash() {
        [Hashtable] $result = @{}
        foreach ($item in $this.list) {
            $result[$item] += 1
        }

        return $result
    }

    #T TinyList_ToHashProperty1 ['abc', 'def', 'ghij', 'kl'].ToHash('length').Count == 3
    #T TinyList_ToHashProperty2 ['abc', 'def', 'ghij', 'kl'].ToHash('length')[3] == ['abc', 'def']
    #T TinyList_ToHashProperty3 ['abc', 'def', 'ghij', 'kl'].ToHash('length')[2] == ['kl']
    #L Convert a list into a hashtable keyed on the named property. The item value is stored in array with the associated key.
    [HashTable] ToHash([string] $property) {
        [HashTable] $result = @{}
        foreach ($item in $this.list) {
            $key = $item.$Property
            if (-not $result.ContainsKey($key)) {
                $result[$key] = [TinyList]::new(@( $item ))
            }
            else {
                $result[$key].Add($item)
            }
        }
        return $result
    }

    #T TinyList_ToHashProperty4 ['abc', 'def', 'ghij', 'kl'].ToHash { it.length }.Count == 3
    #T TinyList_ToHashProperty5 ['abc', 'def', 'ghij', 'kl'].ToHash { n -> n.length } [3] == ['abc', 'def']
    #T TinyList_ToHashProperty6 ['abc', 'def', 'ghij', 'kl'].ToHash { it.length }[2] == ['kl']
    #L Convert a list into a hashtable keyed by the value of returned from the lambda. The item value is stored in an array with it's associated key.
    [HashTable] ToHash([TinyLambda] $lambda) {
        [HashTable] $result = @{}
        foreach ($item in $this.list) {
            $key = $lambda.Invoke(@($item),$item, [AutomationNull]::Value, $null)
            if (-not $result.ContainsKey($key)) {
                $result[$key] = [TinyList]::new(@( $item ))
            }
            else {
                $result[$key].Add($item)
            }
        }
        return $result
    }

    #T TinyList_Reduce1 [1..10].reduce{x, y -> x+y} == 55
    #T TinyList_Reduce2 ['a', 'b', 'c'].reduce{x, y -> x+y} == "abc"
    #T TinyList_Reduce3 ['a', 'b', 'c'].reduce{x, y -> y} == "c"
    #T TinyList_Reduce4 [1..10].reduce{it+it2} == 55
    #T TinyList_Reduce5 ['a', 'b', 'c'].reduce{it+it2} == "abc"
    #T TinyList_Reduce6 ['a', 'b', 'c'].reduce{it2} == "c"
    #L Iteratively apply a lambda to each element in the list e.g. to sum the list do <pre>[1 .. 10].reduce{ it + it2 }<pre>
    [object] Reduce ([TinyLambda] $lambda) {
        if (-not $lambda) { errorMessage "Reduce: lambda argument was null" }
        $lst = $this.list
        $result = $lst[0]
        :inner for ($i=1; $i -lt $lst.Count; $i++) {
            $result = $lambda.Dot(@($result, $lst[$i]), $result, $lst[$i], $null)
        }
        return $result
    }

    #T TinyList_ReduceWithSeed1 [1..10].ReduceWithSeed(0, {x, y -> x+y}) == 55
    #T TinyList_ReduceWithSeed2 [1..10].ReduceWithSeed(0, {it+it2}) == 55
    #T TinyList_ReduceWithSeed3 [1..10].ReduceWithSeed(1, {it*it2}) == 3628800
    #BUGBUG orderdictionary doesn't allow value types as keys(?!)
    #T TinyList_ReduceWithSeed4 keys ( [1..10].ReduceWithSeed({}, {h, k -> h['' + k] += 1; h}) ) == [1 .. 10]
    #L Iteratively apply a lambda to each element in the list starting with an initial seed e.g. add all items to a has table do <pre>[1 .. 10].reducewithseed( {}, {it[it2] += 1 }<pre>
    [object] ReduceWithSeed ([object] $Seed,  [TinyLambda] $lambda) {
        if (-not $lambda) { errorMessage "ReduceWithSeed: lambda argument was null" }
        $lst = $this.list
        $result = $seed
        :inner foreach ($val in $lst) {
            $result = $lambda.Dot(@($result, $val), $result, $val, $null)
        }
        return $result
    }

    #T TinyList_Map1 [1 .. 5].map {n -> n * 2} == [2, 4, 6, 8, 10]
    #T TinyList_Map2 [1 .. 5].map {it * 2} == [2, 4, 6, 8, 10]
    #T TinyList_Map3 [].map{it} == []
    #T TinyList_Map4 fn sqr n -> n * n; [1..10].map(sqr) == [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
    #T TinyList_Map5 ['a', 'b', 'c'].map {[it, it2]} == [['a', 0], ['b', 1], ['c', 2]]
    #L Apply a lambda to each item in the list, returning a new list e.g. <pre>[1,2,3,4,5].map{n -> n * n}</pre><pre>[1 .. 10].map{it * it}</pre><pre>fn sqr n -> n * n; [1..10].map(sqr)</pre>
    [TinyList] Map ($lambda) {
        if ($null -eq $lambda) {
            errorMessage "Map: lambda argument was null."
        }

        $result = [TinyList]::new()
        $index = 0
        $isLambda = $lambda -is [TinyLambda]
        # The label is required for break and continue
        :inner foreach ($item in $this.List) {
            if ($isLambda) {
                $val = $lambda.Dot(@($item, $index), $item, $index, $null)
            }
            else {
                $val = $lambda.Invoke($item)
            }

            $index++
            if ($null -ne $val) {
               $result.Add($val)
            }
        }
        return $result
    }

    #T TinyList_FlatMap1 [1,2,3,4,5].flatmap{n -> [n, n]} == [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
    #T TinyList_FlatMap2 [1,2,3,4,5].flatmap{[it, it]} == [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
    #T TinyList_FlatMap3 [].flatmap{it} == []
    #T TinyList_FlatMap4 fn pair n -> [n, n];  [1,2,3,4,5].flatmap(pair) == [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
    #T TinyList_FlatMap5 ['a', 'b', 'c'].flatmap {[it, it2]} == ['a', 0, 'b', 1, 'c', 2]
    #L Apply a lambda to each item in the list, returning a new flattened list e.g. <pre>[1,2,3,4,5].flatmap{[it, it]}</pre>
    [TinyList] FlatMap ([TinyLambda] $lambda) {
        if ($null -eq $lambda) { errorMessage "flatmap: lambda argument was null" }
        $result = [TinyList]::new()
        $index = 0
        # The label is required for break and continue
        :inner foreach ($item in $this.List) {
            $val = $lambda.Dot(@($item, $index), $item, $index, $null)
            $index++
            if ($null -ne $val) {
                if ($val -is [TinyList]) {
                    $result.list.AddRange($val.List)
                }
                elseif ($val -is [IEnumerable[object]]) {
                    $result.list.AddRange($val)
                }
                elseif ($val -is [IEnumerable]) {
                    $result.list.AddRange(@($val))
                }
                else {
                    $result.add($val)
                }
            }
        }
        return $result
    }

    #T TinyList_Flatten1 [].flatten() == []
    #T TinyList_Flatten2 [[1, 2], [3, 4]].flatten() == [1 .. 4]
    #T TinyList_Flatten3 [[1, [2]], [3, 4]].flatten() == [1 .. 4]
    #T TinyList_Flatten4 [[1, [[[[2]]]]], [[[3]], 4]].flatten() == [1 .. 4]
    #L Flatten a list so any nested lists are merged into the parent e.g. <pre>[[1,2], [2,3]].flatten() == [1,2,3,4]</pre>
    [TinyList] Flatten() {
        $result = [TinyList]::new()
        foreach ($item in $this.list) {
            if ($null -eq $item) {
                continue
            }
            elseif ($item -is [TinyList]) {
                $result.list.AddRange($item.List)
            }
            elseif ($item -is [IEnumerable]) {
                $result.list.AddRange(@($item))
            }
            else {
                $result.add($item)
            }
        }
        $result.count = $result.list.count
        return $result
    }

    #T TinyList_FindIndex1 [1,3,5].FindIndex{it == 1} == 0
    #T TinyList_FindIndex2 [1,3,5].FindIndex{it == 3} == 1
    #T TinyList_FindIndex3 [1,3,5].FindIndex{it == 5} == 2
    #T TinyList_FindIndex4 [1,3,5].FindIndex{it == 10} == -1
    #T TinyList_FindIndex5 [1,3,5].FindIndex{it == -3} == -1
    #L Find the index of the item matched by the lambda e.g. <pre>[1,3,5].FindIndex{it == 3} == 1</pre>
    [int] FindIndex ([TinyLambda] $lambda) {
        if ($null -eq $lambda) { errorMessage "FindIndex: lambda argument was null" }
        for ($i=0; $i -lt $this.list.count; $i++) {
            $item = $this.list[$i]
            if ($lambda.Dot(@($item), $item, $i, $null))
            {
                return $i
            }
        }
        return -1
    }

    #T TinyList_Find1 [1,3,5].FindIndex{it == 3} == 1
    #T TinyList_Find2 ['a', 'b', 'c'].FindIndex{it == 'b'} == 1
    #T TinyList_Find3 [1, 3, 5].FindIndex{it == 10} == -1
    #T TinyList_Find4 ['a', 'b', 'c'].FindIndex{it == 'd'} == -1
    #T TinyList_Find5 fn isc c -> c == 'c'; ['a', 'b', 'c'].FindIndex(isc) == 2
    #T TinyList_Find6 isc = {c -> c == 'c'}; ['a', 'b', 'c'].FindIndex(isc) == 2
    #L Find the item matched by the lambda e.g. <pre>[1, 3, 5].FindIndex{it == 3} == 1</pre>
    [object] Find ([TinyLambda] $lambda) {
        if ($null -eq $lambda) { errorMessage "Find: lambda argument was null" }
        for ($i=0; $i -lt $this.list.count; $i++) {
            $item = $this.list[$i]
            if ($lambda.Dot(@($item), $item, $i, $null))
            {
                return $item
            }
        }
        return $null
    }

    #T TinyList_FindLastIndex1 [1, 3, 3, 5].FindLastIndex{it == 3} == 2
    #T TinyList_FindLastIndex2 [1, 3, 3, 5].FindLastIndex{it == 10} == -1
    #T TinyList_FindLastIndex3 ['a', 'b', 'b', 'c'].FindLastIndex{it == 'b'} == 2
    #T TinyList_FindLastIndex4 ['a', 'b', 'b', 'c'].FindLastIndex{it == 'd'} == -1
    #L Find the index of the last item matched by the lambda e.g. <pre>[1, 3, 3, 5].FindLastIndex{it == 3} == 2</pre>
    [int] FindLastIndex ([TinyLambda] $lambda) {
        if ($null -eq $lambda) { errorMessage "FindLastIndex: lambda argument was null" }
        for ($i=$this.list.count - 1; $i -ge 0; $i--) {
            $item = $this.list[$i]
            if ($lambda.Dot(@($item), $item, $i, $null))
            {
                return $i
            }
        }
        return -1
    }

    #T TinyList_FindLast1 [1, 2, 3, 4, 5].FindLast{it % 2 == 0} == 4
    #T TinyList_FindLast2 [1, 3, 5].FindLast{it % 2 == 0} == null
    #L Find the last item matched by the lambda e.g. <pre>[1, 2, 3, 4, 5].FindLastIndex{it % 2 == 0} == 3</pre>
    [object] FindLast ([TinyLambda] $lambda) {
        if ($null -eq $lambda) { errorMessage "FindLast: lambda argument was null" }
        for ($i=$this.list.count - 1; $i -ge 0; $i--) {
            $item = $this.list[$i]
            if ($lambda.Dot(@($item), $item, $i, $null))
            {
                return $item
            }
        }
        return $null
    }

    #T TinyList_Contains1 [1..100].contains(40)
    #T TinyList_Contains2 [1..100].contains(400) == false
    #L Returns true if the list contains the specified object.
    [bool] Contains([object] $object) {
        return [Linq.Enumerable]::Contains($this.List, $object)
    }

    #T TinyList_MatchAll1 ['a', 'b', 'c', 'd'].MatchAll('[ac]') == ['a', 'c']
    #T TinyList_MatchAll2 ['a', 'b', 'c', 'd'].MatchAll(r/[ac]/) == ['a', 'c']
    #T TinyList_MatchAll3 ['a', 'b', 'c', 'd'].MatchAll(r/g/) == []
    #L Return all items matching a regular expression e.g. <pre>ls().match('\.tiny$')</pre>
   [TinyList] MatchAll ($pattern) {
        return $this.MatchAll($pattern, $null)
    }

    #T TinyList_MatchAll4 ['a', 'bb', 'ccc', 'dddd'].MatchAll(r/[ac]/, {it.length}) == [1, 3]
    #T TinyList_MatchAll5 ['a', 'bb', 'ccc', 'dddd'].MatchAll('[ac]', {it.length}) == [1, 3]
    #T TinyList_MatchAll6 ['a12b', 'c3d'].MatchAll(r/([0-9]+)/, {matches[1]}) == [12, 3]
    #BUGBUG - why is this failing?
    # T TinyList_MatchAll7 fn m1 -> matches[1]; ['a12b', 'c3d'].MatchAll(m1) == [12, 3]
    #T TinyList_MatchAll8 m1 = {matches[1]}; ['a12b', 'c3d'].MatchAll(r/([0-9]+)/, m1) == [12, 3]
    #T TinyList_MatchAll9 m1 = {matches[1]}; ['a12b', 'c3d'].MatchAll('([0-9]+)', m1) == [12, 3]
    #L Return all items matching a regular expression and apply a lambda to the result e.g. <pre>ls().match('\.tiny$', {it.Length}).Sum() </pre>
    [TinyList] MatchAll ($pattern, [TinyLambda] $lambda = $null) {
        if (-not $pattern) {errorMessage "Match: pattern argument is null"}
        if ($pattern -isnot [RegexLiteral]) {
            $pattern = [RegexLiteral]::new($null, "matches", $pattern)
        }
        $result = [tinylist]::new()
        $rlist = $result.List
        if ($null -ne $lambda) {
            :inner foreach ($s in $this.list) {
                if ($pattern.Match($s)) {
                    $rlist.Add($lambda.Dot(@($s), $s, [AutomationNull]::Value, $null))
                }
            }
        }
        else
        {
            :inner foreach ($s in $this.List) {
                if ($pattern.Match($s)) {
                    $rlist.Add($s);
                }
            }
        }
        $result.Count = $rlist.Count
        return $result
    }

    #T TinyList_NotMatch1 ['a', 'b', 'c', 'd'].NotMatch('[ac]') == ['b', 'd']
    #T TinyList_NotMatch2 ['a', 'b', 'c', 'd'].NotMatch(r/[ac]/) == ['b', 'd']
    #T TinyList_NotMatch3 ['a', 'b', 'c', 'd'].NotMatch(r/[xyz]/) == ['a', 'b', 'c', 'd']
    #L Return all items NOT matching a regular expression e.g. <pre>ls().notmatch('\.tiny$')</pre>
    [TinyList] NotMatch ([string] $pattern) { return $this.notmatch($pattern, $null) }

    #T TinyList_NotMatch4 ['a', 'bb', 'ccc', 'dddd'].NotMatch('[ac]', {it.length}) == [2, 4]
    #T TinyList_NotMatch5 ['a', 'bb', 'ccc', 'dddd'].NotMatch(r/[ac]/, {it.length}) == [2, 4]
    #L Return all items NOT matching a regular expression and apply a lambda to the result e.g. <pre>ls().notmatch('\.tiny$', {it.Length}).Sum() </pre>
    [TinyList] NotMatch ([string] $pattern, [TinyLambda] $lambda = $null) {
        if (-not $pattern) {errorMessage "NotMatch: pattern argument is null"}
        $result = [tinylist]::new()
        if ($lambda) {
            :inner foreach ($s in @($this.list -notmatch $pattern)) {
                # since we're triggering the lambda on not match, we don't set matches
                $result.Add($lambda.Dot(@($s), $s, [AutomationNull]::Value, $null))
            }
        }
        else
        {
            $result.list.AddRange(@($this.list -notmatch $pattern))
        }
        return $result
    }

    #T TinyList_Equals0 [].Equals([])
    #T TinyList_Equals1 [1,2,3].Equals([1,2,3])
    #T TinyList_Equals2 [1,2,3].Equals([1,2,4]) == false
    #T TinyList_Equals3 [1,2,3].Equals([1,2,3,4]) == false
    #T TinyList_Equals5 [1,2,3].Equals([1,2]) == false
    #L Compare this list against another collection; this methods is used by == to compare lists e.g. <pre> [1,2,3].Equals([1, 2, 3]) </pre>
    [bool] Equals ($listToCompare) {
        if ($listToCompare -is [TinyList]) {
            $cnt = $this.list.count
            if ($cnt -ne $listToCompare.list.count) {
                return $false
            }
            for ($i=0; $i -lt $cnt; $i++) {
                if ($this.list[$i] -ne $listToCompare.list[$i]) {
                    return $false
                }
            }
            return $true
        }
        else {
            try {
#BUGBUGBUG THis should return false but it returns true: [[1, 2], [3, 4], [5]] == [[1, 2], [3, 4], 5]
                return [linq.enumerable]::SequenceEqual(@($this.list), @($listToCompare))
            }
            catch {
                return $false
            }
        }
    }

    #T TinyList_JoinLists1 [1,2,3,4].joinlists([1,2,3,4], {it}, {it}, {[it,it2]}) == [[1,1], [2, 2], [3, 3], [4, 4]]
    #T TinyList_JoinLists2 [1,2,3,4].joinlists([1,2,3,4], {it}, {it}, {it + it2}) == [2, 4, 6, 8]
    #L Join two lists together using three lambdas - the outer key selector, the inner key selector and the result selector
    [TinyList] JoinLists($innerList,
                        [TinyLambda] $outerKeySelector,
                        [TinyLambda] $innerKeySelector,
                        [TinyLambda] $resultSelector) {

        if ($innerList -is [TinyList]) {
            $innerList = $innerList.List
        }
        elseif ($innerList -isnot [IEnumerable[object]]) {
            errorMessage "The innerList parameter must be of type [IEnumerable[object]]";
        }

        [Func[object,object]] $outerKeySelectorSb       = {param ($oobj) $outerKeySelector.Invoke(@($oobj), $oobj)}
        [Func[object,object]] $innerKeySelectorSb       = {param ($iobj) $innerKeySelector.Invoke(@($iobj), $iobj)}
        [Func[object,object,object]] $resultSelectorSb  = {param ($robj, $robj1) $resultSelector.Invoke(@($robj, $robj1), $robj, $robj1)}

        return [TinyList]::New(@([Linq.Enumerable]::Join(
            [IEnumerable[object]] $this.List,
            [IEnumerable[object]] $innerList,
            $outerKeySelectorSb, $innerKeySelectorSb, $resultSelectorSb)))
    }

    #T TinyList_Join1 [1, 2, 3].join() == '123'
    #T TinyList_Join2 [].join() == ''
    #T TinyList_Join3 ['a', 'b', 'c'].join() == 'abc'
    #L Join the members of a list into a string without a separator e.g. <pre>'abcde'.tochararray().aslist().reverse().join()</pre>
    [string] Join() { return $this.list -join '' }

    #T TinyList_Join4 [1, 2, 3].join('+') == '1+2+3'
    #T TinyList_Join5 [].join('+') == ''
    #L Join the members of a list into a string with an optional separator e.g. <pre> eval([1 .. 10].Join('+')) == 55</pre>
    [string] Join([string] $sep) { return $this.list -join $sep }

    #T TinyList_Replace1 ['abc', 'cba'].replace('a') == ['bc', 'cb']
    #T TinyList_Replace2 ['abc', 'cba'].replace(r/a/) == ['bc', 'cb']
    #T TinyList_Replace3 [].Replace('x') == []
    [TinyList] Replace($pattern) {
        return [TinyList]::new($this.list -replace $pattern)
    }

    #T TinyList_Replace4 ['abc', 'cba'].replace('a', 'X') == ['Xbc', 'cbX']
    #T TinyList_Replace5 ['abc', 'cba'].replace(r/a/, 'X') == ['Xbc', 'cbX']
    #T TinyList_Replace6 [].Replace('x', 'y') == []
    #L Convert all lists items into streams and then do a regex replace with an optional replacement e.g. <pre> ls('*.tiny').replace('^.*[\\/]', '').print() </pre>
    [TinyList] Replace($pattern, $subst) {
        return [TinyList]::new(($this.list -replace $pattern, $subst))
    }

    #T TinyList_Reverse1 [1 .. 10].Reverse() == [10 .. 1]
    #T TinyList_Reverse2 a = [1 .. 10]; b = a.Reverse(); b == [10 .. 1] && a == [1 .. 10] # verify b reverse, a unchanged
    #T TinyList_Reverse3 [].Reverse() == []
    #L Return a new list with the order of the items reversed e.g. <pre> [1, 2, 3].Reverse() == [3, 2, 1] </pre>
    [TinyList] Reverse() {
        $tl = [TinyList]::new(@($this.list))
        $tl.List.Reverse()
        return $tl
    }

    #T TinyList_Distinct1 [1, 2, 2, 3, 1, 3].Distinct() == [1, 2, 3]
    #T TinyList_Distinct2 ['a', 'b', 'b', 'c', 'a', 'c'].Distinct() == ['a', 'b', 'c']
    #T TinyList_Distinct3 [1, 'b', 'b', 'c', 1, 'c'].Distinct() == [1, 'b', 'c']
    #T TinyList_Distinct4 [].Distinct() == []
    #L Return a new list containing only the distinct items from the list e.g. <pre> [1,2,2,3,4,3].Distinct() == [1,2,3,4] </pre>
    [TinyList] Distinct() {
        return [TinyList]::New(@([Linq.Enumerable]::Distinct($this.list)))
    }

    #T TinyList_Intersect1 [1,2,3,4,5].Intersect([3,4,5,6,7]) == [3,4,5]
    #T TinyList_Intersect2 [1,2,3].Intersect([4,5,6,7]) == []
    #L Return the intersection of two lists
    [TinyList] Intersect($secondList) {
        if ($secondList -is [TinyList]) {
            $secondList = $secondList.List
        }
        return [TinyList]::new([Linq.Enumerable]::Intersect($this.List, $secondList))
    }

    #T TinyList_Union1 [1,2,3,4,5].Union([3,4,5,6,7]) == [1,2,3,4,5,6,7]
    #T TinyList_Union2 [1,2,3].Union([5,6,7]) == [1,2,3,5,6,7]
    #L Return the union of two lists
    [TinyList] Union($secondList) {
        if ($secondList -is [TinyList]) {
            $secondList = $secondList.List
        }
        return [TinyList]::new([Linq.Enumerable]::Union($this.List, $secondList))
    }

    #T TinyList_Except1 [1..10].Except([4,5,6,7]) == [1,2,3,8,9,10]
    #L Returns the items in the first list that don't appear in the second list e.g. <pre>[1..0].Except([4,5,6])</pre>
    [TinyList] Except($secondList) {
        if ($secondList -is [TinyList]) {
            $secondList = $secondList.List
        }
        return [TinyList]::new([Linq.Enumerable]::Except($this.List, $secondList))
    }

    Print() { $this.Print('') }

    #L Print the list to the screen, one item at a time. An optional regex can be used to filter the items e.g. <pre>ls().Print()</pre>
    Print($pattern) {
        if ($pattern -is [RegexLiteral]) {
            $pattern = $pattern.Value
        }
        foreach ($e in $this.list -match $pattern) { ShowValue "$e" }
    }

    #L Display the list using the PowerShell 'Out-Host' command.
    OutHost() {
        $this.List | Out-Host
    }

    #T TinyList_Split1 even::odd::_ = [1..10].Split{ it % 2 == 0}; even == [2, 4, 6, 8, 10] && odd == [1, 3, 5, 7, 9]
    #L Split a list into two parts based on whether a lambda returns true or not e.g.<pre>  even :: odd = [1..10].split{ it % 2 == 0} </pre>
    [TinyList] Split([TinyLambda] $lambda) {
        $parts = $this.list.where({$lambda.Dot(@($_), $_, [AutomationNull]::Value, $null)}, "split")
        $result = [tinylist]::new()
        foreach ($p in $parts) {
            $result.Add([tinylist]::new($p))
        }
        return $result
    }

    #T TinyList_Split2 ['a b', 'c d'].Split() == ['a', 'b', 'c', 'd']
    #L Turn each item in a list into a string, split that string on spaces and then flatten the result.
    [TinyList] Split() {
        $result = [TinyList]::new()
        $result.list.AddRange($this.list -split '\s+')
        $result.count = $result.list.count
        return $result
    }

    #T TinyList_Split3 ['a-b', 'c-d'].Split(r/-/) == ['a', 'b', 'c', 'd']
    #L Turn each item in a list into a string, split that string using the provided regex and then flatten the result e.g. <pre> ['a-b', 'c-d'].Split(r/-/) == ['a', 'b', 'c', 'd'] </pre>
    [TinyList] Split($pattern) {
        $result = [TinyList]::new()
        if ($pattern -is [RegexLiteral]) {
            $pattern = $pattern.Value
        }
        $result.list.AddRange($this.list -split $pattern)
        $result.count = $result.list.count
        return $result
    }

    #T TinyList_Sort1 [10 .. 1].Sort() == [1 .. 10]
    #L Sort a list in ascending order e.g. <pre>getRandom(10).Sort()</pre>
    [TinyList] Sort() {
        $tl = [TinyList]::new(@($this.list))
        $tl.List.Sort()
        return $tl
    }

    #T TinyList_SortDescending1 [1 .. 10].SortDescending() == [10 .. 1]
    #T TinyList_SortDescending2 [].SortDescending() == []
    #L Sort a list in descending order e.g. <pre>getRandom(10).SortDescending()</pre>
    [TinyList] SortDescending() {
        return [TinyList]::new(@($this.list | Sort-Object -Descending))
    }

    #T TinyList_Sort2 [1 .. 10].Sort { Minus(it) } == [10 .. 1]
    #L Sort a list based on the result of a lambda e.g. <pre> shell('gps').sort{ it.WS }.Last(10) </pre>
    [TinyList] Sort([TinyLambda] $lambda) {
        # Uses a scriptblock to run the lambda since Tiny lambdas
        # can't be cast to delegates.
        $result = [system.linq.enumerable]::OrderBy(
            [system.collections.generic.icollection[object]] $this.list,
                [func[object,object]] {param($n) $lambda.Dot(@($n), $n, [AutomationNull]::Value, $null)})
        return [TInyList]::new(@($result))
    }

    #T TinyList_SortDescending3 [1 .. 10].SortDescending { minus(it) } == [1 .. 10]
    #L Sort a list in descending order based on the result of a lambda e.g. <pre> shell('gps').sortdescending{ it.WS }.take(10) </pre>
    [TinyList] SortDescending([TinyLambda] $lambda) {
        $result = [system.linq.enumerable]::OrderByDescending(
            [system.collections.generic.icollection[object]] $this.list,
                [func[object, object]] {param($n) $lambda.Dot(@($n), $n, [AutomationNull]::Value, $null)})
        return [TinyList]::new(@($result))
    }

    #T TinyList_Where1 [1 .. 10].where{it % 2 == 0} == [2, 4, 6, 8, 10]
    #T TinyList_Where2 [10..1].where{it2 > 6} == [3,2,1]
    #L Filter a list of objects <pre>[1 .. 10].where{it % 2 == 0}</pre>
    [TinyList] Where([TinyLambda] $lambda) {
        if (-not $lambda) {errorMessage "Where(): lambda argument was null"}
        $result = [TinyList]::new()
        $index = 0
        :inner foreach ($v in $this.list) {
            if ($lambda.Dot(@($v, $index), $v, $index,  $null)) {
                $result.Add($v)
            }
            $index++
        }
        Return $result
    }

    #T TinyList_TrueForAll1 [true, true, true, true].TrueForAll()
    #T TinyList_TrueForAll2 [true, false, true].TrueForAll() == false
    #T TinyList_TrueForAll3 [1,2,3,4].TrueForAll()
    #T TinyList_TrueForAll4 [1,0,3,4].TrueForAll() == false
    #T TinyList_TrueForAll5 [].TrueForAll() == true
    #L Return true of all of the list elements are true
    [bool] TrueForAll() {
        return $this.TrueForAll($null)
    }

    #BUGBUGBUG - this is redundant now that we have All() but All() doesn't use list.TrueForAll()
    #T TinyList_TrueForAll6 [true, true, true, true].TrueForAll{ it } == true
    #T TinyList_TrueForAll7 [true, true, true, true].TrueForAll{ not(it) } == false
    #T TinyList_TrueForAll8 [true, false, true].TrueForAll{true} == true
    #T TinyList_TrueForAll9 [1,2,3,4].TrueForAll{it < 5} == true
    #T TinyList_TrueForAll10 [1,2,3,4].TrueForAll{it < 3}  == false
    #T TinyList_TrueForAll11 [].TrueForAll{ true } == true
    #L Return true if result of applying the lambda to each list item is true
    [bool] TrueForAll([TinyLambda] $lambda) {
        if ($null -eq $lambda) {
            return $this.List.TrueForAll{ param ($it) [bool] $it }
        }
        else {
            return $this.List.TrueForAll{ param ($it) [bool] $lambda.Dot(@($it), $it, [AutomationNull]::Value, $null) }
        }
    }
}

################################################################
#
# Add ForEach and Filter to TinyList as script methods because they can't
# be real methods on classes due to limitations in PowerShell class implementation.
#

#T TinyList_ForEach1 s = 0; [1 .. 5].foreach{n -> s += n} == null && s == 15
#T TinyList_ForEach2 s = 0; [1 .. 5].foreach{s += it} == null && s == 15
#L Apply a lambda to each item in the list. NOTE: ForEach{} returns nothing. e.g. <pre> [1 .. 10].foreach{ println("It is " + it)} == null </pre>
# ForEach( [TinyLambda] )
Update-TypeData -Force `
        -MemberType ScriptMethod `
        -MemberName ForEach `
        -TypeName "TinyList" `
        -Value { param([TinyLambda] $lambda)
            if (-not $lambda) { errorMessage "ForEach: lambda argument was null" }
            :inner foreach ($v in $this.list) {
                [void] $lambda.Dot(@($v), $v, [AutomationNull]::Value, $null)
            }
        }

#T TinyList_Filter1 [1 .. 10].filter{it % 2 == 0} == [2, 4, 6, 8, 10]
#T TinyList_Filter2 [1 .. 10].filter{n -> n % 2 == 0} == [2, 4, 6, 8, 10]
#L Filter a list of items based on the return value of a lambda <pre> [1 .. 10].filter{ i % 2 == 0 } </pre>
# [TinyList] Filter( [TinyLambda] )
Update-TypeData -Force `
        -MemberType ScriptMethod `
        -MemberName Filter `
        -TypeName "TinyList" `
        -Value { param ([TinyLambda] $lambda)
            if (-not $lambda) {errorMessage "Filter: lambda argument was null"}
            $result = [TinyList]::new()
            :inner foreach ($v in $this.list) {
                if ($lambda.Dot(@($v), $v, [AutomationNull]::Value, $null)) {
                    $result.Add($v)
                }
            }
            Return $result
        }

#T TinyList_AsList {a:1 b:2}.Values.AsList() == [1, 2]
#M Add a method to [object] to turn any object into a TinyList
Update-TypeData -Force `
        -MemberType ScriptMethod `
        -MemberName AsList `
        -TypeName "object" `
        -Value {
            if ($this -is [TinyList]) {
                return $this
            }

            if ($this -is [IList]) {
                return [TinyList]::New(@($this))
            }

            if ($this -is [IEnumerator]) {
                $result = [TinyList]::new()
                while ($this.MoveNext()) {
                    $result.Add($this.Current)
                }
                return $result
            }

            $result = [TinyList]::new()
            foreach ($o in $this) {
                $result.Add($o)
            }
            return $result
        }

# Add string extension methods to match TinyList methods (head, tail, match).

#T String_Match1 'abc'.match(r/a/) # match regex literal
#T String_Match2 'abc'.match('a')  # match a string
#T String_Match3 'abc'.match([<regex>].new('a')) # match a constructed regex object
#T String_Match4 'Abc'.match(r/a/) # Case insensitive
#T String_Match5 'Abc'.match(r/a/c) == false # case sensitive
#T String_Match6 'Abc'.match(r/A/c) == true  # case sensitive
#T String_Match7 'abcd'.match(r/bc/:result) then result[0] == 'bc'
#M add a .Match() method to strings
#BUGBUG fixup to use Regexliteral
Update-TypeData -Force `
        -MemberType ScriptMethod `
        -MemberName Match `
        -TypeName "string" `
        -Value {param($pattern)
            if ($pattern -is [RegexLiteral]) {
                return $pattern.Match($this)
            }
            $this -match $pattern
        }

#T String_Head1 "abc".head() == 'a'
#M add a .Head() method to strings
Update-TypeData -Force `
        -MemberType ScriptMethod `
        -MemberName head `
        -TypeName "string" `
        -Value {if ($this -ne '') {$this.substring(0,1)} else {''}}

#T String_Tail1 "abc".Tail() == 'bc'
#M add a .Tail() method to strings
Update-TypeData -Force `
        -MemberType ScriptMethod `
        -MemberName Tail `
        -TypeName "string" `
        -Value {if ($this -ne '') {$this.substring(1)} else {''}}


#####################################################################
#
# Names to compile as constant values
#
#T Constants_True       true
#T Constants_False      ! false
#T Constants_Null       null == _
#T Constants_UnderBar   _ == null
#T Constants_TinyHome   TinyHome is [<string>]
#T Constants_Pid        Pid is [<int>]
#T Constants_Global     __global.ContainsKey('__parent')
#T Constants_IsLinux    IsLinux is [<bool>]
#T Constants_IsMacOS    IsMacOs is [<bool>]
#T Constants_IsWindows  IsWindows is [<bool>]
#T Constants_IsCoreCLR  IsCoreClr
#T Constants_Host       host != null

$script:TinyConstants = @{

    'true'      = $true
    'false'     = $false
    'null'      = $null
    '_'         = $null
    'TinyHome'  = $PSScriptRoot
    'Pid'       = $pid
    'IsLinux'   = if (Test-Path variable:IsLinux)   { $IsLinux }  else { $false }
    'IsMacOS'   = if (Test-Path variable:IsMacOS)   { $IsMacOS }  else { $false }
    'IsWindows' = if (Test-Path variable:IsWindows) { $IsWindows } else { $true }
    'IsCoreCLR' = if (Test-Path variable:IsCoreClr) { $IsCoreCLR } else { $false }
    'Host'      = $global:host
    'PSVersionTable' = $PSVersionTable
}

#####################################################################
#
# Implements the Tiny variable scoping subsystem.
#
class Scopes
{
    static [List[Dictionary[string, object]]] $scopes

    static Init() {
        [scopes]::scopes = [List[Dictionary[string, object]]]::new()
        [scopes]::scopes.Add([Dictionary[string, object]]::new([StringComparer]::OrdinalIgnoreCase))
        # Add some default variables
        [scopes]::SetVariable('error',      $error)
        [scopes]::SetVariable('args',       @())
        [scopes]::SetVariable('it',         $null)
        [scopes]::SetVariable('it2',        $null)
        [scopes]::SetVariable('body',       $null)
        [scopes]::SetVariable('Matches',    $null)
        [scopes]::SetVariable('TinyError',  $null)
        [scopes]::scopes[0]['__file'] =     '<stdin>'
        [scopes]::Scopes[0]['__scopes'] =   [scopes]::scopes
        [scopes]::Scopes[0]['ToJson'] =     Get-Command ConvertTo-Json
        [scopes]::Scopes[0]['FromJson'] =   Get-Command ConvertFrom-Json
        # Set the current and parent scope pointers to the global scope initially
        [scopes]::SetVariable('__current',  [scopes]::Scopes[0])
        [scopes]::SetVariable('__parent',   [scopes]::Scopes[0])
        # Add __global to the constants table
        $script:TinyConstants['__global'] = [scopes]::scopes[-1]
    }

    #
    # Push a new, empty scope on the stack
    #
    static PushScope() {
        [scopes]::PushScope([Dictionary[string, object]]::new([StringComparer]::OrdinalIgnoreCase))
    }

    static PushScope([Dictionary[string, object]] $scope) {
        $parent = [scopes]::scopes[0]
        [scopes]::scopes.Insert(0, $scope)
        # Check for overflow BUGBUGBUG -this just doesn't work.
        #if ([scopes]::scopes.Count -gt 300) {
        #    write-host -fore Red "Tiny stack overflow (Stack depth: $([scopes]::scopes.Count)) - exiting!"
        #    # exit -1 # doesn't work
        #    # stop_current_execution # doesn't work
        #    Stop-Process $global:pid
        #}
        # set the current and parent scope pointer
        [scopes]::scopes[0]["__current"] = $scope
        [scopes]::Scopes[0]["__parent"]  = $parent
    }

    static  PopScope() {
        # don't pop the global scope
        if ([scopes]::scopes.Count -gt 1) {
            [scopes]::scopes[0].Clear()
            [scopes]::scopes[0] = $null
            [scopes]::scopes.RemoveAt(0)
        }
        else {
            throw "Trying to pop global scope"
        }
    }

    #
    # Pop and return the current scope. The popped scope is
    # used by the TinyLambda.Parent() invocation method.
    # (This is tested through that API.)
    #
    static  [Dictionary[string,object]] PopKeepScope() {
        # don't pop the global scope
        if ([scopes]::scopes.Count -gt 1) {
            $scope = [scopes]::scopes[0]
            [scopes]::scopes.RemoveAt(0)
        }
        else {
            throw "Trying to pop global scope"
        }
        return $scope
    }

    #T Variable_Case1 abc = 3.14; ABC == 3.14;
    #T Variable_Case2 __current.abc = 789; Abc == 789;
    #T Variable_Case3 aBC = 'hi there'; __current.abc == 'hi there';
    #
    # Set the value of a variable in the current scope. Trying to change the
    # value of a constant is an error.
    #
    static SetVariable ([string] $name, $value) {
        if ($script:TinyConstants.Contains($name)) {
             if ($script:TinyConstants[$name] -ne $value) {
                errorMessage "Can't rebind the constant '$name' with value '$value'"
            }
        }
        else {
            [scopes]::scopes[0][$name] = $value
        }
    }

    #
    # Get the current value of a variable. Throw an
    # exception if it doesn't exist.
    # (Tested everywhere in the test suites.)
    #
    static [object] GetVariable($name) {

        if ($script:TinyConstants.Contains($name)) {
            return $script:TinyConstants[$name]
        }

        foreach ($scope in [scopes]::scopes) {
            $val = $null
            if ($scope.TryGetValue($name, [ref] $val)) {
                # Copy into the local scope so the next access will be faster
                if ($scope -ne [scopes]::scopes[0]) {
                    [scopes]::scopes[0][$name] = $val
                }
                return $val
            }
        }

        errorMessage "Undefined variable '$name'."
        # never reached; just shuts up the compiler
        return $null
    }

    #
    # Try and get a variable; returns null if not found
    #
    static [object] TryGetVariable([string] $name) {
        foreach ($scope in [scopes]::scopes) {
            if ($scope.ContainsKey($name)) {
                $val = $scope[$name]
                # Copy into the local scope so next access will be faster
                [scopes]::scopes[0][$name] = $val
                return $val
            }
        }
        return $null
    }

    #
    # Return true if a variable exists
    # (Tested through the VarExists function.)
    #
    static [bool] VarExists([string] $name) {
        foreach ($scope in [scopes]::scopes) {
            if ($scope.ContainsKey($name)) {
                $val = $scope[$name]
                # Copy into the local scope so next access will be faster
                [scopes]::scopes[0][$name] = $val
                return $true
            }
        }
        return $false
    }

    #
    # Remove a variable if it's defined
    # (Tested through the VarRemove function.)
    #
    static [void] VarRemove ([string] $name) {
        foreach ($scope in [scopes]::scopes) {
            if ($scope.ContainsKey($name)) {
                $scope.Remove($name)
                break
            }
        }
    }

    #
    # Return the names of all of the visible variables
    # (Tested through the Vars() function.)
    #
    static [TinyList] vars() {
        #BUGBUG shouldn't have to do it this way; find out why more obvious methods don't work
        $count = [scopes]::scopes.Count-1
        $allKeys = foreach ($index in 0 .. $count) {
            [scopes]::Scopes[$index].get_Keys()
        }
        $allKeys += $script:TinyConstants.get_Keys()
        return ([TinyList]::new(($allKeys | Sort-Object -Unique)))
    }
}

[scopes]::init()


#############################################################################
#
# Utility function for displaying objects - not quite the same as PowerShell's mechanism as
# it displays lists below a certain size and containing certain types as a single string.
# This function is used by the println(), info(), warning() and error() Tiny functions.
#
function ShowValue ([AllowNull()] $valToShow, [ConsoleColor] $color = [consolecolor]::white ) {
    function ShouldToString($val) {
        if ($null -eq $val) {
            return $false
        }

        if ($val.GetType().IsValueType) {
            return $true
        }

        return $false
    }

    if ($null -ne $valToShow) {
        # Always show errors in red
        if ($valToShow -is [ErrorRecord]) {
             Write-Host -fore red $valToShow.Exception.ToString()
        }
        elseif ($valToShow -is [TinyList]) {
            # If the list is below a certain size and is of certain types fall render using ToString()
            if ($ValToShow.Count -gt 0 -and $valToShow.Count -le 100 -and (ShouldToString $valToShow.List[0])) {
                Write-Host -fore $color $valToShow.ToString()
            }
            else {
                $valToShow.List | Out-String -stream | Write-Host -fore $color
            }
        }
        else {
            $valToShow | Out-String -stream | Write-Host -fore $color
        }
    }
}

#############################################################################
#
# The Tiny module subsystem implementation. Modules are essentially
# scripts that are dot-sourced into the current environmnet. This is
# made somewhat more complicated by the fact that functions are bound
# at compile time and don't appear in the compiled expression tree
# for a script. A separate mechanism ($script:LocalFunctions) is used
# to accumulate these functions. Both the function bindings and the
# module body are captured in the TinyModule object. Once a module
# has been loaded and compiled, it won't be loaded again until its
# LastWriteTime changes (changed not newer) at which point the new
# text will parsed and the module object will be updated. The current module
# table is available in the environment through the variable __modules
#
[HashTable] $script:ModuleTable = @{}
$TinyConstants['__modules'] = $script:ModuleTable
class TinyModule {

    [string]
        $name

    [string]
        $Path

    [DateTime]
        $LastWriteTime

    [Expression]
        $ModuleBody

    [string]
        $ModuleText

    [HashTable]
        $FunctionsToDefine

    [string] ToString() {
        return ('[Module:{0}]' -f $this.Name)
    }

    #
    # Imports a module into the current scope.
    #
    static ImportTinyModule([string] $fileToImport) {
        $psmodule = $false
        $originalName = $fileToImport
        if (-not ($fileToImport -match '\.tiny$')) {
            $fileToImport += ".tiny"
        }

        if (-not (Test-Path $fileToImport)) {
            $fileToImport = [io.path]::Combine($PSScriptRoot,
                 [io.path]::Combine('modules', $fileToImport))
            if (-not (Test-Path $fileToImport)) {

                if ($originalName -notmatch '.tiny$') {
                    $fileToImport = $originalName + '.ps1'
                    $fileToImport = [io.path]::Combine($PSScriptRoot,
                        [io.path]::Combine('modules', $fileToImport))
                    if (-not (test-path $fileToImport)) {
                        errorMessage "Unable to locate module file '$fileToImport'"
                    } else {
                        $psmodule = $true
                    }
                }
                else {
                    errorMessage "Unable to locate module file '$fileToImport'"
                }
            }
        }

        # Get the absolute path since that's how modules are stored in the module table
        $fileToImport = (Resolve-Path $fileToImport).Path
        :return_from_function do {
            $oldFile = [scopes]::scopes[0]["__file"]
            $oldImport = [scopes]::scopes[0]["__ImportedFrom"]
            try {
                [scopes]::scopes[0]["__ImportedFrom"] = [scopes]::scopes[0]["__file"]
                [scopes]::scopes[0]["__file"] = $fileToImport
                # See if the module has already been loaded
                if ($null -ne ($module = $script:ModuleTable[$fileToImport])) {
                    # See if it's changed since the last load. If it has,
                    # load and parse the file.
                    $lwt = (Get-Item $fileToImport).LastWriteTime
                    if ($module.LastWriteTime -ne $lwt) {
                        $module.ModuleText = Get-Content -raw $fileToImport
                        $module.LastWriteTime = $lwt
                        if ($psmodule) {
                            try {
                                [scopes]::scopes[0][$module.name] = & $fileToImport
                            }
                            catch {
                                errorMessage $_
                            }
                        }
                        else {
                            [Tokenizer]::PushTokenizerState()
                            try {
                                [TinyModule]::ParseModule($module)
                            }
                            finally {
                                [Tokenizer]::PopTokenizerState()
                            }
                        }
                    }
                }
                else {
                    $module = [TinyModule]::New()
                    $module.Name = [System.IO.Path]::GetFileNameWithoutExtension($fileToImport)
                    $module.Path = $fileToImport
                    $module.ModuleText = Get-Content -raw $fileToImport
                    $module.LastWriteTime = (Get-Item $fileToImport).LastWriteTime
                    if ($psmodule) {
                        # If its a PowerShell module, evaluating it should return
                    `   # a dictionary which will be assigned to a variable in the current scope
                        # BUGBUGBUG - should verify that it's returning a dictionary
                        #BUGBUGBUG - Import into the global scope
                        [scopes]::scopes[-1][$module.name] = & $fileToImport
                    }
                    else {
                        [Tokenizer]::PushTokenizerState()
                        try {
                            [TinyModule]::ParseModule($module)
                        }
                        finally {
                            [Tokenizer]::PopTokenizerState()
                        }
                    }

                    $script:ModuleTable[$fileToImport] = $module
                }

                # Process all the functions that were bound during the parse.
                if ($module.FunctionsToDefine) {
                    foreach ($pair in $module.FunctionsToDefine.GetEnumerator()) {
                        #BUGBUGBUG Import into the global scope
                        [scopes]::scopes[-1][$pair.key] = $pair.Value
                    }
                }

                # And evaluate the tree
                if (-not $psmodule) {
                    [void] $module.ModuleBody.Eval()
                }
            }
            finally {
                [scopes]::scopes[0]["__file"] = $oldFile
                [scopes]::scopes[0]["__ImportedFrom"] = $oldImport
            }
        } while ($false)
    }

    static ParseModule($module) {
        $oldLocalFunctions = $script:LocalFunctions
        try {
            $script:LocalFunctions = [Dictionary[string,object]]::new([StringComparer]::OrdinalIgnoreCase)
            $module.ModuleBody = [Tiny]::parse($module.ModuleText)
            $module.FunctionsToDefine = $script:LocalFunctions
        }
        finally {
            $script:LocalFunctions = $oldLocalFunctions
        }
    }
}

#
# Utility class for representing LISP atoms
#
class Atom {
    [string] $Value

    [int] $lineNo

    Atom([string] $value, [int] $lineno) {
        if (! $value) {
            errorMessage "Cannot construct an Atom with a null value"
        }

        $this.Value = $value
        $this.lineno = $lineno
    }

    # The ToString() of an atom is used in places to
    # access members by name.
    [string] ToString() {
        return ($this.Value)
    }
}

#############################################################################
#
# A static class containing the implementation of Tiny built-in functions. These
# methods are using in $FunctionTable
#
class TinyFunctions {

    static [object] Sum ($list) {
        return [TinyFunctions]::Sum($list, $null)
    }

    static [object] Sum ($list, $propertyOrLambda) {
        if ($null -eq $list) {
            return $null
        }
        if ($list -is [TinyList]) {
            $list = $list.list
        }

        $result, $list = $list
        if ($null -ne $propertyOrLambda) {
            if ($propertyOrLambda -is [TinyLambda]) {
                $index = 0
                $result = $propertyOrLambda.Dot(@($result, $index), $result, $index, $null)
                foreach ($item in $list) {
                    $index++
                    $result += $propertyOrLambda.Dot(@($item, $index), $item, $index, $null)
                }
                return $result
            }
            else {
                $result = $result.$propertyOrLambda
                foreach ($item in $list) {
                    $result += $item.$propertyOrLambda
                }
                return $result
            }
        }
        else {
            foreach ($item in $list) {
                $result += $item
            }
        }
        return $result
    }

    static [object] Average($list) {
        return [TinyFunctions]::Average($list, $null)
    }

    static [object] Average($list, $propertyOrLambda) {
        if ($null -eq $list) {
            return 0
        }

        if ($list -is [TinyList]) {
            $list = $list.list
        }

        if ($list -isnot [IList]) {
            errorMessage "average: the first argument to average must be a list, not $($list.GetType())."
        }

        return [TinyFunctions]::Sum($list, $propertyOrLambda) / $list.Count
    }

    static [object] Head($list) {
        if ($list -is [TinyList]) {
            $list = $list.list
        }

        if (-not $list) {
            return $null
        }

        if ($list -is [string]) {
            if ($list.length -gt 0) {
                return $list.Substring(0,1)
            }
            else {
                return ""
            }
        }

        if ($list -is [IList]) {
            if ($list.Count -gt 0) {
                return $list[0]
            }
            else {
                return $null
            }
        }

        if ($list -is [IEnumerable]) {
            return [Linq.Enumerable]::FirstOrDefault([IEnumerable[object]] @($list))
        }

        return $list
    }

    static [object] First($list) {
        return [TinyFunctions]::First($list, 1)
    }

    static [object] First($list, [int] $n) {
        if ($null -eq $list) {
            return $null
        }

        if ($n -eq 1) {
            if ($list -is [string]) {
                # want a string result, not a char so can't use indexing
                if ($list.length -gt 0) {
                    return $list.substring(0, 1)
                }
                else {
                    return ""
                }
            }

            if ($list -is [Tinylist]) {
                if ($list.list.count -gt 0) {
                    return $list.list[0]
                }
                else {
                    return $null
                }
            }

            if ($list -is [IList]) {
                if ($list.count -gt 0) {
                    return $list[0]
                }
                else {
                    return $null
                }
            }

            if ($list -is [IEnumerable]) {
                return [Linq.Enumerable]::FirstOrDefault([IEnumerable[object]] @($list))
            }

            # Assume scalar and just return the object.
            return $list
        }
        else {
            if ($list -is [string]) {
                if ($list.Length -gt  $n) {
                    return $list.substring(0, $n)
                }
                else {
                    return $list
                }
            }

            if ($list -is [TinyList]) {
                $list = $list.list
            }

            if ($list.count -lt $n) {
                return [TinyList]::New(@($list))
            }

            return [TinyList]::new($list.GetRange(0, $n))
        }
    }

    #T FunctionFirstLambda1 [10..1] |> first {it % 4 == 0} == 8
    #BUGBUG Agment the method to have the same functionality
    #H Find the first element in a last where the lambda evaluates to true.
    static [object] First($list, [TinyLambda] $func) {
        if ($null -eq $list) {
            return $null
        }

        if ($list -is [string]) {
            $index = 0
            foreach ($c in $list.GetEnumerator()) {
                if ($func.Dot(@($c, $index), $c, $index, $null)) {
                    return $c
                }
                $index++
            }
            return $null
        }

        if ($list -is [TinyList]) {
            $list = $list.list
        }

        if ($list -isnot [IEnumerable]) {
            errorMessage "First: requires an enumerable to work with, not and object of type [$($list.GetType)]"
        }

        $index = 0
        foreach ($obj in $list.GetEnumerator()) {
            if ($func.Dot(@($obj, $index), $obj, $index, $null)) {
                return $obj
            }
            $index++
        }
        return $null
    }

    static [object] Tail ( $list ) {
        if ($null -eq $list) {
            return [TinyList]::new()
        }

        if ($list -is [TinyList]) {
            if ($list.list.Count -eq 0) {
                return [TinyList]::new()
            }
            else {
                return [TinyList]::new($list.List.GetRange(1, $list.List.Count-1))
            }
        }

        if ($list -is [string]) {
            if ($list -ne '') {
                return $list.substring(1, $list.length-1)
            }
            else {
                return ''
            }
        }
        else {
            errorMessage "Tail: Can't index into object '$list' of type $($list.gettype())"
        }
        return $null
    }

    static [void] Add($list, $itemToAdd) {
        if ($null -eq $list) {
            errorMessage "Add: can't add an item to a null list"
        }

        if ($list -is  [IDictionary]) {
            if ($itemToAdd -is [IDictionary]) {
                # This should use Tiny semantics, not PowerShell ones
                $list += $itemToAdd
            }
            else {
                $list[$itemToAdd] = $list[$itemtoadd]
            }
        }
        else {
            $list.Add($itemToAdd)
        }
    }

    static [object] AddPass($list, $itemToAdd) {
        if ($null -eq $list) {
            errorMessage "Add: can't add an item to a null list"
            return $null
        }

        if ($list -is [string]) {
            $list += $itemToAdd
        }
        elseif ($list -is  [IDictionary]) {
            if ($itemToAdd -is [IDictionary]) {
                #BUGBUGBUG This should use Tiny semantics, not PowerShell ones
                $list += $itemToAdd
            }
            else {
                $list[$itemToAdd] = $itemtoadd
            }
        }
        else {
            $list.Add($itemToAdd)
        }

        return $list
    }

    static [object] Range($lower, $upper) {
        return [TinyFunctions]::Range($lower, $upper, 1)
    }

    static [object] Range($lower, $upper, $step) {
        return [TinyOperators]::RangeImpl($lower, $upper, $step)
    }

    static [object] Map($list, $lambda) {
        if ($null -eq $lambda) {
            errorMessage "Map: lambda argument was null."
        }

        if ($null -eq $list) {
            return [TinyList]::new()
        }

        if ($list -is [TinyList]) {
            $list = $list.list
        }
        elseif ($list -isnot [IEnumerable]) {
            errorMessage ('Map: the ''Map'' function requires a collection to work on, ' +
                        "not an object of type '$($list.GetType())'")
        }

        # PowerShell doesn't enumerate dictionaries by default so we need to do this explicitly
        if ($list -is [IDictionary]) {
            $list = $list.GetEnumerator()
        }

        $isLambda = $lambda -is [TinyLambda]
        $index = 0
        # The label is required for break and continue
        $result = :inner foreach ($item in $list) {
            if ($islambda) {
                $lambda.Dot(@($item, $index), $item, $index, $null)
            }
            else {
                $lambda.Invoke($item)
            }
            $index++
        }

        <# Tried using the LINQ Select() implementation that follows however
         # the PowerShell 'foreach' implementation turned out to be faster,
         # probably due to avoidance of scriptblock as delegate invokes.
        $result = [Linq.Enumerable]::Select([IEnumerable[object]] $list,
                    [Func[object,object]] {param ($val) $lambda.Dot(@($val), $val, [AutomationNull]::Value, $null)})
        #>

        return [TinyList]::new($result)
    }

    static [object] FlatMap($list, [TinyLambda] $lambda) {
        if ($null -eq $lambda) {
            errorMessage "flatmap: lambda argument was null"
        }

        $result = [TinyList]::new()

        if ($null -eq $list) {
            return $result
        }

        if ($list -is [TinyList]) {
            $list = $list.list
        }
        elseif ($list -isnot [IEnumerable]) {
            errorMessage ('flatmap: the ''FlatMap'' function requires a collection to work on, ' +
                        "not an object of type '$($list.GetType())'")
        }

        # PowerShell doesn't enumerate dictionaries by default so we need to do this explicitly
        if ($list -is [IDictionary]) {
            $list = $list.GetEnumerator()
        }

        $index = 0
        # The label is required for break and continue
        :inner foreach ($item in $list) {
            $val = $lambda.Dot(@($item, $index), $item, $index, $null)
            $index++
            if ($null -ne $val) {
                if ($val -is [TinyList]) {
                    $result.list.AddRange($val.List)
                }
                elseif ($val -is [IEnumerable[object]]) {
                    $result.list.AddRange($val)
                }
                elseif ($val -is [IEnumerable]) {
                    # Need to use @() to copy var into an object array.
                    $result.list.AddRange(@($val))
                }
                else {
                    $result.Add($val)
                }
            }
        }

        $result.Count = $result.list.Count
        return $result
    }

##AddTinyFunction

}

#
# The following table all of Tiny's predefined functions.
# The contents of this table gets copied into the global scope
# at initialization time so function lookups are just
# variable lookups. Entries in this table can be PSMethodInfos,
# PowerShell CommandInfos or ScriptBlocks. ScriptBlock entries
# will be converted to 'InvokeReturnAsIs' methodinfos at
# initialization time.
#
$script:FunctionTable = @{

    #H function to quit the Tiny repl. e.g. <pre>quit()</pre>
    Quit = {break "outer"}

    #H Clear the screen .e.g. <pre>cls()</pre>
    Cls = [console]::Clear

    #T Function_Sum1 sum([1,2,3]) == 6
    #T Function_Sum2 [1,2,3] |> sum() == 6
    #T Function_Sum3 [ {a:1}, {a:2}, {a:3} ] |> sum 'a' == 6
    #T Function_Sum4 [ {a:1}, {a:2}, {a:3} ] |> sum{v -> v.a} == 6
    #T Function_Sum5 [ {a:1}, {a:2}, {a:3} ] |> sum('a') == 6
    #T Function_Sum6 [ {a:1}, {a:2}, {a:3} ] |> sum 'a' == 6
    #H Sum the values in the argument list e.g. <pre> sum([1,2,3,4]) == 10 </pre><pre>shell('Get-ChildItem -file') |> sum 'length'</pre>
    Sum = [TinyFunctions]::Sum

    #T Function_Product1 [1, 2, 3, 4] |> product == 24
    #T Function_Product3 [ {a:1}, {a:2}, {a:3}, {a:4} ] |> product{it.a} == 24
    #H Compute the product ot the values in the list argument, using a lambda if one is supplied. e.g. <pre> [1, 2, 3, 4] |> product == 24</pre>
    Product = {param ([TinyList] $list, $propertyOrLambda) $list.Product($propertyOrLambda) }

    #T Function_Average1 average([1,2,3]) == 2
    #T Function_Average2 [1,2,3] |> average() == 2
    #T Function_Average3 [ {a:1}, {a:2}, {a:3} ] |> average 'a' == 2
    #T Function_Average4 [ {a:1}, {a:2}, {a:3} ] |> average{v -> v.a} == 2
    #T Function_Average5 [ {a:1}, {a:2}, {a:3} ] |> average('a') == 2
    #H Get the average of a list of items e.g. <pre>  shell('Get-Process') |> average 'ws' </pre>
    Average = [TinyFunctions]::Average

    #H Print a string without a new line. Formatting is supported like String.Format. <pre>print('Hi {0}, it is {1}', 'Bob', getdate())</pre>
    Print = {
        if ($args -eq $null -or $args.Length -eq 0) { return }
        if ($args.Length -gt 1) {
            $str, $vals = $args
            $msg = $str -f @($vals)
        }
        else {
            $msg = "$args"
        }
        Write-Host -nonewline $msg
    }

    #H Print a line to the screen; formatting is supported like String.Format e.g. <pre>println('Hi {0}, it is {1}', 'Bob', getdate())</pre>
    Println = {
        if ($args -eq $null -or $args.Length -eq 0) {
            ShowValue ""
        }
        elseif ($args.Length -eq 1) {
            ShowValue $args[0]
        }
        else {
            $f, $v = $args
            ShowValue ("$f" -f @($v))
        }
    }

    #H Use the PowerShell Out-Host command to display an object e.g. <pre>OutHost(shell('gps'))</pre> <pre>Utils.GetProcesses() |> OutHost() </pre>
    OutHost = { param ($value)
        if ($value -is [TinyList]) {
            $value = $value.List
        }
        $value | Out-Host
    }

    #T Function_Format format('{0}+{1}', 2, 3) == '2+3'
    #h Varargs utility to format a string line String.Format() e.g. <pre>str = println('Hi {0}, it is {1}', 'Bob', getdate())</pre>
    Format = {
        if ($args -eq $null -or $args.Length -eq 0) {
            return ''
        }
        elseif ($args.Length -eq 1) {
            $args[0]
        }
        else {
            $f, $v = $args
            "$f" -f @($v)
        }
    }

    #T Function_FormatArgList1 formatArgList() == ''
    #T Function_FormatArgList2 formatArgList('Hi there') == 'Hi there'
    #T Function_FormatArgList3 formatArgList('{0}+{1}', [2, 3]) == '2+3'
    #H Variant on Format that takes a format string and a list of values to print <pre>formatArgList("{0}+{1}", [2, 3]) == '2+3'</pre>
    FormatArgList = {
        param($fmtString, $argList)

        if ($fmtString -eq $null -or $fmtString -eq "") {
            return ''
        }

        if (-not $argList) {
            return $fmtString
        }

        if ($argList -is [TinyList]) {
            $argList = $argList.List | write-output
        }

        $fmtString -f $argList
    }

    #H Write out a warning message. This function supports string formatting. e.g. <pre>warning('This is an error message {0}', 123)</pre><pre>[1..10] |> warning</pre>
    Warn = {
        if (-not $args) {
            ShowValue -color magenta ""
        }
        elseif ($args.Length -eq 1) {
            $err = $args[0]
            if ($err -is [TinyException]) {
                ShowValue -color magenta $err.ToString()
            }
            else {
                ShowValue -color magenta $err
            }
        }
        else {
            $f, $v = $args
            ShowValue -color magenta ("$f" -f @($v))
        }
    }

    #H Display an informational message on the screen. This function supports string formatting. e.g. <pre>info('Hello')</pre><pre>ls() |> info</pre>
    Info = {
        if (-not $args) {
            ShowValue -color yellow ""
        }
        elseif ($args.Length -eq 1) {
            $val = $args[0]
            if ($val -is [TinyException]) {
                ShowValue -color yellow $val.ToString()
            }
            else {
                ShowValue -color yellow $val
            }
        }
        else {
            $f, $v = $args
            ShowValue -color yellow ("$f" -f @($v))
        }
    }

    #H Write out an alert message in such a way that it will be distinguishable on the screen. This function supports string formatting.  e.g. <pre>alert('Hello')</pre><pre>ls() |> alert</pre>
    Alert = {
        if (-not $args) {
            ShowValue -color green ""
        }
        elseif ($args.Length -eq 1) {
            $err = $args[0]
            if ($err -is [TinyException]) {
                ShowValue -color green $err.ToString()
            }
            else {
                ShowValue -color green $err
            }
        }
        else {
            $f, $v = $args
            ShowValue -color green ("$f" -f @($v))
        }
    }

    #H Write out an error message. This function supports string formatting. e.g. <pre>error 'This is an error message'</pre>
    Error = {
        if (-not $args) {
            ShowValue -color red ""
        }
        elseif ($args.Length -eq 1) {
            $err = $args[0]
            if ($err -is [TinyException]) {
                ShowValue -color red $err.ToString()
            }
            else {
                ShowValue -color red $err
            }
        }
        else {
            $f, $v = $args
            ShowValue -color red ("$f" -f @($v))
        }
    }

    #H Print a list to the screen, one object at a time e.g. <pre>printlist([1,2,3,4,5])</pre>
    PrintList = {param([TinyList] $list)
        if ($null -ne $list) {
            foreach ($e in $list.list) { ShowValue $e }
        }
        else {
            ShowValue ''
        }
    }

    #T Function_Length1 getlength("abc") == 3
    #T Function_Length1a "abc" |> getlength() == 3
    #T Function_Length1b "abc" |> getlength == 3
    #T Function_Length2 getlength([1, 2, 3, 4]) == 4
    #T Function_Length2a [1, 2, 3, 4] |> getlength() == 4
    #T Function_Length3 getlength({a:1 b:2}) == 2
    #T Function_Length3a {a:1 b:2} |> getlength() == 2
    # Get the length of any collection; returns 1 for a scalar, 0 for null e.g. <pre>length([1,2,3,4]) == 4</pre> <pre>length(null) == 0</pre>
    GetLength = {
        param ($list)
        if ($null -eq $list) {
            0
        }
        elseif ($list -is [TinyList]) {
            $list.List.Count
        }
        elseif ($list -is [IDictionary]) {
            $list.get_Count()
        }
        elseif ($list -is [Array]) {
            $list.Length
        }
        elseif ($list -is [IList]) {
            $list.Count
        }
        elseif ($list -is [string]) {
            $list.Length
        }
        elseif ($list -is [IEnumerable]) {
            [Linq.Enumerable]::Count([IEnumerable[object]] @($list))
        }
        else {
            # assume scalar so return 1
            1
        }
    }

    #T Function_Head1 head('abc') == 'a'
    #T Function_Head2 head([1, 2, 3]) == 1
    #T Function_Head3 head(null) == null
    #T Function_Head4 head("") == null
    #T Function_Head5 head([]) == null
    #H Nondestructively retrieve the first item from a list e.g. <pre>head([1,2,3]) == 1</pre><pre>head("abc") == "a"</pre>
    Head = [TinyFunctions]::Head

    #T Function_First1 First('abcde') == 'a'
    #T Function_First2 First('abcde', 3) == 'abc'
    #T Function_First3 'abcde' |> first(3) == 'abc'
    #T Function_First4 First([1, 2, 3]) == 1
    #T Function_First5 First([1, 2, 3], 2) == [1, 2]
    #T Function_First6 [1, 2, 3] |> first(2) == [1, 2]
    #H Nondestructively fetch the first item (or first n items) from a list or string, the object if the argument is scalar and null if the argument is null.
    First = [TinyFunctions]::First

    #T Function_Tail1 tail('abc') == 'bc'
    #T Function_Tail2 'abc' |> tail() == 'bc'
    #T Function_Tail3 tail([1,2,3]) == [2, 3]
    #T Function_Tail4 [1,2,3] |> tail() == [2, 3]
    #T Function_Tail5 tail(null) == []
    #T Function_Tail6 tail([]) == []
    #T Function_Tail7 tail("") == ''
    #H Non-destructively retrieve all but the first elements from a list or stringe.g. <pre>tail([1,2,3]) == [2,3]</pre>
    Tail = [TinyFUnctions]::Tail

    #T Function_Add1 l = [1,2,3]; add(l, 4); l == [1, 2, 3, 4]
    #T Function_Add2 l = [1,2,3,4]; l |> add(5); l == [1, 2, 3, 4, 5]
    #T Function_Add3 l = [1,2,3,4]; l |> add([5, 6]); l == [1, 2, 3, 4, [5, 6]]
    #H Add an item to the end of a list; modifies it's argument and returns nothing.
    Add = [TinyFunctions]::Add

    #T Function_AddPass1 l = [1,2,3]; AddPass(l, 4) == [1, 2, 3, 4]
    #T Function_AddPass2 [1,2,3,4] |> AddPass(5) == [1, 2, 3, 4, 5]
    #T Function_AddPass3 "abc" |> AddPass 'def' == 'abcdef'
    #T Function_AddPass4 {a:1} |> AddPass 'b' |> Keys == ['a', 'b']
    #T Function_AddPass5 {a:1} |> AddPass {b:2 c:3 d:4} |> Keys == ['a', 'b', 'c', 'd']
    #T Function_AddPass6 {a:1} |> AddPass {b:2 c:3 d:4} |> Values == [1, 2, 3, 4]
    #H Add an item to the end of a list; modifies it's argument (if possible) then returns it
    AddPass = [TinyFunctions]::AddPass

    #T Function_TypeName1 typename(1) == 'System.Int32'
    #T Function_TypeName2 typename([]) == 'TinyList'
    #T Function_TypeName3 {2+2} |> typename == 'TinyLambda'
    #H Return the type name string for the argument's type.
    TypeName = {param($object) if ($null -eq $object) { "null" } else { $object.gettype().FullName }}

    #T Function_Type1 gettype(123) == [<int>]
    #T Function_Type2 123 |> gettype() == [<int>]
    #T Function_Type3 [] |> gettype() == [<TinyList>]
    #T Function_Type4 {2+2} |> gettype() == [<TinyLambda>]
    #H Return the argument's .NET type.
    GetType = {param($object) if ($null -eq $object) { $null } else { $object.gettype() }}

    #T Function_Cast1 Cast("123", [<int>]) is [<int>]
    #T Function_Cast2 Cast("abc", [<int>]) == null
    #H Cast an object to the specified type, return null if it's not possible e.g. <pre>cast("123", [&lt;int&gt;]) is [&lt;int&gt;]</pre>
    Cast = {
        [CmdletBinding()]
        param ([Parameter(Mandatory)] $object, [Parameter(Mandatory)] [type] $type)
        $object -as $type
    }

    #T Function_GetMembers1 GetMembers("abc") ~ 'substring'
    #T Function_GetMembers2 GetMembers("abc").where{ it ~ r/substring/ }.Count == 1
    #T Function_GetMembers3 GetMembers(getdate(), "DayOfWeek") == ['DayOfWeek']
    #H Return all of the members implemented by a type formatted, as strings e.g. <pre>getmembers("abc")</pre>
    GetMembers = {
        param($object, $pattern = '')

        if ($null -eq $object) {
            return [TinyList]::new()
        }

        if ($pattern -is [RegexLiteral]) {
            $pattern = $pattern.Value
        }

        # Handle dictionary members first        
        if ($object -is [IDictionary]) {
            return [TinyList]::new($object.get_Keys() -match $pattern)
        }

        if ($object -is [type]) {
            $type = $object
        }
        else {
            $type = $object.GetType()
        }

        return [TinyList]::new(@($type.GetMembers().Name -match $pattern -notmatch '_' | Sort-Object -Unique))
    }

    #T Function_Not1 not(false) == true
    #T Function_Not2 false |> not == true
    #T Function_Not3 not(null) == true
    #T Function_Not4 not(0) == true
    #T Function_Not5 not("") == true
    #T Function_Not6 not([]) == true
    #T Function_Not7 not(true) == false
    #T Function_Not8 true |> not == false
    #T Function_Not9 not(1) == false
    #T Function_Not10 not('hi there') == false
    #T Function_Not11 'hi there' |> not() == false
    #T Function_Not12 'hi there' |> not == false
    #H Return the boolen complement of the argument object using PowerShell truthy semantics.
    Not = {param($val) -not $(if ($val -is [TinyList]) { $val.list } else { $val }) }

    #T Function_Void1 void(123) == null
    #T Function_Void2 void(getdate()) == null
    #T Function_Void3 getdate() |> void == null
    #H Discard the argument value (essentially a cast to void).
    Void = {param ($val)}

    #T Function_Range1 range(1,10) == [1,2,3,4,5,6,7,8,9,10]
    #T Function_Range2 range(0, 10, 2) == [0, 2, 4, 6, 8, 10]
    #T Function_Range3 range('a', 'd') == ['a', 'b', 'c', 'd']
    #H Return a list of numbers over the range specified with an optional step ex: range(low, hi[, step]). This function is semantically equivalent to the range operator.
    Range = [TinyFunctions]::Range

    #T Function_List1 newlist(10).Count == 10
    #T Function_List2 newlist(10)[0] == null
    #T Function_List3 newlist(10)[9] == null
    #T Function_List4 newlist(5, '+') == ['+', '+', '+', '+', '+']
    #H Create a new list of the specified size with an optional initial value e.g. <pre>list(size, 'X') == ['x'] * size</pre>
    NewList = {param([int] $size, $initialValue = $null)
        if ($size) {
            $mylist = [TinyList]::new(@($initialValue) * $size)
        }
        else {
            $mylist = [TinyList]::new()
        }
        $mylist
    }

    #T Function_AsNumber1 Asnumber('123') == 123
    #T Function_AsNumber2 '123' |> AsNumber() == 123
    #T Function_AsNumber3 Asnumber('123') is [<int>]
    #T Function_AsNumber4 Asnumber('123.456') == 123.456
    #T Function_AsNumber5 Asnumber('123.456') is [<double>]
    #T Function_AsNumber6 Asnumber('abc') == 0
    #T Function_AsNumber7 Asnumber('abc') is [<double>]
    #H Convert an object into a number if possible (int or double); null otherwise e.g.<pre>AsNumber('123') == 123</pre><pre>'123' |> AsNumber</pre>
    AsNumber = {param ($object)
        if ($object -match '\.') {
            # Has a decimal point so it must be a double
            $object -as [double]
        }
        elseif ($object -match 'i$') {
            $strVal = $object.ToString()
            $strval = $strval.substring(0, $strval.length -1)
            $strval -as [BigInt]
        }
        else {
            if ($val = $object -as [int]) {
                $val
            }
            else {
                # failed to convert to int so try double
                $val -as [double]
            }
        }
    }

    #T Function_AsString1 Asstring(123) is [<string>]
    #T Function_AsString2 Asstring({}) is [<string>]
    #T Function_AsString3 Asstring(getdate()) is [<string>]
    #T Function_AsString4 [1,2,3] |> AsString == ["1", "2", "3"]
    #H Convert an object into a string. If the argument is a list, convert each element into a list. e.g.<pre>[1,2,3] |> AsString == ["1","2","3"]</pre>
    AsString = { param ($object)
        if ($object -is [TinyLambda]) {
            $result = foreach ($el in $object.List) {
                "$el"
            }
            [TinyList]::New($result)
        }
        else {
            "$object"
        }
    }

    #T Function_Minus1 minus(-1) == 1
    #T Function_Minus2 minus(1) == -1
    #T Function_Minus3 minus(1) is [<int>]
    #T Function_Minus4 minus(-1.0) == 1.0
    #T Function_Minus5 minus(1.0) == -1.0
    #T Function_Minus6 minus(-1.0) is [<double>]
    #H Negate a value (essentially the unary '-' operator) e.g. <pre>not(true) == false</pre>
    Minus = {param($val) -$val }

    #T Function_Bnot1 bnot(0xdeadbeef) == 559038736
    #T Function_Bnot2 bnot(bnot(0xdeadbeef)) == 0xdeadbeef
    #H Bitwise compliment of a number
    Bnot = {param($val) -bnot $val}

    #T Function_Band1 band(1, 1) == 1
    #T Function_Band2 band(0, 1) == 0
    #T Function_Band3 band(1, 0) == 0
    #T Function_Band4 band(0, 0) == 0
    #T Function_Band5 band(0xF, 0x7) == 7
    #H Bitwise 'and' of two words
    Band = { param($v1, $v2) $v1 -band $v2}
  
    #T Function_Bor1 bor(1, 1) == 1
    #T Function_Bor2 bor(0, 1) == 1
    #T Function_Bor3 bor(1, 0) == 1
    #T Function_Bor4 bor(0, 0) == 0
    #T Function_Bor5 bor(9, 7) == 0xF
    #H Bitwise 'or' of two words
    Bor = { param($v1, $v2) $v1 -bor $v2}

    #T Function_Bxor1 bxor(1, 1) == 0 
    #T Function_Bxor2 bxor(0, 1) == 1 
    #T Function_Bxor3 bxor(1, 0) == 1 
    #T Function_Bxor4 bxor(0, 0) == 0
    #T Function_Bxor5 bxor(0xff, 0x66) == 0x99
    #H Bitwise 'xor' of two words
    Bxor = { param($v1, $v2) $v1 -bxor $v2}

    #T Function_Bshl1 bshl(0x01, 1) == 2
    #T Function_Bshl2 bshl(0x01, 2) == 4
    #T Function_Bshl3 bshl(0x01, 3) == 8
    #H Bitwise shift left
    bshl = { param($v1, $v2) $v1 -shl $v2}

    #T Function_Bshr1 bshr(8, 1) == 4
    #T Function_Bshr2 bshr(8, 2) == 2
    #T Function_Bshr3 bshr(8, 3) == 1
    #T Function_Bshr4 bshr(8, 4) == 0
    #T Function_Bshr5 bshr(8, 5) == 0
    #H Bitwise shift right
    bshr = { param($v1, $v2) $v1 -shr $v2}
  
    #T Function_AsBool1 Asbool(1) is [<boolean>]
    #T Function_AsBool2 Asbool(1) == true
    #T Function_AsBool3 Asbool(0) == false
    #T Function_AsBool4 Asbool("abc") == true
    #T Function_AsBool5 Asbool("") == false
    #T Function_AsBool6 Asbool([1,2,3]) == true
    #T Function_AsBool7 Asbool([]) == false
    #T Function_AsBool8 Asbool({}) == true
    #H Convert the argument to a boolean value e.g. <pre>AsBool("hi") == true</pre><pre>AsBool(0) == false</pre>
    AsBool = {param($val) [Tiny]::AsBool($val) }

    #T Function_IsNumber1 IsNumber(1) == true
    #T Function_IsNumber2 IsNumber(1.0) == true
    #T Function_IsNumber3 IsNumber("abc") == false
    #T Function_IsNumber4 IsNumber("123") == false
    #T Function_IsNumber5 [<long>].Parse(123) is [<long>] && IsNumber([<long>].Parse(123)) == true
    #H True if the argument is a System.Double or System.Int.
    IsNumber = {param($val) $val -is [Double] -or $val -is [Long] -or $val -is [Int] -or $val -is [Decimal] -or $val -is [BigInt]}

    #T Function_IsHash1 IsHash({a:1 b:2}) == true
    #T Function_IsHash2 IsHash({}) == true
    #T Function_IsHash3 IsHash('abc') == false
    #T Function_IsHash4 IsHash({2+2}) == false
    #H True if the argument is a Dictionary.
    IsHash = {param($val) $val -is [System.Collections.IDictionary]}

    #T Function_IsList1 IsList([1,2,3]) == true
    #T Function_IsList2 IsList([]) == true
    #T Function_IsList3 IsList([1,2,3].list) == true
    #T Function_IsList4 IsList([].list) == true
    #T Function_IsList5 IsList("abc") == false
    #T Function_IsList6 IsList {}  == false
    #H True if the argument is a list (TinyList or System.Collections.IList).
    IsList = {param($val)  $val -is [TinyList] -or $val -is [System.Collections.IList]}

    #T Function_AsList1 'hi' |> AsList |> map {it.Length} == [2]
    #T Function_AsList2 null |> aslist == []
    #H Convert any object into a TinyList. e.g. <pre>'hi' |> AsList |> map {it.Length}</pre>
    AsList = {param($val)

        if ($val -is [TinyList]) {
            return $val
        }

        if ($val -is [IList]) {
            return [TinyList]::New(@($val))
        }

        if ($val -is [IEnumerator]) {
            $result = [TinyList]::new()
            while ($val.MoveNext()) {
                $result.Add($val.Current)
            }
            return $result
        }

        $result = [TinyList]::new()
        foreach ($o in $val) {
            $result.Add($o)
        }
        return $result
    }

    #T Function_IsNull1 IsNull(null) == true
    #T Function_IsNull2 IsNull(123) == false
    #T Function_IsNull3 IsNull("") == false
    #T Function_IsNull4 IsNull([]) == false
    #T Function_IsNull5 IsNull({}) == false
    #H True if the argument is null.
    IsNull = {param ($val) $null -eq $val}

    #T Function_IsString1 IsString('abc') == true
    #T Function_IsString2 IsString('')    == true
    #T Function_IsString3 IsString(null)  == false
    #T Function_IsString4 IsString(123)   == false
    #H True if the argument is a string.
    IsString = { param($object) $object -is [string] }

    #T Function_MatchAll matchall(['a', 'b', 'c', 'd'], '[ad]') == ['a', 'd']
    #T Function_MatchAll2 ['a', 'b', 'c', 'd'] |> matchAll('[ad]') == ['a', 'd']
    #T Function_MatchAll3 ['a', 'b', 'c', 'd'] |> matchAll(r/[ad]/) == ['a', 'd']
    #T Function_MatchAll4 ['a', 'aa', 'aaa'] |> matchAll('a', {it.length}) == [1, 2, 3]
    #T Function_MatchAll5 ['a', 'aa', 'aaa'] |> matchAll(r/a/, {it.length}) == [1, 2, 3]
    #W Function_MatchAll6 [<diagnostics.process>].getprocesses() |> matchall 'csrss' |> getlength > 1
    #W Function_MatchAll7 [<diagnostics.process>].getprocesses() |> matchall r/csrss/ {it.WS} |> getlength > 1
    #T Function_MatchAll8 ['a1a', 'a2a', 'a34aa'] |> matchAll(r/([0-9]+)/:num, {num[0]}) == [1, 2, 34]
#BUGBUGBUGBUG - can't use a pattern as an r-value
    # Function_MatchAll9 [[1,2,3]] |> matchall(a::b::c::_, {a+b+c}) == [6]
    #H Select items from a list that match a regex, list pattern or property pattern, optionally applying a lambda e.g. <pre>MatchAll(['one', 'two', 'three'], r/[oe]/, {it.Substring(0,3)})</pre><pre>['one', 'two', 'three'] |> matchall(r/[oe]/, {it.Substring(0,3)})</pre>
    MatchAll = {param ($array, $pattern, [TinyLambda] $lambda = $null)
        if ($null -eq $array) {errorMessage "Match: array argument is null"}
        if (-not $pattern) {errorMessage "Match: pattern argument is null"}

        if ($array -is [TinyList]) {
            $array = $array.list
        }
        elseif ($array -isnot [IEnumerable]) {
            errorMessage 'The ''MatchAll'' function requires a collection to work on.'
        }

        $result = [tinylist]::new()
        if ($lambda) {
            :inner foreach ($s in $array) {
                if ($pattern -is [Pattern] -or $pattern -is [PropertyPattern]) {
                    if ($pattern.Set($s)) {
                        if ($null -ne ($res = $lambda.Dot(@($s), $s, [AutomationNull]::Value, $null))) {
                            $result.Add($res)
                        }
                    }
                }
                elseif ($pattern -is [RegexLiteral]) {
                    if ($pattern.Match($s)) {
                        if ($null -ne ($res = $lambda.Dot(@($s), $s, [AutomationNull]::Value, $null))) {
                            $result.Add($res)
                        }
                    }
                }
                elseif ($s -match $pattern) {
                    [scopes]::SetVariable("Matches", $matches)
                    if ($null -ne ($res = $lambda.Dot(@($s), $s, [AutomationNull]::Value, $null))) {
                        $result.Add($res)
                    }
                }
            }
        }
        else {
            if ($pattern -is [Pattern] -or $pattern -is [Propertypattern]) {
                $result.list.AddRange($array.where{ $pattern.Set($_) })
            }
            else {
                $result.list.AddRange(@($array -match $pattern))
            }
        }
        $result.count = $result.list.count
        $result
    }

    #T Function_NotMatchAll1 NotMatchAll(['a', 'b', 'c', 'd'], '[ad]') == ['b', 'c']
    #T Function_NotMatchAll2 ['a', 'b', 'c', 'd'] |> NotMatchAll('[ad]') == ['b', 'c']
    #T Function_NotMatchAll3 ['a', 'b', 'c', 'd'] |> NotMatchAll(r/[ad]/) == ['b', 'c']
    #T Function_NotMatchAll4 [<diagnostics.process>].getprocesses() |> notmatchall 'csrss' |> getlength > 1
    #T Function_NotMatchAll5 [<diagnostics.process>].getprocesses() |> notmatchall 'csrss' {it.WS} |> getlength > 1
    #T Function_NotMatchAlk6 [<diagnostics.process>].getprocesses() |> notmatchall r/csrss/ {it.WS} |> getlength > 1
    #H Select items from a list that don't match a regex, optionally applying a lambda e.g. <pre>NotMatchAll( ['one', 'two', 'three'], r/[xyz]/, {it.Substring(0, 3)})</pre>
    NotMatchAll = {param ($array, [string] $pattern, [TinyLambda] $lambda = $null)
        if ($null -eq $array) {errorMessage "Match: array argument is null"}
        if (-not $pattern) {errorMessage "Match: pattern argument is null"}
        if ($array -is [TinyList]) {
            $array = $array.list
        }
        elseif ($array -isnot [IEnumerable]) {
            errorMessage 'The ''NotMatchAll'' function requires a collection to work on.'
        }

        $result = [tinylist]::new()
        if ($pattern -is [RegexLiteral]) {
            $pattern = $pattern.Value
        }

        if ($lambda) {
            :inner foreach ($s in @($array -notmatch $pattern)) {
                $result.Add($lambda.Dot(@($s), $s, [AutomationNull]::Value, $null))
            }
        }
        else {
            $result.list.AddRange(@($array -notmatch $pattern))
        }
        $result.count = $result.count
        $result
    }

    #T Function_Split1 split('a b c', r/ /) == ['a', 'b', 'c']
    #T Function_Split2 split('', r/ /) == [''] # BUGBUGBUG Why is this [''] instead of []
    #T Function_Split3 split(null, r/ /) == ['']
    #T Function_Split4 split('a b c', r/ /, 1) == 'b'
    #T Function_Split5 'a b c' |> split(r/ /) == ['a', 'b', 'c']
    #T Function_Split6 '' |> split(r/ /) == ['']
    #T Function_Split7 null |> split(r/ /) == ['']
    #T Function_Split8 'a b c' |> split(r/ /, 1) == 'b'
    #H Split a string into a list using a regex, optionally selecting a specific index e.g. <pre>split("a b c", " ",1) # returns "b"</pre>
    Split = {param($valToSplit, $pattern, [int] $index = -1)
        #BUGBUGBUG the split function should match the .Split() method and support lambdas
        if ($valToSplit -is [TinyList]) {
            $valToSplit = $valToSplit.List
        }

        if ($pattern -is [RegexLiteral]) {
            $pattern = $pattern.Value
        }

        if ($valToSplit -is [IList]) {
            $result = [TinyList]::new()
            foreach ($e in $valToSplit) {
                $val = @($e -split $pattern)
                if ($index -ge 0) {
                    $val = $val[$index]
                }
                else {
                     $val = [TinyList]::new($val)
                }
                $result.List.Add($val)
            }
            $result.Count = $result.list.count
            $result
        }
        else {
            if ($index -eq -1) {
                $result = [TinyList]::new(@($valToSplit -split $pattern) -ne $null)
                return $result
            }
            else {
                @($valToSplit -split $pattern)[$index]
            }
        }
    }

    #T Function_Slice1 [1..10] |> slice(5,-1)  == [6..10]
    #T Function_Slice2 [1..10] |> slice(-4,-1) == [7,8,9,10]
    #T Function_Slice3 [1..10] |> slice(1,-2)  == [2, 3, 4, 5, 6, 7, 8, 9]
    #T Function_Slice4 [1..10] |> slice(2, -3) == [3, 4, 5, 6, 7, 8]
    #T Function_Slice5 [1..10] |> slice(3, -4) == [4, 5, 6, 7]
    #T Function_Slice6 [1..10] |> slice(4, -5) == [5, 6]
    #T Function_Slice7 [1..10] |> slice(5, -6) == []
    # T Function_Slice8 [1..10] |> slice(6, -7)  #BUGBUBUG this errors out. Fix it.
    #H Cut a slice from a list (Uses the TinyList .Slice() method.) Positive indexs start from the front of the list. Negative indexes are from the end of the list. e.g. <pre>[1..10] |> Slice(1,4)</pre>
    Slice = {param ($list, [int] $start, [int] $end = -1)

        if ($list -isnot [TinyList]) {
            $list = [TinyList]::new(@($list))
        }

        return $list.Slice($start, $end)
    }

    #T Function_Map1 [1, 2, 3] |> map {it * 2} == [2, 4, 6]
    #T Function_Map2 [<Diagnostics.Process>].getprocesses() |> map {it.ws} |> sum > 0
    #T Function_Map3 null |> map {it} == []
    #T Function_Map4 null |> map {} == []
    #T Function_Map5 [] |> map {} == [null] #BUGBUGBUG Why does this happen?
    #T Function_Map6 {a:1 b:2 c:3} |> map {it.value * 2} == [2, 4, 6]
    #T Function_Map7 ['a', 'b', 'c'] |> map {[it, it2]} == [['a', 0], ['b', 1], ['c', 2]]
    #H Apply a function to each member of a list e.g. <pre> ls() |> map { it.name } </pre>
    Map = [TinyFunctions]::Map

    #T Function_Flatmap1  [1, 2, 3] |> flatmap { [it, it, it] } == [1, 1, 1, 2, 2, 2, 3, 3, 3]
    #T Function_FlatMap2  {a:1 b:2} |> flatmap {it.key * 2} == ['aa', 'bb']
    #T Function_FlatMap3  [] |> flatmap { it } == []
    #T Function_FlatMap4  null |> flatmap { it } == []
    #T Function_FlatMap5  ['a', 'b', 'c'] |> flatmap {[it, it2]} == ['a', 0, 'b', 1, 'c', 2]
    #H Apply a lambda to each item in the input list, merging the results into a single flat list. The variable 'it' holds the value and 'it2' holds the index.
    FlatMap = [TinyFunctions]::FlatMap

    #T Function_Flatten1 [[1, 2], [3, 4]] |> flatten() == [1 .. 4]
    #T Function_Flatten2 [[1, [2]], [[3, 4]], 5] |> flatten() == [1, [2], [3, 4], 5]
    #T Function_Flatten3 [] |> flatten() == []
    #T Function_Flatten4 null |> flatten() == []
    #T Function_Flatten5 'abc' |> flatten() == ['abc']
    #T Function_Flatten6 {a:1 b:2 c:3} |> flatten |> property 'key' == ['a', 'b', 'c']
    #L A function to flatten a list so any nested lists are merged into the parent e.g. <pre>[[1,2], [2,3]] |> flatten() == [1,2,3,4]</pre>
    Flatten = {
        param($list)

        $result = [TinyList]::new()
        if ($null -eq $list) {
            return $result
        }

        if ($list -is [TinyList]) {
            $list = $list.list
        }
        elseif ($list -isnot [IEnumerable]) {
            errorMessage 'flatten: the ''Flatten'' function requires a collection to work on.'
        }

        # BUGBUGBUG not sure that flattening a dictionary makes sense
        # We don't flatten dictionaries that are returned by the lambda. Should we?
        if ($list -is [IDictionary]) {
            $list = $list.GetEnumerator()
        }

        foreach ($item in $list) {
            if ($null -eq $item) {
                continue
            }
            elseif ($item -is [TinyList]) {
                $result.list.AddRange($item.List)
            }
            elseif ($val -is [IEnumerable[object]]) {
                $result.list.AddRange($item)
            }
            elseif ($item -is [IEnumerable]) {
                $result.list.AddRange(@($item))
            }
            else {
                $result.add($item)
            }
        }
        $result.count = $result.list.count
        return $result
    }

    #T Function_SkipNullOrEmpty1  [1, null, 2, "", 3] |> skipnullorempty == [1, 2, 3]
    #T Function_SkipNullOrEmpty2  {a:1 b:2 c:3} |> skipnullorempty |> property 'key' == ['a', 'b', 'c']
    #T Function_SkipNullOrEmpty3  [] |> skipnullorempty == []
    #T Function_SkipNullOrEmpty4  null |> skipnullorempty == []
    #H Skip over nulls in a list producing new clean list.
    SkipNullOrEmpty = {
        param($list)

        $result = [TinyList]::new()
        if ($null -eq $list) { return $result }

        if ($list -is [TinyList]) {
            $list = $list.list
        }
        elseif ($list -isnot [IEnumerable]) {
            errorMessage ("The 'SkipNullOrEmpty' function requires a collection to work on, " +
                          "not an object of type $($list.GetType())")
        }

        if ($list -is [IDictionary]) {
            $list = $list.GetEnumerator()
        }

        :inner foreach ($item in $list) {
            if ($null -ne $item -and $item -ne "" ) {
                $result.add($item)
            }
        }

        $result.Count = $result.list.Count
        return $result
    }

    #T Function_Foreach1 s = 0; [1, 2, 3] |> foreach {s += it}; s == 6
    #T Function_ForEach2 s = 0; [<Diagnostics.Process>].getprocesses() |> foreach { s += it.ws }; s > 0
    #T Function_Foreach3 s = []; [1, 2, 3] |> foreach {s = [it, it2] :+ s }; s == [[3, 2], [2, 1], [1, 0]]
    #H Apply a function to each member of a list returning nothing<pre> ls() |> foreach { println( it.name ) } </pre>
    ForEach = {param ($list, [TinyLambda] $lambda)
        if ($null -eq $list) { return }

        if ($list -is [TinyList]) {
            $list = $list.list
        }
        elseif ($list -isnot [IEnumerable]) {
            errorMessage 'The ''foreach'' function requires a collection to work on.'
        }

        if ($list -is [IDictionary]) {
            $list = $list.GetEnumerator()
        }

        $index = 0
        :inner foreach ($i in $list) {
            [void] $lambda.Dot(@($i, $index), $i, $index, $null)
            $index++
        }
    }

    #T Function_Where1 [1..10] |> where {it % 2 == 0} == [2, 4, 6, 8, 10]
    #T Function_Where2 [<Diagnostics.Process>].getprocesses() |> where {it.ws > 1_000_000} |> getlength > 0
    #T Function_Where3 [1..10] |> where {it2 > 5} == [7, 8, 9, 10] # check that item index is in 'it2'
    #H Filter a list using a lambda e.g. <pre>[1 .. 10] |> where { it % 2 == 0 }
    Where = {param ( $list, [TinyLambda] $lambda)
        $result = [TinyList]::new()
        if ($null -eq $list) { return $result }

        if ($list -is [TinyList]) {
            $list = $list.list
        }
        elseif ($list -isnot [IEnumerable]) {
            errorMessage 'The ''where'' function requires a collection to work on.'
        }

        # Explicitly enumerate dictionaries.
        if ($list -is [IDictionary]) {
            $list = $list.GetEnumerator()
        }

        $result = [TinyList]::new()
        $index = 0
        :inner foreach ($item in $list) {
            if ($lambda.Dot(@($item, $index), $item, $index, $null)) {
                $result.List.Add($item)
            }
            $index++
        }

        $result.Count = $result.list.Count
        return $result
    }

    #T Function_Distinct [1, 1, 2, 2, 3, 3] |> distinct() == [1, 2, 3]
    #H Return only the unique elements in a list
    Distinct = {param([TinyList] $list) $list.Distinct() }

    #T Function_ToHash1 h = tohash(['a', 'a', 'b', 'c']); keys(h).sort() == ['a', 'b', 'c'] && h['a'] == 2
    #T TinyList_ToHash2 ['abc', 'def', 'ghij', 'kl'] |> ToHash 'length' |> Count == 3
    #T TinyList_ToHash3 (['abc', 'def', 'ghij', 'kl'] |> ToHash {it.length})[3] == ['abc', 'def']
    #H Convert a list into a hash table indexed by item, named property or lambda calculation value with a count (basic) or list of items (property or lambda) as the value e.g. <pre> getrandom(100) |> tohash() </pre> <pre>shell('gps') |> ToHash { it.ProcessName } </pre>
    ToHash = {
        param ([TinyList] $list, $propertyOrLambda = $null)
        if ($null -ne $propertyOrLambda) {
            $list.ToHash($propertyOrLambda)
        }
        else {
            $list.ToHash()
        }
    }

    #T Function_Reduce [1, 2, 3] |> reduce { it + it2 } == 6
    #H A function to reduce/fold/aggregate a list using a lambda (this function calls the Reduce() method) <pre>[1..10] |> reduce{it + it2} == 55</pre>
    Reduce = {param ([TinyList] $list, [TinyLambda] $lambda)
        $list.Reduce($lambda)
    }

    #T Function_ReduceWithSeed [1, 2, 3] |> ReduceWithSeed (4, { it + it2 }) == 10
    #H A function to reduce/fold/aggregate a list using a lambda, starting with an initial seed value. (This function calls the ReduceWithSeed() method) <pre>[1..10] |> ReduceWithSeed(45, {it + it2}) == 100</pre>
    ReduceWithSeed = {param ([TinyList] $list, [object] $seed, [TinyLambda] $lambda)
        $list.ReduceWithSeed($seed, $lambda)
    }

    #T Function_Skip1  [1, 2, 3] |> skip(0) == [1, 2, 3]
    #T Function_Skip2  [1, 2, 3] |> skip(1) == [2, 3]
    #T Function_Skip3  [1, 2, 3] |> skip(2) == [3]
    #T Function_Skip4  [1, 2, 3] |> skip(4) == []
    #H Skip n items in the list, returning a new list
    Skip = {param ([TinyList] $list, $numToSkip) $list.Skip($numToSkip) }

    #T Function_Take1 [1..10].take(3) == [1, 2, 3]
    #T Function_Take2 [1, 2].Take(3) == [1, 2]
    #T Function_Take3 [1].Take(3) == [1]
    #T Function_Take4 [].Take(3) == []
    #H Take the first n items from the list
    Take = {param ([TinyList] $list, [int] $numToTake) $list.Take($numToTake) }

    #T Function_Property {foo: 13} |> property("foo") == [13]
    #H Extract a specific property from each member in a list e.g. <pre> ls() |> property('name') </pre>
    Property = {param ([TinyList] $list, [string] $Property)
        $result = foreach ($i in $list.list) {
            $i.$Property
        }
        return [TinyList]::new($result)
    }

    #T Function_Join1 [1, 2, 3] |> join() == '123'
    #T Function_Join2 [1, 2, 3] |> join('+') == '1+2+3'
    #H Join a list into a string e.g. <pre>join(['1', '2', '3'], '+')</pre> results in <pre>'a+b+c'</pre>
    Join = {param ([TinyList] $list, $sep = "") if ($list) {$list.List -join $sep} else {''}}

    #T Function_Zip1 zip([1,2,3],   [4,5,6])             == [[1,4], [2,5], [3,6]]
    #T Function_Zip2 zip([1,2,3,4], [4,5,6])             == [[1,4], [2,5], [3,6]]
    #T Function_Zip3 zip([1,2,3],   [4,5,6,7])           == [[1,4], [2,5], [3,6]]
    #T Function_Zip4 zip([1,2,3],   [4,5,6], {it+it2})   == [5,7,9]
    #T Function_Zip5 zip([1,2,3,4], [4,5,6], {it+it2})   == [5,7,9]
    #T Function_Zip6 zip([1,2,3],   [4,5,6,7], {it+it2}) == [5,7,9]
    #H Join two lists together, either in pairs or using an optional lambda e.g. <pre>zip([1,2,3], [4,5,6])</pre>results in<pre>[[1,4], [2,5], [3,6]]</pre>and<pre>zip([1,2,3], [4,5,6], {it + it2})</pre>results in<pre>[5, 7, 9]</pre>
    Zip = { param($list1, $list2, $lambda)
        if ($list1 -is [TinyList]) { $list1 = $list1.list }
        if ($list2 -is [TinyList]) { $list2 = $list2.list }

        if ($lambda) {
            $result = [linq.enumerable]::Zip(
                    [ienumerable[object]] $list1,
                    [ienumerable[object]] $list2,
                    [Func[object,object,object]] {
                        param($x, $y)
                        $lambda.Dot(@($x, $y), $x, $y, $null)
                    }
                )
        }
        else {
            $result = [linq.enumerable]::Zip(
                    [ienumerable[object]] $list1,
                    [ienumerable[object]] $list2,
                    [Func[object,object,object]] {
                        param($x, $y)
                        $element = [TinyList]::New()
                        $element.Add($x)
                        $element.Add($y)
                        $element
                    }
                )
        }
        return [TinyList]::new(@($result))

        # In general, the LINQ zip is preferred for performance
        # but it only zips the minimum number of elements. To zip
        # the max number of elements, uncomment the code below

<#
        $e1 = $list1.GetEnumerator()
        $e2 = $list2.GetEnumerator()
        $result = [TinyList]::New()
        $continue = $true
        while ($continue) {
            $continue = $false
            $pair = [TinyList]::New()
            if ($e1.MoveNext())
            {
                $pair.list.Add($e1.Current)
                $continue = $true
            }
            else {
                $pair.list.Add($null)
            }
            if ($e2.MoveNext())
            {
                $pair.list.Add($e2.Current)
                $continue = $true
            }
            else {
                $pair.list.Add($null)
            }
            if ($continue)
            {
                $result.list.Add($pair)
            }
        }
        $result
#>

    }

    #T Function_Union1 [1,2,3,4,5] |> Union([3,4,5,6,7]) == [1,2,3,4,5,6,7]
    #T Function_Union2 [1,2,3] |> Union([5,6,7]) == [1,2,3,5,6,7]
    #H Return the union of two lists e.g. <pre>[1,2,3,4] |> Union([3,4,5,6]) == [1,2,3,4,5,6]</pre>
    Union = {param ($firstList, $secondList)
        if ($firstList -is [TinyList]) {
            $firstList = $firstList.List
        }

        if ($secondList -is [TinyList]) {
            $secondList = $secondList.List
        }

        return [TinyList]::new([Linq.Enumerable]::Union($firstlist, $secondList))
    }

    #T Function_Except1 [1..10] |> Except([4,5,6,7]) == [1,2,3,8,9,10]
    #T Function_Except2 [<parser>].getmethods().AsList() |> except([<type>].GetMethods()) |> count == 32
    #H Returns the items in the first list that don't appear in the second list e.g. <pre>[1..10].Except([4,5,6])</pre>
    Except = { param ($firstList, $secondList)
        if ($firstList -is [TinyList]) {
            $firstList = $firstList.List
        }
        else {
            $firstList = @($firstList)
        }

        if ($secondList -is [TinyList]) {
            $secondList = $secondList.List
        }
        else {
            $secondList = @($secondList)
        }

        return [TinyList]::new([Linq.Enumerable]::Except([IEnumerable[object]]$firstList, [IEnumerable[object]] $secondList))
    }

    #T Function_All1 [1,2,3] |> All
    #T Function_All2 ([1, false, 3] |> All) == false
    #T Function_All3 [2,4,6] |> All {it % 2 == 0}
    #T Function_All4 ([3,4,6] |> All{it % 2 == 0}) == false
    #H Returns true if the result of evaluating a lambda on all of the items in the list returns true. The item is passed as 'it' and the index is passed as 'it2'  e.g. <pre>[1,2,3] |> all{it > 0}</pre>
    All = {param ($list, [TinyLambda] $lambda = $null)
        if ($List -is [TinyList]) {
            $List = $List.List
        }

        $cnt = $list.Count
        if ($cnt -eq 0) { return $false }
        for ($i=0; $i -lt $cnt; $i++) {
            $val = $list[$i]
            # Pass the value as 'it' and the index as 'it2'.
            if ($lambda) {
                if (-not $lambda.Dot(@($val), $val, $i, $null)) {
                    return $false
                }
            }
            else {
                if (-not $val) {
                    return $false
                }
            }
        }

        return $true
    }

    #T Function_Any1 [0, 0, 1, 0] |> Any()
    #T Function_Any2 [0, 0, 0, 0] |> Any() == false
    #T Function_Any3 [5, 6 , 1, 7] |> Any{it % 2 == 0}
    #T Function_Any4 [5, 3 , 1, 7] |> Any{it % 2 == 0} == false
    #T Function_Any5 [] |> Any{it % 2 == 0} == false
    #H Returns true if the result of evaluating a lambda on any of the items in the list returns true. The item is passed as 'it' and the index is passed as 'it2'  e.g. <pre>[1,2,3] |> all{it > 0}</pre>
    Any = { param ($list, [TinyLambda] $lambda = $null)
        if ($List -is [TinyList]) {
            $List = $List.List
        }

        $cnt = $list.Count
        if ($cnt -eq 0) { return $false }
        for ($i=0; $i -lt $cnt; $i++) {
            $val = $list[$i]
            # Pass the value as it and the index as it2.
            if ($lambda) {
                if ([bool] $lambda.Dot(@($val), $val, $i, $null)) {
                    return $true
                }
            }
            else {
                if ([bool] $val) {
                    return $true
                }
            }
        }

        return $false
    }

    #T Function_Replace1 replace(['abc', 'cba'], r/a/) == ['bc', 'cb']
    #T Function_Replace2 ['abc', 'cba'] |> replace(r/a/) == ['bc', 'cb']
    #T Function_Replace3 replace(['abc', 'cba'], r/a/, 'X') == ['Xbc', 'cbX']
    #T Function_Replace4 ['abc', 'cba'] |> replace(r/a/, 'X') == ['Xbc', 'cbX']
    #H Replace elements in a string or array of strings with an (optional) replacement string e.g. <pre>replace('abc', 'a', 'A')</pre> results in <pre>'Abc'</pre>and<pre>['abc', 'adf'] |> replace('a', 'A') == ['Abc', 'Adf']</pre>
    Replace = {param ($valToReplace, [string] $pattern, [string] $replace = "")
        if ($valToReplace -is [TinyList]) {
            $valToReplace = $valToReplace.List
        }
        if ($valToReplace -is [IList]) {
            $result = [TinyList]::new()
            foreach ($val in $valToReplace) {
                $result.list.Add(($val -replace $pattern, $replace))
            }
            $result.count = $result.List.Count
            $result
        }
        else {
            $valToReplace  -replace $pattern, $replace
        }
    }

    #t Function_Reverse1 [1..10] |> reverse == [10..1]
    #t Function_Reverse2 'abcd' |> reverse == 'dcba'
    #H Returns the argument collection or string reversed. e.g. <pre>[1,2,3,4] |> reverse == [4,3,2,1]</pre><pre>'abcd' |> reverse == 'dcba'</pre>
    Reverse = {param($listOrString)

        if ($listOrString -is [string]) {
            $arr = $listOrString.ToCharArray()
            [Array]::Reverse($arr)
            -join $arr
        }
        else {
            if ($listOrString -isnot [TinyList]) {
                $listOrString = [TinyList]::new($listOrString)
            }

            $listOrString.Reverse()
        }
    }

    #T Function_Eval1 eval('2+3*4') == 14
    #H Evaluate a string as a Tiny script. The resulting code is executed in the current scope e.g. <pre>eval('2+3')</pre> results in 5.The following sets the value of x in the current scope.<pre>eval 'x=13' ; x == 13</pre>
    Eval = [Tiny]::EvalExpr

    #T Function_Parse1 parse('2+2') is [<StatementList>]
    #T Function_Parse2 parse('3 +4').Statements[0] is [<binop>]
    #T Function_Parse3 parse('3 + 4').Statements[0].Left.Value == 3
    #H Parse text into a Tiny expression tree e.g. <pre>parse('2 + 3 * 4')</pre>
    Parse = [Tiny]::Parse

    #T Function_ParseFile1 parsefile('tinytest.tiny') is [<Expression>]
    #H Read and parse a file into a Tiny expression tree. e.g. <pre>parsefile('hello.tiny')</pre>
    ParseFile = {param ($file)
        [scopes]::PushScope()
        [Tiny]::ParseFile($file)
        [scopes]::PopScope()
    }

    #T Function_Regex1 regex('abc') is [<regex>]
    #T Function_Regex2 regex('abc', 'IgnoreCase') is [<regex>]
    #T Function_Regex3 regex('abc', 'IgnoreCase').match('ABC').success == true
    #T Function_Regex4 regex('abc', 'none').match('ABC').success == false
    #T Function_Regex5 'Abc' ~ regex('abc', 'IgnoreCase') == true
    #T Function_Regex6 'ABC' ~ regex('abc', 0) == false
    #H Convert a string to a regular expression, possibly with options ([<RegexOption>]).
    Regex = { param( [string] $patternString, [RegexOptions] $options = 'CultureInvariant,Ignorecase')
        [regex]::new($patternString, $options)
    }

    #T Function_Sort1 sort([3, 1, 5, 2, 4]) == [1,2,3,4,5]
    #T Function_Sort2 [3, 1, 5, 2, 4] |> sort() == [1,2,3,4,5]
    #T Function_Sort3 [3, 1, 5, 2, 4] |> sort {n -> minus(n) } == [5..1]
    #T Function_Sort4 [{a:2}, {a:3}, {a:1}] |> sort('a') |> map { it.a }  == [1, 2, 3]
    #H Sort a list, optionally specifing a lambda or property name e.g. <pre>list(shell('gps'), 'WS')</pre> Also see the methods section<pre>list.sort()</pre> or <pre>list.sort("propname")<pre> or <pre>shell('gps').Sort{it.WS}</pre>
    Sort = {param ($list, $PropertyOrLambda = $null)
        if ($list -is [TinyList]) {
            if ($PropertyOrLambda -is [TinyLambda]) {
                return $list.Sort($PropertyOrLambda)
            }
            elseif ($null -ne $PropertyOrLambda) {
                #BUGBUGBUG there is a bug in PowerShell where 'Sort-Object foo' won't sort hashtables
                #BUGBUGBUG with member 'foo' but the lambda variant does work.
                return [TinyList]::new(@($list.list | Sort-Object {$_.$PropertyOrLambda}))
            }
            else {
                return $list.Sort()
            }
        }
        elseif ($list -is [System.Collections.IDictionary]) {
            if ($PropertyOrLambda -is [TinyLambda]) {
                return [TinyList]::New(@($list.GetEnumerator())).Sort($PropertyOrLambda)
            }
            elseif ($null -ne $PropertyOrLambda) {
                return [TinyList]::new(@($list.GetEnumerator() | Sort-Object {$_.$PropertyOrLambda}))
            }
            else {
                return[TinyList]::new($list.GetEnumerator()).Sort()
            }
        }
        else {
            if ($PropertyOrLambda -is [TinyLambda]) {
                [TinyList]::New(@($list)).sort($PropertyOrLambda)
            }
            elseif ($null -ne $PropertyOrLambda) {
                [TinyList]::new(@($list | Sort-Object {$_.$PropertyOrLambda}))
            }
            else {
                [TinyList]::new(@($list)).Sort()
            }
        }
    }

    #T Function_SortDescending1 sortdescending([3, 1, 5, 2, 4]) == [5..1]
    #T Function_SortDescending2 [3, 1, 5, 2, 4] |> sortdescending() == [5..1]
    #T Function_SortDescending3 [3, 1, 5, 2, 4] |> sortdescending {n -> minus(n) } == [1..5]
    #T Function_SortDescending4 [{a:2}, {a:3}, {a:1}] |> sortdescending('a') |> map { it.a }  == [3, 2, 1]
    #H Sort a list in descending order, optionally specifing a lambda or property name e.g. <pre>list(shell('gps'), 'WS')</pre> See also the methods <pre>list.sort()</pre> or <pre>list.sort("propname")<pre> or <pre>shell('gps').Sort{it.WS}</pre>
    SortDescending = {param ($list, $PropertyOrLambda)
        if ($list -is [TinyList]) {
            if ($PropertyOrLambda -is[tinyLambda]) {
                $list.SortDescending($PropertyOrLambda)
            }
            elseif ($null -ne $PropertyOrLambda) {
                [TinyList]::new(@($list.list | Sort-Object -Descending {$_.$PropertyOrLambda}))
            }
            else {
                [TinyList]::new(@($list.list | Sort-Object -Descending))
            }
        }
        elseif ($list -is [System.Collections.IDictionary]) {
            if ($PropertyOrLambda -is [TinyLambda]) {
                [tinyList]::New(@($list.GetEnumerator())).SortDescending($PropertyOrLambda)
            }
            elseif ($null -ne $PropertyOrLambda) {
                [TinyList]::new(@($list.GetEnumerator() | Sort-Object -Descending {$_.$PropertyOrLambda}))
            }
            else {
                [TinyList]::new(@($list.GetEnumerator() | Sort-Object -Descending))
            }
        }
        else {
            if ($PropertyOrLambda -is [TinyLambda]) {
                [TinyList]::New(@($list)).sortDescending($PropertyOrLambda)
            }
            elseif ($null -ne $PropertyOrLambda) {
                [TinyList]::new(@($list | Sort-Object -Descending {$_.$PropertyOrLambda}))
            }
            else {
                [TinyList]::new(@($list | Sort-Object -Descending))
            }
        }
    }

    # Function_Hash1 newhash('a', 1, 'b', 2) is [<IDictionary>]
    # Function_Hash2 newhash('a', 1, 'b', 2) |> keys() == ['a', 'b']
    # Function_Hash3 newhash('a', 1, 'b', 2) |> Values() == [1, 2]
    #H Varargs function that builds a hash table out of it's arguments e.g. <pre>hash(k1, v2, k2, v2, k3, v3)</pre>
    NewHash = {
        $h = [ordered] @{}
        while ($args) {
            $k, $v, $args = $args
            if ($null -ne $k) {
                $h[$k] =$v
            }
            else {
                errorMessage "Hash: Null key encountered creating hash."
            }
        }
        $h
    }

    #T Function_New1 new([<hashtable>]) is [<hashtable>]
    #H Varargs create a new instance of an object. e.g. <pre>New([<SomeType>], 1, 2, 3)</pre>
    New = {
        $type, $arg1, $arg2, $arg3, $arg4 = $args
        switch ($args.Count) {
            1 { $type::new() }
            2 { $type::new($arg1) }
            3 { $type::new($arg1, $arg2) }
            4 { $type::new($arg1, $arg2, $arg3) }
            5 { $type::new($arg1, $arg2, $arg3, $arg4) }
        }
    }

    #T Function_Keys1 keys({a:1 b:2 c:3}) == ['a', 'b', 'c']
    #T Function_Keys2 {a:1 b:2 c:3} |> keys |> sort == ['a', 'b', 'c']
    #T Function_Keys3 {a:1 b:2 Keys:3} |> keys |> sort == ['a', 'b', 'Keys']
    #H Return all the keys from a dictionary e.g. <pre>keys(hashtable)</pre>
    Keys = {param([IDictionary] $dictionary)
        if ($null -ne $dictionary) {[TinyList]::new(@($dictionary.get_Keys()))}
    }

    #T Function_Values1 values({a:1 b:2 c:3}) == [1, 2, 3]
    #T Function_Values2 {a:1 b:2 c:3} |> values |> sort == [1, 2, 3]
    #T Function_Values3 {a:1 b:2 Values:3} |> values |> sort == [1, 2, 3]
    #H Return all the values from a dictionary e.g. <pre>values(hashtable)</pre>
    Values = {param([IDictionary] $dictionary)
        if ($null -ne $dictionary) { [TinyList]::new(@($dictionary.get_Values()))}
    }

    #T Function_HashContains1 {a:1} |> hashcontains('a')
    #T Function_HashContains2 {a:1} |> hashcontains('x') == false
    #H Returns true if the dictionary contains a specified key (or use the :> and <: operators instead) e.g. HashContains(hashtable, 'key')</pre>
    HashContains = {param([IDictionary] $dictionary, $key)
        if ($null -ne $dictionary) {$dictionary.Contains($key)} else {$false}
    }

    #T Function_Readfile1 readfile('tiny.ps1') |> count > 1000
    # THis test counts the number of classes defined in 'tiny.ps1'. If that changes, then the test needs to be updated.
    #T Function_Readfile2 readfile('tiny.ps1', '^ *class') |> count == 53
    #T Function_Readfile3 readfile('tiny.ps1', '^ *class', { it.split(r/ /)[1] }) |> count == 53
    #H Read the lines of a file or files into a list with an optional regex filter and an optional action (lambda) to take for each line. If more than one file is specified, the lines are concatenated together. Within the lambda, the filename is available as 'filename' and the regex matches are available as the match specification of the regex (matches by default). Also, when a lambda is specified, the file is read line by line rather than all at once. e.g. <pre>ReadFile('foo.tiny', 'fn +([a-z_0-9]+)')</pre><pre>ls '*.tiny' {recurse:true} |> readfile r/^ *fn (\w*)/:name { name[1] }</pre>
    ReadFile = {param ($filenames, $filter = '', [TinyLambda] $lambda)
        if (-not $filenames) {
            errorMessage "ReadFile: parameter 'filenames' should not be null or empty"
        }

        if ($filenames -is [TinyList]) {
            $filenames = $filenames.list
        }
 
        if ($filter -is [TinyLambda]) {
            $lambda = $filter
            $filter = ''
        }

        if ($lambda) {
            # For lambdas, we always use RegexLiteral so the match variable is properly set.
            if ($filter -isnot [RegexLiteral]) {
                $filter = [RegexLiteral]::new($null, 'matches', $filter)
            }
        }
        else {
            # if no lambda was specified, then just use a regex
            if ($filter -is [RegexLiteral]) {
                $filter = $filter.Value
            }
            else {
                $filter = [regex] $filter
            }
        }

        $allresults = foreach( $filename in $filenames) {
            if($filename -is [IO.FileSystemInfo]) {
                $filename = $filename.fullname
            }

            if (Test-Path $filename) {
                $filename = Resolve-Path $filename

                # If there's a lambda, do explicit iteration so that the ambient variables
                # get set properly. The 'it' variable holds the line, 'it2' holds the line
                # index and the match variable is set per the RegexLiteral. We'll also use a stream reader
                # so we don't have to load the entire file into memory.
                if ($lambda) {
                    $index = 0
                    [scopes]::scopes[0]["filename"] = $filename
                    $stream = [Io.File]::OpenText($filename)
                    try {
                        while ($null -ne ($line = $stream.ReadLine())) {
                            if ($filter.Match($line)) {
                                $lambda.Dot(@($line, $index), $line, $index, $null)
                            }
                            $index++
                        }
                    }
                    finally {
                        $stream.Close()
                    }
                }
                else {
                    if ($null -ne $filter) {
                        [io.file]::ReadAllLines($filename) -match $filter
                    }
                    else {
                        [io.file]::ReadAllLines($filename)
                    }
                }
            }
            else {
                errorMessage "Readfile: file name '$filename' not found."
            }
        }
        [TinyList]::New($allresults)
    }

    #H Return the contents of a file as a single string. If multiple files are specified, they are concatenated together e.g. <pre>ReadText('foo.tiny')</pre>
    ReadText = {param ($filenames)
        if ($filenames -eq $null -or $filenames -eq "") {
            errorMessage "ReadFile: parameter 'filenames' should not be null or empty."
        }

        if ($filenames -is [TinyList]) {
            $filenames = $filenames.list
        }

        $allresults = foreach( $filename in $filenames) {
            if (Test-Path $filename) {
                $filename = Resolve-Path $filename
                [io.file]::ReadAllText($filename)
            }
            else {
                errorMessage "ReadText: file name '$filename' not found"
            }
        }
        -join $allResults
    }

    #H Write an array of strings to a file <pre>WriteFile([1, 2, 3, 4, 5], 'output.txt')</pre><pre> [1 .. 10 ] |> writefile 'output.txt' </pre>
    WriteFile = {
        [CmdletBinding()]
        param ( [Parameter(Mandatory)] $lines, [Parameter(Mandatory)] $fileName, [Parameter()] $Encoding = [System.Text.Encoding]::Default)

        try {
            if ($lines -is [TinyList]) {
                $lines = $lines.list
            }
            [System.IO.File]::WriteAllLines($filename, [string[]] $lines, $Encoding)
        }
        catch {
            errorMessage "WriteFile: writing '$filename': $($_.Exception.Message)"
        }
    }

    #H Write a string to a file e.g. <pre>writetext('output.txt', 'The text to write.')</pre>
    WriteText = {param ([string] $text, $fileName)
        try {
            [io.file]::WriteAllText($filename, $text)
        }
        catch {
            errorMessage "WriteText: writing '$filename': $($_.Exception.Message)"
        }
    }

    #H Read a line from the console with an optional prompt e.g. <pre>name = readfile('Enter your name:')</pre>
    ReadLine = {param ([string] $prompt)
        if ($prompt) {
            [console]::Write($prompt)
        }
        [console]::readline()
    }

    #H Read a keystroke from the console e.g. <pre>key = readkey()</pre>
    ReadKey = [console]::ReadKey

    #h A function to get the CommandInfo for a PowerShell command.
    GetCommand = Get-Command Get-Command

    #T Function_GetRandom1 GetRandom()  is [<int>]
    #T Function_GetRandom2 GetRandom(1) is [<int>]
    #T Function_GetRandom3 GetRandom(0) is [<int>]
    #T Function_GetRandom4 getrandom(1, 0, 100) <: [0 .. 100]
    #T Function_GetRandom5 getrandom(1, 50, 100) <: [50 .. 100]
    #T Function_GetRandom6 IsList(GetRandom(10)) 
    #T Function_GetRandom7 getrandom(10).count == 10
    #T Function_GetRandom8 10 |> getrandom |> count == 10
    #T Function_GetRandom9 getrandom(30, 1, 20).ReduceWithSeed(true, {it && it2 <: [1..20]})
    #T Function_GetRandom  getrandom(100, 1, 10) |> distinct |> sort == [1..10]
    #H Get a random number or numbers. If no arg is specified, returns a single number, arg n will give you n numbers between 1 and n e.g. <pre>GetGandom(100)</pre> will return 100 random number between 0 and 100.<pre>GetRandom(10, 1, 100)</pre>will give you 10 numbers between 1 and 100.
    GetRandom = {param([int] $number = 0, [int] $minmax = 0, [int] $max = 0)
        if ($number -lt 0) {
            errorMessage 'getRandom: the number of values to return must be greater than 0'
        }

        if (-not $max) {
            # if min/max is not specified the range is 1 to the number specified.
            if (-not $minmax) {
                $minmax = 1
                $max = $number
            }
            else {
                $max = $minmax
                $minmax = 1
            }
        }

        # There seems to be a bug in Get-Random such that it never resturns max
        # thus we increment max to get the full range specified.
        $max += 1

        if ($number -eq 1) {
            Get-Random -Min $MinMax -Max $max
        }
        elseif ($number -gt 1) {
            [TinyList]::new($(foreach ($n in 1..$number) {Get-Random -Min $MinMax -Max $max}))
        }
        else {
            Get-Random
        }
    }

    #h Print help for the Tiny built-in functions
    Functions = { param ($pattern = '')

        $scriptText = Get-Content (Join-Path $psscriptroot 'tiny.ps1')
        $text = switch -regex ($scriptText) {
            '^ +#h' {
                $entry = $_ -replace '^.*#h *' -replace 'e\.g\..*$' -replace '\<pre.*$'
                [void] $switch.MoveNext()
                $function = ($switch.Current -split " *= *")[0].trim()
                $def = $FunctionTable[$function]
                if ($def -is [ScriptBlock]) {
                    $def = $def.Ast.ParamBlock
                    if ($def) {
                        $def = ('(' + ($def.Parameters.Name -join ', ') + ')') -replace '\$'
                    }
                    else {
                        $def = "()"
                    }
                    $function += $def
                }
                else {
                    $function = $function.ToString()
                }
                $function.ToString().padright(25) + ' : ' + $entry
            }
        }
        [TinyList]::new(($text -match $pattern | Sort-Object))
    }

    #H Return all of the defined constants.
    Constants = { param ($pattern = '')
        $result = @{}
        foreach ($pair in $script:TinyConstants.GetEnumerator()) {
            if ($pair.key -match $pattern) {
                $result[$pair.key] = $pair.value
            }
        }
        $result
    }

    #h Print help for the Tiny built-in operators.
    Operators = { param ($pattern = '')

        $scriptText = Get-Content (Join-Path $psscriptroot 'tiny.ps1')
        $helpText = ($scriptText -match '^ +#O' -replace '^.*#O *', '').foreach{
                        if ($_ -match '([^ ]+) +(.*$)') { $matches[1].PadRight(5) + ' : ' + $matches[2] }
                    } -replace '(\<pre|e\.g\.).*$' -match $pattern 

        $helpText
    }

    #T Function_Vars1  vars() |> matchall '^args$' == ['args']
    #T Function_Vars2  vars r/^args/ == ['args']
    #T Function_Vars3  vars('^bod')  == ['body']
    #T Function_Vars4  vars r/^it2$/ == ['it2']
    #T Function_Vars5  vars r/^doesnotexist$/ == []
    #h Return the names of all currently defined variables, filtered by an optional regular expression e.g. <pre>vars r/void/ == ['void']</pre>
    Vars = {param ($filter = '')
        if ($filter -is [RegexLiteral]) {
            $filter = $filter.Value
        }
        elseif ($filter -isnot [Regex]) {
            $options = 'CultureInvariant,Ignorecase'
            $filter = [regex]::new($filter, $options)
        }

        $result = [scopes]::Vars()
        if ($null -ne $result -and $result.Count -gt 0) {
            $result = $result.MatchAll($filter)
        }
        return $result
    }

    #T Function_VarExists1 x = 1; varexists('x') == true
    #T Function_VarExists2 varexists('doesnotexist') == false
    #H Returns true if a variable exists e.g. <pre>x = 1</pre><pre>varexists('x')</pre>
    VarExists = {param ($name)
        if (-not $name) {
            errorMessage "VarExists: parameter 'name' can't be null or empty" 
        }
        [scopes]::VarExists($name)
    }

    #T Function_VarRemove __x__ = 1; varremove('__x__'); not(varexists('__x__'))
    #H Removes a variable from the scope stack. e.g. <pre>VarRemove('foo')</pre><pre>VarExists('foo') then VarRemove('foo')</pre>
    VarRemove = {param ($name)
        if (-not $name) {
            errorMessage "VarRemove: parameter 'name' can't be null or empty" 
        }
        [scopes]::VarRemove($name)
    }

    #T FunctionVarSet1 x = 0; varset('x', 3.14); x == 3.14
    #T FunctionVarSet2 x = 'hi'; varset('x'); x == null
    #H Sets the named Tiny variable. e,g, <pre>VarSet('foo', 13)</pre>
    VarSet = {param ($name, $value)
        if (-not $name) {
            errorMessage "VarSet: parameter 'name' can't be null or empty" 
        }
        [Scopes]::SetVariable($name, $value)
    }

    #T Function_Env1 env().PATH != null # path is case-sensitive PATH on non-Windows systems
    #T Function_Env2 env('PATH') != null
    #T Function_Env3 env('_foo_', 123); env('_foo_') == 123   # create an env var 'foo'
    #T Function_Env4 env('_foo_', null); env('_foo_') == null # delete it`
    #T Function_Env5 keys(env()) |> matchall 'foo' == []      # verify the deletion
    #H The env() function does a number of things: gets a hastable of all of the environment variables; gets a specific environment variable or sets an environment variable. Setting a variable to null deletes the variable. (Note: on non-Windows systems, the environment is case-sensitive) e.g. <pre>env().Path</pre><pre>env('PATH')</pre><pre>env('FOO', 13, 'Process')</pre><pre>env('FOO', null)</pre>
    Env = {
        [CmdletBinding()]
        param ($Var, $ValToSet, $Scope = 'Process')

        if ($PSBoundParameters.ContainsKey("Var") -eq $false) {
            if ($IsWindows) {
                # On Windows, copy to a case-insensitive hashtable before returning the table
                @{} + [environment]::GetEnvironmentVariables()
            }
            else {
                [environment]::GetEnvironmentVariables()
            }
        }
        else {
            if ($PSBoundParameters.ContainsKey("ValToSet")) {
                # Setting an environment variable to null removes it
                [environment]::SetEnvironmentVariable($var, $valToSet, $scope)
            }
            else {
                [environment]::GetEnvironmentVariable($var)
            }
        }
    }

    #h Turns on the underlying PowerShell script stack trace for debugging the Tiny interpreter e.g. <pre>DetailedErrors(true)</pre>
    DetailedErrors = {
        param ([bool] $flag)
        [Tiny]::DetailedErrors = $flag
    }

    #H Display a detailed dump of the last error.
    DumpError = {
        $err = [scopes]::GetVariable('TinyError')
        Write-Host -Foreground green "Error dump:"
        $err | Format-List -force * | Out-Host
        $err.ErrorRecord | Format-List -force * | Out-Host
        $err.Exception | Format-List -force * | Out-Host
    }

    #T Function_Call1 call {2+2} == 4
    #T Function_Call2 {7 - 3 * 2} |> call() == 1 # test with piping
    #T Function_Call3 call({x, y -> x+y}, 2, 3) == 5  # test with arguments
    #T Function_Call4 {x, y -> x+y} |> call(2, 3) == 5  # test with arguments when piping
    #H Run a script or lambda with optional arguments eg: <pre>run("./hello.tiny")</pre> or <pre>run({it + it2}, 2, 3)</pre><pre>call 'tinytest' {external:false pattern:'function'}</pre>
    Call = {
        $codeToRun, $arguments = $args
        $arguments = @($arguments)
        :return_from_function do {
            if ($codeToRun -is [string]) {
                [scopes]::PushScope()
                [scopes]::scopes[0]["args"] = [TinyList]::New($arguments)
                [scopes]::scopes[0]["__file"] = $codeToRun
                try {
                    $expr = [Tiny]::parseFile($codeToRun)
                    return $expr.Eval()
                }
                finally {
                    [void] [scopes]::PopScope()
                }
            }
            elseif ($codeToRun -is [TinyLambda]) {
                return $codeToRun.Invoke($arguments,
                    [AutomationNull]::Value,
                    [AutomationNull]::Value,
                    $null)
            }
            else {
                errorMessage "Call: the 'call' function can only run scripts or lambdas"
            }
        }
        while ($false)
        return $script:FunctionReturnValue
    }

    #T Function_Dot1 Dot{x = 3.14} ; x == 3.14
    #T Function_Dot2 Dot({n -> x = n}, 'abc')  ; x == 'abc'
    #T Function_Dot3 {x = 3.14} |> dot; x == 3.14
    #T Function_Dot4 {n -> x = n} |> dot('abcde') ; x == 'abcde'
    #H Execute a lambda or script in the current scope. e.g. <pre>dot { a = 13 }; a == 13</pre><pre>dot 'tinytest' {external:false}</pre>
    Dot = {
        $codeToRun, [array] $arguments = $args
        :return_from_function do {
            if ($codeToRun -is [string]) {
                $oldArgs   =  [scopes]::scopes[0]["args"]
                $old__File =  [scopes]::scopes[0]["__file"]
                [scopes]::scopes[0]["args"] = [TinyList]::New($arguments)
                [scopes]::scopes[0]["__file"] = $codeToRun
                try {
                    $expr = [Tiny]::ParseFile($codeToRun)
                    return $expr.Eval()
                }
                finally {
                    [scopes]::scopes[0]["args"] = $oldArgs
                    [scopes]::scopes[0]["__file"] = $old__File
                }
            }
            elseif ($codeToRun -is [TinyLambda]) {
                return $codeToRun.Dot($arguments,
                    [AutomationNull]::Value,
                    [AutomationNull]::Value,
                    $null)
            }
            else {
                errorMessage "Dot: the 'dot' function can only run scripts or lambdas"
            }
        }
        while ($false)
        return $script:FunctionReturnValue
    }

    #H This function is essentially the same as Call() but adds execution timing e.g. <pre>time { [1..10].sum() }</pre> or <pre>time('./scripttorun.tiny')</pre>
    Time = {
        $codeToRun, $arguments = $args
        $arguments = @($arguments)

        :return_from_function do {
            $sw = [system.diagnostics.Stopwatch]::new()
            if ($codeToRun -is [string]) {
                [scopes]::PushScope()
                [scopes]::scopes[0]["args"] = [TinyList]::New($arguments)
                [scopes]::scopes[0]["__file"] = $codeToRun
                try {
                    $sw.start()
                    $expr = [Tiny]::ParseFile($codeToRun)
                    return $expr.Eval()
                }
                finally {
                    [void] [scopes]::PopScope()
                    $sw.Stop()
                    Write-Host -fore green `
                        "Elapsed time for script '$codeToRun': $($sw.Elapsed.TotalMilliSeconds) ms."
                }
            }
            elseif ($codeToRun -is [TinyLambda]) {
                try {
                    $sw.start()
                    return $codeToRun.Invoke($arguments,
                        [AutomationNull]::Value,
                        [AutomationNull]::Value,
                        $null)
                }
                finally {
                    $sw.Stop()
                    Write-Host -fore green `
                        "Elapsed time for lambda: $($sw.Elapsed.TotalMilliSeconds) ms."
                }
            }
            else {
                errorMessage "Time: the 'time' function can only run scripts or lambdas"
            }
        } while ($false)
        return $script:FunctionReturnValue
    }

    #H Turn on function tracing; the argument is a regular expression used to match the names of functions to trace.
    TraceOn = {param ($pattern)
        if ($pattern -is [RegexLiteral]) {
            $pattern = $pattern.Value
        }
        [FunctionCall]::tracePattern = [regex] $pattern
    }

    #H Turn off function tracing
    TraceOff = {  [FunctionCall]::tracePattern = $null }

    #T Function_Ls1 ls('tiny.ps1').count == 1
    #T Function_Ls2 ls('*.tiny').count > 1
    #H Get the contents of the current directory.
    ls = {param($pattern = "*", $body = $null)
        if ($pattern -is [IDictionary]) {
            [TinyList]::New((Get-Childitem @pattern))
        }
        else {
            if ($null -ne $body) {
                [TinyList]::New((Get-ChildItem $pattern @body))
            }
            else {
                [TinyList]::New((Get-ChildItem $pattern))
            }
        }
    }

    #H Change the current directory.
    cd = { param($path) Set-Location $path
            # Synchronize the PowerShell and process paths to make using methods easier.
            Set-Location $path
            [environment]::CurrentDirectory = (Get-Location).path
    }

    #T Function_Pwd1 pwd() == shell('$pwd.path')[0]
    #H Get the current working directory.
    Pwd = {
        [environment]::CurrentDirectory = (Get-Location).path
        [environment]::CurrentDirectory
    }

    #H Remove a file e.g.<pre>RemoveFile('bar.txt')
    RemoveFile = Get-Command 'Remove-Item'

    #H Copy a file e.g. <pre>CopyFile( 'foo.txt', 'bar.txt')</pre>
    CopyFile = Get-Command 'Copy-Item'

    #T Function_GetDate1 getdate() is [<datetime>]
    #H Get the current date e.g. <pre>GetDate()</pre>
    GetDate = Get-Command Get-Date

    #T Function_Sleep1 100 |> sleep() ; true
    #H Sleep for a period of time; time is in ms; default is 100ms e.g. <pre>sleep(2000)</pre> sleeps 2 seconds.
    Sleep = {param($duration = 100) Start-Sleep -milliseconds $duration}

    #T Function_Shell1 shell('123') == [123]
    #T Function_Shell2 shell('Get-Date')[0] is [<datetime>]
    #T Function_Shell3 shell('pwd | foreach path')[0] == pwd()
    #T Function_Shell4 [3,1,2] |> shell('sort-object') == [1, 2, 3]
    #T Function_Shell5 [3,1,2] |> shell('sort-object -descending') == [3, 2, 1]
    #H Evaluate a PowerShell expression; the result is always an array e.g. <pre>shell('ls -filter *.tiny')</pre><pre>shell('Get-Process').sum {p -> p.WS}</pre>
    Shell = {
        if (-not $args) {
            errorMessage 'Shell: must specify a command string to run"'
        }
        $inputList, $cmdToRun, $null = $args
        if ($inputList -and $cmdToRun) {
            if ($inputList -is [TinyList]) {
                $inputList = $inputList.List
            }
            [tinylist]::new((Invoke-Expression "`$inputList |$cmdToRun "))

        }
        else {
            $cmdToRun, $null = $args
            [tinylist]::new((Invoke-Expression $cmdToRun))
        }
        [environment]::CurrentDirectory = (Get-Location).Path
    }

    #T Function_Sh1 sh 'Get-Command' {name: "Get-Date"} |> property 'Name' == ['Get-Date']
    #T Function_Sh2 sh 'Get-Childitem' {filter: '*.tiny'} |> sh 'Get-Content' |> count > 0
    #T Function_Sh3 [1..10] |> sh 'Measure-Object' {sum:true} |> property 'sum' == [55]
    #H Invoke a cmdlet by name passing parameters in a dictionary <pre>sh 'get-command' {name: "get-date"}</pre><pre>sh 'Get-ChildItem' {filter: '*.txt' recurse:true} |> sh 'Get-Content'</pre>
    sh = {

        if (-not $args) {
            errorMessage "sh: The name of the command to run is missing"
        }

        $pipelineInput = $null
        $argsDict = $null
        if ($args.Count -eq 3) {
            # Pipeline input case
            $pipelineInput, $commandToRun, [IDictionary] $argsDict = $args
        }
        elseif ($args.Count -eq 2) {
            if ($args[-1] -is [IDictionary]) {
                $commandToRun, $argsDict, $null = $args
            }
            else {
                $pipelineInput, $commandToRun, $null = $args
            }
        }
        else {
            $commandToRun, $null = $args
        }
        if ($pipelineInput -is [TinyList]) {
            $pipelineInput = $pipelineInput.List
        }
 
        if (-not $commandToRun) {
            errorMessage 'sh: must specify a command to run.'
        }

        $ci = Get-Command $commandToRun -ea SilentlyContinue
        if ($null -eq $ci) {
            errorMessage "sh: command '$commandToRun' was not found. Please check the name and try again."
        }

        if ($null -ne $pipelineInput) {
            if ($null -ne $argsDict) {
                $result = $pipelineInput | & $ci @argsDict
            }
            else {
                $result = $pipelineInput | & $commandToRun
            }
    
        }
        else {
            if ($null -ne $argsDict) {
                $result = & $ci @argsDict
            }
            else {
                $result = & $ci
            }
        }

        if ($result -is [IList]) {
            $result = [TinyList]::New($result)
        }
        return $result
    }

    #H Invoke a cmdlet with th option to explicitly pass in pipieline input TBD BUGBUGBUG
    shargs = {
        param ($cmd, $positionalArgs = @(), [IDictionary] $namedArgs = @{}, $pipelineInput = $null)
        if (-not $cmd) {
            errorMessage "shargs: The name of the command to run is missing"
        }

        if ($positionalArgs -is [TinyList]) {
            $positionalArgs = @($positionalArgs.List)
        }
        elseif ($positionalArgs -eq $null) {
            $positionalArgs = @()
        }

        if ($pipelineInput -is [TinyList]) {
            $pipelineInput = $pipelineInput.List
        }

        if ($null -eq $namedArgs) {
            $namedArgs = @{}
        }

        $ci = if ($cmd -isnot [CommandInfo]) {
            Get-Command $cmd -ea ignore
        }
        else {
            $cmd
        }

        if ($null -eq $ci) {
            errorMessage "sh: command '$cmd' was not found. Please check the name and try again."
        }

        if ($null -ne $pipelineInput) {
            if ($namedArgs) {
                $result = $pipelineInput | & $ci @positionalArgs @namedArgs
            }
            else {
                $result = $pipelineInput | & $ci @positionalArgs
            }
        }
        else {
            if ($namedArgs) {
                $result = & $ci @positionalArgs @namedArgs
            }
            else {
                $result = & $ci @positionalArgs
            }
        }

        if ($result -is [IList]) {
            $result = [TinyList]::New($result)
        }

        return $result
    }

    #T Function_ScriptBlock1 scriptblock('2 + 5') is [<ScriptBlock>]
    #T Function_ScriptBlock2 scriptblock('2 + 5').InvokeReturnAsIs() == 7
    #H Create a PowerShell scriptblock from a string. A scriptblock can be assigned to a variable and then used as a function.<pre>plus = ScriptBlock 'param ($x, $y) $x + $y'</pre><pre>plus(2, 3) == 5</pre>
    ScriptBlock = [scriptblock]::Create

    #H Launch the program associated with a file using ShellExecute e.g. <pre>start('./Tiny Documentation.html')</pre>
    Start = Get-Command 'Start-Process'

    #H Get the value of a PowerShell (as opposed to a Tiny) variable e.g. <pre>PwshVar('PSHome')</pre><pre>shellvar('PSHome') {scope:'global'}</pre>
    ShellVar = Get-Command 'Get-Variable'
}

# Used to hold the list of functions defined in the file being parsed.
# As the file is parsed, functions are added to this table
# Once the parse is complete, the collection is copied into the lambda.
[Dictionary[string,object]] $script:LocalFunctions = $null

# Create function "aliases" for 'run', 'count', 'take'and 'gvim'
$functionTable.Run   = $functionTable.Call
$functionTable.Count = $functionTable.GetLength
$functionTable.Take  = $functionTable.First

# Copy the scriptblock InvokeReturnAsIs methods into the TinyConstants table
foreach ($func in $functionTable.GetEnumerator()) {
    if ($func.Value -is [ScriptBlock]) {
        $script:tinyconstants[$func.key] = $func.value.InvokeReturnAsIs
    }
    else {
        $script:tinyconstants[$func.Key] = $func.Value
    }
}

# Make the function table visible to the Tiny runtime.
[scopes]::SetVariable("__FunctionTable", $script:FunctionTable)

# Make the operator table visible to the Tiny runtime
[scopes]::SetVariable("__OperatorTable", $script:OperatorTable)

#T TinyLambda_Test1 {it + it2}.invoke([], 2,3) == 5
#T TinyLambda_Test2 {x, y -> x+y}.invoke([5,7]) == 12
#T TinyLambda_Test3 {x, y -> x+y}(9, 11) == 20
# Watch for collisions with functions (in the function table) named 'foo'
#T TinyLambda_Test4 foo2 = {x, y -> x+y}; foo2(9, 11) == 20
#T TinyLambda_Test5 foo2 = {x, y -> 1; 2; 3; x+y}; foo2(9, 11) == 20
#T TinyLambda_Test6 5 |> {n -> n * n } == 25
#T TinyLambda_Test7 6 |> {it * it} == 36
# Test the ability to create a function that generates stateful counter functions
#T TinyClosure1 fn mc by {s=0; {s+=by; s}.bind()} p1 = mc(5); p2 = mc(2); p1() == 5 && p1() == 10 && p2() == 2
#T TinyLambda_PatternInvoke1  { 10 -> 'yay'}.invoke(10) == 'yay'
#T TinyLambda_PatternInvoke2  { 10 -> 'yay'}(10) == 'yay'
#  TinyLambda_PatternInvoke3  try { { 10 -> 'yay'}(20); false } catch { true }
#T TinyLambda_PatternInvoke4  { x::xs -> xs + x }([1,2,3]) == [2, 3, 1]

#
# Implementation of a user-defined anonymous function in Tiny
# (Similar to a ScriptBlock in PowerShell). Functions in Tiny are
# just TinyLambda's bound to a name in the variable or constant table
# so
#     fn foo n -> n * n
# is essentially the same as
#    foo = {n -> n * n }
# except the 'fn' causes the function to be bound at compile time instead
# of at run time.
#
class TinyLambda
{
    [Token]
        $Token = [Token]::new()

    [Assignable[]]
        $Parameters

    [Expression]
        $Body

    [Dictionary[string,object]]
        $Environment

    [Dictionary[string,object]]
        $LocalFunctions

    [Type]
        $ReturnType

    [object]
        $TinyThis

    # Field to allow functions to memoize values
    [Dictionary[object,object]] 
        $_Memo;

    # returns true if the argument has been memoized
    [bool] IsMemoized($obj) {
        return $null -ne $this._memo -and $this._memo.ContainsKey($obj)
    }

    # Returns the memoized object if it exists
    [object] GetMemoized($obj) {
        if ($null -ne $this._memo) {
            return $this._memo[$obj]
        }
        else {
            return $null
        }
    }

    # Returns the entire memoization table
    [object] GetMemoized() {
        if ($null -ne $this._memo) {
            return $this._memo
        }
        else {
            return $null
        }
    }
    
#BUGBUGBUG add memoization tests
    # Memoize the object/value pair
    [object] Memoize($obj, $value) {
        if ($null -eq $this._memo) {
            $this._Memo = [Dictionary[object,object]]::new()
        }

        $this._memo[$obj] = $value
        return $value
    }

    # Memoize the object/value pair
    [object] Memoize($obj, [TinyLambda] $computeValue) {
        if ($null -eq $this._memo) {
            $this._Memo = [Dictionary[object,object]]::new()
        }

        if ($this._memo.ContainsKey($obj)) {
            return $this._memo[$obj];
        }

        $value = $computeValue.Dot()
        $this._memo[$obj] = $value
        return $value
    }

    # Clear the memo table.
    ClearMemoized() {
        if ($null -ne $this._memo) {
            $this._memo.Clear()
            $this._memo = $null
        }
    }

    # the file that was active when this lambda was instantiated
    [string]
        $fileName = '<stdin>'


    [string] ToString() {
        return 'TinyLambda(' + ($this.Parameters -join ', ') + ')'
    }

    TinyLambda ([type] $returnType,[Assignable[]] $parameters, [Expression] $body, [Dictionary[string,object]] $localFns) {
        if ($null -eq $parameters) {
            $this.Parameters = [Assignable[]]::new(0)
        }
        else {
            $this.Parameters = @($parameters)
        }

        $this.ReturnType     = $returnType
        $this.body           = $body
        $this.LocalFunctions = $localFns
        $this.FileName       = [scopes]::scopes[0]['__file']
        $this.Environment    = [scopes]::scopes[0]
    }

    # Return a new lambda bound to the current environment (scope).
    # This was too slow to do in all cases so it was refactored into
    # a special method.
    [TinyLambda] Bind() {
        $newLambda = [TInyLambda]::new($this.ReturnType, $this.Parameters, $this.Body, $this.LocalFunctions)
        # clone the current environment
        $newlambda.Environment = [Dictionary[string, object]]::new( [scopes]::scopes[0],
            [StringComparer]::OrdinalIgnoreCase )
        $newLambda.Token = $this.Token
        return $newLambda
    }

    # Return a clone of the current lambda.
    [TinyLambda] Clone() {
        return [TInyLambda]::new($this.ReturnType, $this.Parameters, $this.Body, $this.LocalFunctions)
    }

    #T ScopeTest_Invoke1 x=3 {x=4}.invoke(); x == 3
    [object] Invoke() { return $this.Invoke(@(), [AutomationNull]::Value, [AutomationNull]::Value, $null) }

    #T ScopeTest_Invoke2 {x, y -> x + y}.Invoke([2, 3]) == 5
    [object] Invoke ([TinyList] $arguments) {
        return $this.Invoke(@($arguments.list), [AutomationNull]::Value, [AutomationNull]::Value, $null)
    }

    #T ScopeTest_Invoke3 {it}.invoke([], 13) == 13
    #T ScopeTest_Invoke4 {x,y -> [x, y, it] }.invoke([1, 2], 3) == [1, 2, 3]
    [object] Invoke ([TinyList] $arguments, $it) {
        return $this.Invoke(@($arguments.list), $it, [AutomationNull]::Value, $null)
    }

    #T ScopeTest_Invoke5 {x,y -> [x, y, it, it2]}.invoke([1, 2], 3, 4) == [1, 2, 3, 4]
    [object] Invoke ([TinyList] $arguments, $it, $it2) {
        return $this.Invoke(@($arguments.list), $it, $it2, $null)
    }

    # body really should be either a hash or a lambda but we'll pass a number for test purposes
    #T ScopeTest_Invoke6 {x,y -> [x, y, it, it2, body]}.invoke([1, 2], 3, 4, 5) == [1, 2, 3, 4, 5]
    [object] Invoke ([TinyList] $arguments, $it, $it2, $body) {
        return $this.Invoke(@($arguments.list), $it, $it2, $body)
    }

    [object] Invoke ([object[]] $arguments) {
        return $this.Invoke($arguments, [AutomationNull]::Value, [AutomationNull]::Value, $null)
    }

    [object] Invoke ([object[]] $arguments, $it) {
        return $this.Invoke($arguments, $it, [AutomationNull]::Value, $null)
    }

    [object] Invoke ([object[]] $arguments, $it, $it2) {
        return $this.Invoke($arguments, $it, $it2, $null)
    }

    [object] Invoke ([object[]] $arguments, $it, $it2, $body) {
        return $this.Invoke($arguments, $it, $it2, $body, $null)
    }

    #
    # Invoke this lambda in a new scope
    #
    [object] Invoke ([object[]] $arguments, $it, $it2, $body, [ref] $BindSuccess) {
        [scopes]::PushScope()
        $result = $null
        $current = [scopes]::scopes[0]
        $env = $this.Environment
        try {
            # Copy the environment captured with the Bind() method.
            if ($null -ne $env) {
                foreach ($p in $env.GetEnumerator()) {
                    $current[$p.Key] = $p.Value
                }
            }

            # Bind the local functions
            $localfns = $this.LocalFunctions
            if ($null -ne $localfns) {
                foreach ($p in $localFns.GetEnumerator()) {
                    $current[$p.Key] = $p.Value
                }
            }

            if ($it -ne [AutomationNull]::Value) {
                $current['it'] = $it
            }

            if ($it2 -ne [AutomationNull]::Value) {
                $current['it2'] = $it2
            }

            if ($this.TinyThis -ne [AutomationNull]::Value) {
                $current['this'] = $this.TinyThis
            }

            # set a pointer back to the script block
            $current['_me'] = $this

            $current['__file'] = $this.filename
            $current['body'] = $body

            # If a bindSuccess variable  reference was passed in, use it to
            # return the status of the bind.
            if ($null -ne $bindSuccess.Value) {
                if ($this.BindParameters($arguments, $false)) {
                    $bindSuccess.Value = $true
                }
                else {
                    $bindSuccess.Value = $false
                    return $null
                }
            }
            else {
                [void] $this.BindParameters($arguments, $true)
            }

            $result = $this.body.Eval()
        }
        finally {
            # Copy the updated environment back.
            if ($null -ne $env) {
                foreach ($p in $current.GetEnumerator()) {
                    $env[$p.Key] = $p.Value
                }
            }
            [void] [scopes]::PopScope()
        }

        # if there is a return type specified for this lambda, apply it now.
        if ($this.ReturnType) {
            try {
                return [LanguagePrimitives]::ConvertTo($result, $this.ReturnType)
            }
            catch {
                errorMessage $_ $this.Token
            }
        }

#say "TinyLambda Invoke: result is $result"
        return $result
                
    }

    #
    # Invoke this lambda in a new scope, but return the results of all
    # of the statements that are executed instead of just the last one.
    #
    [object] InvokeReturnAll() {
        [scopes]::PushScope()
        $result = $null
        $current = [scopes]::scopes[0]
        $env = $this.Environment

        try {

            # Copy the environment captured with the Bind() method.
            if ($null -ne $env) {
                foreach ($p in $env.GetEnumerator()) {
                    $current[$p.Key] = $p.Value
                }
            }

            # Bind the local functions
            $localfns = $this.LocalFunctions
            if ($null -ne $localfns) {
                foreach ($p in $localFns.GetEnumerator()) {
                    $current[$p.Key] = $p.Value
                }
            }

            if ($this.TinyThis -ne [AutomationNull]::Value) {
                [scopes]::SetVariable('this', $this.TinyThis)
            }

            $current['__file'] = $this.filename

            $result = [TinyList]::new()
            if ($this.body -is [StatementList]) {
                foreach ($stmt in $this.Body.Statements) {
                    # don't add output of assignment statements
                    if ($stmt -is [Assignment]) {
                        $null = $stmt.Eval()
                    }
                    else {
                        $val = $stmt.Eval()

                        # skip nulls
                        if ($null -ne $val -and '' -ne $val) {
                            $result.List.Add($val)
                        }
                    }
                }
                $result.Count = $result.List.Count
                if ($result.Count -eq 0) {
                    $result = $null
                }
            }
            else {
                $result = $this.body.Eval()
            }
        }
        finally {
            # Copy the environment back.
            if ($null -ne $env) {
                foreach ($p in $current.GetEnumerator()) {
                    $env[$p.Key] = $p.Value
                }
            }
            [void] [scopes]::PopScope()
        }

        return $result -as $this.ReturnType
    }

    #T ScopeTest_Dot1 x = 3; {x = 4}.Dot(_, _, _, _); x == 4 # confim that the lambda is run in the current scope
    #T ScopeTest_Dot2 it = 5; {inner_it = it}.Dot(_, 3.14, _, _); it == 5 && inner_it == 3.14
    #T ScopeTest_Dot3 it2 = 6; {inner_it2 = it2}.Dot(_, _, 60, _); it2 == 6 && inner_it2 == 60
    #T ScopeTest_Dot4 body = 6; {inner_body = body}.Dot(_, _, _, 'hi'); body == 6 && inner_body == 'hi'
    # dotting with params doesn't set the variables in the current scope
    #T ScopeTest_Dot5 x = 1; y = 2; {x, y -> x+y}.Dot([10,20], _, _, _) == 30
    [object] Dot ([TinyList] $arguments, $it, $it2, $body) {
        return $this.Dot($arguments.List, $it, $it2, $body)
    }

    # Execute the lambda in the current scope - like dotting in PowerShell.
    [object] Dot ([object[]] $arguments, $it, $it2, $body) {
        $current_Scope = [scopes]::Scopes[0]

        # Preserve any variables in the current scope that would be overridden
        # either by parameter binding or one of the 'magic' it, it2 or body variables.
        $old_variable_values = [Dictionary[string,object]]::new()

        if ($it -ne [AutomationNull]::Value) {
            $out = $null
            if ($current_scope.TryGetValue('it', [ref] $out)) {
                $old_variable_values['it'] = $out
            }
            $current_Scope['it'] = $it
        }

        if ($it2 -ne [AutomationNull]::Value) {
            $out = $null
            if ($current_scope.TryGetValue('it2', [ref] $out)) {
                $old_variable_values['it2'] = $out
            }
            $current_Scope['it2'] = $it2
        }

        $out = $null
        if ($current_scope.TryGetValue('body', [ref] $out)) {
            $old_variable_values['body'] = $out
        }
        $current_Scope['body'] = $body
        $old_variable_values['__file'] = $current_scope['__file']
        $current_Scope['__file'] = $this.filename

        # Bind the local functions
        $localfns = $this.LocalFunctions
        if ($null -ne $localfns) {
            foreach ($p in $localFns.GetEnumerator()) {
                if ($current_scope.TryGetValue($p.Key, [ref] $out)) {
                    $old_variable_values[$p.Key] = $out
                }
                $current_scope[$p.Key] = $p.Value
            }
        }

        # Save the current values of variables corresponding to parameters
        foreach ($p in $this.Parameters) {
            $out = $null
            if ($p -is [Variable] -and $current_scope.TryGetValue($p.Name, [ref] $out)) {
                $old_variable_values[$p] = $out
            }
        }

        $result = $null
        try {
            $this.BindParameters($arguments, $true)
            $result = $this.body.Eval()
        }
        finally {
            # Restore the old variable bindings
            foreach ($pair in $old_variable_values.GetEnumerator()) {
                $current_Scope[$pair.key] = $pair.Value
            }
        }
        return $result -as $this.ReturnType
    }

    #T ScopeTest_SimpleDot1 x = 3; {x = 4}.Dot(); x == 4 # same as above but with faster binding
    # A faster variant on Dot() that doesn't do any binding and just Evals()s the body
    [object] Dot() {
        if ($null -ne $this.parameters -and $this.parameters.count -gt 0 ) {
            errorMessage 'Dot() can''t be called on a lambda with parameters.'
        }
        return $this.body.Eval() -as $this.ReturnType
    }

    #T ScopeTest_SimpleReturnALl1 x = 3; r = {x = 4; 1; 2; 3}.DotReturnAll(); x == 4 && r == [1, 2, 3]
    #T ScopeTest_SimpleReturnALl2 it = 5; x = 3; r = {x = 4; 1; 2; 3; it}.DotReturnAll(); x == 4 && r == [1, 2, 3, 5]
    # A faster variant on Dot() that doesn't do any binding but
    # returns the values of all of the statements in the body.
    [object] DotReturnAll() {
        if ($null -ne $this.parameters -and $this.parameters.count -gt 0 ) {
            errorMessage 'DotReturnAll() can''t be called on a lambda with parameters.'
        }
        $result = [TinyList]::new()
        if ($this.body -is [StatementList]) {
            foreach ($stmt in $this.Body.Statements) {
                # don't add output of assignment statements
                if ($stmt -is [Assignment]) {
                    $null = $stmt.Eval()
                }
                else {
                    $val = $stmt.Eval() -as $this.ReturnType

                    # skip nulls
                    if ($null -ne $val -and '' -ne $val) {
                        $result.List.Add($val)
                    }
                }
            }
            $result.Count = $result.List.Count
            if ($result.Count -eq 0) {
                $result = $null
            }
        }
        else {
            $result = $this.body.Eval() -as $this.ReturnType
        }

        return $result
    }

    # Bind the parameters for the current invocation.
    # Note: we don't bind the 'args' unless it has been specified as a formal arg.
    [bool] BindParameters([object[]] $arguments, [bool] $throwException) {
        $arglen = if ($null -ne $arguments) {
                    $arguments.length
                  }
                  else {
                    0
                  }

        if ($null -ne $this.Parameters -and @($this.Parameters).Length -gt 0) {
            $parms    = @($this.parameters)
            $paramLen = $parms.Length-1
        }
        else {
            # Nothing to bind so return true
            return $true
        }

        foreach ($i in 0 .. $paramlen) {

            $p = $parms[$i]

            $valueToBind = $null
            if ($i -lt $arglen) {
                $valueToBind = $arguments[$i]
            }
            elseif ($p -is [Assignable] -and $p.InitialValue -ne [AutomationNull]::Value) {
                # BUGBUGBUG need to check on the scope being used for evaluation
                $valueToBind = $p.InitialValue.Eval()
            }

            $result = $p.Set($valueToBind)

            # If there's a bind failure during a pattern match, just return false.
            # If it's not a pattern match, throw an exception so the user sees an error.
            if (($p -is [Pattern] -or $p -is [PropertyPattern] -or $p -is [Constant] -or $p -is [TypeConstrainedVariable]) -and
                $false -eq $result)
            {
                if ($throwException) {
                    throw [BindException]::New("Binding value '$valueToBind' to  parameter '$p' failed.", $this.Token)
                }
                else {
                    return $false
                }
            }
        }

        # if the last arg is 'args' assign the remaining values to it
        # args is always an array, even if there are exactly the right number of arguments
        if ($parms[-1] -is [Variable] -and $parms[-1].Name -eq 'args') {
            if ($i -lt $arglen ) {
                $argList = [List[object]] $arguments
                [void] $parms[-1].Set([TinyList]::new($arglist.getrange($i, $arglen-$i)))
            }
            else {
                [void] $parms[-1].Set([TinyList]::new())
            }
        }
<# BUGBUGBUG
        else {
            if ($i -lt $arglen-1) {
                $argList = [List[object]] $arguments
                errorMessage "Too many arguments; name the last variable 'args' in varargs functions. Extra args: $($arglist.getrange($i, $arglen-$i))"
            }
        }
#>
        return $true
    }

    #T ScopeTest_Parent1  x = 3; { {x = 4}.parent(_, _, _, _) }.invoke(); x == 4
    #T ScopeTest_ParentPointer1 abc = 3.14; {{__parent.abc = 6.28}(); abc}() == 6.28 &&  abc == 3.14
    #T ScopeTest_GlobalPointer1 __global.abc = 0; { {__global.abc = 6.28 }() }(); abc == 6.28
    #
    # Invoke a lambda in the parent (caller's) scope
    #
    [object] Parent ([object[]] $arguments, $it, $it2, $body) {
        [Dictionary[string, object]] $old_Scope = [scopes]::PopKeepScope()
        try {
            return $this.Dot($arguments, $it, $it2, $body)
        }
        finally {
            [scopes]::PushScope($old_Scope)
        }
        return $null
    }
}

###################################################################################
#
# The Parser. Grammar rules are implemented as static methods which build an expression
# tree from the script text using the Tokenizer class. The associated grammar rules 
# are indicated in comments above the method.
#
###################################################################################
class Parser
{

    #G Name = '[a-z_][a-z0-9__]*' ;;
    #G
    static [Variable] nameRule() {
        # variables can be regular or pinned. _ and null are readonly.
        if ($res = [tokenizer]::next([tokenkind]::Name)) {
            if ($res.Value -eq '_' -or $res.Value -eq 'null') {
                return [ReadonlyVariable]::new($res)
            }
            return [Variable]::new($res)
        }

        if ($res = [Tokenizer]::next([TokenKind]::PinnedVar)) {
            return [PinnedVariable]::new($res)
        }

        return $null
    }

    #G LambdaLiteral = '{' ParameterList '->' StatementList '}' ;;
    #G
    static [Literal] lambdaLiteralRule([token] $lcurly) {
        $syncPoint = [tokenizer]::Offset
        $params =  [parser]::parameterListRule()
        if (-not ([tokenizer]::Next([tokenkind]::arrow))) {
            if ($params.Count -gt 1) {
                parseError "Missing '->' operator while processing lambda expression." $params[0].Token
            }
            elseif ($params.Count -eq 1) {
                [tokenizer]::Offset = $syncPoint
                $params = [List[Assignable]]::new()
            }
        }
        else {
            # constants can be used in parameters for pattern matching but using
            # a constant function probably an error.
            # BUGBUGBUG - maybe this should be a warning?
            foreach ($expr in $params) {
                if ($expr -is [constant] -and ($expr.value -is [PSMethod] -or $expr.Value -is [TinyLambda])) {
                    errorMessage `
                        "The constant '$($expr.Name)' is bound to a function and should not be used as a parameter name." `
                            $expr.Token
                }
            }
        }

        $localFns = $null
        $body     = $null
        $oldLocalFunctions = $script:LocalFunctions
        try {
            $script:LocalFunctions = [Dictionary[string,object]]::new([StringComparer]::OrdinalIgnoreCase)
            $body = [parser]::statementListRule()
            $localFns = $script:LocalFunctions
        }
        finally {
            $script:LocalFunctions = $oldLocalFunctions
        }

        if (-not ($rcurly = [tokenizer]::next([tokenkind]::rcurly))) {
            parseError "missing '}' in lambda expression (lambda started at $([tokenizer]::PositionMessage($lCurly)))"
        }

        return [Literal]::new($rcurly, [TinyLambda]::new([object], $params, $body, $localFns))
    }

    #G ObjectLiteral = '{' Name ':' Expression [[';'] Expression ] * '}' ;;
    #G
    static [expression] objectLiteralRule ($openBrace) {
            return [parser]::objectLiteralRule($openBrace, [TokenKind]::RCurly)
    }

    static [expression] objectLiteralRule ($openBrace, $closeToken) {
        $hash = [ordered] @{}
        $colon = $null
        [bool] $isPropertyPattern = $openBrace.Kind -eq [TokenKind]::StartPP
        while ($true) {
            $name = ""
            $token = [tokenizer]::next([tokenkind]::name)
            if (-not $token) {
                $token = [tokenizer]::next([tokenkind]::string)
                if (-not $token) {
                    if (($badstring = [tokenizer]::next([tokenkind]::badstring))) {
                        errormessage "Unterminated string-valued key in object literal." $badstring
                    }
                    else {
                        parseError "Missing or invalid property name in object literal."
                    }
                }
                else {
                    $name = [parser]::StringHelper($token.Value)
                }
            }
            else {
              $name = $token.Value
            }

            if (-not ($colon = [tokenizer]::next([tokenkind]::colon))) {
                if ($isPropertyPattern) {
                    $newToken = [token]::New()
                    $newToken.Value = $name
                    # BUGBUGBUG this should be a constant
                    $hash[$name] = if ($newToken.Value -eq '_' -or $newToken.Value -eq 'null') {
                        [ReadonlyVariable]::new($newToken)
                    }
                    else {
                        [Variable]::new($newToken)
                    }

                    while ([tokenizer]::next([tokenkind]::semicolon)) { }

                    if ([tokenizer]::next($closeToken)) {
                        break
                    } else {
                        continue
                    }
                }
                else {
                    parseError "Missing colon in object literal."
                }
            }

            $value = $null
            if (-not ($value = [parser]::statementRule())) {
                errorMessage "Missing or invalid value for property '$($name)' in object literal." $colon
            }

            $current = $hash[$name]
            if (-not $current) {
                $hash[$name] = $value
            }
            elseif ($current -is [Literal] -and $current.Value -is [TinyLambda] -and $Value -is [Literal] -and $value.Value -is [TinyLambda]) {
                $list = [List[TinyLambda]]::new()
                $list.Add($current.Value)
                $list.Add($value.Value)
                $current.Value = $list
            }
            elseif ($current -is [Literal] -and $current.Value -is [List[TinyLambda]] -and $value -is [Literal] -and $value.Value -is [TinyLambda]) {
                $current.Value.Add($value.Value)
            }
            else {
                errorMessage "objectLiteral: key '$name' already exists in the table." $colon
            }

            #BUGBUGBUG - to make it compatible with JSON
            # Eat semicolons and commas (semicolon because we allow statements
            # in object literals and commas so we're compatible with JSON)
            while ([tokenizer]::next([tokenkind]::semicolon)) { }
            while ([tokenizer]::next([tokenkind]::Comma)) { }

            if ([tokenizer]::next($closeToken)) {
                break
            }
        }

        if ($openBrace.Kind -eq [TokenKind]::StartPP) {
            return [PropertyPattern]::new($openBrace, $hash)
        }
        else {
            return [ObjectLiteral]::new($openBrace, $hash)
        }
    }

    #
    # Helper method to clean up string tokens: trim quotes
    # handle escapes, etc.
    #
    static [string] StringHelper ([string] $val) {
        if ($val[0] -eq '"') {
            return $val.substring(1, $val.length-2).
                replace('""', '"').
                replace('\n', [Environment]::NewLine).
                replace('\r', "`r").
                replace('\t', "`t").
                replace('\\', '\')
        }
        elseif ($val[0] -eq '`') {
            if ([char]::iswhiteSpace($val[-1])) {
                return $val.substring(1, $val.length-2)
            }
            else {
                return $val.substring(1)
            }
        }
        else {
            return $val.substring(1, $val.length-2).replace("''", "'")
        }
    }

    #
    # Helper method that takes a regular expression token and builds a RegexLiteral
    # expression out of it; including option parsing and handling the
    # name element.
    #
    static [RegexLiteral] RegexHelper([Token] $RegexToken) {
        $var = $null
        $val = $RegexToken.Value
        if ($val -match ':([_a-z][_a-z0-9]*)$') {
            # Extract the variable to set
            $var = $matches[1]
            # Extract the regex itself
            $val = $val -replace ':[_a-z][_a-z0-9]*$'
        }

        if ($val[-1] -eq 'c') {
            # Case sensitive match
            $options = 'CultureInvariant'
            # trim the options characters
            $val = $val.substring(0, $val.Length-1)
        }
        else {
            $options = 'IgnoreCase,CultureInvariant'
        }

        # Trim off the 'r/' and trailing '/' before compiling the expression
        $val = $val.Substring(2, $val.Length-3) -replace '//','/'
        try {
            $val = [regex]::New($val, $options)
        }
        catch {
            errorMessage "Invalid regular expression literal: r/$val/" $RegexToken
        }
        return [RegexLiteral]::new($RegexToken, $var, $val)
    }

    #G FunctionCall = Expression '(' ArgumentList ')' [ LambdaLiteral | ObjectLiteral ] |
    #G                Expression LambdaLiteral [ LambdaLiteral | ObjectLiteral ]        |
    #G                Expression ObjectLiteral [ LambdaLiteral | ObjectLiteral ]        |
    #G                Expression StringLiteral [ LambdaLiteral | ObjectLiteral ]        |
    #G                Expression RegexLiteral  [ LambdaLiteral | ObjectLiteral ]
    #G                ;;
    #G
    static [Expression] FunctionCallOrIndexRule([Expression] $expression, [Token] $token) {
        try {
            # Handle function calls with a literal argument e.g. myfunc 'hi' or myfunc {println 'hit' }.
            # Note: don't allow numeric literals without parens because 'foo+2' is ambiguous. It could be foo(+2) or foo() + 2
            if ($token.Kind -eq [TokenKind]::String -or $token.Kind -eq [TokenKind]::Regex) {
                if ($token.Kind -eq [TokenKind]::String) {
                    # See if this is an expandable string token.
                    $expandable = $token.Value[0] -eq '"' -and $token.Value -match '\$[a-z_]|\$\{'
                    $val = [parser]::StringHelper($token.Value)
                    if ($expandable) {
                        $Literal = [ExpandableString]::new($token, $val)
                    }
                    else {
                        $Literal = [Literal]::new($token, $val)
                    }
                }
                else {
                    $Literal = [Parser]::RegexHelper($token)
                }

                # Allow a lambda or hashtable after the formal args
                $body = $null
                $currentChar = [tokenizer]::CurrentCharacter()
                if ($currentChar -eq '{') {
                    $lcurly = [tokenizer]::Next([tokenkind]::lcurly)
                    $syncPoint = [tokenizer]::offset
                    $hashKey = [tokenizer]::next([tokenkind]::name)
                    if (-not $hashKey) {
                        $hashKey = [tokenizer]::next([tokenkind]::string)
                    }
                    $colon = [tokenizer]::next([tokenkind]::colon)
                    [tokenizer]::offset = $syncPoint
                    if ($null -ne $hashKey -and $null -ne $colon) {
                        $body = [parser]::objectLiteralRule($lCurly)
                    }
                    else {
                        if ($body = [parser]::lambdaLiteralRule($lcurly)) {
                            $body = $body.Value
                        }
                    }
                }
                $expression = [FunctionCall]::new($token, $expression, @( $Literal ), $body)
            }
            elseif ($token.Value -eq '(') {
                $arglist = [parser]::argumentListRule()
                if (-not ([tokenizer]::Next([tokenkind]::rparen))) {
                    # fix up the function name sp we get a better error message
                    if ($expression -is [Variable]) {
                        $name = $expression.Name
                    }
                    elseif ($expression -is [Constant]) {
                        $name = $expression.Token.Value
                    }
                    elseif ($expression -is [Literal]) {
                        $name = $expression.Value.ToString()
                    }
                    else {
                        $name = $expression.ToString()
                    }

                    # And emit the error message
                    parseError (
                                "Missing ')' in call to function '$($name)(', " +
                                "function call started at $([tokenizer]::PositionMessage($token))."
                               )
                }

                # Allow a lambda or hashtable after the formal args
                $body = $null
                $currentChar = [tokenizer]::CurrentCharacter()
                if ($currentChar -eq '{') {
                    $lcurly = [tokenizer]::Next([tokenkind]::lcurly)
                    $syncPoint = [tokenizer]::offset
                    $hashKey = [tokenizer]::next([tokenkind]::name)
                    if (-not $hashKey) {
                        $hashKey = [tokenizer]::next([tokenkind]::string)
                    }
                    $colon = if ($null -ne $hashkey) {
                        [tokenizer]::next([tokenkind]::colon)
                    }
                    else {
                        $null
                    }
                    [tokenizer]::offset = $syncPoint
                    if ($hashKey -and $colon) {
                        $body = [parser]::objectLiteralRule($lCurly)
                    }
                    else {
                        $body = [parser]::lambdaLiteralRule($lCurly)
                    }
                }
                $expression = [FunctionCall]::new($Token, $expression, $arglist, $body)
            }

            # Function call that takes a single lambda as an argument
            elseif ($token.value -eq '{') {
                $syncPoint = [tokenizer]::offset
                $hashKey = [tokenizer]::next([tokenkind]::name)
                if (-not $hashKey) {
                    $hashKey = [tokenizer]::next([tokenkind]::string)
                }
                $colon = [tokenizer]::next([tokenkind]::colon)
                [tokenizer]::offset = $syncPoint
                if ($hashKey -and $colon) {
                    $arglist = [parser]::objectLiteralRule($token)
                }
                else {
                    $arglist = [parser]::lambdaLiteralRule($token)
                }
                
                # Allow a lambda after a hashtable
                $body = $null
                $currentChar = [tokenizer]::CurrentCharacter()
                if ($currentChar -eq '{') {
                    $lcurly = [tokenizer]::Next([tokenkind]::lcurly)
                    if ($body = [parser]::lambdaLiteralRule($lcurly) ) {
                        $body = $body.Value
                    }
                }
                $expression = [FunctionCall]::new($token, $expression, $arglist, $body)
            }
            elseif ($token.value -eq '[' -or $token.value -eq '?[') {
                if (-not ($indexExpr = [parser]::ExpressionRule())) {
                        parseError "Missing index expression after '$($token.Value)'."
                 }
                 if (-not ([tokenizer]::next([tokenkind]::rsquare))) {
                        parseError "Missing ']' in array index after '$($token.Value)'."
                 }
                 $handleNull = $token.Value -eq '?['
                 $expression = [ArrayIndexExpression]::new($token, $expression, $indexExpr, $handleNull)
            }
            else {
                errorMessage "FunctionCallOrIndexRule: token was $token; nothing matched; fell through to here!" ([Token]::New())
            }
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($exception -is [TinyException]) {
                if ($null -eq $exception.Token) {
                    $exception.Token = [Token]::New()
                }
                throw $exception
            }
            Write-Host -fore red "Unexpected error in FunctionCallOrIndexRule at $([tokenizer]::PositionMessage())"
            $_ | Format-List * -force | Out-Host
            throw $_.Exception
        }
        return $expression
    }

    #T UnaryOperator_Minus1 10 == --10
    #T UnaryOperator_Minus2 a = 123; -a  == -123
    #T UnaryOperator_Minus3 -10 == ---10
    #T UnaryOperator_Minus4 10 == -(-10)
    #T UnaryOperator_Not1 ! true == false
    #T UnaryOperator_Not2 true  == !false
    #T UnaryOperator_Not3 false == !!false
    #T UnaryOperator_Not4 ! 1 == false
    #T UnaryOperator_Not5 ! 0 == true
    #T UnaryOperator_Not6 ! "abc" == false
    #T UnaryOperator_Not7 ! ""  == true
    #T UnaryOperator_Not8 ! [1] == false
    #T UnaryOperator_Not9 ! []  == true
    #T UnaryOperator_Plus1 + 3.14 == 3.14
    #T UnaryOperator_Plus2  4 - + - 7 == 11
    #T UnaryOperator_Plus3 +[1..10] == 55
    #T UnaryOperator_Minus5 -[1,2,3] == [-1, -2, -3]
    #T UnaryOperator_Minus6 - [1..10] |> zip ([10..1]) |> map {x::y::_ -> x+y} == [9, 7, 5, 3, 1, -1, -3, -5, -7, -9]
    static [Expression] unaryOperatorRule() {
        $op = [Tokenizer]::Next([TokenKind]::UnaryOperator)
        if ($null -eq $op) {
            $op = [Tokenizer]::Next([TokenKind]::TypeLiteral)
            if ($null -ne $op) {
                $typeStr = $op.value
                $typeName = $typeStr.Substring(2, $typeStr.Length-4)
                # Types must be known at compile time
                $type = $typename -as [type]
                if (-not $type) {
                    errorMessage "Type name '$typename' doesn't resolve to an known type." $op
                }

                # Handle return typed lambdas: [<int>] {n -> 123+n}
                if ([tokenizer]::CurrentCharacter() -eq '{') {
                    $lcurly = [Tokenizer]::Next([TokenKind]::lCurly)
                    $lambdaLiteral = [parser]::lambdaliteralrule($lcurly)
                    $lambdaLiteral.Value.ReturnType = $type
                    return $lambdaLiteral
                }

                if ($null -eq ($expr = [parser]::unaryOperatorRule()))
                {
                    $expr = [parser]::ValueRule()
                }

                # if there was a value after the expression, return a cast. Otherwise treat it as a type literal.
                if ($null -ne $expr) {
                    return [CastOperator]::New($op, $type, $expr)
                }
                else {
                    return [Literal]::new($op, $type)
                }
            }

            return [parser]::ValueRule()
        }
        elseif ($op.Value -eq '->') {
            # need to handle the arrow to disambiguiate between -> and -
            [Tokenizer]::Unget($op);
            return $null
        }
        else {
            if ($null -eq ($expr = [parser]::unaryOperatorRule()))
            {
                $expr = [parser]::ValueRule()
            }

            if ($null -ne $expr) {
                return [UnaryOperator]::New($op, $expr)
            }

            parseError "missing operand after unary operator '$($op.Value)'" $op
        }

        return $null
    }
    
    #T NumberLiteral_Test1  123 is [<int>]
    #T NumberLiteral_Test2  123.45 is [<double>]
    #T NumberLiteral_Test3  (123 + 2.4) is [<double>]
    #T NumberLiteral_Test4  1_2_3_4 == 1234
    #T NumberLiteral_Test5  1_2_3_4._5_6 == 1234.56
    #T NumberLiteral_Test6  0xABCD == 43981
    #T NumberLiteral_Test7  0xAB_CD__ == 43981
    #T Variable_Test1 abc = 3; abc == 3
    #T Variable_Test2 abc = 3; abc == ABC
    #T Variable_Test3 abc1 = 3; abc1 == ABC1 && abc1 == 3
    #T Variable_Test4 __a = 5; __a == 5
    #T Variable_Test5 __a_5_6 = 10; __a_5_6 == 10
    #T RegexLiteral_Test1 r/abc/ is [<regexliteral>]
    #T RegexLiteral_Test2 r/abc/.match('abcde')
    #T ObjectLiteral_Test1 {a:1 b:2} is [<IDictionary>]
    #T ObjectLiteral_Test2 {} is [<IDictionary>]
    #T ObjectLiteral_Test3 {a:{b:{c:{d:3.14}}}} .a.b.c.d == 3.14
    #T LambdaLiteral_Test1 {x, y -> x+y} is [<TinyLambda>]
    #T LambdaLiteral_Test2 {x} is [<TinyLambda>]
    #T LambdaLiteral_Test3 {x:1} is [<IDictionary>]
    #T LambdaLiteral_Test4 {x=1} is [<TinyLambda>]
    #T ArrayLiteral_Test1 [] is [<TinyList>] && [].Count == 0
    #T ArrayLiteral_Test2 x = [1,2,3]; x is [<TinyList>] && x.count == 3
    #T ArrayLiteral_Test3 x = [1,[2,3,4], 5]; x.Count == 3 && x[1].Count == 3 && x[1][2] == 4
    #T Interpolation_Test x = 1; y = 2; "x is $x y is $y" == 'x is 1 y is 2'
    #G Value       = NumericLiteral     |
    #G               StringLiteral      |
    #G               RegexLiteral       |
    #G               Variable           |
    #G               ObjectLiteral      |
    #G               LambdaLiteral      |
    #G               TypeLiteral        |
    #G               FunctionCall       |
    #G               PropertyExpression |
    #G               ArrayIndex         |
    #G              '(' Statement ')'
    #G              ;;
    #G
    #G NumericLiteral = '(-)?[0-9]+(\.[0-9]+)?(e[0-9]+)?' ;;
    #G
    #G # Tiny supports both single and double-quoted strings.
    #G # Escapes are processed in double-quoted strings.
    #G # Expansions are not supported.
    #G
    #G StringLiteral = '"[^"]*"' | '''[^''']*' ;;
    #G
    #G RegexLiteral = 'r/[^/]*/[c]?([a-z][a-z0-9]*)?' ;;  # Case-insensitive by default,
    #G                                                    # Case-sensitive with trailing option e.g. r/abc/c
    #G                                                    # Optionally specify the name of the variable to store matches in.
    #G
    #G # Type literals look like  [<int>] or [<System.Collections.IDictionary>]. This
    #G # is similar to F#'s attribute syntax.
    #G
    #G TypeLiteral  =  '\[<[a-z][a-z0-9.\[\]]*>]' ;;
    #G
    #G Variable     =  Name;;
    #G
    #G PropertyExpression = PropertyExpression [ '.' | '?.' ] Expression ;;
    #G
    #G ArrayIndex = Expression [ '[' | '?[' ] Expression ']' ;;
    #G
    static [Expression] valueRule() {
        try {
            $currentChar = [tokenizer]::CurrentCharacter()
            switch ($currentChar) {
                '`' {
                    # Handle atoms: `foo `bar
                    $atom = [Tokenizer]::Next([TokenKind]::String)
                    if ($null -eq $atom) {
                        errorMessage -dummy "Incomplete atom"
                    }
                    return [Literal]::new($atom, [parser]::StringHelper($atom.Value))
                }
                '(' {
                    # Parenthetical expressions
                    [Tokenizer]::Offset++
                    $res = [parser]::statementRule()
                    if (-not ([tokenizer]::Next([tokenkind]::rparen))) {
                        parseError "Missing ')' in parenthetical expression."
                    }
                    return $res
                }
                '[' {
                    # might be type literal or array literal; check type literal first
                    if ($res = [tokenizer]::next([TokenKind]::typeLiteral)) {
                        $typeStr = $res.value
                        $typeName = $typeStr.substring(2, $typeStr.length-4)
                        # Types must be known at compile time
                        $type = $typename -as [type]
                        if (-not $type) {
                            errorMessage "Type name '$typename' doesn't resolve to an known type." $res
                        }
                        return [Literal]::new($res, $type)
                    }
                    else {
                        # Array literal
                        $lb = [tokenizer]::Next([tokenkind]::lsquare)
                        $expr = [parser]::argumentListRule($true)
                        if (-not ([tokenizer]::Next([tokenkind]::rsquare))) {
                            parseError "Missing ']' in array literal."
                        }
                        return [ArrayLiteral]::new($lb, $expr)
                    }
                }
                '{' {
                    # property pattern {:: a:1 b:2 ::}
                    $startPP = [tokenizer]::next([TokenKind]::StartPP)
                    if ($null -ne $startPP) {
                        return [parser]::ObjectLiteralRule($startPP, [TokenKind]::endPP)
                    }

                    # lambda or object literal
                    $lcurly = [tokenizer]::Next([tokenkind]::lcurly)
                    if ([tokenizer]::next([tokenkind]::rcurly)) {
                        return [ObjectLiteral]::new($lcurly, $null)
                    }

                    $syncPoint = [tokenizer]::offset
                    $name = [tokenizer]::next([TokenKind]::name)
                    if (-not $name) {
                        $name = [tokenizer]::next([TokenKind]::string)
                    }

                    $colon = [tokenizer]::next([TokenKind]::colon)
                    [tokenizer]::offset = $syncPoint
                    if ($null -ne $name -and $null -ne $colon) {
                        return [parser]::ObjectLiteralRule($lCurly)
                    }
                    else {
                        return [parser]::lambdaLiteralRule($lcurly)
                    }
                }
                default {
                    # Regular expression literal
                    if ($res = [tokenizer]::next([TokenKind]::regex)) {
                        return [parser]::RegexHelper($res)
                    }

                    # Variables and Constants
                    if ($name = [parser]::nameRule()) {
                        # Compile constants inline
                        if ($script:TinyConstants.Contains($name.Name)) {
                            return [Constant]::New($name.Token, $script:TinyConstants[$name.Name])
                        }

                        # Otherwise it's  a variable.
                        return $name
                    }

                    #T Literal_String1 "abc" is [<string>]
                    #T Literal_String2 "ab""c" is [<string>]
                    #T Literal_String3 "a'b'c" is [<string>]
                    #T Literal_String4 'abc' is [<string>]
                    #T Literal_String5 'ab''c' is [<string>]
                    #T Literal_String6 'a"b"c' is [<string>]
                    #BUGBUGBUG Platform specific tests!
                    #W Literal_String7 ("a\nbc".tochararray() as [<int[]>]).AsList() == [97, 13, 10, 98, 99]
                    #U Literal_String7a ("a\nbc".tochararray() as [<int[]>]).AsList() == [97, 10, 98, 99]
                    #T Literal_String8 ("a\tbc".tochararray() as [<int[]>]).AsList() == [97, 9, 98, 99]
                    #W Literal_String9 ('a\nbc'.tochararray() as [<int[]>]).AsList() == [97, 92, 110, 98, 99]
                    #U Literal_String9a ('a\nbc'.tochararray() as [<int[]>]).AsList() == [97, 92, 110, 98, 99]
                    #T Literal_String10 ('a\tb'.tochararray() as [<int[]>]).AsList() == [97, 92, 116, 98]
                    #T Literal_String11 x = 'Hi'; "$x there." == 'Hi there.'
                    #T Literal_String12 x = 'Hi'; '$x there.' == '$x there.'
                    #T Literal_String13 a = 1; b = 2; c = 3; "<$b $c $a>" == '<2 3 1>'
                    #T Literal_String14 "sum is ${2+2} ok." == 'sum is 4 ok.'
                    #T Literal_String15 x = 'abc'; "len = ${x.length}" == 'len = 3'
                    #T Literal_String16 import math; "num = ${math.sqrt(9)}" == 'num = 3'

                    # String literals
                    if ($currentChar -eq '"' -or $currentChar -eq "'") {
                        $res = [tokenizer]::next([TokenKind]::string)
                        if (-not $res) {
                            if (($badstring = [tokenizer]::next([TokenKind]::badstring))) {
                                errormessage -dummy "Unterminated string literal." $badstring
                            }
                            else {
                                errorMessage -dummy "Invalid string literal."
                            }
                        }
                        $expandable = $res.Value[0] -eq '"' -and $res.Value -match '\$[a-z_]|\$\{'
                        $val = [parser]::StringHelper($res.Value)
                        if ($expandable) {
                            return [ExpandableString]::new($res, $val)
                        }
                        else {
                            return [literal]::new($res, $val)
                        }
                    }

                    #T Literal_Number1  123i is         [<BigInt>]
                    #T Literal_Number2  x = -123i; x is [<BigInt>] && x < 0
                    #T Literal_Number3  12.34 is        [<Double>]
                    #T Literal_Number4  x = -12.34; x is [<Double>] && x < 0
                    #T Literal_Number5  123 is          [<Int>]
                    #T Literal_Number6  x = -123; x is [<Int>] && x < 0 && x == -123.0
                    #T Literal_Number7  123345 is       [<Int>]
                    #T Literal_Number8  1233456789 is   [<Int>]
                    #T Literal_Number9  (12334567890 is [<int>]) == false
                    #T Literal_Number10 12334567890 is  [<long>]
                    #T Literal_Number11 123345678901234567890 isnot [<long>]
                    #T Literal_Number12 123345678901234567890 is    [<double>]
                    #T Literal_Number13 122e7 is        [<int>]
                    #T Literal_Number14 122e14 is       [<long>]
                    #T Literal_Number15 122e21 is       [<Double>]
                    #T Literal_Number16 x = -122e21; x < 0 && x is [<Double>]
                    #T Literal_Number17 000000000000000000050 is [<int>]
                    #T Literal_Number18 x = -000000000000000000050; x < 0 && x is [<int>]

                    # Numeric literals
                    if ($res = [Tokenizer]::next([TokenKind]::number)) {
                        return [Parser]::NumberHelper($res);
                    }
                }
            }
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($exception -is [TinyException]) {
                if ($null -eq $exception.Token) {
                    $exception.Token = [Token]::New()
                }
                throw $exception
            }
            Write-Host -fore red "Unexpected error in valueRule at $([tokenizer]::PositionMessage())"
            $_ | Format-List * -force | Out-Host
            throw $_.Exception
        }
        return $null
    }

    #
    # Utility function to convert number tokens into Literals.
    #
    static [Literal] NumberHelper([Token] $res) {
        $val = $res.value -replace "_"
        $num = 0
        if ($val -match '\.' -and $null -ne ($num = $val -as [double])) {
            return [Literal]::new($res, $num)
        }
        elseif ($res.Value[-1] -eq 'i') {
            # Support BigInt literals
            return [Literal]::new($res,
                $res.value.Substring(0, $res.Value.Length-1) -as [BigInt])
        }
        elseif ($null -ne ($num = $val -as [int])) {
            return [Literal]::new($res, $num)
        }
        elseif ($null -ne ($num = $val -as [long])) {
            return [Literal]::new($res, $num)
        }
        elseif ($null -ne ($num = $val -as [double])) {
            return [Literal]::new($res, $num)
        }
        else {
            errorMessage "Invalid numeric literal '$($res.value)'." $res
            return $null
        }
    }

    #G ArgumentList = Expression [ ',' Expression ] ;;
    #G
    static [object] ArgumentListRule () {
        return [parser]::argumentListRule($false)
    }

    static [object] ArgumentListRule ($commaOptional) {
        try {
            if ($val = [parser]::ExpressionRule()) {
                $list = [System.Collections.Generic.List[object]]::new()
                $list.Add($val)
                while ([tokenizer]::AtEof() -eq $false) {
                    $comma = [tokenizer]::next([TokenKind]::comma)
                    if ( -not $comma ) { break }
                    $val = [parser]::ExpressionRule()
                    if (-not $val) {
                        parseError "Missing value after ',' in value list."
                    }
                    $list.Add($val)
                }
                return $list
            }
            else {
                return $null
            }
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($exception -is [TinyException]) {
                if ($null -eq $exception.Token) {
                    $exception.Token = [Token]::New()
                }
                throw $exception
            }
            Write-Host -fore red "Unexpected error in argumentListRule at $([tokenizer]::PositionMessage())"
            $_ | Format-List -force * | Out-Host
            throw $_.Exception
        }
        return $null
    }

    #T Operator_Dot1 "abc" . length == 3
    #T Operator_Dot2 {a:'abc'}.a.length == 3
    #T Operator_Dot3 "abcde".substring(1,3) == 'bcd'
    #T Operator_Dot4 "abcde".substring(1,3).length == 3
    #T Operator_QuestionDot1 "abc" ?. length == 3
    #T Operator_QuestionDot2 null ?. length == null
    #T Operator_QuestionDot3 null ?. length . length == null
    #T Operator_QuestionDot4 null?.substring(1,3) == null
    #T Operator_QuestionDot5 null?.substring(1,3).length == null
    static [Expression] methodRule([Expression] $lval, [Token] $operator) {
        try {
            $expr = $null
            if ($operator.Value -eq '(') {
                $argumentList = [parser]::argumentListRule()
                if (-not ([tokenizer]::next([TokenKind]::rparen))) {
                    parseError "Missing ')' after method parameter list."
                }
                $expr = [MethodInvocation]::new($operator, $lval, $argumentList)
            }
            elseif ($operator.Value -eq '{') {
                $syncPoint = [tokenizer]::Offset
                if ($null -ne [tokenizer]::Next([TokenKind]::Name) -and $null -ne [tokenizer]::Next([TokenKind]::Colon)) {
                    [tokenizer]::Offset = $syncPoint
                    $dict = [parser]::ObjectLiteralRule($operator)
                    $expr = [MethodInvocation]::new($operator, $lval, $dict)
                }
                else {
                    [tokenizer]::Offset = $syncPoint
                    $lambda = [parser]::lambdaLiteralRule($operator)
                    $expr   = [MethodInvocation]::new($operator, $lval, @($lambda))
                }
            }
            else {
                errorMessage -dummy "Invalid method invocation operator $($operator.Value)"
            }
            return $expr
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($exception -is [TinyException]) {
                if ($null -eq $exception.Token) {
                    $exception.Token = [Token]::New()
                }
                throw $exception
            }
            Write-Host -fore red "Unexpected error in methodRule at $([tokenizer]::PositionMessage())"
            $_ | Format-List -force * | Out-Host
            throw $_.Exception
        }
        return $null
    }

    #G ParameterList = Name [ ',' Name ] * ;;
    #G
    static [List[Assignable]] parameterListRule () {
        return [parser]::parameterListRule($false)
    }

    static [List[Assignable]] parameterListRule([bool] $commasOptional) {
        $list = [List[Assignable]]::new()
        try {
            $expr = [parser]::parameterRule()

            if ($null -eq $expr) {
                return $list
            }

            $list.Add($expr)
            $comma = $null
            while ($true) {
                if ($commasOptional) {
                    $comma = [tokenizer]::next([TokenKind]::comma)
                }
                elseif ($null -eq ($comma = [tokenizer]::next([TokenKind]::comma))) {
                    break
                }

                $expr = [Parser]::parameterRule()

                if ($null -eq $expr) {
                    if ($comma) {
                        parseError "Missing parameter after , in parameter declaration"
                    }
                    else {
                        break
                    }
                }

                $list.Add($expr)
            }
        }
        catch {
            $exception = $_.Exception
            while ($exception -is [MethodInvocationException]) {
                $exception = $exception.GetBaseException()
            }
            if ($exception -is [TinyException]) {
                if ($null -eq $exception.Token) {
                    $exception.Token = [Token]::New()
                }
                throw $exception
            }
            Write-Host -fore red "Unexpected error in parameterListRule at $([tokenizer]::PositionMessage())"
            $_ | Format-List -force * | Out-Host
            throw $_.Exception
        }
        return $list
    }

    #T Parameter_TypeConstraint1 undef foo; def foo [<int>] x -> "int"; foo(123) == 'int' && foo('hi') == null
    static [Expression] parameterRule() {
        $syncPoint = [tokenizer]::Offset
        $typeConstraint = [tokenizer]::Next([TokenKind]::typeliteral)
        $expr = [parser]::UnaryOperatorRule()
        if ($null -ne $typeConstraint)  {
            if ($expr -is [Variable]) { #BUGBUGBUG need to deal with constants too.
                $type = $typeConstraint.Value.substring(2, $typeConstraint.Value.Length-3) -as [type]
                # Create a new type-constrained variable expression
                $expr = [TypeConstrainedVariable]::new($expr.Token, $type)
            } elseif ($expr -is [Constant]) {
                errorMessage -dummy "Type constraints can''t be applied to constants ($($expr.Name), only to variables."
            }

            #BUGBUGBUG - enabling this error causes parsing problems with code like:
            #                { [<System.Diagnostics.StopWatch>].New() }
            #else {
            #    errorMessage -dummy 'Type constraints can only be applied to variables.'
            #}
        }

        while ($true) {
            $op = $null
            if ($null -eq ($op = [tokenizer]::Next([TokenKind]::patOp))) {
                if ($expr -is [Assignable]) {

                    if (($assign = [tokenizer]::Next([TokenKind]::Operator)) -and $assign.Value -eq '=') {
                        # if we see 'name =' look for an initializer expression
                        $initializer = [parser]::UnaryOperatorRule()
                        if ($null -eq $initializer) {
                            errorMessage "Missing initializer value after '=' operator in parameter definition." $assign 
                        }

                        $expr.InitialValue = $initializer
                    }
                    elseif ($assign) {
                        [Tokenizer]::Unget($assign)
                    }

                    return $expr
                }
                elseif ($expr -is [LiteralBase] -and ($expr -isnot [Literal] -or $expr.Value -isnot [TinyLambda])) {
                    return [pattern]::new($null, @($expr, $null))
                }
                else {
                    [tokenizer]::Offset = $syncPoint
                    return $null
                }
            }
            $syncPoint = [tokenizer]::Offset
            $nextExpr = [parser]::unaryOperatorRule()
            if ($op -and $null -eq $nextExpr) {
                parseError "Missing pattern fragment in parameterRule"
            }

            $expr = [BinOp]::New($expr, $nextExpr, $op, $op.Value).Eval()
        }
        return $null
    }

    #G ParameterDeclaration = '(' ParameterList ')' | ParameterList ;;
    #G
    static [object] ParameterDeclarationRule() {
        $lparen = [tokenizer]::next([TokenKind]::lparen)
        $list = [parser]::parameterListRule($true)
        if ($lparen -and -not ([tokenizer]::next([TokenKind]::rparen))) {
            parseError "Missing ')' in function parameter list."
        }
        return $list
    }

    static [bool] IsAssignOp([Token] $token) {
        if (-not $token) { return $false }
        $val = $token.Value
        return ($val -eq '=' -or
                $val -eq '+=' -or
                $val -eq '-=' -or
                $val -eq '*=' -or
                $val -eq '/=' -or
                $val -eq '%=' -or
                $val -eq '?=')
    }

    #G Expression = Value [ BinaryOperator  Expression ] * ;;
    #G
    static [Expression] ExpressionRule() {
        $OperatorStack = [stack]::New()
        $ValueStack    = [stack]::New()
        [bool] $wasAssignOp   = $false
        while ($true) {
            if ($wasAssignOp) {
                $wasAssignOp = $false

                # For an assignment operation, see if the next token is a keyword. If so, call statementRule
                # so you can do the following:  n = try { someFunc() } catch { "default" }
                $keyword = [Tokenizer]::Next([TokenKind]::Keyword)

                if ($null -ne $keyword) {
                    [Tokenizer]::Unget($keyword)
                    $expr = [Parser]::statementRule()
                } else {
                    $expr = [parser]::UnaryOperatorRule()
                }
            }
            else {
                $expr = [parser]::UnaryOperatorRule()
            }

            if (-not $expr) {
                if ($operatorStack.Count -gt 0) {
                    parseError "Missing operand for binary operator '$($operatorStack.Peek().Value)'."
                }
                break
            }

            $valueStack.Push($expr)
            $operator = $null
            :outer_expr_loop while ($true) {
                # only allow a string or regex as an operator after a variable
                # e.g. function "foo"  but not "foo" "foo"; "foo"("foo") is 'o.k.'
                $operator = $null
                if ($valueStack.Peek() -is [Variable] -or $valueStack.Peek() -is [Constant] ) {
                    $operator = [tokenizer]::next([TokenKind]::String)
                    if (-not $operator) {
                        $operator = [tokenizer]::next([TokenKind]::Regex)
                    }
                }

                if (-not $operator) {
                    $operator = [tokenizer]::next([TokenKind]::Operator)
                }

                if (-not $operator) {
                    break
                }

                $wasAssignOp = [parser]::IsAssignOp($operator)

                if ($operator.Kind -eq [TokenKind]::String -or
                    $operator.Kind -eq [TokenKind]::Regex -or
                    $operator.value -match '\?\[|\[|\(|\{')
                {

                    #BUGBUGBUG - this should be handled by normal precedence handing.
                    #BUGBUGBUG - it will fail with patterns for example
                    # Got function or array reference operator; need to reduce the TOS if it's properties
                    if ($operator.Value -match '\?\[|\[') {
                        while ($true) {
                            $tosval = if ($operatorStack.Count) { $operatorStack.Peek().Value } else { $null }
                            if (-not ($tosval -eq '.' -or $tosval -eq '?.' -or $tosval -eq '*.' -or $tosval -eq '?*.')) {
                                break
                            }
                            $v2 = $valuestack.pop()
                            $v1 = $valuestack.pop()
                            $stackop = $operatorstack.Pop()

                            if ($stackOp.Value -eq ".") {
                                $valueStack.push([PropertyExpression]::new($stackOp, $v1, $v2, $false, $false))
                            }
                            elseif ($stackOp.Value -eq "*.") {
                                $valueStack.push([PropertyExpression]::new($stackOp, $v1, $v2, $false, $true))
                            }
                            elseif ($stackOp.Value -eq "?*.") {
                                $valueStack.push([PropertyExpression]::new($stackOp, $v1, $v2, $true, $true))
                            }
                            else {
                                $valueStack.push([PropertyExpression]::new($stackOp, $v1, $v2, $true, $false))
                            }
                        }
                    }

                    $tosval = if ($operatorStack.Count) { $operatorStack.Peek().Value } else { $null }
                    if (($tosval -eq '.' -or $tosval -eq '?.' -or $tosval -eq '*.' -or $tosval -eq '?*.') -and
                    $operator.Value -notmatch '\?\[|\[') {
                        if ($operator.Kind -eq [TokenKind]::String -or $operator.Kind -eq [Tokenkind]::Regex) {
                            errorMessage "Invalid token for method invocation '$($operator.Value)'" $operator
                        }
                        $expr = [parser]::MethodRule($valueStack.Pop(), $operator)
                        $valueStack.Push($expr)
                    }
                    else {
                        $vPeek = $valueStack.Peek()
                        if ($operator.Value -eq '{' -and ($vPeek -isnot [Variable] -and $vPeek -isnot [Constant])) {
                            [tokenizer]::unget($operator)
                            $operator = $null
                            break 'outer_expr_loop'
                        }
                        $expr = [parser]::FunctionCallOrIndexRule($valueStack.pop(), $operator)
                        $valueStack.Push($expr)

                        # Don't chain strings i.e.  foo 'a' 'b' 'c' should be foo 'a'; 'b'; 'c' not (((foo 'a') 'b') 'c')
                        # The only thing that should come after this (other than a lambda or hash which is already
                        # handled) is an operator. Likewise, 'abc' {2+2} should not be treated as a function call
                        # but rather as two discrete values whereas abc {2+2} IS a function call
                        if ($operator.Kind -eq [TokenKind]::String -or $operator.Kind -eq [TokenKind]::Regex) {
                            continue outer_expr_loop
                        }
                    }
                }
                else {
                    break
                }
            }

            if (-not $operator) { break }

            while ($operatorstack.count -and
                    ([parser]::getprec($operatorstack.Peek().value) -ge [parser]::getprec($operator.Value))) {
                $v2 = $valuestack.pop()
                $v1 = $valuestack.pop()
                $stackop = $operatorstack.Pop()
                if ($stackOp.Value -eq ".") {
                    $valueStack.push([PropertyExpression]::new($stackOp, $v1, $v2, $false, $false))
                }
                elseif ($stackOp.Value -eq "?.") {
                    $valueStack.push([PropertyExpression]::new($stackOp, $v1, $v2, $true, $false))
                }
                elseif ($stackOp.Value -eq "*.") {
                    $valueStack.push([PropertyExpression]::new($stackOp, $v1, $v2, $false, $true))
                }
                elseif ($stackOp.Value -eq "?*.") {
                    $valueStack.push([PropertyExpression]::new($stackOp, $v1, $v2, $true, $true))
                }
                elseif ([parser]::IsAssignOp($stackOp)) {
                    # Fold LHS cast operations into type-constrained variable operations
                    if ($v1 -is [CastOperator] -and $v1.Expr -is [Variable]) {
                        $v1 = [TypeConstrainedVariable]::new($v1.Expr.Token, $v1.Type)
                    }
                    elseif ($v1 -isnot [Assignable] -and $v1 -isnot [Assignment] -and -not ($v1 -is [BinOp] -and $v1.Op -eq '::') ) {
                        errorMessage ("The left hand side of an assignment operation must " +
                            "be something like a variable, " +
                            "array or property value not $($v1.GetType())") $stackOp
                    }
                    # Fold a pattern expression into a constant pattern object at compile time
                    elseif ($v1 -is [BinOp] -and $v1.Op -eq '::') {
                        $v1 = $v1.Eval()
                    }

                    $valuestack.push([Assignment]::new($v1, $v2, $stackOp, $stackOp.Value))
                }
                elseif ($stackOp.Value -eq '+') {
                        $valuestack.push([AddOp]::new($v1, $v2, $stackOp, $stackOp.Value))
                }
                # handle function calls used as infix operators e.g. 2 `times` 3
                elseif ($stackOp.Value[0] -eq '`') {
                    $valueStack.Push([FunctionCall]::new($stackOp, $stackOp.Value.Trim('`'), @($v1, $v2), $null))
                }
                else {
                    $valuestack.push([BinOp]::new($v1, $v2, $stackOp, $stackOp.Value))
                }
            }
            $operatorstack.Push($operator)
        }

        # drain the stacks (BUGBUG this duplicates the reduction code above - could be factored into a helper method).
        while ($operatorstack.Count) {
            if ($valueStack.count -lt 2) {
                parseError "Missing operand for binary operator '$($operatorStack.Peek().Value)' during draining."
            }
            $v2 = $valuestack.pop()
            $v1 = $valuestack.pop()
            $operator = $operatorstack.Pop()
            if ($operator.Value -eq ".") {
                $valueStack.push([PropertyExpression]::new($operator, $v1, $v2, $false, $false))
            }
            elseif ($operator.Value -eq "?.") {
                 $valueStack.push([PropertyExpression]::new($operator, $v1, $v2, $true, $false))
            }
            elseif ($operator.Value -eq "*.") {
                 $valueStack.push([PropertyExpression]::new($operator, $v1, $v2, $false, $true))
            }
            elseif ($operator.Value -eq "?*.") {
                 $valueStack.push([PropertyExpression]::new($operator, $v1, $v2, $true, $true))
            }
            elseif ([parser]::IsAssignOp($operator)) {
                # Fold LHS cast operations into type-constrained variable operations
                if ($v1 -is [CastOperator] -and $v1.Expr -is [Variable]) {
                    $v1 = [TypeConstrainedVariable]::new($v1.Expr.Token, $v1.Type)
                }
                elseif ($v1 -isnot [Assignable] -and $v1 -isnot [Assignment] -and -not ($v1 -is [BinOp] -and $v1.Op -eq '::') ) {
                    errorMessage ("The left hand side of an assignment operation must " +
                        "be something like a variable, " +
                        "array or property value not $($v1.GetType())") $operator
                }
                # Fold a pattern expression into a constant pattern object at compile time
                elseif ($v1 -is [BinOp] -and $v1.Op -eq '::') {
                    $v1 = $v1.Eval()
                }

                $valuestack.push([Assignment]::new($v1, $v2, $operator, $operator.Value))
            }
            elseif ($operator.Value -eq '+') {
                    $valuestack.push([AddOp]::new($v1, $v2, $operator, $operator.Value))
            }
            # handle function calls used as infix operators e.g. 2 `times` 3
            elseif ($operator.Value[0] -eq '`') {
                $valueStack.Push([FunctionCall]::new($operator, $operator.Value.Trim('`'), @( $v1, $v2), $null))
            }
#BUGBUGBUGBUGBUGBUGBUG
#            elseif ($operator.Value -eq '::') {
#                $patOp = [BinOp]::new($v1, $v2, $operator, $operator.Value)
#                $pattern = $patOp.Eval()
#                $valueStack.Push([PatternLiteral]::new($operator, $pattern))
#            }
#BUGBUGBUGBUGBUG
            else {
                $valuestack.push([BinOp]::new($v1, $v2, $operator, $operator.Value))
            }
        }

        # and return the resulting value
        if ($valuestack.Count -gt 0) {
            return $valuestack.Pop()
        }
        else {
            return $null
        }
    }

    #
    # Get the precedence of an operator
    #
    static [int] GetPrec([string] $operator) {
        # Infix function calls e.g. 2 `plus` 3 have fixed precedence 5
        # BUGBUGBUG the precedence here should be a symbol value not a literal constant
        if ($operator[0] -eq '`') {
            return 5
        }
        return $script:OperatorTable[$operator].Prec
    }

    #T Statement_If1 (if (false) { 10 }) == null
    #T Statement_If2 (if (true) { 10 }) == 10
    #T Statement_If3 r = if (true) { 'true' } else { 'false' }; r == 'true'
    #T Statement_If4 r = if (false) { 'true' } else { 'false' }; r == 'false'
    #T Statement_If5 n=1 ; (if (n == 1) { 'one' } elseif (n == 2) { 'two' } else { 'three' }) == 'one'
    #T Statement_If6 n=2 ; (if (n == 1) { 'one' } elseif (n == 2) { 'two' } else { 'three' }) == 'two'
    #T Statement_If7 n=3 ; (if (n == 1) { 'one' } elseif (n == 2) { 'two' } else { 'three' }) == 'three'
    #G IfStatement = 'if' '(' Expression ')' '{' StatementList '}'
    #G               ['elseif' '(' Expression ')' '{' StatementList '}'] *
    #G               ['else' '{' StatementList '}' ] ;;
    #G
    static [IfStatement] ifStatementRule($keyword) {
        $condExpr, $ifBody, $elseBody = $null
        $currentChar = [tokenizer]::CurrentCharacter()
        if ($currentChar -ne '(') {
            parseError "Missing opening '(' in 'if' statement."
        }
        else {
            [tokenizer]::Offset++
        }

        if (-not ($condExpr = [parser]::expressionrule())) {
            parseError "Missing or invalid condition expression in 'if' statement."
        }

        $currentChar = [tokenizer]::CurrentCharacter()
        if ($currentChar -ne ')') {
            parseError "Missing closing ')' in 'if' statement."
        }
        else {
            [tokenizer]::Offset++
        }

        $currentChar = [tokenizer]::CurrentCharacter()
        if ($currentChar -ne '{') {
            parseError "Missing '{' in 'if' statement."
        }
        else {
            [tokenizer]::Offset++
        }

        if (-not ($ifBody = [parser]::statementListRule())) {
            parseError "Missing or invalid 'if' body in 'if' statement."
        }

        $currentChar = [tokenizer]::CurrentCharacter()
        if ($currentChar -ne '}') {
            parseError "Missing '}' in 'if' statement body."
        }
        else {
            [tokenizer]::Offset++
        }

        if ($elseif = [tokenizer]::next([TokenKind]::elseif)) {
           $elseBody = [parser]::ifStatementRule($elseIf)
        }
        elseif ([tokenizer]::next([TokenKind]::else)) {
            $currentChar = [tokenizer]::CurrentCharacter()
            if ($currentChar -ne '{') {
                parseError "Missing '{' in 'else' clause."
            }
            else {
                [tokenizer]::Offset++
            }


            if (-not ($elseBody = [parser]::statementListRule())) {
                parseError "Missing or invalid body in 'else' clause."
            }

            $currentChar = [tokenizer]::CurrentCharacter()
            if ($currentChar -ne '}') {
                parseError "Missing '}' in 'else' clause."
            }
            else {
                [tokenizer]::Offset++
            }
        }
        return [ifStatement]::new($keyword, $condExpr, $ifBody, $elseBody)
    }

    #T Statement_While1 i=5; while (i > 0) { i -= 1 }; i == 0
    # Verify that the while loop returns a value
    #T Statement_While2 i=5; (while (i > 0) { i -= 1; i}) == [4 .. 0]
    #T Statement_While3 i=5; while (i > 0) { break; i -= 1 }; i == 5
    #G WhileStatement = 'while' '(' Expression ')' '{' StatementList '}' ;;
    #G
    static [WhileStatement] whileStatementRule($keyword) {
        if (-not ([tokenizer]::next([TokenKind]::lparen))) {
            parseError "Missing opening '(' in 'while' statement"
        }

        if (-not ($condExpr = [parser]::expressionrule())) {
            parseError "Missing or invalid conditional expression in 'while' statement"
        }

        if (-not ([tokenizer]::next([TokenKind]::rparen))) {
            parseError "Missing closing ')' in 'while' statement"
        }

        if (-not ([tokenizer]::next([TokenKind]::lcurly))) {
            parseError "Missing '{' in 'while' statement"
        }

        if (-not ($Body = [parser]::statementListRule())) {
            parseError "Missing or invalid body in 'while' statement"
        }

        if (-not ([tokenizer]::next([TokenKind]::rcurly))) {
            parseError "Missing '}' in 'while' statement"
        }

        return [whilestatement]::new($keyword, $condExpr, $body)
    }

    #T Statement_Foreach1 (foreach (i in 1..5) { i * 2 }) == [2, 4, 6, 8, 10]
    #T Statement_Foreach2 foreach (i in 5..1) { i * 2 ; break} i == 5
    #T Statement_Foreach3 s = 0; (foreach (i in 1..5) { s += i * 2 }) == null
    #T Statement_Foreach4 s = 0; (foreach (i in 1..5) { s += i * 2 }); s == 30
    #  Verify that foreach returns null if the last statement is void
    #T Statement_Foreach5 (foreach (i in 1..5) { _ = i }) == null
    #G ForeachStatement = 'foreach' '(' Name 'in' Expression ')' '{' StatementList '}' ;;
    #G
    static [ForeachStatement] foreachStatementRule($keyWord) {
        if (-not ([tokenizer]::next([TokenKind]::lparen))) {
            parseError "Missing opening '(' in 'foreach' statement"
        }

        $assignable = $null
        $expr = [parser]::expressionRule()

        if ($expr -is [Assignable]) {
            $assignable = $expr
        }
        elseif ($expr -is [BinOP] -and $expr.Token.Value -eq '::' ) {
            $assignable = $expr.Eval()
        }
        else {
            parseError "Missing  or invalid loop variable in 'foreach' statement"
        }

        if (-not ([tokenizer]::next([TokenKind]::in))) {
            parseError "Missing 'in' in 'foreach' statement"
        }

        if (-not ($enumerable = [parser]::expressionrule())) {
            parseError "Missing or invalid array expression in 'foreach' statement"
        }

        if (-not ([tokenizer]::next([TokenKind]::rparen))) {
            parseError "Missing closing ')' in 'foreach' statement"
        }

        if (-not ([tokenizer]::next([TokenKind]::lcurly))) {
            parseError "Missing '{' in 'foreach' statement"
        }

        if (-not ($Body = [parser]::statementListRule())) {
            parseError "Missing or invalid body in 'foreach' statement"
        }

        if (-not ([tokenizer]::next([TokenKind]::rcurly))) {
            parseError "Missing '}' in 'foreach' statement"
        }

        return [foreachStatement]::new($keyword, $assignable, $enumerable, $body)
    }

    # Literal matches
    #T Statement_Match1 (match 1 | 1 -> 'one' | 2 -> 'two' | -> 'default') == 'one'
    #T Statement_Match2 (match 2 | 1 -> 'one' | 2 -> 'two' | -> 'default') == 'two'
    #T Statement_Match3 (match 3 | 1 -> 'one' | 2 -> 'two' | -> 'default') == 'default'
    #T Statement_Match4 (match [1,2,3] | [1,2,3] -> 'matches' | -> 'not match') == 'matches'
    # Regular expressions
    #T Statement_Match5 (match 'abc' | r/a/ -> 'eh' | r/b/ -> 'bee' | -> 'default') == 'eh'
    #T Statement_Match6 (match 'bc'  | r/a/ -> 'eh' | r/b/ -> 'bee' | -> 'default') == 'bee'
    #T Statement_Match7 (match 'c'   | r/a/ -> 'eh' | r/b/ -> 'bee' | -> 'default') == 'default'
    # Pattern match
    #T Statment_Matches8 (match [1,2,3] | a::b::c::_ -> 'matches' | -> 'not match') == 'matches' && a == 1 && b == 2 && c == 3
    #T Statment_Matches9 (match [1,2,3] | a::2::{it == 3}::_ -> 'matches' | -> 'not match') == 'matches' && a == 1
    #T Statement_MatchList1 (matchlist [1,2,3] | {it % 2 == 0} -> 'even' | -> 'odd') == ['odd', 'even', 'odd']
    #G MatchStatement = 'match' Expression [ '|' Expression '->' Statement ] + | '|' '->' statement ;;
    #G # Match expressions can be patterns in which case binding is done
    #G # There can only be one default case which looks line '| -> "I'm the default"'
    #G
    #G MatchListStatement = 'matchlist' Expression [ '|' Expression '->' Statement ] | '|' '->' Statement ;;
    #G # iterates over the a list matching against each item
    #G
    static [MatchStatement] matchStatementRule($keyword) {
        if (-not ($matchValue = [parser]::expressionrule())) {
            parseError "missing or invalid condition expression in 'match' statement."
        }
        $pairs = [OrderedDictionary]::new()
        [Expression] $DefaultAction = $null
        $action = $null
        [bool] $gotDefault = $false
        $matchPatterns = [List[object]]::new()
        while ($true) {
            if (-not ([tokenizer]::next([TokenKind]::orbar))) {
                break
            }

            $matchExpr = [Parser]::ExpressionRule()
            if (-not $matchExpr) {
                if ($arrow = [tokenizer]::next([TokenKind]::arrow)) {
                    if ($gotDefault) {
                        errorMessage "More than one default clause in a 'match' statement" $arrow
                    }
                    [Tokenizer]::Unget($arrow)
                    $gotDefault = $true # used as the value for default
                }
                # No error here - the next if statement will get the missing '->'
            }
            else {
                # Fold cast operations into type-constrained variables.
#BUGBUGBUG add tests for constrained match variables
                if ($matchExpr -is [CastOperator] -and $matchExpr.Expr -is [Variable]) {
                    $matchExpr = [TypeConstrainedVariable]::new($matchExpr.Expr.Token, $matchExpr.Type)
                }

                $matchPatterns.Add($matchExpr)
            }

            if ($orBar = [tokenizer]::Next([TokenKind]::orbar)) {
                [Tokenizer]::Unget($orbar)
                continue
            }

            if (-not ($arrow = [tokenizer]::next([TokenKind]::arrow))) {
                parseError "Missing '->' after expression in 'match' statement clause."
            }

            if (-not ($action = [Parser]::statementRule())) {
                parseError "Missing or invalid action after '->' in 'match' statement clause"
            }

            if ($gotDefault -and -not $defaultAction) {
                $defaultAction = $action
            }
            else {
                foreach ($expr in $matchPatterns) {
                    $pairs[$expr] = $action
                }
                $matchPatterns.Clear()
            }
        }
        # If no pairs were found, throw an incomplete parse error
        if ($pairs.get_Count() -eq 0) {
            parseError "Missing match clause after match statement."
        }

        return [MatchStatement]::new($keyword, $matchValue, $pairs, $defaultAction, $keyword.Value -eq "matchlist")
    }

    #T Statement_FunctionDefinition1 fn foo(x, y) {x + y}; foo(3, 5) == 8
    #T Statement_FunctionDefinition2 fn foo x y {x + y}; foo(3, 5) == 8
    #T Statement_FunctionDefinition3 fn foo x y -> x + y;  foo(3, 5) == 8
    # Test recursion
    #T Statement_FunctionDefinition4 fn fact x -> match x | 0 -> 1 | -> x * fact(x-1); fact(5) == 120
    #T Statement_FunctionDefinition5 fn fib x -> if (x < 3) { 1 } else {fib(x-1) + fib(x-2)}; fib(10) == 55
    #T Statement_FunctionDefinition6 fn mfor l -> foreach (i in l) { body.dot([i], i, _, _) };  mfor (1..5) { it * 2 } == [2, 4, 6, 8, 10]
    #T Statement_FunctionDefinition7 fn foo x::y::_ -> x + y foo([3, 5]) == 8
    #T Statement_FunctionDefinition8 fn foo null -> 'null' foo(null) == 'null'
    #T Statement_FunctionDefinition9 fn foo _ -> 'anything' foo('something') == 'anything'
#BUGBUGBUG - this test is wrong...
    #T Statement_FunctionDefinition10 fn foo 'ten' -> 10 foo('ten') == 10
    #T Statement_Def1 undef fact def fact 0 -> 1; def fact x -> x*fact(x-1); fact(7) == 5040
    #T Statement_Def2 undef foo def foo 1 -> 'one'; def foo 2 -> 'two' def foo x -> 'Other ' + x; foo(1) == 'one' && foo(2) == 'two' && foo(3) == 'Other 3'
    # Test constraining the return type of a function; conversion failure results in null, not an error.
    #T Statement_FunctionDefinition11  fn [<int>] foo -> '123'; foo() is [<int>]
    #T Statement_FunctionDefinition12  fn [<int>] foo -> '123'; foo() == 123
    #T Statement_FunctionDefinition13  fn [<int>] foo -> 'abc'; try { foo(); false } catch { true }
    #T Statement_FunctionDefinition14  fn [<int>] foo x -> x; foo('123') is [<int>]
    #T Statement_FunctionDefinition15  fn [<int>] foo x -> x; foo('123') == 123
    #T Statement_FunctionDefinition16  fn [<int>] foo x -> x; try { foo('abc'); false } catch { true }
    #G FunctionDefinition = 'fn' Name ParameterDeclaration ( '{' StatementList '}' | '->' Statement ) ;;
    #G # The result of a function is the result of the last statement executed (no return statement)
    #G # Examples: 'fn double x -> x * 2' or 'fn add (x, y) { x+y }
    #G
    static functionDefinitionRule ($keyword, [bool] $isConstant) {
        $returnType = [object]
        $typeToken = [Tokenizer]::next([TokenKind]::TypeLiteral)
        if ($null -ne $typeToken) {
            $val = $typeToken.Value
            $returnType = $val.substring(2, $val.Length-4)
        }

        if (-not ($token = [tokenizer]::next([TokenKind]::name))) {
            parseError "Missing function name after 'fn' keyword"
        }

        $localFns = $null
        $body = $null
        $oldLocalFunctions = $script:LocalFunctions
        $params = [parser]::ParameterDeclarationRule()

        # constants (e.g. pid) can be used in parameters for pattern matching but using
        # a constant function probably an error.
        # BUGBUGBUG - maybe this should be a warning?
        foreach ($expr in $params) {
            if ($expr -is [constant] -and ($expr.value -is [PSMethod] -or $expr.Value -is [TinyLambda])) {
                errorMessage `
                    "The constant '$($expr.Name)' is bound to a function and should not be used as a parameter name." `
                        $expr.Token
            }
        }

        try {
            $script:LocalFunctions = [Dictionary[string,object]]::new([StringComparer]::OrdinalIgnoreCase)

            if ([tokenizer]::next([TokenKind]::arrow)) {
                $body = [parser]::stateMentRule()
                if ($null -eq $body) {
                    parseError "Missing or invalid function body after '->' in function declaration"
                }
            }
            else {
                if (-not ([tokenizer]::next([TokenKind]::lcurly))) {
                    parseError "Missing '{' in function declaration."
                }

                if (-not ($body = [parser]::statementListRule())) {
                    parseError "Missing or invalid function body in function declaration"
                }

                if (-not ([tokenizer]::next([TokenKind]::rcurly))) {
                    parseError "Missing close '}' in function declaration."
                }
            }
        }
        finally {
            $localfns = $script:LocalFunctions
            $script:LocalFunctions = $oldLocalFunctions
        }

        $params = [Assignable[]] @($params)

        if ($null -ne $script:LocalFunctions) {
            $script:localfunctions[$token.value] = [TinyLambda]::new($returnType, $params, $body, $localfns)
        }
        else {
            # it's an error to try and rebind a named constant.
            if ($script:TinyConstants.ContainsKey($token.Value)) {
                errorMessage "Can't rebind the constant '$($token.Value)' with a function definition." $token
            }

            $lambda = [TinyLambda]::new($returnType, $params, $body, $localfns);
            if ($keyword.Value -eq 'def') {
                if ($isConstant) {
                    errorMessage "def: cannot define a constant function set for $($token.Value)"
                }

                if ([Tiny]::DontBind -eq $false) {
                    $currentBinding = [scopes]::TryGetVariable($token.Value)
                    if ($currentBinding -is [List[TinyLambda]]) {
                        $currentBinding.Add($lambda)
                    }
                    else {
                        $lambdaList = [List[TinyLambda]]::new()
                        $lambdaList.Add($lambda)
                        [scopes]::scopes[0][$token.Value] = $lambdaList
                    }
                }
            }
            else {
                if ([Tiny]::DontBind -eq $false) {
                    if ($isConstant) {
                        $script:TinyConstants[$token.Value] = $lambda
                    }
                    else {
                        [scopes]::scopes[0][$token.Value] = $lambda
                    }
                }
            }
        }
    }

    #G Statement =    FunctionDefinition | IfStatement        | WhileStatement | ForeachStatement  |
    #G                MatchStatement     | MatchListStatement | BreakStatement | ContinueStatement |
    #G                ThrowStatement     | TryCatchStatement  | ReturnStatement ;;
    #G
    static [Expression] statementRule() {
        try {
            while ($true) {
                if ($keyword = [tokenizer]::next([TokenKind]::keyword)) {
                    switch ($keyword.Value) {
                        'fn'      {
                                    $null = [parser]::functionDefinitionRule($keyword, $false)
                                    # We'll continue parsing so we need to explicitly eat semicolons here
                                    while ([tokenizer]::next([TokenKind]::semicolon)) { }
                        }
                        'def'     {
                                    $null = [parser]::functionDefinitionRule($keyword, $false)
                                    # We'll continue parsing so we need to explicitly eat semicolons here
                                    while ([tokenizer]::next([TokenKind]::semicolon)) { }
                        }
                        'undef'   {
                                    $name = [tokenizer]::next([TokenKind]::Name)
                                    if ($null -eq $name) {
                                        $name = [tokenizer]::next([TokenKind]::string)
                                        if ($null -eq $name) {
                                            parseError "Missing name after the 'undef' keyword."
                                        }
                                    }

                                    # Remove the binding from all scopes and the constant table
                                    [int] $count = [scopes]::Scopes.Count - 1

                                    foreach ($scopeNum in 0 .. $count) {
                                        [scopes]::scopes[$scopeNum].Remove($name.Value)
                                    }

                                    $script:TinyConstants.Remove($name.Value)

                                    # Eat trailing semicolons
                                    while ([tokenizer]::next([TokenKind]::semicolon)) { }
                        }
                        'const'    {
                                    if ($keyword = [tokenizer]::next([TokenKind]::keyword)) {
                                        if ($keyword.Value -eq 'fn') {
                                            $null = [parser]::functionDefinitionRule($keyword, $false)
                                        }
                                        elseif ($keyword.Value -eq 'def') {
                                            $null = [parser]::functionDefinitionRule($keyword, $false)
                                        }
                                        else {
                                            errorMessage ("The 'const' keyword can only be used as part of a " +
                                                "function definition or variable assignment")
                                        }

                                        # We'll continue parsing so we need to explicitly eat semicolons here
                                        while ([tokenizer]::next([TokenKind]::semicolon)) { }
                                    }
                                    else {
                                        $assignExpr = [Parser]::ExpressionRule()
#say "assignexpr: $assignexpr; call stack"
#get-pscallstack | out-host
#say "end of call stack\n"
                                        if (($assignexpr -is [Assignment] -or $assignexpr -is [BinOp]) -and
                                             $assignexpr.Op -eq '=' -and (
                                                $assignexpr.Left -is [Variable] -or
                                                $assignexpr.Left -is [Constant]))
                                        {
                                            # BUGBUGBUG - binds constants immediately. This should probably
                                            # only be allowed at the top level.
                                            $script:TinyConstants[$assignExpr.Left.Name] = $assignExpr.right.Eval();

                                            # We'll continue parsing so we need to explicitly eat semicolons here
                                            while ([tokenizer]::next([TokenKind]::semicolon)) { }
                                        }
                                        else {
                                            errorMessage "The const keyword can only be used for simple variable assignment"
                                        }
                                   }
                        }
                        #T Keyword_Import1 import 'io'; io != null
                        # The import keyword imports a Tiny module into the current environment. Modules are loaded from tinyhome/modules e.g.<pre>import 'console'; console.cls()</pre><pre>import 'utils'; utils.GetProcesses()</pre>
                        'import'   {
                                    $token = [tokenizer]::next([TokenKind]::Name)
                                    if ($null -eq $token) {
                                        $token = [tokenizer]::next([TokenKind]::string)
                                        if ($null -eq $token) {
                                            parseError "Missing name after the 'import' keyword."
                                        }
                                    }
                                    $moduleName = $token.Value.Trim('''"')

                                    [TinyModule]::ImportTinyModule($moduleName);

                                    # Eat trailing semicolons
                                    while ([tokenizer]::next([TokenKind]::semicolon)) { }
                        }
                        'if'      {
                                    return [parser]::ifStatementRule($keyword)
                        }
                        'while'   {
                                    return [parser]::whileStatementRule($keyword)
                        }
                        'foreach' {
                                    return [parser]::foreachStatementRule($keyword)
                        }
                        'match' {
                                    return [parser]::matchStatementRule($keyword)
                        }
                        'matchlist' {
                                    return [parser]::matchStatementRule($keyword)
                        }
                        #G BreakStatement    = 'break' ;;
                        #G
                        'break' {
                                    return [BreakStatement]::New($keyword)
                        }
                        #G ContinueStatement = 'continue' ;;
                        #G
                        'continue' {
                                    return [ContinueStatement]::New($keyword)
                        }
                        #G ReturnStatement   = 'return' [ Expression ] ;;
                        #G
                        'return' {
                                    $valueToReturn = [parser]::expressionRule()
                                    return [ReturnStatement]::New($keyword, $valueToReturn)
                        }
                        #G ThrowStatement    = 'throw' Expression ;;
                        #G
                        'throw' {
                                    $whatToThrow = [parser]::expressionRule()
                                    return [ThrowStatement]::New($keyword, $whatToThrow)
                        }

                        'try' {
                                    return [parser]::tryCatchStatementRule($keyword)
                        }

                        default   {
                            # should never happen
                            throw "Unexpected keyword '$($keyword.Value)': this should never happen"
                        }
                    }
                }
                else {
                    $stmt = [parser]::ExpressionRule()
                    return $stmt
                }
            }
        }
        finally {
            # Eat semicolons
            while ([tokenizer]::next([TokenKind]::semicolon)) { }
        }
        # Here to shut up the compiler
        return $null
    }

    #T Statement_StatementList fn foo {1; 2; 3} foo() == 3
    #G StatementList    = Statement * ;;
    #G
    static [StatementList] statementListRule() {
        $statements = [List[object]]::new()
        $token = $null
        while ($true) {
            if ($null -ne ($stmt = [parser]::statementRule())) {
                $statements.Add($stmt)
                if ($null -eq $token) {
                    $token = $stmt.Token
                }
            }
            else {
                break
            }

            if ($token = [tokenizer]::next([TokenKind]::rcurly)) {
                [tokenizer]::unget($token)
                break
            }
        }

        return [StatementList]::new([token]::new(),
            $(if ($null -ne $statements) { $statements.ToArray() }
              else { $null }))
    }

    #T Statement_TryCatch1 r = try { 'ok' } catch { 'caught' }; r == 'ok'
    #T Statement_TryCatch2 r = try { 1/0 } catch { 'caught' }; r == 'caught'
    #G TryCatchStatement = 'try'   '{' StatementList '}' 'catch' '{' StatementList '}'
    #G                     [ 'finally' '{' StatementList '}' ]
    #G                     ;;
    #G
    static [TryCatchStatement] TryCatchStatementRule ([Token] $keyword) {
        $tryPart = $catchPart = $finallyPart = $null
        if (-not [tokenizer]::next([TokenKind]::lcurly)) {
            parseError "Missing '{' in try/catch statement"
        }

        $tryPart = [parser]::statementListRule()

        if (-not [tokenizer]::next([TokenKind]::rcurly)) {
            parseError "Missing '}' in try/catch statement"
        }

        if ([tokenizer]::next([TokenKind]::catch)) {
            if (-not [tokenizer]::next([TokenKind]::lcurly)) {
                parseError "Missing '{' after catch statement"
            }

            $catchPart = [parser]::statementListRule()

            if (-not ($rcurly = [tokenizer]::next([TokenKind]::rcurly))) {
                parseError` "Missing '}' after catch clause"
            }
        }

        if ([tokenizer]::next([TokenKind]::finally)) {
            if (-not [tokenizer]::next([TokenKind]::lcurly)) {
                parseError "Missing '{' after 'finally' in try/catch statement"
            }

            $finallyPart = [parser]::statementListRule()

            if (-not ($rcurly = [tokenizer]::next([TokenKind]::rcurly))) {
                parseError "Missing '}' after 'finally' in try/catch statement"
            }
        }

        if (-not ($catchPart -or $finallyPart)) {
            parseError "A 'try' statement must have at least one of 'catch' or 'finally' clauses"
        }

        return [TryCatchStatement]::new($keyword, $tryPart, $catchPart, $finallyPart)
    }

} # end parser class


#############################################################################
#
# Static class that provides interfaces to the the Tiny language
#

# Holds a cache of scripts that have been parsed
$script:LoadedScripts = [ordered] @{}

class Tiny {

    # Set from the tiny.ps1 script's parameter
    static [bool] $DetailedErrors

    # Initialize tokenization on the script. (Doesn't actually tokenize.)
    static [void] tokenizeFile ([string] $path) {
        if ([string]::IsNullOrEmpty($path)) {
            throw "Empty string passed to tokenizeFile"
        }
        if (-not ($path -match '\.tiny$')) {
            $path += ".tiny"
        }
        if (-not (Test-Path $path)) { errorMessage "Script file '$path' does not exist." }
        [tokenizer]::FileName = $path
        [tokenizer]::Start((Get-Content -Raw $path))
    }

    # If true, don't bind functions while compiling
    hidden static [bool] $DontBind

    # Parse a string into an expression tree
    static [Expression] Parse($ScriptText) {
        return [Tiny]::Parse($scriptText, $false)
    }

    # Parse a string into an expression tree but if dontBind is set,
    # don't bind the functions defined in the text.
    static [Expression] Parse($ScriptText, $dontBind) {
        [Tokenizer]::Start($scriptText)
        $oldDontBind = [tiny]::DontBind
        try {
            [Tiny]::DontBind = $dontBind
            $statements = [Parser]::StatementListRule()
            if (-not [Tokenizer]::AtEof()) {
                errormessage -dummy "parse failed at unexpected input >>> $([tokenizer]::CurrentString())"
            }
            return $statements
        }
        finally {
            [Tiny]::DontBind = $oldDontBind
        }
    }

    # Parse and evaluate a piece fo tiny text.
    static [object] EvalExpr ([string] $stringToEvaluate) {
        $statementList = [Tiny]::Parse($stringToEvaluate)
        $result = $null
        $result = $statementList.Eval()
        if ($result -is [IList]) {
            return [TinyList]::new($result)
        }
        else {
            return $result
        }
    }

    # Parse, evaluate, then display the results of the evaluation to
    # the user of the API (part of the Tiny repl.)
    static [void] Executor ($expression) {
        try {
            $t = if ($expression -is [Expression]) {
                $expression
            }
            else {
                [Tiny]::Parse($expression)
            }

            # Evaluate and return the results of ALL of the statements
            # This is for interactive use.
            :inner foreach ($n in $t.Statements) {
                if ($n -is [Assignment]) {
                    $null = $n.Eval()
                }
                else {
                    ShowValue $n.Eval()
                }
            }
        }
        catch [IncompleteParseException] {
            Write-Host -fore red ("IncompleteParse: " + $_.Exception.ToString())
        }
        catch [TinyException] {
            Write-Host -fore red ($_.Exception.ToString())
        }
        catch {
            Write-Host -fore red "**UNEXPECTED ERROR:"
            Write-Host -fore red ($_ | format-list -force * | Out-String)
        }
    }

    # Parse a tiny script file.
    static [Expression] ParseFile([string] $path) {
        if (-not $path) {
            errorMessage 'ParseFile: The "path" parameter requires a non-null or empty string.'
        }

        if (-not ($path -match '\.tiny$')) {
            $path += ".tiny"
        }
    
        # $oldPath = [Tokenizer]::FileName
        $statements = $null
        try {
            if (-not (Test-Path $path)) { errorMessage "Script file '$path' does not exist." }
            $content = Get-Content -Raw $path

            # BUGBUGBUG - because of the way functions are loaded, can't simply cache the expression tree
            #             We also need to record all functions and modules loaded by the script
            #if ($script:LoadedScripts.Contains($content)) {
#            if ($false) {
#                # The cache contains the script so just return the parsed statements
#                $statements = $script:LoadedScripts[$content]
#            }
#            else {
                [Tokenizer]::FileName = $path
                [Tokenizer]::Start($content)
                $statements = [Parser]::StatementListRule()
                if (-not [tokenizer]::AtEof()) {
                    errormessage -dummy "parse failed at unexpected input >>> $([tokenizer]::CurrentString())"
                }
#                # Add the compiled script to the cache
#                $script:LoadedScripts[$content] = $statements
#            }
        }
        finally {
            #BUGBUGBUG this doesn't work properly.
            # [Tokenizer]::Filename = $oldPath
        }
        return $statements
    }

    #
    # Utility routine to handle TinyLists in a boolean context. Normal
    # PowerShell rule don't work becasuse TinyList is not an enumerable. 
    # BUGBUGBUG need to use this routine everywhere we check for boolean
    #
    static [bool] AsBool($obj) {
        if ($obj -is [TinyList]) {
            return $obj.list.count -ne 0
        }
        return [bool] $obj
    }
}
[Tiny]::DetailedErrors = $DetailedErrors

########################################################################################

function TinyTabExpansion {
    param ($prefix)

#say "\ntte prefix: $prefix\n"
    if ($prefix -match '\.\w*$') {
        $pattern = ($prefix -replace '^.*\.') + "*" 
#say "pattern is $pattern"
        $exprstr = $prefix -replace '\.\w*$'

        try {
            $expr = [tiny]::parse($exprstr, $true)
            if ($expr -is [StatementList]) {
                $expr = $expr.Statements[0]
            }
            else {
                return $null
            }

            if ($expr -and $expr -isnot [Statement]) {
                $object = $expr.Eval()
            }
            else {
                return $null
            }
        }
        catch {
            return $null
        }

#say "object is $object"

        if ($object -is [IDictionary]) {
            [object[]] $result = @(@($object.get_Keys()).
                where{ $_ -like $pattern }.
                foreach{
                    $mem = $object[$_]
                    if ($mem -is [TinyLambda] -or
                        $mem -is [PSMethod]    -or
                        $mem -is [CommandInfo] -or
                        $mem -is [ScriptBlock]
                    ) {
                        $exprstr + '.' + $_ + '('
                    }
                    else {
                        $exprstr + '.' + $_
                    }
                })
#say "Processing dictionary for $pattern : matched $result"
            return $result
        }

        if ($object -is [type]) {
            $type = $object
            $memberType = "static,instance,public"
        }
        else {
            $type = $object.GetType()
            $memberType = "instance,public"
        }

        [object[]] $result = @(@($type.GetMembers($memberType)).
                             where{ $_.name -like $pattern -and $_.name -notmatch '_'}.
                             foreach{
                                    $mem = $_


                                    if ($mem.MemberType -eq 'method') {
                                        $exprstr + '.' + $mem.Name + "("
                                    }
                                    else {
                                        $exprstr + "." + $mem.Name
                                    }
                                })
#say "Processing type $type result is $result"
        return $result
    }
#say "returning null"

    return $null
}


$completerScriptBlock = {
    param ($prefix)

    # function completion
    [object[]] $res = @([scopes]::vars().List -like "$prefix*").foreach{
        $val = [scopes]::GetVariable($_)
#say "val $($val.gettype().name) $val"
        if ($val -is [PSMethod] -or $val -is [TinyLambda] -or $val -is [CommandInfo]) {
            "$_("
        }
        else {
            "$_"
        }
    }

    if (-not $res) {
        $res = TinyTabExpansion $prefix
#say "back from tte $res"
    }

    # Path completion
    if (-not $res) {
        $cwd = (Get-Location).path
        $addQuotes = ''
        if ($prefix -match "^'") {
            $prefix = $prefix.substring(1)
            $addQuotes = "'"
        }

        $res = Get-ChildItem "$prefix*" | foreach {
            $name = $_.fullname
            if ($name.startswith($cwd)) {
                $name = $name.substring($cwd.length+1)
            }
            "${addQuotes}$name${addQuotes}"
        }
    }

#say "expansion returning $res"

    return @($res)
}

########################################################################################

function repl {

    # In the REPL well load a profile
    $profilePath = Join-Path $PWD '_profile.tiny'
    $script:TinyConstants['__profile'] = $profilePath
    if (Test-Path $profilePath) {
        try {
            [Tiny]::Executor([Io.File]::ReadAllText($profilePath))
        }
        catch {
            Write-Host -fore red $_.ToString()
        }
    }
    
    Write-Host -fore green "Welcome to Tiny! (A tiny PowerShell language)."
    Write-Host -fore green " (type ~q to exit or ? to get help)`n"
    [Environment]::CurrentDirectory = (Get-Location).Path

    #BUGBUG load the line editor
    try {
        if ($PSVersionTable.PSEdition -eq 'desktop') {
                [void] [Reflection.Assembly]::LoadFrom((resolve-path './LineEditor.dll'))
        }
        else {
            Add-Type (Get-Content -Raw (Join-Path $PSScriptRoot 'lineeditor.cs'))
        }
    }
    catch {
        "Line editor failed to load: $_"
    }

    $lineEditor = [Tiny.LineEditor]::new('tiny', 100)
    [scopes]::scopes[0]['__LineEditor'] = $lineEditor

    #
    # This is a wrapper that allows for a Tiny function to be
    # used as the prompt function.
    #
    $promptScriptBlock = {
        if ([scopes]::TryGetVariable("prompt")) {
            $promptStr = [scopes]::scopes[0]["prompt"]
            if ($promptStr -is [TinyLambda]) {
                $promptStr = $promptStr.Invoke()
            }
        }
        else {
            $promptStr = "Tiny -> "
        }
        $promptStr
    }

    #
    # Enter the reply. Note the very specific labels. These are used
    # by Tiny for non-local gotos.
    #
    try {
        :outer do {
            :current_execution do {
                :return_from_function while ($true) {
                    # Garbage collect before each prompt.
                    [void] [gc]::GetTotalMemory($true)

                    # Synchronize the process and PowerShell directories.
                    [environment]::CurrentDirectory = (Get-Location).Path
                    $host.UI.RawUI.WindowTitle = "Tiny -> {$(Get-Location)}"
    
                    # prepare the list for tab completion.
                    $bindings = [Dictionary[string,object]]::new()
                    foreach ($name in [scopes]::vars().list) {
                        $bindings[$name] = [scopes]::GetVariable($name)
                    }
                    [Tiny.LineEditor]::TinyVars = $bindings
                    [Tiny.LineEditor]::TinyCompleter = $completerScriptBlock

                    try {
                        $expression = $lineEditor.Edit($promptScriptBlock)
                    }
                    catch {
                        $_ | Format-List -force * | out-string | write-host -fore red
                        $expression = ""
                    }

                    if ($null -eq $expression) {
                        # Should never get a null so if we do, somethings wrong
                        break
                    }
    
                    try {
                        switch -regex ($expression) {
                            "^~q" {
                                break outer
                            }
                            "^(help|\?)" {
    
                                switch -regex ($expression) {
                                    '^(help|\?) *$' {
                                        Write-Host @"
   Tiny REPL Interactive Commands:

?                       Get this help.
? syntax                Get the Tiny language grammar
? html                  Launch the Tiny HTML documentation in the browser
? functions             List all of the predefined functions
? operators             List all of the predefined operators

~q                      Quit the interpreter.

~h [number | pattern ]  List command history; if number is specified,
                        only list that number of history items. If a pattern
                        is specified, only print history items matching that
                        pattern

~r [number | pattern ]  Rerun the command from history as specified by the number.
                        If no number is specified, then run the last command. If
                        a pattern is specified, run the first command matching that pattern.

~v [number | pattern ]  Edit the command history element corresponding to the number using
                        vim. If no number is specified, the last command is edited. If a
                        pattern is specified, edit the first item matching the pattern.

! text...               Run a PowerShell command. THe rest of the line after the space is
                        treated as PowerShell script text to evaluate.

\                       Multiline mode - allows you to paste multiline text directly into
                        the REPL. Exit multiline mode with ';;' on its own line.

"@
                                        break
                                    }
                                    '^(help|\?) *syn' {
                                        Write-Host -fore green "The Tiny grammar"
                                        Write-Host -fore green "================"
                                        (Get-Content $myinvocation.scriptname) `
                                            -match '^ *#g' `
                                            -replace '^ *#G' | Out-Host
                                        break
                                    }
                                    '^(help|\?) *html' {
                                        Start-Process (Join-Path $PSScriptRoot 'Tiny Documentation.html')
                                    }
                                    '^(help|\?) *f[^ ]* ([a-z][a-z0-9]*)' {
                                        $pattern = $matches[2]
                                        $functionTable['functions'].Invoke($pattern) | Write-Host -fore yellow
                                    }
                                    '^(help|\?) *o[^ ]* ([a-z][a-z0-9]*)' {
                                        $pattern = $matches[2]
                                        $functionTable['operators'].Invoke($pattern) | Write-Host -fore yellow
                                    }
                                }
                                continue return_from_function
                            }
                            '^~h' {
                                # print out the command history
                                $number = 30
                                $pattern = ''
                                if ($expression -match '^~h +([0-9]+)') {
                                    $number = [int] $matches[1]
                                }
                                elseif ($expression -match '^~h +/(.*$)') {
                                    $pattern = $matches[1]
                                }
                                $lineEditor.CommandHistory.Dump() |
                                    Where-Object   { $_ -match $pattern } |
                                    Select-Object  -Last $number |
                                    ForEach-Object { Write-Host $_ }
    
                                continue return_from_function
                            }
                            '^~v' {
                                # Edit a history command with vim
                                $tinyHistory = $lineEditor.CommandHistory.Dump()
                                $index = -1
                                $pattern = $null
                                if ($expression -match '^~v +([0-9]+)') {
                                    $index = [int] $matches[1]
                                }
                                elseif ($expression -match '^~v +/(.*$)') {
                                    $pattern = $matches[1]
                                }
    
                                if ($null -ne $pattern) {
                                    $index = $tinyHistory.Count - 1
                                    while ($index-- -ge 0) {
                                        if ($tinyHistory[$index] -match $pattern) {
                                            break
                                        }
                                    }
                                }
    
                                $strToEdit = $tinyHistory[$index]
                                $file = 'c:\temp\tinyCommandHistory.tiny'
                                $strToEdit > $file
                                vim $file
                                $expression = Get-Content -Raw $file
                                $expression -split "\n" | foreach {
                                    Write-Host -fore yellow "> $_"
                                }
                                Write-Host ''
                                [Tiny]::Executor($expression)
                            }
                            '^~r' {
                                # Re-run a command
                                $index = -1
                                if ($expression -match '^~r +([0-9]+)') {
                                    $index = [int] $matches[1]
                                }
                                elseif ($expression -match '^~r +/(.*$)') {
                                    $pattern = $matches[1]
                                }
                                else {
                                    write-host "command syntax: ~r <int>"
                                    continue return_from_function
                                }
    
                                $tinyHistory = $lineEditor.CommandHistory.Dump()
                                if ($null -ne $pattern) {
                                    $index = $tinyHistory.Count - 1
                                    while ($index-- -ge 0) {
                                        if ($tinyHistory[$index] -match $pattern) {
                                            break
                                        }
                                    }
                                }
    
                                $expression = $tinyHistory -match "^ *${index}:" -replace '^[ 0-9]*:'
                                $expression -split "\n" | foreach {
                                    Write-Host -fore yellow "> $_"
                                }
                                Write-Host ''
                                [Tiny]::Executor($expression)
                            }
                            "^~g$" {
                                Write-Host -fore green "The Tiny grammar"
                                Write-Host -fore green "================"
                                (Get-Content $myinvocation.scriptname) `
                                    -match '^#g' `
                                    -replace '^#G' | Out-Host
                                continue return_from_function
                            }
                            "^!" {
                                $cmd = $_.substring(1)
                                if ($null -ne $cmd) {
                                    Invoke-Expression $cmd | Out-Host
                                }
                                else {
                                    Write-Host "usage: ! powershell command..."
                                }
                                continue return_from_function
                            }
                            "^\\$" {
                                write-host "Multi-line mode (';;' on a separate line to quit)"
                                $expression = ' '
                                while (($line = Read-Host) -ne ';;') {
                                    $expression += "`n" + $line
                                }
                                [Tiny]::Executor($expression)
                                break
                            }
                            default {
                                while ($true) {
                                    try {
                                        $parsedExpr = [Tiny]::parse($expression)
                                        [Tiny]::Executor($parsedExpr)
                                        break
                                    }
                                    catch [IncompleteParseException] {
                                        if ([Tokenizer]::AtEof()) {
                                            $nextLine = $lineEditor.Edit({'>>> '})
                                            if ($nextLine -eq ';;') {
                                                errorMessage $_
                                            }
                                            else {
                                                $expression += "`n" + $nextLine
                                            }
                                        }
                                        else {
                                            errorMessage $_
                                        }
                                    }
                                }
                            }
                        }
                    }
                    catch [TinyException] {
                        Write-Host -fore red ($_.Exception.ToString())
                    }
                    catch {
                        $_ | Format-List * -force | out-host
                        Write-Error $_
                    }
                }
            }
            while ($true)
        }
        while ($false)
    }
    finally {
        $lineEditor.Close()
    }
}

# If a script to run was specified, run it then exit
if ($scriptToRun) {
    try {
        $sw = [system.diagnostics.Stopwatch]::new()
        $sw.Start()
        $functionTable['call'].InvokeReturnAsIs(@($scriptToRun) + $TinyScriptArguments)
        $sw.Stop()
        $ms = $sw.Elapsed.TotalMilliSeconds
    }
    catch {
        Write-host -fore red $_.Exception.Message
    }

    if ($time) {
        Write-Host -fore green "The expression took $ms milliseconds to run."
    }

    exit
}

# If an expression to evaluate was specified, run it then exit.
if ($expression) {
    try {
        $sw = [system.diagnostics.Stopwatch]::new()
        $sw.start()
        [tiny]::evalexpr($expression)
        $sw.Stop()
        $ms = $sw.Elapsed.TotalMilliSeconds
    }
    catch {
        Write-host -fore red $_.Exception.Message
    }

    if ($time) {
        Write-Host -fore green "The expression took $ms milliseconds to run."
    }
    exit
}

# At this point, unless -norepl has been specified, start the repl.
if (-not $norepl) {
    repl
}


