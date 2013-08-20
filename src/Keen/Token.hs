module Keen.Token where


data Token
    = TokenMacroBegin String
    | TokenMacroEnd
    | TokenLower String
    | TokenUpper String
    | TokenInt Int
    | TokenParenthesisBegin
    | TokenParenthesisEnd
    | TokenBracketBegin
    | TokenBracketEnd
    | TokenBraceBegin
    | TokenBraceEnd
    | TokenIs
    | TokenBind
    | TokenColon
    | TokenComma
    | TokenWildcard

