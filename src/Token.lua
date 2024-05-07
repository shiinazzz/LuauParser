local Token = {}
Token.__index = Token

export type TokenPositionInfo = {
	Index: number,
	Line: number,
	Column: number
}

export type TokenPosition = {
	Start: TokenPositionInfo,
	End: TokenPositionInfo
}

export type TokenSyntaxKind = number

local DummyTokenPositionInfo: TokenPositionInfo = {
	Index = -1,
	Line = -1,
	Column = -1
}
local DummyPosition: TokenPosition = {
	Start = DummyTokenPositionInfo,
	End = DummyTokenPositionInfo
}

Token.SyntaxKind = {
	Invalid = 0,
	
	-- Reserved
	And = 1,
	Or = 2,
	Not = 3,
	True = 4,
	False = 5,
	Nil = 6,
	Local = 7,
	Do = 8,
	Function = 9,
	In = 10,
	For = 11,
	While = 12,
	Repeat = 13,
	Until = 14,
	If = 15,
	Then = 16,
	Else = 17,
	ElseIf = 18,
	End = 19,
	Break = 20,
	Return = 21,
	Continue = 22,
	
	-- Operators
	Plus = 23,
	Minus = 24,
	Star = 25,
	Slash = 26,
	DoubleSlash = 27,
	Modulo = 28,
	Hashtag = 29,
	Equal = 30,
	Caret = 31,

	PlusEqual = 32,
	MinusEqual = 33,
	StarEqual = 34,
	SlashEqual = 35,
	DoubleSlashEqual = 36,
	ModuloEqual = 37,
	NotEqual = 38,
	EqualTo = 39,
	LessThan = 40,
	LessEqual = 41,
	GreaterThan = 42,
	GreaterEqual = 43,
	CaretEqual = 44,

	Comma = 45,
	SemiColon = 46,
	Colon = 47,
	Dot = 48,
	Dot2 = 49,
	Dot2Equal = 50,
	Dot3 = 51,

	DoubleColon = 52,
	SkinnyArrow = 53,
	Pipe = 54,
	Ampersand = 55,
	QuestionMark = 56,

	LeftParen = 57,
	RightParen = 58,
	LeftBracket = 59,
	RightBracket = 60,
	LeftBrace = 61,
	RightBrace = 62,
	
	-- Misc
	String = 63,
	BrokenString = 64,
	LongString = 65,
	InterpolatedStringStart = 66,
	InterpolatedStringMiddle = 67,
	InterpolatedStringBrokenDoubleBrace = 68,
	InterpolatedStringEnd = 69,
	InterpolatedStringSimple = 70,
	Comment = 71,
	
	Number = 72,
	
	Identifier = 73,
	
	EndOfFile = 74
} :: { [string]: number }

function Token.__tostring(self)
	local KindName = "<unknown>"

	-- Normalized names
	if self.Kind == Token.SyntaxKind.EndOfFile then
		return "<eof>"
	elseif self.Kind == Token.SyntaxKind.Equal then
		return "'=='"
	elseif self.Kind == Token.SyntaxKind.LessEqual then
		return "'<='"
	elseif self.Kind == Token.SyntaxKind.GreaterEqual then
		return "'>='"
	elseif self.Kind == Token.SyntaxKind.NotEqual then
		return "'~='"
	elseif self.Kind == Token.SyntaxKind.Dot2 then
		return "'..'"
	elseif self.Kind == Token.SyntaxKind.Dot3 then
		return "'...'"
	elseif self.Kind == Token.SyntaxKind.SkinnyArrow then
		return "'->'"
	elseif self.Kind == Token.SyntaxKind.DoubleColon then
		return "'::'"
	elseif self.Kind == Token.SyntaxKind.DoubleSlash then
		return "'//'"
	elseif self.Kind == Token.SyntaxKind.PlusEqual then
		return "'+='"
	elseif self.Kind == Token.SyntaxKind.MinusEqual then
		return "'-='"
	elseif self.Kind == Token.SyntaxKind.StarEqual then
		return "'*='"
	elseif self.Kind == Token.SyntaxKind.SlashEqual then
		return "'/='"
	elseif self.Kind == Token.SyntaxKind.DoubleSlashEqual then
		return "'//='"
	elseif self.Kind == Token.SyntaxKind.CaretEqual then
		return "'^='"
	elseif self.Kind == Token.SyntaxKind.Dot2Equal then
		return "'..='"
	elseif self.Kind == Token.SyntaxKind.String then
		return self.Characters and string.format("'%s'", self.Characters) or "string"
	elseif self.Kind == Token.SyntaxKind.InterpolatedStringStart then
		return self.Characters and string.format("`%s{", self.Characters) or "the beginning of an interpolated string"
	elseif self.Kind == Token.SyntaxKind.InterpolatedStringMiddle then
		return self.Characters and string.format("}%s{", self.Characters) or "the middle of an interpolated string"
	elseif self.Kind == Token.SyntaxKind.InterpolatedStringEnd then
		return self.Characters and string.format("}%s`", self.Characters) or "the end of an interpolated string"
	elseif self.Kind == Token.SyntaxKind.InterpolatedStringSimple then
		return self.Characters and string.format("'%s'", self.Characters) or "interpolated string"
	elseif self.Kind == Token.SyntaxKind.Number then
		return self.Characters and string.format("`%s`", self.Characters) or "string"
	elseif self.Kind == Token.SyntaxKind.String then
		return self.Characters and string.format("'%s'", self.Characters) or "number"
	elseif self.Kind == Token.SyntaxKind.Identifier then
		return self.Characters and string.format("'%s'", self.Characters) or "identifier"
	elseif self.kind == Token.SyntaxKind.Comment then
		return "comment"
	elseif self.kind == Token.SyntaxKind.BrokenString then
		return "malformed string"
	elseif self.kind == Token.SyntaxKind.InterpolatedStringBrokenDoubleBrace then
		return "'{{', which is invalid (did you mean '\\{'?)"
	end
	
	for Kind, Identifier in Token.SyntaxKind :: {[string]: number} do
		if Identifier == self.Kind then
			KindName = Kind
			break
		end
	end

	return string.lower(KindName)
end

function Token.new(syntaxKind: TokenSyntaxKind, position: TokenPosition, characters: string): Token
	return setmetatable({
		Kind = syntaxKind,
		Position = position,
		Characters = characters
	}, Token)
end

export type Token = typeof(Token.new(-1, {} :: any, ""))

Token.Invalid = Token.new(Token.SyntaxKind.Invalid, DummyPosition, "")

return Token