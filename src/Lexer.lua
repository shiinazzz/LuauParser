type Array<T> = { [number]: T }

local Token = require(script.Parent.Token)

local Lexer = {}
Lexer.__index = Lexer

Lexer.WhitespaceCharacters = " \t\n\r\f"
Lexer.ValidCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
Lexer.AlphabetCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
Lexer.DigitCharacters = "0123456789"
Lexer.ValidNumberCharacters = "0123456789abcdefABCDEF"
Lexer.ReservedKeywords = {
	["and"] = Token.SyntaxKind.And,
	["or"] = Token.SyntaxKind.Or,
	["not"] = Token.SyntaxKind.Not,
	["true"] = Token.SyntaxKind.True,
	["false"] = Token.SyntaxKind.False,
	["nil"] = Token.SyntaxKind.Nil,
	["local"] = Token.SyntaxKind.Local,
	["do"] = Token.SyntaxKind.Do,
	["function"] = Token.SyntaxKind.Function,
	["in"] = Token.SyntaxKind.In,
	["for"] = Token.SyntaxKind.For,
	["while"] = Token.SyntaxKind.While,
	["repeat"] = Token.SyntaxKind.Repeat,
	["until"] = Token.SyntaxKind.Until,
	["if"] = Token.SyntaxKind.If,
	["then"] = Token.SyntaxKind.Then,
	["else"] = Token.SyntaxKind.Else,
	["elseif"] = Token.SyntaxKind.ElseIf,
	["end"] = Token.SyntaxKind.End,
	["break"] = Token.SyntaxKind.Break,
	["return"] = Token.SyntaxKind.Return,
	["continue"] = Token.SyntaxKind.Continue
}
Lexer.Operators = {
	[1] = {
		["+"] = Token.SyntaxKind.Plus,
		["-"] = Token.SyntaxKind.Minus,
		["*"] = Token.SyntaxKind.Star,
		["/"] = Token.SyntaxKind.Slash,
		["%"] = Token.SyntaxKind.Modulo,
		["#"] = Token.SyntaxKind.Hashtag,
		["="] = Token.SyntaxKind.Equal,
		["^"] = Token.SyntaxKind.Caret,

		["<"] = Token.SyntaxKind.LessThan,
		[">"] = Token.SyntaxKind.GreaterThan,

		[","] = Token.SyntaxKind.Comma,
		[";"] = Token.SyntaxKind.SemiColon,
		[":"] = Token.SyntaxKind.Colon,
		["."] = Token.SyntaxKind.Dot,

		["|"] = Token.SyntaxKind.Pipe,
		["&"] = Token.SyntaxKind.Ampersand,
		["?"] = Token.SyntaxKind.QuestionMark,

		["("] = Token.SyntaxKind.LeftParen,
		[")"] = Token.SyntaxKind.RightParen,
		["["] = Token.SyntaxKind.LeftBracket,
		["]"] = Token.SyntaxKind.RightBracket,
		["{"] = Token.SyntaxKind.LeftBrace,
		["}"] = Token.SyntaxKind.RightBrace
	},
	[2] = {
		["+="] = Token.SyntaxKind.PlusEqual,
		["-="] = Token.SyntaxKind.MinusEqual,
		["*="] = Token.SyntaxKind.StarEqual,
		["/="] = Token.SyntaxKind.SlashEqual,
		["%="] = Token.SyntaxKind.ModuloEqual,
		["~="] = Token.SyntaxKind.NotEqual,
		["=="] = Token.SyntaxKind.EqualTo,
		["<="] = Token.SyntaxKind.LessEqual,
		[">="] = Token.SyntaxKind.GreaterEqual,
		["^="] = Token.SyntaxKind.CaretEqual,

		[".."] = Token.SyntaxKind.Dot2,
		["..="] = Token.SyntaxKind.Dot2Equal,

		["::"] = Token.SyntaxKind.DoubleColon,
		["->"] = Token.SyntaxKind.SkinnyArrow
	},
	[3] = {
		["..."] = Token.SyntaxKind.Dot3
	}
} :: { [number]: { [string]: Token.TokenSyntaxKind } }

function Lexer.new(source: string): Lexer
	local SourceLines = string.split(source, "\n")
	local LinesSizePreprocessed: Array<number> = table.create(#SourceLines + 1, 0)
	for i = 1, #SourceLines do
		LinesSizePreprocessed[i + 1] = LinesSizePreprocessed[i] + #SourceLines[i] + 1
	end

	return setmetatable({
		Source = source,
		Position = 1,
		Line = 1,
		Column = 1,
		LinesSizePreprocessed = LinesSizePreprocessed,

		BraceStack = {} :: Array<boolean>
	}, Lexer)
end

export type Lexer = typeof(Lexer.new(""))

function Lexer.NextCharacter(self: Lexer, count: number): string
	local characters = self:Peek(count - 1)

	self.Position += count
	if self.LinesSizePreprocessed[self.Line + 1] and self.Position >= self.LinesSizePreprocessed[self.Line + 1] then
		while self.LinesSizePreprocessed[self.Line + 1] and self.Position >= self.LinesSizePreprocessed[self.Line + 1] do
			self.Line += 1
		end
	end
	self.Column = self.Position - self.LinesSizePreprocessed[self.Line]

	return characters
end

function Lexer.Match(self: Lexer, characters: string): boolean
	return self.Source:sub(self.Position, self.Position + #characters - 1) == characters
end

function Lexer.Validate(self: Lexer, characters: string): string?
	if self:Match(characters) then
		self.Position += #characters
		if self.LinesSizePreprocessed[self.Line + 1] and self.Position >= self.LinesSizePreprocessed[self.Line + 1] then
			while self.LinesSizePreprocessed[self.Line + 1] and self.Position >= self.LinesSizePreprocessed[self.Line + 1] do
				self.Line += 1
			end
		end
		self.Column = self.Position - self.LinesSizePreprocessed[self.Line]

		return characters
	end

	return nil
end

function Lexer.Expect(self: Lexer, characters: string): string
	local match = self:Validate(characters)
	if match == nil then
		error(`Expected {characters}`)
	end

	return match
end

function Lexer.Peek(self: Lexer, offset: number): string
	local position: number = self.Position
	return self.Source:sub(position, position + offset)
end

function Lexer.PeekAt(self: Lexer, startPosition: number, offset: number): string
	return self.Source:sub(startPosition, startPosition + offset)
end

function Lexer.NextNumber(self: Lexer, start: Token.TokenPositionInfo): Token.Token
	local prefix = self:Validate("0b") or self:Validate("0B") or self:Validate("0x") or self:Validate("0X") or ""
	local characters: Array<string> = {}
	local nextCharacter
	repeat
		table.insert(characters, self:NextCharacter(1))
		if self:Validate(".") ~= nil then
			table.insert(characters, ".")
		end

		nextCharacter = self:Peek(0)
	until not Lexer.ValidNumberCharacters:find(nextCharacter, 1, true) or self.Position > #self.Source

	return Token.new(Token.SyntaxKind.Number, {
		Start = start,
		End = {
			Index = self.Position,
			Line = self.Line,
			Column = self.Column
		}
	}, prefix .. table.concat(characters))
end

function Lexer.NextIdentifier(self: Lexer, start: Token.TokenPositionInfo): Token.Token
	local characters: Array<string> = {}
	while Lexer.ValidCharacters:find(self:Peek(0), 1, true) and self.Position <= #self.Source do
		table.insert(characters, self:NextCharacter(1))
	end

	return Token.new(Token.SyntaxKind.Identifier, {
		Start = start,
		End = {
			Index = self.Position,
			Line = self.Line,
			Column = self.Column
		}
	}, table.concat(characters))
end

function Lexer.SkipLongSeparator(self: Lexer)
	local StartChar = self:Peek(0)
	assert(StartChar == "[" or StartChar == "]")
	self:NextCharacter(1)

	local count = 0
	while self:Validate("=") do
		count += 1
	end

	return StartChar == self:Peek(0) and count or -count - 1
end

function Lexer.ReadLongString(self: Lexer, separator: number): string
	self:Expect("[")

	local characters: Array<string> = {}

	while self.Position <= #self.Source do
		if self:Peek(0) == "]" then
			if self:SkipLongSeparator() == separator then
				self:Expect("]")
				return table.concat(characters)
			end
		else
			table.insert(characters, self:NextCharacter(1))
		end
	end

	error("Broken string")
end

function Lexer.NextComment(self: Lexer, start: Token.TokenPositionInfo): Token.Token
	if self:Match("[") then
		local separator = self:SkipLongSeparator()
		if separator >= 0 then
			return Token.new(Token.SyntaxKind.Comment, {
				Start = start,
				End = {
					Index = self.Position,
					Line = self.Line,
					Column = self.Column
				}
			}, self:ReadLongString(separator))
		end
	end
	local characters: Array<string> = {}
	while self:Validate("\n") == nil and self.Position <= #self.Source do
		table.insert(characters, self:NextCharacter(1))
	end

	return Token.new(Token.SyntaxKind.Comment, {
		Start = start,
		End = {
			Index = self.Position,
			Line = self.Line,
			Column = self.Column
		}
	}, table.concat(characters))
end

function Lexer.NextLongString(self: Lexer, start: Token.TokenPositionInfo, separator: number): Token.Token
	return Token.new(Token.SyntaxKind.LongString, {
		Start = start,
		End = {
			Index = self.Position,
			Line = self.Line,
			Column = self.Column
		}
	}, self:ReadLongString(separator))
end

function Lexer.NextString(self: Lexer, start: Token.TokenPositionInfo): Token.Token
	local quote = self:Validate("'") or self:Validate('"')
	local characters: Array<string> = {}
	while self:Validate(quote :: string) == nil and self.Position <= #self.Source do
		local character = self:NextCharacter(1)
		if character == "\\" then
			character = self:NextCharacter(1)
		end

		table.insert(characters, character)
	end

	return Token.new(Token.SyntaxKind.String, {
		Start = start,
		End = {
			Index = self.Position,
			Line = self.Line,
			Column = self.Column
		}
	}, table.concat(characters))
end

function Lexer.NextInterpolatedStringSection(self: Lexer, start: Token.TokenPositionInfo, formatType: Token.TokenSyntaxKind, endType: Token.TokenSyntaxKind): Token.Token
	local characters: Array<string> = {}
	while self:Validate("`") == nil do
		local character = self:Peek(0)
		if character == "" or character == "\r" or character == "\n" then
			return Token.new(Token.SyntaxKind.BrokenString, {
				Start = start,
				End = {
					Index = self.Position,
					Line = self.Line,
					Column = self.Column
				}
			}, table.concat(characters))
		elseif character == "\\" then
			if self:Peek(1) == "u{" then
				table.insert(characters, character)
				table.insert(characters, self:NextCharacter(1))
				table.insert(characters, self:NextCharacter(1))
				character = ""
			else
				character = self:NextCharacter(1)
			end
		elseif character == "{" then
			table.insert(self.BraceStack, true)
			if self:PeekAt(start.Index + 1, 0) == "{" then
				local BrokenBrace = Token.new(Token.SyntaxKind.InterpolatedStringBrokenDoubleBrace, {
					Start = start,
					End = {
						Index = self.Position,
						Line = self.Line,
						Column = self.Column
					}
				}, table.concat(characters))
				self:NextCharacter(2)
				return BrokenBrace
			end
			self:NextCharacter(1)

			return Token.new(formatType, {
				Start = start,
				End = {
					Index = self.Position,
					Line = self.Line,
					Column = self.Column
				}
			}, table.concat(characters))
		end

		table.insert(characters, self:NextCharacter(1))
	end

	return Token.new(endType, {
		Start = start,
		End = {
			Index = self.Position,
			Line = self.Line,
			Column = self.Column
		}
	}, table.concat(characters))
end

function Lexer.NextInterpolatedString(self: Lexer, start: Token.TokenPositionInfo): Token.Token
	self:Expect("`")
	return self:NextInterpolatedStringSection(start, Token.SyntaxKind.InterpolatedStringStart, Token.SyntaxKind.InterpolatedStringSimple)
end

function Lexer.NextToken(self: Lexer): Token.Token?
	local start: Token.TokenPositionInfo = {
		Index = self.Position,
		Line = self.Line,
		Column = self.Column
	}

	if self:Validate("--") then
		return self:NextComment(start)
	end
	if self:Match("'") or self:Match('"') then
		return self:NextString(start)
	end
	if self:Match("[") then
		local separator = self:SkipLongSeparator()
		if separator >= 0 then
			return self:NextLongString(start, separator)
		else
			self:NextCharacter(-1)
		end
	end
	if self:Match("`") then
		return self:NextInterpolatedString(start)
	end
	if self:Match(".") and Lexer.DigitCharacters:find(self:PeekAt(start.Index + 1, 0), 1, true) then
		return self:NextNumber(start)
	end

	for i = #Lexer.Operators, 1, -1 do
		for operator, syntaxKind in pairs(Lexer.Operators[i]) do
			if self:Validate(operator) then
				if syntaxKind == Token.SyntaxKind.LeftBrace then
					if #self.BraceStack == 0 then
						table.insert(self.BraceStack, false)
					end
				elseif syntaxKind == Token.SyntaxKind.RightBrace then
					if #self.BraceStack > 0 then
						if table.remove(self.BraceStack, #self.BraceStack) == true then
							return self:NextInterpolatedStringSection(start, Token.SyntaxKind.InterpolatedStringMiddle, Token.SyntaxKind.InterpolatedStringEnd)
						end
					end
				end

				return Token.new(syntaxKind, {
					Start = start,
					End = {
						Index = self.Position,
						Line = self.Line,
						Column = self.Column
					}
				}, operator)
			end
		end
	end
	for keyword, syntaxKind in pairs(Lexer.ReservedKeywords) do
		local nextWord = self:PeekAt(start.Index + #keyword, 0)

		if (not Lexer.ValidCharacters:find(nextWord, 1, true) or nextWord == "") and self:Validate(keyword) ~= nil then -- TODO optimise but this is necessary, it can't be removed
			return Token.new(syntaxKind, {
				Start = start,
				End = {
					Index = self.Position,
					Line = self.Line,
					Column = self.Column
				}
			}, keyword)
		end
	end

	do
		local character = self:Peek(0)
		if Lexer.WhitespaceCharacters:find(character, 1, true) then
			self:NextCharacter(1)
			return Token.Invalid
		end
		if Lexer.DigitCharacters:find(character, 1, true) then
			return self:NextNumber(start)
		end
		if Lexer.AlphabetCharacters:find(character, 1, true) then
			return self:NextIdentifier(start)
		end
	end

	return nil
end

function Lexer.Scan(self: Lexer): Array<Token.Token>
	local tokens = {}
	while self.Position <= #self.Source do
		local token = self:NextToken()
		if token == nil then break end

		if token.Kind ~= Token.SyntaxKind.Invalid then
			table.insert(tokens, token)
		end
	end

	table.insert(tokens, Token.new(Token.SyntaxKind.EndOfFile, {
		Start = {
			Index = self.Position,
			Line = self.Line,
			Column = self.Column
		},
		End = {
			Index = self.Position,
			Line = self.Line,
			Column = self.Column
		}
	}, "\0"))

	return tokens
end

return Lexer