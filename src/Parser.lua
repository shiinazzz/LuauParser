--!nonstrict
type Array<T> = { [number]: T }
type Dictionary<T1, T2> = { [T1]: T2 }

local Token = require(script.Parent.Token)
local SyntaxNode = require(script.Parent.SyntaxNode)

local Parser = {}
Parser.__index = Parser

Parser.ExitScopeTokens = {
	Token.SyntaxKind.EndOfFile,
	Token.SyntaxKind.End,
	Token.SyntaxKind.Until,
	Token.SyntaxKind.Else,
	Token.SyntaxKind.ElseIf
}
Parser.BuiltInTypesTokens = {
	[Token.SyntaxKind.True] = SyntaxNode.SyntaxNodeKind.True,
	[Token.SyntaxKind.False] = SyntaxNode.SyntaxNodeKind.False,
	[Token.SyntaxKind.Nil] = SyntaxNode.SyntaxNodeKind.Nil,
	[Token.SyntaxKind.Dot3] = SyntaxNode.SyntaxNodeKind.Dot3
}
Parser.CompoundOperators = {
	[Token.SyntaxKind.PlusEqual] = SyntaxNode.CompoundOperationKind.Add,
	[Token.SyntaxKind.MinusEqual] = SyntaxNode.CompoundOperationKind.Subtract,
	[Token.SyntaxKind.StarEqual] = SyntaxNode.CompoundOperationKind.Multiply,
	[Token.SyntaxKind.SlashEqual] = SyntaxNode.CompoundOperationKind.Divide,
	[Token.SyntaxKind.ModuloEqual] = SyntaxNode.CompoundOperationKind.Modulo,
	[Token.SyntaxKind.CaretEqual] = SyntaxNode.CompoundOperationKind.Power,
	[Token.SyntaxKind.Dot2Equal] = SyntaxNode.CompoundOperationKind.Concaternate,
}
Parser.UnaryOperators = {
	[Token.SyntaxKind.Hashtag] = SyntaxNode.UnaryOperationKind.Length,
	[Token.SyntaxKind.Not] = SyntaxNode.UnaryOperationKind.Not,
	[Token.SyntaxKind.Minus] = SyntaxNode.UnaryOperationKind.Negate,
}
Parser.BinaryOperators = {
	[Token.SyntaxKind.Plus] = SyntaxNode.BinaryOperationKind.Add,
	[Token.SyntaxKind.Minus] = SyntaxNode.BinaryOperationKind.Subtract,
	[Token.SyntaxKind.Star] = SyntaxNode.BinaryOperationKind.Multiply,
	[Token.SyntaxKind.Slash] = SyntaxNode.BinaryOperationKind.Divide,
	[Token.SyntaxKind.Modulo] = SyntaxNode.BinaryOperationKind.Modulo,
	[Token.SyntaxKind.Caret] = SyntaxNode.BinaryOperationKind.Power,
	[Token.SyntaxKind.Dot2] = SyntaxNode.BinaryOperationKind.Concaternate,
	[Token.SyntaxKind.NotEqual] = SyntaxNode.BinaryOperationKind.CompareNotEqual,
	[Token.SyntaxKind.EqualTo] = SyntaxNode.BinaryOperationKind.CompareEqual,
	[Token.SyntaxKind.LessThan] = SyntaxNode.BinaryOperationKind.CompareLessThan,
	[Token.SyntaxKind.LessEqual] = SyntaxNode.BinaryOperationKind.CompareLessEqual,
	[Token.SyntaxKind.GreaterThan] = SyntaxNode.BinaryOperationKind.CompareGreaterThan,
	[Token.SyntaxKind.GreaterEqual] = SyntaxNode.BinaryOperationKind.CompareGreaterEqual,
	[Token.SyntaxKind.And] = SyntaxNode.BinaryOperationKind.And,
	[Token.SyntaxKind.Or] = SyntaxNode.BinaryOperationKind.Or,
}
Parser.BinaryOperatorValues = {
	[SyntaxNode.BinaryOperationKind.Add] = 1,
	[SyntaxNode.BinaryOperationKind.Subtract] = 2,
	[SyntaxNode.BinaryOperationKind.Multiply] = 3,
	[SyntaxNode.BinaryOperationKind.Divide] = 4,
	[SyntaxNode.BinaryOperationKind.Modulo] = 5,
	[SyntaxNode.BinaryOperationKind.Power] = 6,
	[SyntaxNode.BinaryOperationKind.Concaternate] = 7,
	[SyntaxNode.BinaryOperationKind.CompareNotEqual] = 8,
	[SyntaxNode.BinaryOperationKind.CompareEqual] = 9,
	[SyntaxNode.BinaryOperationKind.CompareLessThan] = 10,
	[SyntaxNode.BinaryOperationKind.CompareLessEqual] = 11,
	[SyntaxNode.BinaryOperationKind.CompareGreaterThan] = 12,
	[SyntaxNode.BinaryOperationKind.CompareGreaterEqual] = 13,
	[SyntaxNode.BinaryOperationKind.And] = 14,
	[SyntaxNode.BinaryOperationKind.Or] = 15,
}

Parser.LValueSyntaxNodeKinds = {
	SyntaxNode.SyntaxNodeKind.Identifier,
	SyntaxNode.SyntaxNodeKind.IndexIdentifier
}

export type ParserError = {
	Message: string,
	Position: Token.TokenPosition
}

export type ParserResult = {
	Tree: SyntaxNode.SyntaxNode,
	Errors: Array<ParserError>
}

function Parser.new(tokens: Array<Token.Token>): Parser
	local recoveryStopOnToken: Dictionary<Token.TokenSyntaxKind, number> = {}
	for _, syntaxKind in Token.SyntaxKind do
		recoveryStopOnToken[syntaxKind] = 0
	end

	return setmetatable({
		Tokens = tokens :: Array<Token.Token>,
		CurrentToken = nil :: Token.Token?,
		EndMismatchSuspect = Token.new(Token.SyntaxKind.EndOfFile, {
			Start = {
				Index = -1,
				Line = -1,
				Column = -1
			},
			End = {
				Index = -1,
				Line = -1,
				Column = -1
			}
		}, "") :: Token.Token,
		TokenPosition = {
			Start = {
				Index = -1,
				Line = -1,
				Column = -1
			},
			End = {
				Index = -1,
				Line = -1,
				Column = -1
			}
		} :: Token.TokenPosition,
		Index = 0,

		ScopeLevel = 0,
		RecoveryStopOnToken = recoveryStopOnToken,
		LocalVariablesStack = {} :: Array<Array<string>>,
		GlobalVariablesStack = {} :: Array<string>,
		Errors = {} :: Array<ParserError>
	}, Parser)
end

export type Parser = typeof(Parser.new({}))

function Parser.NextToken(self: Parser)
	local index: number = self.Index
	index += 1
	self.CurrentToken = self.Tokens[index]
	while self:Peek(Token.SyntaxKind.Comment) do
		index += 1
		self.CurrentToken = self.Tokens[index]
	end

	if self.CurrentToken then
		self.TokenPosition = self.CurrentToken.Position
	end

	self.Index = index
end

function Parser.ReportError(self: Parser, message: string)
	table.insert(self.Errors, {
		Message = message,
		Position = self.TokenPosition
	})
end

function Parser.ReportExpressionError(self: Parser, message: string, tokenPosition: Token.TokenPosition?)
	self:ReportError(message)

	local tokenPosition = tokenPosition or self.TokenPosition
	return SyntaxNode.MakeError(message, false, tokenPosition)
end

function Parser.ReportStatementError(self: Parser, message: string, tokenPosition: Token.TokenPosition?)
	self:ReportError(message)

	local tokenPosition = tokenPosition or self.TokenPosition
	return SyntaxNode.MakeError(message, true, tokenPosition)
end

function Parser.ValidateFail(self: Parser, syntaxKind: Token.TokenSyntaxKind, beginToken: Token.Token, suggestionString: string?)
	local token = Token.new(syntaxKind, {
		Start = {
			Index = -1,
			Line = -1,
			Column = -1
		},
		End = {
			Index = -1,
			Line = -1,
			Column = -1
		}
	}, "")

	if self.TokenPosition.Start.Line == beginToken.Position.Start.Line then
		self:ReportError(`Expected {token} (to close {beginToken} at column {beginToken.Position.Start.Column + 1}, got {self.CurrentToken}{suggestionString or ""}`)
	else
		self:ReportError(`Expected {token} (to close {beginToken} at line {beginToken.Position.Start.Line}, got {self.CurrentToken}{suggestionString or ""}`)
	end
end

function Parser.ValidateEndFail(self: Parser, syntaxKind: Token.TokenSyntaxKind, beginToken: Token.Token)
	if self.EndMismatchSuspect.Kind ~= Token.SyntaxKind.EndOfFile and self.EndMismatchSuspect.Position.Start.Line > beginToken.Position.Start.Line then
		self:ValidateFail(syntaxKind, beginToken, `; did you forget to close {self.EndMismatchSuspect} at line {self.EndMismatchSuspect.Position.Start.Line + 1}?`)
	else
		self:ValidateFail(syntaxKind, beginToken)
	end
end

function Parser.ValidateRecover(self: Parser, syntaxKind: Token.TokenSyntaxKind, beginToken: Token.Token, searchForRecovery: boolean?): boolean
	if searchForRecovery then
		local currentLine = (self.Tokens[self.Index - 1] or self.Tokens[#self.Tokens]).Position.Start.Line
		local currentSyntaxKind = (self.CurrentToken :: Token.Token).SyntaxKind

		while self.CurrentToken and currentLine == self.CurrentToken.Position.Start.Line and currentSyntaxKind ~= syntaxKind and self.RecoveryStopOnToken[syntaxKind] == 0 do
			self:NextToken()
			currentSyntaxKind = self.CurrentToken and self.CurrentToken.SyntaxKind
		end

		if self.CurrentToken and self.CurrentToken.SyntaxKind == syntaxKind then
			self:NextToken()
			return true
		end
	else
		-- Check if this is an extra token and the expected token is next
		if self.CurrentToken and self.CurrentToken.SyntaxKind == syntaxKind then
			-- Skip invalid and consume expected
			self:NextToken()
			self:NextToken()

			return true
		end
	end

	return false
end

function Parser.Validate(self: Parser, syntaxKind: Token.TokenSyntaxKind, beginToken: Token.Token, searchForRecovery: boolean?): boolean
	if self.CurrentToken and self.CurrentToken.Kind == syntaxKind then
		self:NextToken()
		return true
	else
		self:ValidateFail(syntaxKind, beginToken)
		return self:ValidateRecover(syntaxKind, beginToken, searchForRecovery)
	end
end

function Parser.ValidateEnd(self: Parser, syntaxKind: Token.TokenSyntaxKind, beginToken: Token.Token): boolean
	if self.CurrentToken and self.CurrentToken.Kind == syntaxKind then
		-- If the token matches on a different line and a different column, it suggests misleading indentation
		-- This can be used to pinpoint the problem location for a possible future *actual* mismatch
		if self.CurrentToken.Position.Start.Line ~= beginToken.Position.Start.Line and self.CurrentToken.Position.Start.Column ~= beginToken.Position.Start.Column and self.EndMismatchSuspect.Position.Start.Line < beginToken.Position.Start.Line then
			-- Only replace the previous suspect with more recent suspects
			self.EndMismatchSuspect = beginToken
		end

		self:NextToken()
		return true
	else
		self:ValidateEndFail(syntaxKind, beginToken)

		-- Check if this is an extra token and the expected token is next
		if self.CurrentToken and self.CurrentToken.SyntaxKind == syntaxKind then
			-- Skip invalid and consume expected
			self:NextToken()
			self:NextToken()

			return true
		end
	end

	return false
end

function Parser.ExpectFail(self: Parser, syntaxKind: Token.TokenSyntaxKind, context: string?)
	local token = Token.new(syntaxKind, {
		Start = {
			Index = -1,
			Line = -1,
			Column = -1
		},
		End = {
			Index = -1,
			Line = -1,
			Column = -1
		}
	}, "")

	if context then
		self:ReportError(`Expected {token} when parsing {context}, got {self.CurrentToken}`)
	else
		self:ReportError(`Expected {token}, got {self.CurrentToken}`)
	end
end

function Parser.Expect(self: Parser, syntaxKind: Token.TokenSyntaxKind, context: string?): boolean
	local token: Token.Token? = self.CurrentToken
	if token == nil or token.Kind ~= syntaxKind then
		self:ExpectFail(syntaxKind, context)

		-- Check if this is an extra token and the expected token is next
		if self.CurrentToken and self.CurrentToken.SyntaxKind == syntaxKind then
			-- Skip invalid and consume expected
			self:NextToken()
			self:NextToken()
		end

		return false
	end

	self:NextToken()
	return true
end

function Parser.Peek(self: Parser, syntaxKind: Token.TokenSyntaxKind): Token.Token?
	local token: Token.Token? = self.CurrentToken
	if token ~= nil and token.Kind == syntaxKind then
		return token
	end

	return nil
end

function Parser.ParsePrefixExpression(self: Parser): SyntaxNode.SyntaxNode
	if self:Peek(Token.SyntaxKind.LeftParen) then
		local MatchParenthesis = self.CurrentToken :: Token.Token
		self:NextToken()

		local expression = self:ParseExpression()
		if not self:Peek(Token.SyntaxKind.RightParen) then
			local suggestion = self:Peek(Token.SyntaxKind.Equal) and "; did you mean to use '{' when defining a table?"
			self:ValidateFail(Token.SyntaxKind.RightParen, MatchParenthesis, suggestion)
		else
			self:NextToken()
		end

		return expression
	else
		local start = self.TokenPosition.Start
		local expressions: Array<SyntaxNode.ValueSyntaxNode | SyntaxNode.IndexIdentifierSyntaxNode | SyntaxNode.MethodIdentifierSyntaxNode> = {self:ParseIdentifier("expression")}

		local expression: SyntaxNode.SyntaxNode = expressions[1]
		if self:Peek(Token.SyntaxKind.Dot) ~= nil then
			while self:Peek(Token.SyntaxKind.Dot) ~= nil do
				self:NextToken()
				table.insert(expressions, self:ParseIdentifier())
				-- If we didn't advance at all (no identifier), we break
				if self.TokenPosition.Start.Index == start.Index then
					break
				end
			end
			expression = SyntaxNode.MakeIndexIdentifier(expressions :: Array<SyntaxNode.ValueSyntaxNode>, {
				Start = start,
				End = self.TokenPosition.End
			})
		end

		if self:Peek(Token.SyntaxKind.Colon) ~= nil then
			local methodName: SyntaxNode.ValueSyntaxNode = self:ParseIdentifier()
			expression = SyntaxNode.MakeMethodIdentifier(expression :: SyntaxNode.IndexIdentifierSyntaxNode, methodName, {
				Start = start,
				End = self.TokenPosition.End
			})
		end
		return expression
	end
end

function Parser.ParsePrimaryExpression(self: Parser): SyntaxNode.SyntaxNode
	local expression = self:ParsePrefixExpression()
	local expressions = {expression}
	if expression:is(SyntaxNode.SyntaxNodeKind.IndexIdentifier) then
		expressions = (expression :: SyntaxNode.IndexIdentifierSyntaxNode).Identifiers
	elseif expression:is(SyntaxNode.SyntaxNodeKind.MethodIdentifier) then
		expressions = (expression :: SyntaxNode.MethodIdentifierSyntaxNode).IndexIdentifier.Identifiers
		table.insert(expressions, (expression :: SyntaxNode.MethodIdentifierSyntaxNode).MethodName)
	end
	
	local start = self.TokenPosition.Start

	while true do
		if self:Peek(Token.SyntaxKind.Dot) then
			self:NextToken()
			table.insert(expressions, self:ParseIdentifier())	
			expression = SyntaxNode.MakeIndexIdentifier(expressions :: Array<SyntaxNode.ValueSyntaxNode>, {
				Start = start,
				End = self.TokenPosition.End
			})
		elseif self:Peek(Token.SyntaxKind.LeftBracket) then
			local MatchBracket = self.CurrentToken :: Token.Token
			self:NextToken()
			table.insert(expressions, self:ParseExpression())

			expression = SyntaxNode.MakeIndexIdentifier(expressions :: Array<SyntaxNode.ValueSyntaxNode>, {
				Start = start,
				End = self.TokenPosition.End
			})

			self:Validate(Token.SyntaxKind.RightBracket, MatchBracket)
		elseif self:Peek(Token.SyntaxKind.Colon) then
			self:NextToken()
			
			local methodName = self:ParseIdentifier()
			local fn = SyntaxNode.MakeMethodIdentifier(expression :: SyntaxNode.IndexIdentifierSyntaxNode, methodName, {
				Start = start,
				End = self.TokenPosition.End
			})
			local args = self:ParseFunctionArguments(expression :: SyntaxNode.SyntaxNode?)
			expression = SyntaxNode.MakeFunctionCall(fn, args, {
				Start = start,
				End = self.TokenPosition.End
			})

			expressions = {expression}
		elseif self:Peek(Token.SyntaxKind.LeftParen) or self:Peek(Token.SyntaxKind.LeftBrace) or self:Peek(Token.SyntaxKind.String) or self:Peek(Token.SyntaxKind.LongString) then
			local args = self:ParseFunctionArguments()
			expression = SyntaxNode.MakeFunctionCall(expression :: any, args, {
				Start = start,
				End = self.TokenPosition.End
			})
			--table.clear(expressions)
			expressions = {expression}
		else
			break
		end
	end

	return expression
end

function Parser.ParseSimpleExpression(self: Parser): SyntaxNode.SyntaxNode
	local start = self.TokenPosition
	
	local syntaxNodeKind = Parser.BuiltInTypesTokens[(self.CurrentToken :: Token.Token).Kind]
	if syntaxNodeKind ~= nil then
		self:NextToken()
		return SyntaxNode.MakeNode(syntaxNodeKind, start)
	end
	if self:Peek(Token.SyntaxKind.LeftBrace) then
		return self:ParseTableConstructor()
	end
	if self:Peek(Token.SyntaxKind.If) then
		self:NextToken()
		return self:ParseIfElseExpression()
	end
	if self:Peek(Token.SyntaxKind.Function) then
		return self:ParseFunctionBody()
	end

	local str = self:Peek(Token.SyntaxKind.String) or self:Peek(Token.SyntaxKind.LongString) or self:Peek(Token.SyntaxKind.InterpolatedStringSimple)
	if str then
		self:NextToken()
		return SyntaxNode.MakeValueNode(SyntaxNode.SyntaxNodeKind.String, str.Characters, start)
	elseif self:Peek(Token.SyntaxKind.InterpolatedStringStart) then
		return self:ParseInterpolatedString()
	elseif self:Peek(Token.SyntaxKind.BrokenString) then
		local ExpressionError = self:ReportExpressionError("Malformed string")
		self:NextToken()
		return ExpressionError
	elseif self:Peek(Token.SyntaxKind.InterpolatedStringBrokenDoubleBrace) then
		local ExpressionError = self:ReportExpressionError("Double braces are not permitted within interpolated strings. Did you mean '\\{'?")
		self:NextToken()
		return ExpressionError
	end

	local number = self:Peek(Token.SyntaxKind.Number)
	if number ~= nil then
		self:NextToken()
		return SyntaxNode.MakeValueNode(SyntaxNode.SyntaxNodeKind.Number, number.Characters, start)
	end

	return self:ParsePrimaryExpression() :: SyntaxNode.SyntaxNode
end

function Parser.GenericToken(self: Parser, tokens: Array<Token.TokenSyntaxKind>, nodes: {[Token.TokenSyntaxKind]: SyntaxNode.SyntaxNodeKind}): SyntaxNode.SyntaxNodeKind?
	for _, token in tokens do
		if self:Peek(token) then
			return nodes[token]
		end
	end

	return nil
end

function Parser.ParseAssertionExpression(self: Parser): SyntaxNode.SyntaxNode
	local expression = self:ParseSimpleExpression()

	-- TODO typechecking type cast

	return expression
end

function Parser.ParseIfElseExpression(self: Parser): SyntaxNode.IfElseExpressionNode
	local start = self.TokenPosition.Start
	
	local condition = self:ParseExpression()
	self:Expect(Token.SyntaxKind.Then, "if then else expression")

	local thenExpression = self:ParseExpression()
	local elseExpression: SyntaxNode.SyntaxNode?
	local elseifExpression: SyntaxNode.IfElseExpressionNode?
	if self:Peek(Token.SyntaxKind.ElseIf) then
		self:NextToken()
		elseifExpression = self:ParseIfElseExpression()
	else

		print(self.CurrentToken, self.Tokens[self.Index - 1])
		self:Expect(Token.SyntaxKind.Else, "if then else expression")
		elseExpression = self:ParseExpression()
	end

	return SyntaxNode.MakeIfElse(condition, thenExpression, elseExpression, elseifExpression, {
		Start = start,
		End = self.TokenPosition.End
	})
end

function Parser.ParseUnarySyntax(self: Parser): SyntaxNode.SyntaxNodeKind?
	local tokens = { Token.SyntaxKind.Minus, Token.SyntaxKind.Not, Token.SyntaxKind.Hashtag }
	return self:GenericToken(tokens, Parser.UnaryOperators)
end

function Parser.ParseBinarySyntax(self: Parser): SyntaxNode.SyntaxNodeKind?
	local tokens = {
		Token.SyntaxKind.Plus,
		Token.SyntaxKind.Minus,

		Token.SyntaxKind.Star,
		Token.SyntaxKind.Slash,
		Token.SyntaxKind.Modulo,
		Token.SyntaxKind.Caret,

		Token.SyntaxKind.Dot2,

		Token.SyntaxKind.NotEqual,
		Token.SyntaxKind.EqualTo,
		Token.SyntaxKind.LessThan,
		Token.SyntaxKind.LessEqual,
		Token.SyntaxKind.GreaterThan,
		Token.SyntaxKind.GreaterEqual,

		Token.SyntaxKind.And,
		Token.SyntaxKind.Or
	}
	return self:GenericToken(tokens, Parser.BinaryOperators)
end

function Parser.ParseCompoundSyntax(self: Parser): SyntaxNode.SyntaxNodeKind?
	local tokens = {
		Token.SyntaxKind.PlusEqual,
		Token.SyntaxKind.MinusEqual,
		Token.SyntaxKind.StarEqual,
		Token.SyntaxKind.SlashEqual,
		Token.SyntaxKind.ModuloEqual,
		Token.SyntaxKind.CaretEqual,
		Token.SyntaxKind.Dot2Equal,
	}
	return self:GenericToken(tokens, Parser.CompoundOperators)
end

function Parser.ParseExpression(self: Parser, limit: number?): SyntaxNode.SyntaxNode
	local limit: number = limit or 0

	local expression: SyntaxNode.SyntaxNode = nil
	local currentPosition = self.TokenPosition
	local unarySyntax = self:ParseUnarySyntax()

	if unarySyntax then
		local start = self.TokenPosition.Start
		local unaryExpression = self:ParseExpression()
		expression = SyntaxNode.MakeUnaryOperation(unarySyntax, unaryExpression, {
			Start = start,
			End = self.TokenPosition.End
		})
	else
		expression = self:ParseAssertionExpression()
	end

	local binarySyntax = self:ParseBinarySyntax()
	local binaryPriority = {
		{ 6, 6 },
		{ 6, 6 },
		{ 7, 7 },
		{ 7, 7 },
		{ 7, 7 }, -- `+' `-' `*' `/' `%'
		{ 10, 9 },
		{ 5, 4 }, -- power and concat (right associative)
		{ 3, 3 },
		{ 3, 3 }, -- equality and inequality
		{ 3, 3 },
		{ 3, 3 },
		{ 3, 3 },
		{ 3, 3 }, -- order
		{ 2, 2 },
		{ 1, 1 }, -- logical (and/or)
	}

	while binarySyntax and binaryPriority[Parser.BinaryOperatorValues[binarySyntax]][1] > limit do
		local start = self.TokenPosition.Start
		local subExpression = self:ParseExpression(binaryPriority[Parser.BinaryOperatorValues[binarySyntax]][2])
		expression = SyntaxNode.MakeBinaryOperation(binarySyntax, expression, subExpression, {
			Start = start,
			End = self.TokenPosition.End
		})

		binarySyntax = self:ParseBinarySyntax()
	end

	return expression
end

function Parser.ParseExpressionList(self: Parser): SyntaxNode.ExpressionListSyntaxNode
	local start = self.TokenPosition.Start
	
	local expressions = {self:ParseExpression()}
	while self:Peek(Token.SyntaxKind.Comma) do
		self:NextToken()

		if self:Peek(Token.SyntaxKind.RightParen) then
			self:ReportError("Expected expression after ',' but got ')' instead")
			break
		end

		table.insert(expressions, self:ParseExpression())
	end

	return SyntaxNode.MakeExpressionList(expressions, {
		Start = start,
		End = self.TokenPosition.End
	})
end

type TableField = { Key: SyntaxNode.SyntaxNode, Value: SyntaxNode.SyntaxNode? }
function Parser.ParseTableConstructor(self: Parser): SyntaxNode.SyntaxNode
	local start = self.TokenPosition.Start
	local MatchBrace = self.CurrentToken :: Token.Token

	self:Expect(Token.SyntaxKind.LeftBrace, "table literal")

	local fields: Array<SyntaxNode.TableFieldSyntaxNode> = {}
	local currentIndexArray = 1

	while self:Peek(Token.SyntaxKind.RightBrace) == nil do
		if self:Peek(Token.SyntaxKind.LeftBracket) then
			local start = self.TokenPosition.Start
			local MatchBracket = self.CurrentToken :: Token.Token
			self:NextToken()

			local key = self:ParseExpression()
			self:Validate(Token.SyntaxKind.RightBracket, MatchBracket)
			self:Expect(Token.SyntaxKind.Equal, "table field")
			local value = self:ParseExpression()

			table.insert(fields, SyntaxNode.MakeTableField(key, value, {
				Start = start,
				End = self.TokenPosition.End
			}))

		elseif self:Peek(Token.SyntaxKind.Identifier) == nil then
			local start = self.TokenPosition.Start
			local key = SyntaxNode.MakeValueNode(SyntaxNode.SyntaxNodeKind.Number, currentIndexArray, self.TokenPosition)
			local value = self:ParseExpression()

			table.insert(fields, SyntaxNode.MakeTableField(key, value, {
				Start = start,
				End = self.TokenPosition.End
			}))
			currentIndexArray += 1
		else
			local start = self.TokenPosition.Start
			local identifier = self:ParseIdentifier("table field")
			self:Expect(Token.SyntaxKind.Equal, "table field")
			local value = self:ParseExpression()

			table.insert(fields, SyntaxNode.MakeTableField(identifier, value, {
				Start = start,
				End = self.TokenPosition.End
			}))
		end

		if self:Peek(Token.SyntaxKind.Comma) or self:Peek(Token.SyntaxKind.Colon) then
			self:NextToken()
		elseif self:Peek(Token.SyntaxKind.LeftBracket) or self:Peek(Token.SyntaxKind.Identifier) then
			self:ReportError("Expected ',' after table constructor element")
		elseif not self:Peek(Token.SyntaxKind.RightBrace) then
			break
		end
	end

	self:Validate(Token.SyntaxKind.RightBrace, MatchBrace)

	return SyntaxNode.MakeTable(fields, {
		Start = start,
		End = self.TokenPosition.End
	})
end

function Parser.ParseFunctionArguments(self: Parser, selfExpression: SyntaxNode.SyntaxNode?): Array<SyntaxNode.SyntaxNode | SyntaxNode.ErrorSyntaxNode>
	local start = self.TokenPosition
	local arguments: Array<SyntaxNode.SyntaxNode> = {}
	if self:Peek(Token.SyntaxKind.LeftParen) then
		local MatchParenthesis = self.CurrentToken :: Token.Token
		self:NextToken()

		if self:Peek(Token.SyntaxKind.RightParen) == nil then
			arguments = self:ParseExpressionList().Expressions
		end
		self:Validate(Token.SyntaxKind.RightParen, MatchParenthesis)
	elseif self:Peek(Token.SyntaxKind.LeftBrace) then
		arguments = { self:ParseTableConstructor() }
	elseif self:Peek(Token.SyntaxKind.String) or self:Peek(Token.SyntaxKind.LongString) then
		arguments = { SyntaxNode.MakeValueNode(SyntaxNode.SyntaxNodeKind.String, (self.CurrentToken :: Token.Token).Characters, self.TokenPosition) }
	else
		if selfExpression then
			return { self:ReportExpressionError("Expected function call arguments after '('") }
		else
			return { self:ReportExpressionError(`Expected '(', '\{' or <string> when parsing function call, got {self.CurrentToken}`) }
		end
	end

	if selfExpression ~= nil then
		table.insert(arguments, 1, selfExpression)
	end

	return arguments
end

function Parser.ParseIdentifier(self: Parser, context: string?): SyntaxNode.ValueSyntaxNode
	local nameValue = "%error-iden%"
	local nameToken = self.CurrentToken :: Token.Token
	if self:Peek(Token.SyntaxKind.Identifier) then
		nameValue = nameToken.Characters
		self:NextToken()
	else
		if context then
			self:ReportError(`Expected identifier when parsing {context}, got {self.CurrentToken}`)
		else
			self:ReportError(`Expected identifier, got {self.CurrentToken}`)
		end
	end

	return SyntaxNode.MakeValueNode(SyntaxNode.SyntaxNodeKind.Identifier, nameValue, self.TokenPosition)
end

function Parser.ParseBinding(self: Parser): SyntaxNode.BindingSyntaxNode
	local tokenPosition = self.TokenPosition
	local identifier = self:ParseIdentifier()
	-- TODO type annotation

	return SyntaxNode.MakeBinding(identifier, tokenPosition)
end

function Parser.ParseBindingList(self: Parser): Array<SyntaxNode.BindingSyntaxNode>
	local bindings: Array<SyntaxNode.BindingSyntaxNode> = {}
	while true do
		table.insert(bindings, self:ParseBinding())
		if not self:Peek(Token.SyntaxKind.Comma) then
			break
		end

		self:NextToken()
	end

	return bindings
end

function Parser.ParseInterpolatedString(self: Parser): SyntaxNode.InterpolatedStringSyntaxNode | SyntaxNode.ErrorSyntaxNode
	local start = self.TokenPosition.Start
	
	local expressions: Array<SyntaxNode.SyntaxNode> = {}
	local a = 0
	while a < 10 do
		a += 1
		local canContinue = self:Peek(Token.SyntaxKind.InterpolatedStringEnd) == nil and self:Peek(Token.SyntaxKind.InterpolatedStringSimple) == nil
		local str = (self:Peek(Token.SyntaxKind.InterpolatedStringStart) or self:Peek(Token.SyntaxKind.InterpolatedStringMiddle)
			or self:Peek(Token.SyntaxKind.InterpolatedStringEnd) or self:Peek(Token.SyntaxKind.InterpolatedStringSimple)) :: Token.Token
		table.insert(expressions, SyntaxNode.MakeValueNode(SyntaxNode.SyntaxNodeKind.String, self.TokenPosition, str.Characters))

		if not canContinue then break end
		self:NextToken()

		if self:Peek(Token.SyntaxKind.InterpolatedStringMiddle) or self:Peek(Token.SyntaxKind.InterpolatedStringEnd) then
			table.insert(expressions, self:ReportExpressionError("Malformed interpolated string, expected an expression in '{}'"))
			self:NextToken()
			break
		elseif self:Peek(Token.SyntaxKind.BrokenString) then
			table.insert(expressions, self:ReportExpressionError("Maliformed interpolated string, did you forget to add a '`'?"))
			self:NextToken()
			break
		else 
			table.insert(expressions, self:ParseExpression())
		end

		if not (self:Peek(Token.SyntaxKind.InterpolatedStringStart) or self:Peek(Token.SyntaxKind.InterpolatedStringMiddle)
			or self:Peek(Token.SyntaxKind.InterpolatedStringEnd)) then
			if self:Peek(Token.SyntaxKind.InterpolatedStringBrokenDoubleBrace) then
				local ExpressionError = self:ReportExpressionError("Malformed interpolated string, did you forget to add a '`'?")
				self:NextToken()
				return ExpressionError
			elseif self:Peek(Token.SyntaxKind.BrokenString) then
				local ExpressionError = self:ReportExpressionError("Malformed interpolated string, did you forget to add a '}'?")
				self:NextToken()
				return ExpressionError
			else
				return self:ReportExpressionError(`Malformed interpolated string, got {self.CurrentToken}`)
			end
		end
	end

	return SyntaxNode.MakeInterpolatedString(expressions, {
		Start = start,
		End = self.TokenPosition.End
	})
end

function Parser.ParseFunctionBody(self: Parser): SyntaxNode.FunctionDefinitionSyntaxNode
	local start = self.TokenPosition.Start
	local MatchFunction = self.CurrentToken :: Token.Token

	-- TODO generic types
	local MatchParenthesis = self.CurrentToken :: Token.Token
	self:Expect(Token.SyntaxKind.LeftParen, "function")
	local bindings: Array<SyntaxNode.BindingSyntaxNode>
	if self:Peek(Token.SyntaxKind.RightParen) == nil then
		bindings = self:ParseBindingList()
	else
		bindings = {}
	end

	for _, binding in bindings do
		self:AddLocalVariableToScope(binding, 1)
	end

	self:Validate(Token.SyntaxKind.RightParen, MatchParenthesis, true)

	-- TODO return type annotation
	local body = self:ParseScope()
	local hasEnd = self:ValidateEnd(Token.SyntaxKind.End, MatchFunction)

	return SyntaxNode.MakeFunctionDefinition(bindings, body, {
		Start = start,
		End = self.TokenPosition.End
	})
end

function Parser.ParseIfStatement(self: Parser): SyntaxNode.IfNode
	local start = self.TokenPosition.Start
	local condition = self:ParseExpression()
	local matchThen = self.CurrentToken :: Token.Token

	self:Expect(Token.SyntaxKind.Then, "if statement")

	local thenScope = self:ParseScope()
	local elseScope: SyntaxNode.ScopeSyntaxNode = nil
	local elseifScope: SyntaxNode.IfNode? = nil
	local hasEnd = false

	if self:Peek(Token.SyntaxKind.ElseIf) then
		self:NextToken()
		elseifScope = self:ParseIfStatement()
	else
		local matchThenElse = matchThen

		if self:Peek(Token.SyntaxKind.Else) then
			matchThenElse = self.CurrentToken :: Token.Token
			self:NextToken()
			elseScope = self:ParseScope()
		end
		hasEnd = self:ValidateEnd(Token.SyntaxKind.End, matchThenElse)
	end

	return SyntaxNode.MakeIf(condition, thenScope, elseScope, elseifScope, {
		Start = start,
		End = self.TokenPosition.End
	})
end

function Parser.CheckIsLValue(left: SyntaxNode.SyntaxNode): boolean
	return table.find(Parser.LValueSyntaxNodeKinds, left.Kind) ~= nil
end

function Parser.ParseCompoundAssignment(self: Parser, left: SyntaxNode.SyntaxNode, tokenKind: Token.TokenSyntaxKind, operator: SyntaxNode.CompoundOperationKind): SyntaxNode.CompoundOperationSyntaxNode
	if not Parser.CheckIsLValue(left) then
		left = self:ReportExpressionError("Assigned expression must be a variable or a field", left.Position)
	end
	--self:Expect(tokenKind)
	self:NextToken()

	local expression = self:ParseExpression()
	return SyntaxNode.MakeCompoundOperation(operator, left, expression, {
		Start = left.Position.Start,
		End = self.TokenPosition.End
	})
end

function Parser.ParseAssignment(self: Parser, left: SyntaxNode.SyntaxNode): SyntaxNode.AssignmentStatementSyntaxNode
	--local expressions = self:ParseExpressionList().Children
	if not Parser.CheckIsLValue(left) then
		left = self:ReportExpressionError("Assigned expression must be a variable or a field", left.Position)
	end

	local variables: Array<SyntaxNode.ValueSyntaxNode | SyntaxNode.IndexIdentifierSyntaxNode> = {left :: SyntaxNode.ValueSyntaxNode | SyntaxNode.IndexIdentifierSyntaxNode}
	while self:Peek(Token.SyntaxKind.Comma) do
		self:NextToken()

		local expression = self:ParsePrimaryExpression()
		if not Parser.CheckIsLValue(expression) then
			expression = self:ReportExpressionError("Assigned expression must be a variable or a field", expression.Position)
		end

		table.insert(variables, expression :: SyntaxNode.ValueSyntaxNode | SyntaxNode.IndexIdentifierSyntaxNode)
	end

	self:Expect(Token.SyntaxKind.Equal, "assignment")

	local values = self:ParseExpressionList().Expressions :: Array<SyntaxNode.SyntaxNode>

	local assignments: Array<SyntaxNode.AssignmentSyntaxNode> = {}
	for index, binding in variables :: Array<SyntaxNode.ValueSyntaxNode | SyntaxNode.IndexIdentifierSyntaxNode> do
		local identifier = (binding :: SyntaxNode.SyntaxNode):is(SyntaxNode.SyntaxNodeKind.Identifier) and (binding :: SyntaxNode.ValueSyntaxNode).Value or (binding :: SyntaxNode.IndexIdentifierSyntaxNode).Identifiers[1].Value

		for i = self.ScopeLevel, 1, -1 do
			if table.find(self.LocalVariablesStack[i], identifier) ~= nil then
				-- Local variable reassignment
				table.insert(assignments, SyntaxNode.MakeAssignment(binding, values[index], true, true, {
					Start = binding.Position.Start,
					End = values[index] and values[index].Position.End or binding.Position.End
				}))
			else
				if table.find(self.GlobalVariablesStack, identifier) == nil then
					-- Global variable declaration
					table.insert(self.GlobalVariablesStack, identifier)
					table.insert(assignments, SyntaxNode.MakeAssignment(binding, values[index], false, false, {
						Start = binding.Position.Start,
						End = values[index] and values[index].Position.End or binding.Position.End
					}))
				else
					-- Global variable reassignment
					table.insert(assignments, SyntaxNode.MakeAssignment(binding, values[index], false, true, {
						Start = binding.Position.Start,
						End = values[index] and values[index].Position.End or binding.Position.End
					}))
				end
			end
		end
	end

	return SyntaxNode.MakeAssignmentStatement(assignments, {
		Start = left.Position.Start,
		End = self.TokenPosition.End
	})
end

function Parser.AddLocalVariableToScope(self: Parser, binding: SyntaxNode.ValueSyntaxNode | SyntaxNode.BindingSyntaxNode, levelOffset: number)
	local scopeLevel: number = self.ScopeLevel
	local stack = self.LocalVariablesStack[scopeLevel + levelOffset]
	if stack == nil then
		stack = {}
		table.insert(self.LocalVariablesStack, stack)
	end

	local identifier = (binding :: SyntaxNode.SyntaxNode):is(SyntaxNode.SyntaxNodeKind.Identifier) and (binding :: SyntaxNode.ValueSyntaxNode).Value or (binding :: SyntaxNode.BindingSyntaxNode).Identifier.Value
	if table.find(stack, identifier) == nil then
		table.insert(stack, identifier)
	end
end

function Parser.ParseStatement(self: Parser): SyntaxNode.SyntaxNode
	local Start = self.TokenPosition.Start
	if self:Peek(Token.SyntaxKind.If) ~= nil then
		-- If statement
		self:NextToken()
		return self:ParseIfStatement()
	end

	if self:Peek(Token.SyntaxKind.Local) ~= nil then
		self:NextToken()
		if self:Peek(Token.SyntaxKind.Function) ~= nil then
			self:NextToken()
			-- Local function
			local identifier = self:ParseIdentifier("variable name")
			self:AddLocalVariableToScope(identifier, 0)
			self.RecoveryStopOnToken[Token.SyntaxKind.End] += 1
			local body = self:ParseFunctionBody()
			self.RecoveryStopOnToken[Token.SyntaxKind.End] -= 1

			return SyntaxNode.MakeFunction(identifier, body, true, {
				Start = Start,
				End = self.TokenPosition.End
			})
		else
			self.RecoveryStopOnToken[Token.SyntaxKind.Equal] += 1
			-- Local variable
			local variables = self:ParseBindingList()
			for _, binding in variables do
				self:AddLocalVariableToScope(binding, 0)
			end
			self.RecoveryStopOnToken[Token.SyntaxKind.Equal] -= 1

			local values = {}
			if self:Peek(Token.SyntaxKind.Equal) ~= nil then
				self:NextToken()
				values = self:ParseExpressionList().Expressions
			end
			
			local assignments: Array<SyntaxNode.AssignmentSyntaxNode> = {}
			for index, binding in variables :: Array<SyntaxNode.BindingSyntaxNode> do
				table.insert(assignments, SyntaxNode.MakeAssignment(binding, values[index], true, false, {
					Start = binding.Position.Start,
					End = values[index] and values[index].Position.End or binding.Position.End
				}))
			end

			return SyntaxNode.MakeAssignmentStatement(assignments, {
				Start = Start,
				End = self.TokenPosition.End
			})
		end
	end
	if self:Peek(Token.SyntaxKind.Function) ~= nil then
		-- Global Function
		-- TODO add to global scope only if it's not a function from a local variable?
		self:NextToken()
		local expression: SyntaxNode.ValueSyntaxNode | SyntaxNode.IndexIdentifierSyntaxNode | SyntaxNode.MethodIdentifierSyntaxNode = self:ParseIdentifier("function name")
		if self:Peek(Token.SyntaxKind.Dot) ~= nil then
			local expressions: Array<SyntaxNode.ValueSyntaxNode> = {expression :: SyntaxNode.ValueSyntaxNode}
			self:NextToken()
			
			local startDot = self.TokenPosition.Start.Index
			while self:Peek(Token.SyntaxKind.Dot) ~= nil do
				self:NextToken()
				table.insert(expressions, self:ParseIdentifier("field name"))
			end
			expression = SyntaxNode.MakeIndexIdentifier(expressions, {
				Start = expressions[1].Position.Start,
				End = self.TokenPosition.End
			})
		end
		if self:Peek(Token.SyntaxKind.Colon) ~= nil then
			self:NextToken()
			local methodName = self:ParseIdentifier("method name")
			expression = SyntaxNode.MakeMethodIdentifier(expression :: SyntaxNode.IndexIdentifierSyntaxNode, methodName, {
				Start = expression.Position.Start,
				End = self.TokenPosition.End
			})
		end

		self.RecoveryStopOnToken[Token.SyntaxKind.End] += 1
		local body = self:ParseFunctionBody()
		self.RecoveryStopOnToken[Token.SyntaxKind.End] -= 1

		return SyntaxNode.MakeFunction(expression, body, false, {
			Start = Start,
			End = self.TokenPosition.End
		})
	end

	if self:Peek(Token.SyntaxKind.For) ~= nil then
		local bindings = self:ParseBindingList()
		if self:Peek(Token.SyntaxKind.Equal) then
			self:NextToken()

			local binding = bindings[1]
			self:AddLocalVariableToScope(binding, 1)
			-- Numerical loop
			local StartNum = self:ParseExpression()
			self:Expect(Token.SyntaxKind.Comma, "index range")
			local EndNum = self:ParseExpression()
			local Step: SyntaxNode.SyntaxNode?
			if self:Peek(Token.SyntaxKind.Comma) then
				self:NextToken()
				Step = self:ParseExpression()
			end

			local MatchDo = self.CurrentToken :: Token.Token
			local HasDo = self:Expect(Token.SyntaxKind.Do, "for loop")

			local Scope = self:ParseScope()
			local HasEnd = self:ValidateEnd(Token.SyntaxKind.End, MatchDo)

			return SyntaxNode.MakeNumericalFor(binding, StartNum, EndNum, Step, Scope, {
				Start = Start,
				End = self.TokenPosition.End
			})
		else
			-- Generic loop
			for _, binding in bindings do
				self:AddLocalVariableToScope(binding, 1)
			end

			local HasIn = self:Expect(Token.SyntaxKind.In, "for loop")

			local values = self:ParseExpressionList().Expressions

			local MatchDo = self.CurrentToken :: Token.Token
			local HasDo = self:Expect(Token.SyntaxKind.Do, "for loop")

			local Scope = self:ParseScope()

			local HasEnd = self:ValidateEnd(Token.SyntaxKind.End, MatchDo)

			return SyntaxNode.MakeGenericFor(bindings, values, Scope, {
				Start = Start,
				End = self.TokenPosition.End
			})
		end
	end
	if self:Peek(Token.SyntaxKind.While) ~= nil then
		self:NextToken()
		-- While loop
		local condition = self:ParseExpression()

		local MatchDo = self.CurrentToken :: Token.Token
		local HasDo = self:Expect(Token.SyntaxKind.Do, "for loop")

		local body = self:ParseScope()

		local HasEnd = self:ValidateEnd(Token.SyntaxKind.End, MatchDo)

		return SyntaxNode.MakeWhile(condition, body, {
			Start = Start,
			End = self.TokenPosition.End
		})
	end
	if self:Peek(Token.SyntaxKind.Do) ~= nil then
		local body = self:ParseScope()
		self:Expect(Token.SyntaxKind.End)

		return SyntaxNode.MakeDo(body, {
			Start = Start,
			End = self.TokenPosition.End
		})
	end

	if self:Peek(Token.SyntaxKind.Repeat) ~= nil then
		local MatchRepeat = self.CurrentToken :: Token.Token

		self:NextToken()
		-- Repeat loop
		local body = self:ParseScope()

		local HasUntil = self:ValidateEnd(Token.SyntaxKind.End, MatchRepeat)

		local condition = self:ParseExpression()

		return SyntaxNode.MakeRepeatUntil(condition, body, {
			Start = Start,
			End = self.TokenPosition.End
		})
	end

	if self:Peek(Token.SyntaxKind.Return) ~= nil then
		self:NextToken()

		local expressions: Array<SyntaxNode.SyntaxNode>
		if table.find(Parser.ExitScopeTokens, (self.CurrentToken :: Token.Token).Kind) == nil and not self:Peek(Token.SyntaxKind.SemiColon) then
			expressions = self:ParseExpressionList().Expressions
		end

		local firstPosition = expressions and #expressions > 0 and expressions[1].Position.Start or -math.huge
		local lastPosition = expressions and #expressions > 0 and expressions[#expressions].Position.End or math.huge
		return SyntaxNode.MakeValueNode(SyntaxNode.SyntaxNodeKind.Return, expressions, {
			Start = Start,
			End = self.TokenPosition.End
		})
	end

	local tokenPosition = self.TokenPosition
	if self:Peek(Token.SyntaxKind.Break) ~= nil then
		self:NextToken()
		return SyntaxNode.MakeNode(SyntaxNode.SyntaxNodeKind.Break, tokenPosition)
	end

	local expression: SyntaxNode.SyntaxNode = self:ParsePrimaryExpression()
	if expression:is(SyntaxNode.SyntaxNodeKind.FunctionCall) then -- We can return early
		return expression
	end

	-- The next token is , or =, it's an assignment
	if self:Peek(Token.SyntaxKind.Comma) or self:Peek(Token.SyntaxKind.Equal) then
		return self:ParseAssignment(expression)
	end

	local tokenKind = (self.CurrentToken :: Token.Token).Kind
	local operator = Parser.CompoundOperators[tokenKind]
	if operator ~= nil then
		return self:ParseCompoundAssignment(expression, tokenKind, operator)
	end

	if self:Peek(Token.SyntaxKind.Continue) then
		return SyntaxNode.MakeNode(SyntaxNode.SyntaxNodeKind.Continue, self.TokenPosition)
	end

	local reservedIdentifier = (expression :: SyntaxNode.ValueSyntaxNode).Value or ""
	if reservedIdentifier == "type" then
		return self:ReportExpressionError("Cannot use `type` keyword: cannot parse Luau types.")
	elseif reservedIdentifier == "export" then
		return self:ReportExpressionError("Cannot use `export` keyword: cannot parse Luau types.")	
	end

	return self:ReportStatementError("Incomplete statement: expected assignment or function call")
end

function Parser.IncrementScopeLevel(self: Parser)
	local scopeLevel: number = self.ScopeLevel
	scopeLevel += 1
	self.ScopeLevel = scopeLevel

	if self.LocalVariablesStack[scopeLevel] == nil then
		table.insert(self.LocalVariablesStack, {})
	end
end

function Parser.DecrementScopeLevel(self: Parser)
	local scopeLevel: number = self.ScopeLevel
	table.clear(self.LocalVariablesStack[scopeLevel])

	self.ScopeLevel = scopeLevel - 1
end

function Parser.ParseScope(self: Parser): SyntaxNode.ScopeSyntaxNode
	self:IncrementScopeLevel()

	local start = self.TokenPosition.Start
	local statements: Array<SyntaxNode.SyntaxNode> = {}
	while self.CurrentToken and table.find(Parser.ExitScopeTokens, self.CurrentToken.Kind) == nil do
		local curStart = self.TokenPosition.Start.Index
		local node = self:ParseStatement()
		-- Did not advance, we force advance
		if self.TokenPosition.Start.Index == curStart then
			self:NextToken()
		end

		if self:Peek(Token.SyntaxKind.SemiColon) then
			self:NextToken()
		end
		table.insert(statements, node)

		if node.Kind == SyntaxNode.SyntaxNodeKind.Break or node.Kind == SyntaxNode.SyntaxNodeKind.Return or node.Kind == SyntaxNode.SyntaxNodeKind.Continue then break end
	end

	self:DecrementScopeLevel()

	return SyntaxNode.MakeScope(statements, {
		Start = start,
		End = self.TokenPosition.End
	})
end

function Parser.Parse(self: Parser): ParserResult
	self:NextToken()
	local root = self:ParseScope()
	if not self:Peek(Token.SyntaxKind.EndOfFile) then
		self:ExpectFail(Token.SyntaxKind.EndOfFile)
	end

	table.clear(self.GlobalVariablesStack)

	return {
		Tree = root,
		Errors = self.Errors
	}
end

return Parser