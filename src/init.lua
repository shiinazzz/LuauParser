local LuauParser = {
    Lexer = require(script.Lexer),
    Parser = require(script.Parser)
}

local Token = require(script.Token)
local SyntaxNode = require(script.SyntaxNode)

export type Token = Token.Token

export type SyntaxNode = SyntaxNode.SyntaxNode
export type ValueSyntaxNode = SyntaxNode.ValueSyntaxNode
export type BinaryOperationSyntaxNode = SyntaxNode.BinaryOperationSyntaxNode
export type UnaryOperationSyntaxNode = SyntaxNode.UnaryOperationSyntaxNode
export type CompoundOperationSyntaxNode = SyntaxNode.CompoundOperationSyntaxNode
export type ErrorSyntaxNode = SyntaxNode.ErrorSyntaxNode
export type ScopeSyntaxNode = SyntaxNode.ScopeSyntaxNode
export type BindingSyntaxNode = SyntaxNode.BindingSyntaxNode
export type AssignmentSyntaxNode = SyntaxNode.AssignmentSyntaxNode
export type AssignmentStatementSyntaxNode = SyntaxNode.AssignmentStatementSyntaxNode
export type FunctionCallSyntaxNode = SyntaxNode.FunctionCallSyntaxNode
export type TableSyntaxNode = SyntaxNode.TableSyntaxNode
export type TableFieldSyntaxNode = SyntaxNode.TableFieldSyntaxNode
export type IfElseExpressionNode = SyntaxNode.IfElseExpressionNode
export type IfNode = SyntaxNode.IfNode
export type FunctionDefinitionSyntaxNode = SyntaxNode.FunctionDefinitionSyntaxNode
export type FunctionSyntaxNode = SyntaxNode.FunctionSyntaxNode
export type ExpressionListSyntaxNode = SyntaxNode.ExpressionListSyntaxNode
export type DoSyntaxNode = SyntaxNode.DoSyntaxNode
export type WhileSyntaxNode = SyntaxNode.WhileSyntaxNode
export type RepeatUntilSyntaxNode = SyntaxNode.RepeatUntilSyntaxNode
export type InterpolatedStringSyntaxNode = SyntaxNode.InterpolatedStringSyntaxNode
export type NumericalForSyntaxNode = SyntaxNode.NumericalForSyntaxNode
export type GenericForSyntaxNode = SyntaxNode.GenericForSyntaxNode

return LuauParser